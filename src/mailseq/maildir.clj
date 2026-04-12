;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.maildir
  "Read-only Maildir backend.

  A Maildir is a directory with three subdirectories: `cur/`, `new/`
  and `tmp/`. Each message lives in its own file. Messages in `cur/`
  carry flags as a `:2,<letters>` suffix; messages in `new/` have no
  flags. We never move files between `new/` and `cur/` — this library
  is strictly read-only.

  This namespace intentionally does not touch the filesystem beyond
  listing `cur/` and `new/` and reading individual files as bytes.
  Parsing reuses `mailseq.parse/message->map` via an in-memory
  `MimeMessage`."
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [mailseq.filter :as flt]
            [mailseq.parse :as parse])
  (:import [jakarta.mail Session]
           [jakarta.mail.internet MimeMessage]
           [java.io ByteArrayInputStream File]
           [java.nio.file Files]
           [java.util Date Properties]))

;; ---------------------------------------------------------------------------
;; Flag decoding
;; ---------------------------------------------------------------------------

(def ^:private flag-char->keyword
  "Standard Maildir flag letters (see https://cr.yp.to/proto/maildir.html).
  Letters outside this map (e.g. `P` for Passed) are dropped to stay
  within the shared flag vocabulary from the message-map contract."
  {\R :answered
   \S :seen
   \T :deleted
   \D :draft
   \F :flagged})

(defn ^:no-doc parse-flags
  "Decode the `:2,XYZ` suffix of a Maildir filename into a set of flag
  keywords. Returns `#{}` when the suffix is missing or empty."
  [^String filename]
  (let [idx (.indexOf filename ":2,")]
    (if (neg? idx)
      #{}
      (let [letters (.substring filename (+ idx 3))]
        (into #{} (keep flag-char->keyword) letters)))))

(defn ^:no-doc stable-id
  "Return the portion of the filename before the `:2,` suffix. This is
  the part Maildir guarantees to be stable across flag changes, so it
  is a safe identifier for `-by-id` even as a client re-flags a
  message.
  Also used by `mailseq.maildir.watch`."
  [^String filename]
  (let [idx (.indexOf filename ":2,")]
    (if (neg? idx) filename (.substring filename 0 idx))))

;; ---------------------------------------------------------------------------
;; Filesystem scan
;; ---------------------------------------------------------------------------

(defn- maildir-children
  "Return a seq of message files found in `cur/` and `new/` under
  `path`. The `tmp/` dir is deliberately ignored."
  [^File path]
  (let [cur (io/file path "cur")
        new (io/file path "new")]
    (concat (when (.isDirectory cur) (filter #(.isFile ^File %) (.listFiles cur)))
            (when (.isDirectory new) (filter #(.isFile ^File %) (.listFiles new))))))

(defn ^:no-doc validate-maildir!
  "Throw an ex-info if `path` is not a usable Maildir. We require the
  directory to exist and contain at least `cur/` and `new/`.
  Also used by `mailseq.maildir.watch`."
  [^File path]
  (when-not (.isDirectory path)
    (throw (ex-info (str "Not a directory: " (.getAbsolutePath path))
                    {:type ::invalid-maildir :path (.getAbsolutePath path)})))
  (doseq [sub ["cur" "new"]]
    (when-not (.isDirectory (io/file path sub))
      (throw (ex-info (str "Missing Maildir subdirectory: " sub)
                      {:type ::invalid-maildir
                       :path (.getAbsolutePath path)
                       :missing sub})))))

;; ---------------------------------------------------------------------------
;; File → message map
;; ---------------------------------------------------------------------------

(def ^:private shared-session
  ;; A Session is safe to share across threads for parsing. We hold a
  ;; single instance to avoid re-building Properties on every file.
  ;; Wrapped in delay so the Session is only created when actually needed.
  (delay (Session/getInstance (Properties.))))

(defn- read-bytes
  "Read the full contents of `f` as a byte array. Bytes upstream keeps
  parsing a pure function of its input."
  ^bytes [^File f]
  (Files/readAllBytes (.toPath f)))

(defn- file->envelope
  "Read one Maildir file, returning a lightweight map with just enough
  info for date filtering: `:file`, `:bytes`, `:date-sent`, `:fname`.
  Returns nil if the file cannot be read."
  [^File f]
  (try
    (let [bytes (read-bytes f)
          msg   (MimeMessage. ^Session @shared-session (ByteArrayInputStream. bytes))]
      {:file f :bytes bytes :date-sent (.getSentDate msg) :fname (.getName f)})
    (catch Exception e
      (log/warn "Skipping Maildir file"
                (.getAbsolutePath f)
                "- failed to read:" (.getMessage e))
      nil)))

(defn- envelope->map
  "Full-parse a message from retained bytes + file metadata."
  [{:keys [^File file ^bytes bytes fname]} parse-opts]
  (try
    (let [msg    (MimeMessage. ^Session @shared-session (ByteArrayInputStream. bytes))
          parsed (parse/message->map msg parse-opts)
          flags  (parse-flags fname)]
      (-> parsed
          (assoc :id            (stable-id fname)
                 :flags         flags
                 :date-received (Date. (.lastModified file))
                 :uid           nil)
          (dissoc :message-number)))
    (catch Exception e
      (log/warn "Skipping Maildir file"
                (.getAbsolutePath file)
                "- failed to parse:" (.getMessage e))
      nil)))

(defn ^:no-doc file->map
  "Read one Maildir file and parse it into a message map, overriding
  the keys that are Maildir-specific (id, flags, date-received, uid).
  Returns nil and logs a warning if the file cannot be read or parsed.
  Also used by `mailseq.maildir.watch`."
  [^File f parse-opts]
  (when-let [env (file->envelope f)]
    (envelope->map env parse-opts)))

;; ---------------------------------------------------------------------------
;; Public operations (called by the MaildirSource record in `mailseq`)
;; ---------------------------------------------------------------------------

(defn- sort-for-limit
  "Sort messages oldest-first by `:date-sent`, falling back to
  `:date-received` (file mtime) when `:date-sent` is nil. This places
  the freshest messages at the tail, which is the stable slice
  `apply-opts` picks when `:limit` is set."
  [messages]
  (sort-by (fn [m] (or (some-> ^Date (:date-sent m) .getTime)
                       (some-> ^Date (:date-received m) .getTime)
                       (some-> ^File (:file m) .lastModified)
                       Long/MIN_VALUE))
           messages))


(defn messages
  "Read every message under `path` (a Maildir directory), parse them
  and return the filtered vector according to `opts`. `opts` is
  validated against the common contract.

  When `:since` or `:before` is present, a two-pass strategy avoids
  full MIME parsing on messages outside the date window: pass 1
  extracts just the `Date:` header (cheap), filters + sorts + limits,
  then pass 2 does the full `message->map` only on survivors."
  [^String path opts]
  (flt/validate-opts opts)
  (let [dir   (io/file path)
        _     (validate-maildir! dir)
        files (maildir-children dir)]
    (if (or (:since opts) (:before opts))
      ;; Two-pass: envelope-only filter, then full parse on survivors
      (let [envelopes (into [] (keep file->envelope) files)
            filtered  (filterv #(flt/matches? % opts) envelopes)
            sorted    (sort-for-limit filtered)
            limited   (if-let [limit (:limit opts)]
                        (subvec sorted (max 0 (- (count sorted) limit)))
                        sorted)
            p-opts    (flt/parse-opts opts)]
        (into [] (keep #(envelope->map % p-opts)) limited))
      ;; No date filter: single pass
      (let [parsed (into [] (keep #(file->map % (flt/parse-opts opts))) files)
            sorted (sort-for-limit parsed)]
        (flt/apply-opts sorted opts)))))

(defn list-ids
  "Return a vector of stable id strings for every message in the
  Maildir at `path`. Only reads filenames — no file content is
  touched. Cost: one `listFiles` per subdirectory."
  [^String path]
  (let [dir (io/file path)]
    (validate-maildir! dir)
    (mapv (fn [^File f] (stable-id (.getName f)))
          (maildir-children dir))))

(defn by-id
  "Return the single message whose stable id matches `id`, or nil.

  Because Maildir flag changes rewrite the filename suffix, we match
  on the stable prefix. Scans `cur/` then `new/` — this is O(n) in
  v1; an index can come later if it proves necessary."
  [^String path ^String id opts]
  (flt/validate-opts opts)
  (let [dir (io/file path)]
    (validate-maildir! dir)
    (some (fn [^File f]
            (when (= id (stable-id (.getName f)))
              (file->map f (flt/parse-opts opts))))
          (maildir-children dir))))

(defn by-ids
  "Return a vector of message maps for every message whose stable id
  is in `ids` (a set of strings). A single scan of `cur/` + `new/`
  reads only the matching files.

  Typical use: call `list-ids` to get all ids, diff against a set of
  already-seen ids, then call `by-ids` with the remainder."
  [^String path ids opts]
  (flt/validate-opts opts)
  (let [dir    (io/file path)
        _      (validate-maildir! dir)
        id-set (if (set? ids) ids (set ids))
        p-opts (flt/parse-opts opts)]
    (into []
          (keep (fn [^File f]
                  (when (contains? id-set (stable-id (.getName f)))
                    (file->map f p-opts))))
          (maildir-children dir))))
