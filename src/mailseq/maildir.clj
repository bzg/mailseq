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

(defn- parse-flags
  "Decode the `:2,XYZ` suffix of a Maildir filename into a set of flag
  keywords. Returns `#{}` when the suffix is missing or empty."
  [^String filename]
  (let [idx (.indexOf filename ":2,")]
    (if (neg? idx)
      #{}
      (let [letters (.substring filename (+ idx 3))]
        (into #{} (keep flag-char->keyword) letters)))))

(defn- stable-id
  "Return the portion of the filename before the `:2,` suffix. This is
  the part Maildir guarantees to be stable across flag changes, so it
  is a safe identifier for `-by-id` even as a client re-flags a
  message."
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

(defn- validate-maildir!
  "Throw an ex-info if `path` is not a usable Maildir. We require the
  directory to exist and contain at least `cur/` and `new/`."
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

(def ^:private ^Session shared-session
  ;; A Session is safe to share across threads for parsing. We hold a
  ;; single instance to avoid re-building Properties on every file.
  (Session/getInstance (Properties.)))

(defn- read-bytes
  "Read the full contents of `f` as a byte array. Bytes upstream keeps
  parsing a pure function of its input."
  ^bytes [^File f]
  (Files/readAllBytes (.toPath f)))

(defn- file->map
  "Read one Maildir file and parse it into a message map, overriding
  the keys that are Maildir-specific (id, flags, date-received, uid).
  Returns nil and logs a warning if the file cannot be read or parsed."
  [^File f parse-opts]
  (try
    (let [bytes   (read-bytes f)
          msg     (MimeMessage. shared-session (ByteArrayInputStream. bytes))
          parsed  (parse/message->map msg parse-opts)
          fname   (.getName f)
          flags   (parse-flags fname)]
      (-> parsed
          (assoc :id            (stable-id fname)
                 :flags         flags
                 :date-received (Date. (.lastModified f))
                 :uid           nil)
          (dissoc :message-number)))
    (catch Exception e
      (log/warn "Skipping Maildir file"
                (.getAbsolutePath f)
                "- failed to parse:" (.getMessage e))
      nil)))

;; ---------------------------------------------------------------------------
;; Public operations (called by the MaildirSource record in `mailseq`)
;; ---------------------------------------------------------------------------

(defn- sort-for-limit
  "Sort messages oldest-first by `:date-sent`, with nil-dated messages
  at the head. This places the freshest dated messages at the tail,
  which is the stable slice `apply-opts` picks when `:limit` is set."
  [messages]
  (sort-by (fn [m] (some-> ^Date (:date-sent m) .getTime))
           (fn [a b]
             (cond
               (and (nil? a) (nil? b)) 0
               (nil? a) -1
               (nil? b) 1
               :else (compare a b)))
           messages))

(defn messages
  "Read every message under `path` (a Maildir directory), parse them
  and return the filtered vector according to `opts`. `opts` is
  validated against the common contract."
  [^String path opts]
  (flt/validate-opts opts)
  (let [dir (io/file path)]
    (validate-maildir! dir)
    (let [parsed (into [] (keep #(file->map % opts)) (maildir-children dir))
          sorted (vec (sort-for-limit parsed))]
      (flt/apply-opts sorted opts))))

(defn by-id
  "Return the single message whose stable id matches `id`, or nil.

  Because Maildir flag changes rewrite the filename suffix, we match
  on the stable prefix. Scans `cur/` then `new/` — this is O(n) in
  v1; an index can come later if it proves necessary."
  [^String path ^String id opts]
  (flt/validate-opts (dissoc opts :limit))
  (let [dir (io/file path)]
    (validate-maildir! dir)
    (some (fn [^File f]
            (when (= id (stable-id (.getName f)))
              (file->map f opts)))
          (maildir-children dir))))
