;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.imap.fetch
  "Fetch IMAP messages by date range and/or by absolute count.

  mailseq is read-only and driven by just two selection criteria:
  `:since`/`:before` (via `SentDateTerm`) and `:limit` (via UID-range
  optimisation or tail-of-result). The IMAP backend does not use
  `SEARCH FROM/TO/SUBJECT/…` because those verbs are implemented
  inconsistently across servers.

  All functions return parsed Clojure maps (via
  `mailseq.parse/message->map`) unless `:raw? true` is passed, in
  which case raw Jakarta Mail `Message` objects are returned."
  (:require [clojure.tools.logging :as log]
            [mailseq.filter :as flt]
            [mailseq.imap.folder :as folder]
            [mailseq.parse :as parse])
  (:import [jakarta.mail Folder Message UIDFolder UIDFolder$FetchProfileItem
            FetchProfile FetchProfile$Item]
           [jakarta.mail.search AndTerm ComparisonTerm SentDateTerm]))

;; ---------------------------------------------------------------------------
;; FetchProfile for efficient batch retrieval
;; ---------------------------------------------------------------------------

(defn- make-fetch-profile
  "Create a FetchProfile to pre-fetch envelope and content info."
  [{:keys [body?] :or {body? true}}]
  (let [fp (FetchProfile.)]
    (.add fp FetchProfile$Item/ENVELOPE)
    (.add fp FetchProfile$Item/FLAGS)
    (when body?
      (.add fp FetchProfile$Item/CONTENT_INFO))
    ;; Pre-fetch UID
    (.add fp UIDFolder$FetchProfileItem/UID)
    fp))

;; ---------------------------------------------------------------------------
;; Date filter
;; ---------------------------------------------------------------------------

(defn- build-date-term
  "Build a `SentDateTerm` (or `AndTerm`) from `:since`/`:before`, or
  nil when neither is present. This is the only server-side term
  mailseq uses — full-text SEARCH verbs are out of scope."
  [{:keys [since before]}]
  (let [terms (cond-> []
                since  (conj (SentDateTerm. ComparisonTerm/GE (flt/->date since)))
                before (conj (SentDateTerm. ComparisonTerm/LT (flt/->date before))))]
    (case (count terms)
      0 nil
      1 (first terms)
      (reduce (fn [a b] (AndTerm. a b)) terms))))

;; ---------------------------------------------------------------------------
;; Internal helpers
;; ---------------------------------------------------------------------------

(defn- apply-limit
  "Take the last `limit` messages (most recent) from an array."
  [^"[Ljakarta.mail.Message;" msgs limit]
  (if (and limit (< limit (alength msgs)))
    (let [start (- (alength msgs) limit)]
      (java.util.Arrays/copyOfRange msgs start (alength msgs)))
    msgs))

(defn- assoc-stable-id
  "The IMAP backend's stable `:id` is the UID as a string. Derived
  here so that a parsed IMAP message map is self-sufficient and
  conforms to the common message-map contract without further
  post-processing by the public API layer."
  [m]
  (if-let [uid (:uid m)]
    (assoc m :id (str uid))
    m))

(defn- fetch-and-parse
  "Fetch messages from a folder, apply FetchProfile, and parse.
  Messages that fail to parse are logged and skipped."
  [^Folder folder msgs parse-opts]
  (let [fp (make-fetch-profile parse-opts)]
    (.fetch folder msgs fp)
    (into []
          (keep (fn [msg]
                  (try
                    (assoc-stable-id (parse/message->map msg parse-opts))
                    (catch Exception e
                      (log/warn "Skipping message"
                                (.getMessageNumber msg)
                                "- failed to parse:" (.getMessage e))
                      nil))))
          msgs)))

;; ---------------------------------------------------------------------------
;; Folder lifecycle
;; ---------------------------------------------------------------------------

(defmacro ^:private with-folder
  "Open `folder-name` in read-only mode on `conn`, bind it to `sym`,
  execute `body`, and ensure the folder is closed on exit."
  [[sym conn folder-name] & body]
  `(let [~sym (folder/open-folder ~conn ~folder-name)]
     (try
       ~@body
       (finally
         (folder/close-folder ~sym)))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn- resolve-uid-upper-bound
  "Return the UID of the newest message in the folder, or -1 if unavailable."
  [^UIDFolder uid-folder ^Folder folder]
  (let [uid-next (.getUIDNext uid-folder)]
    (if (pos? uid-next)
      uid-next
      ;; UIDNEXT not reported — derive from the last message
      (let [cnt (.getMessageCount folder)]
        (if (pos? cnt)
          (inc (.getUID uid-folder (.getMessage folder cnt)))
          -1)))))

(defn- fetch-by-uid-limit
  "Fetch the last `limit` messages using UID range instead of loading all.
  Estimates a starting UID from the folder's UID upper bound, then widens
  the window if too many UIDs were deleted (gaps).  Falls back to
  getMessages() if the folder does not support UIDs."
  [^Folder folder limit]
  (if (instance? UIDFolder folder)
    (let [uid-folder ^UIDFolder folder
          uid-upper  (resolve-uid-upper-bound uid-folder folder)]
      (if (pos? uid-upper)
        (loop [window    (long limit)
               prev-start Long/MAX_VALUE]
          (let [start (max 1 (- uid-upper window))]
            (if (= start prev-start)
              ;; Window didn't actually move — we're at UID 1, done
              (apply-limit
               (into-array Message
                           (remove nil? (.getMessagesByUID uid-folder 1 UIDFolder/LASTUID)))
               limit)
              (let [msgs  (.getMessagesByUID uid-folder (long start) UIDFolder/LASTUID)
                    valid (into-array Message (remove nil? msgs))
                    n     (alength valid)]
                (if (or (>= n limit)
                        (<= start 1))
                  (apply-limit valid limit)
                  (let [missing    (- limit n)
                        new-window (+ window (* missing 2))]
                    (log/debug "UID range had" (- window n) "gaps, widening to" new-window)
                    (recur new-window start)))))))
        ;; uid-upper not available, fall back
        (apply-limit (.getMessages folder) limit)))
    ;; Not a UIDFolder, fall back
    (apply-limit (.getMessages folder) limit)))

(defn messages
  "Fetch messages from a folder.

  Selection options (combinable):
    :limit        - maximum number of messages to return (most recent)
    :since        - messages sent on/after this date (string or Date)
    :before       - messages sent strictly before this date
    :headers?     - include all headers in output (default: true)
    :body?        - parse body content (default: true)
    :attachments? - include attachment byte data (default: true)
    :raw?         - return raw Message objects instead of maps (default: false)

  To fetch messages newer than a known UID, use `by-uid-range`.

  Returns a vector of message maps (or Message objects if :raw? true).

  Example:
    (messages conn \"INBOX\" {:limit 10})
    (messages conn \"INBOX\" {:since \"2025-01-01\"})"
  ([conn folder-name] (messages conn folder-name {}))
  ([conn folder-name opts]
   ;; :raw? is an IMAP-only escape hatch, whitelisted on top of the
   ;; common contract. validate-opts rejects anything else, matching
   ;; the strictness Maildir has always had.
   (flt/validate-opts opts #{:raw?})
   (with-folder [folder conn folder-name]
     (let [date-term (build-date-term opts)
           limit     (:limit opts)
           msgs      (cond
                       date-term
                       (apply-limit (.search folder date-term) limit)
                       ;; limit-only: use UID range to avoid loading all messages
                       (some? limit)
                       (fetch-by-uid-limit folder limit)
                       ;; No criteria at all: load everything
                       :else
                       (.getMessages folder))]
       (if (:raw? opts)
         (vec msgs)
         (fetch-and-parse folder msgs (flt/parse-opts opts)))))))

(defn by-uid
  "Fetch messages by their IMAP UIDs.

  uids can be a single long or a collection of longs.
  Returns a vector of message maps.

  Example:
    (by-uid conn \"INBOX\" 12345)
    (by-uid conn \"INBOX\" [12345 12346 12347])"
  ([conn folder-name uids] (by-uid conn folder-name uids {}))
  ([conn folder-name uids opts]
   (flt/validate-opts opts #{:raw?})
   (with-folder [folder conn folder-name]
     (let [uid-folder ^UIDFolder folder
           uid-seq  (if (coll? uids) uids [uids])
           msgs     (into-array Message
                                (keep #(try (.getMessageByUID uid-folder (long %))
                                            (catch Exception _ nil))
                                      uid-seq))]
       (if (:raw? opts)
         (vec msgs)
         (fetch-and-parse folder msgs (flt/parse-opts opts)))))))

(defn by-uid-range
  "Fetch messages within a UID range [start, end] inclusive.

  Use UIDFolder/LASTUID as end to fetch from start to the latest message.

  Example:
    (by-uid-range conn \"INBOX\" 1000 2000)
    (by-uid-range conn \"INBOX\" 1000 UIDFolder/LASTUID)"
  ([conn folder-name start end] (by-uid-range conn folder-name start end {}))
  ([conn folder-name start end opts]
   (flt/validate-opts opts #{:raw?})
   (with-folder [folder conn folder-name]
     (let [uid-folder ^UIDFolder folder
           msgs       (.getMessagesByUID uid-folder (long start) (long end))
           valid      (into-array Message (remove nil? msgs))]
       (if (:raw? opts)
         (vec valid)
         (fetch-and-parse folder valid (flt/parse-opts opts)))))))

(defn list-uids
  "Return a vector of UID strings for every message in `folder-name`,
  without fetching or parsing message content.

  Opens the folder, retrieves all messages, and extracts their UIDs
  via a lightweight FetchProfile (UID only)."
  [conn folder-name]
  (with-folder [folder conn folder-name]
    (let [uid-folder ^UIDFolder folder
          msgs       (.getMessages folder)
          fp         (doto (FetchProfile.)
                       (.add UIDFolder$FetchProfileItem/UID))]
      (.fetch folder msgs fp)
      (into []
            (keep (fn [^Message msg]
                    (try
                      (str (.getUID uid-folder msg))
                      (catch Exception _ nil))))
            msgs))))
