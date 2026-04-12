;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq
  "Unified, read-only public API over mail sources.

  A *source* is created from a plain config map via `open`, which
  dispatches on `:type` (`:imap` or `:maildir`).
  Every source exposes the same operations — `list-folders`,
  `messages`, `by-id`, `by-ids`, `list-ids`, `by-id-range`,
  `watch`, `watch-async`, `close` — regardless of its backend.

  Every source owns an explicit `:folders` map from logical folder
  name to backend-specific location. The public API only accepts
  logical folder names; passing an unknown name throws `ex-info` with
  `{:type ::unknown-folder}`.

  Example:

      (with-source [src {:type :imap
                         :host \"imap.example.com\"
                         :user \"me@example.com\"
                         :password \"secret\"
                         :folders {\"INBOX\"   \"INBOX\"
                                   \"Clojure\" \"Lists/clojure\"}}]
        (mailseq/messages src \"INBOX\" {:limit 10}))"
  (:require [mailseq.source :as source]
            [mailseq.imap.connect :as imap-connect]
            [mailseq.imap.fetch :as imap-fetch]
            [mailseq.imap.idle :as imap-idle]
            [mailseq.maildir :as maildir]
            [mailseq.maildir.watch :as maildir-watch])
  (:import [java.io Closeable]
           [jakarta.mail UIDFolder]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- ->long
  "Coerce a string or number to a long."
  ^long [x]
  (if (string? x) (Long/parseLong x) (long x)))

(defn- resolve-folder
  "Resolve a logical folder name to its backend-specific location,
  throwing `ex-info` if the name is not in the explicit folder map."
  [folders folder-name]
  (if-let [entry (find folders folder-name)]
    (val entry)
    (throw (ex-info (str "Unknown folder: " (pr-str folder-name))
                    {:type    ::unknown-folder
                     :folder  folder-name
                     :known   (vec (keys folders))}))))

;; ---------------------------------------------------------------------------
;; Closed-source guard
;; ---------------------------------------------------------------------------

(defn- ensure-open! [closed?]
  (when @closed?
    (throw (ex-info "Source is closed" {:type ::closed}))))

;; ---------------------------------------------------------------------------
;; IMAP backend
;; ---------------------------------------------------------------------------

(defrecord ImapSource [conn folders closed?]
  source/MailSource
  (-list-folders [_]
    (ensure-open! closed?)
    (vec (keys folders)))
  (-list-ids [_ folder-name]
    (ensure-open! closed?)
    (imap-fetch/list-uids conn (resolve-folder folders folder-name)))
  (-messages [_ folder-name opts]
    (ensure-open! closed?)
    (imap-fetch/messages conn (resolve-folder folders folder-name) opts))
  (-by-id [_ folder-name id opts]
    (ensure-open! closed?)
    (first (imap-fetch/by-uid conn (resolve-folder folders folder-name) (->long id) opts)))
  (-by-ids [_ folder-name ids opts]
    (ensure-open! closed?)
    (imap-fetch/by-uid conn (resolve-folder folders folder-name) (mapv ->long ids) opts))
  (-watch [_ folder-name on-message opts]
    (ensure-open! closed?)
    (imap-idle/idle conn (resolve-folder folders folder-name) on-message opts))
  (-watch-async [_ folder-name on-message opts]
    (ensure-open! closed?)
    (imap-idle/idle-async conn (resolve-folder folders folder-name) on-message opts))
  (-by-id-range [_ folder-name start-id end-id opts]
    (ensure-open! closed?)
    (let [start (->long start-id)
          end   (if (nil? end-id) UIDFolder/LASTUID (->long end-id))]
      (imap-fetch/by-uid-range conn (resolve-folder folders folder-name) start end opts)))
  (-close [_]
    (reset! closed? true)
    (imap-connect/disconnect conn))
  Closeable
  (close [this] (source/-close this)))

(defn- open-imap
  [{:keys [folders] :as cfg}]
  (let [conn (imap-connect/connect (dissoc cfg :type :folders))]
    (->ImapSource conn (or folders {}) (atom false))))

;; ---------------------------------------------------------------------------
;; Maildir backend
;; ---------------------------------------------------------------------------

(defrecord MaildirSource [folders closed?]
  source/MailSource
  (-list-folders [_]
    (ensure-open! closed?)
    (vec (keys folders)))
  (-list-ids [_ folder-name]
    (ensure-open! closed?)
    (maildir/list-ids (resolve-folder folders folder-name)))
  (-messages [_ folder-name opts]
    (ensure-open! closed?)
    (maildir/messages (resolve-folder folders folder-name) opts))
  (-by-id [_ folder-name id opts]
    (ensure-open! closed?)
    (maildir/by-id (resolve-folder folders folder-name) id opts))
  (-by-ids [_ folder-name ids opts]
    (ensure-open! closed?)
    (maildir/by-ids (resolve-folder folders folder-name) ids opts))
  (-watch [_ folder-name on-message opts]
    (ensure-open! closed?)
    (maildir-watch/watch (resolve-folder folders folder-name) on-message opts))
  (-watch-async [_ folder-name on-message opts]
    (ensure-open! closed?)
    (maildir-watch/watch-async (resolve-folder folders folder-name) on-message opts))
  (-by-id-range [_ folder-name start-id end-id opts]
    (ensure-open! closed?)
    (let [path    (resolve-folder folders folder-name)
          all-ids (maildir/list-ids path)
          in-range (filterv (fn [id]
                              (and (>= (compare id (str start-id)) 0)
                                   (or (nil? end-id)
                                       (<= (compare id (str end-id)) 0))))
                            all-ids)]
      (maildir/by-ids path in-range opts)))
  (-close [_]
    (reset! closed? true))
  Closeable
  (close [this] (source/-close this)))

(defn- open-maildir
  [{:keys [folders]}]
  (->MaildirSource (or folders {}) (atom false)))

;; ---------------------------------------------------------------------------
;; Dispatch
;; ---------------------------------------------------------------------------

(defmulti ^:private open*
  "Backend-specific open. Dispatches on `:type`."
  :type)

(defmethod open* :imap    [cfg] (open-imap cfg))
(defmethod open* :maildir [cfg] (open-maildir cfg))

(defmethod open* :default [{:keys [type]}]
  (throw (ex-info (str "Unsupported source type: " (pr-str type))
                  {:type        ::unsupported-type
                   :source-type type})))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn open
  "Open a mail source from a config map. Returns a record implementing
  `mailseq.source/MailSource` and `java.io.Closeable`.

  The config must contain a `:type` key (`:imap` or `:maildir`) and a
  `:folders` map from logical folder name to backend location."
  [cfg]
  (open* cfg))

(defn close
  "Release the resources held by `src`. Idempotent."
  [src]
  (source/-close src))

(defn list-folders
  "Return the logical folder names known to `src`."
  [src]
  (source/-list-folders src))

(defn messages
  "Fetch messages from `folder-name` in `src`.

  Selection options:
    :since        - messages sent on/after this date (Date or string)
    :before       - messages sent strictly before this date
    :limit        - keep the last N (most recent) matching messages

  Parsing options (passed through to `mailseq.parse`):
    :headers?     - include the full header map (default: true)
    :body?        - parse body content (default: true)
    :attachments? - include attachment byte data (default: true)

  mailseq is read-only and intentionally exposes no substring search
  on subject/addressees/message-id: if you need that, filter the
  returned sequence yourself.

  Returns a vector of parsed message maps."
  ([src folder-name]      (messages src folder-name {}))
  ([src folder-name opts] (source/-messages src folder-name opts)))

(defn list-ids
  "Return a vector of stable id strings for every message in
  `folder-name`, without reading or parsing message content.

  For IMAP, ids are UID strings. For Maildir, ids are the filename
  prefixes before the `:2,` flag suffix.

  This is the cheapest possible operation: for Maildir it reads only
  directory listings; for IMAP it fetches only UIDs. Use it to diff
  against a set of already-seen ids, then call `by-ids` to fetch
  only the new ones."
  [src folder-name]
  (source/-list-ids src folder-name))

(defn by-id
  "Fetch a single message map (or nil) from `folder-name` by its
  backend-specific stable id. For IMAP, `id` is a UID (long or its
  string representation). For Maildir, `id` is the filename prefix
  before the `:2,` flag suffix."
  ([src folder-name id]      (by-id src folder-name id {}))
  ([src folder-name id opts] (source/-by-id src folder-name id opts)))

(defn by-ids
  "Fetch message maps for a collection of ids in a single pass.

  More efficient than repeated `by-id` calls: for Maildir, one
  directory scan reads only the matching files; for IMAP, one
  server round-trip fetches all requested UIDs.

  Typical incremental workflow:
    1. `(list-ids src folder)` → all ids
    2. diff against previously seen ids → new-ids
    3. `(by-ids src folder new-ids)` → only the new messages"
  ([src folder-name ids]      (by-ids src folder-name ids {}))
  ([src folder-name ids opts] (source/-by-ids src folder-name ids opts)))

(defn watch
  "Watch `folder-name` for new messages, calling `on-message` with each
  parsed message map. Blocks the current thread.

  For IMAP, delegates to IMAP IDLE. For Maildir, delegates to
  `java.nio.file.WatchService`.

  Options (passed through to the backend):
    :parse-opts    - options for message->map (default: {})
    :on-error      - function called with Exception on errors
    :heartbeat-ms  - IMAP only: interval between NOOP heartbeats (default: 1200000)
    :settle-ms     - Maildir only: delay after file creation (default: 50)

  Returns nil when the watch terminates."
  ([src folder-name on-message]      (watch src folder-name on-message {}))
  ([src folder-name on-message opts] (source/-watch src folder-name on-message opts)))

(defn watch-async
  "Like `watch` but starts in a new daemon thread. Returns the Thread.

  Call `(.interrupt thread)` to stop watching.

  Example:
    (def t (watch-async src \"INBOX\" (fn [msg] (println (:subject msg)))))
    ;; Later:
    (.interrupt t)"
  ([src folder-name on-message]      (watch-async src folder-name on-message {}))
  ([src folder-name on-message opts] (source/-watch-async src folder-name on-message opts)))

(defn by-id-range
  "Fetch messages whose ids fall in the range [start-id, end-id].

  For IMAP, delegates to UID-range fetch — this is the most efficient
  way to implement incremental fetching with a UID watermark.  Pass
  nil as `end-id` to fetch from `start-id` to the latest message.

  For Maildir, filters `list-ids` by lexicographic comparison and
  fetches the matching messages.

  Example (IMAP watermark pattern):
    (by-id-range src \"INBOX\" (str (inc last-uid)) nil)"
  ([src folder-name start-id end-id]
   (by-id-range src folder-name start-id end-id {}))
  ([src folder-name start-id end-id opts]
   (source/-by-id-range src folder-name start-id end-id opts)))

(defn underlying-conn
  "Return the underlying IMAP connection map from an IMAP source, or
  nil for non-IMAP sources.

  The returned map contains `:store` (a `jakarta.mail.Store`) and
  `:session` (a `jakarta.mail.Session`), suitable for passing to
  low-level functions in `mailseq.imap.fetch`, `mailseq.imap.idle`,
  and `mailseq.imap.folder`.

  This is a stable part of the public API — use it when you need
  IMAP-specific operations that the unified API does not cover."
  [src]
  (when (instance? ImapSource src)
    (:conn src)))

(defmacro with-source
  "Bind `sym` to `(open cfg)` for the extent of `body`, ensuring
  `close` runs on exit."
  [[sym cfg] & body]
  `(let [~sym (open ~cfg)]
     (try
       ~@body
       (finally
         (close ~sym)))))
