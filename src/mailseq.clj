;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq
  "Unified, read-only public API over mail sources.

  A *source* is created from a plain config map via `open`, which
  dispatches on `:type` (`:imap` or `:maildir`).
  Every source exposes the same four operations — `list-folders`,
  `messages`, `by-id`, `close` — regardless of its backend.

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
            [mailseq.maildir :as maildir])
  (:import [java.io Closeable]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

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
;; IMAP backend
;; ---------------------------------------------------------------------------

(defrecord ImapSource [conn folders]
  source/MailSource
  (-list-folders [_]
    (vec (keys folders)))
  (-list-ids [_ folder-name]
    (imap-fetch/list-uids conn (resolve-folder folders folder-name)))
  (-messages [_ folder-name opts]
    (imap-fetch/messages conn (resolve-folder folders folder-name) opts))
  (-by-id [_ folder-name id opts]
    ;; IMAP ids are UIDs (or UID strings). imap-fetch/by-uid accepts
    ;; both a scalar and a collection and always returns a vector; we
    ;; take the first element to honour the single-message contract.
    (let [uid (if (string? id) (Long/parseLong id) id)]
      (first (imap-fetch/by-uid conn (resolve-folder folders folder-name) uid opts))))
  (-by-ids [_ folder-name ids opts]
    (let [uids (mapv #(if (string? %) (Long/parseLong %) %) ids)]
      (imap-fetch/by-uid conn (resolve-folder folders folder-name) uids opts)))
  (-close [_]
    (imap-connect/disconnect conn))
  Closeable
  (close [this] (source/-close this)))

(defn- open-imap
  [{:keys [folders] :as cfg}]
  (let [conn (imap-connect/connect (dissoc cfg :type :folders))]
    (->ImapSource conn (or folders {}))))

;; ---------------------------------------------------------------------------
;; Maildir backend
;; ---------------------------------------------------------------------------

(defrecord MaildirSource [folders]
  source/MailSource
  (-list-folders [_]
    (vec (keys folders)))
  (-list-ids [_ folder-name]
    (maildir/list-ids (resolve-folder folders folder-name)))
  (-messages [_ folder-name opts]
    (maildir/messages (resolve-folder folders folder-name) opts))
  (-by-id [_ folder-name id opts]
    (maildir/by-id (resolve-folder folders folder-name) id opts))
  (-by-ids [_ folder-name ids opts]
    (maildir/by-ids (resolve-folder folders folder-name) ids opts))
  (-close [_] nil)
  Closeable
  (close [this] (source/-close this)))

(defn- open-maildir
  [{:keys [folders]}]
  (->MaildirSource (or folders {})))

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

(defmacro with-source
  "Bind `sym` to `(open cfg)` for the extent of `body`, ensuring
  `close` runs on exit."
  [[sym cfg] & body]
  `(let [~sym (open ~cfg)]
     (try
       ~@body
       (finally
         (close ~sym)))))
