;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq
  "Unified, read-only public API over mail sources.

  A *source* is created from a plain config map via `open`, which
  dispatches on `:type` (`:imap` in v1; `:maildir` and `:mbox` later).
  Every source exposes the same four operations — `list-folders`,
  `messages`, `by-id`, `close` — regardless of its backend.

  Every source owns an explicit `:folders` map from logical folder
  name to backend-specific location. The public API only accepts
  logical folder names; passing an unknown name throws `ex-info` with
  `{:type ::unknown-folder}`.

  Example:

      (with-open [src (mailseq/open
                       {:type :imap
                        :host \"imap.example.com\"
                        :user \"me@example.com\"
                        :password \"secret\"
                        :folders {\"INBOX\"   \"INBOX\"
                                  \"Clojure\" \"Lists/clojure\"}})]
        (mailseq/messages src \"INBOX\" {:limit 10}))"
  (:refer-clojure :exclude [list])
  (:require [mailseq.source :as source]
            [mailseq.core :as imap-core]
            [mailseq.fetch :as imap-fetch]
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

(defn- imap-assoc-id
  "Derive the stable `:id` of an IMAP message from its UID. IMAP UIDs
  are unique within a folder and stable across sessions, which is
  exactly what the common message-map contract requires."
  [m]
  (cond-> m
    (and (nil? (:id m)) (:uid m)) (assoc :id (str (:uid m)))))

(defrecord ImapSource [conn folders]
  source/MailSource
  (-list-folders [_]
    (vec (keys folders)))
  (-messages [_ folder-name opts]
    (mapv imap-assoc-id
          (imap-fetch/messages conn (resolve-folder folders folder-name) opts)))
  (-by-id [_ folder-name id opts]
    (mapv imap-assoc-id
          (imap-fetch/by-uid conn (resolve-folder folders folder-name) id opts)))
  (-close [_]
    (imap-core/disconnect conn))
  Closeable
  (close [this] (source/-close this)))

(defn- open-imap
  [{:keys [folders] :as cfg}]
  (let [conn (imap-core/connect (dissoc cfg :type :folders))]
    (->ImapSource conn (or folders {}))))

;; ---------------------------------------------------------------------------
;; Maildir backend
;; ---------------------------------------------------------------------------

(defrecord MaildirSource [folders]
  source/MailSource
  (-list-folders [_]
    (vec (keys folders)))
  (-messages [_ folder-name opts]
    (maildir/messages (resolve-folder folders folder-name) opts))
  (-by-id [_ folder-name id opts]
    (maildir/by-id (resolve-folder folders folder-name) id opts))
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

  The config must contain a `:type` key (`:imap` in v1) and a
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

  `opts` is the common fetch contract (see plan §14):
    :limit, :since, :before, :from, :to, :cc, :subject, :message-id,
    :headers?, :body?, :attachments?

  Returns a vector of parsed message maps."
  ([src folder-name]      (messages src folder-name {}))
  ([src folder-name opts] (source/-messages src folder-name opts)))

(defn by-id
  "Fetch a single message (or nil) from `folder-name` by its backend
  identifier. For IMAP, `id` is a UID."
  ([src folder-name id]      (by-id src folder-name id {}))
  ([src folder-name id opts] (source/-by-id src folder-name id opts)))

(defmacro with-source
  "Bind `sym` to `(open cfg)` for the extent of `body`, ensuring
  `close` runs on exit."
  [[sym cfg] & body]
  `(let [~sym (open ~cfg)]
     (try
       ~@body
       (finally
         (close ~sym)))))
