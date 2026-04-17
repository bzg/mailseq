;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.imap.folder
  "IMAP folder operations: list, open, close, and query folders."
  (:import [jakarta.mail Folder Store UIDFolder]))

(defn list-folders
  "List all folders on the server. Returns a vector of maps with keys:
   :name, :full-name, :type (:holds-messages, :holds-folders, :holds-both),
   :message-count, :unread-count.

  Example:
    (list-folders conn)"
  [{:keys [^Store store]}]
  (let [default (.getDefaultFolder store)
        folders (.list default "*")]
    (mapv (fn [^Folder f]
            (let [ftype (.getType f)]
              {:name          (.getName f)
               :full-name     (.getFullName f)
               :type          (cond
                                (pos? (bit-and ftype Folder/HOLDS_MESSAGES))
                                (if (pos? (bit-and ftype Folder/HOLDS_FOLDERS))
                                  :holds-both
                                  :holds-messages)
                                (pos? (bit-and ftype Folder/HOLDS_FOLDERS))
                                :holds-folders
                                :else :unknown)
               :message-count (try (.getMessageCount f) (catch Exception _ -1))
               :unread-count  (try (.getUnreadMessageCount f) (catch Exception _ -1))}))
          folders)))

(defn open-folder
  "Open a folder by name in read-only mode. Returns a jakarta.mail.Folder.

  Example:
    (open-folder conn \"INBOX\")"
  [{:keys [^Store store]} ^String folder-name]
  (let [folder (.getFolder store folder-name)]
    (.open folder Folder/READ_ONLY)
    folder))

(defn close-folder
  "Close a folder. Never expunges (this library is read-only)."
  [^Folder folder]
  (when (.isOpen folder)
    (.close folder false)))

(defn- with-temp-folder*
  "Open `folder-name` read-only for the duration of `f`, then close it.
  If the folder was already open, leaves it open."
  [{:keys [^Store store]} ^String folder-name f]
  (let [folder  (.getFolder store folder-name)
        opened? (not (.isOpen folder))]
    (try
      (when opened?
        (.open folder Folder/READ_ONLY))
      (f folder)
      (finally
        (when (and opened? (.isOpen folder))
          (.close folder false))))))

(defn message-count
  "Return the number of messages in the named folder."
  [conn folder-name]
  (with-temp-folder* conn folder-name
    (fn [^Folder f] (.getMessageCount f))))

(defn unread-count
  "Return the number of unread messages in the named folder."
  [conn folder-name]
  (with-temp-folder* conn folder-name
    (fn [^Folder f] (.getUnreadMessageCount f))))

(defn uid-validity
  "Return the UIDVALIDITY of the named folder as a long.

  UIDVALIDITY is assigned by the server (RFC 3501) and changes when the
  mailbox's UID space is no longer valid — for example after a mailbox
  recreation or a server migration. Callers persisting UIDs as watermarks
  must persist UIDVALIDITY alongside and discard the watermark when the
  value has changed, otherwise a stale watermark can silently skip every
  new message."
  [conn folder-name]
  (with-temp-folder* conn folder-name
    (fn [^Folder f] (.getUIDValidity ^UIDFolder f))))

(defn uid-next
  "Return the folder's UIDNEXT (the UID the next new message will receive),
  or -1 if the server does not report it.

  Useful for diagnosing a stuck watermark: if a caller's stored UID is
  greater than UIDNEXT, UIDVALIDITY has almost certainly changed."
  [conn folder-name]
  (with-temp-folder* conn folder-name
    (fn [^Folder f] (.getUIDNext ^UIDFolder f))))
