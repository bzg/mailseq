;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.imap.folder
  "IMAP folder operations: list, open, close, and query folders."
  (:import [jakarta.mail Folder Store]))

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

(defn message-count
  "Return the number of messages in the named folder."
  [{:keys [^Store store]} ^String folder-name]
  (let [folder (.getFolder store folder-name)]
    (try
      (when-not (.isOpen folder)
        (.open folder Folder/READ_ONLY))
      (.getMessageCount folder)
      (finally
        (when (.isOpen folder)
          (.close folder false))))))

(defn unread-count
  "Return the number of unread messages in the named folder."
  [{:keys [^Store store]} ^String folder-name]
  (let [folder (.getFolder store folder-name)]
    (try
      (when-not (.isOpen folder)
        (.open folder Folder/READ_ONLY))
      (.getUnreadMessageCount folder)
      (finally
        (when (.isOpen folder)
          (.close folder false))))))
