;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.idle
  "IMAP IDLE support for receiving push notifications when new messages arrive.

  IDLE keeps a connection open and the server notifies the client when
  the folder state changes (new messages, deletions, flag changes).

  A heartbeat thread periodically breaks out of IDLE to force a NOOP
  roundtrip, preventing NATs/firewalls/servers from killing the connection."
  (:require [mailseq.folder :as folder]
            [mailseq.parse :as parse])
  (:import [jakarta.mail Folder Store MessagingException]
           [jakarta.mail.event MessageCountListener MessageCountEvent]
           [org.eclipse.angus.mail.imap IMAPFolder]))

(defn- make-message-listener
  "Create a MessageCountListener that calls on-added when new messages arrive."
  [on-added parse-opts]
  (reify MessageCountListener
    (messagesAdded [_ event]
      (let [msgs (.getMessages ^MessageCountEvent event)]
        (doseq [msg msgs]
          (try
            (on-added (parse/message->map msg parse-opts))
            (catch Exception e
              (println "Error processing new message:" (.getMessage e)))))))
    (messagesRemoved [_ _event]
      ;; We don't act on removals in a read-only library
      nil)))

(defn- start-heartbeat
  "Start a daemon thread that sends a NOOP to the folder at regular intervals
  by calling .doCommand, which forces the server to break out of IDLE.
  Returns the Thread."
  [^IMAPFolder imap-folder ^Store store interval-ms on-error]
  (let [t (Thread.
           (fn []
             (while (and (not (Thread/interrupted))
                         (.isOpen imap-folder)
                         (.isConnected store))
               (try
                 (Thread/sleep interval-ms)
                 ;; doCommand with a NOOP forces the folder to exit IDLE,
                 ;; do a server roundtrip, then IDLE will be re-entered
                 ;; by the main loop.
                 (when (and (.isOpen imap-folder)
                            (.isConnected store))
                   (.doCommand imap-folder
                               (reify org.eclipse.angus.mail.imap.IMAPFolder$ProtocolCommand
                                 (doCommand [_ protocol]
                                   (.simpleCommand protocol "NOOP" nil)
                                   nil))))
                 (catch InterruptedException _
                   nil)
                 (catch Exception e
                   (on-error e)))))
           "mailseq-heartbeat")]
    (.setDaemon t true)
    (.start t)
    t))

(defn idle
  "Start an IDLE loop on a folder, calling on-message for each new message.

  This function blocks the current thread. A heartbeat thread periodically
  breaks IDLE and forces a NOOP roundtrip to keep the connection alive.

  Options:
    :parse-opts    - options to pass to message->map (default: {})
    :on-error      - function called with Exception on errors (default: prints)
    :heartbeat-ms  - interval between NOOP heartbeats in ms
                     (default: 1200000 = 20 minutes)

  Returns nil when the folder or store is closed, or when the thread
  is interrupted.

  Example:
    (future
      (idle conn \"INBOX\"
            (fn [msg] (println \"New:\" (:subject msg)))
            {:parse-opts {:attachments? false}
             :heartbeat-ms 900000}))"
  ([conn folder-name on-message] (idle conn folder-name on-message {}))
  ([conn folder-name on-message {:keys [parse-opts on-error heartbeat-ms]
                                 :or   {parse-opts   {}
                                        on-error     #(println "IDLE error:" (.getMessage %))
                                        heartbeat-ms 1200000}}]
   (let [folder       (folder/open-folder conn folder-name)
         imap-folder  ^IMAPFolder folder
         ^Store store (:store conn)
         listener     (make-message-listener on-message parse-opts)
         heartbeat    (start-heartbeat imap-folder store heartbeat-ms on-error)]
     (.addMessageCountListener folder listener)
     (try
       (loop []
         (when (and (.isOpen folder)
                    (.isConnected store)
                    (not (Thread/interrupted)))
           (try
             ;; IDLE command — blocks until:
             ;; - server sends a notification (new mail, expunge, etc.)
             ;; - heartbeat thread sends a NOOP (breaking IDLE)
             ;; - connection dies
             (.idle imap-folder)
             (catch jakarta.mail.FolderClosedException _
               nil)  ;; Exit the loop
             (catch jakarta.mail.MessagingException e
               (if (re-find #"(?i)IDLE not supported" (or (.getMessage e) ""))
                 ;; Server doesn't support IDLE — fall back to sleep + NOOP
                 (do (Thread/sleep heartbeat-ms)
                     (.getMessageCount folder))
                 (do (on-error e)
                     (Thread/sleep 5000))))
             (catch Exception e
               (on-error e)
               (Thread/sleep 5000)))
           (recur)))
       (finally
         (.interrupt heartbeat)
         (.removeMessageCountListener folder listener)
         (folder/close-folder folder))))))

(defn idle-async
  "Start IDLE in a new daemon thread. Returns the Thread object.

  Call (.interrupt thread) to stop the IDLE loop.

  Example:
    (def idle-thread
      (idle-async conn \"INBOX\"
                  (fn [msg] (println \"New:\" (:subject msg)))))
    ;; Later:
    (.interrupt idle-thread)"
  ([conn folder-name on-message] (idle-async conn folder-name on-message {}))
  ([conn folder-name on-message opts]
   (let [t (Thread. ^Runnable (fn [] (idle conn folder-name on-message opts))
                    (str "mailseq-idle-" folder-name))]
     (.setDaemon t true)
     (.start t)
     t)))
