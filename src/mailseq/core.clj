;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.core
  "IMAP connection management.

  Connect to an IMAP server and obtain a store that can be passed
  to functions in mailseq.folder and mailseq.fetch."
  (:import [jakarta.mail Session Store Folder]
           [java.util Properties]))

(defn- make-properties
  "Build a java.util.Properties from an options map."
  [{:keys [ssl starttls timeout connection-timeout]
    :or   {ssl true timeout 30000 connection-timeout 30000}}]
  (let [protocol (if ssl "imaps" "imap")
        props    (doto (Properties.)
                   (.put "mail.store.protocol" protocol)
                   (.put (str "mail." protocol ".timeout") (str timeout))
                   (.put (str "mail." protocol ".connectiontimeout") (str connection-timeout)))]
    (when (and starttls (not ssl))
      (.put props "mail.imap.starttls.enable" "true"))
    props))

(defn connect
  "Connect to an IMAP server. Returns a map containing :store and :session.

  Options:
    :host    - IMAP server hostname (required)
    :port    - port number (default: 993 for SSL, 143 otherwise)
    :user    - username / email (required)
    :password - password (required)
    :ssl     - use SSL (default: true)
    :starttls - use STARTTLS (default: false, ignored if :ssl is true)
    :timeout  - socket read timeout in ms (default: 30000)
    :connection-timeout - connection timeout in ms (default: 30000)
    :oauth2-token - if provided, authenticate with XOAUTH2 instead of password

  Example:
    (connect {:host \"imap.example.com\"
              :user \"me@example.com\"
              :password \"secret\"})"
  [{:keys [host port user password ssl oauth2-token]
    :or   {ssl true}
    :as   opts}]
  (let [protocol (if ssl "imaps" "imap")
        port     (or port (if ssl 993 143))
        props    (make-properties opts)
        session  (Session/getInstance props)
        store    (.getStore session protocol)]
    (if oauth2-token
      ;; XOAUTH2 authentication
      (.connect store host port user oauth2-token)
      (.connect store host port user password))
    {:store   store
     :session session
     :host    host
     :user    user}))

(defn connected?
  "Returns true if the store is connected."
  [{:keys [^Store store]}]
  (and store (.isConnected store)))

(defn disconnect
  "Disconnect from the IMAP server. Safe to call on already-closed connections."
  [{:keys [^Store store]}]
  (when (and store (.isConnected store))
    (.close store)))

(defmacro with-connection
  "Execute body with a connection, ensuring disconnect on exit.

  Example:
    (with-connection [conn {:host \"imap.example.com\"
                            :user \"me@example.com\"
                            :password \"secret\"}]
      (fetch/messages conn \"INBOX\" {:limit 10}))"
  [[sym opts] & body]
  `(let [~sym (connect ~opts)]
     (try
       ~@body
       (finally
         (disconnect ~sym)))))
