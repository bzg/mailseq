;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.parse
  "Parse Jakarta Mail Message objects into Clojure maps.

  This is the core of the library: robust handling of MIME multipart
  messages, encoded headers, attachments, and charset variations."
  (:require [clojure.string :as str])
  (:import [jakarta.mail Message Message$RecipientType Part]
           [jakarta.mail.internet InternetAddress MimeMessage MimeMultipart MimeUtility]
           [java.io InputStream ByteArrayOutputStream]))

;; ---------------------------------------------------------------------------
;; Address parsing
;; ---------------------------------------------------------------------------

(defn- parse-address
  "Convert an InternetAddress to a map."
  [^InternetAddress addr]
  (when addr
    {:name    (.getPersonal addr)
     :address (.getAddress addr)}))

(defn- parse-addresses
  "Convert an array of Address objects to a vector of maps."
  [addrs]
  (when addrs
    (mapv (fn [addr] (parse-address addr)) addrs)))

;; ---------------------------------------------------------------------------
;; Header parsing
;; ---------------------------------------------------------------------------

(defn- decode-header
  "Decode RFC 2047 encoded header value."
  [^String s]
  (when s
    (try
      (MimeUtility/decodeText s)
      (catch Exception _ s))))

(defn- get-all-headers
  "Extract all headers as a map. Multi-valued headers become vectors."
  [^MimeMessage msg]
  (let [hdrs (.getAllHeaders msg)]
    (loop [result (transient {})]
      (if (.hasMoreElements hdrs)
        (let [h    (.nextElement hdrs)
              name (.getName h)
              val  (decode-header (.getValue h))
              prev (get result name)]
          (recur (assoc! result name
                         (cond
                           (nil? prev)    val
                           (vector? prev) (conj prev val)
                           :else          [prev val]))))
        (persistent! result)))))

;; ---------------------------------------------------------------------------
;; Body / MIME part parsing
;; ---------------------------------------------------------------------------

(defn- input-stream->bytes
  "Read an InputStream into a byte array."
  [^InputStream is]
  (let [baos (ByteArrayOutputStream.)]
    (let [buf (byte-array 8192)]
      (loop []
        (let [n (.read is buf)]
          (when (pos? n)
            (.write baos buf 0 n)
            (recur)))))
    (.toByteArray baos)))

(defn- content-type-base
  "Extract the base MIME type from a content-type string, e.g.
  \"text/plain; charset=utf-8\" -> \"text/plain\"."
  [^String ct]
  (when ct
    (-> ct (str/split #";\s*") first str/lower-case str/trim)))

(defn- text-part?
  "Is this part a text/* content type?"
  [^Part part]
  (try
    (let [ct (content-type-base (.getContentType part))]
      (and ct (str/starts-with? ct "text/")))
    (catch Exception _ false)))

(defn- attachment?
  "Is this MIME part an attachment?"
  [^Part part]
  (try
    (let [disp (.getDisposition part)]
      (or (= Part/ATTACHMENT disp)
          ;; Some mailers set inline for actual attachments with filenames
          (and (= Part/INLINE disp)
               (some? (.getFileName part))
               (not (text-part? part)))))
    (catch Exception _ false)))

(defn- parse-text-content
  "Safely extract text content from a Part."
  [^Part part]
  (try
    (let [content (.getContent part)]
      (cond
        (string? content)              content
        (instance? InputStream content) (String. (input-stream->bytes content) "UTF-8")
        :else                          (str content)))
    (catch Exception _ nil)))

(defn- parse-attachment
  "Parse an attachment Part into a map."
  [^Part part]
  (try
    {:filename     (decode-header (.getFileName part))
     :content-type (content-type-base (.getContentType part))
     :size         (.getSize part)
     :data         (input-stream->bytes (.getInputStream part))}
    (catch Exception e
      {:filename     (try (decode-header (.getFileName part)) (catch Exception _ nil))
       :content-type (try (content-type-base (.getContentType part)) (catch Exception _ nil))
       :error        (.getMessage e)})))

(declare walk-parts)

(defn- walk-multipart
  "Recursively walk a MimeMultipart, collecting text bodies and attachments."
  [^MimeMultipart mp result]
  (let [cnt (.getCount mp)]
    (loop [i 0 res result]
      (if (< i cnt)
        (recur (inc i) (walk-parts (.getBodyPart mp i) res))
        res))))

(defn- walk-parts
  "Walk a MIME Part tree, accumulating :text, :html, and :attachments."
  [^Part part result]
  (let [ct (content-type-base (.getContentType part))]
    (cond
      ;; Attachment - any disposition=attachment or inline with filename (non-text)
      (attachment? part)
      (update result :attachments (fnil conj []) (parse-attachment part))

      ;; Multipart container - recurse
      (and ct (str/starts-with? ct "multipart/"))
      (let [content (try (.getContent part) (catch Exception _ nil))]
        (if (instance? MimeMultipart content)
          (walk-multipart content result)
          result))

      ;; Plain text body
      (= ct "text/plain")
      (let [txt (parse-text-content part)]
        (if (and txt (not (:text result)))
          (assoc result :text txt)
          result))

      ;; HTML body
      (= ct "text/html")
      (let [html (parse-text-content part)]
        (if (and html (not (:html result)))
          (assoc result :html html)
          result))

      ;; Other text/* types (e.g. text/calendar) - store as attachment-like
      (and ct (str/starts-with? ct "text/"))
      (update result :attachments (fnil conj [])
              {:filename     (decode-header (.getFileName part))
               :content-type ct
               :data         (.getBytes (or (parse-text-content part) "") "UTF-8")})

      ;; Binary inline content without attachment disposition
      :else
      (try
        (update result :attachments (fnil conj [])
                {:filename     (decode-header (.getFileName part))
                 :content-type ct
                 :size         (.getSize part)
                 :data         (input-stream->bytes (.getInputStream part))})
        (catch Exception _ result)))))

(defn- parse-body
  "Parse the body of a MimeMessage, returning a map with :text, :html,
  and :attachments keys."
  [^MimeMessage msg]
  (walk-parts msg {:text nil :html nil :attachments []}))

;; ---------------------------------------------------------------------------
;; Message UID
;; ---------------------------------------------------------------------------

(defn- get-uid
  "Get the UID of a message from its folder, or nil if unavailable."
  [^Message msg]
  (try
    (let [folder (.getFolder msg)]
      (when (instance? jakarta.mail.UIDFolder folder)
        (.getUID ^jakarta.mail.UIDFolder folder msg)))
    (catch Exception _ nil)))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn message->map
  "Convert a Jakarta Mail Message into a Clojure map.

  Returns a map with keys:
    :uid          - IMAP UID (long, or nil if unavailable)
    :message-id   - Message-ID header
    :message-number - sequence number in folder
    :size         - message size in bytes (-1 if unknown)
    :from         - vector of {:name :address} maps
    :to           - vector of {:name :address} maps
    :cc           - vector of {:name :address} maps
    :bcc          - vector of {:name :address} maps
    :reply-to     - vector of {:name :address} maps
    :subject      - decoded subject string
    :date-sent    - java.util.Date
    :date-received - java.util.Date
    :content-type - raw content-type string
    :body         - map with :text, :html, and :attachments
    :headers      - map of all headers
    :flags        - set of keyword flags (:seen :answered :flagged :deleted :draft :recent)

  Options (optional second argument):
    :headers?     - include all headers (default: true)
    :body?        - parse body content (default: true)
    :attachments? - include attachment data (default: true)

  Example:
    (message->map msg)
    (message->map msg {:headers? false :attachments? false})"
  ([^Message msg] (message->map msg {}))
  ([^Message msg {:keys [headers? body? attachments?]
                  :or   {headers? true body? true attachments? true}}]
   (let [mime-msg  ^MimeMessage msg
         safe      (fn [f] (try (f) (catch Exception _ nil)))
         body-data (when body?
                     (safe #(parse-body mime-msg)))
         flags     ^jakarta.mail.Flags (safe #(.getFlags msg))]
     (cond-> {:uid            (get-uid msg)
              :message-id     (safe #(.getMessageID mime-msg))
              :message-number (safe #(.getMessageNumber msg))
              :size           (or (safe #(.getSize msg)) -1)
              :from           (safe #(parse-addresses (.getFrom msg)))
              :to             (safe #(parse-addresses (.getRecipients msg Message$RecipientType/TO)))
              :cc             (safe #(parse-addresses (.getRecipients msg Message$RecipientType/CC)))
              :bcc            (safe #(parse-addresses (.getRecipients msg Message$RecipientType/BCC)))
              :reply-to       (safe #(parse-addresses (.getReplyTo msg)))
              :subject        (safe #(decode-header (.getSubject msg)))
              :date-sent      (safe #(.getSentDate msg))
              :date-received  (safe #(.getReceivedDate msg))
              :content-type   (safe #(.getContentType msg))
              :flags          (if flags
                                (cond-> #{}
                                  (.contains flags jakarta.mail.Flags$Flag/SEEN)     (conj :seen)
                                  (.contains flags jakarta.mail.Flags$Flag/ANSWERED) (conj :answered)
                                  (.contains flags jakarta.mail.Flags$Flag/FLAGGED)  (conj :flagged)
                                  (.contains flags jakarta.mail.Flags$Flag/DELETED)  (conj :deleted)
                                  (.contains flags jakarta.mail.Flags$Flag/DRAFT)    (conj :draft)
                                  (.contains flags jakarta.mail.Flags$Flag/RECENT)   (conj :recent))
                                #{})}

       body?
       (assoc :body (if attachments?
                      body-data
                      (dissoc body-data :attachments)))

       headers?
       (assoc :headers (or (safe #(get-all-headers mime-msg)) {}))))))
