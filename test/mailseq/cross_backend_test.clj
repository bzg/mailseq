;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.cross-backend-test
  "The same set of `.eml` files served through every backend must yield
  identical message maps, modulo a small set of keys that are
  legitimately backend-specific. This is the regression harness that
  locks the common message-map contract as we add new backends.

  Three fixtures are covered: a plain-text mail, a multipart/alternative
  mail with HTML + accented characters, and a multipart/mixed mail with
  a text attachment. Together they exercise the three shapes the
  parser actually encounters in the wild."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.java.io :as io]
            [mailseq :as mailseq])
  (:import [com.icegreen.greenmail.util GreenMail ServerSetup]
           [com.icegreen.greenmail.user GreenMailUser]
           [jakarta.mail.internet MimeMessage]
           [jakarta.mail Session]
           [java.io ByteArrayInputStream]
           [java.nio.file Files]
           [java.util Properties]))

;; ---------------------------------------------------------------------------
;; Shared fixtures
;; ---------------------------------------------------------------------------

(def ^:private fixtures
  "Seq of [message-id file] pairs delivered to both backends."
  [["<test-002@example.com>" (io/file "dev-resources/emails/plain-text.eml")]
   ["<test-001@example.com>" (io/file "dev-resources/emails/simple-multipart.eml")]
   ["<test-003@example.com>" (io/file "dev-resources/emails/with-attachment.eml")]])

(def ^:private imap-user "test@example.com")
(def ^:private imap-pass "secret")

;; Keys whose value is allowed to differ between backends.
;; - :uid, :id, :date-received : explicitly declared as backend-specific.
;; - :headers, :size            : IMAP servers rewrite/append headers and
;;                                may report a size that differs from the
;;                                raw file length after re-encoding.
;; - :message-number            : IMAP sequence number, meaningless elsewhere.
;; - :content-type              : the GreenMail IMAP server drops the quotes
;;                                around the `boundary=` parameter, producing
;;                                a lexically different but semantically
;;                                equivalent header.
(def ^:private vary-keys
  #{:uid :id :date-received :headers :size :message-number :content-type})

;; Attachment `:size` is reported by jakarta.mail from the server and
;; may differ from the byte length returned by a direct file read,
;; so we drop it from attachment comparison. `:data` is a byte array,
;; which does not compare structurally with `=`; we wrap it in a vec
;; so equality falls back on element-wise comparison.
(defn- normalize-attachment [a]
  (-> a
      (dissoc :size)
      (update :data #(when % (vec %)))))

(defn- normalize-body [body]
  (when body
    (update body :attachments
            #(when % (mapv normalize-attachment %)))))

(defn- normalize [m]
  (-> (apply dissoc m vary-keys)
      ;; :recent is an IMAP-only flag with no Maildir equivalent.
      (update :flags (fnil disj #{}) :recent)
      (update :body normalize-body)))

;; ---------------------------------------------------------------------------
;; GreenMail harness
;; ---------------------------------------------------------------------------

(def ^:dynamic *imap-port* nil)

(defn- deliver-fixture! [^GreenMailUser user ^Session sess ^java.io.File f]
  (let [bytes (Files/readAllBytes (.toPath f))
        msg   (MimeMessage. sess (ByteArrayInputStream. bytes))]
    (.deliver user msg)))

(defn- with-greenmail [f]
  (let [setup (ServerSetup. 0 "127.0.0.1" "imap")
        gm    (doto (GreenMail. setup) .start)]
    (try
      (let [user (.createUser (.getUserManager gm) imap-user imap-user imap-pass)
            sess (Session/getInstance (Properties.))]
        (doseq [[_ file] fixtures]
          (deliver-fixture! user sess file))
        (binding [*imap-port* (.. gm getImap getServerSetup getPort)]
          (f)))
      (finally
        (.stop gm)))))

(use-fixtures :once with-greenmail)

;; ---------------------------------------------------------------------------
;; Maildir fixture (temp dir so the test is hermetic)
;; ---------------------------------------------------------------------------

(defn- make-maildir-fixture
  "Create a throwaway Maildir and copy every fixture into cur/."
  ^String []
  (let [root (Files/createTempDirectory
              "mailseq-cross-"
              (into-array java.nio.file.attribute.FileAttribute []))
        cur  (io/file (.toFile root) "cur")]
    (.mkdirs cur)
    (.mkdirs (io/file (.toFile root) "new"))
    (doseq [[i [_ file]] (map-indexed vector fixtures)]
      (io/copy file
               (io/file cur (format "17000000%02d.M%d.host:2,S" i i))))
    (.getAbsolutePath (.toFile root))))

(defn- index-by-message-id [messages]
  (into {} (map (juxt :message-id identity)) messages))

;; ---------------------------------------------------------------------------
;; The contract test
;; ---------------------------------------------------------------------------

(deftest imap-and-maildir-agree-on-message-map
  (let [maildir-path (make-maildir-fixture)]
    (mailseq/with-source [imap-src {:type :imap
                                    :host "localhost"
                                    :port *imap-port*
                                    :ssl false
                                    :user imap-user
                                    :password imap-pass
                                    :folders {"INBOX" "INBOX"}}]
      (mailseq/with-source [md-src {:type :maildir
                                    :folders {"INBOX" maildir-path}}]
        (let [imap-by-id (index-by-message-id (mailseq/messages imap-src "INBOX"))
              md-by-id   (index-by-message-id (mailseq/messages md-src  "INBOX"))]

          (testing "every fixture round-trips through both backends"
            (is (= (count fixtures) (count imap-by-id)))
            (is (= (count fixtures) (count md-by-id))))

          (doseq [[mid _] fixtures]
            (testing (str "fixture " mid)
              (let [m-imap (imap-by-id mid)
                    m-md   (md-by-id   mid)]
                (is (some? m-imap) "present on IMAP")
                (is (some? m-md)   "present on Maildir")

                (testing "normalized maps are strictly equal"
                  (is (= (normalize m-imap) (normalize m-md))))

                (testing "stable backend ids are present"
                  (is (some? (:id m-imap)))
                  (is (some? (:id m-md))))

                (testing "uid is IMAP-only"
                  (is (nil?  (:uid m-md)))
                  (is (some? (:uid m-imap))))))))))))

;; ---------------------------------------------------------------------------
;; by-id returns a single message or nil (not a vector)
;; ---------------------------------------------------------------------------

(deftest by-id-returns-single-message-imap
  (mailseq/with-source [src {:type :imap
                             :host "localhost"
                             :port *imap-port*
                             :ssl false
                             :user imap-user
                             :password imap-pass
                             :folders {"INBOX" "INBOX"}}]
    (let [all (mailseq/messages src "INBOX")
          one (first all)
          id  (:id one)
          fetched (mailseq/by-id src "INBOX" id)]
      (is (map? fetched) "by-id must return a map, not a vector")
      (is (= (:message-id one) (:message-id fetched)))
      (testing "unknown id returns nil"
        (is (nil? (mailseq/by-id src "INBOX" "999999999")))))))

(deftest by-id-returns-single-message-maildir
  (let [maildir-path (make-maildir-fixture)]
    (mailseq/with-source [src {:type :maildir :folders {"INBOX" maildir-path}}]
      (let [all (mailseq/messages src "INBOX")
            one (first all)
            id  (:id one)
            fetched (mailseq/by-id src "INBOX" id)]
        (is (map? fetched) "by-id must return a map, not a vector")
        (is (= (:message-id one) (:message-id fetched)))
        (testing "unknown id returns nil"
          (is (nil? (mailseq/by-id src "INBOX" "nope"))))))))
