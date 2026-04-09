;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.cross-backend-test
  "The same `.eml` served through every backend must yield identical
  message maps, modulo a small set of keys that are legitimately
  backend-specific. This is the regression harness that locks the
  common message-map contract as we add new backends."
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
;; Shared fixture
;; ---------------------------------------------------------------------------

(def ^:private fixture-file
  (io/file "dev-resources/emails/simple-multipart.eml"))

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

(defn- normalize [m]
  (-> (apply dissoc m vary-keys)
      ;; :recent is an IMAP-only flag with no Maildir equivalent.
      (update :flags (fnil disj #{}) :recent)))

;; ---------------------------------------------------------------------------
;; GreenMail harness
;; ---------------------------------------------------------------------------

(def ^:dynamic *greenmail* nil)
(def ^:dynamic *imap-port* nil)

(defn- with-greenmail [f]
  (let [setup (ServerSetup. 0 "127.0.0.1" "imap")
        gm    (doto (GreenMail. setup) .start)]
    (try
      (let [user (.createUser (.getUserManager gm) imap-user imap-user imap-pass)
            sess (Session/getInstance (Properties.))
            bytes (Files/readAllBytes (.toPath fixture-file))
            msg  (MimeMessage. sess (ByteArrayInputStream. bytes))]
        (.deliver ^GreenMailUser user msg)
        (binding [*greenmail* gm
                  *imap-port* (.. gm getImap getServerSetup getPort)]
          (f)))
      (finally
        (.stop gm)))))

(use-fixtures :once with-greenmail)

;; ---------------------------------------------------------------------------
;; Maildir fixture (temp dir so the test is hermetic)
;; ---------------------------------------------------------------------------

(defn- make-maildir-fixture
  "Create a throwaway Maildir with the shared .eml copied into cur/."
  ^String []
  (let [root (Files/createTempDirectory "mailseq-cross-" (into-array java.nio.file.attribute.FileAttribute []))
        cur  (io/file (.toFile root) "cur")
        new  (io/file (.toFile root) "new")]
    (.mkdirs cur)
    (.mkdirs new)
    (io/copy fixture-file (io/file cur "1700000000.M1.host:2,S"))
    (.getAbsolutePath (.toFile root))))

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
        (let [m-imap (first (mailseq/messages imap-src "INBOX"))
              m-md   (first (mailseq/messages md-src  "INBOX"))]
          (is (some? m-imap) "IMAP delivered the test message")
          (is (some? m-md)   "Maildir scanned the test message")

          (testing "contract keys declared stable are equal across backends"
            (is (= (normalize m-imap) (normalize m-md))))

          (testing "backend-specific keys are present but may differ"
            (is (some? (:id m-imap)))
            (is (some? (:id m-md)))
            (is (nil? (:uid m-md))    "Maildir has no UID")
            (is (some? (:uid m-imap)) "IMAP messages carry a UID"))

          (testing "spot-checks on the shared shape"
            (is (= "<test-001@example.com>" (:message-id m-imap)))
            (is (= "<test-001@example.com>" (:message-id m-md)))
            (is (= [{:name "Alice Test" :address "alice@example.com"}]
                   (:from m-imap)))
            (is (= (:from m-imap) (:from m-md)))
            (is (= (:to m-imap)   (:to m-md)))
            (is (= (:cc m-imap)   (:cc m-md)))
            (is (= (:subject m-imap) (:subject m-md)))
            (is (= (:date-sent m-imap) (:date-sent m-md)))
            (is (= (:body m-imap) (:body m-md)))))))))
