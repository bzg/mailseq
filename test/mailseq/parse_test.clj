;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.parse-test
  "Unit tests for mailseq.parse.

  These tests use .eml files loaded as MimeMessage objects — no IMAP
  server required."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [mailseq.parse :as parse])
  (:import [jakarta.mail Session]
           [jakarta.mail.internet MimeMessage]
           [java.util Properties]))

(defn- load-eml
  "Load an .eml file from the classpath as a MimeMessage."
  [resource-path]
  (let [session (Session/getInstance (Properties.))
        stream  (io/input-stream (io/resource resource-path))]
    (MimeMessage. session stream)))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(deftest parse-multipart-message
  (let [msg (load-eml "emails/simple-multipart.eml")
        m   (parse/message->map msg)]
    (testing "envelope fields"
      (is (= "<test-001@example.com>" (:message-id m)))
      (is (= "Test message with écents" (:subject m)))
      (is (= [{:name "Alice Test" :address "alice@example.com"}]
             (:from m)))
      (is (= [{:name "Bob Test" :address "bob@example.com"}]
             (:to m)))
      (is (= [{:name "Carol Test" :address "carol@example.com"}]
             (:cc m))))

    (testing "body content"
      (is (some? (get-in m [:body :text])))
      (is (some? (get-in m [:body :html])))
      (is (clojure.string/includes? (get-in m [:body :text]) "plain text test"))
      (is (clojure.string/includes? (get-in m [:body :html]) "<b>HTML</b>")))

    (testing "no attachments in this message"
      (is (empty? (get-in m [:body :attachments]))))))

(deftest parse-plain-text-message
  (let [msg (load-eml "emails/plain-text.eml")
        m   (parse/message->map msg)]
    (testing "envelope fields"
      (is (= "<test-002@example.com>" (:message-id m)))
      (is (= "Plain text only" (:subject m)))
      (is (= [{:name nil :address "sender@example.com"}]
             (:from m))))

    (testing "body is plain text only"
      (is (some? (get-in m [:body :text])))
      (is (nil? (get-in m [:body :html])))
      (is (clojure.string/includes? (get-in m [:body :text]) "simple plain text")))))

(deftest parse-options
  (let [msg (load-eml "emails/simple-multipart.eml")]
    (testing "skip headers"
      (let [m (parse/message->map msg {:headers? false})]
        (is (nil? (:headers m)))
        (is (some? (:subject m)))))

    (testing "skip body"
      (let [m (parse/message->map msg {:body? false})]
        (is (nil? (:body m)))
        (is (some? (:subject m)))))

    (testing "skip attachments"
      (let [m (parse/message->map msg {:attachments? false})]
        (is (nil? (get-in m [:body :attachments])))
        (is (some? (get-in m [:body :text])))))))

(deftest headers-extraction
  (let [msg (load-eml "emails/simple-multipart.eml")
        m   (parse/message->map msg)]
    (testing "headers map is populated"
      (is (map? (:headers m)))
      (is (some? (get (:headers m) "Message-ID")))
      (is (some? (get (:headers m) "From"))))))
