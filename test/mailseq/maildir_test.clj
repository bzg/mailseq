;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.maildir-test
  "Integration tests for the Maildir backend, driven against a real
  committed fixture under dev-resources/maildir/."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [mailseq :as mailseq]
            [mailseq.maildir :as maildir]))

(def ^:private fixture-path
  (.getAbsolutePath (io/file "dev-resources/maildir/inbox")))

;; ---------------------------------------------------------------------------
;; Flag decoding (private)
;; ---------------------------------------------------------------------------

(def ^:private parse-flags (deref #'maildir/parse-flags))
(def ^:private stable-id   (deref #'maildir/stable-id))

(deftest parse-flags-standard
  (is (= #{:seen}             (parse-flags "123.M1.host:2,S")))
  (is (= #{:seen :flagged}    (parse-flags "123.M1.host:2,FS")))
  (is (= #{:answered :draft}  (parse-flags "123.M1.host:2,DR")))
  (testing "no suffix"
    (is (= #{} (parse-flags "123.M1.host"))))
  (testing "unknown letters are ignored"
    (is (= #{:seen} (parse-flags "123.M1.host:2,PS"))))
  (testing "empty flag string"
    (is (= #{} (parse-flags "123.M1.host:2,")))))

(deftest stable-id-strips-flag-suffix
  (is (= "123.M1.host" (stable-id "123.M1.host:2,S")))
  (is (= "123.M1.host" (stable-id "123.M1.host")))
  (is (= "123.M1.host" (stable-id "123.M1.host:2,"))))

;; ---------------------------------------------------------------------------
;; Reading the committed fixture
;; ---------------------------------------------------------------------------

(deftest reads-committed-fixture
  (let [msgs (maildir/messages fixture-path {})]
    (is (= 2 (count msgs)))
    (testing "every message has an id and a :flags set"
      (is (every? :id msgs))
      (is (every? #(set? (:flags %)) msgs)))
    (testing "uid is nil for Maildir"
      (is (every? #(nil? (:uid %)) msgs)))
    (testing "cur/ message carries the Seen flag"
      (let [seen-msg (first (filter #(contains? (:flags %) :seen) msgs))]
        (is (some? seen-msg))
        (is (= "Plain text only" (:subject seen-msg)))))
    (testing "new/ message has no flags"
      (let [unseen (first (filter #(empty? (:flags %)) msgs))]
        (is (some? unseen))
        (is (= "Test message with écents" (:subject unseen)))))))

(deftest validates-opts
  (is (thrown? clojure.lang.ExceptionInfo
               (maildir/messages fixture-path {:bogus true}))))

(deftest filters-from
  (let [msgs (maildir/messages fixture-path {:from "alice"})]
    (is (= 1 (count msgs)))
    (is (= "<test-001@example.com>" (:message-id (first msgs))))))

(deftest filters-date-range
  (testing "only the 2025-01-15 message matches"
    (let [msgs (maildir/messages fixture-path
                                 {:since "2025-01-01" :before "2025-01-31"})]
      (is (= 1 (count msgs)))
      (is (= "<test-002@example.com>" (:message-id (first msgs)))))))

(deftest limit-keeps-most-recent
  (let [msgs (maildir/messages fixture-path {:limit 1})]
    (is (= 1 (count msgs)))
    (testing "most recent by :date-sent is the 2025-02-10 message"
      (is (= "<test-001@example.com>" (:message-id (first msgs)))))))

(deftest by-id-stable-across-flags
  (testing "Matching by the stable prefix finds the cur/ file even though its filename has a flag suffix"
    (let [m (maildir/by-id fixture-path "1700000001.M1.host" {})]
      (is (some? m))
      (is (= "<test-002@example.com>" (:message-id m)))
      (is (contains? (:flags m) :seen))))
  (testing "unknown id returns nil"
    (is (nil? (maildir/by-id fixture-path "does-not-exist" {})))))

(deftest invalid-maildir-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (maildir/messages "/tmp/definitely-not-a-maildir-xyz" {}))))

;; ---------------------------------------------------------------------------
;; mailseq public API plugged to the Maildir backend
;; ---------------------------------------------------------------------------

(deftest public-api-open-and-query
  (let [src (mailseq/open {:type :maildir :folders {"INBOX" fixture-path}})]
    (try
      (is (= ["INBOX"] (mailseq/list-folders src)))
      (is (= 2 (count (mailseq/messages src "INBOX"))))
      (testing "unknown logical folder"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown folder"
                              (mailseq/messages src "nope"))))
      (finally
        (mailseq/close src)))))

(deftest with-source-macro
  (mailseq/with-source [src {:type :maildir :folders {"INBOX" fixture-path}}]
    (is (= 2 (count (mailseq/messages src "INBOX" {}))))))

(deftest unsupported-type
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported source type"
                        (mailseq/open {:type :pop3}))))
