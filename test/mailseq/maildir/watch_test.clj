;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.maildir.watch-test
  "Integration test for the Maildir filesystem watch."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [mailseq :as mailseq]
            [mailseq.maildir.watch :as watch]
            [mailseq.test-util :as tu]))

(def ^:private fixture-eml
  (io/file "dev-resources/emails/plain-text.eml"))

(deftest watch-detects-new-message
  (let [maildir-path (tu/make-empty-maildir "mailseq-watch-")
        received     (promise)
        watcher      (watch/watch-async
                      maildir-path
                      (fn [msg] (deliver received msg))
                      {:settle-ms 20})]
    (try
      ;; Give the WatchService time to register
      (Thread/sleep 200)
      ;; Deliver a message into new/ (simulating an MDA)
      (io/copy fixture-eml
               (io/file maildir-path "new" "1700099999.M1.host"))
      ;; Wait for the callback
      (let [msg (deref received 5000 :timeout)]
        (is (not= :timeout msg) "on-message should have been called")
        (when (not= :timeout msg)
          (is (= "<test-002@example.com>" (:message-id msg)))
          (is (= "Plain text only" (:subject msg)))
          (is (string? (:id msg)))))
      (finally
        (.interrupt watcher)))))

(deftest watch-ignores-existing-messages
  (let [maildir-path (tu/make-empty-maildir "mailseq-watch-")
        ;; Pre-populate cur/ before starting the watch
        _            (io/copy fixture-eml
                              (io/file maildir-path "cur" "1700000001.M1.host:2,S"))
        call-count   (atom 0)
        watcher      (watch/watch-async
                      maildir-path
                      (fn [_] (swap! call-count inc))
                      {:settle-ms 20})]
    (try
      (Thread/sleep 500)
      (is (zero? @call-count)
          "existing messages at startup should not trigger on-message")
      (finally
        (.interrupt watcher)))))

(deftest watch-async-unified-api-delivers-id
  (let [maildir-path (tu/make-empty-maildir "mailseq-watch-")
        received     (promise)]
    (mailseq/with-source [src {:type :maildir
                               :folders {"INBOX" maildir-path}}]
      (let [thread (mailseq/watch-async src "INBOX"
                                        (fn [msg] (deliver received msg))
                                        {:settle-ms 20})]
        (try
          (Thread/sleep 200)
          (io/copy fixture-eml
                   (io/file maildir-path "new" "1700099998.M9.host"))
          (let [msg (deref received 5000 :timeout)]
            (is (not= :timeout msg) "on-message should have been called")
            (when (not= :timeout msg)
              (is (string? (:id msg)))
              (is (seq (:id msg)))
              (is (= "<test-002@example.com>" (:message-id msg)))))
          (finally
            (.interrupt thread)))))))

(deftest watch-deduplicates-across-new-and-cur
  (let [maildir-path (tu/make-empty-maildir "mailseq-watch-")
        call-count   (atom 0)
        watcher      (watch/watch-async
                      maildir-path
                      (fn [_] (swap! call-count inc))
                      {:settle-ms 20})]
    (try
      (Thread/sleep 200)
      ;; Deliver to new/
      (io/copy fixture-eml
               (io/file maildir-path "new" "1700099999.M1.host"))
      (Thread/sleep 500)
      ;; Simulate an MUA moving it to cur/ with flags (creates a new file)
      (io/copy fixture-eml
               (io/file maildir-path "cur" "1700099999.M1.host:2,S"))
      (Thread/sleep 500)
      (is (= 1 @call-count)
          "same stable-id should only trigger on-message once")
      (finally
        (.interrupt watcher)))))
