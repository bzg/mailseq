;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.filter-test
  "Pure tests for the common filtering layer. No IMAP, no filesystem."
  (:require [clojure.test :refer [deftest testing is]]
            [mailseq.filter :as f])
  (:import [java.util Date GregorianCalendar]))

(defn- date
  "Build a java.util.Date at midnight UTC for the given y/m/d."
  ^Date [y m d]
  (.getTime (GregorianCalendar. y (dec m) d)))

(def msg-a
  {:id         "a"
   :message-id "<a@example.com>"
   :from       [{:name "Alice Dupont" :address "alice@example.com"}]
   :to         [{:name nil :address "bob@example.com"}]
   :cc         []
   :subject    "Hello Clojure world"
   :date-sent  (date 2025 3 15)
   :flags      #{:seen}})

(def msg-b
  {:id         "b"
   :message-id "<b@example.com>"
   :from       [{:name "Charlie" :address "charlie@corp.io"}]
   :to         [{:name nil :address "alice@example.com"}]
   :cc         [{:name nil :address "dave@example.com"}]
   :subject    "Re: meeting notes"
   :date-sent  (date 2026 1 10)
   :flags      #{}})

(def msg-c
  {:id         "c"
   :message-id "<c@example.com>"
   :from       [{:name "Bob" :address "bob@example.com"}]
   :to         [{:name nil :address "list@example.com"}]
   :cc         []
   :subject    nil
   :date-sent  nil
   :flags      #{}})

(def msgs [msg-a msg-b msg-c])

;; ---------------------------------------------------------------------------
;; validate-opts
;; ---------------------------------------------------------------------------

(deftest validate-opts-accepts-contract
  (testing "all contract keys are accepted"
    (is (map? (f/validate-opts {:since "2025-01-01" :before "2026-01-01"
                                :limit 10 :headers? true :body? false
                                :attachments? false})))))

(deftest validate-opts-rejects-dropped-full-text-keys
  (testing ":from/:to/:cc/:subject/:message-id are out of scope"
    (doseq [k [:from :to :cc :subject :message-id]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported"
                            (f/validate-opts {k "x"}))))))

(deftest validate-opts-rejects-raw-by-default
  (testing ":raw? is IMAP-only, rejected by the common contract"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported"
                          (f/validate-opts {:raw? true})))))

(deftest validate-opts-accepts-extras
  (testing "extra-allowed set whitelists backend-specific keys"
    (is (map? (f/validate-opts {:raw? true} #{:raw?})))))

(deftest validate-opts-rejects-unknown
  (testing "unknown keys raise ex-info"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Unsupported fetch option"
                          (f/validate-opts {:unseen true}))))
  (testing "the offending key is reported"
    (try
      (f/validate-opts {:body "foo"})
      (is false "should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= #{:body} (:unknown (ex-data e))))))))

;; ---------------------------------------------------------------------------
;; matches?
;; ---------------------------------------------------------------------------

(deftest matches-empty-opts
  (testing "no options matches everything"
    (is (every? #(f/matches? % {}) msgs))))

(deftest matches-since
  (is (f/matches? msg-b {:since "2025-06-01"}))
  (is (not (f/matches? msg-a {:since "2025-06-01"})))
  (testing "nil date-sent excluded when :since is present"
    (is (not (f/matches? msg-c {:since "2020-01-01"})))))

(deftest matches-before
  (is (f/matches? msg-a {:before "2025-06-01"}))
  (is (not (f/matches? msg-b {:before "2025-06-01"}))))

(deftest matches-range
  (let [in-range #(f/matches? % {:since "2025-01-01" :before "2026-01-01"})]
    (is (in-range msg-a))
    (is (not (in-range msg-b)))
    (is (not (in-range msg-c)))))

;; ---------------------------------------------------------------------------
;; apply-opts
;; ---------------------------------------------------------------------------

(deftest apply-opts-filters-by-date
  (is (= [msg-a] (f/apply-opts msgs {:before "2025-06-01"})))
  (is (= [msg-b] (f/apply-opts msgs {:since "2025-06-01"}))))

(deftest apply-opts-limit-keeps-tail
  (testing ":limit keeps the last N after filtering"
    (is (= [msg-b msg-c] (f/apply-opts msgs {:limit 2})))))

(deftest apply-opts-limit-larger-than-result
  (is (= msgs (f/apply-opts msgs {:limit 99}))))

(deftest apply-opts-validates
  (is (thrown? clojure.lang.ExceptionInfo
               (f/apply-opts msgs {:unseen true}))))

;; ---------------------------------------------------------------------------
;; ->date
;; ---------------------------------------------------------------------------

(deftest to-date-passthrough
  (let [d (Date.)]
    (is (identical? d (f/->date d)))))

(deftest to-date-parses-strings
  (is (instance? Date (f/->date "2025-06-15")))
  (is (instance? Date (f/->date "15/06/2025"))))

(deftest to-date-invalid
  (is (thrown? IllegalArgumentException (f/->date "garbage")))
  (is (thrown? IllegalArgumentException (f/->date 12345))))
