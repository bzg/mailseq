;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.imap.fetch-test
  "Unit tests for mailseq.imap.fetch internals (build-date-term).

  These test private functions via var references — no IMAP server required."
  (:require [clojure.test :refer [deftest testing is]]
            [mailseq.imap.fetch])
  (:import [jakarta.mail.search SentDateTerm AndTerm]))

(def ^:private build-date-term #'mailseq.imap.fetch/build-date-term)

;; ---------------------------------------------------------------------------
;; build-date-term
;; ---------------------------------------------------------------------------

(deftest build-date-term-empty
  (testing "no date keys returns nil"
    (is (nil? (build-date-term {})))
    (is (nil? (build-date-term {:limit 10})))
    (is (nil? (build-date-term {:headers? true :body? false})))))

(deftest build-date-term-single
  (testing "single :since returns SentDateTerm"
    (is (instance? SentDateTerm (build-date-term {:since "2025-01-01"}))))
  (testing "single :before returns SentDateTerm"
    (is (instance? SentDateTerm (build-date-term {:before "2025-12-31"})))))

(deftest build-date-term-range
  (testing ":since + :before returns AndTerm"
    (is (instance? AndTerm
                   (build-date-term {:since "2025-01-01" :before "2025-12-31"})))))

(deftest build-date-term-invalid-date
  (testing "invalid date in :since throws IllegalArgumentException"
    (is (thrown? IllegalArgumentException
                 (build-date-term {:since "garbage"})))))
