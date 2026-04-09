;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.fetch-test
  "Unit tests for mailseq.fetch internals (parse-date, build-date-term).

  These test private functions via var references — no IMAP server required."
  (:require [clojure.test :refer [deftest testing is]]
            [mailseq.fetch])
  (:import [java.util Date]
           [jakarta.mail.search SentDateTerm AndTerm]))

(def ^:private parse-date       #'mailseq.fetch/parse-date)
(def ^:private build-date-term #'mailseq.fetch/build-date-term)

;; ---------------------------------------------------------------------------
;; parse-date
;; ---------------------------------------------------------------------------

(deftest parse-date-iso-format
  (testing "yyyy-MM-dd"
    (let [d (parse-date "2025-06-15")]
      (is (instance? Date d))
      (is (= 2025 (+ 1900 (.getYear d))))
      (is (= 5 (.getMonth d)))     ;; June = 5 (zero-based)
      (is (= 15 (.getDate d))))))

(deftest parse-date-iso-datetime-format
  (testing "yyyy-MM-dd'T'HH:mm:ss"
    (let [d (parse-date "2025-06-15T14:30:00")]
      (is (instance? Date d))
      (is (= 2025 (+ 1900 (.getYear d))))
      (is (= 5 (.getMonth d)))
      (is (= 15 (.getDate d))))))

(deftest parse-date-european-format
  (testing "dd/MM/yyyy"
    (let [d (parse-date "15/06/2025")]
      (is (instance? Date d))
      (is (= 2025 (+ 1900 (.getYear d))))
      (is (= 5 (.getMonth d)))
      (is (= 15 (.getDate d))))))

(deftest parse-date-passthrough
  (testing "java.util.Date passes through unchanged"
    (let [now (Date.)]
      (is (identical? now (parse-date now))))))

(deftest parse-date-invalid-string
  (testing "unparseable string throws IllegalArgumentException"
    (is (thrown? IllegalArgumentException (parse-date "not-a-date")))
    (is (thrown? IllegalArgumentException (parse-date "")))
    (is (thrown? IllegalArgumentException (parse-date "yesterday")))))

(deftest parse-date-wrong-type
  (testing "non-string non-Date throws IllegalArgumentException"
    (is (thrown? IllegalArgumentException (parse-date 12345)))
    (is (thrown? IllegalArgumentException (parse-date :keyword)))))

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
