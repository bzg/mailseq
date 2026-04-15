;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.maildir-test
  "Integration tests for the Maildir backend, driven against a real
  committed fixture under dev-resources/maildir/."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [mailseq :as mailseq]
            [mailseq.maildir :as maildir]
            [mailseq.test-util :as tu]))

(def ^:private fixture-path
  (.getAbsolutePath (io/file "dev-resources/maildir/inbox")))

;; ---------------------------------------------------------------------------
;; Flag decoding (private)
;; ---------------------------------------------------------------------------

(def ^:private parse-flags maildir/parse-flags)
(def ^:private stable-id   maildir/stable-id)

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

(deftest filters-date-range
  (testing "only the 2025-01-15 message matches"
    (let [msgs (maildir/messages fixture-path
                                 {:since "2025-01-01" :before "2025-01-31"})]
      (is (= 1 (count msgs)))
      (is (= "<test-002@example.com>" (:message-id (first msgs)))))))

(deftest filters-date-range-with-large-body
  (testing "date filter survives a message whose body exceeds header-read-cap"
    ;; Write a synthetic Maildir message with small headers + huge body.
    ;; The envelope pass should read only up to the header/body separator
    ;; and still extract Date: for filtering; pass 2 re-reads the full
    ;; file for survivors.
    (let [path  (tu/make-empty-maildir "mailseq-big-body-")
          big   (apply str "Hello " (repeat 100000 "x"))
          eml   (str "From: sender@example.com\r\n"
                     "To: rcpt@example.com\r\n"
                     "Subject: Huge body\r\n"
                     "Message-ID: <huge-001@example.com>\r\n"
                     "Date: Wed, 15 Jan 2025 12:00:00 +0000\r\n"
                     "\r\n"
                     big)]
      ;; Filename timestamp matches the Date: header (2025-01-15)
      (spit (io/file path "cur" "1736942400.M1.host:2,S") eml)
      (let [in-range  (maildir/messages path {:since "2025-01-01" :before "2025-01-31"})
            out-range (maildir/messages path {:since "2024-01-01" :before "2024-12-31"})]
        (is (= 1 (count in-range)))
        (is (= "<huge-001@example.com>" (:message-id (first in-range))))
        (is (= "Huge body" (:subject (first in-range))))
        (is (zero? (count out-range)))))))

(deftest limit-keeps-most-recent
  (let [msgs (maildir/messages fixture-path {:limit 1})]
    (is (= 1 (count msgs)))
    (testing "most recent by :date-sent is the 2025-02-10 message"
      (is (= "<test-001@example.com>" (:message-id (first msgs)))))))

(deftest by-id-stable-across-flags
  (testing "Matching by the stable prefix finds the cur/ file even though its filename has a flag suffix"
    (let [m (maildir/by-id fixture-path "1736938800.M1.host" {})]
      (is (some? m))
      (is (= "<test-002@example.com>" (:message-id m)))
      (is (contains? (:flags m) :seen))))
  (testing "unknown id returns nil"
    (is (nil? (maildir/by-id fixture-path "does-not-exist" {})))))

(deftest list-ids-returns-stable-ids
  (let [ids (maildir/list-ids fixture-path)]
    (is (= 2 (count ids)))
    (is (every? string? ids))
    (testing "ids match what by-id accepts"
      (doseq [id ids]
        (is (some? (maildir/by-id fixture-path id {})))))))

(deftest by-ids-batch-fetch
  (let [all-ids (maildir/list-ids fixture-path)
        msgs    (maildir/by-ids fixture-path (set all-ids) {})]
    (is (= 2 (count msgs)))
    (is (every? :id msgs))
    (testing "fetching a subset"
      (let [one-id  (first all-ids)
            subset  (maildir/by-ids fixture-path #{one-id} {})]
        (is (= 1 (count subset)))
        (is (= one-id (:id (first subset))))))
    (testing "empty set returns empty"
      (is (empty? (maildir/by-ids fixture-path #{} {}))))
    (testing "unknown ids are silently skipped"
      (is (empty? (maildir/by-ids fixture-path #{"no-such-id"} {}))))))

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

(deftest with-source-propagates-exception
  (testing "exception in body propagates out of with-source"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom"
                          (mailseq/with-source [s {:type :maildir
                                                   :folders {"INBOX" fixture-path}}]
                            (mailseq/messages s "INBOX")
                            (throw (ex-info "boom" {})))))))

(deftest unsupported-type
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported source type"
                        (mailseq/open {:type :pop3}))))

;; ---------------------------------------------------------------------------
;; Incremental fetch: list-ids → add files → list-ids → by-ids
;; ---------------------------------------------------------------------------

(defn- make-temp-maildir
  "Create a throwaway Maildir with one pre-existing message in cur/."
  ^String []
  (let [path (tu/make-empty-maildir "mailseq-incr-")]
    (io/copy (io/file "dev-resources/emails/plain-text.eml")
             (io/file path "cur" "1700000001.M1.host:2,S"))
    path))

(deftest incremental-fetch-via-list-ids-and-by-ids
  (let [path     (make-temp-maildir)
        ids-v1   (set (maildir/list-ids path))
        _        (do (io/copy (io/file "dev-resources/emails/simple-multipart.eml")
                              (io/file path "new" "1700000002.M2.host"))
                     (io/copy (io/file "dev-resources/emails/with-attachment.eml")
                              (io/file path "new" "1700000003.M3.host")))
        ids-v2   (set (maildir/list-ids path))
        new-ids  (clojure.set/difference ids-v2 ids-v1)]
    (testing "diff detects exactly the two new messages"
      (is (= 2 (count new-ids))))
    (testing "by-ids on new-ids returns the right messages"
      (let [msgs (maildir/by-ids path new-ids {})]
        (is (= 2 (count msgs)))
        (is (= new-ids (set (map :id msgs))))
        (is (every? :id msgs))
        (is (every? :message-id msgs))))))

;; ---------------------------------------------------------------------------
;; by-id-range: lexicographic ordering on Maildir
;; ---------------------------------------------------------------------------

(defn- make-maildir-with-varied-ids
  "Create a Maildir with filenames that have varied timestamp prefixes
  to exercise lexicographic filtering in by-id-range."
  ^String []
  (let [path (tu/make-empty-maildir "mailseq-range-")
        eml  (io/file "dev-resources/emails/plain-text.eml")]
    (doseq [name ["100.M1.host:2,S"
                  "1700000001.M2.host:2,S"
                  "1700000050.M3.host:2,S"
                  "1700000100.M4.host:2,S"
                  "9999999999.M5.host:2,S"]]
      (io/copy eml (io/file path "cur" name)))
    path))

;; ---------------------------------------------------------------------------
;; :since pre-filter using filename timestamp
;; ---------------------------------------------------------------------------

(defn- write-eml
  [path subdir fname date-header]
  (let [eml (str "From: sender@example.com\r\n"
                 "To: rcpt@example.com\r\n"
                 "Subject: " fname "\r\n"
                 "Message-ID: <" fname "@example.com>\r\n"
                 "Date: " date-header "\r\n"
                 "Content-Type: text/plain\r\n"
                 "\r\n"
                 "body\r\n")]
    (spit (io/file path subdir fname) eml)))

(deftest since-prefilter-skips-old-filenames
  (testing ":since excludes messages whose filename timestamp is well before the window"
    (let [path (tu/make-empty-maildir "mailseq-prefilter-")]
      ;; Old: filename 2020-01-01, Date: 2020-01-01
      (write-eml path "cur" "1577836800.old.host:2,S" "Wed, 01 Jan 2020 00:00:00 +0000")
      ;; Recent: filename 2025-06-01, Date: 2025-06-01
      (write-eml path "cur" "1748736000.new.host:2,S" "Sun, 01 Jun 2025 00:00:00 +0000")
      (let [msgs (maildir/messages path {:since "2025-01-01"})]
        (is (= 1 (count msgs)))
        (is (= "1748736000.new.host" (:id (first msgs))))))))

(deftest since-prefilter-falls-through-on-unparseable-filename
  (testing "filenames without a numeric prefix fall through to the envelope read"
    (let [path (tu/make-empty-maildir "mailseq-prefilter-fallback-")]
      ;; No numeric prefix but Date: is in range — must be kept
      (write-eml path "cur" "weird-name.host:2,S" "Sun, 01 Jun 2025 00:00:00 +0000")
      (let [msgs (maildir/messages path {:since "2025-01-01"})]
        (is (= 1 (count msgs)))))))

(deftest by-id-range-lexicographic-on-maildir
  (let [path    (make-maildir-with-varied-ids)
        all-ids (sort (maildir/list-ids path))]
    (testing "full range returns all messages"
      (mailseq/with-source [src {:type :maildir :folders {"INBOX" path}}]
        (let [msgs (mailseq/by-id-range src "INBOX" (first all-ids) (last all-ids))]
          (is (= 5 (count msgs))))))
    (testing "range from mid-point excludes earlier ids"
      (mailseq/with-source [src {:type :maildir :folders {"INBOX" path}}]
        (let [msgs (mailseq/by-id-range src "INBOX" "1700000050.M3.host" nil)]
          (is (every? #(>= (compare (:id %) "1700000050.M3.host") 0) msgs))
          (is (pos? (count msgs))))))
    (testing "short numeric prefix sorts before long ones (lexicographic)"
      (mailseq/with-source [src {:type :maildir :folders {"INBOX" path}}]
        (let [msgs (mailseq/by-id-range src "INBOX" "100.M1.host" "1700000001.M2.host")]
          (is (= #{"100.M1.host" "1700000001.M2.host"}
                 (set (map :id msgs)))))))))
