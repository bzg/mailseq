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
            [mailseq :as mailseq]
            [mailseq.imap.connect :as imap-connect]
            [mailseq.imap.fetch :as imap-fetch]
            [mailseq.imap.folder :as imap-folder]
            [mailseq.imap.idle :as imap-idle])
  (:import [com.icegreen.greenmail.util GreenMail ServerSetup]
           [com.icegreen.greenmail.user GreenMailUser]
           [jakarta.mail.internet MimeMessage]
           [jakarta.mail Session UIDFolder]
           [java.io ByteArrayInputStream]
           [java.nio.file Files]
           [java.util Properties]))

;; ---------------------------------------------------------------------------
;; Shared fixtures
;; ---------------------------------------------------------------------------

(def ^:private fixtures
  "Seq of [message-id file] pairs delivered to both backends."
  [["<test-002@example.com>"          (io/file "dev-resources/emails/plain-text.eml")]
   ["<test-001@example.com>"          (io/file "dev-resources/emails/simple-multipart.eml")]
   ["<test-003@example.com>"          (io/file "dev-resources/emails/with-attachment.eml")]
   ["<test-latin1@example.com>"       (io/file "dev-resources/emails/latin1-undeclared.eml")]
   ["<test-qp@example.com>"           (io/file "dev-resources/emails/quoted-printable.eml")]
   ["<test-b64@example.com>"          (io/file "dev-resources/emails/base64-body.eml")]
   ["<test-folded@example.com>"       (io/file "dev-resources/emails/folded-headers.eml")]
   ["<test-mixed-rfc2047@example.com>" (io/file "dev-resources/emails/mixed-rfc2047.eml")]])

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

(defn- imap-cfg
  "Build an IMAP config map for the test GreenMail server."
  ([] (imap-cfg {}))
  ([extra]
   (merge {:type     :imap
           :host     "localhost"
           :port     *imap-port*
           :ssl      false
           :user     imap-user
           :password imap-pass
           :folders  {"INBOX" "INBOX"}}
          extra)))

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
    ;; Filename timestamps post-date every fixture's Date: header (Apr 2025)
    ;; so the Maildir :since pre-filter never wrongly prunes them.
    (doseq [[i [_ file]] (map-indexed vector fixtures)]
      (io/copy file
               (io/file cur (format "17500000%02d.M%d.host:2,S" i i))))
    (.getAbsolutePath (.toFile root))))

(defn- index-by-message-id [messages]
  (into {} (map (juxt :message-id identity)) messages))

;; ---------------------------------------------------------------------------
;; The contract test
;; ---------------------------------------------------------------------------

(deftest imap-and-maildir-agree-on-message-map
  (let [maildir-path (make-maildir-fixture)]
    (mailseq/with-source [imap-src (imap-cfg)]
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
  (mailseq/with-source [src (imap-cfg)]
    (let [all (mailseq/messages src "INBOX")
          one (first all)
          id  (:id one)
          fetched (mailseq/by-id src "INBOX" id)]
      (is (map? fetched) "by-id must return a map, not a vector")
      (is (= (:message-id one) (:message-id fetched)))
      (testing "unknown id returns nil"
        (is (nil? (mailseq/by-id src "INBOX" "999999999")))))))

;; ---------------------------------------------------------------------------
;; Filter contract is honoured end-to-end on every backend
;; ---------------------------------------------------------------------------

(defn- ids-by-filter [src opts]
  (into #{} (map :message-id) (mailseq/messages src "INBOX" opts)))

(deftest date-filters-applied-identically-on-both-backends
  (let [maildir-path (make-maildir-fixture)]
    (mailseq/with-source [imap-src (imap-cfg)]
      (mailseq/with-source [md-src {:type :maildir
                                    :folders {"INBOX" maildir-path}}]
        (doseq [opts [{}
                      {:since "2025-02-01"}
                      {:before "2025-06-01"}
                      {:since "2025-02-01" :before "2025-06-01"}
                      {:limit 1}]]
          (testing (str "opts " (pr-str opts))
            (is (= (ids-by-filter imap-src opts)
                   (ids-by-filter md-src   opts)))))))))

(deftest full-text-keys-rejected-on-both-backends
  (let [maildir-path (make-maildir-fixture)]
    (mailseq/with-source [imap-src (imap-cfg)]
      (mailseq/with-source [md-src {:type :maildir
                                    :folders {"INBOX" maildir-path}}]
        (doseq [src [imap-src md-src]
                k   [:from :to :cc :subject :message-id]]
          (testing (str "key " k " rejected")
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported"
                                  (mailseq/messages src "INBOX" {k "x"})))))))))

(deftest imap-rejects-unknown-options
  (mailseq/with-source [src (imap-cfg)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported"
                          (mailseq/messages src "INBOX" {:bogus 1})))))

(deftest raw-is-imap-only
  (testing "raw? is accepted on IMAP"
    (mailseq/with-source [src (imap-cfg)]
      (let [raw (mailseq/messages src "INBOX" {:raw? true})]
        (is (every? #(instance? jakarta.mail.Message %) raw)))))
  (testing "raw? is rejected on Maildir"
    (let [maildir-path (make-maildir-fixture)]
      (mailseq/with-source [src {:type :maildir :folders {"INBOX" maildir-path}}]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported"
                              (mailseq/messages src "INBOX" {:raw? true})))))))

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

;; ---------------------------------------------------------------------------
;; list-ids / by-ids incremental workflow
;; ---------------------------------------------------------------------------

(deftest list-ids-on-both-backends
  (let [maildir-path (make-maildir-fixture)]
    (mailseq/with-source [imap-src (imap-cfg)]
      (mailseq/with-source [md-src {:type :maildir
                                    :folders {"INBOX" maildir-path}}]
        (let [imap-ids (mailseq/list-ids imap-src "INBOX")
              md-ids   (mailseq/list-ids md-src   "INBOX")]
          (testing "returns the right count"
            (is (= (count fixtures) (count imap-ids)))
            (is (= (count fixtures) (count md-ids))))
          (testing "all ids are non-blank strings"
            (is (every? #(and (string? %) (seq %)) imap-ids))
            (is (every? #(and (string? %) (seq %)) md-ids))))))))

(deftest by-ids-batch-on-both-backends
  (let [maildir-path (make-maildir-fixture)]
    (mailseq/with-source [imap-src (imap-cfg)]
      (mailseq/with-source [md-src {:type :maildir
                                    :folders {"INBOX" maildir-path}}]
        (doseq [[label src] [["IMAP" imap-src] ["Maildir" md-src]]]
          (testing label
            (let [all-ids (mailseq/list-ids src "INBOX")
                  msgs    (mailseq/by-ids src "INBOX" all-ids)]
              (is (= (count fixtures) (count msgs)))
              (is (= (set (map :message-id msgs))
                     (set (map first fixtures)))))
            (testing "subset fetch"
              (let [one-id (first (mailseq/list-ids src "INBOX"))
                    msgs   (mailseq/by-ids src "INBOX" [one-id])]
                (is (= 1 (count msgs)))
                (is (= one-id (:id (first msgs))))))))))))

;; ---------------------------------------------------------------------------
;; IMAP-specific: by-uid-range, folder operations
;; ---------------------------------------------------------------------------

(defn- make-imap-conn []
  (imap-connect/connect (dissoc (imap-cfg) :type :folders)))

(deftest by-uid-range-fetches-subset
  (let [conn (make-imap-conn)]
    (try
      (let [all   (imap-fetch/messages conn "INBOX")
            uids  (mapv :uid all)
            start (apply min uids)
            end   (apply max uids)
            range (imap-fetch/by-uid-range conn "INBOX" start end)]
        (is (= (count all) (count range)))
        (testing "same message-ids"
          (is (= (set (map :message-id all))
                 (set (map :message-id range))))))
      (finally
        (imap-connect/disconnect conn)))))

(deftest by-uid-range-with-lastuid
  (let [conn (make-imap-conn)]
    (try
      (let [all    (imap-fetch/messages conn "INBOX")
            uids   (mapv :uid all)
            mid    (nth (sort uids) 1)
            subset (imap-fetch/by-uid-range conn "INBOX" mid UIDFolder/LASTUID)]
        (is (<= (count subset) (count all)))
        (is (every? #(>= (:uid %) mid) subset)))
      (finally
        (imap-connect/disconnect conn)))))

(deftest folder-list-and-counts
  (let [conn (make-imap-conn)]
    (try
      (let [folders (imap-folder/list-folders conn)]
        (testing "list-folders returns at least INBOX"
          (is (seq folders))
          (is (some #(= "INBOX" (:name %)) folders)))
        (testing "folder maps have expected keys"
          (let [inbox (first (filter #(= "INBOX" (:name %)) folders))]
            (is (contains? inbox :full-name))
            (is (contains? inbox :type))
            (is (contains? inbox :message-count))
            (is (contains? inbox :unread-count)))))
      (testing "message-count"
        (is (= (count fixtures)
               (imap-folder/message-count conn "INBOX"))))
      (testing "unread-count"
        (is (number? (imap-folder/unread-count conn "INBOX"))))
      (finally
        (imap-connect/disconnect conn)))))

(deftest idle-async-starts-and-stops
  (let [conn    (make-imap-conn)
        msgs    (atom [])
        thread  (imap-idle/idle-async conn "INBOX"
                                     (fn [m] (swap! msgs conj m))
                                     {:heartbeat-ms 500})]
    (try
      (testing "thread is alive"
        (is (.isAlive thread)))
      (Thread/sleep 200)
      (testing "interrupting stops the thread"
        (.interrupt thread)
        (.join thread 8000)
        (is (not (.isAlive thread))))
      (finally
        (when (.isAlive thread) (.interrupt thread))
        (imap-connect/disconnect conn)))))

;; ---------------------------------------------------------------------------
;; by-id-range — unified API on both backends
;; ---------------------------------------------------------------------------

(deftest by-id-range-on-imap
  (mailseq/with-source [src (imap-cfg)]
    (let [all-ids (mailseq/list-ids src "INBOX")
          sorted  (sort all-ids)]
      (testing "full range returns all messages"
        (let [msgs (mailseq/by-id-range src "INBOX" (first sorted) (last sorted))]
          (is (= (count fixtures) (count msgs)))))
      (testing "nil end-id means to the end"
        (let [msgs (mailseq/by-id-range src "INBOX" (first sorted) nil)]
          (is (= (count fixtures) (count msgs)))))
      (testing "range from second id returns subset"
        (when (> (count sorted) 1)
          (let [msgs (mailseq/by-id-range src "INBOX" (second sorted) nil)]
            (is (< (count msgs) (count all-ids)))))))))

(deftest by-id-range-on-maildir
  (let [maildir-path (make-maildir-fixture)]
    (mailseq/with-source [src {:type :maildir
                               :folders {"INBOX" maildir-path}}]
      (let [all-ids (mailseq/list-ids src "INBOX")
            sorted  (sort all-ids)]
        (testing "full range returns all messages"
          (let [msgs (mailseq/by-id-range src "INBOX" (first sorted) (last sorted))]
            (is (= (count fixtures) (count msgs)))))
        (testing "nil end-id means to the end"
          (let [msgs (mailseq/by-id-range src "INBOX" (first sorted) nil)]
            (is (= (count fixtures) (count msgs)))))
        (testing "range beyond existing ids returns empty"
          (let [msgs (mailseq/by-id-range src "INBOX" "zzzzzzz" nil)]
            (is (empty? msgs))))))))

;; ---------------------------------------------------------------------------
;; underlying-conn
;; ---------------------------------------------------------------------------

(deftest underlying-conn-imap
  (mailseq/with-source [src (imap-cfg)]
    (let [conn (mailseq/underlying-conn src)]
      (testing "returns a connection map for IMAP"
        (is (some? conn))
        (is (contains? conn :store))
        (is (contains? conn :session)))
      (testing "connection is usable with low-level API"
        (is (imap-connect/connected? conn))))))

(deftest underlying-conn-maildir-returns-nil
  (let [maildir-path (make-maildir-fixture)]
    (mailseq/with-source [src {:type :maildir
                               :folders {"INBOX" maildir-path}}]
      (is (nil? (mailseq/underlying-conn src))))))

;; ---------------------------------------------------------------------------
;; uid-validity — IMAP-only escape hatch
;; ---------------------------------------------------------------------------

(deftest uid-validity-imap-returns-long
  (mailseq/with-source [src (imap-cfg)]
    (let [uv (mailseq/uid-validity src "INBOX")]
      (testing "returns a positive long for IMAP"
        (is (integer? uv))
        (is (pos? uv)))
      (testing "stable across calls on the same mailbox"
        (is (= uv (mailseq/uid-validity src "INBOX")))))))

(deftest uid-validity-maildir-returns-nil
  (let [maildir-path (make-maildir-fixture)]
    (mailseq/with-source [src {:type :maildir
                               :folders {"INBOX" maildir-path}}]
      (is (nil? (mailseq/uid-validity src "INBOX"))))))

(deftest uid-validity-unknown-folder-throws
  (mailseq/with-source [src (imap-cfg)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (mailseq/uid-validity src "no-such-folder")))))

;; ---------------------------------------------------------------------------
;; watch-async — unified API
;; ---------------------------------------------------------------------------

(deftest watch-async-imap-starts-and-stops
  (mailseq/with-source [src (imap-cfg)]
    (let [thread (mailseq/watch-async src "INBOX"
                                      (fn [_])
                                      {:heartbeat-ms 500})]
      (try
        (testing "thread is alive"
          (is (.isAlive thread)))
        (Thread/sleep 200)
        (.interrupt thread)
        (.join thread 8000)
        (testing "thread stopped after interrupt"
          (is (not (.isAlive thread))))
        (finally
          (when (.isAlive thread) (.interrupt thread)))))))

;; ---------------------------------------------------------------------------
;; IMAP IDLE: verify that watch-async receives a newly delivered message
;; ---------------------------------------------------------------------------

(deftest idle-async-receives-new-message
  (let [gm-setup (ServerSetup. 0 "127.0.0.1" "imap")
        gm       (doto (GreenMail. (into-array ServerSetup [gm-setup])) .start)]
    (try
      (let [user (.createUser (.getUserManager gm) imap-user imap-user imap-pass)
            sess (Session/getInstance (Properties.))
            port (.. gm getImap getServerSetup getPort)
            ;; Deliver one message so INBOX exists
            _    (deliver-fixture! user sess (io/file "dev-resources/emails/plain-text.eml"))
            received (promise)
            conn (imap-connect/connect (dissoc (imap-cfg {:port port}) :type :folders))
            thread (imap-idle/idle-async conn "INBOX"
                                        (fn [m] (deliver received m))
                                        {:heartbeat-ms 500})]
        (try
          (Thread/sleep 300)
          ;; Deliver a second message while IDLE is watching
          (deliver-fixture! user sess (io/file "dev-resources/emails/simple-multipart.eml"))
          (let [msg (deref received 8000 :timeout)]
            (testing "IDLE callback received a message"
              (is (not= :timeout msg) "on-message should have been called"))
            (when (not= :timeout msg)
              (testing "received message has :id"
                (is (some? (:id msg))))
              (testing "received message has :message-id"
                (is (some? (:message-id msg))))))
          (finally
            (.interrupt thread)
            (.join thread 5000)
            (imap-connect/disconnect conn))))
      (finally
        (.stop gm)))))

;; ---------------------------------------------------------------------------
;; OAuth2: invalid token fails gracefully
;; ---------------------------------------------------------------------------

(deftest oauth2-invalid-token-fails-gracefully
  (is (thrown? Exception
              (imap-connect/connect
               (dissoc (imap-cfg {:password nil :oauth2-token "invalid-token-xyz"})
                       :type :folders)))))

;; ---------------------------------------------------------------------------
;; Encoding fixtures: content survives parsing on both backends
;; ---------------------------------------------------------------------------

(deftest encoding-fixtures-parsed-correctly
  (let [maildir-path (make-maildir-fixture)]
    (mailseq/with-source [imap-src (imap-cfg)]
      (mailseq/with-source [md-src {:type :maildir
                                    :folders {"INBOX" maildir-path}}]
        (let [imap-by-id (index-by-message-id (mailseq/messages imap-src "INBOX"))
              md-by-id   (index-by-message-id (mailseq/messages md-src   "INBOX"))]

          (testing "Latin-1 / quoted-printable: accented body decoded"
            (doseq [[label by-id] [["IMAP" imap-by-id] ["Maildir" md-by-id]]]
              (testing label
                (let [m (by-id "<test-latin1@example.com>")]
                  (is (some? m))
                  (is (re-find #"encodé" (get-in m [:body :text])))
                  (is (re-find #"guillemets français" (get-in m [:body :text])))
                  (is (= "Présentation du problème" (:subject m)))))))

          (testing "Quoted-printable: soft line breaks and special chars"
            (doseq [[label by-id] [["IMAP" imap-by-id] ["Maildir" md-by-id]]]
              (testing label
                (let [m (by-id "<test-qp@example.com>")]
                  (is (some? m))
                  (is (re-find #"éàü" (get-in m [:body :text])))
                  (is (re-find #"needs to be wrapped" (get-in m [:body :text])))))))

          (testing "Base64: body correctly decoded"
            (doseq [[label by-id] [["IMAP" imap-by-id] ["Maildir" md-by-id]]]
              (testing label
                (let [m (by-id "<test-b64@example.com>")]
                  (is (some? m))
                  (is (re-find #"encoded in base64" (get-in m [:body :text])))
                  (is (re-find #"éàü" (get-in m [:body :text])))))))

          (testing "Folded headers: unfolded correctly"
            (doseq [[label by-id] [["IMAP" imap-by-id] ["Maildir" md-by-id]]]
              (testing label
                (let [m (by-id "<test-folded@example.com>")]
                  (is (some? m))
                  (is (re-find #"Folded headers" (:subject m)))
                  (is (re-find #"continuation" (:subject m)))
                  (is (= "longname@example.com"
                         (:address (first (:from m)))))))))

          (testing "Mixed RFC 2047: UTF-8 + ISO-8859-1 in From and Subject"
            (doseq [[label by-id] [["IMAP" imap-by-id] ["Maildir" md-by-id]]]
              (testing label
                (let [m (by-id "<test-mixed-rfc2047@example.com>")]
                  (is (some? m))
                  (is (re-find #"François" (:name (first (:from m)))))
                  (is (re-find #"à traiter" (:subject m))))))))))))

;; ---------------------------------------------------------------------------
;; Closed source throws
;; ---------------------------------------------------------------------------

(deftest closed-imap-source-throws
  (let [src (mailseq/open (imap-cfg))]
    (mailseq/close src)
    (testing "messages after close"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"closed"
                            (mailseq/messages src "INBOX"))))
    (testing "list-ids after close"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"closed"
                            (mailseq/list-ids src "INBOX"))))
    (testing "close is idempotent"
      (mailseq/close src))))

(deftest closed-maildir-source-throws
  (let [maildir-path (make-maildir-fixture)
        src (mailseq/open {:type :maildir :folders {"INBOX" maildir-path}})]
    (mailseq/close src)
    (testing "messages after close"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"closed"
                            (mailseq/messages src "INBOX"))))
    (testing "list-ids after close"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"closed"
                            (mailseq/list-ids src "INBOX"))))
    (testing "close is idempotent"
      (mailseq/close src))))

