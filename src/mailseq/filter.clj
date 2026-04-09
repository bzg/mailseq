;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.filter
  "Pure, in-memory filtering of parsed message maps.

  This namespace is the canonical implementation of the fetch-option
  contract described in plan §14. It has no I/O and no backend
  dependencies: it consumes message maps that already conform to the
  shape described in plan §13 and returns the subset matching a set of
  options.

  The IMAP backend translates the same options into server-side
  SearchTerms for efficiency; Maildir and Mbox will consume this
  namespace directly."
  (:require [clojure.string :as str])
  (:import [java.util Date]
           [java.text SimpleDateFormat]))

;; ---------------------------------------------------------------------------
;; Option contract
;; ---------------------------------------------------------------------------

(def ^:private match-keys
  "Options that constrain which messages match (applied by `matches?`)."
  #{:since :before :from :to :cc :subject :message-id})

(def ^:private parse-keys
  "Options that control how messages are parsed (passed through to parse)."
  #{:headers? :body? :attachments? :raw?})

(def ^:private shape-keys
  "Options that reshape the result set (applied after `matches?`)."
  #{:limit})

(def allowed-option-keys
  "The full set of keys accepted by `mailseq/messages` across every backend."
  (into #{} (concat match-keys parse-keys shape-keys)))

(defn validate-opts
  "Throw ex-info if `opts` contains any key outside the common contract.
  Returns `opts` unchanged on success."
  [opts]
  (let [unknown (into #{} (remove allowed-option-keys) (keys opts))]
    (when (seq unknown)
      (throw (ex-info (str "Unsupported fetch option(s): " (pr-str unknown))
                      {:type    ::unsupported-options
                       :unknown unknown
                       :allowed allowed-option-keys}))))
  opts)

;; ---------------------------------------------------------------------------
;; Date coercion (pure)
;; ---------------------------------------------------------------------------

(def ^:private date-formats
  ["yyyy-MM-dd" "yyyy-MM-dd'T'HH:mm:ss" "dd/MM/yyyy"])

(defn ->date
  "Coerce `d` to a `java.util.Date`. Accepts Date instances and strings in
  the formats declared by the library. Throws IllegalArgumentException
  on unparseable input."
  ^Date [d]
  (cond
    (instance? Date d) d
    (string? d)
    (or (some (fn [fmt]
                (try (.parse (SimpleDateFormat. fmt) d)
                     (catch Exception _ nil)))
              date-formats)
        (throw (IllegalArgumentException.
                (str "Cannot parse date: " (pr-str d)))))
    :else
    (throw (IllegalArgumentException.
            (str "Expected a Date or date string, got: " (type d))))))

;; ---------------------------------------------------------------------------
;; Per-filter predicates (pure, operate on a parsed message map)
;; ---------------------------------------------------------------------------

(defn- ci-contains?
  "Case-insensitive substring test. Nil-safe on both sides."
  [haystack needle]
  (and haystack needle
       (str/includes? (str/lower-case haystack)
                      (str/lower-case needle))))

(defn- address-matches?
  "True if substring `needle` appears in any :name or :address of an
  address vector (e.g. the value of :from, :to, :cc)."
  [addresses needle]
  (boolean
   (some (fn [{:keys [name address]}]
           (or (ci-contains? name needle)
               (ci-contains? address needle)))
         addresses)))

(defn- since? [{:keys [date-sent]} since]
  (and date-sent (not (.before ^Date date-sent (->date since)))))

(defn- before? [{:keys [date-sent]} before]
  (and date-sent (.before ^Date date-sent (->date before))))

(defn matches?
  "True if message map `m` satisfies every filter option in `opts`.

  A message is excluded when a filter is present but cannot be
  evaluated (e.g. `:since` on a message with no `:date-sent`). Filters
  absent from `opts` are ignored.

  Options not in the match set (`:limit`, `:headers?`, …) are
  silently passed through — they do not influence the decision."
  [m {:keys [since before from to cc subject message-id] :as _opts}]
  (and (or (nil? since)      (since? m since))
       (or (nil? before)     (before? m before))
       (or (nil? from)       (address-matches? (:from m) from))
       (or (nil? to)         (address-matches? (:to m) to))
       (or (nil? cc)         (address-matches? (:cc m) cc))
       (or (nil? subject)    (ci-contains? (:subject m) subject))
       (or (nil? message-id) (= (:message-id m) message-id))))

;; ---------------------------------------------------------------------------
;; Convenience: apply the full contract to a seq of messages
;; ---------------------------------------------------------------------------

(defn apply-opts
  "Filter `messages` by `opts` and apply `:limit` (most recent kept).

  `messages` must already be sorted oldest-to-newest; `:limit` keeps
  the tail. This mirrors the IMAP backend's behaviour and is the
  canonical implementation for Maildir and Mbox."
  [messages opts]
  (validate-opts opts)
  (let [filtered (filterv #(matches? % opts) messages)
        limit    (:limit opts)]
    (if (and limit (< limit (count filtered)))
      (subvec filtered (- (count filtered) limit))
      filtered)))
