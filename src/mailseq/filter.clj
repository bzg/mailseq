;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.filter
  "Pure, in-memory filtering of parsed message maps.

  mailseq is a strictly read-only library driven by two selection
  criteria only: a date range (`:since` / `:before`) and a count
  (`:limit`). Full-text matching on subject, addressees, or
  message-id is out of scope — if you need that, filter the sequence
  returned by `mailseq/messages` in your own code.

  This namespace is the canonical implementation of that contract.
  It has no I/O and no backend dependencies: it consumes message
  maps and returns the subset whose `:date-sent` falls in the
  requested window."
  (:import [java.util Date]
           [java.text SimpleDateFormat]))

;; ---------------------------------------------------------------------------
;; Option contract
;; ---------------------------------------------------------------------------

(def ^:private match-keys
  "Options that constrain which messages match (applied by `matches?`)."
  #{:since :before})

(def ^:private parse-keys
  "Options that control how messages are parsed (passed through to parse)."
  #{:headers? :body? :attachments?})

(def ^:private shape-keys
  "Options that reshape the result set (applied after `matches?`)."
  #{:limit})

(def allowed-option-keys
  "The full set of keys accepted by `mailseq/messages` across every
  backend. Backend-specific extras (e.g. the IMAP-only `:raw?`) are
  not part of this set and must be whitelisted at the call site via
  the 2-arity of `validate-opts`."
  (clojure.set/union match-keys parse-keys shape-keys))

(defn validate-opts
  "Throw ex-info if `opts` contains any key outside the common contract.
  Returns `opts` unchanged on success.

  The 2-arity accepts an `extra-allowed` set of backend-specific keys
  that should also be tolerated (e.g. `#{:raw?}` on IMAP)."
  ([opts] (validate-opts opts #{}))
  ([opts extra-allowed]
   (let [allowed (into allowed-option-keys extra-allowed)
         unknown (into #{} (remove allowed) (keys opts))]
     (when (seq unknown)
       (throw (ex-info (str "Unsupported fetch option(s): " (pr-str unknown))
                       {:type    ::unsupported-options
                        :unknown unknown
                        :allowed allowed}))))
   opts))

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

(defn- since? [{:keys [date-sent]} since]
  (and date-sent (not (.before ^Date date-sent (->date since)))))

(defn- before? [{:keys [date-sent]} before]
  (and date-sent (.before ^Date date-sent (->date before))))

(defn matches?
  "True if message map `m` satisfies the date window in `opts`.

  A message is excluded when `:since` or `:before` is present but
  `:date-sent` is nil — there is no date to evaluate, so the safest
  answer is to drop the message rather than silently keep it.

  Options not in the match set (`:limit`, `:headers?`, …) are
  silently passed through — they do not influence the decision."
  [m {:keys [since before]}]
  (and (or (nil? since)  (since? m since))
       (or (nil? before) (before? m before))))

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

(defn parse-opts
  "Extract the keys that `message->map` cares about from an opts map."
  [opts]
  (select-keys opts [:headers? :body? :attachments?]))
