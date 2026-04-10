;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.maildir.watch
  "Watch a Maildir for new messages using `java.nio.file.WatchService`.

  This is the Maildir counterpart of `mailseq.imap.idle`: a blocking
  (or async) loop that calls a user-supplied function each time a new
  message file appears in `cur/` or `new/`.

  Internally, files are delivered to Maildir via an atomic rename from
  `tmp/` into `new/` (or directly into `cur/` by some MDAs). The
  WatchService sees an `ENTRY_CREATE` event for each such rename.

  A set of already-seen stable ids prevents processing the same
  message twice — for instance when another MUA moves a file from
  `new/` to `cur/`, which produces a CREATE in `cur/` for a message
  we already saw in `new/`."
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [mailseq.filter :as flt])
  (:import [java.io File]
           [java.nio.file FileSystems Path StandardWatchEventKinds
            WatchEvent WatchKey WatchService]
           [java.util.concurrent TimeUnit]))

;; Access maildir internals via var references (same library, not public API)
(def ^:private stable-id       #'mailseq.maildir/stable-id)
(def ^:private file->map       #'mailseq.maildir/file->map)
(def ^:private validate-maildir! #'mailseq.maildir/validate-maildir!)

(defn- register-dirs!
  "Register `cur/` and `new/` under `path` for ENTRY_CREATE events.
  Returns a map from WatchKey to the directory Path."
  [^WatchService ws ^File path]
  (let [cur (.toPath (io/file path "cur"))
        new (.toPath (io/file path "new"))
        events (into-array [StandardWatchEventKinds/ENTRY_CREATE])]
    {(.register cur ws events) cur
     (.register new ws events) new}))

(defn watch
  "Watch a Maildir for new messages, calling `on-message` for each.

  Blocks the current thread. Monitors `cur/` and `new/` for new files
  using Java's WatchService (inotify on Linux, kqueue on macOS).

  Existing messages at startup are recorded as seen — only truly new
  files trigger `on-message`.

  Options:
    :parse-opts  - options for message->map (default: {})
    :on-error    - function called with Exception on errors
                   (default: log/error)
    :settle-ms   - delay in ms after ENTRY_CREATE before reading
                   the file (default: 50). Handles rare cases where
                   the filesystem hasn't fully committed the rename.

  Returns nil when the thread is interrupted or the WatchService is
  closed.

  Example:
    (watch \"/home/me/Mail\"
           (fn [msg] (println \"New:\" (:subject msg)))
           {:parse-opts {:attachments? false}})"
  ([path on-message] (watch path on-message {}))
  ([path on-message {:keys [parse-opts on-error settle-ms]
                     :or   {parse-opts {}
                            on-error   #(log/error % "Maildir watch error")
                            settle-ms  50}}]
   (let [dir (io/file path)]
     (validate-maildir! dir)
     (let [seen   (atom (set (mailseq.maildir/list-ids path)))
           ws     (.newWatchService (FileSystems/getDefault))
           keys   (register-dirs! ws dir)
           p-opts (flt/parse-opts parse-opts)]
       (try
         (loop []
           (when-not (Thread/interrupted)
             (let [^WatchKey wk (try
                                  (.poll ws 1 TimeUnit/SECONDS)
                                  (catch InterruptedException _ nil))]
               (when wk
                 (let [watched-dir (get keys wk)]
                   (doseq [^WatchEvent event (.pollEvents wk)]
                     (when (= (.kind event) StandardWatchEventKinds/ENTRY_CREATE)
                       (try
                         (let [^Path rel-path (.context event)
                               ^File f (.toFile (.resolve ^Path watched-dir rel-path))
                               fname   (.getName f)
                               id      (stable-id fname)]
                           (when-not (contains? @seen id)
                             (swap! seen conj id)
                             (when (pos? settle-ms)
                               (Thread/sleep settle-ms))
                             (when-let [m (file->map f p-opts)]
                               (on-message m))))
                         (catch InterruptedException _ nil)
                         (catch Exception e (on-error e)))))
                   (.reset wk)))
               (recur))))
         (finally
           (.close ws)))))))

(defn watch-async
  "Start watching a Maildir in a new daemon thread. Returns the Thread.

  Call `(.interrupt thread)` to stop the watch loop.

  Example:
    (def watcher
      (watch-async \"/home/me/Mail\"
                   (fn [msg] (println \"New:\" (:subject msg)))))
    ;; Later:
    (.interrupt watcher)"
  ([path on-message] (watch-async path on-message {}))
  ([path on-message opts]
   (let [t (Thread. ^Runnable (fn [] (watch path on-message opts))
                    (str "mailseq-watch-" path))]
     (.setDaemon t true)
     (.start t)
     t)))
