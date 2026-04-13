;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns mailseq.test-util
  "Shared test helpers for creating temporary Maildir fixtures."
  (:require [clojure.java.io :as io])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn make-empty-maildir
  "Create a temporary, empty Maildir directory with cur/ and new/."
  (^String [] (make-empty-maildir "mailseq-test-"))
  (^String [prefix]
   (let [root (Files/createTempDirectory
               prefix (into-array FileAttribute []))]
     (.mkdirs (io/file (.toFile root) "cur"))
     (.mkdirs (io/file (.toFile root) "new"))
     (.getAbsolutePath (.toFile root)))))
