(ns install-eldev
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(defn installed? []
  (try
    (sh "eldev")
    true
    (catch Exception _
            false)))

