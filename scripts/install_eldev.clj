(ns install-eldev
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(defn installed? []
  (-> (sh "eldev")
      :out
      (str/includes? "Usage: eldev")))
