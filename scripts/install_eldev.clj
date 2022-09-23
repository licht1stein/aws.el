(ns install-eldev
  (:require [clojure.java.shell :refer [sh]]
            [babashka.curl :as curl]
            [clojure.java.io :as io]))

(defn installed? []
  (try
    (sh "eldev")
    true
    (catch Exception _
            false)))

(defn download []
  (io/copy
   (:body (curl/get "https://raw.github.com/doublep/eldev/master/bin/eldev"
    {:as :bytes}))
   (io/file "./eldev"))
  (.length (io/file "./eldev")))
