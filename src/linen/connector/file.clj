(ns linen.connector.file
  (:require [clj-yaml.core :as yaml]
            [linen.data :as data])
  (:gen-class))

(defn slurp-yaml
  [s]
  (let [s (if (.startsWith s (str "~" java.io.File/separator))
            (clojure.string/replace
              s #"~" (str (System/getProperty "user.home")))
            s)]
    (-> s slurp yaml/parse-all)))


(defrecord FileDataConnector []
  data/PDataConnector
  (resolve-module [self s]
    (slurp-yaml s)))
 

(defn connector [] (FileDataConnector.))

