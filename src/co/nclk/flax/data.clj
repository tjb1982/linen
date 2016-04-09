(ns co.nclk.flax.data
  (:require [clj-yaml.core :as yaml])
  (:gen-class))


(defprotocol PDataConnector
  (resolve-program [self m])
  (resolve-module [self m]))


(defn slurp-yaml
  [s]
  (let [s (if (.startsWith s (str "~" java.io.File/separator))
            (clojure.string/replace
              s #"~" (str (System/getProperty "user.home")))
            s)]
    (-> s slurp yaml/parse-string)))


(defrecord FileDataConnector []
  PDataConnector
  (resolve-program [self s]
    (slurp-yaml s))
  (resolve-module [self s]
    (slurp-yaml s)))
 

(defn connector [] (FileDataConnector.))

