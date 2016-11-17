(ns co.nclk.linen.connector.http
  (:require [clj-yaml.core :as yaml]
            [co.nclk.linen.data :as data])
  (:gen-class))

(defn slurp-yaml
  [s]
  (-> s clojure.java.io/resource
        slurp
        yaml/parse-string))

(defrecord HTTPDataConnector []
  data/PDataConnector
  (resolve-program [self s]
    (slurp-yaml s))
  (resolve-module [self s]
    (slurp-yaml s)))

(defn connector [] (HTTPDataConnector.))

