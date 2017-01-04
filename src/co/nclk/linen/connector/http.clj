(ns co.nclk.linen.connector.http
  (:require [clj-yaml.core :as yaml]
            [co.nclk.linen.data :as data])
  (:gen-class))

(defn pret
  [it]
  (clojure.pprint/pprint it)
  it)

(defn slurp-yaml
  [s]
  (-> s clojure.java.io/as-url
        slurp
        yaml/parse-string
        :data))

(defrecord HTTPDataConnector []
  data/PDataConnector
  (resolve-program [self s]
    (slurp-yaml s))
  (resolve-module [self s]
    (slurp-yaml s)))

(defn connector [] (HTTPDataConnector.))

