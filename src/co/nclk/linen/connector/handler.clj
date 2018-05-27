(ns co.nclk.linen.connector.handler
  (:require [clj-yaml.core :as yaml]
            [co.nclk.linen.data :as data])
  (:gen-class))

(defrecord HandlerDataConnector [phandler mhandler]
  data/PDataConnector
  (resolve-program [self s]
    (phandler s))
  (resolve-module [self s]
    (mhandler s)))

(defn connector [phandler mhandler]
  (HandlerDataConnector. phandler mhandler))

