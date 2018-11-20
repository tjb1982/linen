(ns linen.connector.handler
  (:require [clj-yaml.core :as yaml]
            [linen.data :as data])
  (:gen-class))

(defrecord HandlerDataConnector [handler]
  data/PDataConnector
  (resolve-module [self s]
    (handler s)))

(defn connector [handler]
  (HandlerDataConnector. handler))

