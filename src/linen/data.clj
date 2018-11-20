(ns linen.data
  (:gen-class))


(defprotocol PDataConnector
  (resolve-module [self m]))


