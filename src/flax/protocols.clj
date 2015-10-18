(ns flax.protocols)

(defprotocol PDataConnector
  (resolve-program [self m])
  (resolve-module [self m]))

(defprotocol PNodeConnector
  (create [self node])
  (destroy [self])
  (invoke [self checkpoint])
  (clone [self]))

(defprotocol PNodeManager
  (get-node [self node])
  (remove-node [self node])
  (isolate [self]))
