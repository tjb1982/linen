(ns flax.node-manager
  (:require [flax.protocols :refer :all])
  (:gen-class))


(defn resolve-connector
  [namesp]
  (-> namesp symbol require)
  (-> namesp (str "/connector") symbol resolve))


(defrecord LocalConnector []
  PNodeConnector
  (create [self node full-name] self)
  (destroy [self] nil)
  (invoke [self checkpoint]
    (-> checkpoint :invocation))
  (clone [self]
    self))


(def local-connector (LocalConnector.))


(defrecord NodeManager [nodes effective version logger]
  PNodeManager
  (get-node [self node]
    (cond
      ;; In the case where you want to run the checkpoint on foreign nodes
      ;; and the local node.
      (= node "local") local-connector
      ;; If you're only passing a string, the only outcome can be
      ;; retrieving an existing node (or nil).
      (string? node) (-> @nodes (get (full-node-name self node)))
      ;; If it's a map, then we can go one of two ways:
      (map? node)
      (if (contains? @nodes (full-node-name self node))
        ;; If a node already exists with the node name, just return it.
        (-> @nodes (get (full-node-name self node)))
        ;; Else, create the node and add it to the list of managed nodes.
        (let [ctor (-> node :connector resolve-connector)
              node (-> (ctor logger) (create node (full-node-name self node)))]
          (-> node start)
          ;; The new node record has a :name which is the full node name.
          (swap! nodes #(assoc % (-> @(:node node) :name)
                                 node))
          node))))
  (remove-node [self node]
    (let [fun (cond
                (string? node) #(dissoc % node)
                (map? node) #(dissoc % (-> node :name)))]
      (swap! nodes fun)))
  (full-node-name [self node]
    (str "flax-" (:name node) "-" (.getTime (:effective self)) "-" (:version self)))
  (isolate [self]
    self))

