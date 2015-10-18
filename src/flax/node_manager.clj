(ns flax.node-manager
  (:require [flax.protocols :refer :all])
  (:gen-class))


(defn resolve-connector
  [namesp]
  (-> namesp symbol require)
  ((-> namesp (str "/connector") symbol resolve)))


(defrecord LocalConnector []
  PNodeConnector
  (create [self node] self)
  (destroy [self] nil)
  (invoke [self checkpoint]
    (-> checkpoint :invocation))
  (clone [self]
    self))


(defrecord NodeManager [nodes]
  PNodeManager
  (get-node [self node]
    (cond
      ;; In the case where you want to run the checkpoint on foreign and
      ;; the local node.
      (= node "local") (LocalConnector.)
      ;; If you're only passing a string, the only outcome can be
      ;; retrieving an existing node (or nil).
      (string? node) (-> @nodes (get node))
      ;; If it's a map, then we can go one of two ways:
      (map? node)
      (if (contains? @nodes (-> node :name))
        ;; If a node already exists with the node name, just return it.
        (-> @nodes (get (-> node :name)))
        ;; Else, create the node and add it to the list of managed nodes.
        (let [n (-> node :connector resolve-connector (create node))]
          (swap! nodes #(assoc % (-> @(:node n) :name) n))
          n))))
  (remove-node [self node]
    (let [fun (cond
                (string? node) #(dissoc % node)
                (map? node) #(dissoc % (-> node :name)))]
      (swap! nodes fun)))
  (isolate [self]
    self))

