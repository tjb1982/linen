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
    (println (-> checkpoint :invocation)))
  (clone [self]
    self))


(defrecord NodeManager [nodes]
  PNodeManager
  (get-node [self node]
    (cond
      (string? node) (-> @nodes (get node))
      (map? node)
      (if (contains? @nodes (-> node :name))
        (-> @nodes (get (-> node :name)))
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

