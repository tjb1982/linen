(ns flax.node-manager
  (:require [flax.protocols :refer :all]
            [flax.logutil :refer :all])
  (:gen-class))


(defn resolve-connector
  [namesp]
  (-> namesp symbol require)
  (-> namesp (str "/connector") symbol resolve))


(defrecord LocalConnector [logger]
  PNodeConnector
  (create [self node full-name] self)
  (destroy [self] nil)
  (invoke [self checkpoint]
    (if-not (:invocation checkpoint)
      checkpoint
      (let [tmpfile-name (str ".flax-temp-script-" (java.util.UUID/randomUUID))
            argv (remove nil?
                   (flatten
                     [(if-let [u (:user checkpoint)] ["sudo" "-u" u]) "bash" tmpfile-name]))]
        (spit tmpfile-name (:invocation checkpoint))
        (let [proc (-> (Runtime/getRuntime)
                     (.exec (into-array String argv)))
              stdout (stream-to-reader (.getInputStream proc))
              stderr (stream-to-reader (.getErrorStream proc))]
          (when-not (false? (:log checkpoint))
            (log logger :info (clojure.string/join " " argv))
            (log logger :info (str "Contents of " tmpfile-name ":\n" (:invocation checkpoint))))
          (loop [out []
                 err []]
            (let [line (.readLine stdout)
                  errl (.readLine stderr)]
              (when-not (false? (:log checkpoint))
                (when-not (clojure.string/blank? line)
                  (log logger :info line))
                (when-not (clojure.string/blank? errl)
                  (log logger :info errl)))
              (if-not (and (nil? line)
                           (nil? errl))
                (recur (if-not (nil? line)
                         (conj out line) out)
                       (if-not (nil? errl)
                         (conj err errl) err))
                (let [out (clojure.string/join "\n" out)
                      err (clojure.string/join "\n" err)]
                  (clojure.java.io/delete-file tmpfile-name)
                  (assoc checkpoint :out {:keys (:out checkpoint)
                                          :value out}
                                    :err {:keys (:err checkpoint)
                                          :value err}
                                    :exit {:keys (:exit checkpoint)
                                           :value (.waitFor proc)})))))))))
  (clone [self]
    self))


(def ^:dynamic local-connector nil)


(defrecord NodeManager [nodes effective version logger]
  PNodeManager
  (get-node [self node]
    (cond
      ;; In the case where you want to run the checkpoint on foreign nodes
      ;; and the local node.
      (= (-> node :name) "local") local-connector
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

(defn node-manager
  [effective version logger]
  (alter-var-root #'local-connector
                  (fn [_] (LocalConnector. logger)))
  (NodeManager. (atom {}) effective version logger))
