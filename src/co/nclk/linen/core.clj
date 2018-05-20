(ns co.nclk.linen.core
  (:require [clj-yaml.core :as yaml]
            [cheshire.core :as json]
            [cheshire.generate :refer [add-encoder encode-str]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :refer [log]]
            [co.nclk.linen.node :refer [invoke destroy node-manager]]
            [co.nclk.linen.data :refer [resolve-module resolve-program]]
            [stencil.parser :refer [parse]]
            [stencil.core :refer [render]]
            [co.nclk.flax.core :as flax]
            )
  (:import co.nclk.linen.data.FileDataConnector)
  (:gen-class))


(declare evaluate run-module run-checkpoint extract-env)


;; Cheshire encoder for all things Runnable.
(add-encoder java.lang.Runnable encode-str)


(defn runnable?
  [config]
  (let [run? (:runnable? config)]
    (or (nil? run?)
        (-> @run? false? not))))


(defn bool?
  [tester]
  (boolean (some #(% tester) #{true? false?})))
  ;;(-> tester type (= java.lang.Boolean)))


(defn evaluate
  [m config]
  (let [config (if (:merge-global-environment config)
                 (assoc config :env
                               (merge ((:genv config))
                                      (:env config)))
                 config)]
    (cond

      (map? m)
      (cond

        ;; Contains :parent/:child dependency
        (and (contains? m :parent) (contains? m :child))
        ;; `dissoc` the :then entry and run it as if it didn't exist.
        ;; Then, extract the new environment from the result, and run the 
        ;; :then entry with that environment.
        (let [parent (evaluate (:parent m) config)
              env (extract-env parent (:env config))]
          ;;(assoc patch :then (evaluate (:then m) (assoc config :env env)))
          {:parent parent :child (evaluate (:child m) (assoc config :env env))}
          #_(conj [patch]
                (evaluate (:then m) (assoc config :env env))))

        ;; Functions and special forms
        (->> (keys m) (some #(-> % str (subs 1) (.startsWith "~("))))
        (flax/evaluate m config evaluate)

        ;; Modules
        (contains? m :module)
        (let [config (assoc config :env (merge (:env config)
                                               (evaluate (:in m) config)))
              module (let [module (:module m)]
                       (if (string? module)
                         (resolve-module
                           (:data-connector config)
                           ;; only evaluating a string here:
                           (evaluate module config))
                         module))]
              ;; Run the module. Each module returns a report with a new env
              ;; based on what it requires/provides, and a list of return values
              ;; for each checkpoint.
          (assoc m :module (run-module module config)))

        ;; Checkpoints
        (contains? m :checkpoint)
        (run-checkpoint (:checkpoint m) config)

        ;; All other maps
        :else
        (flax/evaluate m config evaluate))

      :else (flax/evaluate m config evaluate))))


(defn extract-env
  [m & [env]]
  (cond

    (map? m)
    (cond

      (contains? m :module)
      (let [[{provides :provides
              :as header}
             & body] (:module m)
            venv (extract-env body env)
            next-env
            (merge env
                   (->> provides
                        (map #(if (string? %) {:key % :value ((keyword %) venv)} %))
                        (reduce #(assoc %1 (keyword (:key %2))
                                           (flax/evaluate (:value %2) venv))
                                {})))]
        (merge env (evaluate (:out m) next-env)))


      (every? #(contains? m %) #{:checkpoint :env})
      (merge env (:env m))

      (contains? m :out)
      (let [out (evaluate (:out m) {:env env})]
        (println "LLLLLLLLLL" out env)
        (merge env out))

      :else
      (reduce
        (fn [env [k v]]
          (merge env (extract-env v env)))
        env m))

    (coll? m)
    (reduce
      (fn [env x]
        (extract-env x env))
      env m)

    :else env))


(defn assert-checkpoint
  [resolved node config]
  (let [{:keys [out err exit]} resolved
        assert-fn (evaluate (or (:assert node) (:assert resolved)) config)
        success (if (or (nil? assert-fn)
                        (bool? assert-fn))
                  (if (true? assert-fn) (zero? (:value exit)) true)
                  (apply assert-fn (map :value [out err exit])))
        resolved (assoc-in resolved [:success :value] success)]

    (when (not (true? success))
      (swap! (:failed? config)
             (fn [_] true))
      (log :debug (with-out-str (clojure.pprint/pprint resolved)))
      (log :error (str "[" (:runid resolved) "] Failed.")))

 
    (when (and (:log-checkpoints? config)
               (:runid resolved))
      (log :checkpoint resolved))

    (if (true? success)
      resolved
      (if (or (:throw node) (:throw resolved))
        (throw (AssertionError. resolved))
        resolved))))


(defn assoc-key-exists
  [x key* value]
  (if (nil? key*) x (assoc x key* value)))


(defn checkpoint-env
  [env checkpoint]
  (->> #{:out :err :exit :success}
       (reduce #(let [stream (%2 checkpoint)
                      [array-key single-key] (map keyword (:keys stream))
                      value (:value stream)]
                  (-> %1
                      (assoc-key-exists array-key (conj (get %1 array-key) value))
                      (assoc-key-exists single-key value)))
               env)))


(defn checkpoint-node
  "Create a checkpoint-node from a well-formed checkpoint and node.
  "
  [checkpoint node]
  (-> (->> #{:out :err :exit :success}
           (reduce #(assoc %1 %2 [(%1 %2) (%2 node)]) checkpoint))
      (dissoc :nodes)
      (assoc :proxy (:proxy node)
             :node node
             :user (:user node))))


(defn normalized-node
  [& [node]]
  (if node
    (-> node (assoc :name (:name node "local")))
    {:name "local"}))


(defn run-checkpoint-node
  "Run a given checkpoint and config on a given node.
  "
  [checkpoint node config]
  (-> config
      :node-manager
      (invoke (checkpoint-node checkpoint node) node)
      (assert-checkpoint node config)))


(defn run-checkpoint
  "Run a given checkpoint and config.
  "
  [checkpoint config]
  (when (runnable? config)
    (let [checkpoint (-> checkpoint
                         (evaluate config)
                         (assoc :runid (java.util.UUID/randomUUID)))
          return
          (if (:nodes checkpoint)
            ;; Run the checkpoint on some nodes in parallel.
            (->> (:nodes checkpoint)
                 (map normalized-node)
                 ;; TODO is pmap the best choice here?
                 (pmap #(run-checkpoint-node checkpoint % config)))
            ;; Run it on the local host, returning a list as if it were run on
            ;; potentially several nodes.
            (list (let [node (normalized-node)]
                    (-> (or (:node-manager config)
                            ;; TODO: why wouldn't there be a node-manager here?
                            (node-manager (:effective config)))
                        (invoke (checkpoint-node checkpoint nil) node)
                        (assoc :node node)
                        (assert-checkpoint node config)))))]
      {:checkpoint return
       :env (reduce checkpoint-env {} return)}
      )))


(defn normalize-module
  "Attempt to make a given module well-formed.
   A module is a seq of items. If the argument is not a seq, we wrap it as a lazy-seq.
   The first item is possibly a header; the rest is the body. If the first item is 
   not a well-formed header (i.e., contains keys other than `:name`, `:requires`,
   or `:provides`), then it is regarded as the first item in the body.
   If a header does exist, and has an entry for `:name`, then that name can only be a
   non-empty string.
  "
  [module*]
  (let [module (lazy-seq
                 (if (sequential? module*) module* `(~module*)))
        head (first module)]
    (if (and (map? head)
             (->> head keys (remove #{:name :requires :provides}) empty?))
      (conj (rest module)
            (if (-> head :name clojure.string/blank?)
              (assoc head :name nil) head))
      (conj module {:name nil}))))


(defn run-module
  "Runs a given module and config.
  "
  [module* {env :env :or {env {}} :as config}]
  (when (runnable? config)
    (let [module (normalize-module module*)
          [{name* :name
            requires :requires
            provides :provides
            :as header} & body] module
          default-entries (->> requires
                               (remove #(or (string? %)
                                            (not (contains? % :default)))))
          required-keys (->> requires
                             (remove (set default-entries))
                             (map #(keyword (or (:key %) %))))
          missing (->> required-keys
                       (remove #(contains? env %)))]

      (if-not (empty? missing)
        (throw (IllegalArgumentException.
                 (str "Linen: module \"" name* "\" is missing inputs: " (set missing)))))

      (let [default-env (->> default-entries
                             (map (fn [{:keys [key default]}] [(keyword key) default]))
                             (into {}))
            env (merge (evaluate default-env config) env)
            config (assoc config :env env)]
        (flatten [header (evaluate body config)]))
      )))


(defn harvest
  [m hkey & [reports]]
  (cond
    (map? m)
    (if-let [report ((keyword hkey) m)]
      (conj reports report)
      (reduce
        (fn [reports [k v]]
          (harvest v hkey reports))
        reports m))

    (coll? m)
    (reduce
      (fn [reports x]
        (harvest x hkey reports))
      reports m)

    :else reports))


(def config-keyset
  "The required keyset for the global config map.
  "
  #{:effective :main})


(defn config-pre-check
  "Checks to be sure the global config is well-formed.
  "
  [config]
  (let [missing-keys (->> config-keyset
                          (remove #(contains? config %))
                          set)]
    (if-not (empty? missing-keys)
      (throw (IllegalArgumentException.
               (str "Linen: config missing required entries for: " missing-keys)))))
  ;; TODO: make this a separate function so that we can do this (or something akin to it)
  ;; recursively for the entire program, and then again for each
  ;; module resolution (i.e., when we get something from an outside data source)
  #_(let [module (normalize-module (:main config))]
    (if-not (every? #(-> module %) [#(-> % coll?)
                                    #(-> % map? not)
                                    #(-> % count (>= 2))
                                    #(-> % first map?)])
      (throw (IllegalArgumentException.
               (str "Linen: `:main` must be a complete module, a sequence of at least two objects. "
                    "The first should contain a map, the module header; "
                    "the rest should contain the module body. Received: "
                    (json/generate-string module)))))
    (let [[header & body] module]
      ;; assert things about header and body
      #_(if-not (:name header)
        (throw (IllegalArgumentException.
                 (str "Linen: the :main module header must contain a :name.")))))))


(defn genv
  "Returns the global system environment.
  "
  []
  (-> (System/getenv)
      (map (fn [[k v]] [(keyword k) v]))))


(defn run
  "The entry point for any well-formed program config.
  "
  [config]
  (config-pre-check config)
  (let [effective (java.util.Date. (long (:effective config)))
        config (assoc config :effective effective
                             :runnable? (atom true)
                             :node-manager (node-manager effective)
                             :genv (or (:genv config) genv)
                             :failed? (atom false))
        {module :main} config
        result (run-module module config)]
    (log :info (str "Seed: " (.getTime effective)))
    result))

