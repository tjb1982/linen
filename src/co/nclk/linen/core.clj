(ns co.nclk.linen.core
  (:require [clj-yaml.core :as yaml]
            [cheshire.core :as json]
            [cheshire.generate :refer [add-encoder encode-str]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as logging]
            [co.nclk.linen.node :refer [invoke destroy node-manager clean log]]
            [co.nclk.linen.data :refer [resolve-module resolve-program]]
            [stencil.parser :refer [parse]]
            [stencil.core :refer [render]]
            [co.nclk.flax.core :as flax]
            )
  (:import co.nclk.linen.data.FileDataConnector
           clojure.lang.MapEntry
           LinenJNI)
  (:gen-class))


(declare evaluate run-module run-checkpoint extract-env normalize-module reduce-outputs)


;; Cheshire encoder for all things Runnable.
(add-encoder java.lang.Runnable encode-str)


(def checkpoint-binding-keys
  #{:stdout :stderr :exit :success})


(defn runnable?
  [config]
  (let [run? (:runnable? config)]
    (or (nil? run?)
        (-> @run? false? not))))


(defn bool?
  [tester]
  (boolean (some #(% tester) #{true? false?})))
  ;;(-> tester type (= java.lang.Boolean)))


(defn harvest
  [m hkey & [blocked container? reports]]
  (cond
    (map? m)
    (if-let [report ((keyword hkey) m)]
      (conj reports (if container? m report))
      (reduce
        (fn [reports [k v]]
          ;; TODO: (if-not (k blocked) ... where blocked is like #{:module}
          ;; should make black-box modules possible by coordinating with parent/child/env
          (if-not (k blocked)
            (harvest v hkey blocked container? reports)
            reports))
        reports m))

    (coll? m)
    (reduce
      (fn [reports x]
        (harvest x hkey blocked container? reports))
      reports m)

    :else reports))


(defn name-or-nil
  [x]
  (try (name x) (catch Throwable t)))


(defn normalize-output
  [p]
  (if (name-or-nil p)
    ;; TODO: something like `(flax/var-str p)`
    {(keyword p) (str "«" (name p))}

    (if (map? p)
      (if (-> p keys set (remove #{:key :value}) empty?)
        {(keyword (:key p)) (:value p)}
        (into {} (->> p (map (fn [[k v]] {(keyword k) v}))))
      ))))


(defn reduce-outputs
  [bindings* env venv]
  (let [bindings (if (sequential? bindings*) bindings* [bindings*])]
    (->> bindings
         (map normalize-output)
         (reduce #(merge %1 (flax/evaluate %2 (merge %1 venv)))
                 env))))


(defn extract-module-env
  [provides & [body input-env]]
  (let [envs* (harvest body :env #{:env})
        envs (if (empty? envs*) [{}] envs*)
        input-env (if-not (map? input-env) {} input-env)
        ret (reduce (partial reduce-outputs provides) input-env envs)]
    ret
    ))


(defn extract-parent-env
  [m env]
  (let [envs (harvest m :env #{:env})]
    (merge env (reduce merge envs))))


(defn evaluate
  [m config]
  (assert (contains? config :env) "linen: config must have entry for `:env`")
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
        (let [parent (evaluate (:parent m) config)
              env (extract-parent-env parent (:env config))]
          (merge (evaluate (dissoc m :parent :child) config)
                 {:parent parent
                  :child (evaluate (:child m) (assoc config :env env))}))

        ;; Functions and special forms
        (->> (keys m) (some #(-> % str (subs 1) (.startsWith "("))))
        (flax/evaluate m config evaluate)
        ;;(flax/evaluate m config (fn [m & [env custom-eval]] (evaluate m config)))

        ;; Modules
        (contains? m :module)
        (let [{:keys [module inputs outputs]} m
              module (if-let [module-name (name-or-nil module)]
                       (resolve-module
                         (:data-connector config)
                         (evaluate module-name config))
                       module)
              clean-env (reduce-outputs inputs {} (:env config))
              resolved-module (run-module module (assoc config :env clean-env))
              {provided-env :env} resolved-module
              output-env (reduce-outputs outputs {} (merge (:env config) provided-env))]
          (assoc m :module (assoc resolved-module :env output-env)))

        ;; Checkpoints
        (contains? m :checkpoint)
        (run-checkpoint (:checkpoint m) config)

        ;; All other maps
        :else
        (flax/evaluate m config evaluate))

      :else (flax/evaluate m config evaluate))))


(defn assert-checkpoint
  [resolved node config]
  (let [{:keys [stdout stderr exit]} resolved
        assert-fn (evaluate (or (:assert node) (:assert resolved)) config)
        success (if (or (nil? assert-fn)
                        (bool? assert-fn))
                  (if (true? assert-fn) (zero? (:value exit)) true)
                  (apply assert-fn (map :value [stdout stderr exit])))
        resolved (assoc-in resolved [:success :value] success)]

    (when (not (true? success))
      (deliver (:failed? config) true)
      (log :debug (with-out-str (clojure.pprint/pprint resolved)))
      (log :error (str "[" (:runid resolved) "] Failed.")))

    (when-let [handler (-> config :callbacks :checkpoint :finished)]
      (handler resolved))

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
  (->> checkpoint-binding-keys
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
  (-> (->> checkpoint-binding-keys
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


(defn log-checkpoint-started
  [config checkpoint]
  (when-let [handler (-> config :callbacks :checkpoint :started)]
    (handler checkpoint)))


(defn run-checkpoint-node
  "Run a given checkpoint and config on a given node.
  "
  [checkpoint node config]
  (let [cpn (checkpoint-node checkpoint node)]
    (log-checkpoint-started config cpn)

    (-> config
        :node-manager
        (invoke (checkpoint-node checkpoint node) node)
        (assert-checkpoint node config))))


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
                    (log-checkpoint-started config (checkpoint-node checkpoint node))
                    (-> (or (:node-manager config)
                            ;; TODO: why wouldn't there be a node-manager here?
                            (node-manager (:effective config) (:callbacks config)))
                        (invoke (checkpoint-node checkpoint nil) node)
                        (assoc :node node)
                        (assert-checkpoint node config)))))]
      {:checkpoint return
       ;; merging (:env config) here allows us to add `provides`
       ;; that use the inputs from the `requires`
       ;; XXX: it's not appropriate to include the module env here, right? We can do that at
       ;; the Module level.
       ;:env (merge (:env config) (reduce checkpoint-env {} return))}
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


(defn normalize-require
  [r]
  ;; TODO: allow {FOO: bar} to be equivalent to {key: FOO, default: bar}
  ;; also allow dictionary instead of (map normalize-require)
  ;; i.e., this should behave more like the `provides`/`outputs`
  (if (string? r) {:key r} r))


(defn run-module
  "Runs a given module and config.
  "
  [module* {env* :env :or {env* {}} :as config*}]
  (when (runnable? config*)
    (let [module (normalize-module module*)
          [{name* :name
            requires* :requires
            provides :provides
            :as header} & body] module
          requires (map normalize-require requires*)
          requires-keyset (->> requires (map :key) (map keyword) set)
          default-requires (->> requires (remove #(not (contains? % :default))))
          non-default-keys (->> requires
                                (remove (set default-requires))
                                (map #(keyword (:key %))))
          ;; Only keys that have been declared `:required` are allowed.
          env (select-keys env* requires-keyset)
          missing (->> non-default-keys (remove #(contains? env %)))]

      (if-not (empty? missing)
        (throw (IllegalArgumentException.
                 (str "Linen: module \"" name* "\" is missing inputs: " (set missing)))))

      (let [default-env (-> (->> default-requires
                                 (map (fn [{:keys [key default]}] [(keyword key) default]))
                                 (into {}))
                            (evaluate config*))
            input-env (merge default-env env)
            config (assoc config* :env input-env)
            resolved-body (evaluate body config)
            output-env (extract-module-env provides resolved-body input-env)]
        ;;(flatten [(assoc header :env output-env) resolved-body]))
        {:header header :env output-env :body resolved-body})
        ;(flatten [header (evaluate body config)]))
      )))


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


(defn clean-up-fn
  [node-manager runnable? failed?]
  (fn []
    (deliver failed? false)
    (swap! runnable? (constantly false))
    (when-not (empty? @(:nodes node-manager))
      (log :info "Cleaning up.")
      (clean node-manager @failed?)
      (log :info "Done cleaning up.")
    )))


(defn run
  "The entry point for any well-formed program config.
  "
  [config]
  (config-pre-check config)
  (let [{callbacks :callbacks} config]

    (let [effective (java.util.Date. (long (:effective config)))
          config (assoc config :effective effective
                               :runnable? (atom true)
                               :node-manager (node-manager effective callbacks)
                               :genv (or (:genv config) genv)
                               :failed? (promise))
          {module :main
           nm :node-manager
           runnable? :runnable?
           failed? :failed?} config
          clean-up (clean-up-fn nm runnable? failed?)]
      (-> (Runtime/getRuntime) (.addShutdownHook (Thread. clean-up)))
      (log :info (str "Seed: " (.getTime effective)))
      (let [result (run-module module config)]
        (clean-up)
        (log :info (str "Seed: " (.getTime effective)))
        result))))

