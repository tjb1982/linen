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
  (:import co.nclk.linen.data.FileDataConnector
           clojure.lang.MapEntry)
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
        (let [parent (evaluate (:parent m) config)
              env (extract-env parent (:env config))]
          (merge (evaluate (dissoc m :parent :child) config)
                 {:parent parent
                  :child (evaluate (:child m) (assoc config :env env))}))

        ;; Functions and special forms
        (->> (keys m) (some #(-> % str (subs 1) (.startsWith "~("))))
        (flax/evaluate m config evaluate)

        ;; Modules
        (contains? m :module)
        (let [module (let [module (:module m)]
                       (if (string? module)
                         (resolve-module
                           (:data-connector config)
                           ;; only evaluating a string here:
                           (evaluate module config))
                         module))
              clean-env (reduce-outputs (:inputs m) {} (:env config))]
          (assoc m :module (run-module module (assoc config :env clean-env))))

        ;; Checkpoints
        (contains? m :checkpoint)
        (run-checkpoint (:checkpoint m) config)

        ;; All other maps
        :else
        (flax/evaluate m config evaluate))

      :else (flax/evaluate m config evaluate))))


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


(defn map-entry
  [[k v]]
  (MapEntry. (keyword k) v))


(defn normalize-output
  [p]
  (if (string? p)
    ;; TODO: something like `(flax/var-str p)`
    {(keyword p) (str "~@" p)}

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


(declare extract-env)

(defn extract-env-from-module
  "Dealing with a single module, compile its provided environment, then compile
   its output environment using the provided environment, returning the result.
   A module's environment is composed of checkpoint environments and the environments of
   inner modules."
  [module outputs* env]
  (let [[{provides :provides input-env :env name* :name} & body] module
        ;; the cp envs directly owned by this module:
        cp-envs (harvest body :env #{:module})
        mod-envs (map #(extract-env % {}) (harvest body :module #{} true))
        combined-envs (concat cp-envs mod-envs)
        provides-env (reduce (partial reduce-outputs provides)
                             (merge env input-env)
                             ;; if the body is empty or otherwise doesn't provide any
                             ;; envs, we still want to run this once to make sure we can
                             ;; provide the `provides` with whatever `requires`/`inputs`
                             ;; were given.
                             (if (empty? combined-envs) [{}] combined-envs))
        next-env provides-env]
    (let [outputs (if (sequential? outputs*) outputs* [outputs*])
      ;; output venv includes outside env for binding, so that the new keys can be 
      ;; evaluated with awareness of the outside env, and then those new entries get
      ;; merged into the outside env, which is the return value here.
          ret (reduce-outputs outputs env (merge env input-env next-env))]
      (when (contains? #{"opscd/initialize" "lcm/initialize"} name*)
        (spit "/tmp/debug" (with-out-str
                             (clojure.pprint/pprint {:module module :env ret})
                           :append true)))
      ret
      )))


(defn extract-env
  [m & [env]]
  (cond

    (map? m)
    (cond

      ;; TODO: pre-compiled environment from parent/child for efficiency
      ;(contains? m :extracted-env)
      ;(:extracted-env m)

      (contains? m :module)
      (let [{:keys [outputs module]} m]
        (extract-env-from-module module outputs env))

      ;; TODO: this might be okay after all, since checkpoints inside of modules won't be seen.
      ;; So this would refer to rogue checkpoints in the parent's body. Caveat emptor.
      ;;(every? #(contains? m %) #{:checkpoint :env})
      ;;(merge env (:env m))

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
  (let [{:keys [stdout stderr exit]} resolved
        assert-fn (evaluate (or (:assert node) (:assert resolved)) config)
        success (if (or (nil? assert-fn)
                        (bool? assert-fn))
                  (if (true? assert-fn) (zero? (:value exit)) true)
                  (apply assert-fn (map :value [stdout stderr exit])))
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
       ;; merging (:env config) here allows us to add `provides`
       ;; that use the inputs from the `requires`
       :env (merge (:env config) (reduce checkpoint-env {} return))}
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
  (if (string? r) {:key r} r))


(defn run-module
  "Runs a given module and config.
  "
  [module* {env* :env :or {env* {}} :as config}]
  (when (runnable? config)
    (let [module (normalize-module module*)
          [{name* :name
            requires* :requires
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

      (let [default-env (->> default-requires
                             (map (fn [{:keys [key default]}] [(keyword key) default]))
                             (into {}))
            env (merge (evaluate default-env config) env)
            config (assoc config :env env)]
        ;; below shouldn't be necessary because it effectively happens for each checkpoint
        ;; cf. bottom of `run-checkpoint`
        (flatten [(assoc header :env env) (evaluate body config)]))
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


(defn clean-up
  [node-manager & [failed]]
  (doall
    (pmap
      (fn [[k n]]
        (when (:data n)
          (let [options (-> @(:data n) :options)]
          ;; when destroy-on-exit is true, destroy, unless persist-on-failure
          ;; is true and the test failed.
          (when (and (-> options :destroy-on-exit true?)
                     (not (and failed
                               (-> options :persist-on-failure true?))))
            (destroy n)))))
      @(-> node-manager :nodes))))


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
        {module :main
         nm :node-manager
         failed? :failed?} config
        result (try (run-module module config)
                 (finally
                   (log :info "Cleaning up.")
                   (clean-up nm @failed?)
                   (log :info "Done cleaning up.")))]
    (log :info (str "Seed: " (.getTime effective)))
    result))

