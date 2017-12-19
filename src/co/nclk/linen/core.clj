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

(def parser-options (atom {:tag-open "~{" :tag-close "}"}))
(def genv (atom
            (into {}
              (for [[k v] (System/getenv)]
                [(keyword k) v]))))

(defn upmap
  [fn coll & [delay]]
  (doall
    (for [p (doall
              (map
                #(do
                   (Thread/sleep (or delay 0))
                   (future (fn %)))
                coll))]
      @p)))


(defn bool?
  [tester]
  (-> tester type (= java.lang.Boolean)))


(defn die
  [& args]
  (apply println args)
  (System/exit 1))


(defn get-with-string-maybe-index
  [key* env]
  (try
    (let [idx (Integer/parseInt key*)]
      (if (sequential? env)
        (nth env idx)
        (-> key* keyword env)))
    (catch NumberFormatException nfe
      (-> key* keyword env))))


(declare evaluate)


(defn assoc-flags
  [c m]
  (if (map? m)
    (merge c (select-keys m [:throw :skip :log :assert]))
    c))


(defn do-groups
  [fun m config]
  (doall
    (mapcat
      #(if (map? %)
        (let [config (assoc-flags config %)]
          (doall
            (pmap (fn [member] (apply fun member))
                  (map (fn [x] [x config])
                       (:group %)))))
        (doall
          (pmap (fn [member] (apply fun member))
                (map (fn [x] [x config])
                     %))))
      (if (map? m) (:groups m) m))))


(defn assert-checkpoint
  [resolved config node]
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

 
    (when (:log-checkpoints? config)
      (log :checkpoint resolved))

    (if (true? success)
      resolved
      (if (or (:throw node) (:throw resolved))
        (throw (AssertionError. resolved))
        resolved))))


(defn run-checkpoint
  [checkpoint config]
  (let [;; First we snag the flags from the checkpoint
        ;; and assoc them to the config
        config (assoc-flags config checkpoint)
        ;; Then we assoc the config to the checkpoint, so the flags are visible
        ;; to the `invoke` function. We also evaluate the checkpoint to make
        ;; sure the variables are mapped to their appropriate values from the
        ;; environment.
        checkpoint (-> checkpoint
                     (assoc-flags config)
                     (evaluate config)
                     (assoc :runid (java.util.UUID/randomUUID)))]

    (if (:nodes checkpoint)
      ;; Run the checkpoint on some nodes in parallel.
      (->> (:nodes checkpoint)
        (upmap
          #(-> config
            :node-manager
            (invoke
              (assoc checkpoint :out [(:out checkpoint)
                                      (:out %)]
                                :err [(:err checkpoint)
                                      (:err %)]
                                :exit [(:exit checkpoint)
                                       (:exit %)]
                                :success {:keys [(:success checkpoint)
                                                 (:success %)]}
                                :proxy (:proxy %)
                                :user (:user %))
              %)
            (assert-checkpoint config %)
            )))
      ;; Run it on the local host, returning a list as if it were run on
      ;; potentially several nodes.
      (list (if-not (:node-manager config)
              (-> (node-manager (or (:effective config) 0))
                  (invoke checkpoint :local)
                  (assert-checkpoint config {}))
              (-> config :node-manager
                  (invoke checkpoint :local)
                  (assert-checkpoint config {})))))
    ))

(defn checkpoint-env
  [env checkpoint]
  (let [out-arr (-> checkpoint :out :keys first keyword)
        err-arr (-> checkpoint :err :keys first keyword)
        exit-arr (-> checkpoint :exit :keys first keyword)
        success-arr (-> checkpoint :success :keys first keyword)
        out (-> checkpoint :out :value)
        err (-> checkpoint :err :value)
        exit (-> checkpoint :exit :value)
        success (-> checkpoint :success :value)]
    (reduce (fn [env [k v]]
              (if-not (nil? k)
                (assoc env k v) env))
            env
            [[out-arr (conj (get env out-arr) out)]
             [(-> checkpoint :out :keys second keyword) out]
             [err-arr (conj (get env err-arr) err)]
             [(-> checkpoint :err :keys second keyword) err]
             [exit-arr (conj (get env exit-arr) exit)]
             [(-> checkpoint :exit :keys second keyword) exit]
             [success-arr (conj (get env success-arr) success)]
             [(-> checkpoint :success :keys second keyword) success]])))

(defn run-module
  [m c]
  ;; Check that the required params exist in the env for this module to run.
  ;; Either the param exists, or the module defines a default to use instead.
  (let [env (if (:merge-global-environment c)
              (merge @(:genv c) (:env c))
              (:env c))
        vars (map #(or (contains? % :default)
                       (-> % :key keyword))
                   (-> m :requires))
        missing (remove #(-> % second true?)
                        (map (fn [v]
                               [v (or ;; Is it both boolean and true, signifying a
                                      ;; default value exists?
                                      (and (bool? v)
                                           (true? v))
                                      ;; Or is it a keyword and found in the current
                                      ;; env?
                                      (and (keyword? v)
                                           (not (nil? (-> env v)))))])
                             vars))]
    (if-not (empty? missing)
      ;; If some of the `requires` interfaces are missing, don't bother running it,
      ;; and throw assertion error.
      (throw
        (AssertionError.
          (format "Some required inputs are missing for module `%s`.\nMissing: %s\n\n"
                  (:name m)
                  (into [] (map first missing)))))
      ;; All `requires` interfaces exist, so we're a "go."
      ;; Run the checkpoints. Each checkpoint run will return its return code and
      ;; the vars that should be added to the env.
      ;; Scoop up the "provides" and "requires" values from the local env and put
      ;; them into a new env.
      (when-let [checkpoints (-> m :checkpoints)]
        (let [defaults (into {}
                         (map (fn [x] [(keyword (:key x)) (:default x)])
                              (:requires m)))
              config (assoc c :env (merge (evaluate defaults c)
                                          (:env c)))
              returns (do-groups run-checkpoint
                        (evaluate checkpoints config)
                        (assoc-flags config checkpoints))]

          ;; `returns` is a list of returns.
          ;; A return is a list of done checkpoints, one for each node it was run on.
          ;; The :env here is only referring to the new environment created by
          ;; this module; it's the responsibility of the calling code to merge
          ;; it with any more comprehensive environment before running code that
          ;; depends on the these values.
          {:returns returns
           :env (->> returns
                     (reduce
                       (fn [env return]
                         (merge env
                           (reduce checkpoint-env env return)))
                       {}))})))))


(defn extract-env
  [m & [env]]
  (cond

    (map? m)
    (if (contains? m :returns)
      (let [out (evaluate (:out m)
                          {:env (merge env
                                       (-> m :env))})]
        (merge env out))
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


(defn returns
  [m & [rets]]
  (cond
    (map? m)
    (if-let [r (-> m :returns)]
      (reduce
        #(concat %1 %2)
        rets r)
      (reduce
        (fn [rets [k v]]
          (returns v rets))
        rets m))

    (coll? m)
    (reduce
      (fn [rets x]
        (returns x rets))
      rets m)

    :else rets))


(declare run-program)


(defn evaluate
  [m config]
  (let [config (if (:merge-global-environment config)
                 (assoc config :env
                               (merge @(:genv config)
                                      (:env config)))
                 config)]
    (cond

      (map? m)
      (cond

        ;; Contains :children
        (contains? m :children)
        ;; `dissoc` the :then entry and run it as if it didn't exist.
        ;; Then, extract the new environment from the result, and run the 
        ;; :then entry with that environment.
        (let [patch (evaluate (dissoc m :children) config)
              env (extract-env patch (:env config))]
          ;(assoc patch
          ;       :dependents
          ;(conj [patch]
          ;       (doall (pmap #(evaluate % (assoc config :env env))
          ;                    (:children m)))))
          (conj [patch]
                (evaluate (:children m) (assoc config :env env))))

        ;; Contains :resolve
        (contains? m :resolve)
        (-> config :data-connector
          (resolve-program (evaluate (:resolve m) config))
          :main
          (evaluate config))

        ;; Functions and special forms
        (->> (keys m) (some #(-> % str (subs 1) (.startsWith "~("))))
        (flax/evaluate m config evaluate)
        ;;(flax/evaluate m config (fn [m & [env custom-eval]] (evaluate m config)))

        ;; Modules
        (contains? m :module)
        (let [config (assoc config :env (merge (:env config)
                                               (evaluate (:in m) config)))
              module (if (string? (:module m))
                       (resolve-module
                         (:data-connector config)
                         (evaluate (:module m) config))
                       (:module m))
              ;; Run the module. Each module returns a report with a new env
              ;; based on what it requires/provides, and a list of return values
              ;; for each checkpoint.
              report (run-module module (assoc-flags config m))
              ;; The new env is made up of the old env, with any keys being
              ;; overridden by the :out keys in the patch. All the others are
              ;; ignored.
              out (evaluate (:out m)
                            (assoc config :env (merge (:env config)
                                                      (:env report))))]
          ;(assoc m :report report))
          (merge m report))

        ;; All other maps
        :else
        (flax/evaluate m config evaluate))

      :else (flax/evaluate m config evaluate))))

(defn clean-up
  [node-manager & [failed]]
  (upmap
    (fn [[k n]]
      (when (:data n)
        (let [options (-> @(:data n) :options)]
        ;; when destroy-on-exit is true, destroy, unless persist-on-failure is true and the test failed.
        (when (and (-> options :destroy-on-exit true?)
                   (not (and failed
                             (-> options :persist-on-failure true?))))
          (destroy n)))))
    @(-> node-manager :nodes)))

(defn run-program
  [config]
  ;; A program's main is a collection of one or more entry point groups
  ;; that are kicked off concurrently.
  (let [program (if (map? (:program config))
                  (:program config)
                  (try
                    (resolve-program (:data-connector config)
                                     (evaluate (:program config) config))
                    (catch Exception e
                      (die
                        (str "The configuration must have a program property, "
                             "which must resolve to a valid yaml document:")
                        (.getMessage e)))))
        config (assoc config :env (evaluate (:env config) config))
        exit (try
               (when-let [main (:main program)]
                 (evaluate main config))
               (catch java.util.concurrent.ExecutionException ae
                 (loop [cause ae]
                   (if (nil? cause)
                     (throw ae)
                     (if (= (type cause) java.lang.AssertionError)
                       (do
                         (log :error (str cause))
                         (doseq [el (.getStackTrace cause)]
                           (log :error (str "\t" el))))
                       (recur (.getCause cause))))))
               (finally
                 (log :info "Cleaning up.")
                 (clean-up (:node-manager config)
                           @(:failed? config))
                 (log :info "Done cleaning up.")))]

    exit
    ))


(defn run
  [config]
  ;; Google "clojure stencil" (for the stencil library) for help finding
  ;; what these options can be. It's not particularly easy. -- tjb
  (swap! parser-options #(merge % (or (:parser-options config) {})))
  (try
    ;; Dynamically require the namespace containing the data-connector
    ;; and call its constructor; if no data-connector is specified, then
    ;; use the built-in FileDataConnector.
    (let [effective (let [effective (java.util.Date.
                                      (or (:effective config)
                                          (-> (java.util.Date.) .getTime)))]
                      (log :info (str "Seed: " (.getTime effective)))
                      effective)
          conf (assoc config ;; This timestamp is also intended to be
                             ;; used (via .getTime) as a seed for any
                             ;; pseudorandom number generation, so that
                             ;; randomized testing can be retested as
                             ;; deterministically as possible.
                             :effective effective
                             ;; Instantiate a node manager with an empty
                             ;; node map as an atom.
                             ;; Takes a seed.
                             :node-manager (node-manager effective)
                             :genv (or (:genv config) genv)
                             :failed? (atom false))
          result (do
                   #_(-> (Runtime/getRuntime)
                       (.addShutdownHook
                         (Thread.
                           (fn []
                             (clean-up
                               (-> conf :node-manager))))))
                   (run-program conf))]
      (log :info (str "Seed: " (.getTime effective)))
      result)
    
    (catch java.io.FileNotFoundException fnfe
      (die "Resource could not be found:" (.getMessage fnfe)))
    ))

(defn -main [& argv]
  (if (first argv)
    (run (yaml/parse-string (slurp (first argv))))
    (println "no argument supplied")))


