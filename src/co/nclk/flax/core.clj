(ns co.nclk.flax.core
  (:require [clojure.core.async :as a :refer [chan go >! >!! <! <!! alts!! alts! go-loop thread]]
            [clj-yaml.core :as yaml]
            [cheshire.core :as json]
            [cheshire.generate :refer [add-encoder encode-str]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :refer [log]]
            [co.nclk.flax.node :refer [invoke destroy node-manager]]
            [co.nclk.flax.data :refer [resolve-module resolve-program]]
            [stencil.parser :refer [parse]]
            [stencil.core :refer [render]]
            )
  (:import co.nclk.flax.data.FileDataConnector)
  (:gen-class))

;; Cheshire encoder for all things Runnable.
(add-encoder java.lang.Runnable encode-str)

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


(defn swap
  [s env]
  (let [env (merge @genv env)]
    (cond

      ;; Env dump.
      (= (clojure.string/trim s) "~@")
      env

      ;; Support for keywords.
      (.startsWith s "~:")
      (keyword (subs s 2))

      ;; Support for symbols
      (.startsWith s "~'")
      (symbol (subs s 2))

      ;; If `s` starts with ~@, then replace it with the data held by the
      ;; variable (i.e., not a string interpolation of it).
      (.startsWith s "~@")
      (let [data (-> s (subs 2) keyword env)]
        (if (string? data) (clojure.string/trim data) data))

      ;; If `s` starts with "~$", then replace it with the
      ;; stdout result of running the script locally.
      (.startsWith s "~$")
      (let [s (subs s 2)
            proc (-> (Runtime/getRuntime)
                   (.exec (into-array String
                            ["bash" "-c" (clojure.string/trim s)])))
            stdout (clojure.java.io/reader (.getInputStream proc))]
        (clojure.string/join "\n"
          (loop [lines []]
            (let [line (.readLine stdout)]
              (if (nil? line)
                lines
                (recur (conj lines line)))))))

      ;; Interpolate.
      :else (render
              (parse s @parser-options)
              env))))


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
                  (apply assert-fn (map :value [out err exit])))]
    (when (not (true? success))
      (log :error (str "[" (:runid resolved) "] Failed."))) 
    (if (and (not (true? success))
             (or (:throw node) (:throw resolved)))
      ;;(throw (AssertionError. (assoc-in resolved [:success :value] false)))
      (assoc-in resolved [:success :value] false)
      (assoc-in resolved [:success :value] success))))


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
              (-> (node-manager (or (:effective config) 0)) (invoke checkpoint :local) (assert-checkpoint config {}))
              (-> config :node-manager (invoke checkpoint :local) (assert-checkpoint config {})))))
    ))


(defn run-module
  [m c]
  ;; Check that the required params exist in the env for this module to run.
  ;; Either the param exists, or the module defines a default to use instead.
  (let [env (merge @genv (-> c :env))
        vars (map #(or (contains? % :default)
                       (-> % :key keyword))
                   (-> m :requires))
        missing (remove #(-> % second true?)
                        (map (fn [v]
                               [v (or ;; Is it both boolean and true, signifying a default value exists?
                                      (and (bool? v)
                                           (true? v))
                                      ;; Or is it a keyword and found in the current env?
                                      (and (keyword? v)
                                           (not (nil? (-> env v)))))])
                             vars))]
    (if-not (empty? missing)
      ;; If some of the `requires` interfaces are missing, don't bother running it, and
      ;; return nil.
      (log :error
        (format "Some required inputs are missing for module `%s`.\nMissing: %s\n\nEnvironment:\n%s"
                (:name m)
                (into [] (map first missing))
                env))
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
           :env (reduce (fn [env return]
                          (merge env
                            (reduce (fn [env checkpoint]
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
                                    env
                                    return)))
                        {}
                        returns)}
          )))))


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
  [m config hkey & [reports]]
  (cond
    (map? m)
    (if-let [report ((keyword hkey) m)]
      (conj reports report)
      (reduce
        (fn [reports [k v]]
          (harvest v config hkey reports))
        reports m))

    (coll? m)
    (reduce
      (fn [reports x]
        (harvest x config hkey reports))
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
  (let [config (assoc config :env (merge @genv (:env config)))]
    (cond
      (string? m)
      (swap m (:env config))

      (keyword? m)
      (keyword (swap (subs (str m) 1) (:env config)))

      (symbol? m)
      (symbol (swap (name m) (:env config)))

      (map? m)
      (cond

        ;; Contains :resolve
        (contains? m :resolve)
        (-> config :data-connector
          (resolve-program (evaluate (:resolve m) config))
          :main
          (evaluate config))

        ;; Contains :children
        (contains? m :children)
        ;; `dissoc` the :then entry and run it as if it didn't exist.
        ;; Then, extract the new environment from the result, and run the 
        ;; :then entry with that environment.
        (let [patch (evaluate (dissoc m :children) config)
              env (extract-env patch (:env config))]
          ;(assoc patch
          ;       :dependents
          (conj [patch]
                 (doall (pmap #(evaluate % (assoc config :env env))
                              (:children m)))))

        ;; Functions and special forms
        (->> (keys m) (some #(-> % str (subs 1) (.startsWith "~("))))
        (let [fun-entry (->> m (filter #(-> % key str (subs 1) (.startsWith "~("))) first)
              fun (-> fun-entry key str (subs 3) symbol)
              do-statements (fn [statements env]
                              (loop [statements statements last-ret nil]
                                (if (empty? statements)
                                  last-ret
                                  (let [ret (evaluate (first statements) (assoc config :env env))]
                                    (recur (drop 1 statements) ret)))))]
          (condp = fun
            ;; Special forms
            'if
            (evaluate
              ((if (evaluate (-> fun-entry val first) config)
                 second #(nth % 2 nil))
                (-> fun-entry val))
              config)

            'let
            (let [bindings (-> fun-entry val first)]
              (if-not (even? (count bindings))
                (throw (IllegalArgumentException. "`let` requires an even number of bindings"))
                (let [bindings (-> fun-entry val first)
                      statements (->> fun-entry val (drop 1))
                      env (loop [bindings bindings
                                 env (:env config)]
                            (if (empty? bindings)
                              env
                              (recur (drop 2 bindings)
                                     (merge env
                                            {(evaluate (keyword (first bindings)) config)
                                             (evaluate (second bindings) config)}))))]
                  (do-statements statements env))))

            'fn
            (fn [& argv]
              (let [args (-> fun-entry val first) ;; list of strings
                    statements (->> fun-entry val (drop 1))
                    env (loop [args args
                               argv argv
                               env (:env config)]
                          (if (empty? args)
                            env
                            (let [env (merge env
                                             {(evaluate (keyword (first args)) config)
                                              (evaluate (first argv) config)})]
                              (recur (drop 1 args)
                                     (drop 1 argv)
                                     env))))]
                (do-statements statements env)))

            (symbol "#")
            (fn [& argv]
              (let [env (merge (:env config)
                               (into {}
                                 (map-indexed
                                   (fn [idx item] [(keyword (str idx)) (evaluate item config)])
                                   argv)))]
                (do-statements (-> fun-entry val) env)))

            'for
            (let [coll (-> fun-entry val first second (evaluate config))]
              (doall
                (map
                  #(let [env (merge (:env config) {(keyword (-> fun-entry val ffirst (evaluate config))) %})]
                    (evaluate (->> fun-entry val (drop 1)) (assoc config :env env)))
                  coll)))

            'or
            (loop [args (-> fun-entry val)]
              (when-not (empty? args)
                (let [yield (evaluate (first args) config)]
                  (if-not yield
                    (recur (drop 1 args))
                    yield))))

            'and
            (loop [args (-> fun-entry val)
                   last-yield nil]
              (if (empty? args)
                last-yield
                (let [yield (evaluate (first args) config)]
                  (if-not yield
                    false
                    (recur (drop 1 args) yield)))))

            'parallel
            (upmap #(evaluate % config)
              (-> fun-entry val))

            'upmap
            (apply upmap (evaluate (-> m first val) config))

            'log
            (let [args (-> fun-entry val)]
              (log (evaluate (-> args first keyword) config)
                   (apply str
                     (map #(evaluate % config) (drop 1 args)))))

 
            ;; Functions
            (let [yield (apply (resolve fun) (evaluate (-> m first val) config))]
              (if (coll? yield)
                ;; Laziness disabled.
                (doall yield)
                yield))))


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
        (into (empty m)
          (map (fn [[k v]]
                 (if (= k :assert)
                   [k v]
                   [(evaluate k config) (evaluate v config)]))
               m)))

      ;; Other collections
      (coll? m)
      ((if (seq? m) reverse identity)
        (into (empty m) (doall (map (fn [item] (evaluate item config)) m))))
      
      :else m)))


(defn run-program
  [config]
  ;; A program's main is a collection of one or more entry point groups
  ;; that are kicked off concurrently.
  (let [program (if (map? (:program config))
                  (:program config)
                  (try
                    (resolve-program (:data-connector config) (evaluate (:program config) config))
                    (catch Exception e
                      (die
                        "The configuration must have a program property, which must resolve to a valid yaml document:"
                        (.getMessage e)))))
        config (assoc config :env (evaluate (:env config) config))
        exit (try
               (when-let [main (:main program)]
                 (doall (pmap #(evaluate % config) main)))
               (catch java.util.concurrent.ExecutionException ae
                 (loop [cause ae]
                   (if (nil? cause)
                     (throw ae)
                     (if (= (type cause) java.lang.AssertionError)
                       (log :error (str cause))
                       (recur (.getCause cause))))))
               (finally
                 (log :info "Cleaning up.")
                 (upmap
                   (fn [[k n]]
                     (when (:data n)
                       (when (-> @(:data n) :options :destroy-on-exit true?)
                         (destroy n))))
                   @(-> config :node-manager :nodes))
                 (log :info "Done cleaning up.")))]
    exit
    ))


(def ascii-art "
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»»»»*************»»»***»»»»»»»»»»»»»»»*********»»»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»*************»»»***»»»»»»»»»»»»»*************»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»***»»»»»»»»»»»»»***»»»»»»»***»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»*************»»»***»»»»»»»»»»»»»*************»»»»»***»»»***»»»»»»»»»»»»»»~
~»»»»*************»»»***»»»»»»»»»»»»»*************»»»»»»»*****»»»»»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»***»»»»»»»»»»»»»***»»»»»»»***»»»»»***»»»***»»»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»***»»»»»»»»»»»»»***»»»»»»»***»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»***»»»»»»»»»»»»»***»»»»»»»***»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»*************»»»***»»»»»»»***»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»*************»»»***»»»»»»»***»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
")

(defn run
  [config]
  ;(log :info ascii-art)
  ;; Google "clojure stencil" (for the stencil library) for help finding
  ;; what these options can be. It's not particularly easy. -- tjb
  (swap! parser-options #(merge % (or (:parser-options config) {})))
  (try
    ;; Dynamically require the namespace containing the data-connector
    ;; and call its constructor; if no data-connector is specified, then
    ;; use the built-in FileDataConnector.
    (let [data-connector (if (:data-connector config)
                           (let [dc-ctor (do (-> config :data-connector symbol require)
                                             (-> config :data-connector (str "/connector") symbol resolve))]
                             (if-not dc-ctor
                               ;; we can't proceed without any way of resolving the location of the program
                               (die (format "Constructor for data connector `%s` not found" (:data-connector config)))
                               (dc-ctor)))
                           (FileDataConnector.))
          effective (let [effective (java.util.Date.
                                      (or (:effective config)
                                          (-> (java.util.Date.) .getTime)))]
                      (log :info (str "Seed: " (.getTime effective)))
                      effective)
          result (run-program (assoc config ;; This timestamp is also intended to be used (via .getTime)
                                            ;; as a seed for any pseudorandom number generation, so that
                                            ;; randomized testing can be retested as deterministically
                                            ;; as possible.
                                            :effective effective
                                            ;; Instantiate a node manager with an empty node map as an atom.
                                            ;; Takes a seed.
                                            :node-manager (node-manager effective)
                                            ;; The data connector is for resolving literal
                                            ;; representations of particular resources. In the future,
                                            ;; database persistence should be supported, with
                                            ;; continued support for file based configuration, too.
                                            :data-connector data-connector))]
      (log :info (str "Seed: " (.getTime effective)))
      result)
    
    (catch java.io.FileNotFoundException fnfe
      (die "Resource could not be found:" (.getMessage fnfe)))
    ))

(defn -main [& argv]
  (run (yaml/parse-string (slurp (first argv)))))


