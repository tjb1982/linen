(ns flax.core
  (:require [clojure.core.async :as a :refer [chan go >! >!! <! <!! alts!! alts! go-loop thread]]
            [clj-yaml.core :as yaml]
            [cheshire.core :as json]
            [clojure.pprint :refer [pprint]]
            [flax.logutil :refer :all]
            [flax.protocols :refer :all]
            [flax.node-manager :refer :all]
            [stencil.parser :refer [parse]]
            [stencil.core :refer [render]]
            )
  (:import flax.node_manager.NodeManager
           flax.node_manager.LocalConnector
           flax.logutil.StandardLogger
           flax.logutil.ClojureToolsLogger)
  (:gen-class))


(def parser-options (atom {:tag-open "~{" :tag-close "}"}))
(def help
  (str "usage: flax path/to/properties.yaml"))
(def genv (atom
            (into {}
              (for [[k v] (System/getenv)]
                [(keyword k) v]))))
(def logger (ClojureToolsLogger.))


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
            stdout (stream-to-reader (.getInputStream proc))]
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
  (merge c (select-keys m [:throw :skip :log])))


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

(defn run-checkpoint
  [checkpoint config]
  (let [;; First we snag the flags from the checkpoint
        ;; and assoc them to the config
        config (assoc-flags config checkpoint)
        ;; Then we assoc the config to the checkpoint, so the flags are visible
        ;; to the `invoke` function. We also evaluate the checkpoint to make
        ;; sure the variables are mapped to their appropriate values from the
        ;; environment.
        checkpoint (assoc-flags (evaluate checkpoint config) config)]
    (if (:nodes checkpoint)
      ;; Run the checkpoint on some nodes in parallel.
      (doall
        (pmap
          #(-> config :node-manager
            (get-node %)
            (invoke (assoc checkpoint :out [(:out checkpoint)
                                            (:out %)]
                                      :err [(:err checkpoint)
                                            (:err %)]
                                      :exit [(:exit checkpoint)
                                             (:exit %)]
                                      :user (:user %))))
          (:nodes checkpoint)))
      ;; Run it on the local host, returning a list as if it were run on
      ;; potentially several nodes.
      (list (-> local-connector (invoke checkpoint))))
    ))


(defn run-module
  [m c]
  ;; Check that the required params exist in the env for this module to run.
  ;; Either the param exists, or the module defines a default to use instead.
  (let [vars (map #(or (contains? % :default)
                       (-> % :key keyword))
                   (-> m :requires))
        missing (remove #(-> % second true?)
                        (map (fn [v]
                               [v (or ;; Is it both boolean and true, signifying a default value exists?
                                      (and (bool? v)
                                           (true? v))
                                      ;; Or is it a keyword and found in the current env?
                                      (and (keyword? v)
                                           (not (nil? (-> c :env v)))))])
                             vars))]
    (if-not (empty? missing)
      ;; If some of the `requires` interfaces are missing, don't bother running it, and
      ;; return nil.
      (-> logger
        (log :error
          (format "Some required inputs are missing for module `%s`.\nMissing: %s\n\nEnvironment:\n%s"
                  (:name m)
                  (into [] (map first missing))
                  (:env c))))
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
                        checkpoints
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
                                            out (-> checkpoint :out :value)
                                            err (-> checkpoint :err :value)
                                            exit (-> checkpoint :exit :value)]
                                        (reduce (fn [env [k v]]
                                                  (if-not (nil? k)
                                                    (assoc env k v) env))
                                                env
                                                [[out-arr (conj (get env out-arr) out)]
                                                 [(-> checkpoint :out :keys second keyword) out]
                                                 [err-arr (conj (get env err-arr) err)]
                                                 [(-> checkpoint :err :keys second keyword) err]
                                                 [exit-arr (conj (get env exit-arr) exit)]
                                                 [(-> checkpoint :exit :keys second keyword) exit]])))
                                    env
                                    return)))
                        {}
                        returns)}
          )))))


(defn extract-env
  [m & [env]]
  (cond

    (map? m)
    (if-let [report (:report m)]
      (let [out (evaluate (:out m)
                          {:env (merge env
                                       (-> m :report :env))})]
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


(defn exits
  [m & [es]]
  (cond
    (map? m)
    (if-let [e (-> m :exit :value)]
      (conj es e)
      (reduce
        (fn [es [k v]]
          (exits v es))
        es m))

    (coll? m)
    (reduce
      (fn [es x]
        (exits x es))
      es m)

    :else es))


(defn evaluate
  [m config]
  (cond
    (string? m)
    (swap m (:env config))

    (keyword? m)
    (keyword (swap (subs (str m) 1) (:env config)))

    (symbol? m)
    (symbol (swap (name m) (:env config)))

    (map? m)
    (cond

      ;; Contains :then
      (contains? m :then)
      ;; `dissoc` the :then entry and run it as if it didn't exist.
      ;; Then, extract the new environment from the result, and run the 
      ;; :then entry with that environment.
      (let [patch (evaluate (dissoc m :then) config)
            env (extract-env patch (:env config))]
        (concat [patch]
                (doall (pmap #(evaluate % (assoc config :env env))
                             (:then m)))))

      ;; Functions and special forms
      (->> (keys m) (some #(-> % str (subs 1) (.startsWith "~("))))
      (let [fun-entry (->> m (filter #(-> % key str (subs 1) (.startsWith "~("))) first)
            fun (-> fun-entry key str (subs 3) symbol)]
        (condp = fun
          ;; Special forms
          'if
          (eval (conj (evaluate (-> fun-entry val) config) fun))

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
              (loop [statements statements last-ret nil]
                (if (empty? statements)
                  last-ret
                  (let [ret (evaluate (first statements) (assoc config :env env))]
                    (recur (drop 1 statements) ret)))))) 

          (symbol "#")
          (fn [& argv]
            (let [env (merge (:env config)
                             (into {}
                               (map-indexed
                                 (fn [idx item] [(keyword (str idx)) (evaluate item config)])
                                 argv)))]
              (loop [statements (-> fun-entry val) last-ret nil]
                (if (empty? statements)
                  last-ret
                  (let [ret (evaluate (first statements) (assoc config :env env))]
                    (recur (drop 1 statements) ret))))))

          'for
          (let [coll (-> fun-entry val first second (evaluate config))]
            (doall
              (map
                #(let [env (merge (:env config) {(keyword (-> fun-entry val ffirst (evaluate config))) %})]
                  (evaluate (->> fun-entry val (drop 1)) (assoc config :env env)))
                coll)))
              
          ;; Functions
          (let [yield (apply (resolve fun) (evaluate (-> m first val) config))]
            (if (coll? yield)
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
        (assoc m :report report))

      ;; All other maps
      :else
      (into (empty m)
        (map (fn [[k v]]
               [(evaluate k config) (evaluate v config)])
             m)))

    ;; Other collections
    (coll? m)
    (reverse (into (empty m) (doall (map (fn [item] (evaluate item config)) m))))
    
    :else m))


(defn run-program
  [config]
  ;; A program's main is a collection of one or more entry point groups
  ;; that are kicked off concurrently.
  (let [program (try
                  (resolve-program (:data-connector config) (evaluate (:program config) config))
                  (catch Exception e
                    (die
                      "The configuration must have a program property, which must be a path to a valid yaml document:"
                      (.getMessage e))))
        config (assoc config :env (evaluate (:env config) config))
        exit (when-let [main (:main program)]
               (doall (pmap #(evaluate % config) main)))]
    (doall
      (pmap
        (fn [[k n]]
          (when (-> @(:node n) :options :destroy-on-exit true?)
            (destroy n)))
        @(-> config :node-manager :nodes)))
    exit
    ))


(defn run
  [config]
  ;; Google "clojure stencil" (for the stencil library) for help finding
  ;; what these options can be. It's not particularly easy. -- tjb
  (swap! parser-options #(merge % (or (:parser-options config) {})))
  (try
    ;; dynamically require the namespace containing the data-connector
    (require (symbol (:data-connector config)))

    (let [dc-ctor (-> config :data-connector (str "/connector") symbol resolve)]
      (if-not dc-ctor
        ;; we can't proceed without any way of resolving the location of the program
        (die (format "Constructor for data connector `%s` not found" (-> config :data-connector)))
        (let [effective (let [effective (java.util.Date.
                                          (or (:effective config)
                                              (-> (java.util.Date.) .getTime)))]
                          (log logger :info (str "Seed: " (.getTime effective)))
                          effective)
              return (run-program (assoc config ;; This timestamp is also intended to be used (via .getTime)
                                                ;; as a seed for any pseudorandom number generation, so that
                                                ;; randomized testing can be retested as deterministically
                                                ;; as possible.
                                                :effective effective
                                                ;; Instantiate a node manager with an empty node map as an atom.
                                                :node-manager (node-manager effective 0 logger)
                                                ;; The data connector is for resolving literal
                                                ;; representations of particular resources. In the future,
                                                ;; database persistence should be supported, with
                                                ;; continued support for file based configuration, too.
                                                :data-connector (dc-ctor)))]

          ;; `return` is a fully evaluated data structure based on the data
          ;; structure of the program.

          ;; Write the raw return value of the entire program to the
          ;; logs directory.
          (spit "target/logs/raw.json" (json/generate-string return))

          (spit "target/logs/env.json" (json/generate-string (evaluate (:env config) config)))
          ;; Write any harvested values to the logs directory.
          (spit "target/logs/harvest.json"
                (json/generate-string
                  (reduce
                    (fn [harvested rset]
                      (merge harvested rset))
                    {}
                    (map (fn [hkey]
                           {(keyword hkey) (harvest return config hkey [])})
                         (-> config :harvest)))))

          ;; Write the failing returns to the logs directory and exit with
          ;; the count of the failures.
          (let [failures (->> (returns return)
                              (remove #(-> % :exit :value nil?))
                              (filter #(and (-> % :throw)
                                            (-> % :exit :value zero? not))))
                num-failures (count failures)]
            (spit "target/logs/failures.json" (json/generate-string failures))
            (log logger :info (str "Failures: " num-failures))
            (log logger :info (str "Seed: " (.getTime effective)))

            ;; Allow time for agents to finish logging.
            (Thread/sleep 1000)
            ;; Kill the agents so the program exits without further delay.
            (shutdown-agents)

            num-failures)
          )))
    (catch java.io.FileNotFoundException fnfe
      (die "Resource could not be found:" (.getMessage fnfe)))
    ))

