(ns flax.main
  (:require [clojure.core.async :as a :refer [chan go >! >!! <! <!! alts!! alts! go-loop thread]]
            [clj-yaml.core :as yaml]
            [cheshire.core :as json]
            [clojure.pprint :refer [pprint]]
            [flax.logutil :refer :all]
            [flax.protocols :refer :all]
            [flax.node-manager]
            [stencil.parser :refer [parse]]
            [stencil.core :refer [render]]
            )
  (:import flax.node_manager.NodeManager
           flax.node_manager.LocalConnector
           flax.logutil.StandardLogger)
  (:gen-class))


(def parser-options (atom {:tag-open "~{" :tag-close "}"}))
(def help
  (str "usage: flax path/to/properties.yaml"))
(def genv (atom {}) #_(atom
            (into {}
              (for [[k v] (System/getenv)]
                [(keyword k) v]))))
(def logger (StandardLogger.))


(defn bool?
  [tester]
  (-> tester type (= java.lang.Boolean)))


(defn die
  [& args]
  (apply println args)
  (System/exit 1))


(defn node-list
  [nodes]
  (if (instance? Integer nodes)
    (str nodes)
    (name nodes)))


(defn swap
  [s env]
  (let [env (merge @genv env)]
    (cond
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


#_(defn do-config-groups
  [fun m c]
  (let [groups (:groups m)]
    (doall
      (mapcat
        #(let [config (assoc-flags c %)]
          (doall
            (pmap (fn [member] (apply fun member))
                 (map (fn [x] [x config])
                      (:group %)))))
        groups))))

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
                                      :user (:user %))))
          (:nodes checkpoint)))
      ;; Run it on the local host, returning a list as if it were run on
      ;; potentially several nodes.
      (list (-> (LocalConnector.) (invoke checkpoint))))
    ))


(defn run-module
  [m c]
  ;; Check that the required params exist in the env for this module to run.
  ;; Either the param exists, or the module defines a default to use instead.
  (let [vars (map #(or (contains? % :default)
                       (-> % :key keyword))
                   (-> m :requires))
        missing (remove #(-> % second true?)
                        (map (fn [v] [v (or ;; Is it both boolean and true, signifying a default value exists?
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
          ;; A return is a list of checkpoints, one for each node it was run on.
          {:returns returns
           :env (reduce (fn [env return]
                          (merge env
                            (reduce (fn [env checkpoint]
                                      (let [out-arr (-> checkpoint :out :keys first keyword)
                                            err-arr (-> checkpoint :err :keys first keyword)
                                            out (-> checkpoint :out :value)
                                            err (-> checkpoint :err :value)]
                                        (reduce (fn [env [k v]]
                                                  (if-not (nil? k)
                                                    (assoc env k v) env))
                                                env
                                                [[out-arr (conj (get env out-arr) out)]
                                                 [(-> checkpoint :out :keys second keyword) out]
                                                 [err-arr (conj (get env err-arr) err)]
                                                 [(-> checkpoint :err :keys second keyword) err]])))
                                    env
                                    return)))
                        {}
                        returns)}
          )))))


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
      ;; Functions and special forms
      (-> m first key str (subs 1) (.startsWith "~("))
      (let [fun (-> m first key str (subs 3) symbol)]
        (condp = fun
          'if
          (eval (conj (evaluate (-> m first val) config) fun))
          'for
          (let [coll (-> m first val first second (evaluate config))]
            (for [x coll]
              (let [env (merge (:env config) {(keyword (-> m first val ffirst (evaluate config))) x})]
                (evaluate (-> m first val second) (assoc config :env env)))))
          ;; Functions
          (apply (resolve fun) (evaluate (-> m first val) config))))

      ;; Modules
      (contains? m :module)
      (let [config (assoc config :env (merge (:env config)
                                             (evaluate (:in m) config)))
            module (resolve-module (:data-connector config) (evaluate (:module m) config))
            ;; Run the module. Each module returns a report with a new env
            ;; based on what it requires/provides, and a list of return values
            ;; for each checkpoint.
            report (run-module module (assoc-flags config m))
            ;; The new env is made up of the old env, with any keys being
            ;; overridden by the :out keys in the patch. All the others are
            ;; ignored.
            env (merge (:env config) (evaluate (:out m)
                                               (assoc config :env (merge (:env config)
                                                                         (:env report)))))
            ;; Flat list of reports from the tree of patches.
            reports (conj
                      (flatten
                        (do-groups evaluate
                          (:then m)
                          (if (map? (:then m))
                            (assoc-flags (assoc config :env env) (:then m))
                            (assoc config :env env))))
                      report)]
        (assoc m :reports reports))

      ;; All other maps
      :else
      (into (empty m)
        (map (fn [[k v]]
               [(evaluate k config) (evaluate v config)])
             m)))

    ;; Other collections
    (coll? m)
    (reverse (into (empty m) (map (fn [item] (evaluate item config)) m)))
    
    :else m))


(defn run-program
  [config]
  ;; A program's main is a collection of one or more entry points
  ;; that are all kicked off in parallel.
  (let [program (try
                  (resolve-program (:data-connector config) (:program config))
                  (catch Exception e
                    (die
                      "The configuration must have a program property, which must be a path to a valid yaml document:"
                      (.getMessage e))))]

    (when-let [main (:main program)]
      (doall
        (pmap
          #(evaluate % config)
           main)))))


(defn -main
  [& argv]
  (if (= (first argv) "help")
    ;; the system will exit(0) by default
    (println help)
    (if (empty? argv)
      ;; sorry, we need the config to proceed
      (die help)
      (let [config (try (-> (first argv) slurp yaml/parse-string)
                     (catch Exception e
                       (die
                         (format "The first argument must be a path to a valid yaml document: %s\n%s"
                           (.getMessage e)
                           help))))]
        (swap! parser-options #(merge % (or (:parser-options config) {})))
        (try
          ;; dynamically require the namespace containing the data-connector
          (require (symbol (:data-connector config)))

          (let [dc-ctor (-> config :data-connector (str "/connector") symbol resolve)]
            (if-not dc-ctor
              ;; we can't proceed without any way of resolving the location of the program
              (die (format "Constructor for data connector `%s` not found" (-> config :data-connector)))
              (let [effective (java.util.Date.
                                (or (:effective config)
                                    (-> (java.util.Date.) .getTime)))
                    return (run-program (assoc config :env (evaluate (:env config) config)
                                                      ;; This timestamp is also intended to be used (via .getTime)
                                                      ;; as a seed for any pseudorandom number generation, so that
                                                      ;; randomized testing can be retested as deterministically
                                                      ;; as possible.
                                                      :effective effective
                                                      ;; Instantiate a node manager with an empty node map as an atom.
                                                      :node-manager (NodeManager. (atom {}) effective 0 logger)
                                                      ;; Again, the data connector is for resolving a string
                                                      ;; representation of particular resources. In the future,
                                                      ;; database persistence should be supported, with
                                                      ;; continued support for file based configuration, too.
                                                      :data-connector (dc-ctor)))]
                (let [x (->> return first)]
                  (spit "foo.json" (json/generate-string return)))
                ;; Allow time for agents to finish logging.
                (Thread/sleep 1000)
                ;; Kill the agents so the program exits without delay.
                (shutdown-agents)
                ;; `return` is a list (the main group) of lists of modules
                (System/exit (count (remove #(or (nil? %)
                                                 (zero? %))
                                            (mapcat (fn [entry]
                                                      (mapcat (fn [module]
                                                                (mapcat (fn [ret]
                                                                          (map (fn [node]
                                                                                 (:exit node))
                                                                               ret))
                                                                        (:returns module)))
                                                              entry))
                                                    return))))
                )))
          (catch java.io.FileNotFoundException fnfe
            (die "Resource could not be found:" (.getMessage fnfe)))
          )))))

