(ns flax.main
  (:require [clojure.core.async :as a :refer [chan go >! >!! <! <!! alts!! alts! go-loop thread]]
            [me.raynes.conch :as sh]
            [clj-yaml.core :as yaml]
            [clojure.pprint :refer [pprint]]
            [cheshire.core :as json]
            [flax.core :refer :all]
            [flax.logutil :refer [log *log-script*]]
            [flax.protocols :refer :all]
            [flax.node-manager]
            [stencil.parser :refer [parse]]
            [stencil.core :refer [render]]
            )
  (:import flax.node_manager.NodeManager
           flax.node_manager.LocalConnector)
  (:gen-class))


(def parser-options (atom {}))
(def FAIL 1)

(def help
  (str "usage: flax path/to/properties.yaml"))


(defn bool?
  [tester]
  (-> tester type (= java.lang.Boolean)))

(defn die
  [& args]
  (apply println args)
  (System/exit FAIL))


(defn skipstr
  [& args]
  (str (when *skip* "# ")
       (apply str args)))


(defn bash-printf
  [string]
  (sh/with-programs [bash]
    (bash "-c"
      (str "printf \"" string "\"")
    {:env @genv})))


(defn node-list
  [nodes]
  (if (instance? Integer nodes)
    (str nodes)
    (name nodes)))


;;(defn add-to-genv
;;  [k v]
;;  (swap! genv (fn [old]
;;                (assoc old k v))))
;;
;;
;;(defn run-checkpoint
;;  [t]
;;  (wrap-binding t
;;    (sh/with-programs [bash]
;;      (let [checkpoint-exec-string (bash-printf (:exec t))]
;;        (if (and (:cluster-name t)
;;                 (not= (:cluster-name t) "localhost"))
;;          (ctool-run (assoc t :exec checkpoint-exec-string))
;;          (if *dry-run*
;;            (log {:stdout (skipstr checkpoint-exec-string)})
;;            (let [ret (bash "-c" (:exec t) {:env @genv :verbose true})]
;;
;;              ;;(log ret)
;;              (when-not (zero? (count (:stdout ret)))
;;                (log {:stdout (skipstr checkpoint-exec-string)})
;;                (log {:stderr (:stdout ret)})
;;                (when (:out t) (add-to-genv (:out t) (:stdout ret))))
;;
;;              (when-not (zero? (count (:stderr ret)))
;;                (log {:stdout (skipstr checkpoint-exec-string)})
;;                (log {:stderr (:stderr ret)})
;;                (when (:err t) (add-to-genv (:err t) (:stderr ret))))
;;
;;              @(:exit-code ret))))))))


(defn swap
  [s env]
  (let [env (merge @genv env)]
    (cond
      ;; If `s` starts with ~@, then replace it with the data held by the
      ;; variable (i.e., not a string interpolation of it).
      (.startsWith s "~@") (-> s (subs 2) keyword env)
      ;; Interpolate.
      :else (render
              (parse s @parser-options)
              env))))


(defn swapwalk
  [m env]
  (clojure.walk/postwalk
    #(cond
       (string? %) (swap % env)
       (keyword %) (keyword (swap (name %) env))
       :else %)
    m))


(defn assoc-flags
  [c m]
  (merge c (select-keys m [:throw :skip :log])))


(defn do-groups
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


(defn run-checkpoint
  [checkpoint config]
  (let [;; First we snag the flags from the checkpoint
        ;; and assoc them to the config
        config (assoc-flags config checkpoint)
        ;; Then we assoc the config to the checkpoint, so the flags are visible
        ;; to the `invoke` function. We also swapwalk the checkpoint to make
        ;; sure the variables are mapped to their appropriate values from the
        ;; environment.
        checkpoint (assoc-flags (swapwalk checkpoint (:env config)) config)]
    (if (:nodes checkpoint)
      ;; Run the checkpoint on some nodes in parallel.
      (doall
        (pmap
          #(let [return (-> config :node-manager (get-node %) (invoke checkpoint))]
            (if (and (not (nil? return))
                     (-> config :throw))
              (assoc checkpoint :exit return)
              checkpoint))
          (:nodes checkpoint)))
      ;; Run it on the local host, returning a list as if it were run on
      ;; potentially several nodes.
      (list (-> (LocalConnector.) (invoke checkpoint))))
    ))


(defn run-module
  [m c]
  ;; Check that the required params exist in the env for this module to run.
  ;; Either the param exists, or the module defines a default to use instead.
  (if (some false?
        (map #(or ;; Is it both boolean and true, signifying a default value exists?
                  (and (bool? %)
                       (true? %))
                  ;; Or is it a keyword and found in the current env?
                  (and (keyword? %)
                       (not (nil? (-> c :env %)))))
             (map #(or (contains? % :default)
                       (-> % :key keyword))
                  (-> m :requires))))
    ;; If some of the `requires` interfaces are missing, don't bother running it, and
    ;; return the report as a failure.
    (log (format "Some required inputs are missing for module `%s`.\n%s" (:name m) (:env c)))
    ;  {:returns '()
    ;   :env (:env c)
    ;   :status FAIL})
    ;; All `requires` interfaces exist, so we're a "go."
    ;; Run the checkpoints. Each checkpoint run will return its return code and
    ;; the vars that should be added to the env.
    ;; Scoop up the "provides" and "requires" values from the local env and put
    ;; them into a new env.
    (when-let [checkpoints (-> m :checkpoints)]
      (let [returns (do-groups run-checkpoint
                      checkpoints
                      (assoc-flags c checkpoints))]
        returns))))


(defn run-patch-tree
  [patch config]
  (let [;; Augment the current env with the patch's materialized interface.
        config (assoc config :env (merge (:env config) (:interface patch)))
        ;; Get the module from its data source.
        module (resolve-module (-> config :data-connector) (-> patch :module))
        ;; Run the main module. Each module returns a report with a new env
        ;; based on what it requires/provides, and a list of return values
        ;; for each checkpoint.
        returns (run-module module (assoc-flags config patch))]
    ;; here we want to deal with each patch, determining whether it should be isolated or not.
    (clojure.pprint/pprint returns)
    ))


(defn run-program
  [config]
  ;; A program's main is a collection of one or more entry points
  ;; that are all kicked off in parallel.
  (when-let [main (-> config :program :main)]
    (doall
      (pmap
        #(run-patch-tree
          ;; Swap and/or interpolate the variables contained in the patch with
          ;; the contents of the current env, skipping the `:next` list, as its
          ;; env will be augmented by the vars created by the parent.
          (merge %
                 (swapwalk (dissoc % :then)
                           (:env config)))
          config)
        main))))


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
              (let [dc (dc-ctor)
                    program (try
                              (resolve-program dc (:program config))
                              (catch Exception e
                                (die
                                  "The configuration must have a program property, which must be a path to a valid yaml document:"
                                  (.getMessage e))))
                    effective (java.util.Date.
                                (or (:effective config)
                                    (-> (java.util.Date.) .getTime)))
                    return (run-program (assoc config ;; The program itself. A tree of synchronized patches that
                                                      ;; model changes in the state of the system and various
                                                      ;; asynchronous interactions.
                                                      :program program
                                                      ;; Merge the system env into the local env we will be managing.
                                                      :env (merge (into {} (for [[k v] (System/getenv)] [(keyword k) v]))
                                                                  (:env config))
                                                      ;; This timestamp is also intended to be used (via .getTime)
                                                      ;; as a seed for any pseudorandom number generation, so that
                                                      ;; randomized testing can be retested as deterministically
                                                      ;; as possible.
                                                      :effective effective
                                                      ;; Instantiate a node manager with an empty node map as an atom.
                                                      :node-manager (NodeManager. (atom {}) effective 0)
                                                      ;; Again, the data connector is for resolving a string
                                                      ;; representation of particular resources. In the future,
                                                      ;; database persistence should be supported, with
                                                      ;; continued support for file based configuration, too.
                                                      :data-connector dc))]
                ;; Allow time for agents to finish logging.
                (Thread/sleep 1000)
                ;; Kill the agents so the program exits without delay.
                (shutdown-agents)
                ;; TODO Not sure yet what exactly will be returned and how to handle it.
                (System/exit (count (remove nil? (:exits return)))))))
          (catch java.io.FileNotFoundException fnfe
            (die "Resource could not be found:" (.getMessage fnfe)))
          )))))

