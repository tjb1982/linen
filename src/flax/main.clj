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


(defn do-groups
  [fun groups]
  (doseq [collection groups]
    (let [group (:group collection)]
      (wrap-binding collection
        (doall (pmap fun group)))
      )))


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
      (.startsWith s "~@") (-> s (subs 2) keyword env)
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
  (merge c (select-keys m [:throw :skip])))


(defn do-groups
  [fun m c]
  (let [groups (-> m :groups)]
    ;; run all groups sequentially
    (doall (for [group groups]
      (let [c (assoc-flags c group)]
        ;; run all x in (:group group) in parallel, passing x and the config object as args
        ;; TODO map for now, pmap or core.async later
        (doall (map #(apply fun %)
                    (map (fn [x] [x c])
                         (:group group))))
        )))))


(defn run-checkpoint
  [checkpoint config]
  (let [c (assoc-flags config checkpoint)
        cp (swapwalk checkpoint (-> c :env))]
    (if (:nodes cp)
      ;; run the checkpoint on some nodes in parallel
      ;; TODO make this run in parallel
      (doall
        (for [node (:nodes cp)]
          (-> c :node-manager (get-node node) (invoke cp))))
      ;; run it on the local host
      (-> (LocalConnector.) (invoke cp))
      )))


(defn run-module
  [m c]
  ;; check that the required params exist in the env for this module to run
  ;; run the module
  ;; scoop up the "provides" and "requires" values from the local env and put them into a new environment
  (if (some false?
        (map #(or (and (-> % type (= java.lang.Boolean))
                       (true? %))
                  (and (-> % type (not= java.lang.Boolean))
                       (not (nil? (-> c :env %)))))
             (map #(or (contains? % :default)
                       (-> % :key keyword))
                  (-> m :requires))))
    ;; some of the `requires` interfaces are missing, so don't bother running it, and
    ;; return the fail state
    (do
      (println (format "Some required inputs are missing for module `%s`." (:name m)))
      FAIL)
    ;; all `requires` interfaces exist, so we're a "go"
    (when-let [checkpoints (-> m :checkpoints)]
      (do-groups run-checkpoint
                 checkpoints
                 (assoc-flags c checkpoints)))))


(defn run-patch-tree
  [p c]
  (clojure.pprint/pprint p)
  (let [c (assoc c :env (merge (:env c) (:interface p)))
        module (resolve-module (-> c :data-connector) (-> p :module))
        return (run-module module (assoc-flags c p))]
    ;; here we want to deal with each patch, determining whether it should be isolated or not.
    ;; If so, then the clusters it should use should be a snapshot of the current clusters. It should, in effect,
    ;; branch away from the current clusters as if it were its own path. And the way to do this is to have every clone/snapshot/whatever
    ;; have its own name. With ctool this is going to be awful. But with lxc, we can really make this awesome. But what exactly
    ;; needs to happen? We need to have a list of clusters at any particular point (that follows this path) and when you come across
    ;; a directive to isolate it, you have to clone all of these clusters before running any concurrent tests.
    ;;(doseq [patch (-> p :next)]
    ;;  (run-patch patch dc cc))
    ))


(defn run-program
  [c]
  (when-let [main (-> c :program :main)]
    (let [effective (java.util.Date.
                      (or (-> c :program :effective)
                          (-> (java.util.Date.) .getTime)))]
      (run-patch-tree
        ;; swap and/or interpolate the variables contained in the patch with
        ;; the contents of the current env, skipping the `:next` list, as its
        ;; env will be augmented by the vars created by the parent.
        (merge main (swapwalk (dissoc main :next) (-> c :env)))
        ;; this `effective` timestamp will serve as the datetime of last modification
        ;; as well as the seed for any pseudorandom number generation.
        (assoc c :effective effective)))))


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
                    return (run-program (assoc config :program program
                                                      ;; merge the system env into the local env we will be managing
                                                      :env (merge (into {} (for [[k v] (System/getenv)] [(keyword k) v]))
                                                                  (:env config))
                                                      ;; instantiate a node manager with an empty node map as an atom
                                                      :node-manager (NodeManager. (atom {}))
                                                      ;; in the future, database persistence should be supported, with
                                                      ;; continued support for file based configuration, too
                                                      :data-connector dc))]
                ;; allow time for agents to finish logging
                (Thread/sleep 1000)
                ;; kill the agents so the program exits without delay
                (shutdown-agents)
                ;; checkpoints should conj nil to :exits if they were successful, anything else otherwise
                (System/exit (count (remove nil? (:exits return)))))))
          (catch java.io.FileNotFoundException fnfe
            (die "Dynamically required namespace could not be found:" (.getMessage fnfe)))
          )))))

