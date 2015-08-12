(ns yar.core
  (:require [clojure.core.async :as a :refer [chan go >! >!! <! <!! alts!! alts! go-loop thread]]
            [me.raynes.conch :as sh]
            [clj-yaml.core :as yaml]
            [clojure.pprint :refer [pprint]]
            [cheshire.core :as json]))


(def ^:dynamic *timestamp* nil)
(def ^:dynamic *dry-run* nil)
(def this-ns *ns*)

(def log-chan (chan))
(def logger
  (go-loop []
    (let [msg (<! log-chan)
          date (java.util.Date.)]
      (if (= (class msg) String)
        (println (str "\t" date ": " msg))
        (do
          (let [err (:stderr msg)]
            (when-not (-> err clojure.string/blank?)
              (binding [*out* *err*]
                (println (str "\t" date ": " err)))))
          (let [out (:stdout msg)]
            (when-not (-> out clojure.string/blank?)
              (println (str "\t" date ": " out)))))))
    (recur)))


(defn log
  [msg]
  (>!! log-chan msg)
  (when (:exit-code msg)
    @(:exit-code msg)))


(defn cluster-name
  [cluster]
  (str (or (:cluster-name cluster)
           (-> this-ns str (clojure.string/split #"\.") first))
       "-"
       (.getTime *timestamp*)))


(defn when-arg
  [value flag]
  (when value
    [flag value]))


(defn when-args
  [config arg-vals]
  (flatten (remove nil?
    (for [[k v] arg-vals]
      (when-arg (k config) v)))))


(defn launch-argv
  [cluster]
  (flatten
    (remove
      nil?
      ["launch"
       (when-args cluster {:platform "-p"
                           :instance-type "-i"
                           :jdk "-j"
                           :reserved-nodes "-r"})
       (when-let [image (:image cluster)]
         (str "--image=" image))
       (when (:blockstore-optimized cluster)
         "--blockstore-optimized")
       (when-let [crc (:create-retry-count cluster)]
         (str "--create-retry-count=" crc))
       (when (:force cluster)
         "--force")
       (when (:non-interactive cluster)
         "--non-interactive")
       (when-let [tags (:tags cluster)]
         (str "--tags="
              (json/generate-string
                (:tags cluster))))
       (when (:keep-api-proxy cluster)
         "--keep-api-proxy")
       (cluster-name cluster)
       (or (:num-nodes cluster) 1)])))


(defn install-argv
  [cluster product]
  (flatten
    (remove nil?
      ["install"
       (when-args product
                  {:multi-dc "-m"
                   :config-file "-x"
                   :snitch "-z"
                   :tar-url "-t"
                   :install-type "-i"
                   :repo-alias "-a"
                   :repo-url "-r"
                   :branch-name "-b"
                   :num-seeds-per-dc "-s"
                   :percent-analytics "-y"
                   :percent-search "-e"
                   :percent-searchanalytics "-g"
                   :percent-spark "-k"
                   :version-or-branch "-v"
                   :cass-package-version "-c"
                   :num-tokens "-n"
                   :partitioner "-p"})
       (when-let [brn (:branch-repo-name product)]
         (str "--branch-repo=" brn))
       (when (:spark-hadoop product)
         "--spark-hadoop")
       (when (:isolated-nodes product)
         "--isolated-nodes")
       (when (:multi-data-disks product)
         "--multi-data-disks")
       (when-let [patch (:patch product)]
         (str "--patch=" patch))
       (cluster-name cluster)
       (:product product)])))


(defn ctool
  [argv & [in]]
  (log (str (when in (str "echo " "'" in "' | "))
            "ctool " (clojure.string/join " " argv)))
  (when-not *dry-run*
    (log
      (sh/with-programs [ctool]
        (binding [sh/*throw* false]
          (let [flags (if in {:in in} {})
                argv (conj (into [] argv)
                           (assoc flags :verbose true))]
            (apply ctool argv)))))))


(defn ctool-run
  [cluster-name nodes script]
  (let [argv ["run" cluster-name
              (if (= (class nodes) Integer)
                (str nodes)
                (name nodes))
              "-"]]
    (ctool argv script)))


(defn ctool-run-scripts
  [cluster-name script-maps]
  (doseq [script-map script-maps
          [k v] script-map]
    (ctool-run cluster-name k v)))


(defn install-product
  [cluster product]
  (let [cluster-name (cluster-name cluster)
        argv (install-argv cluster product)]
    (ctool argv)
    (ctool-run-scripts cluster-name (:post-install product))
    ;; TODO: implement other options in the start argv
    (when (:start product)
      (let [argv ["start" cluster-name (:product product)]]
        (ctool argv)
        (ctool-run-scripts cluster-name (:post-start product))))))


(defn provision-cluster
  [cluster]
  (let [argv (launch-argv cluster)]
    (ctool argv)
    (ctool-run-scripts (cluster-name cluster) (:post-launch cluster))
    (doall
      (pmap 
        #(install-product cluster %)
        (:products cluster)))))


(defn help []
  (log "usage: yar /path/to/profile.yaml (n.b., JSON is valid YAML)"))


(defn -main
  [& argv]
  (if (zero? (count argv))
    (do
      (help)
      (System/exit 2))
    (if (= (first argv) "help")
      (help)
      (if-let [profile (try
                         (yaml/parse-string
                           (slurp (first argv)))
                         (catch Exception e
                           (log (str "Invalid argument: " (.getMessage e)))
                           (help)
                           false))]
        (binding [*dry-run* (:dry-run profile)
                  *timestamp* (java.util.Date.
                                (or (:timestamp profile)
                                    (-> (java.util.Date.) .getTime)))]
          (time (doall (pmap provision-cluster (:clusters profile)))
                #_(when (:run-tests profile)
                  run-tests))
          (shutdown-agents))
        (System/exit 100)))))

