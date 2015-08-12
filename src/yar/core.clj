(ns yar.core
  (:require [clojure.core.async :as a :refer [chan go >! >!! <! <!! alts!! alts! go-loop thread]]
            [me.raynes.conch :as sh]
            [me.raynes.conch.low-level :as lsh]
            [clj-yaml.core :as yaml]
            [clojure.pprint :refer [pprint]]
            [cheshire.core :as json])
  (:gen-class))


(def ^:dynamic *timestamp* nil)
(def ^:dynamic *dry-run* nil)
(def this-ns *ns*)
(def genv (atom {}))


(def log-chan (chan))
(def logger
  (go-loop []
    (let [msg (<! log-chan)
          date (java.util.Date.)]
      (if-not (:stdout msg)
        (println (str "\t" date ": " msg))
        (do
          (let [err (:stderr msg)]
            (when-not (-> err clojure.string/blank?)
              (binding [*out* *err*]
                (println (str "\t" date ": ERROR: " err)))))
          (let [out (:stdout msg)]
            (when-not (-> out clojure.string/blank?)
              (println (str "\t" date ": INFO: " out)))))))
    (recur)))


(defn log
  [msg]
  (>!! log-chan msg)
  (when (:exit-code msg)
    @(:exit-code msg)))


(defn expand-home
  [s]
  (-> s
      (clojure.string/replace
        "~" (System/getProperty "user.home"))))


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
        (let [flags (if in {:in in} {})
              argv (conj (into [] argv)
                         (assoc flags :verbose true))]
          (apply ctool argv))))))


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


(defn add-cluster-to-genv
  [cluster]
  (sh/with-programs [ctool]
    (let [ret (ctool "info" (cluster-name cluster) {:seq true})]
      (doseq [ln ret]
        (when (re-find #"public ip" ln)
          (swap! genv (fn [old]
                        (let [variable-base (str (clojure.string/replace 
                                                   (:cluster-name cluster) #"-" "_")
                                                 "_IP_ADDR_")]
                          (loop [idx 0]
                            (if (get @genv (str variable-base idx))
                              (recur (inc idx))
                              (assoc old
                                     (str variable-base idx)
                                     (second (clojure.string/split ln #": ")))))))))))))


(defn provision-cluster
  [cluster]
  (binding [*dry-run* (or *dry-run* (:skip cluster))]
    (let [argv (launch-argv cluster)]
      (ctool argv)
      (ctool-run-scripts (cluster-name cluster) (:post-launch cluster))
      ;; add the ip address (etc.) to the genv
      (add-cluster-to-genv cluster)
      (doall
        (pmap 
          #(install-product cluster %)
          (:products cluster))))))


(defn run-test
  [t]
  (binding [*dry-run* (or *dry-run* (:skip t))]
    (if *dry-run*
      (log (:invocation t))
      (let [proc (apply lsh/proc ["bash" "-c" (:invocation t) :env @genv])]
        (log {:stdout (lsh/stream-to-string proc :out)
              :stderr (lsh/stream-to-string proc :err)})
        (lsh/exit-code proc)))))


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
      (let [e (sh/with-programs [env]
                (into {} (for [ln (env {:seq true})]
                  (conj {} (clojure.string/split ln #"=" 2)))))]
        (swap! genv (fn [_] e))
        (if-let [profile (try
                           (yaml/parse-string
                             (slurp (first argv)))
                           (catch Exception e
                             (log (str "Invalid argument: " (.getMessage e)))
                             (help)
                             false))]
          (time
            (binding [sh/*throw* (:throw profile)
                      *dry-run* (:dry-run profile)
                      *timestamp* (java.util.Date.
                                    (or (:timestamp profile)
                                        (-> (java.util.Date.) .getTime)))]

              (binding [*dry-run* (or *dry-run* (:skip-provision profile))]
                (log "Provisioning nodes")
                (doall (pmap provision-cluster (:clusters profile))))

              (binding [*dry-run* (or *dry-run* (:skip-tests profile))]
                (log "Running tests")
                (doall (map run-test (:tests profile))))

              (shutdown-agents)))
          (System/exit 100))))))

