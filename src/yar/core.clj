(ns yar.core
  (:require [clojure.core.async :as a :refer [chan go >! >!! <! <!! alts!! alts! go-loop thread]]
            [me.raynes.conch :as sh]
            [clj-yaml.core :as yaml]
            [clojure.pprint :refer [pprint]]
            [cheshire.core :as json])
  (:gen-class))


(def ^:dynamic *timestamp* nil)
(def ^:dynamic *dry-run* nil)
(def ^:dynamic *skip* nil)
(def ^:dynamic *log-script* nil)
(def this-ns *ns*)
(def genv (atom {}))


(def log-chan (chan))
(def logger
  (go-loop []
    (let [msg (<! log-chan)
          date (when-not *log-script* (str (java.util.Date.) ": "))]
      (if-not (or (:stdout msg) (:stderr msg))
        (println (str date "### " msg))
        (do
          (let [err (:stderr msg)]
            (when-not (-> err clojure.string/blank?)
              (binding [*out* *err*]
                (doseq [ln (clojure.string/split err #"\n")]
                  (println (str date (when *log-script* "# ") "ERROR: " ln))))))
          (let [out (:stdout msg)]
            (when-not (-> out clojure.string/blank?)
              (doseq [ln (clojure.string/split out #"\n")]
                (println (str date ln))))))))
    (recur)))


(defn log
  [msg]
  (>!! log-chan msg)
  (when (:exit-code msg)
    @(:exit-code msg)))


(defn skipstr
  [& args]
  (str (when *skip* "# ")
       (apply str args)))


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
  [argv & [flags]]
  (when (:in flags)
    (log {:stdout (skipstr "echo \"")})
    (doseq [ln (clojure.string/split (:in flags) #"\n")]
      (log {:stdout (skipstr "\t" ln)}))
    (log {:stdout (skipstr "\" | ")}))
  (log {:stdout (skipstr "ctool " (clojure.string/join " " argv))})
  (when-not *dry-run*
    (log
      (sh/with-programs [ctool]
        (let [argv (conj (into [] argv)
                         (assoc flags :verbose true))]
          (apply ctool argv))))))


(defn ctool-run
  [cluster-name nodes script]
  (let [argv ["run" cluster-name
              (if (= (class nodes) Integer)
                (str nodes)
                (name nodes))
              "-"]]
    (ctool argv {:in script})))


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


(defn add-cluster-ip-to-genv
  [ln cluster]
  (swap! genv
    (fn [old]
      (let [variable-base (str (clojure.string/replace 
                                 (:cluster-name cluster) #"-" "_")
                               "_IP_ADDR_")]
        (loop [idx 0]
          (if (get @genv (str variable-base idx))
            (recur (inc idx))
            (assoc old
                   (str variable-base idx)
                   (second (clojure.string/split ln #": ")))))))))


(defn add-cluster-info-to-genv
  [cluster]
  (sh/with-programs [ctool]
    (let [ret (ctool "info" (cluster-name cluster) {:seq true})]
      (doseq [ln ret]
        (when (re-find #"public ip" ln)
          (add-cluster-ip-to-genv ln cluster))))))


(defn provision-cluster
  [cluster]
  (binding [*skip* (or *skip* (:skip cluster))]
    (binding [*dry-run* (or *dry-run* *skip*)]
      (let [argv (launch-argv cluster)]
        (ctool argv)
        (ctool-run-scripts (cluster-name cluster) (:post-launch cluster))
        ;; add the ip address (etc.) to the genv
        (add-cluster-info-to-genv cluster)
        (doall
          (pmap 
            #(install-product cluster %)
            (:products cluster)))))))


(defn run-test
  [t]
  (binding [*skip* (or *skip* (:skip t))]
    (binding [*dry-run* (or *dry-run* *skip*)]
      (sh/with-programs [bash]
        (let [test-invocation-string (bash "-c"
                                           (str "printf \"" (:invocation t) "\"")
                                           {:env @genv})]
          (if *dry-run*
            (log {:stdout (skipstr test-invocation-string)})
            (let [ret (bash "-c" (:invocation t) {:env @genv :verbose true})]

              (when-not (zero? (count (:stdout ret)))
                (log {:stdout (skipstr test-invocation-string)})
                (log (:stdout ret)))

              (when-not (zero? (count (:stderr ret)))
                (log {:stdout (skipstr test-invocation-string)})
                (log {:stderr (:stderr ret)}))

              @(:exit-code ret))))))))


(defn help []
  (log "usage: yar /path/to/profile.yaml (n.b., JSON is valid YAML)"))


(defn run-profile
  [profile]
  (binding [sh/*throw* (:throw profile)
            *dry-run* (:dry-run profile)
            *timestamp* (java.util.Date.
                          (or (:timestamp profile)
                              (-> (java.util.Date.) .getTime)))]

    (alter-var-root #'*log-script* (fn [_] (:log-script profile)))

    (let [e (sh/with-programs [env]
              (into {} (for [ln (env {:seq true})]
                (conj {} (clojure.string/split ln #"=" 2)))))]
      (swap! genv (fn [_] e)))

    (binding [*skip* (not (:provision profile))]
      (binding [*dry-run* (or *dry-run* *skip*)]
        (log "Provisioning nodes")
        (doall (pmap provision-cluster (:clusters profile)))))

    (binding [*skip* (not (:test profile))]
      (binding [*dry-run* (or *dry-run* *skip*)]
        (log (str "Running tests in \"" (:name profile) "\" profile:"))
        (doseq [test-collection (:tests profile)]
          (log
            (if (> (count test-collection) 1)
              (str "\t\tRunning group of " (count test-collection) " in parallel: ")
              "\t\tRunning group of 1: "))
          (doall (map run-test test-collection)))))

    (shutdown-agents)))


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
        (run-profile profile)
        (System/exit 100)))))

