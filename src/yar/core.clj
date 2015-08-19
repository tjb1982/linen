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
          (let [out (:stdout msg)]
            (when-not (-> out clojure.string/blank?)
              (doseq [ln (clojure.string/split out #"\n")]
                (println (str date ln)))))
          (let [err (:stderr msg)]
            (when-not (-> err clojure.string/blank?)
              (binding [*out* *err*]
                (doseq [ln (clojure.string/split err #"\n")]
                  (println (str date (when *log-script* "# ") ln)))))))))
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
    (remove nil?
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
  [argv & [flags force]]
  (when (:in flags)
    (log {:stdout (skipstr "echo \"")})
    (doseq [ln (clojure.string/split (:in flags) #"\n")]
      (log {:stdout (skipstr "\t" ln)}))
    (log {:stdout (skipstr "\" | ")}))
  (log {:stdout (skipstr "ctool " (clojure.string/join " " argv))})
  (when-not *dry-run*
    (sh/with-programs [ctool]
      (let [argv (conj (into [] argv)
                       (assoc flags :verbose true))
            ret (apply ctool argv)]
        @(:exit-code ret)
        (log {:stderr (:stdout ret)})
        (log {:stderr (:stderr ret)})
        ret))))


(defn add-ctool-out-to-genv
  [argv k v]
  (swap! genv
    #(assoc % (name k)
            (let [lines (into []
                              (-> (ctool argv {:in v})
                                  :stdout
                                  (clojure.string/split #"\n")))
                  out-idx (.indexOf lines "Out: ")
                  err-idx (.indexOf lines "Err: ")]
              (loop [idx (inc out-idx)
                     output nil]
                (if (= idx err-idx)
                  output
                  (let [ln (get lines idx)]
                    (recur (inc idx)
                           (str output (if output
                                         (str "\n" ln)
                                         ln))))))))))


(defn ctool-run
  [cluster-name nodes script]
  (let [argv ["run" cluster-name
              (if (isa? (class nodes) Integer)
                (str nodes)
                (name nodes))
              "-"]]
    (if (isa? (class script) String)
      (ctool argv {:in script})
      (doall (map
        (fn [& [[k v] entry]]
          (add-ctool-out-to-genv argv k v))
        (into [] script)))
      )))


(defn ctool-run-scripts
  [cluster-name script-maps]
  (doseq [script-map script-maps
          [k v] script-map]
    (ctool-run cluster-name k v)))


(defmacro wrap-binding
  [what & forms]
  `(binding [*skip* (or *skip* (:skip ~what))]
    (binding [*dry-run* (or *dry-run* *skip*)
               sh/*throw* (or sh/*throw* (:throw ~what))]
      ~@forms)))


(defn do-groups
  [fun groups]
  (doseq [collection groups]
    (when (:checkpoint collection)
      (doseq [[cluster script-maps] (:checkpoint collection)]
        (ctool-run-scripts
          (cluster-name
            {:cluster-name (name cluster)})
            script-maps)))
    (let [group (:group collection)]
      (wrap-binding collection
        (doall (pmap fun group)))
      )))


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
  (wrap-binding cluster
    (let [argv (launch-argv cluster)]
      (ctool argv)
      (ctool-run-scripts (cluster-name cluster) (:post-launch cluster))
      ;; add the ip address (etc.) to the genv
      (add-cluster-info-to-genv cluster)

      (do-groups
        #(install-product cluster %)
        (-> cluster :products :groups)))))


(defn run-test
  [t]
  (wrap-binding t
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

            @(:exit-code ret)))))))


(defn help []
  (println "usage: yar /path/to/profile.yaml (n.b., JSON is valid YAML)"))


(defn run-profile
  [profile]
  (binding [sh/*throw* (:throw profile)
            *dry-run* (:dry-run profile)
            *timestamp* (java.util.Date.
                          (or (:timestamp profile)
                              (-> (java.util.Date.) .getTime)))]

    (alter-var-root #'*log-script* (fn [_] (:log-script profile)))

    (wrap-binding (-> profile :clusters)
      (log (str "Provisioning clusters in \"" (:name profile) "\" profile:"))
      
      (do-groups
        provision-cluster
        (-> profile :clusters :groups)))

    (wrap-binding (-> profile :tests)
      (log (str "Running tests in \"" (:name profile) "\" profile:"))

      (do-groups
        run-test
        (-> profile :tests :groups)))))


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
                           (println (str "Invalid argument: " (.getMessage e)))
                           (help)
                           false))]
        (do
          (run-profile profile)
          #_(shutdown-agents))
        (System/exit 100)))))

