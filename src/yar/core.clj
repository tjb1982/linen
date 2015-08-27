(ns yar.core
  (:require [clojure.core.async :as a :refer [chan go >! >!! <! <!! alts!! alts! go-loop thread]]
            [me.raynes.conch :as sh]
            [clj-yaml.core :as yaml]
            [clojure.pprint :refer [pprint]]
            [cheshire.core :as json]
            [yar.logutil :refer [log *log-script*]])
  (:gen-class))


(def ^:dynamic *timestamp* nil)
(def ^:dynamic *dry-run* nil)
(def ^:dynamic *skip* nil)
(def this-ns *ns*)
(def genv (atom {}))
(def gclusters (atom {}))


(defn die
  [& args]
  (apply println args)
  (System/exit 1))


(defn skipstr
  [& args]
  (str (when *skip* "# ")
       (apply str args)))


(defn full-cluster-name
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
       (full-cluster-name cluster)
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
       (full-cluster-name cluster)
       (:product product)])))


(defn ctool
  [argv & [flags force]]
  (log {:stdout (str (when (:in flags)
                       (str 
                         (skipstr "echo \"\n")
                         (apply str (map #(skipstr "\t" % "\n")
                                         (clojure.string/split (:in flags) #"\n")))
                         (skipstr "\" | \n")))
                     (skipstr "ctool " (clojure.string/join " " argv)))})
  (when-not *dry-run*
    (sh/with-programs [ctool]
      (let [argv (conj (into [] argv)
                       (assoc flags :verbose true))
            ret (apply ctool argv)]
        @(:exit-code ret)
        (log {:stderr (:stdout ret)})
        (log {:stderr (:stderr ret)})
        ret))))


(defn parse-ctool-run-output
  [input file]
  (let [lines (clojure.string/split input #"\n")
        dir (condp = file :out "Out: " :err "Err: ")
        to-fun (fn [lines]
                 (condp = file
                   :out (.indexOf lines "Err: ")
                   :err (let [found
                              (first
                                (remove nil?
                                  (for [x (range (count lines))]
                                    (let [triple (take 3 (drop x lines))]
                                      (when (every? clojure.string/blank? triple) x)))))]
                          (if-not found
                            (count lines) found))))]
    (loop [lines lines
           idx (inc (.indexOf lines dir))
           to (to-fun lines)
           output nil]
      (if (= idx to)
        (let [sublines (drop (inc idx) lines)]
          (if (> (.indexOf sublines dir) -1)
            (recur sublines
                   (inc (.indexOf sublines dir))
                   (to-fun sublines)
                   output)
            output))
        (let [ln (nth lines idx)]
          (recur lines
                 (inc idx)
                 to
                 (str output (if output
                               (str "\n" ln)
                               ln))))))))


(defn add-ctool-to-genv
  [ret out & [err]]
  (let [output (-> ret :stdout)]
    (when err
      (swap! genv
        #(assoc % (name err)
                (parse-ctool-run-output output :err))))
    (when out
      (swap! genv
        #(assoc % (name out)
                (parse-ctool-run-output output :out))))

    @(:exit-code ret)))


(defn node-list
  [nodes]
  (if (instance? Integer nodes)
    (str nodes)
    (name nodes)))


(defn ctool-run
  [bundle]
  (let [cluster-name (full-cluster-name bundle)
        nodes (or (:node-list bundle) "all")
        ret (ctool ["run" cluster-name nodes "-"] {:in (:exec bundle)})]
    (when ret
      (add-ctool-to-genv
        ret
        (:out bundle)
        (:err bundle)))))


(defn ctool-run-scripts
  [cluster bundles]
  (doseq [bundle bundles]
    (ctool-run (assoc bundle :cluster-name (:cluster-name cluster)))))


(defmacro wrap-binding
  [what & forms]
  `(binding [*dry-run* (or *dry-run* (:dry-run ~what))
             *skip* (if (false? (:skip ~what))
                      false
                      (or *skip* (:skip ~what)))]
    (binding [*dry-run* (or *dry-run* *skip*)
               sh/*throw* (if (false? (:throw ~what))
                            false
                            (or sh/*throw* (:throw ~what)))]
      ~@forms)))


(defn do-groups
  [fun groups]
  (doseq [collection groups]
    (let [group (:group collection)]
      (wrap-binding collection
        (doall (pmap fun group)))
      )))


(defn install-product
  [cluster product]
  (let [cluster-name (full-cluster-name cluster)
        argv (install-argv cluster product)]
    (ctool argv)
    (ctool-run-scripts cluster (:post-install product))

    ;; TODO: implement other options in the start argv
    (when (:start product)
      (let [argv ["start" cluster-name (:product product)]]
        (ctool argv)
        (ctool-run-scripts cluster (:post-start product))))))


(defn provision-cluster
  [cluster]
  (swap! gclusters (fn [old]
                     (assoc old
                            (:cluster-name cluster)
                            cluster)))
  (wrap-binding cluster
    (let [argv (launch-argv cluster)]
      (ctool argv)
      (ctool-run-scripts cluster (:post-launch cluster))

      (do-groups
        #(install-product cluster %)
        (-> cluster :products :groups)))))


(defn run-checkpoint
  [t]
  (wrap-binding t
    (sh/with-programs [bash]
      (let [checkpoint-exec-string 
            (bash "-c"
              (str "printf \"" (:exec t) "\"")
              {:env @genv})]
        (if (and (:cluster-name t)
                 (not= (:cluster-name t) "localhost"))
          (ctool-run (assoc t :exec checkpoint-exec-string))
          (if *dry-run*
            (log {:stdout (skipstr checkpoint-exec-string)})
            (let [ret (bash "-c" (:exec t) {:env @genv :verbose true})]

              ;;(log ret)
              (when-not (zero? (count (:stdout ret)))
                (log {:stdout (skipstr checkpoint-exec-string)})
                (log {:stderr (:stdout ret)}))

              (when-not (zero? (count (:stderr ret)))
                (log {:stdout (skipstr checkpoint-exec-string)})
                (log {:stderr (:stderr ret)}))

              @(:exit-code ret))))))))


(defn run-profile
  [profile]
  (wrap-binding profile

    (binding [*timestamp* (java.util.Date.
                            (or (-> profile :timestamp)
                                (-> (java.util.Date.) .getTime)))]

      (alter-var-root #'*log-script*
        (fn [old] (if (false? (-> profile :log-script))
                    false
                    (or old (-> profile :log-script)))))

      (wrap-binding (-> profile :clusters)
        (log (str "Provisioning clusters in \"" (-> profile :name) "\" profile:"))
        
        (do-groups
          provision-cluster
          (-> profile :clusters :groups)))

      (wrap-binding (-> profile :checkpoints)
        (log (str "Running tests in \"" (-> profile :name) "\" profile:"))

        (do-groups
          run-checkpoint
          (-> profile :checkpoints :groups)))
      
      )))


(defn run-profile-tree
  [tree num-jobs]
  ;; run the root profile, binding its throw/skip/dry-run
  ;; create a snapshot of all of the clusters if necessary
  ;; run each child in parallel with a clone of the snapshots created
  )


(defn help []
  (println "usage: yar db-spec-string profile-id"))


(defn -main
  [& argv]
  (if (empty? argv)
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
          (wrap-binding profile
            (run-profile profile)
            (run-profile (yaml/parse-string (slurp "./resources/profile2.yaml"))))

          (Thread/sleep 500)
          (shutdown-agents))
        (System/exit 100)))))

