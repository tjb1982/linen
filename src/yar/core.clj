(ns yar.core
  (:require [clojure.java.shell :refer [sh]]
            [clojure.core.async :as a :refer [chan go >! >!! <! <!! alts!! alts! go-loop thread]]
            [me.raynes.conch :as sh]
            [clj-yaml.core :as yaml]
            [cheshire.core :as json]))


(def ^:dynamic *timestamp* nil)
(def ^:dynamic *dry-run* nil)
(def this-ns *ns*)

(def log-chan (chan))
(def logger
  (go-loop []
    (println (<! log-chan))
    (recur)))


(defn log
  [msg]
  (>!! log-chan msg))


(defn cluster-name
  [cluster]
  (str (or (:cluster-name cluster)
           (-> this-ns str (clojure.string/split #"\.") first))
       "-"
       (.getTime *timestamp*)))


(defn launch-argv
  [cluster]
  (flatten
    (remove
      nil?
      ["launch"
       (when-let [platform (:platform cluster)]
         ["-p" platform])
       (when-let [it (:instance-type cluster)]
         ["-i" it])
       (when-let [jdk (:jdk cluster)]
         ["-j" jdk])
       (when-let [rn (:reserved-nodes cluster)]
         ["-r" rn])
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
  ;; TODO: implement the rest of the possible options
  (flatten
    (remove nil?
      ["install"
       (when-let [it (:install-type product)]
         ["-i" it])
       (when (:branch product)
         ["-v" "spocklib"])
       (cluster-name cluster)
       (:product product)])))


(defn ctool
  [argv & [in]]
  (log
    (if *dry-run*
      (str (when in (str "echo " "'" in "' | "))
           "ctool " (clojure.string/join " " argv))
      {:argv argv
       :ret (sh/with-programs [ctool]
              (binding [sh/*throw* false]
                (let [flags (if in {:in in} {})
                      argv (conj (into [] argv)
                                 (assoc flags :verbose true))]
                  (apply ctool argv))))})))


(defn install-product
  ;; TODO: implement other options in the argv
  [cluster product]
  (let [cluster-name (cluster-name cluster)
        argv (install-argv cluster product)]
    (ctool argv)
    (doseq [[k v] (:post-install product)]
      (let [argv ["run" cluster-name (name k) "-"]]
        (ctool argv v)))
    (let [argv ["start" cluster-name (:product product)]]
      (ctool argv))))


(defn provision-cluster
  [cluster]
  (let [argv (launch-argv cluster)]
    (ctool argv)
    (doall
      (pmap 
        #(install-product cluster %)
        (:products cluster)))))


;;(defn destroy-clusters
;;  [cluster]
;;  (sh/with-programs [ctool]
;;    (let [argv ["destroy" "all"]
;;          ret (binding [sh/*throw* false]
;;                (apply ctool (conj argv {:verbose true})))]
;;      (log ret)
;;      {:argv argv
;;       :sh ret})))


(defn help []
  (log "usage: yar /path/to/profile.yaml (n.b., JSON is valid YAML)"))


(defn -main
  [& argv]
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
        (time (doall (pmap provision-cluster (:clusters profile))))
        (shutdown-agents))
      (System/exit 2))))

