(ns flax.logutil
  (:require [clojure.core.async :refer [go-loop chan <! >!!]]
            [flax.protocols :refer [PLogger]]
            [me.raynes.conch.low-level :as sh]))


(def ^:dynamic *log-script* nil)
(def log-chan (chan))
(def logloop
  (go-loop []
    (let [msg (<! log-chan)
          date (when-not *log-script* (str (java.util.Date.) ": "))]
      (if-not (instance? java.util.Map msg)
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


(defn stream-to-reader
  [stream]
  (java.io.BufferedReader. (java.io.InputStreamReader. stream))) 


(defn log*
  [msg]
  (>!! log-chan msg)
  (when (:exit-code msg)
    @(:exit-code msg)))


(deftype StandardLogger []
  PLogger
  (log [self level msg]
    (cond
      (string? msg) (log* msg)
      (sequential? msg) (doall (map log* msg)))))

