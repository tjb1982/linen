(ns flax.logutil
  (:require [clojure.core.async :refer [go-loop chan <! >!!]]
            [flax.protocols :refer [PLogger]]
            [clojure.tools.logging :as clog]
            [me.raynes.conch.low-level :as sh]))


(def ^:dynamic *log-script* nil)
(def log-chan (chan 4))
(defn log*
  [msg]
  (>!! log-chan msg)
  (when (:exit-code msg)
    @(:exit-code msg)))


(defn stream-to-reader
  [stream]
  (java.io.BufferedReader. (java.io.InputStreamReader. stream))) 


(def logloop
  (go-loop []
    (let [msg (<! log-chan)
          date (when-not *log-script* (str (java.util.Date.) ": "))]
      (condp instance? msg
        java.io.InputStream
        (log* (stream-to-reader msg))

        java.io.BufferedReader
        (go-loop [reader msg]
          (when-let [line (.readLine reader)]
            (do (println date "### " line)
                (recur reader))))

        String
        (println (str date "### " msg))
        )
      #_(if-not (instance? java.util.Map msg)
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


(deftype StandardLogger []
  PLogger
  (log [self level msg]
    (cond
      (string? msg) (log* msg)
      (sequential? msg) (doall (map log* msg)))))

(deftype ClojureToolsLogger []
  PLogger
  (log [self level msg]
    (when
      (string? msg)
      (clog/log level msg))))

