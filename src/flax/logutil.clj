(ns flax.logutil
  (:require [clojure.core.async :refer [go-loop chan <! >!!]]
            [me.raynes.conch.low-level :as sh]))


(def ^:dynamic *log-script* nil)
(def log-chan (chan))
(def logger
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


;;(def logger
;;  (go-loop []
;;    (let [msg (<! log-chan)]
;;      (println (-> msg :proc :process))
;;      (if-not (instance? java.util.Map msg)
;;        (println msg)
;;        (if-let [proc (-> msg :proc :process)]
;;          (let [out (future (sh/stream-to-out proc :out))
;;                err (future (sh/stream-to-out proc :err))]
;;            (println "lalalalala")
;;            )
;;
;;          #_(.write *out* (str msg)))))
;;    (recur)))


(defn log
  [msg]
  (>!! log-chan msg)
  (when (:exit-code msg)
    @(:exit-code msg)))

