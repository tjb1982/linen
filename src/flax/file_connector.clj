(ns flax.file-connector
  (:require [clj-yaml.core :as yaml])
  )


(defn module->map
  [m]
  (-> m java.io.File. slurp yaml/parse-string))
