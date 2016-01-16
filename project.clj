(defproject co.nclk/flax "0.1.4"
  :description "Implementation of an interpreter for flax, a domain specific language for modeling coordinated, distributed processes."
  :url "https://github.com/tjb1982/flax"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [stencil "0.5.0"]
                 [cheshire "5.5.0"]
                 [clj-ssh "0.5.11"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [circleci/clj-yaml "0.5.3"]
                 ]
  :aot :all
  :main co.nclk.flax.core)
