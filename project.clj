(defproject co.nclk/linen "4.0.0-SNAPSHOT"
  :description "Implementation of an interpreter for flax, a domain specific
               language for modeling coordinated, distributed processes."
  :url "https://github.com/tjb1982/linen"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [stencil "0.5.0"]
                 [cheshire "5.8.0"]
                 [clj-ssh "0.5.14"]
                 [co.nclk/clj-yaml "1.0.0"]
                 [co.nclk/flax "3.0.0-SNAPSHOT"]
                 ]
  :profiles {:dev {:dependencies [[pjstadig/humane-test-output "0.8.3"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]
                   :resource-paths ["test/resources"]}}
  :aot :all
  :main co.nclk.linen.core)
