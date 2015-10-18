(defproject flax "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
;                 [selmer "0.9.2"]
                 [stencil "0.5.0"]
                 [cheshire "5.5.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [circleci/clj-yaml "0.5.4"]
                 [me.raynes/conch "0.8.0"]
                 [org.clojure/java.jdbc "0.3.2"]
                 [org.postgresql/postgresql "9.4-1200-jdbc41"]
                 [flax-file-connector "0.1.0-SNAPSHOT"]
                 [flax-lxc-connector "0.1.0-SNAPSHOT"]
                 ]
  :aot :all
  :main flax.main)
