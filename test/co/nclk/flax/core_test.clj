(ns co.nclk.flax.core-test
  (:require [clojure.test :refer :all]
            ;[clj-yaml.core :as yaml]
            [co.nclk.flax.core :as flax]))


(deftest test-minimal-config
  (testing "A minimal configuration"
    (let [r (flax/run {:program
                       {:main
                        [{:module
                          {:checkpoints
                           [[{:invocation "echo Hello World"}]]}}]}})]
      (-> r :raw nil? not is)
      (-> r :checkpoints count (= 1) is)
      (-> r :failures count zero? is)
      (-> r :env nil? is)
      )))

(deftest test-minimal-with-python
  (testing "A minimal configuration using various interpreters"
    (let [invocations ["#!/usr/bin/env python2\nfrom __future__ import print_function\nimport sys\nprint('Hello Python', file=sys.stderr)"
                       "#!/usr/bin/env tclsh\nputs {Hello Tcl}"
                       "#!/usr/bin/env php\n<?php echo \"Hello \".\"World\";"
                       "#!/usr/bin/env node\nconsole.log('Hello JavaScript');"
                       "#!/usr/bin/env dart\nvoid main() { print('Hello Dart'); }"
                       ]
          r (flax/run {:program
                       {:main
                        [{:module
                          {:checkpoints
                           [(map #(identity {:invocation %}) invocations)]}}]}})]
      (-> r :raw nil? not is)
      (-> r :checkpoints count (= (count invocations)) is)
      (-> r :failures count zero? is)
      (-> r :env nil? is)
      )))

(deftest test-concurrent-checkpoints
  (testing "Concurrent checkpoints"
    (let [fname (str (System/getProperty "user.dir") "/testconcurrent")]
      (spit fname "")
      (let [r (flax/run {:program
                         {:main
                          [{:module
                            {:checkpoints
                             [[{:invocation (str "sleep 1\necho Two >> " fname)}
                               {:invocation (str "echo One >> " fname)}]
                              [{:invocation (str "echo Three >> " fname)}]]}}]}})]
        (-> r :raw nil? not is)
        (println (-> r :checkpoints count))
        (-> r :checkpoints count (= 3) is)
        (-> r :failures count zero? is)
        (-> fname slurp (= "One\nTwo\nThree\n") is)
        (clojure.java.io/delete-file fname)
        ))))
