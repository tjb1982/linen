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
    (let [invocations ["#!/usr/bin/env python2\nprint 'Hello Python'"
                       "#!/usr/bin/env tclsh\nputs {Hello Tcl}"
                       "#!/usr/bin/env node\nconsole.log('Hello JavaScript');"]
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
