(ns co.nclk.flax.core-test
  (:require [clojure.test :refer :all]
            ;[clj-yaml.core :as yaml]
            [co.nclk.flax.node :refer [node-manager]]
            [co.nclk.flax.core :as flax]))


(deftest test-minimal-config
  (testing "A minimal configuration"
    (let [r (flax/run {:program
                       {:main
                        [{:module
                          {:checkpoints
                           [[{:source "echo Hello World"}]]}}]}})]
      (-> r nil? not is)
      (-> r flax/returns count (= 1) is)
      (->> r flax/returns (filter #(-> % :success :value true? not)) count zero? is)
      (clojure.pprint/pprint r)
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
                           [(map #(identity {:source %}) invocations)]}}]}})]
      (-> r nil? not is)
      (-> r flax/returns count (= (count invocations)) is)
      (->> r flax/returns (filter #(-> % :success :value true? not)) count zero? is)
      )))

(deftest test-directive-entry-template-match
  (testing "Using :invocation entry in checkpoint with re-match"
    (let [msg "Directives are cool"
          r (flax/run {:env {:msg msg}
                       :program {:main
                       [{:module {:checkpoints
                        [[{:source "from __future__ import print_function\nimport sys\n\nprint('~{msg}', file=sys.stderr)\n"
                           :invocation
                           {:template "python #" :match "#"}}]]}}]}})]
      (-> r flax/returns count zero? not is)
      (-> r flax/returns first :err :value (= msg) is)
      )))

(deftest test-directive-entry-template-only
  (testing "Using :invocation entry in checkpoint, template only"
    (let [msg "Directives are cool"
          r (flax/run {:env {:msg msg}
                       :program {:main
                       [{:module {:checkpoints
                        [[{:source "from __future__ import print_function\nimport sys\n\nprint('~{msg}', file=sys.stderr)\n"
                           :invocation {:template "python"}}]]}}]}})]
      (-> r flax/returns count zero? not is)
      (-> r flax/returns first :err :value (= msg) is)
      )))

(deftest test-directive-entry-string
  (testing "Using :invocation entry in checkpoint, template only with extra spaces"
    (let [msg "Directives are cool"
          r (flax/run {:env {:msg msg}
                       :program {:main
                       [{:module {:checkpoints
                        [[{:source "from __future__ import print_function\nimport sys\n\nprint('~{msg}', file=sys.stderr)\n"
                           :invocation "    python   "}]]}}]}})]
      (-> r flax/returns count zero? not is)
      (-> r flax/returns first :err :value (= msg) is)
      )))

;;(deftest test-concurrent-checkpoints
;;  (testing "Concurrent checkpoints"
;;    (let [fname (str (System/getProperty "user.dir") "/testconcurrent")]
;;      (spit fname "")
;;      (let [r (flax/run {:program
;;                         {:main
;;                          [{:module
;;                            {:checkpoints
;;                             [[{:source (str "sleep 0.1\necho Two >> " fname)}
;;                               {:source (str "echo One >> " fname)}]
;;                              [{:source (str "echo Three >> " fname)}]]}}]}})]
;;        (-> r :raw nil? not is)
;;        (println (-> r :checkpoints count))
;;        (-> r :checkpoints count (= 3) is)
;;        (-> r :failures count zero? is)
;;        (-> fname slurp (= "One\nTwo\nThree\n") is)
;;        (clojure.java.io/delete-file fname)
;;        ))))
;;
;;(deftest test-evaluate-only
;;  (testing "Using the `evaluate` function by itself"
;;    (let [r (flax/evaluate {:module
;;                            {:checkpoints
;;                             [[{:assert
;;                                {(keyword "~(fn")
;;                                 [["out" "err" "exit"]
;;                                  {(keyword "~(=") ["~@out" "Hello World"]}]}
;;                                :source "echo Hello World"}]]}} {})]
;;      (-> r :report :returns ffirst :success :value true? is)
;;      (-> r :report :returns ffirst :exit :value zero? is)
;;      (-> r :report :returns ffirst :out :value (= "Hello World") is)
;;      (->> r :report :returns ffirst :runid (instance? java.util.UUID) is)
;;      )))
;;
;;(deftest test-run-parameters
;;  (testing "Run with parameters"
;;    (let [text "lalala"
;;          fname (str (System/getProperty "user.dir") "/testparams")
;;          r (flax/run {:env {:TEXT text :FILENAME fname}
;;                       :program
;;                       {:main
;;                        [{:module
;;                          {:checkpoints
;;                           [[{:source "#!/usr/bin/env python2\nwith open('~{FILENAME}', 'w') as f:\n    f.write('~{TEXT}')\n"}]]}}]}})]
;;      (-> r :checkpoints first :success :value true? is)
;;      (-> r :checkpoints first :out :value (= "") is)
;;      (-> r :checkpoints first :exit :value zero? is)
;;      (-> r :checkpoints first :err :value (= "") is)
;;      (clojure.java.io/delete-file fname)
;;      )))
;;
