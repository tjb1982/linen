(ns co.nclk.linen.core-test
  (:require [clojure.test :refer :all]
            [clj-yaml.core :as yaml]
            [co.nclk.linen.node :refer [node-manager]]
            [co.nclk.linen.core :as linen]))


(deftest test-minimal-config
  (testing "A minimal configuration"
    (let [r (linen/run {:program
                       {:main
                        [{:module
                          {:checkpoints
                           [[{:source "echo Hello World"}]]}}]}})]
      (-> r nil? not is)
      (-> r linen/returns count (= 1) is)
      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
      (clojure.pprint/pprint r)
      ))
  (testing "Swap functionality"
     (let [r (linen/run (yaml/parse-string
"
env:
  foo:
    bar:
      baz: quux
  one:
  - 2
  - three:
    - 4
program:
  main:
  - - ~(log:
      - ~:info
      - ~@foo.bar.baz
    - ~(log:
      - ~:info
      - ~@one.0
    - ~(log:
      - ~:info
      - ~{foo.bar.baz}
"))]
      (-> r nil? not is)
      (-> r linen/returns count zero? is)
      (clojure.pprint/pprint r)
      ))
  )

(deftest test-minimal-with-python
  (testing "A minimal configuration using various interpreters"
    (let [invocations ["#!/usr/bin/env python2\nfrom __future__ import print_function\nimport sys\nprint('Hello Python', file=sys.stderr)"
                       "#!/usr/bin/env tclsh\nputs {Hello Tcl}"
                       "#!/usr/bin/env php\n<?php echo \"Hello \".\"World\";"
                       "#!/usr/bin/env node\nconsole.log('Hello JavaScript');"
                       "#!/usr/bin/env dart\nvoid main() { print('Hello Dart'); }"
                       ]
          r (linen/run {:program
                       {:main
                        [{:module
                          {:checkpoints
                           [(map #(identity {:source %}) invocations)]}}]}})]
      (-> r nil? not is)
      (-> r linen/returns count (= (count invocations)) is)
      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
      )))

(deftest test-directive-entry-template-match
  (testing "Using :invocation entry in checkpoint with re-match"
    (let [msg "Interpreter directives are cool"
          r (linen/run {:env {:msg msg}
                       :program {:main
                       [{:module {:checkpoints
                        [[{:source "from __future__ import print_function\nimport sys\n\nprint('~{msg}', file=sys.stderr)\n"
                           :invocation
                           {:template "python #" :match "#"}}]]}}]}})]
      (-> r linen/returns count zero? not is)
      (-> r linen/returns first :err :value (= msg) is)
      )))

(deftest test-directive-entry-template-only
  (testing "Using :invocation entry in checkpoint, template only"
    (let [msg "Directives are cool"
          r (linen/run {:env {:msg msg}
                       :program {:main
                       [{:module {:checkpoints
                        [[{:source "from __future__ import print_function\nimport sys\n\nprint('~{msg}', file=sys.stderr)\n"
                           :invocation {:template "python"}}]]}}]}})]
      (-> r linen/returns count zero? not is)
      (-> r linen/returns first :err :value (= msg) is)
      )))

(deftest test-directive-entry-string
  (testing "Using :invocation entry in checkpoint, template only with extra spaces"
    (let [msg "Directives are cool"
          r (linen/run {:env {:msg msg}
                       :program {:main
                       [{:module {:checkpoints
                        [[{:source "from __future__ import print_function\nimport sys\n\nprint('~{msg}', file=sys.stderr)\n"
                           :invocation "    python   "}]]}}]}})]
      (-> r linen/returns count zero? not is)
      (-> r linen/returns first :err :value (= msg) is)
      )))

(deftest test-concurrent-checkpoints
  (testing "Concurrent checkpoints"
    (let [fname (str (System/getProperty "user.dir") "/testconcurrent")]
      (spit fname "")
      (let [r (linen/run {:program
                         {:main
                          [{:module
                            {:checkpoints
                             [[{:source (str "sleep 0.1\necho Two >> " fname)}
                               {:source (str "echo One >> " fname)}]
                              [{:source (str "echo Three >> " fname)}]]}}]}})]
        (-> r nil? not is)
        (-> r linen/returns count (= 3) is)
        (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
        (-> fname slurp (= "One\nTwo\nThree\n") is)
        (clojure.java.io/delete-file fname)
        ))))

(deftest test-special-forms
  (testing "~(let"
    (let [r (linen/run (yaml/parse-string "
program:
  main:
  - ~(let:
    - - foo
      - lalala
    - module:
        provides:
        - key: FOO
        checkpoints:
        - - source: echo ~{foo}
            nodes:
            - out: FOO
      out:
        FOOS:
          ~(conj: [ ~@FOOS, ~@FOO ]
      children:
      - ~@FOOS
              "))]
      (->> r first second ffirst (= "lalala") is)))
  (testing "~(fn")
  (testing "~(if")
  )

(deftest test-depth-first-extract-env
  (testing "Depth-first extract-env"
    (let [r (linen/run (yaml/parse-string "
               program:
                 main:
                 - ~(map:
                   - ~(#:
                     - module:
                         provides:
                         - key: FOO
                         checkpoints:
                         - - source: echo lalala
                             nodes:
                             - out: FOO
                       out:
                         FOOS:
                             ~(conj: [ ~@FOOS, ~@FOO ]
                       children:
                       - module:
                           checkpoints:
                           - - source: echo ~{#FOOS}~{.}~{/FOOS}
                   - ~(range: [ 0, 3 ]
                   out:
                     ONE_FOO:
                       ~(take: [ 1, ~@FOOS ]
                   children:
                   - ~(let:
                     - - ONE_FOO
                       - ~(first: [ ~@FOOS ]
                     - module:
                         checkpoints:
                         - - source: echo ~{{ONE_FOO}} then ~{#FOOS}~{.}~{/FOOS}
               
               "))]
      (-> r nil? not is)
      (-> r linen/returns count (> 1) is)
      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
      )))

#_(deftest test-evaluate-only
  (testing "Using the `evaluate` function by itself"
    (let [r (linen/evaluate {:module
                            {:checkpoints
                             [[{:assert
                                {(keyword "~(fn")
                                 [["out" "err" "exit"]
                                  {(keyword "~(=") ["~@out" "Hello World"]}]}
                                :source "echo Hello World"}]]}} {})]
      (-> r :returns ffirst :success :value true? is)
      (-> r :returns ffirst :exit :value zero? is)
      (-> r :returns ffirst :out :value (= "Hello World") is)
      (->> r :returns ffirst :runid (instance? java.util.UUID) is)
      )))

(deftest test-run-parameters
  (testing "Run with parameters"
    (let [text "lalala"
          fname (str (System/getProperty "user.dir") "/testparams")
          r (linen/run {:env {:TEXT text :FILENAME fname}
                       :program
                       {:main
                        [{:module
                          {:checkpoints
                           [[{:source "#!/usr/bin/env python2\nwith open('~{FILENAME}', 'w') as f:\n    f.write('~{TEXT}')\n"}]]}}]}})]
      (-> r linen/returns first :success :value true? is)
      (-> r linen/returns first :out :value (= "") is)
      (-> r linen/returns first :exit :value zero? is)
      (-> r linen/returns first :err :value (= "") is)
      (clojure.java.io/delete-file fname)
      )))

(deftest test-output-variable
  (testing "Retrieving output variables"
    (let [r (linen/run {:env {}
                       :program {:main [
                       {:out {:FOO "~@TEST"}
                        :children [{:in {:LALA "~@FOO"}
                                      :module {:requires [{:key "LALA"}] :checkpoints [[{:source "echo junebug ~{LALA}"}]]}}]
                        :module
                        {:provides [{:key "TEST"}]
                         :checkpoints [
                         [{:nodes [{:name "local" :out "TEST"}]
                           :source "echo This is a test"}]]}}]}})]
      (clojure.pprint/pprint r)
      (-> r linen/returns second :out :value (= "junebug This is a test") is)
      )))

(deftest test-resolve-program
  (testing "Resolving a program from a string using the data-connector"
    (let [fname (str (System/getProperty "user.dir") "/test-program-resolve.yaml")]
      (spit fname "main:\n- module:\n    checkpoints:\n    - - source: echo hello resolve")
      (let [r (linen/run {:env {}
                         :program {:main [
                         {:resolve fname}]}})]
        (clojure.java.io/delete-file fname)
        (-> r linen/returns first :out :value (= "hello resolve") is)
        ))))

(deftest test-run-module-in-checkpoint
  (testing "Running a module from a checkpoint"
    (let [r (linen/run {:env {}
                       :program
                       {:main [{:module
                                {:checkpoints [[{:module
                                                 {:checkpoints [[{:source "echo Hello first module"}]]}}
                                                {:module
                                                 {:checkpoints [[{:source "echo Hello second module"}]]}}]]}}]}})]
      (clojure.pprint/pprint r))))

