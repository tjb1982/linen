(ns co.nclk.linen.core-test
  (:require [clojure.test :refer :all]
            [clj-yaml.core :as yaml]
            [co.nclk.linen.node :refer [node-manager]]
            [co.nclk.linen.data :refer [connector]]
            [co.nclk.linen.core :as linen]))

(def conn (connector))

(defn slurp-test
  [name*]
  (-> name* clojure.java.io/resource slurp yaml/parse-all))


(def config-keyset #{:effective :main})
(def seed 8031)
(def base-config
  {:effective seed
   :node-manager (node-manager seed)
   :failed? (atom false)})


(deftest config-pre-check
  (testing "config-pre-check works in isolation"
    (let [r (linen/config-pre-check {:effective 1234 :main [{:name ""} {}]})
          r2 (try
               (linen/config-pre-check {})
               (catch IllegalArgumentException iae iae))]
      (-> r nil? is)
      (->> r2 (instance? IllegalArgumentException) is)
      (is (every? #(.contains (-> r2 .getMessage) (name %)) config-keyset))))

  (testing "config-pre-check works in a program run"
    (let [r (linen/run {:effective 8031 :main [{:name ""} {}]})]
      (-> r nil? not is))))


(deftest config-keyset-test
  (-> config-keyset (= linen/config-keyset) is))


(deftest normalize-module-test
  (let [ah {:name nil}
        test-cases 
        [[nil `(~ah nil)]
         [`() `(~ah)]
         [1 `(~ah 1)]
         ["string" `(~ah "string")]
         [\x `(~ah \x)]
         [0 `(~ah 0)]
         [{} `(~ah)]
         ['(nil) `(~ah nil)]
         [[nil] `(~ah nil)]
         [#{nil} `(~ah #{nil})]
         [#{1 2 3} `(~ah #{1 2 3})]
         [#{{:name "foo"}} `(~ah #{{:name "foo"}})]
         [`(~ah ~ah) `(~ah ~ah)]
         [{:foo nil} `(~ah {:foo nil})]
         [{:foo nil :provides []} `(~ah {:foo nil :provides []})]
         [{:provides []} `({:name nil :provides []})]
         [[nil] `(~ah nil)]
         ['(nil) `(~ah nil)]
         (let [x {:then \x :provides [] :requires [] :name "x"}]
           [`(~x) `(~ah ~x)])
         (let [x {:name "x" :provides [] :requires []}]
           [`(~x) `(~x)])
         (let [x ['() {:a :bunch :of "stuff"
                   :that ["is varied" [{:because [0 {:we {} :like nil :testing "!"}]}]]}]]
           [`(~x) `(~ah ~x)])
         ['(() ()) `(~ah () ())]
         ['((())) `(~ah (()))]
         ['(1 2 3) `(~ah 1 2 3)]
         [[1 2 3] `(~ah 1 2 3)]
         [[{} {}] `(~ah {})]
         [[{:foo "bar"} {}] `(~ah {:foo "bar"} {})]
        ]]
    (doseq [test-cases [test-cases (-> "normalize-module.yaml" slurp-test)]
            tcidx (range 0 (count test-cases))]
      (let [tc (linen/evaluate (nth test-cases tcidx) {})
            module (linen/normalize-module (first tc))]
        (testing (yaml/generate-string {tcidx tc})
          (-> module (= (second tc)) is))))))


(deftest run-module-test-basic
  (testing "A basic module"
    (let [module (-> "basic1.yaml" slurp-test)
          r (linen/run-module module (assoc base-config :env {:FOO {:name "bar"}}))]
      (-> r nil? not is)
      (let [cp (-> r second :checkpoint)]
        (-> cp nil? not is)
        (->> cp first keys (filter #{:runid :out :err :exit :success}) count (= 5) is)
        (->> cp first :exit :value (= 0) is)
        (->> cp first :err :value (= "") is)
        (->> cp first :out :value (= "hello bar") is)
        (->> cp first :success :value true? is))))
  (testing "A module using :in, :out, and :then"
    (let [src (-> "then.yaml" slurp-test first)
          r (linen/run-module src (assoc base-config :env {:FOO {:name "bar"}}))]
      (-> r nil? not is)
      (-> r second :child (nth 2) :then first (= "hello bar") is)
      ;;(clojure.pprint/pprint r)
      )))

(defn rm-dynamic-keys
  [x]
  (let [keys* #{:runid :started :finished}]
    (clojure.walk/postwalk
      #(if (and (map? %)
                (-> % (select-keys keys*) empty? not))
         (apply dissoc % keys*)
         %)
      x)))

(deftest run-module-test
  (let [test-cases (-> "run-module.yaml" slurp-test)]
    (doseq [tcidx (range 0 (count test-cases))]
      (let [{:keys [label env module expected]} (nth test-cases tcidx)
            module (linen/normalize-module module)
            r (-> module (linen/run-module (assoc base-config :env env)) rm-dynamic-keys)]
      (testing label
        (-> expected (= r) is))))))

(deftest run-test
  (let [test-cases (-> "run.yaml" slurp-test)]
    (doseq [tcidx (range 0 (count test-cases))]
      (let [{:keys [label env config expected]} (nth test-cases tcidx)
            r (-> base-config (merge config) linen/run rm-dynamic-keys)]
      (testing label
        (-> expected (= r) is))))))











;;(deftest test-minimal-config
;;  (testing "A minimal configuration"
;;    (let [r (linen/run {:data-connector conn :program 
;;                       {:main
;;                        [{:module
;;                          {:checkpoints
;;                           [[{:source "echo Hello World"}]]}}]}})]
;;      (-> r nil? not is)
;;      (-> r linen/returns count (= 1) is)
;;      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
;;      (clojure.pprint/pprint r)
;;      ))
;;  (testing "Swap functionality"
;;     (let [r (linen/run (yaml/parse-string
;;"
;;env:
;;  foo:
;;    bar:
;;      baz: quux
;;  one:
;;  - 2
;;  - three:
;;    - 4
;;program:
;;  main:
;;  - - ~(log:
;;      - ~:info
;;      - ~@foo.bar.baz
;;    - ~(log:
;;      - ~:info
;;      - ~@one.0
;;    - ~(log:
;;      - ~:info
;;      - ~{foo.bar.baz}
;;"))]
;;      (-> r nil? not is)
;;      (-> r linen/returns count zero? is)
;;      (clojure.pprint/pprint r)
;;      ))
;;  )
;;
;;(deftest test-minimal-with-python
;;  (testing "A minimal configuration using various interpreters"
;;    (let [invocations ["#!/usr/bin/env python2\nfrom __future__ import print_function\nimport sys\nprint('Hello Python', file=sys.stderr)"
;;                       "#!/usr/bin/env tclsh\nputs {Hello Tcl}"
;;                       "#!/usr/bin/env php\n<?php echo \"Hello \".\"World\";"
;;                       "#!/usr/bin/env node\nconsole.log('Hello JavaScript');"
;;                       "#!/usr/bin/env dart\nvoid main() { print('Hello Dart'); }"
;;                       ]
;;          r (linen/run {:data-connector conn :program 
;;                       {:main
;;                        [{:module
;;                          {:checkpoints
;;                           [(map #(identity {:source %}) invocations)]}}]}})]
;;      (-> r nil? not is)
;;      (-> r linen/returns count (= (count invocations)) is)
;;      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
;;      )))
;;
;;(deftest test-directive-entry-template-match
;;  (testing "Using :invocation entry in checkpoint with re-match"
;;    (let [msg "Interpreter directives are cool"
;;          r (linen/run {:env {:msg msg}
;;                       :data-connector conn :program  {:main
;;                       [{:module {:checkpoints
;;                        [[{:source "from __future__ import print_function\nimport sys\n\nprint('~{msg}', file=sys.stderr)\n"
;;                           :invocation
;;                           {:template "python #" :match "#"}}]]}}]}})]
;;      (-> r linen/returns count zero? not is)
;;      (-> r linen/returns first :err :value (= msg) is)
;;      )))
;;
;;(deftest test-directive-entry-template-only
;;  (testing "Using :invocation entry in checkpoint, template only"
;;    (let [msg "Directives are cool"
;;          r (linen/run {:env {:msg msg}
;;                       :data-connector conn :program  {:main
;;                       [{:module {:checkpoints
;;                        [[{:source "from __future__ import print_function\nimport sys\n\nprint('~{msg}', file=sys.stderr)\n"
;;                           :invocation {:template "python"}}]]}}]}})]
;;      (-> r linen/returns count zero? not is)
;;      (-> r linen/returns first :err :value (= msg) is)
;;      )))
;;
;;(deftest test-directive-entry-string
;;  (testing "Using :invocation entry in checkpoint, template only with extra spaces"
;;    (let [msg "Directives are cool"
;;          r (linen/run {:env {:msg msg}
;;                       :data-connector conn :program  {:main
;;                       [{:module {:checkpoints
;;                        [[{:source "from __future__ import print_function\nimport sys\n\nprint('~{msg}', file=sys.stderr)\n"
;;                           :invocation "    python   "}]]}}]}})]
;;      (-> r linen/returns count zero? not is)
;;      (-> r linen/returns first :err :value (= msg) is)
;;      )))
;;
;;(deftest test-concurrent-checkpoints
;;  (testing "Concurrent checkpoints"
;;    (let [fname (str (System/getProperty "user.dir") "/testconcurrent")]
;;      (spit fname "")
;;      (let [r (linen/run {:data-connector conn :program 
;;                         {:main
;;                          [{:module
;;                            {:checkpoints
;;                             [[{:source (str "sleep 0.1\necho Two >> " fname)}
;;                               {:source (str "echo One >> " fname)}]
;;                              [{:source (str "echo Three >> " fname)}]]}}]}})]
;;        (-> r nil? not is)
;;        (-> r linen/returns count (= 3) is)
;;        (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
;;        (-> fname slurp (= "One\nTwo\nThree\n") is)
;;        (clojure.java.io/delete-file fname)
;;        ))))
;;
;;(deftest test-special-forms
;;  (testing "~(let"
;;    (let [r (linen/run (yaml/parse-string "
;;program:
;;  main:
;;  - ~(let:
;;    - - foo
;;      - lalala
;;    - module:
;;        provides:
;;        - key: FOO
;;        checkpoints:
;;        - - source: echo ~{foo}
;;            nodes:
;;            - out: FOO
;;      out:
;;        FOOS:
;;          ~(conj: [ ~@FOOS, ~@FOO ]
;;      children:
;;      - ~@FOOS
;;              "))]
;;      (->> r first second ffirst (= "lalala") is)))
;;  (testing "~(fn")
;;  (testing "~(if")
;;  )
;;
;;(deftest test-depth-first-extract-env
;;  (testing "Depth-first extract-env"
;;    (let [r (linen/run (yaml/parse-string "
;;               program:
;;                 main:
;;                 - ~(map:
;;                   - ~(#:
;;                     - module:
;;                         provides:
;;                         - key: FOO
;;                         checkpoints:
;;                         - - source: echo lalala
;;                             nodes:
;;                             - out: FOO
;;                       out:
;;                         FOOS:
;;                             ~(conj: [ ~@FOOS, ~@FOO ]
;;                       children:
;;                       - module:
;;                           checkpoints:
;;                           - - source: echo ~{#FOOS}~{.}~{/FOOS}
;;                   - ~(range: [ 0, 3 ]
;;                   out:
;;                     ONE_FOO:
;;                       ~(take: [ 1, ~@FOOS ]
;;                   children:
;;                   - ~(let:
;;                     - - ONE_FOO
;;                       - ~(first: [ ~@FOOS ]
;;                     - module:
;;                         checkpoints:
;;                         - - source: echo ~{{ONE_FOO}} then ~{#FOOS}~{.}~{/FOOS}
;;               
;;               "))]
;;      (-> r nil? not is)
;;      (-> r linen/returns count (> 1) is)
;;      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
;;      )))
;;
;;#_(deftest test-evaluate-only
;;  (testing "Using the `evaluate` function by itself"
;;    (let [r (linen/evaluate {:module
;;                            {:checkpoints
;;                             [[{:assert
;;                                {(keyword "~(fn")
;;                                 [["out" "err" "exit"]
;;                                  {(keyword "~(=") ["~@out" "Hello World"]}]}
;;                                :source "echo Hello World"}]]}} {})]
;;      (-> r :returns ffirst :success :value true? is)
;;      (-> r :returns ffirst :exit :value zero? is)
;;      (-> r :returns ffirst :out :value (= "Hello World") is)
;;      (->> r :returns ffirst :runid (instance? java.util.UUID) is)
;;      )))
;;
;;(deftest test-run-parameters
;;  (testing "Run with parameters"
;;    (let [text "lalala"
;;          fname (str (System/getProperty "user.dir") "/testparams")
;;          r (linen/run {:env {:TEXT text :FILENAME fname}
;;                       :data-connector conn :program 
;;                       {:main
;;                        [{:module
;;                          {:checkpoints
;;                           [[{:source "#!/usr/bin/env python2\nwith open('~{FILENAME}', 'w') as f:\n    f.write('~{TEXT}')\n"}]]}}]}})]
;;      (-> r linen/returns first :success :value true? is)
;;      (-> r linen/returns first :out :value (= "") is)
;;      (-> r linen/returns first :exit :value zero? is)
;;      (-> r linen/returns first :err :value (= "") is)
;;      (clojure.java.io/delete-file fname)
;;      )))
;;
;;(deftest test-output-variable
;;  (testing "Retrieving output variables"
;;    (let [r (linen/run {:env {}
;;                       :data-connector conn :program  {:main [
;;                       {:out {:FOO "~@TEST"}
;;                        :children [{:in {:LALA "~@FOO"}
;;                                      :module {:requires [{:key "LALA"}] :checkpoints [[{:source "echo junebug ~{LALA}"}]]}}]
;;                        :module
;;                        {:provides [{:key "TEST"}]
;;                         :checkpoints [
;;                         [{:nodes [{:name "local" :out "TEST"}]
;;                           :source "echo This is a test"}]]}}]}})]
;;      (clojure.pprint/pprint r)
;;      (-> r linen/returns second :out :value (= "junebug This is a test") is)
;;      )))
;;
;;(deftest test-resolve-program
;;  (testing "Resolving a program from a string using the data-connector"
;;    (let [fname (str (System/getProperty "user.dir") "/test-program-resolve.yaml")]
;;      (spit fname "main:\n- module:\n    checkpoints:\n    - - source: echo hello resolve")
;;      (let [r (linen/run {:env {}
;;                         :data-connector conn :program  {:main [
;;                         {:resolve fname}]}})]
;;        (clojure.java.io/delete-file fname)
;;        (-> r linen/returns first :out :value (= "hello resolve") is)
;;        ))))
;;
;;(deftest test-run-module-in-checkpoint
;;  (testing "Running a module from a checkpoint"
;;    (let [r (linen/run {:env {}
;;                       :data-connector conn :program 
;;                       {:main [{:module
;;                                {:checkpoints [[{:module
;;                                                 {:checkpoints [[{:source "echo Hello first module"}]]}}
;;                                                {:module
;;                                                 {:checkpoints [[{:source "echo Hello second module"}]]}}]]}}]}})]
;;      (clojure.pprint/pprint r))))

