(ns co.nclk.linen.core-test
  (:require [clojure.test :refer :all]
            [clj-yaml.core :as yaml]
            [co.nclk.linen.node :refer [node-manager]]
            ;;[co.nclk.linen.data :refer [connector]]
            [co.nclk.linen.connector.handler :refer [connector]]
            [co.nclk.linen.core :as linen]))

(def res-conn
  (let [handler 
        (fn [s]
          (-> s (str ".yaml") clojure.java.io/resource slurp yaml/parse-all))]
    (connector handler handler)))

(defn slurp-test
  [name*]
  (-> name* clojure.java.io/resource slurp yaml/parse-all))


(def config-keyset-stub #{:effective :main})
(def seed 8031)
(def base-config
  {:effective seed
   :node-manager (node-manager seed {})
   :data-connector res-conn
   :failed? (atom false)})


(deftest config-pre-check-test
  (testing "config-pre-check works in isolation"
    (let [r (linen/config-pre-check {:effective 1234 :main [{:name ""} {}]})
          r2 (try
               (linen/config-pre-check {})
               (catch IllegalArgumentException iae iae))]
      (-> r nil? is)
      (->> r2 (instance? IllegalArgumentException) is)
      (is (every? #(.contains (-> r2 .getMessage) (name %)) linen/config-keyset))))

  (testing "config-pre-check works in a program run"
    (let [r (linen/run {:effective 8031 :main [{:name ""} {}]})]
      (-> r nil? not is))))


(deftest config-keyset-test
  (-> config-keyset-stub (= linen/config-keyset) is))


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


(deftest extract-module-env-test
  ;; argv: [provides body input-env]
  ;; expected: {...some environment}
  (let [test-cases
        [{:label (str "When `provides` is nil, and nothing else is provided, it should "
                      "return an empty map.")
          :argv [nil]
          :expected {}}
         {:label (str "When `provides` is nil, body is nil, and input-env is nil, it should "
                      "return an empty map.")
          :argv [nil nil nil]
          :expected {}}
         {:label (str "When `provides` is an empty vector, body is an empty vector, and "
                      "input-env is nil, it should return an empty map.")
          :argv [[] [] nil]
          :expected {}}
         {:label (str "When `provides` is an empty map, body is an empty map, and input-env "
                      "is an empty map, it should return an empty map.")
          :argv [{} {} {}]
          :expected {}}
         {:label (str "When `provides` is nil, body is nil, and input-env is anything other "
                      "than a map, it should return an empty map.")
          :argv [nil nil []]
          :expected {}}
         {:label (str "When `provides` is nil, body is nil, and input-env is anything other "
                      "than a map, it should return an empty map.")
          :argv [nil nil ""]
          :expected {}}
         {:label (str "When `provides` is nil, body is nil, and input-env is anything other "
                      "than a map, it should return an empty map.")
          :argv [nil nil 123]
          :expected {}}
         {:label (str "When `provides` is not a collection, body is not a collection, and "
                      "input-env is anything other than a map, it should return an empty map.")
          :argv [123 123 123]
          :expected {}}
         {:label (str "When `provides` is nil, body has an env, and input-env is empty, it "
                      "should return an empty map.")
          :argv [nil [{:checkpoint [] :env {:a "b"}}] {}]
          :expected {}}
         {:label (str "When `provides` is nil, body has an env, and input-env has entries, it "
                      "should return the entire input-env and nothing else.")
          :argv [nil [{:checkpoint [] :env {:a "b"}}] {:c "d"}]
          :expected {:c "d"}}
         {:label (str "When `provides` is not nil, body has at least one env, and input-env "
                      "is empty, it should return an empty map.")
          :argv [nil [{:checkpoint [] :env {:a "b"}}] {}]
          :expected {}}
         {:label (str "When `provides` is a map containing entries with literal values, body is "
                      "nil, and input-env is nil, it should return an env comprised of the "
                      "`provides` entries.")
          :argv [{:a "b"} {} {}]
          :expected {:a "b"}}
         {:label (str "When `provides` is a map containing an entry with a literal value, "
                      "body has an env containing an entry with a key found in the `provides` "
                      "but with a different value, and input-env is nil, it should return an "
                      "env comprised of the `provides` entries.")
          :argv [{:a "b"} {:env {:a "c"}} {}]
          :expected {:a "b"}}
         {:label (str "When `provides` contains a string that matches the key of any entry "
                      "in the body env, it should return the entry from the body env")
          :argv [["a"] {:env {:a "b"}}]
          :expected {:a "b"}}
         {:label (str "When `provides` contains a map containing a key that matches any entry "
                      "in the body env, and a value that `«` references an entry in the body "
                      "env, it should return the `«`-value of the entry of the body env into the "
                      "key represented in the `provides` entry.")
          :argv [{:a "«b"} {:env {:b "c"}}]
          :expected {:a "c"}}
         {:label (str "When the `provides` arg is a sequencial collection, each of its items "
                      "is `(reduce merge)`-ed.")
          :argv [[{:a "b"} {:b "«a"} {:c "«b"} {:d "«a"}]]
          :expected {:a "b" :b "b" :c "b" :d "b"}}
         {:label (str "When `provides` is a sequential collection, each of its items can be "
                      "any appropriate type.")
          :argv [[{:a "b"} "c" {:key "d" :value "«c"}] {:env {:c "d"}}]
          :expected {:a "b" :c "d" :d "d"}}
         {:label (str "Conflicts between the body env and the input-env should allow the "
                      "body env to win.")
          :argv [{:a "«a" :b "«b"} {:env {:a "b"}} {:a "z" :b "y"}]
          :expected {:a "b" :b "y"}}
         {:label (str "When `provides` is empty, only the input-env should pass through.")
          :argv [nil [{:env {:a "b" :c "d"}}] {:a "z" :b "y"}]
          :expected {:a "z" :b "y"}}
         {:label (str "When `provides` has a binding, only that binding should change the "
                      "resulting env, the rest of the input-env should pass through.")
          :argv [[:a] [{:env {:a "b" :c "d"}}] {:a "z" :b "y"}]
          :expected {:a "b" :b "y"}}
         {:label (str "When `provides` is just a string, it's treated as a key/value binding.")
          :argv ["a" {:env {:a "b"}}]
          :expected {:a "b"}}
         {:label (str "When `provides` has a simple binding and the body contains multiple "
                      "envs with the same key, last out wins (i.e., reverse order).")
          :argv ["a" [{:env {:a "b"}} {:env {:a "d"}}]]
          :expected {:a "b"}}
         {:label (str "When `provides` has a simple binding and the body contains multiple "
                      "envs with the same key, last out wins (i.e., reverse order).")
          :argv ["a" [{:foo [{:bar {:env {:a "z"}}} {:env {:a "b"}}]} {:foo {:env {:a "d"}}}] {:a "y"}]
          :expected {:a "z"}}
        ]]
    (doseq [{:keys [label argv expected]} test-cases]
      (let [actual (apply linen/extract-module-env argv)]
        (testing label
          (is (= expected actual)))))))


(deftest run-module-test-basic
  (testing "A basic module"
    (let [module (-> "basic1.yaml" slurp-test)
          r (linen/run-module module (assoc base-config :env {:FOO {:name "bar"}}))]
      (-> r nil? not is)
      (let [cp (-> r :body first :checkpoint)]
        (-> cp nil? not is)
        (->> cp first keys (filter #{:runid :stdout :stderr :exit :success}) count (= 5) is)
        (->> cp first :exit :value (= 0) is)
        (->> cp first :stderr :value (= "") is)
        (->> cp first :stdout :value (= "hello bar") is)
        (->> cp first :success :value true? is))))
  (testing "A module using :in, :out, and :then"
    (let [src (-> "then.yaml" slurp-test first)
          r (linen/evaluate src (assoc base-config :env {:FOO {:name "bar"}}))]
      (-> r nil? not is)
      (-> r :child (nth 2) :then first (= "hello bar") is)
      )))


(defn rm-dynamic-keys
  [x]
  (let [keys-to-rm #{:runid :started :finished}
        checkpoint-keys (conj keys-to-rm
                              :stdout :stderr :exit :success)]
    (clojure.walk/postwalk
      #(if (and (map? %) (contains? % :checkpoint))
         (do
           (doall (->> % :checkpoint
                         (map (fn [n] (-> n (every? checkpoint-keys) is)))))
           (let [nodes (->> % :checkpoint
                              (map (fn [n] (->> keys-to-rm (apply dissoc n)))))]
             (assoc % :checkpoint nodes)))
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

(defn do-run-test
  [res & [pprint?]]
  (let [test-cases (-> res slurp-test)]
    (doseq [tcidx (range 0 (count test-cases))]
      (let [{:keys [label config expected]} (nth test-cases tcidx)
            r (-> base-config (merge config) linen/run rm-dynamic-keys)]
      (when pprint?
        (clojure.pprint/pprint r))
      (testing label
        (-> expected (= r) is))))))

(deftest run-test
  (do-run-test "run.yaml"))

(deftest run2-test
  (do-run-test "run2.yaml"))

(deftest provides-test
  (let [module (-> "out.yaml" slurp-test)
        r (linen/evaluate module (-> base-config (assoc :env {:FOO {:name "bar"}})))]
    ;;(clojure.pprint/pprint r)
    (testing ":test1 Output is visible by `:child` entry."
      (-> r first :child :test1 first (= '("hello bar 0!", "hello bar 1!", "hello bar 2!")) is))
    (testing ":test2 Outer output is visible by inner `:child` entry."
      (-> r first :child :child :test2 first (= "hello bar 0!") is))
    (testing ":test3 Inner parent output is visible to inner child."
      (-> r first :child :child :test3
          (= "this is a test: hello bar 0! hello bar 1! hello bar 2!") is))
    (testing ":test11 Inner child module is visible to outer dependency."
      (-> r first :child :child :test11 (= "parent can see inner child module.") is))
    (testing ":test12 Inner child module from parent is visible to children."
      (-> r first :child :test12 first (= "parent can see inner child module.") is))
    (testing ":test13 Unprovided output is invisible."
      (-> r first :child :child :test13 nil? is))
    (testing ":test4 Global environment is visible outside of a module."
      (-> r first :child :child :test4 (= {:name "bar"}) is))
    (testing ":test5—:test10 Output can bind literal values."
      (-> r first :child :child :test10 (= "surströmming") is))
    (doseq [idx (range 5 10)]
      (let [k (keyword (str "test" idx))]
        (testing (str "Inputs and other supposedly invisible bindings are invisible where they should be. idx: " idx)
          (-> r first :child :child k nil? is))))
    ))

