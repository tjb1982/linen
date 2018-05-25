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

(deftest run-test
  (let [test-cases (-> "run.yaml" slurp-test)]
    (doseq [tcidx (range 0 (count test-cases))]
      (let [{:keys [label env config expected]} (nth test-cases tcidx)
            r (-> base-config (merge config) linen/run rm-dynamic-keys)]
      (testing label
        (-> expected (= r) is))))))

(deftest provides
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
