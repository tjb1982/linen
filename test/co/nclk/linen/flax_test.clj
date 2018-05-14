(ns co.nclk.linen.flax-test
  (:require [clojure.test :refer :all]
            [clj-yaml.core :as yaml]
            [co.nclk.linen.node :refer [node-manager]]
            [co.nclk.linen.data :refer [connector]]
            [co.nclk.linen.core :as linen]))

(def conn (connector))

(defn yrun
  [x & [config]]
  (-> x yaml/parse-string (assoc :data-connector conn) linen/run))

(deftest test-minimal-flax
  (testing "A minimal configuration"
    (let [src "
program:
  main:
    module:
      checkpoints:
      - - source: echo Hello World
"
          r (yrun src)]
      (-> r nil? not is)
      (-> r linen/returns count (= 1) is)
      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)))


  (testing "A minimal configuration with env"
    (let [src "
program:
  main:
    module:
      checkpoints:
      - - source: echo Hello ~{FOO}
env:
  FOO: bar
"
          r (yrun src)]
      (-> r nil? not is)
      (-> r linen/returns count (= 1) is)
      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
      (-> r :returns ffirst :source (= "echo Hello bar") is)))


  (testing "A minimal configuration with env and ~(map with ~(fn"
    (let [src "
program:
  main:
    ~(map:
    - ~(fn:
      - [idx]
      - module:
          checkpoints:
          - - source: echo Hello ~{FOO} ~{idx}
    - ~(range: [0,3]
env:
  FOO: bar
"
          r (yrun src)]
      (-> r nil? not is)
      (-> r linen/returns count (= 3) is)
      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
      (-> r first :returns ffirst :source (= "echo Hello bar 0") is)))


  (testing "A minimal configuration with env and ~(map with ~(#"
    (let [src "
program:
  main:
    ~(map:
    - ~(#:
      - module:
          checkpoints:
          - - source: echo Hello ~{FOO} ~{0}
    - ~(range: [0,3]
env:
  FOO: bar
"
          r (yrun src)]
      (-> r nil? not is)
      (-> r linen/returns count (= 3) is)
      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
      (-> r first :returns ffirst :source (= "echo Hello bar 0") is)))


  (testing "A minimal configuration with env and ~(map with fn in env"
    (let [src "
program:
  main:
    ~(map:
    - ~@FUN
    - ~(range: [0,3]
env:
  FOO: bar
  FUN:
    ~(fn:
    - [idx]
    - module:
        checkpoints:
        - - source: echo Hello ~{FOO} ~{idx}
"
          r (yrun src)]
      (-> r nil? not is)
      (-> r linen/returns count (= 3) is)
      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
      (-> r first :returns ffirst :source (= "echo Hello bar 0") is)
      ))


  (testing "A minimal configuration with env and ~(apply with fn in env"
    (let [src "
program:
  main:
    ~(apply:
    - ~@FUN
    - - 0
env:
  FOO: bar
  FUN:
    ~(fn:
    - [idx]
    - module:
        checkpoints:
        - - source: echo Hello ~{FOO} ~{idx}
"
          r (yrun src)]
      (-> r nil? not is)
      (-> r linen/returns count (= 1) is)
      (->> r linen/returns (filter #(-> % :success :value true? not)) count zero? is)
      (-> r :returns ffirst :source (= "echo Hello bar 0") is)
      ))
  )

(deftest testing-interpolation
  (testing "JSON interpolation"
    (let [src "
program:
  main: ~@foo
env:
  foo:
    bar\\.baz: quux
"
          r (yrun src)]
      (-> r keys first (= :bar.baz) is)
      (-> r nil? not is)))
  )


(deftest testing-inputs-and-outputs
  (testing "Inputs"
    (let [src "
program:
  main:
    module:
      requires:
      - key: TEST
      checkpoints:
      - - source: echo Hello ~{TEST}
    in:
      TEST: ~@IDX
env:
  IDX: 0
"
          r (yrun src)]
      (-> r nil? not is)
      )
    )
  )
