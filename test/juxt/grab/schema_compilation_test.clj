;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.schema-compilation-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.schema :refer [compile-schema] :as schema]
   [juxt.grab.alpha.parser :refer [parse]]
   [juxt.grab.alpha.graphql :as-alias g]))

(deftest directive-compilation-test
  (let [schema
        (compile-schema (parse "type Query @d(delta: 40) { someField(x: Int @a(alpha: 10) @b(beta: 20) y: Int): String @c(gamma: 30)} "))]
    (is (= 40 (get-in schema [::schema/types-by-name "Query" ::schema/directives-by-name "d" ::g/arguments "delta"])))

    (is (= 30 (get-in schema [::schema/types-by-name "Query" ::schema/fields-by-name "someField" ::schema/directives-by-name "c" ::g/arguments "gamma"])))

    (is (= 10 (get-in schema [::schema/types-by-name "Query" ::schema/fields-by-name "someField" ::g/arguments-definition 0 ::schema/directives-by-name "a" ::g/arguments "alpha"])))

    (is (= 20 (get-in schema [::schema/types-by-name "Query" ::schema/fields-by-name "someField" ::g/arguments-definition 0 ::schema/directives-by-name "b" ::g/arguments "beta"])))))

(deftest enum-values-compilation-test
  (let [schema (compile-schema (parse "type Query { dog: DogCommand }
enum DogCommand { SIT, DOWN, HEEL @deprecated(reason: \"dogs dont like following orders\") }"))
        enum-values (get-in schema [::schema/types-by-name "DogCommand" ::g/enum-values])]
    (is (= "SIT" (get-in enum-values [0 ::g/name])))
    (is (= "deprecated" (get-in enum-values [2 ::g/directives 0 ::g/name])))))
