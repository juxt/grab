;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.schema-compilation-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.schema :refer [compile-schema compile-schema*] :as schema]
   [juxt.grab.alpha.parser :refer [parse]]
   [clojure.java.io :as io]
   [juxt.grab.alpha.parser :as parser]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(defmacro with-schema-compile [bindings & body]
  `((fn [~(first bindings)] ~@body)
    (-> ~(second bindings)
        parse
        compile-schema)))


(defn get-or-nth-in [coll ks]
  (if (seq ks)
    (condp instance? (first ks)
      Number (get-or-nth-in (nth coll (first ks)) (rest ks))
      clojure.lang.Keyword (get-or-nth-in ((first ks) coll) (rest ks)))
    coll))

(deftest directive-compilation-test
  (with-schema-compile [schema
                        "type Query @d(delta: 40) { someField(x: Int @a(alpha: 10) @b(beta: 20) y: Int): String @c(gamma: 30)} "]
    (is (= 40 (get-in schema [::schema/types-by-name "Query" ::schema/directives-by-name "d" ::g/arguments "delta"])))

    (is (= 30 (get-in schema [::schema/types-by-name "Query" ::schema/fields-by-name "someField" ::schema/directives-by-name "c" ::g/arguments "gamma"])))

    (is (= 10 (get-in schema [::schema/types-by-name "Query" ::schema/fields-by-name "someField" ::g/arguments-definition 0 ::schema/directives-by-name "a" ::g/arguments "alpha"])))

    (is (= 20 (get-in schema [::schema/types-by-name "Query" ::schema/fields-by-name "someField" ::g/arguments-definition 0 ::schema/directives-by-name "b" ::g/arguments "beta"])))))


(deftest enum-values-compilation-test
  (with-schema-compile [schema "type Query { dog: DogCommand }
enum DogCommand { SIT, DOWN, HEEL @deprecated(reason: \"dogs dont like following orders\") }"]
    (let [enum-values (get-in schema [::schema/types-by-name "DogCommand" ::g/enum-values])]
      (is (= "SIT" (get-or-nth-in enum-values [0 ::g/name])))
      (is (= "deprecated" (get-or-nth-in enum-values [2 ::g/directives 0 ::g/name]))))))

#_(parse (slurp (io/resource "juxt/grab/alpha/meta-schema.graphql")))
