;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.schema-compilation-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.schema :refer [compile-schema] :as schema]
   [juxt.grab.alpha.parser :refer [parse]]
   [clojure.java.io :as io]
   [juxt.grab.alpha.parser :as parser]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))
(alias 'g (create-ns 'juxt.grab.alpha.graphql))



(deftest directive-compilation-test
  (let [schema
        (-> "type Query @d(delta: 40) { someField(x: Int @a(alpha: 10) @b(beta: 20) y: Int): String @c(gamma: 30)} "
            parse
            compile-schema
            )]
    (is (= 40 (get-in schema [::schema/types-by-name "Query" ::schema/directives-by-name "d" ::g/arguments "delta"])))

    (is (= 30 (get-in schema [::schema/types-by-name "Query" ::schema/fields-by-name "someField" ::schema/directives-by-name "c" ::g/arguments "gamma"])))

    (is (= 10 (get-in schema [::schema/types-by-name "Query" ::schema/fields-by-name "someField" ::g/arguments-definition 0 ::schema/directives-by-name "a" ::g/arguments "alpha"])))

    (is (= 20 (get-in schema [::schema/types-by-name "Query" ::schema/fields-by-name "someField" ::g/arguments-definition 0 ::schema/directives-by-name "b" ::g/arguments "beta"])))))


#_(parse (slurp (io/resource "juxt/grab/alpha/meta-schema.graphql")))
