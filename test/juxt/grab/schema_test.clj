;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.schema-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.reap.alpha.graphql :as reap]
   [juxt.grab.alpha.parser :as parser]
   [juxt.grab.alpha.schema :as schema]
   [clojure.java.io :as io]))

(def example-56 "
type Person @crux(query: \"{:find [e] :where [[?e :name][?e :picture ?p][?p :size $size]]}\") {
  name: String
  picture(size: Int): Url}")

(deftest parse-example-56-test
  (is (= [#::reap{:type "ObjectTypeDefinition",
                  :name "Person",
                  :directives
                  {"crux"
                   {"query"
                    "{:find [e] :where [[?e :name][?e :picture ?p][?p :size $size]]}"}},
                  :fields
                  [#::reap{:name "name", :type "String"}
                   #::reap{:name "picture", :args [#::reap{:name "size", :type "Int"}], :type "Url"}]}]
         (parser/parse-graphql example-56))))

(deftest schema-test
  (let [schema (-> (slurp (io/resource "juxt/grab/test.graphql"))
                   parser/parse-graphql
                   schema/parse-tree->schema)]
    (is (= "Person" (get-in schema [::schema/root-operation-type-names :query])))))

(deftest get-root-query-type-test
  (let [type (-> (slurp (io/resource "juxt/grab/test.graphql"))
                 parser/parse-graphql
                 schema/parse-tree->schema
                 schema/get-root-query-type)]
    (is (= "Person" (::reap/name type)))
    (is (= :object (::schema/kind type)))
    (is (= 2 (count (::reap/fields type))))))
