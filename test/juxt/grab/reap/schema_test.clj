;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.reap.schema-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.reap.alpha.graphql :as reap]
   [juxt.grab.alpha.reap.parser :as reap.parser]
   [juxt.grab.alpha.reap.document :as reap.document]
   [juxt.grab.alpha.document :as document]
   [juxt.grab.alpha.schema :as schema]
   [clojure.java.io :as io]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

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
                  :field-definitions
                  [#::reap{:name "name"
                           :type "String"}
                   #::reap{:name "picture"
                           :arguments-definition [#::reap{:name "size", :type "Int"}]
                           :type "Url"}]}]
         (reap.parser/parse-graphql example-56))))

(deftest schema-test
  (let [schema (-> (slurp (io/resource "juxt/grab/test-schema.graphql"))
                   reap.parser/parse-graphql
                   reap.document/parse-tree->schema)]
    (is (= "Query" (get-in schema [::schema/root-operation-type-names :query])))))

(deftest get-root-query-type-test
  (let [type (-> (slurp (io/resource "juxt/grab/test-schema.graphql"))
                 reap.parser/parse-graphql
                 reap.document/parse-tree->schema
                 schema/get-root-query-type)]
    (is (= "Query" (::g/name type)))
    (is (= :object (::g/kind type)))
    (is (= 1 (count (::g/field-definitions type))))
    (let [[_ user-field] (first (::g/field-definitions type))]
      (is (= "user" (::g/name user-field)))
      (is (= "Person" (::g/type user-field))))))

;; TODO: Write schema tests

#_(-> "type Root { users: [User] } type User { name: String }"
          parser/parse-graphql
          schema/parse-tree->schema)

#_(-> "type Root { user: [[User]!]! } type User { name: String }"
          parser/parse-graphql
          schema/parse-tree->schema)

#_(-> "type User { name: String }"
    parser/parse-graphql
    )
