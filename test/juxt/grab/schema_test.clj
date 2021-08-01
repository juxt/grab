;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.schema-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.graphql :as grab]
   [juxt.grab.alpha.schema :as schema]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def example-56 "
type Person @crux(query: \"{:find [e] :where [[?e :name][?e :picture ?p][?p :size $size]]}\") {
  name: String
  picture(size: Int): Url}")

(deftest parse-example-56-test
    (is (= [{:type "ObjectTypeDefinition",
             :name "Person",
             :directives
             {"crux"
              {"query"
               "{:find [e] :where [[?e :name][?e :picture ?p][?p :size $size]]}"}},
             :fields
             [{:name "name", :type "String"}
              {:name "picture", :args [{:name "size", :type "Int"}], :type "Url"}]}]
           (grab/parse-graphql example-56))))

(deftest schema-test
  (let [schema (-> (slurp (io/resource "juxt/grab/test.graphql"))
                   grab/parse-graphql
                   grab/validate-graphql-document
                   schema/document->schema)]
    (is (= "Person" (get-in schema [:root-operation-type-names :query])))))

(deftest get-root-query-type-test
  (is (= 2
         (-> (slurp (io/resource "juxt/grab/test.graphql"))
             grab/parse-graphql
             grab/validate-graphql-document
             schema/document->schema
             schema/get-root-query-type
             :fields
             count))))

#_(def schema-string
  (str/join
   " "
   [example-56
    "schema { query: Person }"]))

#_(def schema
  (schema/document->schema
   (grab/validate-graphql-document
    (grab/parse-graphql schema-string))))

#_(let [document
      (grab/validate-graphql-document
       (grab/parse-graphql
        "query { name }"))]

  (grab/execute-request
   {:schema
    (->Schema schema)
    :document document
    :operation-name nil
    :variable-values {}
    :initial-value {:db 'db}
    :field-resolver {}
    }))
