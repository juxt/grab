;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.schema-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.graphql :as grab]))

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


(grab/parse-graphql
  (str
   example-56
   "
schema { query: Person }

query { name }
"))

(grab/validate-graphql-document
 (grab/parse-graphql
  (str
   example-56
   "
schema { query: Person }

query { name }
")))

(grab/parse-graphql
 "
schema { query: Person }
")
