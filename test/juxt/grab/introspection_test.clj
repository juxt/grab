;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.introspection-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.execution :refer [execute-request]]
   [juxt.grab.alpha.parser :as parser]
   [clojure.java.io :as io]
   [juxt.grab.alpha.document :as document]
   [juxt.grab.alpha.schema :as schema]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(defn example [n]
  (-> (format "juxt/grab/example-%s.graphql" n)
      io/resource
      slurp
      parser/parse))

(deftest example-87-test
  (is
   (= {:data
       {"__type"
        {"name" "User",
         "fields"
         [{"name" "id", "type" {"name" "String"}}
          {"name" "name", "type" {"name" "String"}}
          {"name" "birthday", "type" {"name" "Date"}}]}},
       :errors []}

      (let [schema (schema/compile-schema
                    (parser/parse
;; We prepare schema which is a super-set of the schema in the text, because we
;; need to declare the scalar 'Date' and the Query type.
                     "
scalar Date

type User {
  id: String
  name: String
  birthday: Date
}

type Query {
  user: User
}
"))
            document
            (document/compile-document (example "87") schema)]

        (is (empty? (::schema/errors schema)))
        (is (empty? (::schema/errors document)))

        (execute-request
         {:schema schema
          :document document
          :field-resolver
          (fn [args]
            (let [provided-types (::schema/provided-types schema)]
              (condp =
                  [(get-in args [:object-type ::g/name])
                   (get-in args [:field-name])]
                  ["Root" "user"]
                  {:name "Isaac Newton"}

                  ["Person" "name"]
                  (get-in args [:object-value :name])

                  ["Person" "profilePic"]
                  (format "https://profile.juxt.site/pic-%d.png" (get-in args [:argument-values "size"]))

                  ["Query" "__type"]
                  (get-in schema [:juxt.grab.alpha.schema/provided-types (get-in args [:argument-values "name"])])

                  ["__Type" "name"]
                  (get-in args [:object-value ::g/name])

                  ["__Type" "fields"]
                  (get-in args [:object-value ::g/field-definitions])

                  ["__Field" "name"]
                  (get-in args [:object-value ::g/name])

                  ["__Field" "type"]
                  (let [type-ref (get-in args [:object-value ::g/type-ref])
                        typ (some-> type-ref ::g/name provided-types)]
                    {::g/name (::g/name (schema/unwrapped-type type-ref))}
                    #_(cond-> {:kind (cond
                                       (::g/list-type type-ref) :list
                                       (::g/non-null-type type-ref) :non-null
                                       (::g/name type-ref) (::g/kind typ))

                               :name (::g/name (schema/unwrapped-type type-ref))}))

                  (throw
                   (ex-info
                    (str "TODO: " (pr-str [(get-in args [:object-type ::g/name])
                                           (get-in args [:field-name])]))
                    args)))))})))))
