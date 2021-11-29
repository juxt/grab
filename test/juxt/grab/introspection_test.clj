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

(set! clojure.core/*print-namespace-maps* false)

(defn example [n]
  (-> (format "juxt/grab/examples/example-%s.graphql" n)
      io/resource
      slurp
      parser/parse))

(deftest example-87-test
  (is
   (= {:data
       {:__type
        {:name "User",
         :fields
         [{:name "id" :type {:name "String"}}
          {:name "name" :type {:name "String"}}
          {:name "birthday" :type {:name "Date"}}]}}}

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
            (throw
             (ex-info
              (str "FAIL: " (pr-str [(get-in args [:object-type ::g/name])
                                     (get-in args [:field-name])]))
              args)))})))))

(deftest full-introspection-query-test
  (let [schema (schema/compile-schema (parser/parse "schema { query: TestQuery mutation: TestMutation subscription: TestSubscription } type TestQuery { foo: String } type TestMutation { test: String } type TestSubscription { bar: String }"))
        document (document/compile-document*
                  (parser/parse
                   (slurp (io/resource "juxt/grab/graphiql-introspection-query.graphql")))
                  schema)
        response (execute-request
                  {:schema schema
                   :document document
                   :field-resolver
                   (fn [args]
                     (throw (ex-info "FAIL" {:args args})))})]

    (is (= "TestQuery" (get-in response [:data :__schema :queryType :name])))
    (is (= "TestMutation" (get-in response [:data :__schema :mutationType :name])))
    (is (= "TestSubscription" (get-in response [:data :__schema :subscriptionType :name])))
    (let [types (get-in response [:data :__schema :types])]
      (is (= 16 (count types)))
      )))

(deftest type-name-introspection-test
  (is (=
       {:data {:person {:__typename "Person"}}}
       (let [schema (schema/compile-schema
                     (parser/parse "
type Query { person: Person } type Person { name: String }"))
             document (document/compile-document
                       (parser/parse
                        "
{ person { __typename } }")
                       schema)

             response
             (execute-request
              {:schema schema
               :document document
               :field-resolver
               (fn [args]
                 (condp =
                     [(get-in args [:object-type ::g/name])
                      (get-in args [:field-name])]
                     ["Query" "person"]
                     {:name "Isaac Newton"}

                     ["Person" "name"]
                     (get-in args [:object-value :name])

                     (throw (ex-info "Fail" args))))})]
         response))))

#_(deftest enums-test
    (let [schema (schema/compile-schema (parser/parse "type Query { foo: String } type TestMutation { test: String } type TestSubscription { bar: String }"))
        document (document/compile-document*
                  (parser/parse
                   (slurp (io/resource "juxt/grab/graphiql-introspection-query.graphql")))
                  schema)
        response (execute-request
                  {:schema schema
                   :document document
                   :field-resolver
                   (fn [args]
                     (throw (ex-info "FAIL" {:args args})))})]

      ))


#_(parser/parse "type Query { relationship: Relationship } enum Relationship { \"A friend\" FRIEND @foo BROTHER SISTER }")

#_(schema/compile-schema
 (parser/parse "type Query { status(a: String @foo): Status } enum Status { OPEN CLOSED @deprecated(reason: \"Unused\") }"))

#_(let [schema (schema/compile-schema (parser/parse "type Query { status: Status } enum Status { \"open\" OPEN CLOSED }"))
      document (document/compile-document*
                (parser/parse
                 "{ __schema { types { enumValues(includeDeprecated: true) { name description }  } } }")
                schema)
      response (execute-request
                {:schema schema
                 :document document
                 :field-resolver
                 (fn [args]
                   (throw (ex-info "FAIL" {:args args})))})]

  response

  )




#_(let [schema (schema/compile-schema
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
        (document/compile-document
         (parser/parse "{
  __type(name: \"User\") {
    name
    fields {
      name
      type {
        name
      }
      args
    }
  }
}
")
         schema)]

    document)





#_(let [schema (schema/compile-schema
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

    )

(deftest possible-types-test
  (is
   (=
    [{:name "Photo", :kind 'OBJECT} {:name "Person", :kind 'OBJECT}]
    (let [schema
          (schema/compile-schema
           (parser/parse
            (str "schema { query: SearchQuery } "
                 (-> "juxt/grab/examples/example-69.graphql" io/resource slurp))))

          document (document/compile-document
                    (parser/parse "{ __type(name: \"SearchResult\") { name possibleTypes { name kind } }}")
                    schema)]
      (-> (execute-request
           {:schema schema
            :document document
            :field-resolver (fn [_] (throw (ex-info "TODO" {})))})
          (get-in [:data :__type :possibleTypes]))))))
