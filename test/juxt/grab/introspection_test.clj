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

(deftest root-types-test
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
                     (throw (ex-info "TODO" {:args args})))})]

    (is (= "TestQuery" (get-in response [:data :__schema :queryType :name])))
    (is (= "TestMutation" (get-in response [:data :__schema :mutationType :name])))
    (is (= "TestSubscription" (get-in response [:data :__schema :subscriptionType :name])))))


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
       (parser/parse (slurp (io/resource "juxt/grab/example-87.graphql")))
        schema)]

  document)


#_(let [schema (schema/compile-schema (parser/parse (slurp (io/resource "juxt/grab/schema-3.graphql"))))
      document (document/compile-document*
                (parser/parse
                 (slurp (io/resource "juxt/grab/graphiql-introspection-query.graphql")))
                schema)]
  (execute-request
   {:schema schema
    :document document
    :field-resolver
    (fn [args]
      (throw (ex-info "TODO" {:args args})))}))


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
