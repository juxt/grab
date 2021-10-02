;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.execution-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.execution :refer [execute-request]]
   [juxt.grab.alpha.parser :as parser]
   [clojure.java.io :as io]
   [juxt.grab.alpha.document :as document]
   [juxt.grab.alpha.schema :as schema]
   [clojure.string :as str]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(deftest warmup-test
  (is
   (= {:data
       {:user
        {:name "Isaac Newton",
         :profilePic "https://profile.juxt.site/pic-100.png"}}}
      (let [schema (schema/compile-schema
                    (parser/parse (slurp (io/resource "juxt/grab/schema-3.graphql"))))
            document (document/compile-document
                      (parser/parse (slurp (io/resource "juxt/grab/query-3.graphql")))
                      schema)]

        (execute-request
         {:schema schema
          :document document
          :field-resolver
          (fn [args]
            (condp =
                [(get-in args [:object-type ::g/name])
                 (get-in args [:field-name])]
                ["Root" "user"]
                {:name "Isaac Newton"}

                ["Person" "name"]
                (get-in args [:object-value :name])

                ["Person" "profilePic"]
                (format "https://profile.juxt.site/pic-%d.png" (get-in args [:argument-values "size"]))

                (throw (ex-info "" args))))})))))


;; 6.4.3  Value Completion

;; 4. If fieldType is a Scalar or Enum type:
;;   a. Return the result of “coercing” result, ensuring it is a legal value of
;;   fieldType, otherwise null.

(let [schema
      (schema/compile-schema
       (parser/parse
        (str/join
         \newline
         ["type Query { hero(episode: ID!): Person! }"
          "type Person {"
          "  id: ID!"
          "  name: String"
          "  friends: [Person!]"
          "}"])))
      document (document/compile-document
                (parser/parse
                 (slurp (io/resource "juxt/grab/example-184.graphql")))
                schema)]

  (execute-request
   {:schema schema
    :document document
    :field-resolver
    (fn [{:keys [object-type object-value field-name] :as args}]
      (condp = [(::g/name object-type) field-name]
        ["Query" "hero"]
        {:name "R2-D2"
         :friends [{:id "1000" :name "Luke Skywalker"}
                   {:id "1002"}
                   {:id "1003" :name "Leia Organa"}]}

        ["Person" "id"]
        (get object-value :id)

        ["Person" "name"]
        (if (= (:id object-value) "1002")
          (throw
           (ex-info
            (format
             "Name for character with ID %s could not be fetched."
             (:id object-value))
            {}))
          (get object-value :name))

        ["Person" "friends"]
        (get object-value :friends)

        (throw
         (ex-info
          "TODO"
          {:case [(::g/name object-type) field-name]
           :args args}))))}))
