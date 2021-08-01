;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.execution-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [clojure.java.io :as io]
   [juxt.grab.alpha.parser :as parser]
   [juxt.grab.alpha.execution :as execution]))

(alias 'schema (create-ns 'juxt.grab.alpha.schema))

(set! *print-level* 20)



(deftest badly-formed-trailing-text
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    (parser/parse-graphql "query foo { user } query bar { }"))
   "The final query clause is invalid and this should cause an error"))

#_(deftest query-test
  (is (= {"users"
          [{"username" "mal", "email" "mal@juxt.pro"}
           {"username" "jdt", "email" "jdt@juxt.pro"}
           {"username" "tim", "email" "tim@juxt.pro"}]}

         (let [document
               (->  "query { users { username email } }"
                    #_"{ user }"
                    grab/parse-graphql
                    grab/validate-graphql-document)]

           (grab/execute-request
            {:schema
             (-> (slurp (io/resource "juxt/grab/test.graphql"))
                 grab/parse-graphql
                 grab/validate-graphql-document
                 schema/document->schema)

             :document document

             :variable-values {}

             :initial-value {"users"
                             [{"username" "mal" "email" "mal@juxt.pro"}
                              {"username" "jdt" "email" "jdt@juxt.pro"}
                              {"username" "tim" "email" "tim@juxt.pro"}]}

             :field-resolver
             (fn [{:keys [object-type object-value field-name argument-values] :as field}]
               (cond

                 (= field-name "user")
                 "Malcolm"

                 (= field-name "users")
                 (get object-value "users")

                 (= field-name "username")
                 (get object-value "username")

                 (= field-name "email")
                 (get object-value "email")

                 :else
                 (throw
                  (ex-info
                   "TODO: Resolve field"
                   {:field field}))))})))))


#_(let [document
      (->  "query { user(id: 4) { name } }"
           grab/parse-graphql
           )]

  (grab/execute-request
   {:schema
    (-> (slurp (io/resource "juxt/grab/test.graphql"))
        grab/parse-graphql
        schema/document->schema)

    :document document

    :variable-values {}

    :initial-value {:_hint "initial-value"}

    :field-resolver
    (fn [{:keys [object-type object-value field-name argument-values] :as field}]
      (cond

        (= field-name "user")
        "Malcolm"

        (= field-name "users")
        (get object-value "users")

        (= field-name "username")
        (get object-value "username")

        (= field-name "email")
        (get object-value "email")

        :else
        (throw
         (ex-info
          "TODO: Resolve field"
          {:field field}))))}))
