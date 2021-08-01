;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.graphql-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.graphql :as grab]))

(set! *print-level* 20)

(deftest parse-query-test
  (is (= '({:operation-type "query",
            :selection-set [[:field {:name "user", :arguments {}}]]})
         (-> "query { user }"
             grab/parse-graphql
             grab/validate-graphql-document))))

(deftest get-operation-test
  (let [doc (-> "query foo { user } query bar { user }"
                grab/parse-graphql
                grab/validate-graphql-document)]
    (are [arg expected]
        (= expected (:name (grab/get-operation doc arg)))
      "foo" "foo"
      "bar" "bar")
    (is (thrown? clojure.lang.ExceptionInfo
                 (grab/get-operation doc nil))
        "Otherwise produce a query error requiring operationName.")
    (is (thrown? clojure.lang.ExceptionInfo
                 (grab/get-operation doc "zip"))
        "If operation was not found, produce a query error.")))

;;
(deftest badly-formed-trailing-text
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    (grab/parse-graphql "query foo { user } query bar { }"))
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
