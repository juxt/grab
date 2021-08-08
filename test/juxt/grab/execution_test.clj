;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.execution-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.execution :as execution]
   [juxt.grab.alpha.document :refer [->document] :as document]
   [juxt.grab.alpha.schema :refer [->schema] :as schema]))

(alias 'schema (create-ns 'juxt.grab.alpha.schema))

(set! *print-level* 20)

;; Just to get warmed up
(deftest warm-up-test
  (is
   (= {:data {"user" "mal"}, :errors []}
      (execution/execute-request
       {:schema (->schema "schema { query: User } type User { user: String }")
        :document (->document "query { user }")
        :field-resolver
        (fn [{:keys [field-name]}] (case field-name "user" "mal"))}))))

(deftest coerce-argument-values-test
  (let [schema (->schema "type Root { user(id: Int): Person }")
        document (->document "query Test { user(id: 4) { name }}")
        object-type (schema/get-type schema "Root")
        field (get-in document [::document/operations-by-name "Test" ::document/selection-set 0])
        variable-values {}]
    (is
     (= {"id" 4}
        (execution/coerce-argument-values
         {:object-type object-type
          :field field
          :variable-values variable-values})))))

(deftest execute-request-test
  (is
   (= {:data {"user" {"name" "Mark Zuckerberg"}}, :errors []}
      (let [schema (->schema "
schema { query: Root }
type Root { user(id: Int): Person }
type Person { name: String
              picture(size: Int): Url }")
            document (->document "query { user(id: 4) { name }}")
            users {4 {:name "Mark Zuckerberg"}}]

        (execution/execute-request
         {:schema schema
          :document document
          :field-resolver
          (fn [{:keys [field-name object-type object-value]
                {:strs [id]} :argument-values
                :as args}]
            (case (::schema/name object-type)
              "Root"
              (case field-name
                "user"
                (get users id)
                (throw (ex-info "TODO:1" args)))
              "Person"
              (case field-name
                "name" (:name object-value)
                (throw (ex-info "TODO:1b" args)))
              (throw (ex-info "TODO:2" args))))})))))


(deftest execute-request-with-list-test
  (is
   (= {:data
       {"users"
        [{"name" "Cheri Berthiaume", "picture" "/cb-800x600.jpg"}
         {"name" "Cruz Jarrard", "picture" "/cj-800x600.jpg"}
         {"name" "Viktoriya Zhukova", "picture" "/vz-800x600.jpg"}
         {"name" "Bernardina Vesely", "picture" "/bv-800x600.jpg"}]},
       :errors []}

      (let [schema (->schema "
schema { query: Root }
scalar Url
type Root { users(filter: String): [Person] }
type Person { name: String
              picture(size: Int): Url }")
            document (->document "query { users { name picture(size: 1) }}")
            users {1 {:name "Cheri Berthiaume" :slug "cb"}
                   2 {:name "Cruz Jarrard" :slug "cj"}
                   3 {:name "Viktoriya Zhukova" :slug "vz"}
                   4 {:name "Bernardina Vesely" :slug "bv"}}]

        (execution/execute-request
         {:schema schema
          :document document
          :field-resolver
          (fn [{:keys [field-name object-type object-value argument-values]
                :as args}]
            (case (::schema/name object-type)
              "Root"
              (case field-name
                "users"
                (vals users)
                (throw (ex-info "Fail" args)))
              "Person"
              (case field-name
                "name" (:name object-value)
                "picture" (case (get argument-values "size")
                            1 (format "/%s-800x600.jpg" (:slug object-value))
                            2 (format "/%s-1024x768.jpg" (:slug object-value)))
                (throw (ex-info "Fail" args)))
              (throw (ex-info "Fail" args))))})))))
