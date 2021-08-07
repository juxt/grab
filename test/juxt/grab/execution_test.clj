;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.execution-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.execution :as execution]
   [juxt.grab.alpha.document :refer [->document]]
   [juxt.grab.alpha.schema :refer [->schema]]
   [juxt.grab.alpha.schema :as schema]
   [juxt.grab.alpha.document :as document]))

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
        field (get-in document [::document/operations "Test" ::document/selection-set 0])
        variable-values {}]
    (is
     (= {"id" 4}
        (execution/coerce-argument-values
         {:object-type object-type
          :field field
          :variable-values variable-values})))))

#_(let [schema (->schema "
type Root { user(id: Int): Person }
type Person { name: String
              picture(size: Int): Url }")
      document (->document "query Test { user(id: 4) { name }}")

      object-type (schema/get-type schema "Root")
      field (get-in document [::document/operations "Test" ::document/selection-set 0])
      variable-values {}]

  (execution/coerce-argument-values
   {:object-type object-type
    :field field
    :variable-values variable-values}))
