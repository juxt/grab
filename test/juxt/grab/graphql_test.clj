;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.graphql-test
  (:require
   [juxt.grab.alpha.execution :refer [execute-request]]
   [juxt.grab.alpha.document :refer [->document] :as document]
   [juxt.grab.alpha.schema :refer [->schema] :as schema]
   [clojure.test :refer [deftest is are testing]]
   [clojure.java.io :as io]))

;; 2.4 Selection Sets

;; "An operation selects the set of information it needs, and will receive
;; exactly that information and nothing more, avoiding over-fetching and
;; under-fetching data."

(defn execute [document schema field-resolver]
  (execute-request {:document (->document document)
                    :schema (->schema schema)
                    :field-resolver field-resolver}))

(deftest selection-sets-test
  (are [query expected]
      (=
       {:data expected :errors []}
       (execute
        query
        (slurp (io/resource "juxt/grab/schema-1.graphql"))
        (fn [{:keys [field-name]}]
          (case field-name
            "id" 4
            "firstName" "Mark"
            "lastName" "Zuckerberg"))))

      "{ id firstName lastName }"
      {"id" 4
       "firstName" "Mark"
       "lastName" "Zuckerberg"}

      ;; Avoid over-fetching data
      "{ firstName }"
      {"firstName" "Mark"}))
