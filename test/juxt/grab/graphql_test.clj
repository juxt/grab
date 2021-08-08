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

(defn execute [args]
  (execute-request
   (-> args
       (update :document ->document)
       (update :schema ->schema))))

(deftest selection-sets-test
  (let [field-resolver
        (fn [{:keys [field-name]}]
          (case field-name
            "id" 4
            "firstName" "Mark"
            "lastName" "Zuckerberg"))]
    (are [query expected]
        (=
         {:data expected :errors []}
         (execute
          {:document query
           :schema (slurp (io/resource "juxt/grab/schema-1.graphql"))
           :field-resolver field-resolver}))

        "{ id firstName lastName }"
        {"id" 4
         "firstName" "Mark"
         "lastName" "Zuckerberg"}

        ;; Avoid over-fetching data
        "{ firstName }"
        {"firstName" "Mark"})))

;; 2.5 Fields

(deftest fields-test
  (let [field-resolver
        (fn [args]
          (condp = [(get-in args [:object-type ::schema/name])
                    (get-in args [:field-name])]
            ["Root" "me"]
            {:id 1
             :first-name "Isaac"
             :last-name "Newton"
             :birthday (java.time.MonthDay/of 1 4)
             :friends [{:id 2
                        :name "Gottfried Wilhelm Leibniz"}]}

            ["User" "id"] (get-in args [:object-value :id])
            ["User" "firstName"] (get-in args [:object-value :first-name])
            ["User" "lastName"] (get-in args [:object-value :last-name])
            ["User" "birthday"] (get-in args [:object-value :birthday])
            ["Birthday" "month"] (.getValue (.getMonth (:object-value args)))
            ["Birthday" "day"] (.getDayOfMonth (:object-value args))
            ["User" "friends"] (get-in args [:object-value :friends])
            ["Friend" "name"] (get-in args [:object-value :name])

            (throw (ex-info "Resolve field" args))))]

    (is
     (= {:data
         {"me"
          {"id" 1
           "firstName" "Isaac"
           "lastName" "Newton"
           "birthday" {"month" 1 "day" 4}
           "friends" [{"name" "Gottfried Wilhelm Leibniz"}]}}
         :errors []}

        (execute
         {:document (slurp (io/resource "juxt/grab/example-08.graphql"))
          :schema (slurp (io/resource "juxt/grab/schema-2.graphql"))
          :field-resolver field-resolver
          :initial-value {}})))))
