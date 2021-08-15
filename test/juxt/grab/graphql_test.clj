;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.graphql-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.execution :refer [execute-request]]
   [juxt.grab.alpha.parser :as parser]
   [clojure.java.io :as io]
   [juxt.grab.alpha.document :as document]
   [juxt.grab.alpha.schema :as schema]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

#_(deftest selection-sets-test
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

(deftest warmup-test
  (is
   (= {:data
       {"user"
        {"name" "Isaac Newton",
         "profilePic" "https://profile.juxt.site/pic-100.png"}},
       :errors []}
      (let [schema (schema/schema
                    (parser/parse (slurp (io/resource "juxt/grab/schema-3.graphql"))))
            document (document/executable
                      (parser/parse (slurp (io/resource "juxt/grab/query-3.graphql"))))]

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
(schema/schema
 (parser/parse (slurp (io/resource "juxt/grab/schema-3.graphql"))))

(let [schema (schema/schema
              (parser/parse (slurp (io/resource "juxt/grab/schema-3.graphql"))))
      document (document/executable
                (parser/parse (slurp (io/resource "juxt/grab/query-3.graphql"))))]

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

          (throw (ex-info "" args))))}))
