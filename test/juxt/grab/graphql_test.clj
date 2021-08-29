;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.graphql-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.execution :refer [execute-request]]
   [juxt.grab.alpha.parser :as parser]
   [clojure.java.io :as io]
   [juxt.grab.alpha.document :as document]
   [juxt.grab.alpha.schema :as schema]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(deftest warmup-test
  (is
   (= {:data
       {"user"
        {"name" "Isaac Newton",
         "profilePic" "https://profile.juxt.site/pic-100.png"}},
       :errors []}
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
