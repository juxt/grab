;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.schema)

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

;; Convenience accessors

(defn compile-schema
  "Create a schema from the parsed document."
  [document]

  {::root-operation-type-names
   (or
    (second (first (first (filter #(contains? % ::g/schema) document))))
    {:query "Query" :mutation "Mutation" :subscription "Subscription"})

   ::types-by-name
   (->> document
        (filter #(= (::g/definition-type %) :type-definition))
        (map (juxt ::g/name identity))
        (into {"Int" {::g/name "Int"
                      ::g/kind :scalar}
               "Float" {::g/name "Float"
                        ::g/kind :scalar}
               "String" {::g/name "String"
                         ::g/kind :scalar}
               "Boolean" {::g/name "Boolean"
                          ::g/kind :scalar}
               "ID" {::g/name "ID"
                     ::g/kind :scalar}}))})
