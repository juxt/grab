;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.schema)

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

;; Convenience accessors

(defn schema
  "Create a schema from the parsed document."
  [document]

  {::root-operation-type-names
   (second (first (first (filter #(contains? % ::g/schema) document))))

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

(defn get-type [schema type-name]
  (let [result (get-in schema [::types-by-name type-name])]
    (assert result (format "Type not found: %s" type-name))
    result))

(defn get-root-query-type [schema]
  (if-let [root-type (get-in schema [::root-operation-type-names :query])]
    (get-type schema root-type)
    (throw (ex-info "Query root type not found" {}))))
