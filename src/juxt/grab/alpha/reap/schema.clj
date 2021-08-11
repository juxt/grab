;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.reap.schema
  (:require
   [juxt.grab.alpha.reap.parser :as parser]
   [juxt.reap.alpha.graphql :as reap]))

(alias 'schema (create-ns 'juxt.grab.alpha.schema))
(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(defn some-match [coll k v]
  (some #(when (= (get % k) v) %) coll))

(def type-kind-map
  {"ScalarTypeDefinition" :scalar
   "ObjectTypeDefinition" :object
   "InterfaceTypeDefinition" :interface
   "UnionTypeDefinition" :union
   "EnumTypeDefinition" :enum
   "InputObjectTypeDefinition" :input-object})

(defn add-kind [typ]
  (if (map? typ)
    (case (::reap/type typ)
      :list (assoc typ
                   ::g/kind :list
                   ::g/item-type (add-kind (::reap/item-type typ)))
      :non-null (assoc typ
                       ::g/kind :non-null
                       ::g/inner-type (add-kind (::reap/inner-type typ)))
      typ)
    typ))

(defn parse-tree->field
  [parse-tree]
  (let [typ (::reap/type parse-tree)]
    (merge
     parse-tree ;; preserve the reap entries, useful when reasoning.
     {::g/name (::reap/name parse-tree)
      ::g/type (add-kind typ)}
     (when-let [args (::reap/arguments-definition parse-tree)]
       {::g/arguments-definition (for [arg args]
                                   (into
                                    arg
                                    {::g/name (::reap/name arg)
                                     ::g/type (::reap/type arg)}))}))))

(defn parse-tree->schema
  "Return a grab-specified schema as a map from a reap-parsed document."
  [parse-tree]
  (let [types-by-name
        (->> parse-tree
             (keep
              (fn [typ]
                ;; See https://spec.graphql.org/June2018/#sec-The-__Type-Type
                (when-let [kind (type-kind-map (::reap/type typ))]
                  [(::reap/name typ)
                   (assoc typ
                          ::g/kind kind
                          ::g/name (::reap/name typ)
                          ::g/field-definitions
                          (->>
                           (for [field (::reap/field-definitions typ)]
                             [(::reap/name field)
                              ;; See https://spec.graphql.org/June2018/#sec-The-__Field-Type
                              (parse-tree->field field)])
                           (into {})))])))
             (into {"Int" {::g/name "Int"
                           ::g/kind :scalar}
                    "Float" {::g/name "Float"
                             ::g/kind :scalar}
                    "String" {::g/name "String"
                              ::g/kind :scalar}
                    "Boolean" {::g/name "Boolean"
                               ::g/kind :scalar}
                    "ID" {::g/name "ID"
                          ::g/kind :scalar}}))
        root-query-type-name
        (some-> (some-match parse-tree ::reap/type "SchemaDefinition")
                ::reap/root-operation-types
                (some-match ::reap/operation-type "query")
                ::reap/named-type)]

    {::schema/types-by-name types-by-name
     ::schema/root-operation-type-names {:query root-query-type-name}}))

(defn ->schema
  "Parse the input string to a data structure representing a GraphQL schema."
  [s]
  (-> s
      parser/parse-graphql
      parse-tree->schema))
