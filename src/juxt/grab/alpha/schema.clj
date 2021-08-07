;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.schema
  (:require
   [juxt.reap.alpha.graphql :as reap]
   [juxt.grab.alpha.schema :as schema]))

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
                   ::kind :list
                   ::item-type (add-kind (::reap/item-type typ)))
      :non-null (assoc typ
                   ::kind :non-null
                   ::inner-type (add-kind (::reap/inner-type typ)))
      typ)
    typ))

(defn parse-tree->field
  [parse-tree]
  (let [typ (::reap/type parse-tree)]
    (merge
     parse-tree ;; preserve the reap entries, useful when reasoning.
     {::name (::reap/name parse-tree)
      ::type (add-kind typ)}
     (when-let [args (::reap/args parse-tree)]
       {::args (for [arg args]
                 (into
                  arg
                  {::name (::reap/name arg)
                   ::type (::reap/type arg)}))})
     )))

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
                          ::kind kind
                          ::name (::reap/name typ)
                          ::fields (->>
                                    (for [field (::reap/fields typ)]
                                      [(::reap/name field)
                                       ;; See https://spec.graphql.org/June2018/#sec-The-__Field-Type
                                       (parse-tree->field field)])
                                    (into {})))])))
             (into {"Int" {::name "Int"
                           ::kind :scalar}
                    "Float" {::name "Float"
                             ::kind :scalar}
                    "String" {::name "String"
                              ::kind :scalar}
                    "Boolean" {::name "Boolean"
                               ::kind :scalar}
                    "ID" {::name "ID"
                          ::kind :scalar}}))
        root-query-type-name
        (some-> (some-match parse-tree ::reap/type "SchemaDefinition")
                ::reap/root-operation-types
                (some-match ::reap/operation-type "query")
                ::reap/named-type)]

    {::types-by-name types-by-name
     ::root-operation-type-names {:query root-query-type-name}}))

;; Convenience accessors

(defn get-type [schema type-name]
  (let [result (get-in schema [::types-by-name type-name])]
    (assert result)
    result))

(defn get-root-query-type [schema]
  (get-type schema (get-in schema [::root-operation-type-names :query])))
