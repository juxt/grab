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

(defn parse-tree->schema
  "Return a grab-specified schema as a map from a reap-parsed document."
  [parse-tree]
  (let [types-by-name
        (->> parse-tree
             (keep
              #(when-let [kind (type-kind-map (::reap/type %))]
                 [(::reap/name %) (assoc % ::kind kind)]))
             (into {}))
        root-query-type-name
        (some-> (some-match parse-tree ::reap/type "SchemaDefinition")
                ::reap/root-operation-types
                (some-match ::reap/operation-type "query")
                ::reap/named-type)]

    {::types-by-name types-by-name
     ::root-operation-type-names {:query root-query-type-name}}))

;; Convenience accessors

(defn get-type [schema type-name]
  (get-in schema [::types-by-name type-name]))

(defn get-root-query-type [schema]
  (get-type schema (get-in schema [::root-operation-type-names :query])))
