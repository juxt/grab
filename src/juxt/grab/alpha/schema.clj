;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.schema)

(defn some-match [coll k v]
  (some #(when (= (get % k) v) %) coll))

(defn type-definition? [s]
  (contains?
   #{"ScalarTypeDefinition"
     "ObjectTypeDefinition"
     "InterfaceTypeDefinition"
     "UnionTypeDefinition"
     "EnumTypeDefinition"
     "InputObjectTypeDefinition"}
   s))

(defn document->schema
  ""
  [doc]
  (let [types-by-name
        (->> doc
             (filter (comp type-definition? :type))
             (map (juxt :name identity))
             (into {}))
        root-query-type-name
        (some-> (some-match doc :type "SchemaDefinition")
                :root-operation-types
                (some-match :operation-type "query")
                :named-type)]

    {:types-by-name types-by-name
     :root-operation-type-names {:query root-query-type-name}}))
