;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.schema)

(defn some-match [coll k v]
  (some #(when (= (get % k) v) %) coll))

(def type-kind-map
  {"ScalarTypeDefinition" :scalar
   "ObjectTypeDefinition" :object
   "InterfaceTypeDefinition" :interface
   "UnionTypeDefinition" :union
   "EnumTypeDefinition" :enum
   "InputObjectTypeDefinition" :input-object})

(defn document->schema
  "Return a grab-specified schema as a map from a reap-parsed document."
  [doc]
  (let [types-by-name
        (->> doc
             (keep
              #(when-let [kind (type-kind-map (:type %))]
                 [(:name %) (assoc % :kind kind)]))
             (into {}))
        root-query-type-name
        (some-> (some-match doc :type "SchemaDefinition")
                :root-operation-types
                (some-match :operation-type "query")
                :named-type)]

    {:types-by-name types-by-name
     :root-operation-type-names {:query root-query-type-name}}))

;; Convenience accessors

(defn get-root-query-type [doc]
  (get-in doc [:types-by-name (get-in doc [:root-operation-type-names :query])]))
