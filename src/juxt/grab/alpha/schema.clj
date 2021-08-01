;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.schema)

(defn some-match [coll k v]
  (some #(when (= (get % k) v) %) coll))

(defn document->schema
    ""
    [doc]
    (let [root-query-type-name
          (some-> (some-match doc :type "SchemaDefinition")
                  :root-operation-types
                  (some-match :operation-type "query")
                  :named-type)]

      {:root-operation-types {:query (some-match doc :name root-query-type-name)}}))
