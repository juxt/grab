;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.schema)

;; Convenience accessors

(defn get-type [schema type-name]
  (let [result (get-in schema [::types-by-name type-name])]
    (assert result (format "Type not found: %s" type-name))
    result))

(defn get-root-query-type [schema]
  (if-let [root-type (get-in schema [::root-operation-type-names :query])]
    (get-type schema root-type)
    (throw (ex-info "Query root type not found" {}))))
