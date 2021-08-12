;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document)

(defn
  ^{:graphql/name "GetOperation"}
  get-operation
  [doc operation-name]

  (if (nil? operation-name)
    ;; 1. If operationName is null:
    (if (= (count (::operations-by-name doc)) 1)
      ;; a. If document contains exactly one operation.
      ;; i. Return the Operation contained in the document.
      (second (first (::operations-by-name doc)))
      ;; ii. Otherwise produce a query error requiring operationName.
      (throw (ex-info "Operation name required" {}))
      )
    ;; 2. Otherwise:
    (let [operation (get (::operations-by-name doc) operation-name)]
      ;; a. Let operation be the Operation named operationName in document.
      (if (nil? operation)
        ;; b. If operation was not found, produce a query error.
        (throw (ex-info "Operation not found" {:operation-name operation-name}))
        ;; c. Return operation.
        operation))))

(defn get-type [schema type-name]
  (let [result (get-in schema [::types-by-name type-name])]
    (assert result (format "Type not found: %s" type-name))
    result))

(defn get-root-query-type [schema]
  (if-let [root-type (get-in schema [::root-operation-type-names :query])]
    (get-type schema root-type)
    (throw (ex-info "Query root type not found" {}))))
