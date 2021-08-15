;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document)

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

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

(defn executable
  "Validate document as an executable, returning a structure that is efficient for
  execution."
  [document]
  (when-not (every? #(#{:executable-definition} (::g/definition-type %)) document)
    (throw (ex-info "A document containing a TypeSystemDefinition is invalid for execution" {:document document})))

  (let [operations (filter #(contains? % ::g/operation-type) document)
        operations-by-name (group-by ::g/name operations)
        anonymous (get operations-by-name nil)]

    (when (> (count operations) 1)
      (when-not (empty? anonymous)
        (throw
         (ex-info "When there are multiple operations in the document, none can be anonymous" {}))))

    {::operations-by-name
     (->> operations-by-name
          (reduce-kv
           (fn [acc k v]
             (when (> (count v) 1)
               (throw (ex-info "Operation name is not unique" {:name k})))
             (assoc acc k (first v)))
           {}))}))
