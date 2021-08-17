;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document
  (:refer-clojure :exclude [compile]))

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

(defn ^{:juxt/see "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  validate-executable-definitions [{::keys [document] :as acc}]
  (when-not (every? #(#{:executable-definition} (::g/definition-type %)) document)
    (update acc ::errors conj
            {:error "A document containing a TypeSystemDefinition is invalid for execution"})))

(defn add-operations [{::keys [document] :as acc}]
  (let [operations (keep ::g/operation-definition document)]
    (assoc acc
           ::operations (vec operations)
           ::operations-grouped-by-name (group-by ::g/name operations))))

(defn ^{:juxt/see "https://spec.graphql.org/June2018/#sec-Lone-Anonymous-Operation"}
  validate-anonymous
  [{::keys [operations operations-grouped-by-name] :as acc}]
  (assert operations)
  (when (> (count operations) 1)
    (when-not (empty? (get operations-grouped-by-name nil))
      (update acc ::errors conj {:error "When there are multiple operations in the document, none can be anonymous"}))))

(defn validate-operation-uniqueness [{::keys [operations-grouped-by-name] :as acc}]
  (reduce-kv
   (fn [acc n ops]
     (if (> (count ops) 1)
       (update acc ::errors conj {:error (format "Operation name '%s' is not unique" n) :name n})
       (assoc-in acc [::operations-by-name n] (first ops))))
   acc
   operations-grouped-by-name))

(defn compile
  "Compile a document with respect to the given schema, returning a structure that
  can be provided to the execution functions."
  [document schema]

  (reduce
   (fn [acc validator]
     (or (validator acc) acc))

   {::errors []
    ::document document
    ::schema schema}

   [validate-executable-definitions
    add-operations
    validate-anonymous
    validate-operation-uniqueness]))
