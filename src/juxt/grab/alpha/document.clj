;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document
  (:require
   [juxt.grab.alpha.parser :as parser]
   [juxt.reap.alpha.graphql :as reap]
   [juxt.grab.alpha.document :as document]))

(defn process-selection-set [selection-set]
  (mapv
   (fn [selection]
     (let [field (::reap/field selection)
           fragment-spread (::reap/fragment-spread selection)]
       (cond
         field
         (into {::selection-type :field}
               (cond->
                   {::name (::reap/name field)
                    ::arguments (::reap/arguments field)}
                   (::reap/selection-set field)
                   (assoc ::selection-set (process-selection-set (::reap/selection-set field)))
                   (::reap/alias field)
                   (assoc ::alias (::reap/alias field))))
         fragment-spread
         {::selection-type :field-spread}
         :else
         (throw (ex-info "TODO" {:selection selection})))))
   selection-set))

(defn- expand-shorthand-document [parse-tree]
  (if (= (count parse-tree) 1)
    (let [operation (first parse-tree)]
      (if (= (keys operation) [::reap/selection-set])
        [(assoc operation ::reap/operation-type :query)]
        parse-tree))
    parse-tree))

(defn parse-tree->document [parse-tree]
  {::operations-by-name
   (->>
    parse-tree
    expand-shorthand-document
    (keep
     (fn [definition]
       (when-let [op-type (::reap/operation-type definition)]
         (let [nm (::reap/name definition)]
           [nm {::name nm
                ::operation-type (keyword op-type)
                ::selection-set (process-selection-set (::reap/selection-set definition))
                ::parse-tree definition}]))))
    (into {}))
   ::parse-tree (expand-shorthand-document parse-tree)})

(defn ->document
  "Parse the input string to a data structure representing a GraphQL document."
  [input]
  (-> input
      parser/parse-graphql
      parse-tree->document))

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
