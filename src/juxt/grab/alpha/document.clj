;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document
  (:require
   [juxt.grab.alpha.parser :as parser]
   [juxt.reap.alpha.graphql :as reap]
   [juxt.grab.alpha.document :as document]))

(defn process-selection-set [selection-set]
  (mapv
   (fn [selection]
     (into
      selection
      (case (::reap/selection-type selection)
        :field
        (cond->
            {::selection-type :field
             ::name (::reap/name selection)
             ::arguments (::reap/arguments selection)}

          (::reap/selection-set selection)
          (assoc ::selection-set (process-selection-set (::reap/selection-set selection)))
          (::reap/directives selection)
          (assoc ::directives (::reap/directives selection))
          (::reap/alias selection)
          (assoc ::alias (::reap/alias selection)))

        :fragment-spread
        (cond->
            {::selection-type :fragment-spread
             ::fragment-name (::reap/fragment-name selection)}
          (::reap/named-type selection)
          (assoc ::named-type (::reap/named-type selection))
          (::reap/directives selection)
          (assoc ::directives (::reap/directives selection))
          (::reap/selection-set selection)
          (assoc ::selection-set (process-selection-set (::reap/selection-set selection))))

        :else
        (throw (ex-info "TODO" {:selection selection})))))
   selection-set))

(defn expand-shorthand-document [parse-tree]
  (let [shorthand?
        (fn [definition]
          (and
           (= (:juxt.reap.alpha.graphql/type definition) "OperationDefinition")
           (if-let [op-type (::reap/operation-type definition)]
             (= op-type "query")
             true)))]
    (if (= (count (filter shorthand? parse-tree)) 1)
      (map (fn [definition]
             (cond-> definition
               (shorthand? definition)
               (assoc ::reap/operation-type "query")))
           parse-tree)
      parse-tree)))

(defn parse-tree->document [parse-tree]
  (->>
   parse-tree
   expand-shorthand-document
   (reduce
    (fn [acc definition]
      (case (::reap/type definition)
        "OperationDefinition"
        (let [nm (::reap/name definition)
              op-type (::reap/operation-type definition)
              directives (::reap/directives definition)]
          (assoc-in
           acc [::operations-by-name nm]
           (into
            definition
            {::name nm
             ::operation-type (keyword op-type)
             ::directives directives
             ::selection-set (process-selection-set (::reap/selection-set definition))})))

        "FragmentDefinition"
        (let [nm (::reap/fragment-name definition)
              directives (::reap/directives definition)]
          (assoc-in
           acc [::fragments-by-name nm]
           (into
            definition
            (cond->
                {::fragment-name nm
                 ::named-type (::reap/named-type definition)
                 ::selection-set (process-selection-set (::reap/selection-set definition))}
              directives (assoc ::directives directives)))))))

    {::operations-by-name {}
     ::fragments-by-name {}})))

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
