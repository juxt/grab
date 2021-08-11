;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.reap.document
  (:require
   [juxt.grab.alpha.reap.parser :as parser]
   [juxt.reap.alpha.graphql :as reap]))

(alias 'document (create-ns 'juxt.grab.alpha.document))

(defn process-selection-set [selection-set]
  (mapv
   (fn [selection]
     (into
      selection
      (case (::reap/selection-type selection)
        :field
        (cond->
            {::document/selection-type :field
             ::document/name (::reap/name selection)
             ::document/arguments (::reap/arguments selection)}

          (::reap/selection-set selection)
          (assoc ::document/selection-set (process-selection-set (::reap/selection-set selection)))
          (::reap/directives selection)
          (assoc ::document/directives (::reap/directives selection))
          (::reap/alias selection)
          (assoc ::document/alias (::reap/alias selection)))

        :fragment-spread
        (cond->
            {::document/selection-type :fragment-spread
             ::document/fragment-name (::reap/fragment-name selection)}
          (::reap/named-type selection)
          (assoc ::document/named-type (::reap/named-type selection))
          (::reap/directives selection)
          (assoc ::document/directives (::reap/directives selection))
          (::reap/selection-set selection)
          (assoc ::document/selection-set (process-selection-set (::reap/selection-set selection))))

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
           acc [::document/operations-by-name nm]
           (into
            definition
            {::document/name nm
             ::document/operation-type (keyword op-type)
             ::document/directives directives
             ::document/selection-set (process-selection-set (::reap/selection-set definition))})))

        "FragmentDefinition"
        (let [nm (::reap/fragment-name definition)
              directives (::reap/directives definition)]
          (assoc-in
           acc [::document/fragments-by-name nm]
           (into
            definition
            (cond->
                {::document/fragment-name nm
                 ::document/named-type (::reap/named-type definition)
                 ::document/selection-set (process-selection-set (::reap/selection-set definition))}
              directives (assoc ::document/directives directives)))))))

    {::document/operations-by-name {}
     ::document/fragments-by-name {}})))

(defn ->document
  "Parse the input string to a data structure representing a GraphQL document."
  [input]
  (-> input
      parser/parse-graphql
      parse-tree->document))
