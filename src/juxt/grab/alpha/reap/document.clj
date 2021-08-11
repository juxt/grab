;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.reap.document
  (:require
   [juxt.grab.alpha.reap.parser :as parser]
   [juxt.reap.alpha.graphql :as reap]))

(alias 'document (create-ns 'juxt.grab.alpha.document))
(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(defn process-selection-set [selection-set]
  (mapv
   (fn [selection]
     (into
      selection
      (case (::reap/selection-type selection)
        :field
        (cond->
            {::g/selection-type :field
             ::g/name (::reap/name selection)
             ::g/arguments (::reap/arguments selection)}

          (::reap/selection-set selection)
          (assoc ::g/selection-set (process-selection-set (::reap/selection-set selection)))
          (::reap/directives selection)
          (assoc ::g/directives (::reap/directives selection))
          (::reap/alias selection)
          (assoc ::g/alias (::reap/alias selection)))

        :fragment-spread
        (cond->
            {::g/selection-type :fragment-spread
             ::g/fragment-name (::reap/fragment-name selection)}
          (::reap/named-type selection)
          (assoc ::g/named-type (::reap/named-type selection))
          (::reap/directives selection)
          (assoc ::g/directives (::reap/directives selection))
          (::reap/selection-set selection)
          (assoc ::g/selection-set (process-selection-set (::reap/selection-set selection))))

        :else
        (throw (ex-info "TODO" {:selection selection})))))
   selection-set))

(defn expand-shorthand-document [parse-tree]
  (let [shorthand?
        (fn [definition]
          (and
           (= (::reap/type definition) "OperationDefinition")
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
            {::g/name nm
             ::g/operation-type (keyword op-type)
             ::g/directives directives
             ::g/selection-set (process-selection-set (::reap/selection-set definition))})))

        "FragmentDefinition"
        (let [nm (::reap/fragment-name definition)
              directives (::reap/directives definition)]
          (assoc-in
           acc [::g/fragments-by-name nm]
           (into
            definition
            (cond->
                {::g/fragment-name nm
                 ::g/named-type (::reap/named-type definition)
                 ::g/selection-set (process-selection-set (::reap/selection-set definition))}
              directives (assoc ::g/directives directives)))))))

    {::document/operations-by-name {}
     ::document/fragments-by-name {}})))

(defn ->document
  "Parse the input string to a data structure representing a GraphQL document."
  [input]
  (-> input
      parser/parse-graphql
      parse-tree->document))
