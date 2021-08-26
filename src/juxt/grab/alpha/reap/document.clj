;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.reap.document
  (:require
   [juxt.grab.alpha.reap.parser :as parser]
   [juxt.reap.alpha.graphql :as reap]))

(alias 'document (create-ns 'juxt.grab.alpha.document))
(alias 'schema (create-ns 'juxt.grab.alpha.schema))
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

(defn some-match [coll k v]
  (some #(when (= (get % k) v) %) coll))

(def type-kind-map
  {"ScalarTypeDefinition" :scalar
   "ObjectTypeDefinition" :object
   "InterfaceTypeDefinition" :interface
   "UnionTypeDefinition" :union
   "EnumTypeDefinition" :enum
   "InputObjectTypeDefinition" :input-object})

(defn add-kind [typ]
  (if (map? typ)
    (case (::reap/type typ)
      :list (assoc typ
                   ::g/kind :list
                   ::g/item-type (add-kind (::reap/item-type typ)))
      :non-null (assoc typ
                       ::g/kind :non-null
                       ::g/inner-type (add-kind (::reap/inner-type typ)))
      typ)
    typ))

(defn parse-tree->field
  [parse-tree]
  (let [typ (::reap/type parse-tree)]
    (merge
     parse-tree ;; preserve the reap entries, useful when reasoning.
     {::g/name (::reap/name parse-tree)
      ::g/type (add-kind typ)}
     (when-let [args (::reap/arguments-definition parse-tree)]
       {::g/arguments-definition (for [arg args]
                                   (into
                                    arg
                                    {::g/name (::reap/name arg)
                                     ::g/type (::reap/type arg)}))}))))

(defn parse-tree->schema
  "Return a grab-specified schema as a map from a reap-parsed document."
  [parse-tree]
  (let [provided-types
        (->> parse-tree
             (keep
              (fn [typ]
                ;; See https://spec.graphql.org/June2018/#sec-The-__Type-Type
                (when-let [kind (type-kind-map (::reap/type typ))]
                  [(::reap/name typ)
                   (assoc typ
                          ::g/kind kind
                          ::g/name (::reap/name typ)
                          ::g/field-definitions
                          (->>
                           (for [field (::reap/field-definitions typ)]
                             [(::reap/name field)
                              ;; See https://spec.graphql.org/June2018/#sec-The-__Field-Type
                              (parse-tree->field field)])
                           (into {})))])))
             (into {"Int" {::g/name "Int"
                           ::g/kind :scalar}
                    "Float" {::g/name "Float"
                             ::g/kind :scalar}
                    "String" {::g/name "String"
                              ::g/kind :scalar}
                    "Boolean" {::g/name "Boolean"
                               ::g/kind :scalar}
                    "ID" {::g/name "ID"
                          ::g/kind :scalar}}))
        root-query-type-name
        (some-> (some-match parse-tree ::reap/type "SchemaDefinition")
                ::reap/root-operation-types
                (some-match ::reap/operation-type "query")
                ::reap/named-type)]

    {::schema/provided-types provided-types
     ::schema/root-operation-type-names {:query root-query-type-name}}))

(defn ->schema
  "Parse the input string to a data structure representing a GraphQL schema."
  [s]
  (-> s
      parser/parse-graphql
      parse-tree->schema))
