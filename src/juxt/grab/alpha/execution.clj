; Copyright © 2020, JUXT LTD.

(ns juxt.grab.alpha.execution
  (:require
   [juxt.grab.alpha.document :as document]
   [juxt.grab.alpha.schema :as schema]
   [flatland.ordered.map :refer [ordered-map]]
   [clojure.string :as str]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(defn field-error [msg]
  (ex-info (str "Field error: " msg) {::field-error true}))

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "6.3.2"
    :juxt.grab.alpha.spec-ref/algorithm "DoesFragmentTypeApply"}
  does-fragment-type-apply?
  [object-type fragment-type]
  (case (::g/kind fragment-type)
    OBJECT (= object-type fragment-type)
    INTERFACE (throw (ex-info "TODO" {}))
    UNION (throw (ex-info "TODO" {}))
    (throw
     (ex-info
      "Unexpected fragment type kind"
      {:kind (::g/kind fragment-type)
       :object-type object-type
       :fragment-type fragment-type}))))

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "6.3.2"
    :juxt.grab.alpha.spec-ref/algorithm "CollectFields"}
  collect-fields
  [{:keys [object-type selection-set variable-values visited-fragments
           fragments-by-name lookup-type]
    ;; 1. If visitedFragments if not provided, initialize it to the empty
    ;; set.
    :or {visited-fragments #{}}}]

  (reduce
   (fn [grouped-fields selection]
     (case (::g/selection-type selection)
       ;; c. If selection is a Field:
       :field
       (let [response-key
             ;; i. Let responseKey be the response key of selection (the alias
             ;; if defined, otherwise the field name).

             ;; TODO: The response-key will be the alias, if it exists
             (or (::g/alias selection) (::g/name selection))]
         (update
          grouped-fields
          ;; ii. Let groupForResponseKey be the list in groupedFields for responseKey;
          response-key

          ;; Append selection to the groupForResponseKey.
          (-> conj
              ;; ii (cont). …if no such list exists, create it as an empty
              ;; list.
              (fnil []))
          selection))

       :fragment-spread
       ;; d. If selection is a FragmentSpread:
       ;; i. Let fragmentSpreadName be the name of selection.

       (let [fragment-spread-name (::g/fragment-name selection)]
         ;; ii. If fragmentSpreadName is in visitedFragments, continue with
         ;; the next selection in selectionSet.

         (if (contains? visited-fragments fragment-spread-name)
           grouped-fields

           (let [ ;; iii. Add fragmentSpreadName to visitedFragments.
                 visited-fragments (conj visited-fragments fragment-spread-name)

                 ;; iv. Let fragment be the Fragment in the current Document
                 ;; whose name is fragmentSpreadName.
                 fragment (get fragments-by-name fragment-spread-name)]

             ;; v. If no such fragment exists, continue with the next
             ;; selection in selectionSet.
             (if-not fragment
               grouped-fields

               ;; vi. Let fragmentType be the type condition on fragment.
               (let [fragment-type (lookup-type (::g/type-condition fragment))]

                 (assert fragment-type (pr-str fragment))

                 ;; vii. If DoesFragmentTypeApply(objectType, fragmentType) is
                 ;; false, ...
                 (if (does-fragment-type-apply? object-type fragment-type)
                   ;; ... continue with the next selection in selectionSet.
                   grouped-fields

                   ;; viii. Let fragmentSelectionSet be the top‐level selection
                   ;; set of fragment.
                   (let [fragment-selection-set (::g/selection-set fragment)
                         ;; ix. Let fragmentGroupedFieldSet be the result of calling
                         ;; CollectFields(objectType, fragmentSelectionSet,
                         ;; visitedFragments).
                         fragment-grouped-field-set
                         (collect-fields
                          {:object-type object-type
                           :selection-set fragment-selection-set
                           :variable-values variable-values
                           :visited-fragments visited-fragments
                           :fragments-by-name fragments-by-name
                           :lookup-type lookup-type})]

                     (reduce
                      (fn [grouped-fields [response-key fragment-group]]
                        (update grouped-fields response-key (fnil concat (list)) fragment-group))
                      grouped-fields
                      fragment-grouped-field-set))))))))

       :inline-fragment
       ;; e. If selection is an InlineFragment:
       ;; i. Let fragmentType be the type condition on selection.
       (let [fragment-type (lookup-type (::g/type-condition selection))]

         ;; ii. If fragmentType is not null and
         ;; DoesFragmentTypeApply(objectType, fragmentType) is false, ...
         (if (and
              (some? fragment-type)
              (false? (does-fragment-type-apply? object-type fragment-type)))

           ;; ... continue with the next selection in selectionSet.
           grouped-fields

           ;; iii. Let fragmentSelectionSet be the top‐level selection set of
           ;; selection.
           (let [fragment-selection-set (::g/selection-set selection)
                 ;; iv. Let fragmentGroupedFieldSet be the result of calling
                 ;; CollectFields(objectType, fragmentSelectionSet, variableValues,
                 ;; visitedFragments).
                 fragment-grouped-field-set
                 (collect-fields
                  {:object-type object-type
                   :selection-set fragment-selection-set
                   :variable-values variable-values
                   :visited-fragments visited-fragments
                   :fragments-by-name fragments-by-name
                   :lookup-type lookup-type})]

             (reduce
              (fn [grouped-fields [response-key fragment-group]]
                (update grouped-fields response-key (fnil concat (list)) fragment-group))
              grouped-fields
              fragment-grouped-field-set)
             )))))

   ;; 2. Initialize groupedFields to an empty ordered map of lists.
   (ordered-map)
   ;; 3. For each selection in selectionSet:
   selection-set))

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "6.4.1"
    :juxt.grab.alpha.spec-ref/algorithm "CoerceArgumentValues"}
  coerce-argument-values
  [{:keys [object-type field variable-values]}]

  (let [
        ;; 1. Let coercedValues be an empty unordered Map.
        coerced-values {}
        ;; 2. Let argumentValues be the argument values provided in field.
        argument-values (::g/arguments field)
        ;; 3. Let fieldName be the name of field.
        field-name (::g/name field)
        ;; 4. Let argumentDefinitions be the arguments defined by objectType
        ;; for the field named fieldName.
        argument-definitions (get-in object-type [::schema/fields-by-name field-name ::g/arguments-definition])]



    ;; 5. For each argumentDefinition in argumentDefinitions:
    (reduce
     (fn [acc argument-definition]

       (let [ ;; a. Let argumentName be the name of argumentDefinition.
             argument-name (::g/name argument-definition)
             ;; b. Let argumentType be the expected type of argumentDefinition.
             argument-type (::g/type argument-definition)
             ;; c. Let defaultValue be the default value for argumentDefinition.
             default-value (find argument-definition ::g/default-value)
             ;; d. Let hasValue be true if argumentValues provides
             ;; a value for the name argumentName.
             has-value (find argument-values argument-name)
             ;; e. Let argumentValue be the value provided in argumentValues for the name argumentName.
             argument-value (second has-value)
             ;; f. If argumentValue is a Variable: (TODO)
             ;; g. Otherwise, let value be argumentValue.
             value argument-value]

         (cond
           ;; h. If hasValue is not true and defaultValue exists (including null):
           (and (not has-value) default-value)
           ;;   i. Add an entry to coercedValues named argumentName
           ;;   with the value defaultValue.
           (conj acc [argument-name (second default-value)])

           ;; i. Otherwise if argumentType is a Non‐Nullable type,
           ;; and either hasValue is not true or value is null,
           ;; throw a field error.
           (and (= (get argument-type "kind") 'NON_NULL)
                (or (not has-value)
                    (nil? (second has-value))))
           (throw (field-error "Field error, argument type is wrapped as non-null, but no argument value given"))

           ;; j. Otherwise if hasValue is true:
           has-value
           (cond
             ;; i. If value is null:
             (nil? argument-value)
             ;; 1. Add an entry to coercedValues named argumentName with the value null.
             (conj acc [argument-name nil])

             ;; ii. Otherwise, if argumentValue is a Variable:
             (and (map? argument-value) (::g/variable argument-value))
             (conj acc [argument-name (get variable-values (::g/variable argument-value))])

             :else
             ;; TODO: apply coercion rules, for now just set it to the value
             (let [coerced-value value]
               ;;(throw (ex-info "here" {argument-name value}))
               (conj acc [argument-name value])))

           :else acc)))

     coerced-values
     argument-definitions)))

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "6.4.3"
    :juxt.grab.alpha.spec-ref/algorithm "ResolveAbstractType"}
  resolve-abstract-type
  [{:keys [abstract-type object-value abstract-type-resolver]}]
  (assert abstract-type)
  (assert object-value)
  (assert abstract-type-resolver)
  (abstract-type-resolver
   {:abstract-type abstract-type :object-value object-value}))

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "6.4.3"
    :juxt.grab.alpha.spec-ref/algorithm "MergeSelectionSets"}
  merge-selection-sets
  [{:keys [fields]}]
  (reduce
   (fn [selection-set field]
     (let [field-selection-set (::g/selection-set field)]
       (cond-> selection-set
         field-selection-set (concat field-selection-set))))
   (list)
   fields))

(defn introspection-field-resolver
  [delegate schema {:keys [object-type field-name object-value argument-values path] :as args}]

  (let [root-query-name (get-in schema [::schema/root-operation-type-names :query])
        types-by-name (::schema/types-by-name schema)]
    (condp = [(::g/name object-type) field-name]

      [root-query-name "__type"]
      (get-in schema [::schema/types-by-name (get argument-values "name")])

      [root-query-name "__schema"]
      schema

      ["__Schema" "types"]
      (sort-by ::g/name (vals (::schema/types-by-name schema)))

      ["__Schema" "queryType"]
      (some-> root-query-name types-by-name)

      ["__Schema" "mutationType"]
      (when-let [type-name (get-in schema [::schema/root-operation-type-names :mutation])]
        (some-> type-name types-by-name))

      ["__Schema" "subscriptionType"]
      (when-let [type-name (get-in schema [::schema/root-operation-type-names :subscription])]
        (some-> type-name types-by-name))

      ["__Schema" "directives"]
      []

      ["__Type" "kind"]
      (or
       (some-> object-value ::g/kind)
       (throw
        (ex-info
         "Type kind is nil!"
         {:object-value object-value
          :path path})))

      ["__Type" "name"]
      (some-> object-value ::g/name)

      ["__Type" "fields"]
      (some-> object-value ::g/field-definitions)

      ["__Type" "ofType"]
      (when-let [type-ref (::of-type-ref object-value)]
        (cond
          (::g/list-type type-ref) {::g/kind 'LIST
                                    ::of-type-ref (::g/list-type type-ref)}
          (::g/non-null-type type-ref) {::g/kind 'NON_NULL
                                        ::of-type-ref (::g/non-null-type type-ref)}
          :else
          (let [typ (some-> type-ref ::g/name types-by-name)]
            (when (nil? typ)
              (throw (ex-info "ofType is nil" {:object-value object-value}))
              )

            {::g/kind (::g/kind typ)
             ::g/name (::g/name typ)})))

      ["__Field" "name"]
      (some-> object-value ::g/name)

      ["__Field" "type"]
      (let [type-ref (some-> object-value ::g/type-ref)]
        (cond
          (::g/list-type type-ref) {::g/kind 'LIST
                                    ::of-type-ref (::g/list-type type-ref)}
          (::g/non-null-type type-ref) {::g/kind 'NON_NULL
                                        ::of-type-ref (::g/non-null-type type-ref)}
          :else
          (let [typ (some-> type-ref ::g/name types-by-name)]
            (assert typ (format "Failed to find field type: %s" (some-> type-ref ::g/name)))
            {::g/name (::g/name typ)
             ::g/kind (::g/kind typ)})))

      ["__Type" "description"] (some-> object-value ::g/description)
      ["__Type" "interfaces"] []    ;; TODO
      ["__Type" "inputFields"] []   ;; TODO
      ["__Type" "enumValues"] (or (some-> object-value ::g/enum-values) [])

      ["__Type" "possibleTypes"] [] ;; TODO

      ["__Field" "description"] (some-> object-value ::g/description)
      ["__Field" "args"] (mapv
                          (fn [arg-def]
                            {::g/name (::g/name arg-def)
                             ::g/description (::g/description arg-def)
                             ::g/type-ref (::g/type-ref arg-def)
                             ::g/default-value (::g/default-value arg-def)
                             ;; Add description, type defaultValue
                             })

                          (some-> object-value ::g/arguments-definition))
      ["__Field" "isDeprecated"] false    ;; TODO
      ["__Field" "deprecationReason"] nil ;; TODO

      ["__EnumValue" "name"] (some-> object-value ::g/name)
      ["__EnumValue" "description"] (some-> object-value ::g/description)
      ["__EnumValue" "isDeprecated"] false
      ["__EnumValue" "deprecationReason"] ""

      ["__InputValue" "name"] (some-> object-value ::g/name)

      ["__InputValue" "description"] (some-> object-value ::g/description)

      ["__InputValue" "type"]
      (let [type-ref (some-> object-value ::g/type-ref)]
        (cond
          (::g/list-type type-ref) {::g/kind 'LIST
                                    ::of-type-ref (::g/list-type type-ref)}
          (::g/non-null-type type-ref) {::g/kind 'NON_NULL
                                        ::of-type-ref (::g/non-null-type type-ref)}
          :else
          (let [typ (some-> type-ref ::g/name types-by-name)]
            (assert typ (format "Failed to find field type: %s" (some-> type-ref ::g/name)))
            {::g/name (::g/name typ)
             ::g/kind (::g/kind typ)})))

      ["__InputValue" "defaultValue"] (some-> object-value ::g/default-value)


      ;; Forward to resolver
      (if (some-> object-type ::g/name (str/starts-with? "__"))
        (throw
         (ex-info
          "Unhandled introspection"
          {:object-value object-value
           :object-type object-type
           :field-name field-name}))
        (delegate args)))))

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "6.4.2"
    :juxt.grab.alpha.spec-ref/algorithm "ResolveFieldValue"}
  resolve-field-value
  [{:keys [object-type object-value field-name argument-values field-resolver schema path] :as args}]
  (assert field-name)
  (assert field-resolver)

  (introspection-field-resolver
   field-resolver
   schema
   {:object-type object-type
    :field-name field-name
    :object-value object-value
    :argument-values argument-values
    :path path}))

(declare execute-selection-set)

(defn coerce-result [result field-type]
  (case (::g/kind field-type)
    SCALAR
    (if (coll? result)
      (throw (field-error (format "A collection (%s) is not coerceable to a %s scalar" (type result) (::g/name field-type))))
      (if (nil? result) nil
          (case (::g/name field-type)
            "String" (cond
                       (keyword? result) (str/upper-case (name result))
                       :else (str result))
            "ID" (cond
                   (keyword? result) (subs (str result) 1)
                   :else (str result))
            "Int" (cond
                    (integer? result) result
                    (string? result)
                    (try
                      (Integer/parseInt result)
                      (catch NumberFormatException e
                        (throw (field-error "String cannot be coerced into an Int"))))
                    :else (throw (field-error "No coercion to Int")))

            result)))
    ENUM
    (cond
      (coll? result)
      (throw (field-error (format "A collection (%s) is not coerceable to a enum" (type result))))
      (string? result) result
      :else result)

    (throw (ex-info "Cannot coerce result" {:kind (::g/kind field-type)}))))

(defn ^{:juxt.grab.alpha.spec-ref/version "June2018"
        :juxt.grab.alpha.spec-ref/section "6.4.3"
        :juxt.grab.alpha.spec-ref/algorithm "CompleteValue"}
  complete-value
  "Return a map of :data and :errors"
  [{:keys [field-type-ref fields result variable-values field-resolver abstract-type-resolver
           schema fragments-by-name path] :as args}]

  (let [{::schema/keys [types-by-name]} schema
        field-type (some-> field-type-ref schema/unwrapped-type ::g/name types-by-name)
        kind (::g/kind field-type)]

    (cond
      ;; 1. If the fieldType is a Non‐Null type:
      (::g/non-null-type field-type-ref)
      ;; a. Let innerType be the inner type of fieldType.
      (let [inner-type-ref (get field-type-ref ::g/non-null-type)
            _ (assert inner-type-ref (format "Field type %s is NON_NULL but doesn't have a non-nil inner type" (pr-str field-type-ref)))

            ;; b. Let completedResult be the result of calling
            ;; CompleteValue(…).
            {:keys [data errors]}
            (complete-value
             {:field-type-ref inner-type-ref
              :fields fields
              :result result
              :variable-values variable-values
              :field-resolver field-resolver
              :abstract-type-resolver abstract-type-resolver
              :schema schema
              :fragments-by-name fragments-by-name
              :path path})]

        ;; c. If completedResult is null, throw a field error.
        (if (nil? data)
          {:errors errors
           ;; "If the parent field may be null then it resolves to null,
           ;; otherwise if it is a Non-Null type, the field error is further
           ;; propagated to it’s parent field."

           ;; Here we set a flag to indicate to the parent to decide whether to
           ;; resolve itself to null, or whether to propagate to it's parent
           ;; field.
           ::invalid? true}

          ;; d. Return completedResult.
          (cond-> {:data data}
            (seq errors) (assoc :errors errors))))

      ;; 2. If result is null (or another internal value similar to null such
      ;; as undefined or NaN), return null.
      (nil? result) {:data nil}

      ;; 3. If fieldType is a List type:
      (::g/list-type field-type-ref)
      (do
        ;; a. If result is not a collection of values, throw a field error.
        (when-not (sequential? result)
          (throw (field-error "Resolver must return a collection")))

        ;; b. Let innerType be the inner type of fieldType.
        (let [inner-type-ref (get field-type-ref ::g/list-type)

              ;; c. Return a list where each list item is the result of calling
              ;; CompleteValue(innerType, fields, resultItem, variableValues),
              ;; where resultItem is each item in result.

              result
              (reduce
               (fn [acc {:keys [data errors]}]
                 (cond-> (update acc :data conj data)
                   (seq errors) (update :errors concat errors)
                   (and (::g/non-null-type inner-type-ref) (nil? data))
                   (assoc :some-nil true)))
               {:data []}
               (map-indexed
                (fn [ix result-item]
                  (complete-value
                   {:field-type-ref inner-type-ref
                    :fields fields
                    :result result-item
                    :variable-values variable-values
                    :field-resolver field-resolver
                    :abstract-type-resolver abstract-type-resolver
                    :schema schema
                    :fragments-by-name fragments-by-name
                    :path (conj path ix)}))
                result))]
          (cond-> result
            ;; "If a List type wraps a Non-Null type, and one of the elements of
            ;; that list resolves to null, then the entire list must resolve to
            ;; null."
            (:some-nil result) (assoc :data nil))))

      ;; 4. If fieldType is a Scalar or Enum type:
      (#{'SCALAR 'ENUM} kind)
      ;; a. Return the result of “coercing” result, ensuring it is a legal value of fieldType, otherwise null.
      {:data (coerce-result result field-type)}

      ;; 5. If fieldType is an Object, Interface, or Union type:
      (#{'OBJECT 'INTERFACE 'UNION} kind)
      (let [object-type
            (if (= kind 'OBJECT)
              field-type
              (some->
               (resolve-abstract-type
                {:abstract-type field-type
                 :object-value result
                 :abstract-type-resolver abstract-type-resolver})
               types-by-name))
            sub-selection-set (merge-selection-sets {:fields fields})]

        (execute-selection-set
         {:selection-set sub-selection-set
          :object-type object-type
          :object-value result
          :variable-values variable-values
          :field-resolver field-resolver
          :abstract-type-resolver abstract-type-resolver
          :schema schema
          :fragments-by-name fragments-by-name
          :path path})))))

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "6.4"
    :juxt.grab.alpha.spec-ref/algorithm "ExecuteField"}
  execute-field
  "Return a map of :data and :errors"
  [{:keys [object-type object-value field-type-ref fields variable-values
           field-resolver abstract-type-resolver schema fragments-by-name path]}]
  (assert schema)
  (assert path)

  ;; 1. Let field be the first entry in fields.
  (let [field (first fields)

        ;; 2. Let fieldName be the field name of field.
        field-name (::g/name field)

        ;; 3. Let argumentValues be the result of CoerceArgumentValues(…).
        argument-values
        (coerce-argument-values
         {:object-type object-type
          :field field
          :variable-values variable-values})]

    (try
      (let [ ;; 4. Let resolvedValue be ResolveFieldValue(…).
            resolved-value
            (resolve-field-value
             {:object-type object-type
              :object-value object-value
              :field-name field-name
              :argument-values argument-values
              :field-resolver field-resolver
              :schema schema
              :path path})]

        ;; 5. Return the result of CompleteValue(…).
        (complete-value
         {:field-type-ref field-type-ref
          :fields fields
          :result resolved-value
          :variable-values variable-values
          :field-resolver field-resolver
          :abstract-type-resolver abstract-type-resolver
          :schema schema
          :fragments-by-name fragments-by-name
          :path path}))

      (catch Exception e
        ;; Error resolving field value. If an error occurs we can set the field
        ;; to nil, marking whether this makes the field invalid with respect to
        ;; any non-nil wrapper.
        (let [ex-data (ex-data e)]
          (-> (if (::g/non-null-type field-type-ref)
                {::invalid? true}
                {:data nil})
              (assoc :errors [(cond-> {:message (.getMessage e) :path path}
                                (seq ex-data) (assoc :extensions ex-data))])))))))

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "6.3"
    :juxt.grab.alpha.spec-ref/algorithm "ExecuteSelectionSet"}
  execute-selection-set
  "Return a map with :data and :errors."
  [{:keys [selection-set object-type object-value variable-values
           field-resolver abstract-type-resolver schema fragments-by-name path]}]

  (assert schema)
  (assert path)

  ;; 1. Let groupedFieldSet be the result of CollectFields
  (let [grouped-field-set
        (collect-fields
         {:object-type object-type
          :selection-set selection-set
          :variable-values variable-values
          :fragments-by-name fragments-by-name
          :lookup-type (::schema/types-by-name schema)})

        result
        (reduce
         (fn [acc [response-key fields]]

           ;; a. Let fieldName be the name of the first entry in fields. Note:
           ;; This value is unaffected if an alias is used.
           (let [field (first fields)
                 field-name (::g/name field)
                 ;; b. Let fieldType be the return type defined for the field fieldName of objectType.
                 field-type-ref
                 (get-in object-type [::schema/fields-by-name field-name ::g/type-ref])]

             ;; c. If fieldType is defined:
             (if field-type-ref
               ;; i. Let responseValue be ExecuteField(objectType, objectValue,
               ;; fields, fieldType, variableValues).
               (let [{:keys [data errors] ::keys [invalid?] :as field-result}
                     (execute-field
                      {:object-type object-type
                       :object-value object-value
                       :field-type-ref field-type-ref
                       :fields fields
                       :variable-values variable-values
                       :field-resolver field-resolver
                       :abstract-type-resolver abstract-type-resolver
                       :schema schema
                       :fragments-by-name fragments-by-name
                       ;; "If the error happens in an aliased field, the path to
                       ;; the error should use the aliased name, since it
                       ;; represents a path in the response, not in the query."
                       ;; -- GraphQL Spec. June 2018, 7.1.2
                       :path (conj path (keyword response-key))})]
                 ;; ii. Set responseValue as the value for responseKey in resultMap.
                 (cond-> acc
                   (find field-result :data) (update :data conj [(keyword response-key) data])
                   (seq errors) (update :errors concat errors)
                   ;; Indicate in the accumulator that we have an invalid nil field.
                   invalid? (assoc ::invalid? true)))
               ;; Otherwise return the accumulator
               acc)))
         ;; 2. Initialize resultMap to an empty ordered map.
         {:data (ordered-map)}
         grouped-field-set)]

    (cond-> result
      ;; If any of the fields are invalid, this invalidates the field's
      ;; parent's selection set. We nil it and clear the flag, letting its
      ;; parent to set the flag based on whether this is acceptable or to
      ;; proppagate in turn to it's parent.
      (::invalid? result)
      (-> (assoc :data nil)
          (dissoc ::invalid?)))))

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "6.2.1"
    :juxt.grab.alpha.spec-ref/algorithm "ExecuteQuery"}
  execute-query
  "Returns a map with :errors and :data"
  [{:keys [query schema variable-values initial-value
           field-resolver abstract-type-resolver fragments-by-name]}]
  (assert schema)

  ;; 1. Let queryType be the root Query type in schema.
  (let [query-type-name (get-in schema [::schema/root-operation-type-names :query])
        query-type (get-in schema [::schema/types-by-name query-type-name])]

    ;; 2. Assert: queryType is an Object type.
    (when-not (= (get query-type ::g/kind) 'OBJECT)
      (throw (ex-info
              "Query type must be an OBJECT"
              (into
               {:query-type query-type
                :schema schema
                :juxt.grab.alpha.spec-ref/step 2}
               (meta #'execute-query)))))

    (assert (::g/selection-set query))

    (let [ ;; 3. Let selectionSet be the top level Selection Set in query.
          selection-set (::g/selection-set query)]

      ;; 4. Let data be the result of running ExecuteSelectionSet
      ;; normally (allowing parallelization).
      ;; 5. Let errors be any field errors produced while executing the selection set.
      ;; 6. Return an unordered map containing data and errors.
      (execute-selection-set
       {:selection-set selection-set
        :object-type query-type
        :object-value initial-value
        :variable-values variable-values
        :schema schema
        :field-resolver field-resolver
        :abstract-type-resolver abstract-type-resolver
        :fragments-by-name fragments-by-name
        :path []}))))

(defn execute-mutation
  [{:keys [mutation schema variable-values initial-value field-resolver fragments-by-name]}]

  (let [mutation-type-name (get-in schema [::schema/root-operation-type-names :mutation])
        mutation-type (get-in schema [::schema/types-by-name mutation-type-name])]

    ;; 2. Assert: mutationType is an Object type.
    (when-not (= (get mutation-type ::g/kind) 'OBJECT)
      (throw (ex-info
              "Query type must be an OBJECT"
              (into
               {:mutation-type mutation-type
                :schema schema
                :juxt.grab.alpha.spec-ref/step 2}
               (meta #'execute-mutation)))))

    (assert (::g/selection-set mutation))

    (let [ ;; 3. Let selectionSet be the top level Selection Set in mutation.
          selection-set (::g/selection-set mutation)]

      ;; 4. Let data be the result of running ExecuteSelectionSet
      ;; normally serially.
      ;; 5. Let errors be any field errors produced while executing the selection set.
      ;; 6. Return an unordered map containing data and errors.
      (execute-selection-set
       {:selection-set selection-set
        :object-type mutation-type
        :object-value initial-value
        :variable-values variable-values
        :schema schema
        :field-resolver field-resolver
        :fragments-by-name fragments-by-name
        :path []}))))

(defn execute-subscription [_]
  (throw (ex-info "Subscriptions are not currently supported" {})))

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "6.1"
    :juxt.grab.alpha.spec-ref/algorithm "ExecuteRequest"}
  execute-request
  "Returns a map with :errors and :data"
  [{:keys [schema document operation-name variable-values
           initial-value field-resolver abstract-type-resolver]}]

  ;; 1. Let operation be the result of GetOperation(document, operationName).
  (let [operation (document/get-operation document operation-name)
        ;; 2. Let coercedVariableValues be the result of
        ;; CoerceVariableValues(schema, operation, variableValues). (TODO)
        coerced-variable-values variable-values
        fragments-by-name (::document/fragments-by-name document)]

    (case (::g/operation-type operation)
      ;; 3. If operation is a
      :query ;; operation:
      ;;   a. Return ExecuteQuery(operation, schema, coercedVariableValues,
      ;;   initialValue).
      (execute-query
       {:query operation
        :schema schema
        :variable-values coerced-variable-values
        :initial-value initial-value
        :field-resolver field-resolver
        :abstract-type-resolver abstract-type-resolver
        :fragments-by-name fragments-by-name})

      ;; 4. Otherwise if operation is a mutation operation:
      ;;   a. Return ExecuteMutation(operation, schema, coercedVariableValues, initialValue).
      :mutation
      (execute-mutation
       {:mutation operation
        :schema schema
        :variable-values coerced-variable-values
        :initial-value initial-value
        :field-resolver field-resolver
        :fragments-by-name fragments-by-name})

      ;; 5. Otherwise if operation is a subscription operation:
      ;;   a. Return Subscribe(operation, schema, coercedVariableValues, initialValue).
      :subscription
      (execute-subscription {})

      (throw (ex-info "Unsupported operation type on operation" {:operation operation})))))
