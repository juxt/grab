;; Copyright © 2022, JUXT LTD.

(ns juxt.grab.alpha.document-old
  (:require
   [juxt.grab.alpha.schema :as schema]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

#_(defn entry? [e] (and (vector? e) (= (count e) 2)))

#_(defn add-error [acc error]
  (update acc ::errors conj error))

#_(defn ^{:juxt/see "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  validate-executable-definitions [{::keys [document] :as acc}]
  (when-not (every? #(#{:executable-definition} (::g/definition-type %)) document)
    (add-error
     acc
     {:message "A document containing a type system definition or extension is invalid for execution"})))

#_(defn add-operations [{::keys [document] :as acc}]
  (assoc acc ::operations (vec (keep ::g/operation-definition document))))

#_(defn add-fragments [{::keys [document] :as acc}]
  (assoc acc ::fragments (vec (keep ::g/fragment-definition document))))

#_(defn group-operations-by-name [{::keys [operations] :as acc}]
  (assoc acc ::operations-grouped-by-name (group-by ::g/name operations)))

#_(defn group-fragments-by-name [{::keys [fragments] :as acc}]
  (assoc acc ::fragments-grouped-by-name (group-by ::g/name fragments)))

#_(defn ^{:juxt/see "https://spec.graphql.org/June2018/#sec-Lone-Anonymous-Operation"}
  validate-anonymous
  [{::keys [operations operations-grouped-by-name] :as acc}]
  (assert operations)
  (when (> (count operations) 1)
    (when-not (empty? (get operations-grouped-by-name nil))
      (add-error acc {:message "When there are multiple operations in the document, none can be anonymous"}))))

#_(defn validate-operation-uniqueness [{::keys [operations-grouped-by-name] :as acc}]
  (reduce-kv
   (fn [acc n ops]
     (if (> (count ops) 1)
       (add-error acc {:message (format "Operation name '%s' is not unique" n) :name n})
       (assoc-in acc [::operations-by-name n] (first ops))))
   acc
   operations-grouped-by-name))

#_(defn validate-fragment-uniqueness [{::keys [fragments-grouped-by-name] :as acc}]
  (reduce-kv
   (fn [acc n ops]
     (if (> (count ops) 1)
       (add-error acc {:message (format "Fragment name '%s' is not unique" n) :name n})
       (assoc-in acc [::fragments-by-name n] (first ops))))
   acc
   fragments-grouped-by-name))

#_(defn add-default-operation-type [acc]
  (update acc ::operations #(mapv (fn [op]
                                    (cond-> op
                                      (not (find op ::g/operation-type))
                                      (assoc ::g/operation-type :query)))
                                  %)))

#_(defn scope-selection-set
  [selection-set scoped-type-name {::schema/keys [types-by-name] :as schema}]
  (->>
   selection-set
   (mapv
    (fn [{field-name ::g/name
          ::g/keys [selection-type selection-set type-condition]
          :as selection}]
      (case selection-type
        :field
        (let [return-type (some-> scoped-type-name types-by-name
                                  ::schema/fields-by-name (get field-name) ::g/type-ref)]
          (as-> selection %
            (assoc % ::scoped-type-name scoped-type-name)
            (assoc % ::return-type return-type)
            #_(assoc % ::argument-definitions-by-name
                   (reduce-kv
                    (fn [acc arg-name _]
                      (assoc acc arg-name
                             (get-in
                              schema
                              [:juxt.grab.alpha.schema/types-by-name
                               scoped-type-name
                               :juxt.grab.alpha.schema/fields-by-name
                               field-name
                               :juxt.grab.alpha.graphql/argument-definitions-by-name
                               arg-name])))
                    {} (:juxt.grab.alpha.graphql/arguments selection)))
            (cond-> %
              selection-set
              (update ::g/selection-set
                      scope-selection-set (some-> return-type schema/unwrapped-type ::g/name) schema))))

        :fragment-spread
        ;; We don't need to do anything because scoped-types are added to
        ;; fragment definitions separately -- just return the selection
        ;; unmodified.
        selection

        :inline-fragment
        (-> selection
            (assoc ::scoped-type-name type-condition)
            (update ::g/selection-set scope-selection-set type-condition schema)
            #_(assoc ::argument-definitions-by-name
                   (reduce-kv
                    (fn [acc arg-name _]
                      (assoc acc arg-name
                             (get-in
                              schema
                              [:juxt.grab.alpha.schema/types-by-name
                               type-condition
                               :juxt.grab.alpha.schema/fields-by-name
                               field-name
                               :juxt.grab.alpha.graphql/argument-definitions-by-name
                               arg-name])))
                    {} (:juxt.grab.alpha.graphql/arguments selection))))

        (throw
         (ex-info
          "TODO: unhandled selection-type"
          {:selection-type selection-type
           :selection-set selection-set
           :scoped-type-name scoped-type-name
           :schema? (some? scoped-type-name)})))))))

#_(defn add-scoped-types-to-operations [{::keys [schema] :as acc}]
  (update
   acc ::operations
   (fn [ops]
     (->>
      ops
      (mapv
       (fn [op]
         (let [op-type (::g/operation-type op)
               scoped-type-name (get-in acc [::schema ::schema/root-operation-type-names op-type])]
           (-> op
               (assoc ::scoped-type-name scoped-type-name)
               (update
                ::g/selection-set
                (fn [selection-set]
                  (scope-selection-set selection-set scoped-type-name schema)))))))))))

#_(defn add-scoped-types-to-fragments [{::keys [schema] :as acc}]
  (update
   acc ::fragments
   (fn [fragments]
     (->>
      fragments
      (mapv
       (fn [fragment]
         (let [scoped-type-name (::g/type-condition fragment)]
           (-> fragment
               (assoc ::scoped-type-name scoped-type-name)
               (update
                ::g/selection-set
                (fn [selection-set]
                  (scope-selection-set selection-set scoped-type-name schema)))))))))))

#_(defn validate-selection [{::g/keys [selection-type] :as selection}
                          ;; TODO: Do we still need to pass down parent-scoped-type?
                          parent-scoped-type
                          {::schema/keys [types-by-name] :as schema}
                          path]
  (case selection-type
    :field
    (let [scoped-type-name (::scoped-type-name selection)
          field-name (::g/name selection)
          path (conj path field-name)
          field-def (some-> scoped-type-name types-by-name ::schema/fields-by-name (get field-name))
          selection-type (some-> field-def ::g/type-ref schema/unwrapped-type ::g/name types-by-name)
          subselection-set (::g/selection-set selection)]

      (cond
        (= field-name "__typename") [] ; allow for introspection

        (nil? field-def)
        [{:message (format
                  "Field name '%s' not defined on type in scope '%s'"
                  (::g/name selection)
                  scoped-type-name)
          :selection selection
          :parent-scoped-type parent-scoped-type
          :field-name (::g/name selection)
          :path path
          :scoped-type-name scoped-type-name
          :type-ref (some-> scoped-type-name types-by-name)}]

        (and (#{'SCALAR 'ENUM} (some-> selection-type ::g/kind)) subselection-set)
        [{:message "The subselection set of a scalar or enum must be empty"}]

        subselection-set
        (mapcat #(validate-selection % scoped-type-name schema path) subselection-set)

        :else []))

    :fragment-spread []                 ; Already covered

    :inline-fragment
    (let [path (conj path (::scoped-type-name selection))]
      (mapcat #(validate-selection % parent-scoped-type schema path)
              (::g/selection-set selection)))))

#_(defn validate-selection-sets [{::keys [schema] :as acc}]
  (update
   acc ::errors into
   (for [op-or-frag (concat (::operations acc) (::fragments acc))
         selection (::g/selection-set op-or-frag)
         error (validate-selection selection (::scoped-type-name op-or-frag) schema [(::g/name op-or-frag)])
         :when error]
     error)))

#_(defn ^{:juxt.grab.alpha.spec-ref/version "June2018"
        :juxt.grab.alpha.spec-ref/section "5.3.2"
        :juxt.grab.alpha.spec-ref/algorithm "SameResponseShape"
        :deprecated true}
  same-response-shape
  [response-name fields {::schema/keys [types-by-name]} path]
  ;; TODO: Non-null and lists
  ;;(throw (ex-info "Same response shape" {:fields fields}))
  (let [kinds (mapv #(some-> % ::return-type schema/unwrapped-type ::g/name types-by-name ::g/kind) fields)]
    (cond
      (some #{'SCALAR 'ENUM} kinds)
      (when (apply not= (map ::return-type fields))
        {:message "Fields have conflicting return types"
         :path path
         :response-name response-name
         :fields fields}))))

#_(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "5.3.2"
    :juxt.grab.alpha.spec-ref/algorithm "FieldsInSetCanMerge"}
  fields-in-set-can-merge
  [selection-set {::schema/keys [types-by-name] :as schema}
   parent-scoped-type path]
  (let [ ;; "1. Let fieldsForName be the set of selections with a given response
        ;; name in set including visiting fragments and inline fragments."
        fields-for-name (collect-fields-by-name selection-set schema)]
    (->>
     fields-for-name
     (mapcat
      (fn [[response-name fields]]
        ;; "2. Given each pair of members fieldA and fieldB in fieldsForName:"

        [
         ;; "a. SameResponseShape(fieldA, fieldB) must be true." (TODO)
         (same-response-shape response-name fields schema path)

         ;; "b. If the parent types of fieldA and fieldB are equal or if
         ;; either is not an Object Type:"
         (when (or
                (->> fields
                     (map ::scoped-type-name)
                     (apply =))
                (->> fields
                     (map #(some-> % ::scoped-type-name types-by-name ::g/kind))
                     (some #(not= % 'OBJECT))))
           (cond
             ;; "i. fieldA and fieldB must have identical field names."
             (not (apply = (map ::g/name fields)))
             {:message "Cannot merge since field names are not identical"
              :selection-set selection-set
              :parent-scoped-type parent-scoped-type
              :path path
              :response-name response-name
              :fields fields
              :field-scoped-types-equal? (apply = (map ::scoped-type-name fields))
              :field-kinds (map (comp ::g/kind ::scoped-type-name) fields)}

             ;; "ii. fieldA and fieldB must have identical sets of arguments."
             (not (apply = (map ::g/arguments fields)))
             {:message "Cannot merge since field arguments are not identical"
              :selection-set selection-set
              :parent-scoped-type parent-scoped-type
              :path path
              :response-name response-name
              :fields fields}

             ;; TODO: "Let mergedSet be the result of adding the selection set of
             ;; fieldA and the selection set of
             ;; fieldB. FieldsInSetCanMerge(mergedSet) must be true."
             ))]))
     (filter some?)
     )))

#_(defn validate-fields-in-set-can-merge [acc]
  (update
   acc ::errors into
   (for [op (concat (::operations acc)
                    (::fragments acc)
                    ;; TODO: Also visit fragments and inline fragments as per spec "including visiting
                    ;; fragments and inline fragments."
                    )
         :let [selection-set (::g/selection-set op)]
         error (fields-in-set-can-merge selection-set (::schema acc) (::scoped-type-name op) [(::g/name op)])
         :when error]
     error)))

#_(defn validate-arguments [{::keys [document] :as acc}]
  (update
   acc ::errors into
   (->>
    document
    ;; Tree seq losses context unless we have a way we can restore it with children function
    (tree-seq (every-pred seqable? (comp not string?)) seq)
    (mapcat
     (fn [m]
       (when (and (map? m) (contains? m ::g/arguments))
         (reduce-kv (fn [acc arg-name _]
                      (let [arg-def (get-in m [::argument-definitions-by-name arg-name])]
                        (when (nil? arg-def)
                          (conj acc {:message "Argument not allowed here" :arg-name arg-name}
                                ))))
                    [] (::g/arguments m)))))))
  ;; TODO: directives
  )





      #_add-operations
      #_add-default-operation-type
      #_add-fragments
      #_add-scoped-types-to-operations
      #_add-scoped-types-to-fragments

      #_group-operations-by-name
      #_group-fragments-by-name

      #_validate-executable-definitions
      #_validate-selection-sets
      #_validate-anonymous
      #_validate-operation-uniqueness
      #_validate-fragment-uniqueness
      #_validate-fields-in-set-can-merge
      #_validate-arguments