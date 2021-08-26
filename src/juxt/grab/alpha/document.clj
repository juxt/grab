;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document
  (:refer-clojure :exclude [compile])
  (:require [juxt.grab.alpha.schema :as schema]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))
(alias 'schema (create-ns 'juxt.grab.alpha.schema))

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
            {:error "A document containing a type system definition or extension is invalid for execution"})))

(defn add-operations [{::keys [document] :as acc}]
  (assoc acc ::operations (vec (keep ::g/operation-definition document))))

(defn add-fragments [{::keys [document] :as acc}]
  (assoc acc ::fragments (vec (keep ::g/fragment-definition document))))

(defn group-operations-by-name [{::keys [operations] :as acc}]
  (assoc acc ::operations-grouped-by-name (group-by ::g/name operations)))

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

(defn add-default-operation-type [acc]
  (update acc ::operations #(mapv (fn [op]
                                    (cond-> op
                                      (not (find op ::g/operation-type))
                                      (assoc ::g/operation-type :query)))
                                  %)))

(defn scope-selection-set
  [selection-set scoped-type {::schema/keys [provided-types] :as schema}]
  (->>
   selection-set
   (mapv
    (fn [{field-name ::g/name
          ::g/keys [selection-type selection-set type-condition]
          :as selection}]
      (case selection-type
        :field
        (let [return-type (some-> scoped-type provided-types
                                  ::g/field-definitions (get field-name) ::g/type)]
          (-> selection
              (assoc ::scoped-type scoped-type)
              (assoc ::return-type return-type)
              (cond-> selection-set
                (update ::g/selection-set scope-selection-set return-type schema))))

        :fragment-spread
        ;; We don't need to do anything because scoped-types are added to
        ;; fragment definitions separately -- just return the selection
        ;; unmodified.
        selection

        :inline-fragment
        (-> selection
            (assoc ::scoped-type type-condition)
            (update ::g/selection-set scope-selection-set type-condition schema))

        (throw
         (ex-info
          "TODO: unhandled selection-type"
          {:selection-type selection-type
           :selection-set selection-set
           :scoped-type scoped-type
           :schema? (some? scoped-type)})))))))

(defn add-scoped-types-to-operations [{::keys [schema] :as acc}]
  (update
   acc ::operations
   (fn [ops]
     (->>
      ops
      (mapv
       (fn [op]
         (let [op-type (::g/operation-type op)
               scoped-type (get-in acc [::schema ::schema/root-operation-type-names op-type])]
           (-> op
               (assoc ::scoped-type scoped-type)
               (update
                ::g/selection-set
                (fn [selection-set]
                  (scope-selection-set selection-set scoped-type schema)))))))))))

(defn add-scoped-types-to-fragments [{::keys [schema] :as acc}]
  (update
   acc ::fragments
   (fn [fragments]
     (->>
      fragments
      (mapv
       (fn [fragment]
         (let [scoped-type (::g/type-condition fragment)]
           (-> fragment
               (assoc ::scoped-type scoped-type)
               (update
                ::g/selection-set
                (fn [selection-set]
                  (scope-selection-set selection-set scoped-type schema)))))))))))

(defn validate-selection [{::g/keys [selection-type] :as selection}
                          ;; TODO: Do we still need to pass down parent-scoped-type?
                          parent-scoped-type
                          {::schema/keys [provided-types built-in-types] :as schema}
                          path]
  ;; TODO: selection-type is mentioned in a GraphQL alog, so perhaps used a
  ;; different keyword.
  (case selection-type
    :field
    (let [scoped-type (::scoped-type selection)
          field-name (::g/name selection)
          path (conj path (::g/name selection))
          field-def (some-> scoped-type provided-types ::g/field-definitions (get field-name))
          selection-type (or
                          (some-> field-def ::g/type provided-types)
                          (some-> field-def ::g/type built-in-types))
          subselection-set (::g/selection-set selection)]

      (cond
        (nil? field-def)
        [{:error (format
                  "Field name '%s' not defined on type in scope '%s'"
                  (::g/name selection)
                  parent-scoped-type)
          :selection selection
          :scoped-type parent-scoped-type
          :field-name (::g/name selection)
          :path path}]

        (and (#{:scalar :enum} (some-> selection-type ::g/kind)) subselection-set)
        [{:error "The subselection set of a scalar or enum must be empty"}]

        subselection-set
        (mapcat #(validate-selection % scoped-type schema path) subselection-set)

        :else []))

    :fragment-spread []                 ; Already covered

    :inline-fragment
    (let [path (conj path (::scoped-type selection))]
      (mapcat #(validate-selection % parent-scoped-type schema path)
              (::g/selection-set selection)))))

(defn validate-selection-sets [{::keys [schema] :as acc}]
  (update
   acc ::errors (comp vec concat)
   (concat
    (for [op (::operations acc)
          selection (::g/selection-set op)
          error (validate-selection selection (::scoped-type op) schema [(::g/name op)])
          :when error]
      error)
    (for [frag (::fragments acc)
          selection (::g/selection-set frag)
          error (validate-selection selection (::scoped-type frag) schema [(::g/fragment-name frag)])
          :when error]
      error))))

(defn visit-fields [selection schema]
  (case (::g/selection-type selection)
    :field [selection]

    :inline-fragment
    (mapcat visit-fields (::g/selection-set selection) (repeat schema))

    :fragment-spread
    ;; If we can't find the fragment, we don't error because this will be
    ;; spotted by another validator.
    (when-let [fragment (get-in schema [::fragments-by-name (::g/fragment-name selection)])]
      (mapcat visit-fields (::g/selection-set fragment) (repeat schema)))

    (throw (ex-info "Unexpected selection type" {:selection selection}))))

(defn fields-by-name [selection-set schema]
  (->>
   (mapcat visit-fields selection-set (repeat schema))
   (group-by (fn [field]
               (or (get field ::g/alias) (get field ::g/name))))))

(defn same-response-shape
  [response-name fields {::schema/keys [provided-types built-in-types] :as schema} path]
  ;; TODO: Non-null and lists
  ;;(throw (ex-info "Same response shape" {:fields fields}))
  (let [kinds (mapv #(or
                      (some-> % ::return-type provided-types ::g/kind)
                      (some-> % ::return-type built-in-types ::g/kind)) fields)]
    (cond
      (some #{:scalar :enum} kinds)
      (when (apply not= (map ::return-type fields))
        {:error "Fields have conflicting return types"
         :path path
         :response-name response-name
         :fields fields}))))

(defn fields-in-set-can-merge
  [selection-set {::schema/keys [provided-types built-in-types] :as schema}
   parent-scoped-type path]
  (let [ ;; "1. Let fieldsForName be the set of selections with a given response
        ;; name in set including visiting fragments and inline fragments."
        fields-for-name (fields-by-name selection-set schema)]
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
                     (map ::scoped-type)
                     (apply =))
                (->> fields
                     (map #(or
                            (some-> % ::scoped-type provided-types ::g/kind)
                            (some-> % ::scoped-type built-in-types ::g/kind)))
                     (some #(not= % :object))))
           (cond
             ;; "i. fieldA and fieldB must have identical field names."
             (not (apply = (map ::g/name fields)))
             {:error "Cannot merge since field names are not identical"
              :selection-set selection-set
              :parent-scoped-type parent-scoped-type
              :path path
              :response-name response-name
              :fields fields
              :field-scoped-types-equal? (apply = (map ::scoped-type fields))
              :field-kinds (map (comp ::g/kind ::scoped-type) fields)}

             ;; "ii. fieldA and fieldB must have identical sets of arguments."
             (not (apply = (map ::g/arguments fields)))
             {:error "Cannot merge since field arguments are not identical"
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

(defn validate-fields-in-set-can-merge [acc]
  (update
   acc ::errors (comp vec concat)
   (concat
    (for [op (::operations acc)
          :let [selection-set (::g/selection-set op)]
          error (fields-in-set-can-merge selection-set (::schema acc) (::scoped-type op) [(::g/name op)])
          :when error]
      error)
    (for [frag (::fragments acc)
          ;; TODO: Also visit fragments and inline fragments as per spec "including visiting
          ;; fragments and inline fragments."
          :let [selection-set (::g/selection-set frag)]
          error (fields-in-set-can-merge selection-set (::schema acc) (::scoped-type frag) [(::g/fragment-name frag)])
          :when error]
      error))))

(defn compile
  "Compile a document with respect to the given schema, returning a structure that
  can be provided to the execution functions."
  ([document schema]
   (compile document schema
            [validate-executable-definitions
             add-operations
             add-default-operation-type
             add-fragments

             add-scoped-types-to-operations
             add-scoped-types-to-fragments
             validate-selection-sets

             group-operations-by-name
             validate-anonymous
             validate-operation-uniqueness
             validate-fields-in-set-can-merge
             ]))

  ([document schema compilers]

   (reduce
    (fn [acc f]
      ;; Allow compilers to return nil (e.g. applicability guards)
      (or (f acc) acc))

    {::errors []
     ::document document
     ::schema schema}

    compilers)))
