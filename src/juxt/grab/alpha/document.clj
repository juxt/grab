;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document
  (:require [juxt.grab.alpha.schema :as schema]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(defn add-error [acc error]
  (update acc ::errors conj error))

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
      (throw (ex-info "Operation name required" {})))
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
    (add-error
     acc
     {:message "A document containing a type system definition or extension is invalid for execution"})))

(defn add-operations [{::keys [document] :as acc}]
  (assoc acc ::operations (vec (keep ::g/operation-definition document))))

(defn add-fragments [{::keys [document] :as acc}]
  (assoc acc ::fragments (vec (keep ::g/fragment-definition document))))

(defn group-operations-by-name [{::keys [operations] :as acc}]
  (assoc acc ::operations-grouped-by-name (group-by ::g/name operations)))

(defn group-fragments-by-name [{::keys [fragments] :as acc}]
  (assoc acc ::fragments-grouped-by-name (group-by ::g/name fragments)))

(defn ^{:juxt/see "https://spec.graphql.org/June2018/#sec-Lone-Anonymous-Operation"}
  validate-anonymous
  [{::keys [operations operations-grouped-by-name] :as acc}]
  (assert operations)
  (when (> (count operations) 1)
    (when-not (empty? (get operations-grouped-by-name nil))
      (add-error acc {:message "When there are multiple operations in the document, none can be anonymous"}))))

(defn validate-operation-uniqueness [{::keys [operations-grouped-by-name] :as acc}]
  (reduce-kv
   (fn [acc n ops]
     (if (> (count ops) 1)
       (add-error acc {:message (format "Operation name '%s' is not unique" n) :name n})
       (assoc-in acc [::operations-by-name n] (first ops))))
   acc
   operations-grouped-by-name))

(defn validate-fragment-uniqueness [{::keys [fragments-grouped-by-name] :as acc}]
  (reduce-kv
   (fn [acc n ops]
     (if (> (count ops) 1)
       (add-error acc {:message (format "Fragment name '%s' is not unique" n) :name n})
       (assoc-in acc [::fragments-by-name n] (first ops))))
   acc
   fragments-grouped-by-name))

(defn add-default-operation-type [acc]
  (update acc ::operations #(mapv (fn [op]
                                    (cond-> op
                                      (not (::g/operation-type op))
                                      (assoc ::g/operation-type :query)))
                                  %)))

(defn scope-selection-set
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
          (-> selection
              (assoc ::scoped-type-name scoped-type-name)
              (assoc ::return-type return-type)
              (cond-> selection-set
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
            (update ::g/selection-set scope-selection-set type-condition schema))

        (throw
         (ex-info
          "TODO: unhandled selection-type"
          {:selection-type selection-type
           :selection-set selection-set
           :scoped-type-name scoped-type-name
           :schema? (some? scoped-type-name)})))))))

(defn add-scoped-types-to-operations [{::keys [schema] :as acc}]
  (update
   acc ::operations
   #(mapv
     (fn [op]
       (let [op-type (::g/operation-type op)
             scoped-type-name (get-in acc [::schema ::schema/root-operation-type-names op-type])]
         (-> op
             (assoc ::scoped-type-name scoped-type-name)
             (update
              ::g/selection-set
              (fn [selection-set]
                (scope-selection-set selection-set scoped-type-name schema)))))) %)))

(defn add-scoped-types-to-fragments [{::keys [schema] :as acc}]
  (update
   acc ::fragments
   #(mapv
     (fn [fragment]
       (let [scoped-type-name (::g/type-condition fragment)]
         (-> fragment
             (assoc ::scoped-type-name scoped-type-name)
             (update
              ::g/selection-set
              (fn [selection-set]
                (scope-selection-set selection-set scoped-type-name schema)))))) %)))

(defn validate-selection [{::g/keys [selection-type] :as selection}
                          {::schema/keys [types-by-name] :as schema}
                          path]
  (case selection-type
    :field
    (let [scoped-type-name (::scoped-type-name selection)
          field-name (::g/name selection)
          path (conj path field-name)
          field-def (some-> scoped-type-name types-by-name ::schema/fields-by-name (get field-name))
          selection-type (some-> field-def ::g/type-ref schema/unwrapped-type ::g/name types-by-name)
          subselection-set (::g/selection-set selection)
          ;; Again for skip include TODO
          ]

      (cond
        (= field-name "__typename") []  ; allow for introspection

        (nil? field-def)
        [{:message (format
                    "Field name '%s' not defined on type in scope '%s'"
                    (::g/name selection)
                    scoped-type-name)
          :selection selection
          :field-name (::g/name selection)
          :path path
          :scoped-type-name scoped-type-name
          :type-ref (some-> scoped-type-name types-by-name)}]

        (and (#{'SCALAR 'ENUM} (some-> selection-type ::g/kind)) subselection-set)
        [{:message "The subselection set of a scalar or enum must be empty"}]

        subselection-set
        (mapcat #(validate-selection % schema path) subselection-set)

        :else []))

    :fragment-spread []                 ; Already covered

    :inline-fragment
    (let [path (conj path (::scoped-type-name selection))]
      (mapcat #(validate-selection % schema path)
              (::g/selection-set selection)
              ;; Again here for skip/include TODO

              ))))

(defn validate-selection-sets [{::keys [schema] :as acc}]
  (update
   acc ::errors into
   (filter identity
           (mapcat (fn [submap]
                     (mapcat (fn [selection]
                               (validate-selection selection
                                                   schema
                                                   [(::g/name submap)]))
                             (::g/selection-set submap))) ;Maybe here would be a good place to introduce some logic for the skip/include directive? TODO
                   (concat
                    (::operations acc)
                    (::fragments acc))))))


(defn visit-fields [selection schema]
  (case (::g/selection-type selection)
    :field [selection]

    :inline-fragment
    (mapcat visit-fields (::g/selection-set selection) (repeat schema))

    :fragment-spread
    ;; If we can't find the fragment, we don't error because this will be
    ;; spotted by another validator.
    (when-let [fragment (get-in schema [::fragments-by-name (::g/name selection)])]
      (mapcat visit-fields (::g/selection-set fragment) (repeat schema)))

    (throw (ex-info "Unexpected selection type" {:selection selection}))))

(defn fields-by-name [selection-set schema]
  (->>
   (mapcat visit-fields selection-set (repeat schema))
   (group-by (fn [field]
               (or (get field ::g/alias) (get field ::g/name))))))

(defn same-response-shape
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

(defn fields-in-set-can-merge
  [selection-set {::schema/keys [types-by-name] :as schema}
   parent-scoped-type path]
  (let [;; "1. Let fieldsForName be the set of selections with a given response
        ;; name in set including visiting fragments and inline fragments."
        fields-for-name (fields-by-name selection-set schema)]
    (->>
     fields-for-name
     (mapcat
      (fn [[response-name fields]]
        ;; "2. Given each pair of members fieldA and fieldB in fieldsForName:"

        [;; "a. SameResponseShape(fieldA, fieldB) must be true." (TODO)
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
     (filter some?))))

(defn validate-fields-in-set-can-merge [acc]
  (update
   acc ::errors into ;; TODO: Also visit fragments and inline fragments as per spec "including visiting
   ;; fragments and inline fragments."

   (mapcat (fn [op-or-fragment]
             (fields-in-set-can-merge (::g/selection-set op-or-fragment)
                                      (::schema acc)
                                      (::scoped-type-name op-or-fragment)
                                      [(::g/name op-or-fragment)]))
           (concat
            (::operations acc)
            (::fragments acc)))))

(defn compile-document*
  "Compile a document with respect to the given schema, returning a structure that
  can be provided to the execution functions."
  ([document schema]
   (compile-document*
    document schema
    {:compilers
     [add-operations
      add-default-operation-type
      add-fragments
      add-scoped-types-to-operations
      add-scoped-types-to-fragments
      group-operations-by-name
      group-fragments-by-name

      validate-executable-definitions
      validate-selection-sets
      validate-anonymous
      validate-operation-uniqueness
      validate-fragment-uniqueness
      validate-fields-in-set-can-merge]}))

  ([document schema {:keys [compilers]}]
   (reduce
    (fn [acc f]
      ;; Allow compilers to return nil (e.g. applicability guards)
      (or (f acc) acc))

    {::errors []
     ::document document
     ::schema schema}

    compilers)))

(defn compile-document [document schema]
  (let [acc (compile-document* document schema)]
    (when (seq (::errors acc))
      (throw
       (ex-info
        "Failed to compile document due to errors"
        {:errors (::errors acc)})))
    acc))
