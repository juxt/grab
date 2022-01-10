;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document
  (:require
   [juxt.grab.alpha.schema :as schema]
   [clojure.walk :refer [postwalk]]
   [clojure.math.combinatorics :as combo]))

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

(defn argument-definitions-by-name [schema type-name field]
  (reduce-kv
   (fn [acc arg-name _]
     (assoc acc arg-name
            (get-in
             schema
             [::schema/types-by-name type-name
              ::schema/fields-by-name (::g/name field)
              ::g/argument-definitions-by-name arg-name])))
   {} (::g/arguments field)))

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

(defn collect-fields-by-name [selection-set schema]
  (->>
   (mapcat visit-fields selection-set (repeat schema))
   (group-by (fn [field]
               (or (get field ::g/alias) (get field ::g/name))))))

(defn ^{:juxt.grab.alpha.spec-ref/version "June2018"
        :juxt.grab.alpha.spec-ref/section "5.3.2"
        :juxt.grab.alpha.spec-ref/algorithm "SameResponseShape"}
  check-same-response-shape [field-a field-b schema]
  #_(let [types (->>
               fields
               (map (fn [field]
                      (if-let [typ (::return-type field)]
                        typ
                        (throw (ex-info "Field does not have a return type" {:field field}))))))]
    ;; TODO: 3. Non-Null (test first)
    ;; TODO: 4. Lists (test first)
    ;; TODO: 5. Scalar/Enum (test first)
    ;; TODO: 6. Composite type (test first)
    ))

(defn ->location [m]
  (let [line (some-> m meta ::g/location :line)
        column (some-> m meta ::g/location :column)]
    (cond-> {}
      line (assoc :line line)
      column (assoc :column column))))

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "5.3.2"
    :juxt.grab.alpha.spec-ref/algorithm "FieldsInSetCanMerge"}
  check-fields-in-set-can-merge [{::g/keys [selection-set] :as node} schema]
  (if-let [errors
           (->>
            (collect-fields-by-name selection-set schema)
            (mapcat
             (fn [[response-name fields]]
               (mapcat
                seq
                (for [[field-a field-b] (combo/combinations fields 2)]
                  (concat
                   (check-same-response-shape field-a field-b schema)

                   ;; b. If the parent types of fieldA and fieldB are equal
                   ;; or if either is not an Object Type:
                   (when (or
                          (and
                           (some? (::type-name field-a))
                           (= (::type-name field-a) (::type-name field-b)))
                          (not= (::type-kind field-a) 'OBJECT)
                          (not= (::type-kind field-b) 'OBJECT))

                     (cond
                       ;; i. fieldA and fieldB must have identical field names
                       (not= (::g/name field-a) (::g/name field-b))
                       [{:message "Fields must have identical field names"
                         :response-name response-name
                         :field-a-name (::g/name field-a)
                         :field-b-name (::g/name field-b)
                         :locations [(->location field-a)
                                     (->location field-b)]
                         :field-a field-a
                         :field-b field-b}])))))))
            seq)]
    (assoc node ::fields-in-set-can-merge-errors (vec errors))
    (assoc node ::fields-in-set-can-merge? true)))

(defn decorate-selection [selection {:keys [type schema path] :as context}]
  (assert type)
  (case (::g/selection-type selection)
    :field
    (let [{field-name ::g/name :as field} selection
          field-definition (get-in type [::schema/fields-by-name field-name])
          return-type (some-> field-definition ::g/type-ref)
          new-path (conj path (or (::g/alias selection) (::g/name selection)))]
      (cond-> selection
        true (assoc ::type-name (::g/name type))
        true (assoc ::type-kind (::g/kind type))
        true (assoc ::field-definition field-definition)
        return-type (assoc ::return-type return-type)
        path (assoc ::path new-path)

        (::g/selection-set field)
        (->
         (update
          ::g/selection-set
          (fn [selection-set]
            (let [type-name (some-> return-type schema/unwrapped-type ::g/name)
                  type (get-in schema [::schema/types-by-name type-name])]
              (->>
               selection-set
               (mapv
                (fn [selection]
                  (decorate-selection
                   selection
                   (-> context
                       (assoc :type type)
                       (assoc :path new-path)))))))))
         (check-fields-in-set-can-merge schema))))

    :inline-fragment
    (let [type-condition (::g/type-condition selection)
          type-in-scope (get-in schema [::schema/types-by-name type-condition])]
      (cond-> selection
        (::g/selection-set selection)
        (->
         (update
          ::g/selection-set
          (fn [selection-set]
            (->> selection-set
                 (mapv
                  (fn [selection]
                    (decorate-selection
                     selection
                     (assoc context :type type-in-scope)))))))
         (check-fields-in-set-can-merge schema))))

    (throw
     (ex-info
      (format "TODO: add selection-type for %s" (::g/selection-type selection))
      {:selection selection}))))

(defn decorate-document
  "Decorate a document with information from the given schema."
  [document schema]
  (->>
   document
   (postwalk
    (fn [node]
      (cond
        (and
         (vector? node)
         (= (first node) :juxt.grab.alpha.graphql/operation-definition))
        ;; Only occurs at the start of a executable definition
        (update
         node 1
         (fn [op-def]
           (let [type-name (get-in schema [::schema/root-operation-type-names (::g/operation-type op-def)])
                 type (get-in schema [::schema/types-by-name type-name])]
             (cond-> op-def
               type-name (assoc ::type-name type-name)
               type
               (update
                ::g/selection-set
                (fn [selection-set]
                  (mapv
                   #(decorate-selection
                     %
                     {:type type
                      :schema schema
                      :path []}) selection-set)))))))

        (and
         (vector? node)
         (= (first node) :juxt.grab.alpha.graphql/fragment-definition))
        (update
         node 1
         (fn [frag-def]
           (let [frag-name (::g/name frag-def)
                 type-name (::g/type-condition frag-def)
                 type-in-scope (get-in schema [::schema/types-by-name type-name])]
             (cond-> frag-def
               type-name (assoc ::type-name type-name)
               type-in-scope
               (->
                (update
                 ::g/selection-set
                 (fn [selection-set]
                   (mapv
                    #(decorate-selection
                      %
                      {:type type-in-scope
                       :schema schema
                       :path [frag-name]}) selection-set)))
                (check-fields-in-set-can-merge schema))))))

        (map? node)
        (cond-> node
          (= (::g/selection-type node) :inline-fragment)
          ;; Use field information to decorate arguments
          (->
           (update
            ::g/selection-set
            (fn [selection-set]
              (let [type-name (::g/type-condition node)]
                (->>
                 selection-set
                 (mapv
                  (fn [{::g/keys [selection-type] :as field}]
                    (cond-> field
                      (and
                       ;; if this is a field
                       (= selection-type :field)
                       ;; with arguments
                       (::g/arguments field))
                      ;; associate the argument definitions close to the
                      ;; arguments
                      (assoc
                       ::argument-definitions-by-name
                       (argument-definitions-by-name schema type-name field)))))))))
           (check-fields-in-set-can-merge schema)))

        :else node)))))

(defn compile-document
  "Compile a document with respect to the given schema, returning a structure that
  can be provided to the execution functions."
  ([document schema]
   (compile-document
    document schema
    {:compilers
     [decorate-document
      ]}))

  ([document schema {:keys [compilers]}]
   (reduce
    (fn [doc f]
      ;; Allow compilers to return nil (e.g. applicability guards)
      (or (f doc schema) doc))
    document
    compilers)))

(defn entry? [e] (and (vector? e) (= (count e) 2)))

(defn validate-document
  "Returns a collection of errors, if any.."
  [document]
  (->>
   document
   (tree-seq (some-fn map? vector?) seq)
   (mapcat
    (fn [form]
      (when (map? form)
        (cond
          (= (find form ::field-definition) [::field-definition nil])
          [(assoc
            form
            ::message
            (format
             "Field name '%s' not defined on type in scope '%s'"
             (::g/name form) (::type-name form)))]

          ;; 5.3.2
          (and
           (find form ::g/selection-set)
           (not (::fields-in-set-can-merge? form)))
          [(assoc form ::message "FieldsInSetCanMerge(set) must be true")]


          ))))
   ))
