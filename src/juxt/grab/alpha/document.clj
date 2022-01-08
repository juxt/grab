;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document
  (:require
   [juxt.grab.alpha.schema :as schema]
   [clojure.walk :refer [postwalk]]))

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
  check-same-response-shape [fields schema]
  (let [types (->>
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

(defn
  ^{:juxt.grab.alpha.spec-ref/version "June2018"
    :juxt.grab.alpha.spec-ref/section "5.3.2"
    :juxt.grab.alpha.spec-ref/algorithm "FieldsInSetCanMerge"}
  check-fields-in-set-can-merge [{::g/keys [selection-set] :as node} schema]
  (if-let [errors
           (->> (collect-fields-by-name selection-set schema)
                (mapcat
                 (fn [[response-name fields]]
                   (try
                     (check-same-response-shape fields schema) ; a.
                     ;; TODO: Get parent type of field

                     (catch Exception e
                       [{:message (.getMessage e)
                         :response-name response-name
                         :fields fields
                         :ex-data (ex-data e)}]))))
                seq
                )]
    (assoc node ::fields-in-set-can-merge-errors errors)
    (assoc node ::fields-in-set-can-merge? true)))

(defn decorate-selection [selection {:keys [scoped-type schema path] :as context}]
  (case (::g/selection-type selection)
    :field
    (let [{field-name ::g/name :as field} selection
          field-definition (get-in scoped-type [::schema/fields-by-name field-name])
          field-type (some-> field-definition ::g/type-ref)
          new-path (conj path (or (::g/alias selection) (::g/name selection)))]
      (cond-> selection
        scoped-type (assoc ::scoped-type-name (::g/name scoped-type))
        true (assoc ::field-definition field-definition)
        ;; TODO: Rename ::return-type to ::field-type?
        field-type (assoc ::return-type field-type)
        path (assoc ::path new-path)

        (::g/selection-set field)
        (->
         (update
          ::g/selection-set
          (fn [selection-set]
            (let [type-name (some-> field-type schema/unwrapped-type ::g/name)
                  scoped-type (get-in schema [::schema/types-by-name type-name])]
              (->>
               selection-set
               (mapv
                (fn [selection]
                  (decorate-selection
                   selection
                   (-> context
                       (assoc :scoped-type scoped-type)
                       (assoc :path new-path)))))))))
         (check-fields-in-set-can-merge schema))))

    :inline-fragment
    (let [type-condition (::g/type-condition selection)
          scoped-type (get-in schema [::schema/types-by-name type-condition])]
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
                     (assoc context :scoped-type scoped-type)))))))
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
           (-> op-def
               (assoc ::scoped-type-name (get-in schema [::schema/root-operation-type-names (::g/operation-type op-def)]))
               (update
                ::g/selection-set
                (fn [selection-set]
                  (let [scoped-type-name (get-in schema [::schema/root-operation-type-names (::g/operation-type node)])
                        scoped-type (get-in schema [::schema/types-by-name scoped-type-name])]
                    (when-not scoped-type
                      (throw
                       (ex-info
                        "No scoped-type"
                        {:types (keys (::schema/types-by-name schema))
                         :type-name scoped-type-name})))
                    (mapv
                     #(decorate-selection
                       %
                       {:scoped-type scoped-type
                        :schema schema
                        :path []}) selection-set)))))))

        (and
         (vector? node)
         (= (first node) :juxt.grab.alpha.graphql/fragment-definition))
        (update
         node 1
         (fn [frag-def]
           (let [frag-name (::g/name frag-def)
                 type-name (::g/type-condition frag-def)]
             (-> frag-def
                 (assoc ::scoped-type-name type-name)
                 (update
                  ::g/selection-set
                  (fn [selection-set]
                    (let [scoped-type (get-in schema [::schema/types-by-name type-name])]
                      (when-not scoped-type
                        (throw
                         (ex-info
                          "No scoped-type"
                          {:types (keys (::schema/types-by-name schema))
                           :type-name type-name})))
                      (mapv
                       #(decorate-selection
                         %
                         {:scoped-type scoped-type
                          :schema schema
                          :path [frag-name]}) selection-set))))
                 (check-fields-in-set-can-merge schema)))))

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

(defn validate-document
  "Returns a collection of errors, if any.."
  [document]
  (->>
   document
   (tree-seq (some-fn map? vector?) seq)
   (mapcat
    (fn [form]
      (when (and (map? form)
                 (= (find form ::field-definition) [::field-definition nil]))
        [(assoc
          form
          ::message
          (format
           "Field name '%s' not defined on type in scope '%s'"
           (::g/name form) (::scoped-type-name form)
           ))])))))
