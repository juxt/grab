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

(defn scope-selection-set [selection-set scoped-type schema]
  (->>
   selection-set
   (mapv
    (fn [{field-name ::g/name
          selection-type ::g/selection-type
          selection-set ::g/selection-set
          :as selection}]
      (case selection-type
        :field
        (let [field-type-name (get-in scoped-type [::g/field-definitions field-name ::g/type])
              scoped-type (get-in schema [::schema/types-by-name field-type-name])]
          (-> selection
              (assoc ::scoped-type scoped-type)
              (cond-> selection-set
                (update ::g/selection-set scope-selection-set scoped-type schema))))

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
               scoped-type-name (get-in acc [::schema ::schema/root-operation-type-names op-type])
               scoped-type (get-in schema [::schema/types-by-name scoped-type-name])]
           (-> op
               (assoc
                ::scoped-type scoped-type)
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
         (let [scoped-type-name (::g/type-condition fragment)
               scoped-type (get-in schema [::schema/types-by-name scoped-type-name])]
           (-> fragment
               (assoc ::scoped-type scoped-type)
               (update
                ::g/selection-set
                (fn [selection-set]
                  (scope-selection-set selection-set scoped-type schema)))))))))))

(defn validate-selection [selection parent-scoped-type path]
  (let [scoped-type (::scoped-type selection)
        path (conj path
                   (case (::g/selection-type selection)
                     :field (::g/name selection)
                     (throw (ex-info "TODO" {}))))]
    (if scoped-type
      (when-let [selection-set (::g/selection-set selection)]
        (mapcat #(validate-selection % scoped-type path) selection-set))
      (let [field-name (::g/name selection)]
        [{:error (format
                  "Field name '%s' not defined on type in scope '%s'"
                  field-name
                  (::g/name parent-scoped-type))
          :selection selection
          :scoped-type parent-scoped-type
          :field-name field-name
          :path path}]))))

(defn validate-selection-sets [acc]
  (update
   acc ::errors (comp vec concat)
   (concat
    (for [op (::operations acc)
          selection (::g/selection-set op)
          error (validate-selection selection (::scoped-type op) [(::g/name op)])
          :when error]
      error)
    (for [frag (::fragments acc)
          selection (::g/selection-set frag)
          error (validate-selection selection (::scoped-type frag) [(::g/fragment-name frag)])
          :when error]
      error))))

(defn compile
  "Compile a document with respect to the given schema, returning a structure that
  can be provided to the execution functions."
  [document schema]

  (reduce
   (fn [acc f]
     ;; Allow functions to return nil (e.g. applicability guards)
     (or (f acc) acc))

   {::errors []
    ::document document
    ::schema schema}

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
    ]))
