;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.schema
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(def ^{:doc "Are there multiple items in the collection? (i.e. more than one)"}
  multiple? (comp pos? dec count))

(defn duplicates-by [pred coll]
  (->> coll
       (group-by pred)
       (keep #(when (-> % second multiple?) (first %)))
       seq))

(defn check-unique-type-names
  "'All types within a GraphQL schema must have unique names. No two provided
  types may have the same name.' -- https://spec.graphql.org/June2018/#sec-Schema"
  [{::keys [document] :as acc}]
  (let [duplicates
        (->> document
             (filter #(= (::g/definition-type %) :type-definition))
             (duplicates-by ::g/name))]
    (cond-> acc
      duplicates
      (update
       ::errors conj
       {:error "All types within a GraphQL schema must have unique names"
        :duplicates duplicates}))))

(defn check-no-conflicts-with-built-in-types
  "'No provided type may have a name which conflicts
  with any built in types (including Scalar and Introspection
  types).' -- https://spec.graphql.org/June2018/#sec-Schema"
  [{::keys [document] :as acc}]
  (let [conflicts
        (seq
         (set/intersection
          (set (map ::g/name (filter #(= (::g/definition-type %) :type-definition) document)))
          (set #{"Int" "Float" "String" "Boolean" "ID"})))]
    (cond-> acc
      conflicts
      (update
       ::errors conj
       {:error "No provided type may have a name which conflicts with any built in types"
        :conflicts (set conflicts)}))))

(defn check-unique-directive-names
  "'All directives within a GraphQL schema must have unique names.' --
  https://spec.graphql.org/June2018/#sec-Schema"
  [{::keys [document] :as acc}]
  (let [duplicates
        (->> document
             (filter #(= (::g/definition-type %) :directive-definition))
             (duplicates-by ::g/name))]
    (cond-> acc
      duplicates
      (update
       ::errors conj
       {:error "All directives within a GraphQL schema must have unique names"
        :duplicates duplicates}))))

(defn check-reserved-names
  "'All types and directives defined within a schema must not have a name which
  begins with \"__\" (two underscores), as this is used exclusively by GraphQL’s
  introspection system.' -- https://spec.graphql.org/June2018/#sec-Schema"
  [{::keys [document] :as acc}]
  (let [reserved-clashes
        (seq
         (filter #(str/starts-with? % "__")
                 (map ::g/name (filter #(#{:type-definition :directive-definition} (::g/definition-type %)) document))))]
    (cond-> acc
      reserved-clashes
      (update
       ::errors conj
       {:error "All types and directives defined within a schema must not have a name which begins with '__' (two underscores), as this is used exclusively by GraphQL's introspection system"}))))

(defn check-field-definition [acc tf]
  (cond-> acc
    (str/starts-with? (::g/name tf) "__")
    (update ::errors conj
            {:error "A field must not have a name which begins with two underscores."
             :field-name (::g/name tf)})))

(defn check-duplicate-field-names [acc td]
  (let [duplicates (duplicates-by ::g/name (::g/field-definitions td))]
    (cond-> acc
      duplicates
      (update
       ::errors conj
       {:error (format "Each field must have a unique name within the '%s' Object type; no two fields may share the same name." (::g/name td))
        :duplicates (vec duplicates)}))))

(defn check-type-fields [acc td]
  (as-> acc %
    (check-duplicate-field-names % td)
    (reduce check-field-definition % (::g/field-definitions td))))

;; See Type Validation sub-section of https://spec.graphql.org/June2018/#sec-Objects
(defn check-types
  [{::keys [document] :as acc}]
  (reduce
   (fn [acc {::g/keys [name field-definitions] :as td}]
     (cond-> acc
       (= (::g/kind td) :object)
       (cond->
           ;; "1. An Object type must define one or more fields."
           (or
            (nil? field-definitions)
            (zero? (count field-definitions)))
           (update ::errors conj {:error "An Object type must define one or more fields"
                                  :type-definition td})

           true (check-type-fields td))))
   acc
   (filter #(= (::g/definition-type %) :type-definition) document)))

(defn provide-types
  "Creates the schema's 'provided-types' entry."
  [{::keys [document] :as acc}]
  (reduce
   (fn [acc {::g/keys [name] :as td}]
     (assoc-in
      acc [::provided-types name]
      (assoc td ::fields-by-name (into {} (map (juxt ::g/name identity) (::g/field-definitions td))))))
   acc
   (filter #(= (::g/definition-type %) :type-definition) document)))

(defn check-root-operation-type
  "Depends on validate-types."
  [acc]
  (let [query-root-op-type-name (get-in acc [::root-operation-type-names :query])
        query-root-op-type (get-in acc [::provided-types query-root-op-type-name])]
    (assert query-root-op-type-name)
    (cond
      (nil? query-root-op-type)
      (update acc
       ::errors conj
       {:error (format "The query root operation type must be provided: '%s'" query-root-op-type-name)})

      (not= :object (get query-root-op-type ::g/kind))
      (update acc
       ::errors conj
       {:error "The query root operation type must be an Object type"})

      :else acc)))

(defn check-schema-definition-count [{::keys [document] :as acc}]
  (when (pos? (dec (count (filter #(= (::g/definition-type %) :schema-definition) document))))
    (update acc ::errors conj {:error "A document must include at most one schema definition"})))

(defn process-schema-definition [{::keys [document] :as acc}]
  (let [schema-def (first (filter #(= (::g/definition-type %) :schema-definition) document))]
    (cond-> acc
      true
      (assoc
       ::root-operation-type-names
       (or
        (when schema-def (::g/operation-types schema-def))
        {:query "Query" :mutation "Mutation" :subscription "Subscription"}))
      (::g/directives schema-def)
      (assoc ::g/directives (::g/directives schema-def)))))

(defn compile-schema
  "Create a schema from the parsed document."
  [document]
  (reduce
   (fn [acc f]
     (or (f acc) acc))
   {::errors []
    ::document document
    ::provided-types
    {}
    ::built-in-types
    {"Int" {::g/name "Int"
            ::g/kind :scalar}
     "Float" {::g/name "Float"
              ::g/kind :scalar}
     "String" {::g/name "String"
               ::g/kind :scalar}
     "Boolean" {::g/name "Boolean"
                ::g/kind :scalar}
     "ID" {::g/name "ID"
           ::g/kind :scalar}}}
   [check-unique-type-names
    check-no-conflicts-with-built-in-types
    check-unique-directive-names
    check-reserved-names
    check-types
    provide-types
    process-schema-definition
    check-schema-definition-count
    check-root-operation-type]))

(defn process-schema-extension [schema {::g/keys [directives operation-types]}]
  (let [add-directives
        (fn [schema directives]
          (let [existing-directives
                (set/intersection
                 (some-> schema ::g/directives keys set)
                 (some-> directives keys set))]
            (if (seq existing-directives)
              (update
               schema ::errors conj
               {:error "Any directives provided must not already apply to the original Schema"
                :existing-directives existing-directives})
              (update schema ::g/directives merge directives))))
        add-operation-types
        (fn [schema operation-types]
          (let [duplicates
                (set/intersection
                 (some-> schema ::root-operation-type-names keys set)
                 (some-> operation-types keys set))]
            (if (seq duplicates)
              (update
               schema ::errors conj
               {:error "Schema extension attempting to add root operation types that already exist"
                :duplicates duplicates})
              (update schema ::root-operation-type-names merge operation-types))))]
    (cond-> schema
      directives (add-directives directives)
      operation-types (add-operation-types operation-types))))

(defn extend-schema
  "Extend a schema"
  [schema document]
  (assert (empty? (::errors schema)) "Cannot extend schema when there are pre-existing errors")
  (reduce
   (fn [schema definition]
     (cond-> schema
       (= (::g/definition-type definition) :schema-extension)
       (process-schema-extension definition)))
   schema
   document))
