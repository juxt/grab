;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.schema
  (:require
   [juxt.grab.alpha.parser :refer [parse]]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(def ^{:doc "Are there multiple items in the collection? (i.e. more than one)"}
  multiple? (comp pos? dec count))

(defn duplicates-by [pred coll]
  (->> coll
       (group-by pred)
       (keep #(when (-> % second multiple?) (first %)))
       seq))

(defn add-error [acc error]
  (update acc ::errors conj error))

(defn check-unique-type-names
  "'All types within a GraphQL schema must have unique names. No two provided
  types may have the same name.' -- https://spec.graphql.org/June2018/#sec-Schema"
  [acc document]
  (let [duplicates
        (->> document
             (filter #(= (::g/definition-type %) :type-definition))
             (duplicates-by ::g/name))]
    (cond-> acc
      duplicates
      (add-error {:message "All types within a GraphQL schema must have unique names."
                  :duplicates duplicates}))))

(defn check-no-conflicts-with-built-in-types
  "'No provided type may have a name which conflicts
  with any built in types (including Scalar and Introspection
  types).' -- https://spec.graphql.org/June2018/#sec-Schema"
  [acc document]
  (let [conflicts
        (for [type-def (map ::g/name (filter #(= (::g/definition-type %) :type-definition) document))
              existing #{"Int" "Float" "String" "Boolean" "ID"}
              :when (= type-def existing)]
          type-def)]
    (cond-> acc
      (seq conflicts)
      (add-error
       {:message "No provided type may have a name which conflicts with any built in types."
        :conflicts conflicts}))))

(defn check-unique-directive-names
  "'All directives within a GraphQL schema must have unique names.' --
  https://spec.graphql.org/June2018/#sec-Schema"
  [acc document]
  (let [duplicates
        (->> document
             (filter #(= (::g/definition-type %) :directive-definition))
             (duplicates-by ::g/name))]
    (cond-> acc
      duplicates
      (add-error
       {:message "All directives within a GraphQL schema must have unique names."
        :duplicates duplicates}))))

(defn check-reserved-names
  "'All types and directives defined within a schema must not have a name which
  begins with \"__\" (two underscores), as this is used exclusively by GraphQL’s
  introspection system.' -- https://spec.graphql.org/June2018/#sec-Schema"
  [acc document]
  (let [reserved-clashes
        (seq
         (filter #(str/starts-with? % "__")
                 (map ::g/name (filter #(#{:type-definition :directive-definition} (::g/definition-type %)) document))))]
    (cond-> acc
      reserved-clashes
      (add-error
       {:message "All types and directives defined within a schema must not have a name which begins with '__' (two underscores), as this is used exclusively by GraphQL's introspection system."}))))

(defn unwrapped-type [typ]
  (cond
    (::g/list-type typ) (unwrapped-type (::g/list-type typ))
    (::g/non-null-type typ) (unwrapped-type (::g/non-null-type typ))
    :else typ))

(defn output-type? [typ]
  (#{'SCALAR 'OBJECT 'INTERFACE 'UNION 'ENUM} (::g/kind (unwrapped-type typ))))

(defn input-type? [typ]
  (#{'SCALAR 'ENUM 'INPUT_OBJECT} (::g/kind (unwrapped-type typ))))

(defn check-object-field-argument-definition [{::keys [types-by-name] :as acc} arg-def tf]
  (let [type-name (some-> arg-def ::g/type-ref unwrapped-type ::g/name)
        typ (get types-by-name type-name)]
    (cond-> acc
      (str/starts-with? (::g/name arg-def) "__")
      (add-error
       {:message "A field argument must not have a name which begins with two underscores."
        :arg-name (::g/name arg-def)
        :field-name (::g/name tf)})

      (nil? typ)
      (add-error
       {:message "A field argument must accept a type that is known."
        :field-name (::g/name tf)
        :type typ
        :argument-definition arg-def})

      (and typ (not (input-type? typ)))
      (add-error
       {:message "A field argument must accept a type that is an input type."
        :field-name (::g/name tf)
        :argument-definition arg-def}))))

(defn check-object-field-argument-definitions [acc arg-defs tf]
  (reduce #(check-object-field-argument-definition %1 %2 tf) acc arg-defs))

(defn resolve-named-type-ref
  "Return nil if no type found"
  [{::keys [types-by-name] :as acc} type-ref]
  (assert type-ref)
  (assert (::g/name type-ref) (pr-str type-ref))
  (get types-by-name (::g/name type-ref)))

(defn check-object-field-definition [acc tf]
  (let [type-ref (some-> tf ::g/type-ref unwrapped-type)
        typ (resolve-named-type-ref acc type-ref)
        arg-defs (::g/arguments-definition tf)]
    (cond-> acc
      ;; 2. The field must not have a name which begins with the characters "__" (two underscores).
      (str/starts-with? (::g/name tf) "__")
      (add-error
       {:message "A field must not have a name which begins with two underscores."
        :field-name (::g/name tf)})

      (nil? typ)
      (add-error
       {:message (str "A field must return a type that is known.")
        :field-name (::g/name tf)
        :field-type-name (::g/name type-ref)
        :location (::g/location (meta tf))
        })

      ;; 4. The field must return a type where IsOutputType(fieldType) returns *true*.
      (and typ (not (output-type? typ)))
      (add-error
       {:message "A field must return a type that is an output type."
        :field-name (::g/name tf)
        :type typ
        :tf tf})
      arg-defs
      (check-object-field-argument-definitions arg-defs tf))))

(defn check-duplicate-object-field-names [acc td]
  (let [duplicates (duplicates-by ::g/name (::g/field-definitions td))]
    (cond-> acc
      duplicates
      (add-error
       {:message (format "Each field must have a unique name within the '%s' Object type; no two fields may share the same name." (::g/name td))
        :duplicates (vec duplicates)}))))

(defn check-object-type-fields [acc {::g/keys [field-definitions] :as td}]
  ;; 3.6.2 For each field of an object-type:
  (as-> acc %
    ;; 1. Each field must have a unique name within that Object type; no two fields may share the same name
    (check-duplicate-object-field-names % td)
    (reduce check-object-field-definition % field-definitions)))

(defn check-object-interfaces-exist [{::keys [types-by-name] :as acc}
                                     {::g/keys [interfaces] :as td}]
  (reduce
   (fn [acc decl]
     (let [ifc (get types-by-name decl)]
       (cond-> acc
         (nil? ifc) (add-error {:message "Interface is declared but not provided." :interface decl}))))
   acc
   ;; We already check that interfaces are distinct, but if they're not we don't
   ;; want to produce identical errors.
   (distinct interfaces)))

(defn check-sub-type-covariance
  [acc type-definition object-field object-field-type-ref interface-field interface-field-type-ref]

  (let [oft (when (::g/name object-field-type-ref)
              (resolve-named-type-ref acc object-field-type-ref))
        ift (when (::g/name interface-field-type-ref)
              (resolve-named-type-ref acc interface-field-type-ref))]
    (cond
      ;; 4.1.1.1. An object field type is a valid sub‐type if it is equal to
      ;; (the same type as) the interface field type.
      #_(and
         (::g/name object-field-type-ref)
         (::g/name interface-field-type-ref)
         (= (resolve-named-type-ref acc object-field-type-ref)
            (resolve-named-type-ref acc interface-field-type-ref)))
      (and
       (= object-field-type-ref interface-field-type-ref))
      acc

      ;; 4.1.1.2. An object field type is a valid sub‐type if it is an Object
      ;; type and the interface field type is either an Interface type or a
      ;; Union type and the object field type is a possible type of the
      ;; interface field type.
      (and
       (::g/name object-field-type-ref)
       (::g/name interface-field-type-ref)
       (and
        (= (::g/kind oft) 'OBJECT)
        (or
         (= (::g/kind ift) 'INTERFACE)
         (= (::g/kind ift) 'UNION))
        (contains? (set (::g/interfaces oft)) (::g/name ift))))
      acc

      ;; 4.1.1.3. An object field type is a valid sub‐type if it is a List type
      ;; and the interface field type is also a List type and the list‐item type
      ;; of the object field type is a valid sub‐type of the list‐item type of
      ;; the interface field type.
      (and
       (::g/list-type object-field-type-ref)
       (::g/list-type interface-field-type-ref))
      (check-sub-type-covariance
       acc
       type-definition
       object-field
       (::g/list-type object-field-type-ref)
       interface-field
       (::g/list-type interface-field-type-ref))

      ;; 4.1.1.4. An object field type is a valid sub‐type if it is a Non‐Null
      ;; variant of a valid sub‐type of the interface field type.
      (::g/non-null-type object-field-type-ref)
      (check-sub-type-covariance
       acc
       type-definition
       object-field
       (::g/non-null-type object-field-type-ref)
       interface-field
       interface-field-type-ref)

      :else
      (add-error
       acc
       {:message "The object field must be of a type which is equal to or a sub‐type of the interface field (covariant)."
        :type-definition type-definition
        :object-field object-field
        :object-field-type-ref object-field-type-ref
        :interface-field interface-field
        :interface-field-type-ref interface-field-type-ref}))))

(defn check-object-field-arguments [acc object-field interface-field]
  (reduce
   (fn [acc {::g/keys [name type-ref]}]
     (let [object-arg-def (some #(when (= name (::g/name %)) %) (::g/arguments-definition object-field))
           object-arg-def-type-ref (::g/type-ref object-arg-def)]
       (cond-> acc
         (nil? object-arg-def)
         (add-error
          {:message "The object field must include an argument of the same name for every argument defined in the interface field."
           :argument-name name
           :field-name (::g/name interface-field)
           :interface (-> interface-field ::interface)})

         (and object-arg-def (not= type-ref object-arg-def-type-ref))
         (add-error
          {:message "The object field argument must accept the same type (invariant) as the interface field argument."
           :argument-name name
           :field-name (::g/name interface-field)
           :interface (-> interface-field ::interface)
           :interface-field-arg-type type-ref
           :object-field-arg-type object-arg-def-type-ref}))))

   acc
   (::g/arguments-definition interface-field)))

(defn check-additional-object-field-arguments [acc object-field interface-field]
  (reduce
   (fn [acc {::g/keys [name type-ref]}]
     (let [interface-arg-def (some #(when (= name (::g/name %)) %) (::g/arguments-definition interface-field))]
       (cond-> acc
         (and (nil? interface-arg-def)
              (some-> type-ref ::g/non-null-type))
         (add-error
          {:message "The object field may include additional arguments not defined in the interface field, but any additional argument must not be required, e.g. must not be of a non‐nullable type."
           :argument-name name
           :field-name (::g/name object-field)}))))
   acc
   (::g/arguments-definition object-field)))

(defn check-object-interface-fields
  [{::keys [types-by-name] :as acc}
   {::g/keys [interfaces field-definitions] :as td}]

  (let [object-fields-by-name (group-by ::g/name field-definitions)
        interfaces (keep types-by-name interfaces)
        interface-fields
        (for [i interfaces f (::g/field-definitions i)]
          (assoc f ::interface (::g/name i)))]

    (reduce
     (fn [acc interface-field]
       (let [field-name (::g/name interface-field)
             object-field (first (get object-fields-by-name field-name))]
         (if-not object-field
           ;; The object type must include a field of the same name for every field
           ;; defined in an interface.
           (add-error
            acc
            {:message "The object type must include a field of the same name for every field defined in an interface."
             :interface (::interface interface-field)
             :missing-field-name field-name})

           (-> acc
               ;; 4.1.1. The object field must be of a type which is equal to or
               ;; a sub‐type of the interface field (covariant).
               (check-sub-type-covariance
                td
                object-field
                (::g/type-ref object-field)
                interface-field
                (::g/type-ref interface-field))

               ;; 4.1.2. The object field must include an argument of the same
               ;; name for every argument defined in the interface field.
               (check-object-field-arguments
                object-field
                interface-field)

               ;; 4.1.3. The object field may include additional arguments not
               ;; defined in the interface field, but any additional argument
               ;; must not be required, e.g. must not be of a non‐nullable
               ;; type. (TODO)
               (check-additional-object-field-arguments
                object-field
                interface-field)))))

     acc interface-fields)))

(defn check-object-interfaces [acc {::g/keys [interfaces] :as td}]
  (cond-> acc
    (not (apply distinct? interfaces))
    (add-error
     {:message "An object type may declare that it implements one or more unique interfaces. Interfaces declaration contains duplicates."
      :type-definition td})
    interfaces (-> (check-object-interfaces-exist td)
                   (check-object-interface-fields td))))

(defn check-object-type [acc {::g/keys [field-definitions interfaces] :as td}]
  (cond-> acc
    ;; "1. An Object type must define one or more fields."
    (or
     (nil? field-definitions)
     (zero? (count field-definitions)))
    (add-error
     {:message "An Object type must define one or more fields"
      :type-definition td})
    true (check-object-type-fields td)
    interfaces (check-object-interfaces td)))

(defn check-interface-duplicate-field-names [acc td]
  (let [duplicates (duplicates-by ::g/name (::g/field-definitions td))]
    (cond-> acc
      duplicates
      (add-error
       {:message (format "Each field must have a unique name within the '%s' Object type; no two fields may share the same name." (::g/name td))
        :duplicates (vec duplicates)}))))

(defn check-interface-field-argument-definition [{::keys [types-by-name] :as acc} arg-def tf]
  (let [type-name (some-> arg-def ::g/type-ref unwrapped-type ::g/name)
        typ (get types-by-name type-name)]
    (cond-> acc
      (str/starts-with? (::g/name arg-def) "__")
      (add-error
       {:message "A field argument must not have a name which begins with two underscores."
        :arg-name (::g/name arg-def)
        :field-name (::g/name tf)})

      (nil? typ)
      (add-error
       {:message "A field argument must accept a type that is known."
        :field-name (::g/name tf)
        :type typ
        :argument-definition arg-def})

      (and typ (not (input-type? typ)))
      (add-error
       {:message "A field argument must accept a type that is an input type."
        :field-name (::g/name tf)
        :argument-definition arg-def}))))

(defn check-interface-field-argument-definitions [acc arg-defs tf]
  (reduce #(check-interface-field-argument-definition %1 %2 tf) acc arg-defs))

(defn check-interface-field-definition [acc tf]
  (let [type-ref (some-> tf ::g/type-ref unwrapped-type)
        typ (resolve-named-type-ref acc type-ref)
        arg-defs (::g/arguments-definition tf)]
    (cond-> acc
      (str/starts-with? (::g/name tf) "__")
      (add-error
       {:message "A field must not have a name which begins with two underscores."
        :field-name (::g/name tf)})

      (nil? typ)
      (add-error
       {:message "A field must return a type that is known."
        :field-name (::g/name tf)
        :field-type-name (::g/name type-ref)})

      (and typ (not (output-type? typ)))
      (add-error
       {:message "A field must return a type that is an output type."
        :field-name (::g/name tf)
        :type typ
        :tf tf})
      arg-defs
      (check-interface-field-argument-definitions arg-defs tf))))

(defn check-interface-type-fields [acc {::g/keys [field-definitions] :as td}]
  (as-> acc %
    (check-interface-duplicate-field-names % td)
    (reduce check-interface-field-definition % field-definitions)))

(defn check-interface-type [acc td]
  (cond-> acc
    true (check-interface-type-fields td)))

(defn check-union-type [acc td document]
  (cond-> acc
    ;; 1. A union type must include one or more unique member types
    (nil? (::g/member-types td))
    (add-error {:message "A union type must include one or more unique member types"
                :td td})
    (not=
     (count (set (::g/member-types td)))
     (count (::g/member-types td)))
    
    (add-error {:message "A union type must include one or more unique member types"
                :members (::g/member-types td)
                :td td})
    ;; 2. The member types of a Union type must be all Object base types; Scalar, Interface, and Union types must not be member types of a Union. Similarly, wrapping types must not be member types of a Union.
    (::g/member-types td)
    ((fn [acc]
       (def td td)
       (reduce (fn [acc member]
                 (let [member-defs (filter #(and
                                             (= (::g/definition-type %) :type-definition)
                                             (= (::g/name %) member)) document)]
                   (if member-defs
                     (let [member-def (first member-defs)]
                       (if (not= (::g/kind member-def) 'OBJECT)
                         (add-error acc {:message "Member types of a Union type must all be Object base types"
                                         :name (::g/name member-def)
                                         :kind (::g/kind member-def)
                                         :td td})
                         acc))
                     
                     (add-error acc {:message "Member types of a Union type must all be Object base types" 
                                     :name member}))))
               acc (::g/member-types td))))
    ))

(defn check-enum-type [acc td]
  ;; 1. An enum type must include one or more unique member types
  (cond-> acc
    (nil? (::g/enum-values td))
    (add-error {:message "An enum type must include one or more unique member types"
                :td td})
    (not=
     (count (set (map ::g/name (::g/enum-values td))))
     (count (map ::g/name (::g/enum-values td))))
    (add-error {:message "An enum type must include one or more unique member types"
                :members (map ::g/name (::g/enum-values td))
                :td td})))

;; See Type Validation sub-section of https://spec.graphql.org/June2018/#sec-Objects
(defn check-types
  [acc document]
  (reduce
   (fn [acc td]
     (condp = (::g/kind td)
       'OBJECT
       (check-object-type acc td)
       'INTERFACE
       (check-interface-type acc td)
       'UNION
       (check-union-type acc td document)
       'ENUM
       (check-enum-type acc td)
       acc))
   acc
   (filter #(= (::g/definition-type %) :type-definition) document)))

(defn compile-directive [directive]
  (-> directive
      (dissoc ::g/name)))

(defn compile-argument-definition [argument-def]
  (let [directives-by-name (into {} (map (juxt ::g/name compile-directive) (::g/directives argument-def)))]
    (cond-> argument-def
      (seq directives-by-name) (assoc ::directives-by-name directives-by-name))))

(defn compile-field-definition [field-def]
  (let [directives-by-name (into {} (map (juxt ::g/name compile-directive) (::g/directives field-def)))
        arguments-definition (mapv compile-argument-definition (::g/arguments-definition field-def))]
    (cond-> field-def
      (seq arguments-definition) (assoc ::g/arguments-definition arguments-definition)
      (seq directives-by-name) (assoc ::directives-by-name directives-by-name))))


(defn provide-types
  "Creates the schema's 'types-by-name' entry."
  [acc document]
  (reduce
   (fn [acc {::g/keys [name] :as td}]
     (assoc-in
      acc [::types-by-name name]
      (let [fields-by-name (into {} (map (juxt ::g/name compile-field-definition) (::g/field-definitions td)))
            directives-by-name (into {} (map (juxt ::g/name compile-directive) (::g/directives td)))
            enum-values-by-name (into {} (map (juxt ::g/name compile-argument-definition) (::g/enum-values td)))
            ]
        (cond-> td
          (seq fields-by-name) (assoc ::fields-by-name fields-by-name)
          (seq directives-by-name) (assoc ::directives-by-name directives-by-name)
          (seq enum-values-by-name) (assoc ::directives-by-name directives-by-name)))))
   acc
   (filter #(= (::g/definition-type %) :type-definition) document)))

(defn check-root-operation-type
  "Depends on validate-types."
  [acc _]
  (let [query-root-op-type-name (get-in acc [::root-operation-type-names :query])
        query-root-op-type (get-in acc [::types-by-name query-root-op-type-name])]
    (assert query-root-op-type-name)
    (cond
      (nil? query-root-op-type)
      (add-error acc
       {:message (format "The query root operation type must be provided: '%s'" query-root-op-type-name)})

      (not= 'OBJECT (get query-root-op-type ::g/kind))
      (add-error
       acc {:message "The query root operation type must be an Object type"})

      :else acc)))

(defn inject-introspection-fields [acc _]
  (let [query-root-op-type-name (get-in acc [::root-operation-type-names :query])
        query (get-in acc [::types-by-name query-root-op-type-name])
        __schema {::g/name "__schema"
                  ::g/type-ref {::g/non-null-type {::g/name "__Schema"}}}
        __type {::g/name "__type"
                ::g/type-ref {::g/name "__Type"}
                ::g/arguments-definition
                [{::g/name "name"
                  ::g/type-ref {::g/non-null-type {::g/name "String"}}}]}]
    (cond-> acc
      query
      (->
       (assoc-in
        [::types-by-name query-root-op-type-name ::fields-by-name "__type"] __type)
       (assoc-in
        [::types-by-name query-root-op-type-name ::fields-by-name "__schema"] __schema)))))

(defn check-schema-definition-count
  [acc document]
  (when (pos? (dec (count (filter #(= (::g/definition-type %) :schema-definition) document))))
    (add-error
     acc
     {:message "A document must include at most one schema definition"})))

(defn process-schema-definition [acc document]
  (let [schema-def (first (filter #(= (::g/definition-type %) :schema-definition) document))]
    (cond-> acc
      true
      (assoc
       ::root-operation-type-names
       (or
        (when schema-def (::g/operation-types schema-def))
        {:query "Query" :mutation "Mutation" :subscription "Subscription"}))
      (::g/directives schema-def)
      (assoc ::directives (into {} (map (juxt ::g/name identity) (::g/directives schema-def)))))))

(defn compile-base-schema [document]
  (reduce
   (fn [acc f]
     (or (f acc document) acc))
   {::errors []
    ::types-by-name {}}
   [provide-types
    check-unique-type-names
    check-unique-directive-names
    check-types
    process-schema-definition
    check-schema-definition-count]))

(defn schema-base []
  (compile-base-schema
   (parse (slurp (io/resource "juxt/grab/alpha/meta-schema.graphql")))))

;; Compiling a schema should be independent of any base

;; Break schema process into construction (build) and validation

;; WIP
(def build-functions
  [process-schema-definition
   provide-types
   inject-introspection-fields])

(def validation-functions
  [check-unique-type-names
   check-no-conflicts-with-built-in-types
   check-unique-directive-names
   check-reserved-names
   check-types
   check-schema-definition-count
   check-root-operation-type])

(defn apply-to-schema
  ([document base xs]
   (reduce
    (fn [acc f]
      (or (f acc document) acc))
    base xs))
  ([document xs]
   (reduce
    (fn [acc f]
      (or (f acc document) acc))
    document xs)))

(defn build-schema [document]
  (apply-to-schema document (schema-base) build-functions))

;; WIP
(defn validate-schema [schema]
  (apply-to-schema schema validation-functions))


(defn compile-schema*
  "Create a schema from the parsed document."
  ([document base]
   (apply-to-schema document base (into build-functions validation-functions)))
  ([document]
   (compile-schema* document (schema-base))))

(defn compile-schema [document]
  (let [result (compile-schema* document)]
    (when (seq (::errors result))
      (throw
       (ex-info
        "Failed to compile schema due to errors"
        {:errors (::errors result)})))
    result))



(defmacro flet [bindings & body]
  "Common Lisp's flet"
  `((fn [~@(map first (partition 2 bindings))] ~@body)
    ~@(map (fn [expr]
             `(fn [~@(first (second expr))]
                ~@(rest (second expr)))) (partition 2 bindings))))


(defn process-schema-extension [schema {::g/keys [directives operation-types]}]
  (flet [add-directives
         ([schema directives]
          (let [existing-directives
                (for [existing (some->> schema ::directives keys)
                      dir (map ::g/name directives)
                      :when (= existing dir)]
                  existing)]
            (if (seq existing-directives)
              (add-error
               schema
               {:message "Any directives provided must not already apply to the original Schema"
                :existing-directives existing-directives})
              (update schema ::directives merge (into {} (map (juxt ::g/name identity) directives))))))
         
         add-operation-types
         ([schema operation-types]
          (let [duplicates
                (set/intersection
                 (some-> schema ::root-operation-type-names keys set)
                 (some-> operation-types keys set))]
            (if (seq duplicates)
              (add-error
               schema
               {:message "Schema extension attempting to add root operation types that already exist"
                :duplicates duplicates})
              (update schema ::root-operation-type-names merge operation-types))))]
        
        (cond-> schema
          directives (add-directives directives)
          operation-types (add-operation-types operation-types))))

(defn build-object-type-extension [schema {::g/keys [name field-definitions directives interfaces]}]
  (let [built-schema
        (cond-> schema
          directives
          (update-in [::types-by-name name ::g/directives] into directives)
          interfaces
          (update-in [::types-by-name name ::g/interfaces] into (map ::g/name interfaces))
          field-definitions
          (update-in [::types-by-name name ::g/field-definitions] into field-definitions))
        
        leftover-interfaces
        (seq
         (filter (fn [interface]
                   (not (clojure.set/subset?
                         (set (map ::g/name (get-in built-schema [::types-by-name interface ::g/field-definitions])))
                         (set (map ::g/name (get-in built-schema [::types-by-name name ::g/field-definitions]))))))
                 (set (get-in built-schema [::types-by-name name ::g/interfaces]))))]
    (if leftover-interfaces
          (add-error built-schema
               {:message "The resulting extended object type must be a super‐set of all interfaces it implements."
                :problem-interfaces (vec leftover-interfaces)})
          built-schema)))

(defn process-object-type-extension [schema {::g/keys [name field-definitions directives interfaces] :as extension}]
  (let [existing-type (get (::types-by-name schema) name)
        duplicates (duplicates-by ::g/name field-definitions)
        pre-existing-fields
        (seq
         (for [i (->> existing-type ::g/field-definitions (map ::g/name))
               j (map ::g/name field-definitions)
               :when (= i j)] i))
        pre-existing-directives
        (seq
         (for [i (->> existing-type ::g/directives (map ::g/name))
               j (map ::g/name directives)
               :when (= i j)] i))
        pre-existing-interfaces
        (seq
         (for [i (::g/interfaces existing-type)
               j (map ::g/name interfaces)
               :when (= i j)] i))
        validated-schema
        (cond-> schema
          ;; 1. The named type must already be defined and must be an Object type.
          (nil? existing-type)
          (add-error
           {:message "The named type must already be defined and must be an Object type"
            :name name})

          ;; 2. The fields of an Object type extension must have unique names; no two fields may share the same name.
          duplicates
          (add-error
           {:message "The fields of an Object type extension must have unique names; no two fields may share the same name."
            :duplicates duplicates})

          ;; 3. Any fields of an Object type extension must not be already defined on the original Object type.
          pre-existing-fields
          (add-error
           {:message "Any fields of an Object type extension must not be already defined on the original Object type."
            :pre-existing-fields pre-existing-fields})

          ;; 4. Any directives provided must not already apply to the original Object type.
          pre-existing-directives
          (add-error
           {:message "Any directives provided must not already apply to the original Object type."
            :pre-existing-directives pre-existing-directives})

          ;; 5. Any interfaces provided must not be already implemented by the original Object type
          pre-existing-interfaces
          (add-error
           {:message "Any interfaces provided must not be already implemented by the original Object type."
            :pre-existing-interfaces pre-existing-interfaces}))]
    
    ;; 6. The resulting extended object type must be a super‐set of all interfaces it implements.
    (if (seq (::errors validated-schema))
      validated-schema
      (build-object-type-extension validated-schema extension))))


(defn build-interface-type-extension [schema {::g/keys [name directives field-definitions]}]
  (let [built-schema
        (cond-> schema
          directives
          (update-in [::types-by-name name ::g/directives] into directives)
          field-definitions
          (update-in [::types-by-name name ::g/field-definitions] into field-definitions))
        invalid-objects
        (seq
         (filter (fn [object]
                   (not (clojure.set/subset?
                         (set (map ::g/name field-definitions))
                         (set (map ::g/name (::g/field-definitions object))))))
                 (filter (fn [defined-type]
                           (and (= (::g/kind defined-type) 'OBJECT)
                                (some (fn [interface] (= name interface)) (::g/interfaces defined-type))))
                         (map second (vec (::types-by-name schema))))))]
    (if invalid-objects
      (add-error built-schema {:message "Any Object type which implemented the original Interface type must also be a super‐set of the fields of the Interface type extension which may be due to Object type extension."
                               :invalid-objects (map ::g/name invalid-objects)})
      built-schema)))

(defn process-interface-type-extension [schema {::g/keys [name directives field-definitions] :as extension}]
  (let [existing-type (get-in schema [::types-by-name name])
        duplicate-fields-in-extension (duplicates-by ::g/name field-definitions)
        duplicate-fields-on-object (when existing-type
                                     (clojure.set/intersection
                                      (->> field-definitions
                                           (map ::g/name)
                                           (set))
                                      (->> existing-type
                                           ::g/field-definitions
                                           (map ::g/name)
                                           (set))))
        duplicate-directives (when existing-type
                               (clojure.set/intersection
                                (->> directives
                                     (map ::g/name)
                                     (set))
                                (->> existing-type
                                     ::g/directives
                                     (map ::g/name)
                                     (set))))
        validated-schema
        (cond-> schema
          ;; 1. The named type must already be defined and must be an Interface type.
          (nil? existing-type)
          (add-error {:message "The named type must already be defined and must be an Interface type."
                      :extension-name name})
          (and existing-type (not= (::g/kind existing-type) 'INTERFACE))
          (add-error {:message "The named type must already be defined and must be an Interface type."
                      :extension-name name
                      :existing-type-kind (::g/kind existing-type)})
          ;; 2. The fields of an Interface type extension must have unique names; no two fields may share the same name.
          (seq duplicate-fields-in-extension)
          (add-error {:message "The fields of an Interface type extension must have unique names; no two fields may share the same name."
                      :duplicate-fields (vec duplicate-fields-in-extension)})
          ;; 3. Any fields of an Interface type extension must not be already defined on the original Interface type.
          (seq duplicate-fields-on-object)
          (add-error {:message "Any fields of an Interface type extension must not be already defined on the original Interface type."
                      :duplicate-fields (vec duplicate-fields-on-object)})
          ;; 5. Any directives provided must not already apply to the original Interface type.
          (seq duplicate-directives)
          (add-error {:message "Any directives provided must not already apply to the original Interface type."
                      :duplicate-directives (vec duplicate-directives)}))]
    (if (seq (::errors validated-schema))
      validated-schema
      (build-interface-type-extension validated-schema extension))))


(defn process-over-filter [predicate function]
  (fn [schema document]
    (reduce
     (fn [schema definition]
       (function schema definition))
     schema
     (filter predicate document))))


(def schema-extension-functions [(process-over-filter (fn [definition]
                                                        (= (::g/definition-type definition) :schema-extension))
                                                      process-schema-extension)
                                 (process-over-filter (fn [definition]
                                                        (= (::g/type-extension-type definition) :object-type-extension))
                                                      process-object-type-extension)
                                 (process-over-filter (fn [definition]
                                                        (= (::g/type-extension-type definition) :interface-type-extension))
                                                      process-interface-type-extension)
                                 ;; Tests mention that we are to leave out scalar extensions for now
                                 ;; 3.8.1 Union Extensions TODO
                                 ;; 3.9.1 Enum Extensions TODO
                                 ;; 3.10.1 Input Object Extensions TODO
                                 ])

(defn extend-schema
  "Extend a schema"
  [schema document]
  (assert (empty? (::errors schema)) "Cannot extend schema when there are pre-existing errors")
  (apply-to-schema document schema schema-extension-functions))
