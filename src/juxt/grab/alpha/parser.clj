;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.parser
  (:require
   [clj-antlr.core :as antlr]
   [clojure.java.io :as io]))

(defonce parser (antlr/parser (slurp (io/resource "GraphQL.g4"))))

;; TODO: Move entries produced by this ns from juxt.grab.alpha.graphql to
;; juxt.grab.alpha.parser
(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(defmulti process
  (fn [x]
    (cond
      (and (sequential? x) (keyword? (first x))) (first x)
      (or (keyword? x) (string? x)) :constant
      :else (throw (ex-info "FAIL" {:x x})))))

(defn process-child [x]
  (let [coords (-> x meta :clj-antlr/position)
        child (process x)]
    (cond-> child
      (and (instance? clojure.lang.IMeta child) coords)
      (with-meta {::g/location {:line (inc (:row coords))
                                :column (:column coords)
                                :index (:index coords)}}))))

(defmethod process :constant [_] nil)

(defmethod process :default [[k & vals]]
  {:not-handled! {:k k :vals vals}})

(defmethod process :document [[_ & definitions]]
  (vec (keep process-child definitions)))

(defmethod process :definition [[_ inner]]
  (process-child inner))

(defmethod process :executableDefinition [[_ inner]]
  (into
   {::g/definition-type :executable-definition}
   (process-child inner)))

(defmethod process :typeSystemDefinition [[_ inner]]
  (process-child inner))

(defmethod process :typeSystemExtension [[_ inner]]
  (process-child inner))

(defmethod process :typeExtension [[_ inner]]
  (process-child inner))

(defmethod process :objectTypeExtension [[_ & terms]]
  (->> terms
       (keep process-child)
       (apply merge)
       (into {::g/type-extension-type :object-type-extension})))

(defmethod process :typeDefinition [[_ inner]]
  (into
   {::g/definition-type :type-definition}
   (process-child inner)))

(defn trim-quotes [s]
  (cond
    (.startsWith s "\"\"\"")
    (subs s 3 (- (count s) 3))
    (.startsWith s "\"")
    (subs s 1 (- (count s) 1))))

(defmethod process :stringValue [[_ val]]
  (trim-quotes val))

(defmethod process :description [[_ inner]]
  {::g/description (process-child inner)})

(defmethod process :name [[_ val]]
  {::g/name val})

(defmethod process :value [[_ val]]
  {::g/value (process-child val)})

(defmethod process :intValue [[_ val]]
  (Integer/parseInt val))

(defmethod process :type_ [[_ val bang? :as args]]
  (-> {::g/type-ref (if (= bang? "!") {::g/non-null-type (process-child val)} (process-child val))
       }))

(defmethod process :namedType [[_ val]]
  (process-child val))

(defmethod process :listType [[_ _ inner-type _]]
  {::g/list-type (::g/type-ref (process-child inner-type))})

(defmethod process :arguments [[_ & terms]]
  {::g/arguments
   (->> terms
        (keep process-child)
        (into {}))})

(defmethod process :argument [[_ name _ value]]
  [(::g/name (process-child name))
   (::g/value (process-child value))])

(defmethod process :fieldDefinition [[_ & terms]]
  (-> terms
      (->>
       (keep process-child)
       (apply merge))))

(defmethod process :fieldsDefinition [[_ & terms]]
  {::g/field-definitions
   (->> terms
        (keep process-child)
        vec)})

(defmethod process :argumentsDefinition [[_ & terms]]
  {::g/arguments-definition
   (->> terms
        (keep process-child)
        (vec))})

(defmethod process :inputValueDefinition [[_ & terms]]
  (-> terms
      (->> (keep process-child)
           (apply merge))
      ;;(update ::g/type-ref get ::g/name)
      ))

(defmethod process :objectTypeDefinition [[_ & terms]]
  (let [res
        (->> terms
             (keep process-child)
             (apply merge)
             (into {::g/kind :object}))]
    (cond-> res
      (::g/interfaces res)
      (assoc
       ::g/interfaces
       (->> res
            ::g/interfaces
            (tree-seq coll? seq)
            (filter (fn [pair]
                      (and
                       (vector? pair)
                       (= (first pair) ::g/name))))
            (mapv second))))))

(defmethod process :scalarTypeDefinition [[_ & inner]]
  (->> inner
       (keep process-child)
       (apply merge)
       (into {::g/kind :scalar})))

(defmethod process :unionTypeDefinition [[_ & terms]]
  (->> terms
       (keep process-child)
       (apply merge)
       (into {::g/kind :union})))

(defmethod process :unionMemberTypes [[_ & terms]]
  {::g/member-types
   (mapv ::g/name (keep process-child terms))})

(defmethod process :enumTypeDefinition [[_ & terms]]
  (->> terms
       (keep process-child)
       (apply merge)
       (into {::g/kind :enum})))

(defmethod process :enumValuesDefinition [[_ & terms]]
  {::g/enum-values (vec (keep process-child terms))})

(defmethod process :enumValueDefinition [[_ & terms]]
  (->> terms (keep process-child) (apply merge)))

(defmethod process :enumValue [[_ & terms]]
  (->> terms (keep process-child) (apply merge)))

(defmethod process :interfaceTypeDefinition [[_ & terms]]
  (->> terms
       (keep process-child)
       (apply merge)
       (into {::g/kind :interface})))

(defmethod process :implementsInterfaces [[_ & terms]]
  {::g/interfaces
   (keep process-child terms)})

(defmethod process :directive [[_ & terms]]
  (->> terms
       (keep process-child)
       (apply merge)))

(defmethod process :directives [[_ & directives]]
  {::g/directives (keep process-child directives)})

(defmethod process :operationDefinition [[_ & terms]]
  {::g/operation-definition (apply merge (keep process-child terms))})

(defmethod process :selectionSet [[_ & selections]]
  {::g/selection-set
   (->> selections
        (keep process-child)
        vec)})

(defmethod process :fragmentDefinition [[_ & terms]]
  {::g/fragment-definition (apply merge (keep process-child terms))})

(defmethod process :typeCondition [[_ _ named-type]]
  {::g/type-condition (::g/name (process-child named-type))})

(defmethod process :fragmentSpread [[_ & terms]]
  (into
   {::g/selection-type :fragment-spread}
   (->> terms
        (keep process-child)
        (apply merge))))

(defmethod process :inlineFragment [[_ & terms]]
  (into
   {::g/selection-type :inline-fragment}
   (->> terms
        (keep process-child)
        (apply merge))))

(defmethod process :fragmentName [[_ name]]
  {::g/fragment-name (::g/name (process-child name))})

(defmethod process :selection [[_ inner]]
  (process-child inner))

(defmethod process :operationType [[_ val]]
  {::g/operation-type
   (case val
     "query" :query
     "mutation" :mutation
     "subscription" :subscription)})

(defmethod process :field [[_ & terms]]
  (into
   {::g/selection-type :field}
   (->> terms
        (keep process-child)
        (apply merge))))

(defmethod process :alias [[_ name]]
  {::g/alias (::g/name (process-child name))})

(defmethod process :schemaDefinition [[_ _ directives & terms]]
  (merge
   {::g/definition-type :schema-definition}
   (process-child directives)
   {::g/operation-types (apply merge (keep process-child terms))}))

(defmethod process :schemaExtension [[_ _ _ directives & terms]]
  (merge
   {::g/definition-type :schema-extension}
   (process-child directives)
   {::g/operation-types (apply merge (keep process-child terms))}))

(defmethod process :directiveDefinition [[_ & terms]]
  (into
   {::g/definition-type :directive-definition}
   (apply merge (keep process-child terms))))

(defmethod process :rootOperationTypeDefinition [[_ operation-type _ named-type]]
  {(::g/operation-type (process-child operation-type))
   (get-in (process-child named-type) [::g/name])})

(defmethod process :operationTypeDefinition [[_ operation-type _ named-type]]
  {(::g/operation-type (process-child operation-type))
   (get-in (process-child named-type) [::g/name])})

(defmethod process :inputObjectTypeDefinition [[_ & terms]]
  (->> terms
       (keep process-child)
       (apply merge)
       (into {::g/kind :input-object})))

(defmethod process :inputFieldsDefinition [[_ & terms]]
  (into {::g/input-values
         (->> terms
              (keep process-child)
              vec)}))

(defmethod process :defaultValue [[_ _ value]]
  {::g/default-value (process-child value)})

(defmethod process :booleanValue [[_ value]]
  (when-not (#{"true" "false"} value)
    (throw (ex-info "Boolean value must be true or false" {})))
  (Boolean/valueOf value))

(defmethod process :variable [[_ _ nm]]
  {::g/variable (::g/name (process-child nm))})

(defn parse* [s]
  (some-> s parser))

(defn parse [s]
  (some-> s parser process))
