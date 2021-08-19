;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.parser
  (:require
   [clj-antlr.core :as antlr]
   [clojure.java.io :as io]))

(defonce parser (antlr/parser (slurp (io/resource "GraphQL.g4"))))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(defmulti process
  (fn [x]
    (cond
      (and (sequential? x) (keyword? (first x))) (first x)
      (or (keyword? x) (string? x)) :constant
      :else (throw (ex-info "FAIL" {:x x})))))

(defmethod process :constant [_]
  nil)

(defmethod process :default [[k & vals]]
  {:not-handled! {:k k :vals vals}})

(defmethod process :document [[_ & definitions]]
  (vec (keep process definitions)))

(defmethod process :definition [[_ inner]]
  (process inner))

(defmethod process :executableDefinition [[_ inner]]
  (into
   {::g/definition-type :executable-definition}
   (process inner)))

(defmethod process :typeSystemDefinition [[_ inner]]
  (process inner))

(defmethod process :typeDefinition [[_ inner]]
  (into
   {::g/definition-type :type-definition}
   (process inner)))

(defmethod process :stringValue [[_ val]]
  val)

(defmethod process :description [[_ inner]]
  {::g/description (process inner)})

(defmethod process :name [[_ val]]
  {::g/name val})

(defmethod process :value [[_ val]]
  {::g/value (process val)})

(defmethod process :intValue [[_ val]]
  (Integer/parseInt val))

(defmethod process :type_ [[_ val]]
  {::g/type (process val)})

(defmethod process :namedType [[_ val]]
  {::g/named-type (process val)})

(defmethod process :arguments [[_ & terms]]
  {::g/arguments
   (->> terms
        (keep process)
        (into {}))})

(defmethod process :argument [[_ name _ value]]
  [(::g/name (process name))
   (::g/value (process value))])

(defmethod process :fieldDefinition [[_ & terms]]
  (-> terms
      (->>
       (keep process)
       (apply merge))
      ;; Update result to extract what we need
      (update ::g/type get-in [::g/named-type ::g/name])))

(defmethod process :fieldsDefinition [[_ & terms]]
  {::g/field-definitions
   (into {}
         (map (juxt ::g/name identity)
              (keep process terms)))})

(defmethod process :argumentsDefinition [[_ & terms]]
  {::g/arguments-definition
   (->> terms
        (keep process)
        (vec))})

(defmethod process :inputValueDefinition [[_ & terms]]
  (-> terms
      (->> (keep process)
           (apply merge))
      (update ::g/type get-in [::g/named-type ::g/name])))

(defmethod process :objectTypeDefinition [[_ & inner]]
  (->> inner
       (keep process)
       (apply merge)
       (into {::g/kind :object})))

(defmethod process :scalarTypeDefinition [[_ & inner]]
  (->> inner
       (keep process)
       (apply merge)
       (into {::g/kind :scalar})))

(defmethod process :unionTypeDefinition [[_ & terms]]
  (->> terms
       (keep process)
       (apply merge)
       (into {::g/kind :union})))

(defmethod process :unionMemberTypes [[_ & terms]]
  {::g/member-types
   (mapv #(get-in % [::g/named-type ::g/name]) (keep process terms))})

(defmethod process :enumTypeDefinition [[_ & terms]]
  (->> terms
       (keep process)
       (apply merge)
       (into {::g/kind :enum})))

(defmethod process :enumValuesDefinition [[_ & terms]]
  {::g/enum-values (vec (keep process terms))})

(defmethod process :enumValueDefinition [[_ & terms]]
  (->> terms (keep process) (apply merge)))

(defmethod process :enumValue [[_ & terms]]
  (->> terms (keep process) (apply merge)))

(defmethod process :interfaceTypeDefinition [[_ & terms]]
  (->> terms
       (keep process)
       (apply merge)
       (into {::g/kind :interface})))

(defmethod process :implementsInterfaces [[_ & terms]]
  {::g/interfaces
   (mapv #(get-in % [::g/named-type ::g/name]) (keep process terms))})

(defmethod process :directive [[_ & terms]]
  (->> terms
       (keep process)
       (apply merge)))

(defmethod process :directives [[_ & directives]]
  {::g/directives
   (->> directives
        (keep process)
        (map (juxt ::g/name identity))
        (into {}))})

(defmethod process :operationDefinition [[_ & terms]]
  {::g/operation-definition (apply merge (keep process terms))})

(defmethod process :selectionSet [[_ & selections]]
  {::g/selection-set
   (->> selections
        (keep process)
        vec)})

(defmethod process :fragmentDefinition [[_ & terms]]
  {::g/fragment-definition (apply merge (keep process terms))})

(defmethod process :typeCondition [[_ _ named-type]]
  {::g/type-condition (get-in (process named-type) [::g/named-type ::g/name])})

(defmethod process :inlineFragment [[_ & terms]]
  (into
   {::g/selection-type :inline-fragment}
   (->> terms
        (keep process)
        (apply merge))))

(defmethod process :fragmentName [[_ name]]
  {::g/fragment-name (::g/name (process name))})

(defmethod process :selection [[_ inner]]
  (process inner))

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
        (keep process)
        (apply merge))))

(defmethod process :alias [[_ name]]
  {::g/alias (::g/name (process name))})

(defmethod process :schemaDefinition [[_ & terms]]
  {::g/schema (apply merge (keep process terms))})

(defmethod process :rootOperationTypeDefinition [[_ operation-type _ named-type]]
  {(::g/operation-type (process operation-type))
   (get-in (process named-type) [::g/named-type ::g/name])})

(defn parse* [s]
  (-> s parser))

(defn parse [s]
  (-> s parser process))
