;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.parser
  (:require
   [clj-antlr.core :as antlr]
   [juxt.grab.alpha.execution :as execution]
   [clojure.java.io :as io]))

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
  (keep process definitions))

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

(defmethod process :operationDefinition [[_ operation-type & terms]]
  (into
   {::g/operation-type (process operation-type)}
   (apply merge (keep process terms))))

(defmethod process :selectionSet [[_ & selections]]
  {::g/selection-set
   (->> selections
        (keep process)
        vec)})

(defmethod process :selection [[_ inner]]
  (process inner))

(defmethod process :operationType [[_ val]]
  (case val
    "query" :query
    "mutation" :mutation
    "subscription" :subscription))

(defmethod process :field [[_ & terms]]
  (into
   {::g/selection-type :field}
   (->> terms
        (keep process)
        (apply merge))))

(defmethod process :document [[_ & definitions]]
  {::g/document (vec (keep process definitions))})

(defmethod process :schemaDefinition [[_ & terms]]
  {::g/schema (apply merge (keep process terms))})

(defmethod process :rootOperationTypeDefinition [[_ operation-type _ named-type]]
  {(process operation-type)
   (get-in (process named-type) [::g/named-type ::g/name])})


;; Note: wouldn't it be easier if we handled strings?

(let [graphql-parser (antlr/parser (slurp (io/resource "GraphQL.g4")))
      schema (process (graphql-parser (slurp (io/resource "juxt/grab/schema-3.graphql"))))
      document (process (graphql-parser (slurp (io/resource "juxt/grab/query-3.graphql"))))
      ]

  (let [document
        (-> document
            (assoc
             :juxt.grab.alpha.document/operations-by-name
             (->> document
                  ::g/document
                  (filter #(contains? % ::g/operation-type))
                  (map (juxt ::g/name identity))
                  (into {}))))

        schema (assoc schema
                      :juxt.grab.alpha.document/types-by-name
                      (->> schema
                           ::g/document
                           (filter #(= (::g/definition-type %) :type-definition))
                           (map (juxt ::g/name identity))
                           (into {"Int" {::g/name "Int"
                                         ::g/kind :scalar}
                                  "Float" {::g/name "Float"
                                           ::g/kind :scalar}
                                  "String" {::g/name "String"
                                            ::g/kind :scalar}
                                  "Boolean" {::g/name "Boolean"
                                             ::g/kind :scalar}
                                  "ID" {::g/name "ID"
                                        ::g/kind :scalar}}))
                      :juxt.grab.alpha.document/root-operation-type-names
                      (second (first (first (filter #(contains? % ::g/schema) (::g/document schema))))))]

    (execution/execute-request
     {:schema schema
      :document document
      :field-resolver
      (fn [args]
        (def args args)
        (condp =
            [(get-in args [:object-type ::g/name])
             (get-in args [:field-name])]
            ["Root" "user"]
            {:name "Isaac Newton"}

            ["Person" "name"]
            (get-in args [:object-value :name])

            ["Person" "profilePic"]
            (format "https://profile.juxt.site/pic-%d.png" (get-in args [:argument-values "size"]))

            (throw (ex-info "" args))))})))
