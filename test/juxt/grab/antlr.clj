;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.antlr
  (:require
   [clj-antlr.core :as antlr]
   [clojure.java.io :as io]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(defmulti process
  (fn [[fst]]
    (cond (keyword? fst) fst
          :else :constant)))

(defmethod process :constant [[token]]
  nil)

(defmethod process :default [[k & vals]]
  {:not-handled! {:k k :vals vals}})

(defmethod process :document [[_ & definitions]]
  (keep process definitions))

(defmethod process :definition [[_ inner]]
  (process inner))

(defmethod process :executableDefinition [[_ inner]]
  (process inner))

(defmethod process :typeSystemDefinition [[_ inner]]
  (process inner))

(defmethod process :typeDefinition [[_ inner]]
  (process inner))

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
  (-> terms
      (->>
       (keep process)
       (apply merge))))

(defmethod process :argument [[_ & terms]]
  {::g/argument
   (->> terms
        (keep process)
        (apply merge))})

(defmethod process :fieldDefinition [[_ & terms]]
  (-> terms
      (->>
       (keep process)
       (apply merge))
      ;; Update result to extract what we need
      (update ::g/type get-in [::g/named-type ::g/name])))

(defmethod process :fieldsDefinition [[ & terms]]
  {::g/field-definitions
   (into {}
         (map (juxt ::g/name identity)
              (keep process terms)))})

(defmethod process :argumentsDefinition [[_ & terms]]
  {::g/arguments-definition
   (->> terms
        (keep process))})

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

(defmethod process :operationDefinition [[_ & terms]]
  (->> terms
        (keep process)
        (apply merge)))

(defmethod process :selectionSet [[_ & selections]]
  {::g/selection-set
   (->> selections
        (keep process)
        vec)})

(defmethod process :selection [[_ inner]]
  (process inner))

(defmethod process :operationType [[_ val]]
  {::g/operation-type
   (case val
     "query" :query
     "mutation" :mutation
     "subscription" :subscription)})

(defmethod process :field [[_ & terms]]
  {::g/selection-type :field
   ::g/field (->> terms
                  (keep process)
                  (apply merge))})

;; Note: wouldn't it be easier if we handled strings?

(let [graphql-parser (antlr/parser (slurp (io/resource "GraphQL.g4")))
      ;;document (graphql-parser (slurp (io/resource "juxt/grab/schema-3.graphql")))
      document (graphql-parser (slurp (io/resource "juxt/grab/query-3.graphql")))
      ]

  (process document)

  )
