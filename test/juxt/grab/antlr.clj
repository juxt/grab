;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.antlr
  (:require
   [clj-antlr.core :as antlr]
   [clojure.java.io :as io]))


(defmulti process first)

(defmethod process :default [[k & vals]]
  {:not-handled! {:k k :vals vals}})

(defmethod process :document [[_ & definitions]]
  (map process definitions))

(defmethod process :definition [[_ inner]]
  (process inner))

(defmethod process :typeSystemDefinition [[_ inner]]
  (process inner))

(defmethod process :typeDefinition [[_ inner]]
  (process inner))

(defmethod process :stringValue [[_ val]]
  val)

(defmethod process :description [[_ inner]]
  {:description (process inner)})

(defmethod process :name [[_ val]]
  {:name val})

(defmethod process :value [[_ val]]
  {:value (process val)})

(defmethod process :intValue [[_ val]]
  (Integer/parseInt val))

(defmethod process :type_ [[_ val]]
  {:type (process val)})

(defmethod process :namedType [[_ val]]
  {:named-type (process val)})

(defmethod process :arguments [[_ & terms]]
  (-> terms
      (->>
       (filter sequential?)
       (map process)
       (apply merge))))

(defmethod process :argument [[_ & terms]]
  {:argument
   (->> terms
        (filter sequential?)
        (map process)
        (apply merge))})

(defmethod process :fieldDefinition [[_ & terms]]
  (-> terms
      (->>
       (filter sequential?)
       (map process)
       (apply merge))
      ;; Update result to extract what we need
      (update :type get-in [:named-type :name])))

(defmethod process :fieldsDefinition [[ & terms]]
  {:field-definitions
   (into {}
         (map (juxt :name identity)
              (map process (filter sequential? terms))))})

(defmethod process :argumentsDefinition [[_ & terms]]
  {:arguments-definition
   (->> terms
        (filter sequential?)
        (map process))})

(defmethod process :inputValueDefinition [[_ & terms]]
  (-> terms
      (->> (filter sequential?)
           (map process)
           (apply merge)
           )
      (update :type get-in [:named-type :name])))

(defmethod process :objectTypeDefinition [[_ & inner]]
  (->> inner
       (filter sequential?)
       (map process)
       (apply merge)
       (into {:kind :object})))

(defmethod process :scalarTypeDefinition [[_ & inner]]
  (->> inner
       (filter sequential?)
       (map process)
       (apply merge)
       (into {:kind :scalar})))

(defmethod process :directive [[_ & terms]]
  (->> terms
       (filter sequential?)
       (map process)
       (apply merge)))

(defmethod process :directives [[_ & directives]]
  {:directives
   (->> directives
        (map process)
        (map (juxt :name identity))
        (into {}))})


(let [graphql-parser (antlr/parser (slurp (io/resource "GraphQL.g4")))
      document (graphql-parser (slurp (io/resource "juxt/grab/schema-3.graphql")))]

  (last (process document))

  )

"Url"
  {:juxt.grab.alpha.schema/kind :scalar,
   :juxt.grab.alpha.schema/name "Url",
   :juxt.grab.alpha.schema/field-definitions {}}

(comment
  "Person"
  {:juxt.grab.alpha.schema/kind :object,
   :juxt.grab.alpha.schema/name "Person",
   :juxt.grab.alpha.schema/field-definitions
   {"id"
    {:juxt.grab.alpha.schema/name "id", :juxt.grab.alpha.schema/type "Int"},
    "name"
    {:juxt.grab.alpha.schema/name "name",
     :juxt.grab.alpha.schema/type "String"},
    "profilePic"
    {:juxt.grab.alpha.schema/name "profilePic",
     :juxt.grab.alpha.schema/type "Url",
     :juxt.grab.alpha.schema/arguments-definition
     ({:juxt.grab.alpha.schema/name "size",
       :juxt.grab.alpha.schema/type "Int"})}}})
