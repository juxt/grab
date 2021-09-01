;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.schema-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.schema :refer [compile-schema] :as s]
   [juxt.grab.alpha.parser :refer [parse parse*]]
   [juxt.grab.validation-test :refer [example]]
   [clojure.java.io :as io]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(defn expected-errors [{::s/keys [errors]} regexes]
  (assert errors)
  (is
   (= (count errors) (count regexes))
   "Count of errors doesn't equal expected count")
  (doall
   (map
    (fn [error regex]
      (is (:error error))
      (when regex
        (is (re-matches regex (:error error)))))
    errors regexes)))

;; https://spec.graphql.org/June2018/#sec-Schema

;; "All types within a GraphQL schema must have unique names."

(deftest duplicate-type-names-test
  (-> "type Query { name: String } type Query { name: String }"
      parse
      compile-schema
      (expected-errors [#"All types within a GraphQL schema must have unique names"])))

;; "No provided type may have a name which conflicts with any built in types
;; (including Scalar and Introspection types)."

(deftest type-conflicts-test
  (-> "type String { length: Int }"
      parse
      compile-schema
      (expected-errors [#"No provided type may have a name which conflicts with any built in types" nil])))

;; "All directives within a GraphQL schema must have unique names."

(deftest directive-conflicts-test
  (-> "directive @foo on FIELD directive @foo on OBJECT"
      parse
      compile-schema
      (expected-errors [#"All directives within a GraphQL schema must have unique names"
                        nil])))

;; "All types and directives defined within a schema must not have a name which
;; begins with '__' (two underscores), as this is used exclusively by GraphQL’s
;; introspection system."

(deftest reserved-names-test
  (-> "type __foo { length: Int }"
      parse
      compile-schema
      (expected-errors [#"All types and directives defined within a schema must not have a name.+"
                        #"The query root operation type must be provided: '\p{Alpha}+'"])))

;; "The query root operation type must be provided and must be an Object type."

(deftest query-root-type-not-provided-test
  (-> "schema { query: MyQueryRootType }"
      parse
      compile-schema
      (expected-errors [#"The query root operation type must be provided: '\p{Alpha}+'"])))

(deftest query-root-type-not-object-type-test
  (-> "schema { query: MyQueryRootType } scalar MyQueryRootType"
      parse
      compile-schema
      (expected-errors [#"The query root operation type must be an Object type"])))

(deftest root-operation-type-test
  (let [s (-> (example "37")
              compile-schema)]
    (is (= "MyQueryRootType" (get-in s [::s/root-operation-type-names :query])))
    (is (= :object (get-in s [::s/provided-types "MyQueryRootType" ::g/kind])))))

;; "When using the type system definition language, a document must include at most one schema definition."

(deftest multiple-schema-definitions-test
  (-> "schema { query: MyQueryRootType } schema { query: MyQueryRootType } type MyQueryRootType { someField: String } "
      parse
      compile-schema
      (expected-errors [#"A document must include at most one schema definition"])))

(deftest schema-with-default-query-no-schema-definition-test
  (-> (example "38")
      compile-schema
      (expected-errors [])))

(deftest schema-extension-test
  (let [schema
        (-> "schema @foo { query: MyQueryRootType } type MyQueryRootType { someField: String } "
            parse
            compile-schema)]
    (-> schema
        (s/extend-schema (parse "extend schema { mutation: MyMutationRootType }"))
        (expected-errors []))
    (-> schema
        (s/extend-schema (parse "extend schema { query: MyMutationRootType }"))
        (expected-errors [#"Schema extension attempting to add root operation types that already exist"]))
    (-> schema
        (s/extend-schema (parse "extend schema @bar"))
        (expected-errors []))
    (-> schema
        (s/extend-schema (parse "extend schema @foo"))
        (expected-errors [#"Any directives provided must not already apply to the original Schema"]))))


(deftest example-40-test
  (-> "juxt/grab/example-40.graphql"
      io/resource
      slurp
      ;; To help this compile
      (str " type Query { someField: String }")
      parse
      compile-schema
      (expected-errors [])))

;; We skip scalar extensions for now. See
;; https://github.com/antlr/grammars-v4/pull/2299 (TODO)

;; https://spec.graphql.org/June2018/#sec-Objects

;; 2.1. The field must have a unique name within that Object type; no two fields may share the same name.

;; Should error because someField is here twice

(deftest unique-field-names-test
  (-> "type Query { someField: String someField: String }"
      parse
      compile-schema
      (expected-errors [#"Each field must have a unique name within the '.+' Object type; no two fields may share the same name."])))

;; 2.2. The field must not have a name which begins with the characters "__" (two underscores).

(deftest reserved-field-names-test
  (-> "type Query { __someField: String }"
      parse
      compile-schema
      (expected-errors [#"A field must not have a name which begins with two underscores."])))

;; 2.3. The field must return a type where IsOutputType(fieldType) returns true.

(deftest output-type-field-test

  (-> "type Query { someField: Int }"
      parse
      compile-schema
      (expected-errors []))

  (-> "type Query { someField: [Int]! }"
      parse
      compile-schema
      (expected-errors []))

  (-> "type Query { someField: [Int]! }"
      parse
      compile-schema
      (expected-errors []))

  (-> "type Query { someField: Point2D } input Point2D { x: Float y: Float }"
      parse
      compile-schema
      (expected-errors [#"A field must return a type that is an output type"])))

;; 2.4. For each argument of the field:

(deftest field-arguments-test
  (-> "type Query { someField(x: Int y: Int): String } "
      parse
      compile-schema
      (expected-errors []))

  ;; 2.4.1. The argument must not have a name which begins with the characters "__" (two underscores).

  (-> "type Query { someField(__x: Int y: Int): String } "
      parse
      compile-schema
      (expected-errors [#"A field argument must not have a name which begins with two underscores."]))

  (-> "type Query { someField(p: Point2D): String } input Point2D { x: Float y: Float }"
      parse
      compile-schema
      (expected-errors []))

  ;; 2.4.2. The argument must accept a type where IsInputType(argumentType) returns true.

  (-> "type Query { someField(p: Point2D): String } type Point2D { x: Float y: Float }"
      parse
      compile-schema
      (expected-errors [#"A field argument must accept a type that is an input type"])))

;; 3. An object type may declare that it implements one or more unique interfaces.

(deftest unique-interfaces-test
  (-> "type Query implements Foo & Foo { a: String }"
      parse
      compile-schema
      (expected-errors [#"An object type may declare that it implements one or more unique interfaces. Declared interfaces contain duplicates."])))
