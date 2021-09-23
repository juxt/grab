;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.schema-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.schema :refer [compile-schema*] :as s]
   [juxt.grab.alpha.parser :refer [parse parse*]]
   [juxt.grab.validation-test :refer [example example-schema]]
   [clojure.java.io :as io]
   [juxt.grab.alpha.schema :as schema]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(set! clojure.core/*print-namespace-maps* false)

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
      compile-schema*
      (expected-errors [#"All types within a GraphQL schema must have unique names."])))

;; "No provided type may have a name which conflicts with any built in types
;; (including Scalar and Introspection types)."

(deftest type-conflicts-test
  (-> "type String { length: Int }"
      parse
      compile-schema*
      (expected-errors [#"No provided type may have a name which conflicts with any built in types." nil])))

;; "All directives within a GraphQL schema must have unique names."

(deftest directive-conflicts-test
  (-> "directive @foo on FIELD directive @foo on OBJECT"
      parse
      compile-schema*
      (expected-errors [#"All directives within a GraphQL schema must have unique names."
                        nil])))

;; "All types and directives defined within a schema must not have a name which
;; begins with '__' (two underscores), as this is used exclusively by GraphQL’s
;; introspection system."

(deftest reserved-names-test
  (-> "type __foo { length: Int }"
      parse
      compile-schema*
      (expected-errors [#"All types and directives defined within a schema must not have a name.+"
                        #"The query root operation type must be provided: '\p{Alpha}+'"])))

;; "The query root operation type must be provided and must be an Object type."

(deftest query-root-type-not-provided-test
  (-> "schema { query: MyQueryRootType }"
      parse
      compile-schema*
      (expected-errors [#"The query root operation type must be provided: '\p{Alpha}+'"])))

(deftest query-root-type-not-object-type-test
  (-> "schema { query: MyQueryRootType } scalar MyQueryRootType"
      parse
      compile-schema*
      (expected-errors [#"The query root operation type must be an Object type"])))

(deftest root-operation-type-test
  (let [s (-> (example "37")
              compile-schema*)]
    (is (= "MyQueryRootType" (get-in s [::s/root-operation-type-names :query])))
    (is (= :object (get-in s [::s/provided-types "MyQueryRootType" ::g/kind])))))

;; "When using the type system definition language, a document must include at most one schema definition."

(deftest multiple-schema-definitions-test
  (-> "schema { query: MyQueryRootType } schema { query: MyQueryRootType } type MyQueryRootType { someField: String } "
      parse
      compile-schema*
      (expected-errors [#"A document must include at most one schema definition"])))

(deftest schema-with-default-query-no-schema-definition-test
  (-> (example "38")
      compile-schema*
      (expected-errors [])))

(deftest schema-extension-test
  (let [schema
        (-> "schema @foo { query: MyQueryRootType } type MyQueryRootType { someField: String } "
            parse
            compile-schema*)]
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
      compile-schema*
      (expected-errors [])))

;; We skip scalar extensions for now. See
;; https://github.com/antlr/grammars-v4/pull/2299 (TODO)

;; https://spec.graphql.org/June2018/#sec-Objects

;; 2.1  The field must have a unique name within that Object type; no two fields may share the same name.

;; Should error because someField is here twice

(deftest unique-field-names-test
  (-> "type Query { someField: String someField: String }"
      parse
      compile-schema*
      (expected-errors [#"Each field must have a unique name within the '.+' Object type; no two fields may share the same name."])))

;; 2.2  The field must not have a name which begins with the characters "__" (two underscores).

(deftest reserved-field-names-test
  (-> "type Query { __someField: String }"
      parse
      compile-schema*
      (expected-errors [#"A field must not have a name which begins with two underscores."])))

;; 2.3  The field must return a type where IsOutputType(fieldType) returns true.

(deftest output-type-field-test

  (-> "type Query { someField: Int }"
      parse
      compile-schema*
      (expected-errors []))

  (-> "type Query { someField: [Int]! }"
      parse
      compile-schema*
      (expected-errors []))

  (-> "type Query { someField: [Int]! }"
      parse
      compile-schema*
      (expected-errors []))

  (-> "type Query { someField: Point2D } input Point2D { x: Float y: Float }"
      parse
      compile-schema*
      (expected-errors [#"A field must return a type that is an output type."])))

;; 2.4  For each argument of the field:

(deftest field-arguments-test
  (-> "type Query { someField(x: Int y: Int): String } "
      parse
      compile-schema*
      (expected-errors []))

  ;; 2.4.1  The argument must not have a name which begins with the characters "__" (two underscores).

  (-> "type Query { someField(__x: Int y: Int): String } "
      parse
      compile-schema*
      (expected-errors [#"A field argument must not have a name which begins with two underscores."]))

  (-> "type Query { someField(p: Point2D): String } input Point2D { x: Float y: Float }"
      parse
      compile-schema*
      (expected-errors []))

  ;; 2.4.2  The argument must accept a type where IsInputType(argumentType) returns true.

  (-> "type Query { someField(p: Point2D): String } type Point2D { x: Float y: Float }"
      parse
      compile-schema*
      (expected-errors [#"A field argument must accept a type that is an input type."])))

;; 3  An object type may declare that it implements one or more unique interfaces.

(deftest unique-interfaces-test
  (-> "type Query implements Foo & Foo { a: String }"
      parse
      compile-schema*
      (expected-errors [#"An object type may declare that it implements one or more unique interfaces. Interfaces declaration contains duplicates." nil])))

;; 4  An object type must be a super‐set of all interfaces it implements:

;; 4.1  The object type must include a field of the same name for every field
;; defined in an interface.

;; 4.1.1  The object field must be of a type which is equal to or a sub‐type of
;; the interface field (covariant).

;; 4.1.1.1  An object field type is a valid sub‐type if it is equal to (the same
;; type as) the interface field type.

(deftest interface-fields-inclusion-test
  (-> "juxt/grab/example-62.graphql"
      (io/resource)
      (slurp)
      (str " type BadBusiness implements NamedEntity & ValuedEntity { foo: String }")
      (str " type Query { business: Business }")
      parse
      compile-schema*
      (expected-errors
       [#"The object type must include a field of the same name for every field defined in an interface."
        #"The object type must include a field of the same name for every field defined in an interface."])))

;; 4.1.1.2  An object field type is a valid sub‐type if it is an Object type and
;; the interface field type is either an Interface type or a Union type and the
;; object field type is a possible type of the interface field type.

(deftest covariant-interface-or-union-test
  (-> (str " interface Address { postcode: String }")
      (str " type BusinessAddress implements Address { postcode: String }")
      (str " interface NamedEntity { address: Address }")
      (str " type Business implements NamedEntity { address: BusinessAddress }")
      (str " type Query { business: Business }")
      parse
      compile-schema*
      (expected-errors [])))

;; 4.1.1.3  An object field type is a valid sub‐type if it is a List type and
;; the interface field type is also a List type and the list‐item type of the
;; object field type is a valid sub‐type of the list‐item type of the interface
;; field type.

(deftest covariant-interface-or-union-list-test
  (-> (str " interface Address { postcode: String }")
      (str " type BusinessAddress implements Address { postcode: String }")
      (str " interface NamedEntity { address: [Address] }")
      (str " type Business implements NamedEntity { address: [BusinessAddress] }")
      (str " type Query { business: Business }")
      parse
      compile-schema*
      (expected-errors [])))

;; 4.1.1.4  An object field type is a valid sub‐type if it is a Non‐Null variant
;; of a valid sub‐type of the interface field type.

(deftest covariant-non-null-variant-test
  (-> (str " interface Address { postcode: String }")
      (str " type BusinessAddress implements Address { postcode: String }")
      (str " interface NamedEntity { address: Address }")
      (str " type Business implements NamedEntity { address: BusinessAddress! }")
      (str " type Query { business: Business }")
      parse
      compile-schema*
      (expected-errors [])))

;; 4.1.2  The object field must include an argument of the same name for every argument
;; defined in the interface field
(deftest object-interface-arguments-test
  (-> (str " interface NamedEntity { address(t: String): String }")
      (str " type Business implements NamedEntity { address: String }")
      (str " type Query { business: Business }")
      parse
      compile-schema*
      (expected-errors [#"The object field must include an argument of the same name for every argument defined in the interface field."]))

  ;; 4.1.2.1  The object field argument must accept the same type (invariant) as the
  ;; interface field argument.
  (-> (str " interface NamedEntity { address(f: [Int]): String }")
      (str " type Business implements NamedEntity { address(f: [String], g: Int): String }")
      (str " type Query { business: Business }")
      parse
      compile-schema*
      (expected-errors [#"The object field argument must accept the same type \(invariant\) as the interface field argument."]))

  ;; 4.1.3  The object field may include additional arguments not defined in the
  ;; interface field, but any additional argument must not be required, e.g. must
  ;; not be of a non‐nullable type.
  (-> (str " interface NamedEntity { address(f: [Int]): String }")
      (str " type Business implements NamedEntity { address(f: [Int], g: Int!): String }")
      (str " type Query { business: Business }")
      parse
      compile-schema*
      (expected-errors [#"The object field may include additional arguments not defined in the interface field, but any additional argument must not be required, e.g. must not be of a non‐nullable type."])))

;; 3.6.3  Object Extensions
(deftest object-extension-test
  (-> (example-schema)
      (schema/extend-schema
       (parse "extend type Goat { eats: String! }"))
      (expected-errors [#"The named type must already be defined and must be an Object type"]))
  (-> (example-schema)
      (schema/extend-schema
       (parse "extend type Dog { eats: String eats: Int }"))
      (expected-errors [#"The fields of an Object type extension must have unique names; no two fields may share the same name."]))
  (-> (example-schema)
      (schema/extend-schema
       (parse "extend type Dog { name: String }"))
      (expected-errors [#"Any fields of an Object type extension must not be already defined on the original Object type."]))
  (->
   (parse "type Query @foo { name: String }")
   (compile-schema*)
   (schema/extend-schema (parse "extend type Query @foo"))
   (expected-errors [#"Any directives provided must not already apply to the original Object type."]))
  ;; 5. Any interfaces provided must not be already implemented by the original Object type. (TODO)
  ;; 6. The resulting extended object type must be a super‐set of all interfaces it implements. (TODO)
  )

;; 3.7  Interfaces

;; Type validation

;; 1  An Interface type must define one or more fields.

(deftest interface-type-validate-test
  ;; 2  For each field of an Interface type:
  ;; 2.1  The field must have a unique name within that Interface type; no two fields may share the same name.
  (-> (str " interface NamedEntity { address: String address: String postcode: String }")
      (str " type Query { business: NamedEntity }")
      parse
      compile-schema*
      (expected-errors [#"Each field must have a unique name within the 'NamedEntity' Object type; no two fields may share the same name."]))

  ;; 2.2  The field must not have a name which begins with the characters "__" (two underscores).
  (-> (str " interface NamedEntity { __address: String postcode: String }")
      (str " type Query { business: NamedEntity }")
      parse
      compile-schema*
      (expected-errors [#"A field must not have a name which begins with two underscores."]))

  ;; 2.3  The field must return a type where IsOutputType(fieldType) returns true.
  (-> (str " input Point2D { x: Float y: Float }")
      (str " interface NamedEntity { location: Point2D }")
      (str " type Query { business: NamedEntity }")
      parse
      compile-schema*
      (expected-errors [#"A field must return a type that is an output type."]))

  ;; 2.4  For each argument of the field:
  ;; 2.4.1  The argument must not have a name which begins with the characters "__" (two underscores).
  (-> (str " interface NamedEntity { address(__t: String): String }")
      (str " type Query { business: NamedEntity }")
      parse
      compile-schema*
      (expected-errors [#"A field argument must not have a name which begins with two underscores."]))

  ;; 2.4.2  The argument must accept a type where IsInputType(argumentType) returns true.
  (-> (str " type Point2D { x: Float y: Float }")
      (str " interface NamedEntity { address(t: Point2D): String }")
      (str " type Query { business: NamedEntity }")
      parse
      compile-schema*
      (expected-errors [#"A field argument must accept a type that is an input type."])))
