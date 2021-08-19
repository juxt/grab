;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.validation-test
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.parser :as parser]
   [juxt.grab.alpha.document :as doc :refer [compile]]
   [juxt.grab.alpha.schema :refer [compile-schema]]
   [clojure.java.io :as io]))

(set! clojure.core/*print-namespace-maps* false)

(defn expected-errors [{::doc/keys [errors]} regexes]
  (assert errors)
  (is
   (= (count errors) (count regexes))
   "Count of errors doesn't equal expected count")
  (doall
   (map
    (fn [error regex]
      (when regex
        (is (re-matches regex (:error error)))))
    errors regexes)))

(deftest schema-parsing-test
  (is
   (compile-schema
    (parser/parse (slurp (io/resource "juxt/grab/example-90.graphql"))))))

(deftest
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  illegal-type-system-definition-test
  (-> "scalar Illegal"
      parser/parse
      (compile {})
      (expected-errors [#"A document containing a type system definition or extension is invalid for execution"])))

(deftest illegal-type-extension-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  (-> "juxt/grab/example-91.graphql"
      io/resource slurp parser/parse (compile {})
      (expected-errors [#"A document containing a type system definition or extension is invalid for execution" nil])))

(deftest operation-name-uniqueness-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Operation-Name-Uniqueness"}
  (is
   (compile
    (parser/parse (slurp (io/resource "juxt/grab/example-92.graphql")))
    {}))
  (-> "juxt/grab/example-93.graphql"
      io/resource
      slurp
      parser/parse
      (compile {})
      (expected-errors [nil nil #"Operation name '.+' is not unique"]))
  (-> "juxt/grab/example-94.graphql"
      io/resource
      slurp
      parser/parse
      (compile {})
      (expected-errors [nil nil #"Operation name '.+' is not unique"])))

(deftest
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Lone-Anonymous-Operation"}
  lone-operation-test
  (is
   (compile
    (parser/parse (slurp (io/resource "juxt/grab/example-95.graphql")))
    {}))
  (-> "juxt/grab/example-96.graphql"
      io/resource
      slurp
      parser/parse
      (compile {})
      (expected-errors [nil nil #"When there are multiple operations in the document, none can be anonymous"])))

;; TODO: 5.2.3 Subscription Operation Definitions
;; These are not yet covered, since subscriptions are not supported.

;; 5.3 Fields

(deftest
  field-name-not-defined-test
  (let [schema
        (compile-schema
         (parser/parse (slurp (io/resource "juxt/grab/example-90.graphql"))))]
    (-> "query { dog { none }}"
        parser/parse
        (compile schema)
        (expected-errors [#"Field name '.+' not defined on type in scope '.+'"]))))

(deftest
  field-name-not-defined-fragment-test
  (let [schema
        (compile-schema
         (parser/parse (slurp (io/resource "juxt/grab/example-90.graphql"))))]
    (-> "juxt/grab/example-102.graphql"
        io/resource
        slurp
        parser/parse
        (compile schema)
        (expected-errors (repeat 2 #"Field name '.+' not defined on type in scope '.+'")))))

(deftest interface-field-selection-test
  (let [schema
        (compile-schema
         (parser/parse (slurp (io/resource "juxt/grab/example-90.graphql"))))]
    (-> "juxt/grab/example-103.graphql"
        io/resource
        slurp
        parser/parse
        (compile schema)
        (expected-errors []))))

(deftest defined-on-implementors-but-not-interface-test
  (let [schema
        (compile-schema
         (parser/parse (slurp (io/resource "juxt/grab/example-90.graphql"))))]
    (-> "juxt/grab/example-104.graphql"
        io/resource
        slurp
        parser/parse
        (compile schema)
        (expected-errors [#"Field name 'nickname' not defined on type in scope 'Pet'"]))))

;; TODO: Add test for example-105 when introspection is added

(deftest direct-field-selection-on-union
  (let [schema
        (compile-schema
         (parser/parse (slurp (io/resource "juxt/grab/example-90.graphql"))))]
    (-> "juxt/grab/example-106.graphql"
        io/resource
        slurp
        parser/parse
        (compile schema)
        (expected-errors [#"Field name 'name' not defined on type in scope 'CatOrDog'"
                          #"Field name 'barkVolume' not defined on type in scope 'CatOrDog'"]))))

;; 5.3.2 Field Selection Merging

(deftest field-merging-test
  (let [schema
        (compile-schema
         (parser/parse (slurp (io/resource "juxt/grab/example-90.graphql"))))
        compile (fn [res]
                  (->
                   res
                   io/resource
                   slurp
                   parser/parse
                   (doc/compile*
                    schema
                    [doc/add-fragments
                     doc/add-scoped-types-to-fragments
                     doc/validate-fields-in-set-can-merge])))]
    (->
     (compile "juxt/grab/example-102.graphql")
     (expected-errors []))

    (->
     (compile "juxt/grab/example-108.graphql")
     (expected-errors [#"Cannot merge since field names are not identical"]))

    (->
     (compile "juxt/grab/example-109.graphql")
     (expected-errors []))

    (->
     (compile "juxt/grab/example-110.graphql")
     (expected-errors (repeat 4 #"Cannot merge since field arguments are not identical")))))

(deftest safe-differing-fields-or-args-test
  (let [schema
        (compile-schema
         (parser/parse (slurp (io/resource "juxt/grab/example-90.graphql"))))]
    (-> "juxt/grab/example-111.graphql"
        io/resource
        slurp
        parser/parse
        (compile schema)
        (expected-errors []))))
