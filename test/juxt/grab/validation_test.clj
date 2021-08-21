;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.validation-test
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.parser :refer [parse]]
   [juxt.grab.alpha.document :as doc :refer [compile]]
   [juxt.grab.alpha.schema :refer [compile-schema]]
   [clojure.java.io :as io]))

(set! clojure.core/*print-namespace-maps* false)

(defn example [n]
  (-> (format "juxt/grab/example-%s.graphql" n)
      io/resource
      slurp
      parse))

(defn example-schema []
  (compile-schema
   (parse (slurp (io/resource "juxt/grab/example-90.graphql")))))

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
  (is (compile-schema (example "90"))))

(deftest
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  illegal-type-system-definition-test
  (-> "scalar Illegal"
      parse
      (compile {})
      (expected-errors [#"A document containing a type system definition or extension is invalid for execution"])))

(deftest example-91-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  (-> (example "91")
      (compile {})
      (expected-errors [#"A document containing a type system definition or extension is invalid for execution" nil])))

(deftest example-93-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Operation-Name-Uniqueness"}
  (is
   (compile
    (parse (slurp (io/resource "juxt/grab/example-92.graphql")))
    {}))
  (-> (example "93")
      (compile {})
      (expected-errors [nil nil #"Operation name '.+' is not unique"])))

(deftest example-94-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Operation-Name-Uniqueness"}
  (is
   (compile
    (parse (slurp (io/resource "juxt/grab/example-92.graphql")))
    {}))
  (-> (example "94")
      (compile {})
      (expected-errors [nil nil #"Operation name '.+' is not unique"])))

(deftest
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Lone-Anonymous-Operation"}
  example-95-test
  (is (compile (example "95") {})))

(deftest
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Lone-Anonymous-Operation"}
  example-96-test
  (-> (example "96")
      (compile {})
      (expected-errors [nil nil #"When there are multiple operations in the document, none can be anonymous"])))

;; TODO: 5.2.3 Subscription Operation Definitions
;; These are not yet covered, since subscriptions are not supported.

;; 5.3 Fields

(deftest field-name-not-defined-test
  (-> "query { dog { none }}"
      parse
      (compile (example-schema))
      (expected-errors [#"Field name '.+' not defined on type in scope '.+'"])))

(deftest example-102-test
  (-> (example "102")
      (compile (example-schema))
      (expected-errors (repeat 2 #"Field name '.+' not defined on type in scope '.+'"))))

(deftest example-103-test
  (-> (example "103")
      (compile (example-schema))
      (expected-errors [])))

(deftest example-104-test
  (-> (example "104")
      (compile (example-schema))
      (expected-errors [#"Field name 'nickname' not defined on type in scope 'Pet'"])))

;; TODO: Add test for example-105 when introspection is added

(deftest example-106-test
  (-> (example "106")
      (compile (example-schema))
      (expected-errors [#"Field name 'name' not defined on type in scope 'CatOrDog'"
                        #"Field name 'barkVolume' not defined on type in scope 'CatOrDog'"])))

;; 5.3.2 Field Selection Merging

(deftest field-merging-test
  (let [compilers [doc/add-fragments
                   doc/add-scoped-types-to-fragments
                   doc/validate-fields-in-set-can-merge]]

    (-> (example "102")
        (compile (example-schema) compilers)
        (expected-errors []))

    (-> (example "108")
        (compile (example-schema) compilers)
        (expected-errors [#"Cannot merge since field names are not identical"]))

    (-> (example "109")
        (compile (example-schema) compilers)
        (expected-errors []))

    (->
     (example "110")
     (compile (example-schema) compilers)
     (expected-errors (repeat 4 #"Cannot merge since field arguments are not identical")))))

(deftest example-111-test
  (-> (example "111")
      (compile (example-schema))
      (expected-errors [])))

(deftest example-112-test
  (-> (example "112")
      (compile (example-schema))
      (expected-errors [#"Fields have conflicting return types"])))

;; 5.3.3 Leaf Field Selections
