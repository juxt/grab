;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.document-validation-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.parser :refer [parse]]
   [juxt.grab.alpha.document :as doc :refer [compile-document validate-document]]
   [juxt.grab.alpha.schema :refer [compile-schema extend-schema]]
   [clojure.java.io :as io]))

(set! clojure.core/*print-namespace-maps* false)

(defn example [n]
  (-> (format "juxt/grab/examples/example-%s.graphql" n)
      io/resource
      slurp
      parse))

(defn example-schema []
  (compile-schema
   (parse (slurp (io/resource "juxt/grab/examples/example-90.graphql")))))

(defn expected-errors [doc regexes]
  (let [errors (validate-document doc)]
    (is
     (= (count errors) (count regexes))
     "Count of errors doesn't equal expected count")
    (doall
     (map
      (fn [error regex]
        (when regex
          (is (re-matches regex (::doc/message error)))))
      errors regexes))))

(deftest schema-parsing-test
  (is (compile-schema (example "90"))))

#_(deftest
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  illegal-type-system-definition-test
  (-> "scalar Illegal"
      parse
      (compile-document {})
      (expected-errors [#"A document containing a type system definition or extension is invalid for execution"])))

#_(deftest example-91-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  (-> (example "91")
      (compile-document {})
      (expected-errors [#"A document containing a type system definition or extension is invalid for execution" nil])))

#_(deftest example-93-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Operation-Name-Uniqueness"}
  (is
   (compile-document
    (parse (slurp (io/resource "juxt/grab/examples/example-92.graphql")))
    {}))
  (-> (example "93")
      (compile-document {})
      (expected-errors [nil nil #"Operation name '.+' is not unique"])))

#_(deftest example-94-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Operation-Name-Uniqueness"}
  (is
   (compile-document
    (parse (slurp (io/resource "juxt/grab/examples/example-92.graphql")))
    {}))
  (-> (example "94")
      (compile-document {})
      (expected-errors [nil nil #"Operation name '.+' is not unique"])))

#_(deftest
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Lone-Anonymous-Operation"}
  example-95-test
  (is (compile-document (example "95") {})))

#_(deftest
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Lone-Anonymous-Operation"}
  example-96-test
  (-> (example "96")
      (compile-document {})
      (expected-errors [nil nil #"When there are multiple operations in the document, none can be anonymous"])))

;; TODO: 5.2.3 Subscription Operation Definitions
;; These are not yet covered, since subscriptions are not supported.

;; 5.3 Fields

#_(deftest field-name-not-defined-test
  (-> "query { dog { none }}"
      parse
      (compile-document (example-schema))
      (expected-errors [#"Field name '.+' not defined on type in scope '.+'"])))

(deftest example-102-test
  (-> (example "102")
      (compile-document (example-schema))
      (expected-errors (repeat 2 #"Field name '.+' not defined on type in scope '.+'"))))

(deftest example-103-test
  (-> (example "103")
      (compile-document (example-schema))
      (expected-errors [])))

(deftest example-104-test
  (-> (example "104")
      (compile-document (example-schema))
      (expected-errors [#"Field name 'nickname' not defined on type in scope 'Pet'"])))

;; TODO: Add test for example-105 when introspection is added

(deftest example-106-test
  (-> (example "106")
      (compile-document (example-schema))
      (expected-errors [#"Field name 'name' not defined on type in scope 'CatOrDog'"
                        #"Field name 'barkVolume' not defined on type in scope 'CatOrDog'"])))

;; 5.3.2 Field Selection Merging

(-> (example "108")
    (compile-document (example-schema))
    )

#_(deftest field-merging-test
  (let [compilers {:compilers
                   [doc/add-fragments
                    doc/add-scoped-types-to-fragments
                    doc/validate-fields-in-set-can-merge]}]

    #_(-> (example "102")
        (compile-document (example-schema) compilers)
        (expected-errors []))

    (-> (example "108")
        (compile-document (example-schema) compilers)
        (expected-errors [#"Fields have conflicting return types"
                          #"Cannot merge since field names are not identical"]))

    (-> (example "109")
        (compile-document (example-schema) compilers)
        (expected-errors []))

    (->
     (example "110")
     (compile-document (example-schema) compilers)
     (expected-errors (repeat 4 #"Cannot merge since field arguments are not identical")))))

(deftest example-111-test
  (-> (example "111")
      (compile-document (example-schema))
      (expected-errors [])))

#_(deftest example-112-test
  (-> (example "112")
      (compile-document (example-schema))
      (expected-errors [#"Fields have conflicting return types"])))

;; 5.3.3 Leaf Field Selections

(deftest example-113-test
  (-> (example "113")
      (compile-document (example-schema))
      (expected-errors [])))

#_(deftest example-114-test
  (-> (example "114")
      (compile-document (example-schema))
      (expected-errors [#"The subselection set of a scalar or enum must be empty"])))


(deftest example-115-test
  (-> (example-schema)
      (extend-schema (parse (slurp (io/resource "juxt/grab/examples/example-115.graphql"))))
      (expected-errors [])))

#_(deftest example-116-test
  (let [schema
        (-> (example-schema)
            (extend-schema (parse (slurp (io/resource "juxt/grab/examples/example-115.graphql")))))]
    (-> (example "116")
        (compile-document schema)
        (expected-errors
         [#"Field name 'human' not defined on type in scope 'Query'"
          #"Field name 'pet' not defined on type in scope 'Query'"
          #"Field name 'catOrDog' not defined on type in scope 'Query'"]))))

;; Arguments

;; Fragments
#_(deftest example-117-test
  (is
   (-> (example "117")
       (compile-document (example-schema))
       (expected-errors []))))

#_(-> (example "117")
    (compile-document (example-schema))
       )

#_(deftest example-118-test
  (is
   (-> (example "118")
       (compile-document (example-schema))
       (expected-errors []))))

;; inline fragments
#_(-> (example "111")
    (compile-document (example-schema))
    ;;(validate-document)
    )
