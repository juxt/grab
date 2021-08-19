;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.validation-test
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.parser :as parser]
   [juxt.grab.alpha.document :refer [compile]]
   [juxt.grab.alpha.schema :refer [compile-schema]]
   [clojure.java.io :as io]))

(alias 'doc (create-ns 'juxt.grab.alpha.document))

(set! clojure.core/*print-namespace-maps* false)

(defn matches? [{::doc/keys [errors]} regexes]
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
      (matches? [#"A document containing a type system definition or extension is invalid for execution"])))

(deftest illegal-type-extension-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  (-> "juxt/grab/example-91.graphql"
      io/resource slurp parser/parse (compile {})
      (matches? [#"A document containing a type system definition or extension is invalid for execution" nil])))

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
      (matches? [nil nil #"Operation name '.+' is not unique"]))
  (-> "juxt/grab/example-94.graphql"
      io/resource
      slurp
      parser/parse
      (compile {})
      (matches? [nil nil #"Operation name '.+' is not unique"])))

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
      (matches? [nil nil #"When there are multiple operations in the document, none can be anonymous"])))

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
        (matches? [#"Field name '.+' not defined on type in scope '.+'"]))))

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
        (matches? [#"Field name '.+' not defined on type in scope '.+'"
                   #"Field name '.+' not defined on type in scope '.+'"]))))
