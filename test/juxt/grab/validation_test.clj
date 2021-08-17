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
  (is (= (count errors) (count regexes)))
  (doall
   (map
    (fn [error regex]
      (is (re-matches regex (:error error))))
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
      (matches? [#"A document containing a TypeSystemDefinition is invalid for execution"])))

(deftest illegal-type-extension-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  (-> "juxt/grab/example-91.graphql"
      io/resource slurp parser/parse (compile {})
      (matches? [#"A document containing a TypeSystemDefinition is invalid for execution"])))

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
      (matches? [#"Operation name '.+' is not unique"]))
  (-> "juxt/grab/example-94.graphql"
      io/resource
      slurp
      parser/parse
      (compile {})
      (matches? [#"Operation name '.+' is not unique"])))

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
      (matches? [#"When there are multiple operations in the document, none can be anonymous"])))

;; TODO: 5.2.3 Subscription Operation Definitions
;; These are not yet covered, since subscriptions are not supported.

;; 5.3 Fields
