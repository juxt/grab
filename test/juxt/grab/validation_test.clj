;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.validation-test
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.parser :as parser]
   [juxt.grab.alpha.document :refer [compile]]
   [juxt.grab.alpha.schema :refer [schema]]
   [clojure.java.io :as io]))

(set! clojure.core/*print-namespace-maps* false)

(deftest schema-parsing-test
  (is
   (schema
    (parser/parse (slurp (io/resource "juxt/grab/example-90.graphql"))))))

(deftest
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  illegal-type-system-definition-test
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    #"A document containing a TypeSystemDefinition is invalid for execution"
    (compile (parser/parse "scalar Illegal") {}))))

(deftest illegal-type-extension-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Executable-Definitions"}
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    #"A document containing a TypeSystemDefinition is invalid for execution"
    (compile
     (parser/parse (slurp (io/resource "juxt/grab/example-91.graphql")))
     {}))))

(deftest operation-name-uniqueness-test
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Operation-Name-Uniqueness"}
  (is
   (compile
    (parser/parse (slurp (io/resource "juxt/grab/example-92.graphql")))
    {}))
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    #"Operation name is not unique"
    (compile
     (parser/parse (slurp (io/resource "juxt/grab/example-93.graphql")))
     {})))
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    #"Operation name is not unique"
    (compile
     (parser/parse (slurp (io/resource "juxt/grab/example-94.graphql")))
     {}))))

(deftest
  ^{:juxt/see
    "https://spec.graphql.org/June2018/#sec-Lone-Anonymous-Operation"}
  lone-operation-test
  (is
   (compile
    (parser/parse (slurp (io/resource "juxt/grab/example-95.graphql")))
    {}))
  (is
   (thrown-with-msg?
    clojure.lang.ExceptionInfo
    #"When there are multiple operations in the document, none can be anonymous"
    (compile
     (parser/parse (slurp (io/resource "juxt/grab/example-96.graphql")))
     {}))))

;; TODO: 5.2.3 Subscription Operation Definitions
;; These are not yet covered, since subscriptions are not supported.

;; 5.3 Fields
