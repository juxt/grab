;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.validation-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.parser :as parser]
   [juxt.grab.alpha.document :refer [executable]]
   [clojure.java.io :as io]))

(deftest illegal-type-system-definition-test
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    (executable (parser/parse "scalar Illegal")))))


(deftest illegal-type-extension-test
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    (executable
     (parser/parse (slurp (io/resource "juxt/grab/example-91.graphql")))))))

(deftest operation-name-uniqueness-test
  (is
   (executable
    (parser/parse (slurp (io/resource "juxt/grab/example-92.graphql")))))
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    (executable
     (parser/parse (slurp (io/resource "juxt/grab/example-93.graphql"))))))
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    (executable
     (parser/parse (slurp (io/resource "juxt/grab/example-94.graphql")))))))

(deftest lone-operation-test
  (is
   (executable
    (parser/parse (slurp (io/resource "juxt/grab/example-95.graphql")))))
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    (executable
    (parser/parse (slurp (io/resource "juxt/grab/example-96.graphql")))))))
