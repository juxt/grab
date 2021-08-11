;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.reap.document-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.reap.parser :as parser]
   [juxt.grab.alpha.reap.document :as rd]
   [juxt.grab.alpha.document :as document]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(deftest get-operation-test
  (let [doc (-> "query foo { user } query bar { user }"
                parser/parse-graphql
                rd/parse-tree->document)]
    (are [arg expected]
        (= expected (::g/name (document/get-operation doc arg)))
        "foo" "foo"
        "bar" "bar")
    (is (thrown? clojure.lang.ExceptionInfo
                 (document/get-operation doc nil))
        "Otherwise produce a query error requiring operationName.")
    (is (thrown? clojure.lang.ExceptionInfo
                 (document/get-operation doc "zip"))
        "If operation was not found, produce a query error.")))

(deftest shorthand-test
  (let [doc (-> "{ user }"
                parser/parse-graphql
                rd/parse-tree->document)
        operation (document/get-operation doc nil)]
    (is (= :query (::g/operation-type operation)))
    (is (nil? (::g/name operation)))))
