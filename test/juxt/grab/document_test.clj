;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.document-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.parser :as parser]
   [juxt.grab.alpha.document :as document]))

(deftest get-operation-test
  (let [doc (-> "query foo { user } query bar { user }"
                parser/parse-graphql
                document/parse-tree->document)]
    (are [arg expected]
        (= expected (::document/name (document/get-operation doc arg)))
        "foo" "foo"
        "bar" "bar")
    (is (thrown? clojure.lang.ExceptionInfo
                 (document/get-operation doc nil))
        "Otherwise produce a query error requiring operationName.")
    (is (thrown? clojure.lang.ExceptionInfo
                 (document/get-operation doc "zip"))
        "If operation was not found, produce a query error.")))
