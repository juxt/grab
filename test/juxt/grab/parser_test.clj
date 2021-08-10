;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.parser-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.parser :as parser]))

(alias 'reap (create-ns 'juxt.reap.alpha.graphql))

(deftest parse-query-test
  (is (= '({::reap/type "OperationDefinition"
            ::reap/operation-type "query"
            ::reap/selection-set
            [{::reap/selection-type :field
              ::reap/name "user"
              ::reap/arguments {}}]})
         (-> "query { user }"
             parser/parse-graphql))))
