;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.reap.parser-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.reap.parser :as reap.parser]))

(alias 'reap (create-ns 'juxt.reap.alpha.graphql))

(deftest parse-query-test
  (is (= '({::reap/type "OperationDefinition"
            ::reap/operation-type "query"
            ::reap/selection-set
            [{::reap/selection-type :field
              ::reap/name "user"
              ::reap/arguments {}}]})
         (-> "query { user }"
             reap.parser/parse-graphql))))
