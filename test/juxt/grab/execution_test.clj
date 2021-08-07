;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.execution-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.execution :as execution]
   [juxt.grab.alpha.document :refer [->document]]
   [juxt.grab.alpha.schema :refer [->schema]]))

(alias 'schema (create-ns 'juxt.grab.alpha.schema))

(set! *print-level* 20)

;; Just to get warmed up
(deftest warm-up-test
  (is
   (= {:data {"user" "mal"}, :errors []}
      (execution/execute-request
       {:schema (->schema "schema { query: User } type User { user: String }")
        :document (->document "query { user }")
        :field-resolver
        (fn [{:keys [field-name]}] (case field-name "user" "mal"))}))))

;; Example 3
