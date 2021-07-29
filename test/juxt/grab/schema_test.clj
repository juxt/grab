;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.schema-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.graphql :as grab]))

(grab/parse-graphql
   "type Person {
  name: String
  picture(size: Int): Url}

schema { query: Person }

query { person }
")
