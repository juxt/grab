;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.reap.parser
  (:require
   [juxt.reap.alpha.api :as reap]
   [juxt.reap.alpha.graphql :as rg]))

(defn parse-graphql
  "Return a document"
  [s]
  (reap/decode rg/Document s))
