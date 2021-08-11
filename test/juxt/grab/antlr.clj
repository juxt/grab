;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.antlr
  (:require
   [clj-antlr.core :as antlr]
   [clojure.java.io :as io]))

(def graphql (antlr/parser (slurp (io/resource "GraphQL.g4"))))


(graphql (slurp (io/resource "juxt/grab/schema-3.graphql")))
