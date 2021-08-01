;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document
  (:require
   [juxt.reap.alpha.graphql :as reap]))

(defn parse-tree->document [parse-tree]
  {::operations
   (->>
    (keep
     (fn [definition]
       (when-let [op-type (::reap/operation-type definition)]
         (let [nm (::reap/name definition)]
           [nm {::name nm
                ::operation-type (keyword op-type)
                ::parse-tree definition}])))
     parse-tree)
    (into {}))
   ::parse-tree parse-tree})

(defn
  ^{:graphql/name "GetOperation"}
  get-operation
  [doc op-name]
  (if-let [op (get (::operations doc) op-name)]
    (if op-name
      op
      (if (= (count (::operations doc)) 1)
        op
        (throw (ex-info "Operation name not specified and multiple operations exist" {}))))
    (if op-name
      (throw (ex-info "Operation not found" {:operation-name op-name}))
      (throw (ex-info "Operation name required" {})))))
