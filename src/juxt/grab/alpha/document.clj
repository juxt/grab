;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document
  (:require
   [juxt.reap.alpha.graphql :as reap]))

(defn parse-tree->document [parse-tree]
  {::operations
   (->>
    (keep
     (fn [definition]
       (when (::reap/operation-type definition)
         (let [nm (::reap/name definition)]
           [nm (assoc definition ::name nm)])))
     parse-tree)
    (into {}))
   ::parse-tree parse-tree})

(defn
  ^{:graphql/name "GetOperation"}
  get-operation
  [doc op-name]
  (if-let [op (get (::operations doc) op-name)]
    (if (nil? op-name)
      (if (= (count (::operations doc)) 1)
        op
        (throw (ex-info "Operation name is nil and multiple operations exist" {})))
      op)
    (if (nil? op-name)
      (throw (ex-info "Operation name required" {}))
      (throw (ex-info "Operation not found" {:operation-name op-name})))))
