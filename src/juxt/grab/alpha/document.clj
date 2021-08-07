;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.document
  (:require
   [juxt.reap.alpha.graphql :as reap]))

(defn process-selection-set [selection-set]
  (for [selection selection-set
        :let [field (::reap/field selection)
              fragment-spread (::reap/fragment-spread selection)]]
    (cond
      field
      (into {::selection-type :field}
            (cond->
                {::name (::reap/name field)
                 ::arguments (::reap/arguments field)}
                (::reap/selection-set field)
                (assoc ::selection-set (process-selection-set (::reap/selection-set field)))))
      fragment-spread
      {::selection-type :field-spread}
      :else
      (throw (ex-info "TODO" {:selection selection})))))

(defn parse-tree->document [parse-tree]
  {::operations
   (->>
    (keep
     (fn [definition]
       (when-let [op-type (::reap/operation-type definition)]
         (let [nm (::reap/name definition)]
           [nm {::name nm
                ::operation-type (keyword op-type)
                ::selection-set (process-selection-set (::reap/selection-set definition))
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
