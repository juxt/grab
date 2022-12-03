;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.alpha.compile
  (:require
   [clojure.walk :refer [keywordize-keys]]
   [juxt.grab.alpha.document :as-alias d]
   [juxt.grab.alpha.graphql :as-alias g]))

(declare compile-query)
(declare compile-fragment-spread)

(defn compile-selection-set [selection-set doc]
  (reduce (fn [acc selection]
            (cond
              (::g/selection-set selection)
              (conj acc (compile-query selection doc))
              (= :fragment-spread (::g/selection-type selection))
              (into acc (compile-fragment-spread selection doc))
              :else
              (conj acc (keyword (::g/name selection)))))
          []
          selection-set))

(defn compile-fragment-spread [selection {::d/keys [fragments] :as doc}]
  (let [fragment (first (filter (fn [fragment]
                                  (= (::g/name fragment) (::g/name selection)))
                                fragments))]
    (compile-selection-set (::g/selection-set fragment) doc)))

(defn compile-query [selection doc]
  {(if (::g/arguments selection)
     (list (keyword (::g/name selection))
           (keywordize-keys (::g/arguments selection))) ;TODO variables
     (keyword (::g/name selection)))
   (compile-selection-set (::g/selection-set selection) doc)})

(defn compile-root [{::d/keys [operations] :as doc}]
  (let [operation (first operations)]
    (if (= :query (::g/operation-type operation))
      [(compile-query (first (::g/selection-set operation)) doc)]
      ;; TODO case statement on type and mutations
      doc)))
