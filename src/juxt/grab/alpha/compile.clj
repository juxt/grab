(ns juxt.grab.alpha.compile
  (:require [clojure.walk :refer [keywordize-keys]]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))
(alias 'd (create-ns 'juxt.grab.alpha.document))

(declare compile-query)
(declare compile-fragment-spread)

(defn compile-selection-set [selection-set {::d/keys [operations fragments] :as doc}]
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


(defn compile-fragment-spread [selection {::d/keys [operations fragments] :as doc}]
  (let [fragment (first (filter (fn [fragment]
                                  (= (::g/name fragment) (::g/name selection)))
                                fragments))]
    (compile-selection-set (::g/selection-set fragment) doc)))

(defn compile-query [selection {::d/keys [operations fragments] :as doc}]
  {(if (::g/arguments selection)
     (list (keyword (::g/name selection))
           (keywordize-keys (::g/arguments selection))) ;TODO variables
     (keyword (::g/name selection)))
   (compile-selection-set (::g/selection-set selection) doc)})


(defn compile-root [{::d/keys [operations fragments] :as doc}]
  (let [operation (first operations)]
    (if (= :query (::g/operation-type operation))
      [(compile-query (first (::g/selection-set operation)) doc)]
      ;; TODO case statement on type and mutations
      doc)))

