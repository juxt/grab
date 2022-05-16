(ns juxt.grab.alpha.compile
  (:require [clojure.walk :refer [keywordize-keys]]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))
(alias 'd (create-ns 'juxt.grab.alpha.document))
(alias 'site (create-ns 'site))
(alias 'pass (create-ns 'pass))

(declare compile-query)
(declare compile-fragment-spread)
(declare compile-lone-field)

(defn get-or-nth-in [coll ks]
  (if (seq ks)
    (condp instance? (first ks)
      Number (get-or-nth-in (nth coll (first ks)) (rest ks))
      clojure.lang.Keyword (get-or-nth-in ((first ks) coll) (rest ks)))
    coll))


(defn compile-selection-set [selection-set {::d/keys [operations fragments] :as doc} acc path]
  (reduce (fn [acc selection]
            (def selection selection)
            (cond
              (::g/selection-set selection)
              (compile-query selection doc acc path)
              (= :fragment-spread (::g/selection-type selection))
              (compile-fragment-spread selection doc acc path)
              :else
              (compile-lone-field selection doc acc path)
              ))
          acc selection-set))


(defn compile-lone-field [selection {::d/keys [operations fragments] :as doc} acc path]
  (update-in acc
             path
             #(conj %
                    (if (::g/arguments selection)
                      ;; I may be confusing idents with params
                      ;; How to translate fields with arguments to EQL? TODO
                      [(keyword (::g/name selection)) (first (vals (::g/arguments selection)))]
                      (keyword (::g/name selection))))))


(defn compile-fragment-spread [selection {::d/keys [operations fragments] :as doc} acc path]
  (let [fragment (first (filter (fn [fragment]
                                  (= (::g/name fragment) (::g/name selection)))
                                fragments))]
    (compile-selection-set (::g/selection-set fragment) doc acc path)))

(defn compile-query
  ([selection doc acc]
   (let [template
         (conj acc {:find []})
         path
         [:find]]
     (compile-query selection doc template path)))
  
  ([selection {::d/keys [operations fragments] :as doc} acc path]
   
   (let [selector (if (::g/arguments selection)
                    (list (keyword (::g/name selection))
                          (keywordize-keys (::g/arguments selection))) ;TODO variables
                    (keyword (::g/name selection)))
         template
         (update-in acc path
                    #(conj %
                           {selector
                            []}))
         path
         (conj path (count (get-or-nth-in acc path)) selector)]
     
     (if (and (::g/directives selection) (seq (filter (fn [directive] (= (::g/name directive) "site")) (::g/directives selection))))
       (if (:action template)
         acc
         (compile-selection-set
          (::g/selection-set selection)
          doc
          (conj template {:action
                          (get
                           (::g/arguments (first (filter (fn [directive] (= (::g/name directive) "site"))
                                                         (::g/directives selection))))
                           "action")})
          path))
       (compile-selection-set (::g/selection-set selection) doc template path)))))

;; (defn compile-mutation [selection {::d/keys [operations fragments] :as doc} acc]
;;   (-> (::g/name selection)
;;       (list {:data (vals (::g/arguments selection))})))

(defn compile-xt-query [{::d/keys [operations fragments] :as doc}]
  (let [operation (first operations)
        template '{:where [[action ::site/type "https://meta.juxt.site/pass/action"]
                           [(contains? actions action)]
                           [permission ::site/type "https://meta.juxt.site/pass/permission"]
                           [permission ::pass/action action]
                           (allowed? permission subject action resource)]
                   :in [subject actions resource]}
        new-template
        (case (::g/operation-type operation)
          :query
          (compile-query (first (::g/selection-set operation))
                         doc
                         template)
          ;; Mutations not sure how the function call will work, possibly a lookup environment (pass a map?)
          ;; :mutation
          ;; (conj template {:mutate
          ;;                 [(compile-mutation (first (::g/selection-set operation)) doc template)]})
          (throw (Exception. "TODO")))]
    (def doc doc)
    new-template))

