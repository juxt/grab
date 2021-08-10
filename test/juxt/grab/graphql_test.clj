;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.graphql-test
  (:require
   [juxt.grab.alpha.execution :refer [execute-request]]
   [juxt.grab.alpha.document :refer [->document] :as document]
   [juxt.grab.alpha.schema :refer [->schema] :as schema]
   [clojure.test :refer [deftest is are]]
   [clojure.java.io :as io]))

(set! clojure.core/*print-namespace-maps* false)

;; 2.4 Selection Sets

;; "An operation selects the set of information it needs, and will receive
;; exactly that information and nothing more, avoiding over-fetching and
;; under-fetching data."

(defn execute [args]
  (execute-request
   (-> args
       (update :document ->document)
       (update :schema ->schema))))

(deftest selection-sets-test
  (let [field-resolver
        (fn [{:keys [field-name]}]
          (case field-name
            "id" 4
            "firstName" "Mark"
            "lastName" "Zuckerberg"))]
    (are [query expected]
        (=
         {:data expected :errors []}
         (execute
          {:document query
           :schema (slurp (io/resource "juxt/grab/schema-1.graphql"))
           :field-resolver field-resolver}))

        "{ id firstName lastName }"
        {"id" 4
         "firstName" "Mark"
         "lastName" "Zuckerberg"}

        ;; Avoid over-fetching data
        "{ firstName }"
        {"firstName" "Mark"})))

;; 2.5 Fields

(deftest fields-test
  (let [field-resolver
        (fn [args]
          (condp = [(get-in args [:object-type ::schema/name])
                    (get-in args [:field-name])]
            ["Root" "me"]
            {:id 1
             :first-name "Isaac"
             :last-name "Newton"
             :birthday (java.time.MonthDay/of 1 4)
             :friends [{:id 2
                        :name "Gottfried Wilhelm Leibniz"}]}

            ["User" "id"] (get-in args [:object-value :id])
            ["User" "firstName"] (get-in args [:object-value :first-name])
            ["User" "lastName"] (get-in args [:object-value :last-name])
            ["User" "birthday"] (get-in args [:object-value :birthday])
            ["Birthday" "month"] (.getValue (.getMonth (:object-value args)))
            ["Birthday" "day"] (.getDayOfMonth (:object-value args))
            ["User" "friends"] (get-in args [:object-value :friends])
            ["Friend" "name"] (get-in args [:object-value :name])

            (throw (ex-info "Resolve field" args))))]

    (is
     (= {:data
         {"me"
          {"id" 1
           "firstName" "Isaac"
           "lastName" "Newton"
           "birthday" {"month" 1 "day" 4}
           "friends" [{"name" "Gottfried Wilhelm Leibniz"}]}}
         :errors []}

        (execute
         {:document (slurp (io/resource "juxt/grab/example-08.graphql"))
          :schema (slurp (io/resource "juxt/grab/schema-2.graphql"))
          :field-resolver field-resolver
          :initial-value {}})))))

;; 2.6 Arguments

(deftest arguments-test
  (is
   (=
    {:data
     {"user"
      {"id" 1,
       "name" "Isaac Newton",
       "profilePic" "https://profiles.juxt.site/newton/100.jpg"}},
     :errors []}

    (execute
     {:document (slurp (io/resource "juxt/grab/example-10.graphql"))
      :schema (slurp (io/resource "juxt/grab/schema-3.graphql"))
      :field-resolver
      (fn [args]
        (condp = [(get-in args [:object-type ::schema/name])
                  (get-in args [:field-name])]
          ["Root" "user"]
          {:id 1
           :name "Isaac Newton"}

          ["Person" "id"]
          (get-in args [:object-value :id])

          ["Person" "name"]
          (get-in args [:object-value :name])

          ["Person" "profilePic"]
          (format "https://profiles.juxt.site/newton/%d.jpg" (get-in args [:argument-values "size"]))

          (throw (ex-info "Resolve field" args))))}))))

;; 2.7 Field Alias

(deftest field-alias-test
  (let [field-resolver
        (fn [args]
          (condp = [(get-in args [:object-type ::schema/name])
                    (get-in args [:field-name])]
            ["Root" "user"]
            (get {4 {:id 4
                     :name "Mark Zuckerberg"}}
                 (get-in args [:argument-values "id"]))

            ["Person" "id"]
            (get-in args [:object-value :id])

            ["Person" "name"]
            (get-in args [:object-value :name])

            ["Person" "profilePic"]
            (format "https://cdn.site.io/pic-4-%d.jpg" (get-in args [:argument-values "size"]))

            (throw (ex-info "Resolve field" args))))]
    (is
     (=
      {:data
       {"user"
        {"id" 4
         "name" "Mark Zuckerberg"
         "smallPic" "https://cdn.site.io/pic-4-64.jpg"
         "bigPic" "https://cdn.site.io/pic-4-1024.jpg"}},
       :errors []}

      (execute
       {:document (slurp (io/resource "juxt/grab/example-14.graphql"))
        :schema (slurp (io/resource "juxt/grab/schema-3.graphql"))
        :field-resolver field-resolver
        :initial-value {}})))

    (is
     (=
      {:data
       {"zuck"
        {"id" 4
         "name" "Mark Zuckerberg"}}
       :errors []}

      (execute
       {:document (slurp (io/resource "juxt/grab/example-16.graphql"))
        :schema (slurp (io/resource "juxt/grab/schema-3.graphql"))
        :field-resolver field-resolver
        :initial-value {}})))))

;; 2.8 Fragments

(deftest fragments-test
  (is
   (=
    {:data
     {"user"
      {"friends"
       [{"id" 5,
         "name" "Daysi Pennock",
         "profilePic" "https://cdn.site.io/pic-5-50.jpg"}
        {"id" 6,
         "name" "Meagan Chason",
         "profilePic" "https://cdn.site.io/pic-6-50.jpg"}
        {"id" 7,
         "name" "Katrina Ulibarri",
         "profilePic" "https://cdn.site.io/pic-7-50.jpg"}],
       "mutualFriends"
       [{"id" 6,
         "name" "Meagan Chason",
         "profilePic" "https://cdn.site.io/pic-6-50.jpg"}
        {"id" 7,
         "name" "Katrina Ulibarri",
         "profilePic" "https://cdn.site.io/pic-7-50.jpg"}]}},
     :errors []}

    (let [people
          {4 {:id 4
              :name "Mark Zuckerberg"
              :friends [5 6 7]
              :mutualFriends [6 7]}
           5 {:id 5
              :name "Daysi Pennock"}
           6 {:id 6
              :name "Meagan Chason"}
           7 {:id 7
              :name "Katrina Ulibarri"}
           8 {:id 8
              :name "Shanae Zajicek"}
           9 {:id 9
              :name "Carlos Stefanik"}
           10 {:id 10
               :name "Tamisha Ciampa"}}]
      (execute
       {:document (slurp (io/resource #_"juxt/grab/example-18.graphql" "juxt/grab/example-19.graphql"))
        :schema (slurp (io/resource "juxt/grab/schema-4.graphql"))
        :field-resolver
        (fn [args]
          (condp = [(get-in args [:object-type ::schema/name])
                    (get-in args [:field-name])]
            ["Root" "user"]
            (get people
                 (get-in args [:argument-values "id"]))

            ["Person" "id"]
            (get-in args [:object-value :id])

            ["Person" "name"]
            (get-in args [:object-value :name])

            ["Person" "friends"]
            (for [id (get-in args [:object-value :friends])]
              (get people id))

            ["Person" "mutualFriends"]
            (for [id (get-in args [:object-value :mutualFriends])]
              (get people id))

            ["Person" "profilePic"]
            (format "https://cdn.site.io/pic-%d-%d.jpg"
                    (get-in args [:object-value :id])
                    (get-in args [:argument-values "size"]))

            (throw (ex-info "Resolve field" args))))}))

    )))
