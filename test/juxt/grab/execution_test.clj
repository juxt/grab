;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.execution-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.execution :refer [execute-request]]
   [juxt.grab.alpha.parser :as parser]
   [clojure.java.io :as io]
   [juxt.grab.alpha.document :as document]
   [juxt.grab.alpha.schema :as schema]
   [clojure.string :as str]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

(deftest warmup-test
  (is
   (= {:data
       {:user
        {:name "Isaac Newton"
         :profilePic "https://profile.juxt.site/pic-100.png"}}}
      (let [schema (schema/compile-schema
                    (parser/parse (slurp (io/resource "juxt/grab/schema-3.graphql"))))
            document (document/compile-document
                      (parser/parse (slurp (io/resource "juxt/grab/query-3.graphql")))
                      schema)]

        (execute-request
         {:schema schema
          :document document
          :field-resolver
          (fn [args]
            (condp =
                [(get-in args [:object-type ::g/name])
                 (get-in args [:field-name])]
                ["Root" "user"]
                {:name "Isaac Newton"}

                ["Person" "name"]
                (get-in args [:object-value :name])

                ["Person" "profilePic"]
                (format "https://profile.juxt.site/pic-%d.png" (get-in args [:argument-values "size"]))

                (throw (ex-info "" args))))})))))

(deftest error-result-format-test
  (is
   (=
    {:data
     {:hero
      {:name "R2-D2"
       :heroFriends
       [{:id "1000" :name "Luke Skywalker"}
        {:id "1002" :name nil}
        {:id "1003" :name "Leia Organa"}]}}
     :errors
     [{:message "Name for character with ID 1002 could not be fetched."
       :path [:hero :heroFriends 1 :name]}]}
    (let [schema
          (schema/compile-schema
           (parser/parse
            (str/join
             \newline
             ["type Query { hero(episode: ID!): Person! }"
              "type Person {"
              "  id: ID!"
              "  name: String"
              "  friends: [Person!]"
              "}"])))
          document (document/compile-document
                    (parser/parse
                     (slurp (io/resource "juxt/grab/example-184.graphql")))
                    schema)]

      (execute-request
       {:schema schema
        :document document
        :field-resolver
        (fn [{:keys [object-type object-value field-name] :as args}]
          (condp = [(::g/name object-type) field-name]
            ["Query" "hero"]
            {:name "R2-D2"
             :friends [{:id "1000" :name "Luke Skywalker"}
                       {:id "1002"}
                       {:id "1003" :name "Leia Organa"}]}

            ["Person" "id"]
            (get object-value :id)

            ["Person" "name"]
            (if (= (:id object-value) "1002")
              (throw
               (ex-info
                (format
                 "Name for character with ID %s could not be fetched."
                 (:id object-value))
                {}))
              (get object-value :name))

            ["Person" "friends"]
            (get object-value :friends)

            (throw
             (ex-info
              "TODO"
              {:case [(::g/name object-type) field-name]
               :args args}))))})))))

(defn execute-example-184-query [schema-str]
  (let [schema (schema/compile-schema (parser/parse schema-str))
        document (document/compile-document
                  (parser/parse
                   (slurp (io/resource "juxt/grab/example-184.graphql")))
                  schema)]

    (execute-request
     {:schema schema
      :document document
      :field-resolver
      (fn [{:keys [object-type object-value field-name] :as args}]
        (condp = [(::g/name object-type) field-name]
          ["Query" "hero"]
          {:name "R2-D2"
           :friends [{:id "1000" :name "Luke Skywalker"}
                     {:id "1002"}
                     {:id "1003" :name "Leia Organa"}]}

          ["Person" "id"]
          (get object-value :id)

          ["Person" "name"]
          (if (= (:id object-value) "1002")
            (throw
             (ex-info
              (format
               "Name for character with ID %s could not be fetched."
               (:id object-value))
              {}))
            (get object-value :name))

          ["Person" "friends"]
          (get object-value :friends)

          (throw
           (ex-info
            "TODO"
            {:case [(::g/name object-type) field-name]
             :args args}))))})))

;; This test checks various combinations of non-null wrappers in the schema to
;; check the behaviour defined in section 6.4.4 of the GraphQL June 2018 spec.
(deftest non-nullability-propagation-test
  (is (=
       {:data
        {:hero
         {:name "R2-D2"
          :heroFriends
          [{:id "1000" :name "Luke Skywalker"}
           {:id "1002" :name nil}
           {:id "1003" :name "Leia Organa"}]}}
        :errors
        [{:message "Name for character with ID 1002 could not be fetched."
          :path [:hero :heroFriends 1 :name]}]}
       (execute-example-184-query
        (str/join
         \newline
         ["type Query { hero(episode: ID!): Person }"
          "type Person {"
          "  id: ID!"
          "  name: String"
          "  friends: [Person]"
          "}"]))))
  (is
   (=
    {:data
     {:hero
      {:name "R2-D2"
       :heroFriends
       [{:id "1000" :name "Luke Skywalker"}
        nil
        {:id "1003" :name "Leia Organa"}]}}
     :errors
     [{:message "Name for character with ID 1002 could not be fetched."
       :path [:hero :heroFriends 1 :name]}]}
    (execute-example-184-query
     (str/join
      \newline
      ["type Query { hero(episode: ID!): Person }"
       "type Person {"
       "  id: ID!"
       "  name: String!"
       "  friends: [Person]"
       "}"]))))

  (is
   (=
    {:data {:hero {:name "R2-D2" :heroFriends nil}}
     :errors
     [{:message "Name for character with ID 1002 could not be fetched."
       :path [:hero :heroFriends 1 :name]}]}
    (execute-example-184-query
     (str/join
      \newline
      ["type Query { hero(episode: ID!): Person }"
       "type Person {"
       "  id: ID!"
       "  name: String!"
       "  friends: [Person!]"           ; renders the list nil
       "}"]))))

  (is
   (=
    {:data {:hero nil}
     :errors
     [{:message "Name for character with ID 1002 could not be fetched."
       :path [:hero :heroFriends 1 :name]}]}
    (execute-example-184-query
     (str/join
      \newline
      ["type Query { hero(episode: ID!): Person }"
       "type Person {"
       "  id: ID!"
       "  name: String!"
       "  friends: [Person!]!" ; wrap the list itself with a non-null
       "}"]))))

  (is
   (=
    {:data nil
     :errors
     [{:message "Name for character with ID 1002 could not be fetched."
       :path [:hero :heroFriends 1 :name]}]}
    (execute-example-184-query
     (str/join
      \newline
      ["type Query { hero(episode: ID!): Person! }"
       "type Person {"
       "  id: ID!"
       "  name: String!"
       "  friends: [Person!]!"
       "}"])))))

;; TODO: Coercion errors with propagation

(deftest mutation-test
  (let [schema (schema/compile-schema
                (parser/parse
                 (str/join
                  "\n"
                  ["type Query { stories: [Story] }"
                   "type Mutation { likeStory(storyID: Int!): LikeStoryResult }"
                   "type LikeStoryResult { story: Story }"
                   "type Story { likeCount: Int }"])))
        document (document/compile-document
                  (parser/parse (slurp (io/resource "juxt/grab/example-5.graphql")))
                  schema)
        stories (atom {12345 {:likes 0}
                       54321 {:likes 0}})]

    (execute-request
     {:schema schema
      :document document
      :field-resolver
      (fn [args]
        (let [story-id (get-in args [:argument-values "storyID"])
              field-name (get-in args [:field-name])]
          (case field-name
            "likeStory" (get
                         (swap! stories update-in [story-id :likes] inc)
                         story-id)
            (condp =
                [(get-in args [:object-type ::g/name])
                 field-name]

                ["LikeStoryResult" "story"]
                (:object-value args)

                ["Story" "likeCount"]
                (:likes (:object-value args))

                (throw (ex-info "TODO: resolve field" {:args args}))))))})

    (is (= {12345 {:likes 1}
            54321 {:likes 0}}
           @stories))))


(deftest example-32-test
  (let [schema (schema/compile-schema
                (parser/parse
                 (str/join
                  "\n"
                  ["type Query { user(id: Int): User }"
                   "type User { id: Int name: String profilePic(size: Int): String }"])))

        document (document/compile-document
                  (parser/parse (slurp (io/resource "juxt/grab/example-32.graphql")))
                  schema)

        users {4 {:id 4
                  :name "Mark Zuckerberg"
                  :profilePic {60 "abc.png" 90 "def.png"}}}]

    (is
     (=
      {:data {:user {:id 4, :name "Mark Zuckerberg", :profilePic "abc.png"}}}
      (execute-request
       {:schema schema
        :document document
        :variable-values {"devicePicSize" 60}
        :field-resolver
        (fn [args]
          (condp =
              [(get-in args [:object-type ::g/name])
               (get-in args [:field-name])]

            ["Query" "user"]
            (get users (get-in args [:argument-values "id"]))

            ["User" "id"]
            (get-in args [:object-value :id])

            ["User" "name"]
            (get-in args [:object-value :name])

            ["User" "profilePic"]
            (get-in args [:object-value :profilePic (get-in args [:argument-values "size"])])

            (throw (ex-info "TODO: resolve field" {:args args}))))})))))

(deftest interface-test
  (is
   (=
    {:data {:entity {:name "JUXT LTD."}}}
    (let [schema
          (schema/compile-schema
           (parser/parse
            (str/join
             \newline
             ["type Query { entity: NamedEntity }"
              (slurp (io/resource "juxt/grab/example-62.graphql"))])))
          document (document/compile-document (parser/parse "{ entity { name } }") schema)]

      (execute-request
       {:schema schema
        :document document
        :field-resolver
        (fn [args]
          (condp = [(get-in args [:object-type ::g/name])
                    (get-in args [:field-name])]

            ["Query" "entity"]
            {:name "JUXT LTD."
             :value 100
             ::type "Business"
             }

            ["Business" "name"]
            (get-in args [:object-value :name])

            (throw (ex-info "TODO" {:args args}))))

        :abstract-type-resolver
        (fn [{:keys [abstract-type object-value]}]
          #_(throw (ex-info "break" {:abstract-type abstract-type
                                     :object-value object-value}))
          (assert abstract-type)
          (assert object-value)
          (::type object-value)
          )}))))
  )
