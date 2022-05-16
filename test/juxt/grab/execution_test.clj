;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.execution-test
  (:require
   [clojure.test :refer [deftest is testing]]
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

(defn remove-error-extensions [result]
  (update result :errors (fn [errors] (mapv #(dissoc % :extensions) errors))))

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
                     (slurp (io/resource "juxt/grab/examples/example-184.graphql")))
                    schema)]

      (->
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
                :args args}))))})
       remove-error-extensions)))))

(defn execute-example-184-query [schema-str]
  (let [schema (schema/compile-schema (parser/parse schema-str))
        document (document/compile-document
                  (parser/parse
                   (slurp (io/resource "juxt/grab/examples/example-184.graphql")))
                  schema)]

    (->
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
              :args args}))))})
     remove-error-extensions)))

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

(defn execute-fragment-example-query [schema-str]
  (let [schema (schema/compile-schema (parser/parse schema-str))
        document (document/compile-document
                  (parser/parse
                   (slurp (io/resource "juxt/grab/examples/fragment-example-query.graphql")))
                  schema)]

    (->
     (execute-request
      {:schema schema
       :document document
       :abstract-type-resolver (fn [{:keys [object-value] :as args}] (get object-value :type))
       :field-resolver
       (fn [{:keys [object-type object-value field-name] :as args}]
         (def foo [(::g/name object-type) field-name])
         (condp = [(::g/name object-type) field-name]
           ["Query" "heros"]
           [{:name "Octoman"
             :id 1
             :type "Alien"
             :tentacles 5}
            {:name "Superman"
             :id 2
             :type "Human"}]

           (get object-value (keyword field-name))))})
     remove-error-extensions)))

(deftest fragment-collection-test
  (testing "happy case"
    (is (=
         {:data {:heros [{:name "Octoman", :tentacles 5} {:name "Superman"}]},
          :errors []}
         (execute-fragment-example-query
          (str/join
           \newline
           ["type Query { heros: [Hero]}"
            "union Hero = Alien | Human"
            "type Alien {"
            "  id: ID!"
            "  name: String"
            " tentacles: Int"
            "}"
            "type Human {"
            "  id: ID!"
            "  name: String"
            "}"])))))
  (testing "Union does not contain requested type"
    (is (=
         {:data {:heros [{:name "Octoman", :tentacles 5} {}]},
          :errors []}
         (execute-fragment-example-query
          (str/join
           \newline
           ["type Query { heros: [Hero]}"
            "union Hero = Alien"
            "type Alien {"
            "  id: ID!"
            "  name: String"
            "  tentacles: Int"
            "}"
            "type Human {"
            "  id: ID!"
            "  name: String"
            "}"])))))
  (testing "Interface implementation not found in type"
    (is (=
         {:data {:heros [{} {:name "Superman"}]},
          :errors []}
         (execute-fragment-example-query
          (str/join
           \newline
           ["type Query { heros: [Hero]}"
            "interface Hero {"
            "  name: String"
            "}"
            "type Human implements Hero {"
            "  name: String"
            "  id: ID!"
            "}"
            "type Alien {"
            "  name: String"
            "  id: ID!"
            "  tentacles: Int"
            "}"]))))))

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
                  (parser/parse (slurp (io/resource "juxt/grab/examples/example-5.graphql")))
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
                  (parser/parse (slurp (io/resource "juxt/grab/examples/example-32.graphql")))
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
  (let [schema (schema/compile-schema
                (parser/parse
                 (str
                  "schema { query: Contact }\n"
                  (slurp (io/resource "juxt/grab/examples/example-62.graphql"))
                  (slurp (io/resource "juxt/grab/examples/example-63.graphql")))))]
    (let [document
          (document/compile-document
           (parser/parse
            (slurp (io/resource "juxt/grab/examples/example-64.graphql")))
           schema)]

      (is
       (=
        {:data {:entity {:name "Alex Davis"}, :phoneNumber "888-123-4567"}}
        (execute-request
         {:schema schema
          :document document
          :field-resolver
          (fn [{:keys [object-type object-value field-name] :as args}]
            (let [pair [(get-in args [:object-type ::g/name])
                        (get-in args [:field-name])]]
              (condp = pair

                ["Contact" "entity"]
                {:name "Alex Davis"
                 :age 61
                 ::type "Person"}

                ["Contact" "phoneNumber"]
                "888-123-4567"

                ["Person" "name"]
                (get object-value :name)

                (throw (ex-info "FAIL" {:args args
                                        :pair pair})))))

          :abstract-type-resolver
          (fn [{:keys [object-value] :as args}]
            (::type object-value))}))))

    (let [document
          (document/compile-document*
           (parser/parse
            (slurp (io/resource "juxt/grab/examples/example-65.graphql")))
           schema)

          errors (::document/errors document)]
      (is (= 1 (count errors)))
      (is (= "Field name 'age' not defined on type in scope 'NamedEntity'"
             (get-in errors [0 :message]))))


    (let [document
          (document/compile-document
           (parser/parse
            (slurp (io/resource "juxt/grab/examples/example-66.graphql")))
           schema)]

      (is (=
           {:data {:entity {:name "Alex Davis", :age 61}, :phoneNumber "888-123-4567"}}
           (execute-request
            {:schema schema
             :document document
             :field-resolver
             (fn [{:keys [object-type object-value field-name] :as args}]
               (let [pair [(get-in args [:object-type ::g/name])
                           (get-in args [:field-name])]]
                 (condp = pair

                   ["Contact" "entity"]
                   {:name "Alex Davis"
                    :age 61
                    ::type "Person"}

                   ["Contact" "phoneNumber"]
                   "888-123-4567"

                   ["Person" "name"]
                   (get object-value :name)

                   ["Person" "age"]
                   (get object-value :age)

                   (throw (ex-info "FAIL" {:args args
                                           :pair pair})))))

             :abstract-type-resolver
             (fn [{:keys [object-value] :as args}]
               (::type object-value))}))))))

(deftest union-test
  (is
   (= {:data {:firstSearchResult {:name "Alex Davis"}}}
      (let [schema
            (schema/compile-schema
             (parser/parse
              (str
               "schema { query: SearchQuery }\n"
               (slurp (io/resource "juxt/grab/examples/example-69.graphql")))))
            document
            (document/compile-document
             (parser/parse
              (slurp (io/resource "juxt/grab/examples/example-71.graphql")))
             schema)]

        (execute-request
         {:schema schema
          :document document
          :field-resolver
          (fn [{:keys [object-type object-value field-name] :as args}]
            (let [pair [(get-in args [:object-type ::g/name])
                        (get-in args [:field-name])]]
              (condp = pair

                ["SearchQuery" "firstSearchResult"]
                {:name "Alex Davis"
                 :age 61
                 ::type "Person"}

                ["Person" "name"]
                (get object-value :name)

                (throw (ex-info "FAIL" {:args args
                                        :pair pair})))))

          :abstract-type-resolver
          (fn [{:keys [object-value] :as args}]
            (::type object-value))})))))

(deftest simple-enum-test
  (let [schema
        (schema/compile-schema
         (parser/parse "
type Query { choose(fruit: Fruit = ORANGE): Fruit }
enum Fruit { APPLE ORANGE BANANA }"))
        document (document/compile-document
                  (parser/parse "{ choose }")
                  schema)]
    (is
     (=
      {:data {:choose "APPLE"}}
      (execute-request
       {:schema schema
        :document document
        :field-resolver
        (fn [{:keys [field-name] :as args}]
          (case field-name
            "choose" "APPLE"
            (throw (ex-info "TODO" {:args args}))))})))))


(deftest stacktrace-test
  (let [schema
        (schema/compile-schema
         (parser/parse "
type Query { error: Error }
type Error { message: String stackTrace: [StackTraceElement] }
type StackTraceElement { fileName: String className: String lineNumber: Int methodName: String }
"))
        document (document/compile-document
                  (parser/parse "{ error { message stackTrace { fileName className lineNumber methodName}} }")
                  schema)
        result (execute-request
                {:schema schema
                 :document document
                 :field-resolver
                 (fn [{:keys [field-name object-type object-value] :as args}]

                   (let [pair [(::g/name object-type) field-name]]
                     (condp = pair

                       ["Query" "error"]
                       (ex-info "My message" {:foo "bar"})

                       ["Error" "message"]
                       (.getMessage object-value)

                       ["Error" "stackTrace"]
                       (map bean (.getStackTrace object-value))

                       ["StackTraceElement" "fileName"]
                       (:fileName object-value)

                       ["StackTraceElement" "className"]
                       (:className object-value)

                       ["StackTraceElement" "lineNumber"]
                       (:lineNumber object-value)

                       ["StackTraceElement" "methodName"]
                       (:methodName object-value))))})]

    (is (some? (:data result)))
    (is (nil? (:errors result)))))
