;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.introspection-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.execution :refer [execute-request]]
   [juxt.grab.alpha.parser :as parser]
   [clojure.java.io :as io]
   [juxt.grab.alpha.document :as document]
   [juxt.grab.alpha.schema :as schema]))

(alias 'g (create-ns 'juxt.grab.alpha.graphql))

;; More ANTLR issues (sigh)
;; https://stackoverflow.com/questions/14504726/antlr-parsing-literals-and-quoted-ids
;; https://github.com/walmartlabs/lacinia/issues/259

#_(schema/compile-schema
              (parser/parse "
scalar Date

type User {
  id: String
  name: String
  birthday: Date
}

type Query {
  user: User
}
"))


#_(let [schema (schema/compile-schema
              (parser/parse "
scalar Date

type User {
  id: String
  name: String
  birthday: Date
}

type Query {
  user: User
}
"))
      document (document/compile-document
                (parser/parse "
{
  __type(name: \"User\") {
    name
    fields {
      name
    }
  }
}

")
                schema)]

  (assert (empty? (::schema/errors schema))  (::schema/errors schema))

  ;;document

  (execute-request
   {:schema schema
    :document document
    :field-resolver
    (fn [{:keys [object-value object-type field-name] :as args}]
      (condp = [(::g/name object-type) field-name]
        ["Root" "user"]
        {:name "Isaac Newton"}

        ["Person" "name"]
        (get-in args [:object-value :name])

        ["Person" "profilePic"]
        (format "https://profile.juxt.site/pic-%d.png" (get-in args [:argument-values "size"]))

        ["Query" "__type"]
        (let [type-name (get-in args [:argument-values "name"])]
          (get-in schema [::schema/provided-types type-name]))

        ["__Type" "name"]
        (::g/name object-value)

        ["__Type" "fields"]
        (::g/field-definitions object-value)

        #_["__Field" "name"]
        #_(throw (ex-info "TODO" {:object-value object-value}))

        (throw (ex-info "TODO: Resolve yet unsupported field" (assoc args :case [(get-in args [:object-type ::g/name])
                                                                                 (get-in args [:field-name])])))))}))

(let [schema (schema/compile-schema
              (parser/parse "
scalar Date

type User {
  id: String
  name: String
  birthday: Date
}

type Query {
  user: User
}
"))
      document (document/compile-document
                (parser/parse "
{
  __type(name: \"User\") {
    name
    fields {
      name
      type {
        name
      }
    }
  }
}

")
                schema)]

  (assert (empty? (::schema/errors schema))  (::schema/errors schema))

  #_document

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

          ["Query" "__type"]
          (get-in schema [:juxt.grab.alpha.schema/provided-types "User"])

          ["__Type" "name"]
          (get-in args [:juxt.grab.alpha.schema/provided-types "User"])


          (throw (ex-info "TODO"
                          (assoc args
                                 :case [(get-in args [:object-type ::g/name])
                                        (get-in args [:field-name])]
                                 :schema schema)))))}))
