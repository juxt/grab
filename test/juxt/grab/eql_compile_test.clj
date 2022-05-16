
(ns juxt.grab.eql-compile-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.schema :as schema]
   [juxt.grab.alpha.document :as doc]
   [juxt.grab.alpha.parser :refer [parse]]
   [juxt.grab.alpha.compile :refer [compile-xt-query]]
   [clojure.java.io :as io]
   [clojure.string :as str]))


(defn expected-errors [{::doc/keys [errors]} regexes]
  (is
   (= (count errors) (count regexes))
   "Count of errors doesn't equal expected count")
  (doall
   (map
    (fn [error regex]
      (when regex
        (is (re-matches regex (:message error)))))
    errors regexes)))

(defn example-schema []
  (schema/compile-schema
   (parse (slurp (io/resource "juxt/grab/examples/example-90.graphql")))))


(deftest simple-query-test
  (is (= (-> (parse "query findDog { dog @site(action: \"https://site/actions/give-a-dog-a-bone\") { name }}")
             (doc/compile-document* (example-schema))
             (compile-xt-query))
         '{:where          
           [[action :site/type "https://meta.juxt.site/pass/action"]
            [(contains? actions action)]
            [permission :site/type "https://meta.juxt.site/pass/permission"]
            [permission :pass/action action]
            (allowed? permission subject action resource)],
           :in [subject actions resource],
           :action "https://site/actions/give-a-dog-a-bone",
           :find [{:dog [:name]}]})))


(deftest arguments-test-1
  (is (= (-> (parse "query WithBarkVolume {
  dog(barkVolume: 12) {
   name
}
}")
             (doc/compile-document* (example-schema))
             (compile-xt-query))
         '{:where          
             [[action :site/type "https://meta.juxt.site/pass/action"]
              [(contains? actions action)]
              [permission :site/type "https://meta.juxt.site/pass/permission"]
              [permission :pass/action action]
              (allowed? permission subject action resource)],
             :in [subject actions resource],
          :find [{(:dog {:barkVolume 12}) [:name]}]})))


;; Path ? so we can retain data about where the action we broke on was ?
(deftest join-test-1
  (is (= (-> (parse "
query getOwnerName {
  dog @site(action: \"https://site/give-a-dog-a-bone\") {
  name
  owner @site(action: \"https://example\") {
      name
    }
  }
}
")
             (doc/compile-document* (example-schema))
             (compile-xt-query))
         '{:where          
             [[action :site/type "https://meta.juxt.site/pass/action"]
              [(contains? actions action)]
              [permission :site/type "https://meta.juxt.site/pass/permission"]
              [permission :pass/action action]
              (allowed? permission subject action resource)],
           :in [subject actions resource],
           :find [{:dog [:name]}],
           :action "https://site/give-a-dog-a-bone"})))

(deftest join-test-2
  (is (= (-> (parse "
query getOwnerName {
  dog @site(action: \"https://site/give-a-dog-a-bone\") {
  name
  owner {
      name
    }
  }
}
")
             (doc/compile-document* (example-schema))
             (compile-xt-query))
         '{:where          
             [[action :site/type "https://meta.juxt.site/pass/action"]
              [(contains? actions action)]
              [permission :site/type "https://meta.juxt.site/pass/permission"]
              [permission :pass/action action]
              (allowed? permission subject action resource)],
             :in [subject actions resource],
             :find [{:dog [:name {:owner [:name]}]}],
             :action "https://site/give-a-dog-a-bone"})))

(deftest fragments-test-1
  (is (= (-> (parse "{
  dog {
    ...fragmentOne
    ...fragmentTwo
  }
}

fragment fragmentOne on Dog {
  name
}

fragment fragmentTwo on Dog {
  owner {
    name
  }
}
")
             (doc/compile-document* (example-schema))
             (compile-xt-query))
         '{:where          
             [[action :site/type "https://meta.juxt.site/pass/action"]
              [(contains? actions action)]
              [permission :site/type "https://meta.juxt.site/pass/permission"]
              [permission :pass/action action]
              (allowed? permission subject action resource)],
             :in [subject actions resource],
          :find [{:dog [:name {:owner [:name]}]}]})))

(deftest fragments-test-2
  (is (= (-> (parse "{
   dog {
     ...fragmentOne
     }
}

fragment fragmentOne on dog {
   doesKnowCommand
}")
             (doc/compile-document* (example-schema))
             (compile-xt-query))
         '{:where          
             [[action :site/type "https://meta.juxt.site/pass/action"]
              [(contains? actions action)]
              [permission :site/type "https://meta.juxt.site/pass/permission"]
              [permission :pass/action action]
              (allowed? permission subject action resource)],
             :in [subject actions resource],
             :find [{:dog [:doesKnowCommand]}]})))

#_(deftest var-test-1
  (is (= (-> (parse "query WithBarkVolume($bvol: Int) {
  dog(barkVolume: $bvol) {
   name
}
}")
             (doc/compile-document* (example-schema))
             (compile-xt-query))
         [{'(:dog {:barkVolume 12}) [:name]}])))


#_(deftest mutation-test-1
    (let [schema (schema/compile-schema
                  (parse
                   (str/join
                    "\n"
                    ["type Query { user(id: Int): User }"
                     "type User { id: Int name: String profilePic(size: Int): String }"
                     "type Mutation { addUser(name: String!, size: Int ): User }"])))

          document (doc/compile-document
                    (parse "mutation addUserNew {
  addUser(name: \"johnson\" size: 12) {
    name
  }
}")
                    schema)]
      ;; TODO check for call functions
      (is (= (compile-xt-query document) ['("addUser" {:data ["johnson" 12]})]))))




#_(deftest ident-test-1
    (is (= (-> (parse "{
   dog {
     ...fragmentOne
     }
}
fragment fragmentOne on dog {
   doesKnowCommand(dogCommand: Sit)
}")
               (doc/compile-document* (example-schema))
               (compile-root))
           [{:dog [[:doesKnowCommand 'Sit]]}])))

