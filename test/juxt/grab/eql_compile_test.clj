
(ns juxt.grab.eql-compile-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.schema :as schema]
   [juxt.grab.alpha.document :as doc]
   [juxt.grab.alpha.parser :refer [parse]]
   [juxt.grab.alpha.compile :refer [compile-root]]
   [clojure.java.io :as io]))


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
  (is (= (-> (parse "{ dog { name }}")
             (doc/compile-document* (example-schema))
             (compile-root))
         [{:dog [:name]}])))


(deftest join-test-1
  (is (= (-> (parse "
query getOwnerName {
  dog {
  name
    owner {
      name
    }
  }
}
")
             (doc/compile-document* (example-schema))
             (compile-root))
         [{:dog [:name {:owner [:name]}]}])))


(deftest arguments-test-1
  (is (= (-> (parse "query WithBarkVolume {
  dog(barkVolume: 12) {
   name
}
}")
             (doc/compile-document* (example-schema))
             (compile-root))
         [{'(:dog {:barkVolume 12}) [:name]}])))

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
             (compile-root))
         [{:dog [:name {:owner [:name]}]}])))
