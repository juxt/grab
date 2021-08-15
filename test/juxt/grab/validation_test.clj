;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.validation-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.grab.alpha.parser :as parser]
   [juxt.grab.alpha.document :refer [executable]]))

(deftest illegal-type-system-definition-test
  (is
   (thrown?
    clojure.lang.ExceptionInfo
    (let [document (parser/parse "scalar Illegal")]
      (executable document)))))
