;; Copyright © 2021, JUXT LTD.

(ns juxt.grab.parser-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.grab.alpha.parser :as parser]))

;; (parser/parse "{ foo(b: 4) @abc(a: [1 2 3 4.3]) } ")
