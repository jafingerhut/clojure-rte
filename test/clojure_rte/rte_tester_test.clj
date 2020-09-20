;; Copyright (c) 2020 EPITA Research and Development Laboratory
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(ns clojure-rte.rte-tester-test
  (:require [clojure-rte.rte-tester :refer :all]
            [clojure.test :refer :all]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.rte-tester-test))

(deftest t-test-canonicalize-pattern
  (testing "test-canonicalize-pattern"
    (test-canonicalize-pattern 10 4 false)))

(deftest t-rte-keywords
  (testing "rte-keywords"
    (for [k *rte-keywords*]
      (gen-rte k 4 *test-types*))))

(deftest t-rte-components
  (testing "rte-components"
    (for [k *rte-keywords*]
      (rte-components (gen-rte k 4 *test-types*)))))

(deftest t-rte-to-dfa-random
  (testing "rte-to-dfa random"
    (test-rte-to-dfa 30 5 false)))
