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
            [clojure.test :refer :all :exclude [testing]]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.rte-tester-test))


(defmacro testing
  [string & body]
  `(do (println [:testing ~string :starting (java.util.Date.)])
       (clojure.test/testing ~string ~@body)
       (println [:finished  ~string (java.util.Date.)])
       ))


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

(deftest t-rte-nullable-not-random
  (testing "nullability of :not"
    ;; if an rte is nullable, then (:not rte) is not nullable
    ;; if an rte is not nullable, then (:not rte) is nullable
    (test-rte-not-nullable 100000 7 false)))

(deftest t-rte-nullable-canonicalize-random
  (testing "canonicalize of :not"
    (test-rte-canonicalize-nullable 1000 4 false)))

(deftest t-rte-not-random
  (testing ":not random"
    (clojure-rte.rte-tester/test-rte-not-1 :sigma)
    (clojure-rte.rte-tester/test-rte-not-1 :empty-set)
    (clojure-rte.rte-tester/test-rte-not-1 '(:* :sigma))
    (test-rte-not 500 4 false)))

;; this test is not yet correctly implemented,
;;    need a good way to compare two rtes for equivalence
;; (deftest t-rte-not-not-canonicalize-random
;;   (testing "nullability of :not"
;;     (test-rte-not-not-canonicalize 1000 5 false)))

