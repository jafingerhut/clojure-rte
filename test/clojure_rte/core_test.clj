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

(ns clojure-rte.core-test
  (:require [clojure.test :refer :all]
            [clojure-rte.core :refer :all]))


(deftest t-nullable
  (testing "nullable"
    (is (nullable :epsilon))
    (is (not (nullable :empty-set)))
    (is (nullable '(:and :epsilon :epsilon)))
    (is (nullable '(:or :epsilon :empty-set)))
    (is (nullable '(:cat :epsilon :epsilon)))
    (is (not (nullable '(:cat :epsilon :empty-set))))
    (is (not (nullable '(:cat :empty-set :epsilon))))
    (is (nullable '(:+ :epsilon :emptyset)))
    (is (nullable '(:? :epsilon)))))


(deftest t-first-types
  (testing "first-types"
    (is (= #{'a} (first-types 'a)))
    (is (= #{'a 'b} (first-types '(:or a b))))
    (is (= #{'a 'b} (first-types '(:and a b))))
    (is (= #{'b} (first-types '(:cat :epsilon b))))
    (is (= #{'a} (first-types '(:cat a b))))
    (is (= #{'a} (first-types '(:* a))))
    (is (= #{'a} (first-types '(:+ a))))
    (is (= #{'a} (first-types '(:? a))))
    (is (= #{'a} (first-types '(:not a))))
    (is (= #{'a 'b 'c 'e 'f 'g} (first-types '(:and (:or a b)
                                     (:cat c d)
                                     (:* e)
                                     (:+ f)
                                     (:? g)))))
    (is (= #{} (first-types :empty-set)))
    (is (= #{} (first-types :epsilon)))))
