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

(ns clojure-rte.util-test
  (:require [clojure-rte.util :refer :all]
            [clojure.test :refer :all]))

(deftest t-sort-operands-1
  (testing "sort-operands"
    (is (= (sort-operands '(::Cat ::Lion))
           '(::Cat ::Lion)))
    (is (= (sort-operands '(::Lion ::Cat))
           '(::Cat ::Lion)))
    (is (= (sort-operands '((:not ::Cat) (:not ::Lion)))
           '((:not ::Cat) (:not ::Lion))))
    (is (= (sort-operands '((:not ::Lion) (:not ::Cat)))
           '((:not ::Cat) (:not ::Lion))))))
    
(deftest t-sort-operands-2
  (testing "sort-operands 2"
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          (:* :sigma)
                          :sigma
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          (:* :sigma)))
    
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          (:* :sigma)
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)

                          :sigma
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          :sigma (:* :sigma)))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma))) (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma) :sigma :sigma :sigma (:* :sigma)
                          :sigma :sigma (:* :sigma) :sigma (:or :epsilon :sigma)
                          (:* :sigma) (:or (:and :sigma BigInteger) (:and :sigma Byte)
                                           (:and :sigma Integer) (:and :sigma Long)
                                           (:and :sigma Short) (:and :sigma clojure.lang.BigInt))))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma))) (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma) :sigma :sigma :sigma (:* :sigma) :sigma (:* :sigma)
                          :sigma (:or :epsilon :sigma) (:* :sigma)
                          (:or (:and :sigma BigInteger)
                               (:and :sigma Byte) (:and :sigma Integer)
                               (:and :sigma Long) (:and :sigma Short)
                               (:and :sigma clojure.lang.BigInt)) :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma))) (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma) :sigma :sigma :sigma :sigma (:* :sigma) :sigma (:* :sigma)
                          :sigma (:or :epsilon :sigma) (:* :sigma)
                          (:or (:and :sigma BigInteger)
                               (:and :sigma Byte) (:and :sigma Integer)
                               (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer)
                               (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma :sigma
                          (:* :sigma)
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer)
                               (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          :sigma
                          :sigma
                          (:* :sigma)))
    ))



(deftest t-visit-permuations
  (testing "visit-permutations"
    (is (= (set (call-with-collector
                 (fn [collect]
                   (visit-permutations collect '()))))
           #{()}))
    (is (= (set (call-with-collector
                 (fn [collect]
                   (visit-permutations collect '(1)))))
           #{'(1)}))
    (is (= (set (call-with-collector
                 (fn [collect]
                   (visit-permutations collect '(1 2)))))
           (set '((1 2)
                  (2 1)))))
    (is (= (set (call-with-collector
                 (fn [collect]
                   (visit-permutations collect '(1 2 3)))))
           (set '((1 2 3)
                  (1 3 2)
                  (2 1 3)
                  (2 3 1)
                  (3 1 2)
                  (3 2 1)))))
    ))
    
(deftest t-call-with-collector
  (testing "call-with-collector"
    (is (= (call-with-collector (fn [collect]
                                  (collect 1)
                                  (collect 3)
                                  (collect 2)))
           '(2 3 1)))
    (is (= (call-with-collector (fn [collect]
                                  ))
           ()))))

(deftest t-remove-once
  (testing "remove-once"
    (is (= (remove-once 4 '(1 2 3))
           '(1 2 3)))
    (is (= (remove-once 1 '())
           ()))

    (is (= (remove-once 1 '(1 2 3))
           '(2 3)))

    (is (= (remove-once 1 '(1 1 2 3))
           '(1 2 3)))

    (is (= (remove-once 1 '(1 2 1 3))
           '(2 1 3)))

    (is (= (remove-once 2 '(1 2 1 2 3 2))
           '(1 1 2 3 2)))
    ))

