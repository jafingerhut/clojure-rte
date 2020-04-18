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

(deftest t-sort-operands-4
  (testing "sort length"
    (is (= (sort-operands (list (list 1 2 3) (list 1 2 3 4)))
           (list (list 1 2 3) (list 1 2 3 4))))
    (is (= (sort-operands (list (list 1 2 3 4) (list 1 2 3)))
           (list (list 1 2 3) (list 1 2 3 4))))))
    

(deftest t-sort-operands-3
  (testing "sort-operands 3"
    (let [a-cons `(1 2 3) ;; clojure.lang.Cons
          b-cons `(1 2 3 4) ;; clojure.lang.Cons
          e-cons (seq `(1 2 3 4)) ;; clojure.lang.Cons
          c-cons (seq (map identity '(1 2 3))) ;; clojure.lang.Cons

          d-cons (seq (map identity [1 2 3])) ;; clojure.lang.ChunkedCons

          a-list (list 1 2 3) ;; clojure.lang.PersistentList
          b-list '(1 2 3 4) ;; clojure.lang.PersistentList
          a-vec [1 2 3] ;; clojure.lang.PersistentVector

          a-lazy (map identity (list 1 2 4)) ;; clojure.lang.LazySeq
          b-lazy (map identity [1 2 4]) ;; clojure.lang.LazySeq
          data (list a-cons  
                     c-cons d-cons 
                     a-list a-vec b-cons e-cons b-list 
                     a-lazy b-lazy)]
      (is (= (sort-operands data)
             data)))))

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


(deftest t-sort-operands-5
  (testing "sort-operands 5"
    (sort-operands
     '((:and (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) (:* :sigma)) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) (:* :sigma)) :epsilon) (:and (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat :sigma (:* :sigma)) (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) :epsilon) (:and (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) (:* :sigma)) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) (:* :sigma)) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:* :sigma) :sigma (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) :epsilon) (:and (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) :epsilon (:not Byte) (:not Integer) (:not Long) (:not Short)) (:and (:cat :sigma (:* :sigma)) (:cat :sigma :sigma) :epsilon) (:and (:cat :sigma (:* :sigma)) (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) :sigma) :epsilon) (:and (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat :sigma (:* :sigma)) (:cat :sigma (:* :sigma) :sigma) :epsilon) (:and (:cat :sigma (:* :sigma)) (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat :sigma (:* :sigma)) (:cat (:* :sigma) :sigma (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) :epsilon) (:and (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat :sigma (:* :sigma)) :epsilon (:not Byte) (:not Integer) (:not Long) (:not Short)) (:and (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) :sigma) (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) :sigma) (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat :sigma :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) (:cat :sigma :sigma) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:* :sigma) :sigma (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon (:not Byte) (:not Integer) (:not Long) (:not Short)) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) (:cat :sigma (:* :sigma) :sigma) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat :sigma (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon (:not Byte) (:not Integer) (:not Long) (:not Short)) :empty-set))))
