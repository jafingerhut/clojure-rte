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


(ns clojure-rte.bdd-test
  (:require [clojure-rte.bdd :refer :all ]
            [clojure.pprint :refer [cl-format]]
            [clojure.test :refer :all])
  ;; this imports the name of the Bdd record, which is otherwise not imported by :require
  (:import [clojure_rte.bdd Bdd]))

(def num-random-samples 500)

(deftest t-typep
  (testing "bdd and-not"
    (with-bdd-hash []
      (is (bdd-typep 42 (bdd 'Long)))
      (is (not (bdd-typep 42 (bdd 'String))))
      (is (not (bdd-typep "42" (bdd 'Long))))
      (is (bdd-typep "42" (bdd 'String)))
      (is (bdd-typep 42 (bdd-or (bdd 'Long)
                                (bdd 'String))))
      (is (bdd-typep "hello" (bdd-or (bdd 'Long)
                                     (bdd 'String))))
      (is (bdd-typep 42 (bdd-and-not (bdd 'Long)
                                     (bdd 'String))))
      (is (bdd-typep "hello" (bdd-not (bdd 'Long))))
      (is (not (bdd-typep 42 (bdd-and-not (bdd 'String)
                                          (bdd 'Long)))))
      (is (bdd-typep "hello" (bdd-and-not (bdd 'String)
                                          (bdd 'Long)))))))

(deftest t-construct
  (testing "bdd construction"
    (with-bdd-hash []
      (is (= false (bdd 'Long false false)))
      (is (= true (bdd 'Long true true)))
      (is (bdd 'Long true false))
      (is (bdd 'Long false true))
      (let [long (bdd 'Long true false)
            string (bdd 'String true false)
            double (bdd 'Double true false)]
        (is (= string (bdd 'Long string string)))
        (is (bdd 'Long string double))
        (is (bdd 'Long double string))))))

(deftest t-commutativity
  (testing "testing Boolean operations commutativity"
    (with-bdd-hash []
      (doseq [n (range num-random-samples)
              :let [bdd1 (gen-random)
                    bdd2 (gen-random)]]
        (is (= (bdd-or bdd1 bdd2)
               (bdd-or bdd2 bdd1)))
        (is (= (bdd-and bdd1 bdd2)
               (bdd-and bdd2 bdd1)))))))

(deftest t-associativity
  (testing "testing Boolean operations associativity"
    (with-bdd-hash []
      (doseq [n (range num-random-samples)
              :let [bdd1 (gen-random)
                    bdd2 (gen-random)
                    bdd3 (gen-random)]]
        (is (= (bdd-or (bdd-or bdd1 bdd2) bdd3)
               (bdd-or bdd1 (bdd-or bdd2 bdd3))))
        (is (= (bdd-and (bdd-and bdd1 bdd2) bdd3)
               (bdd-and bdd1 (bdd-and bdd2 bdd3))))
))))


(deftest t-identities
  (testing "testing Boolean identies"
    (with-bdd-hash []
      (is (= (bdd-or true false)
             true))
      (is (= (bdd-or false false)
             false))
      (is (= (bdd-or false true)
             true))
      (is (= (bdd-or true true)
             true))

      (is (= (bdd-and true false) false))
      (is (= (bdd-and false false) false))
      (is (= (bdd-and false true) false))
      (is (= (bdd-and true true) true))

      (is (= (bdd-and-not true true) false))
      (is (= (bdd-and-not true false) true))
      (is (= (bdd-and-not false true) false))
      (is (= (bdd-and-not false false) false))

      (is (= (bdd-not true) false))
      (is (= (bdd-not false) true)))))

(deftest t-idempotence
  (testing "testing Boolean idempotence"
    (with-bdd-hash []
      (doseq [n (range num-random-samples)
              :let [bdd (gen-random)]]

        (is (= bdd (bdd-and bdd bdd)))
        (is (= bdd (bdd-or bdd bdd)))
        (is (= false (bdd-and-not bdd bdd)))

        (is (= bdd (bdd-and bdd true)))
        (is (= true (bdd-or bdd true)))
        (is (= false (bdd-and-not bdd true)))
        (is (= (bdd-not bdd) (bdd-and-not true bdd)))

        (is (= (bdd-and bdd false) false))
        (is (= (bdd-or bdd false) bdd))
        (is (= (bdd-and-not bdd false) bdd))
        (is (= (bdd-and-not false bdd) false))))))


(deftest t-de-morgan
  (testing "bdd de morgan's theorem"
    (with-bdd-hash []
      (doseq [n (range num-random-samples)
              :let [bdd1 (gen-random)
                    bdd2 (gen-random)]]
        (is (= (bdd-not (bdd-or bdd1 bdd2))
               (bdd-and (bdd-not bdd1) (bdd-not bdd2))))
        (is (= (bdd-not (bdd-and bdd1 bdd2))
               (bdd-or (bdd-not bdd1) (bdd-not bdd2))))))))

(deftest t-or
  (testing "bdd or"
    (with-bdd-hash []
      (for [a [true false]
            b [true false]]
        (is (= (or a b)
               (bdd-or a b)))))))

(deftest t-and
  (testing "bdd and"
    (with-bdd-hash []
      (for [a [true false]
            b [true false]]
        (is (= (and a b)
               (bdd-and a b)))))))

(deftest t-not
  (testing "bdd not"
    (with-bdd-hash []
      (for [a [true false]]
        (is (= (not a)
               (bdd-not a))))
      (doseq [n (range num-random-samples)
              :let [bdd (gen-random)]]
        (is (= bdd (bdd-not (bdd-not bdd)))
    )))))

(deftest t-and-not
  (testing "bdd and-not"
    (with-bdd-hash []
      (for [a [true false]
            b [true false]]
        (is (= (not (and a b))
               (bdd-and a b))))
      (doseq [n (range num-random-samples)
              :let [bdd1 (gen-random)
                    bdd2 (gen-random)]]
        (is (= (bdd-and-not bdd1 bdd2)
               (bdd-and bdd1 (bdd-not bdd2))))))))

(deftest t-dnf
  ;; convert bdd to dnf
  ;; convert dnf back to bdd
  ;; compare them
  (testing "dnf by serialization out and in"
    (with-bdd-hash []
      (doseq [n (range num-random-samples)
              :let [bdd1 (gen-random)
                    serialized (dnf bdd1)
                    bdd2 (bdd serialized)
                    ]]
        (is (= bdd1 bdd2) (cl-format false "dnf serialization failed on ~a" serialized))))))

(deftest t-itenf
  ;; convert bdd to itenf
  ;; convert itenf back to bdd
  ;; compare them
  (testing "itenf by serialization out and in"
    (with-bdd-hash []
      (doseq [_ (range num-random-samples)
              :let [bdd1 (gen-random)
                    serialized (itenf bdd1)
                    bdd2 (bdd serialized)
                    ]]
        (is (= bdd1 bdd2) (cl-format false "itenf serialization failed on ~a" serialized))))))

(deftest t-eq
  (testing "that bdds which are equal are also eq"
    (with-bdd-hash []
      (doseq [_ (range num-random-samples)
              :let [bdd-1 (gen-random)
                    bdd-2 (bdd (itenf bdd-1))
                    bdd-3 (bdd (dnf bdd-1))]]
        (is (identical? bdd-1 bdd-2))
        (is (identical? bdd-1 bdd-3))))))
            

