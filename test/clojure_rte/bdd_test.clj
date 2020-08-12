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
            [clojure-rte.util :refer [print-vals member]]
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

(deftest t-dnf-previously-failed
  (testing "dnf test which previously failed"
    (with-bdd-hash []
      (doseq [td '[(or (and (not java.io.Serializable) java.lang.Comparable) 
                       (and (not String)               java.lang.Comparable)
                       (and (not Character) String)
                       Character)
                   (or (and (not String)               java.lang.Comparable) 
                       (and (not Character) String)
                       Character)
                   (or (and (not java.io.Serializable) java.lang.Comparable) (and (not Long) java.lang.Comparable) Long)
                   (or (and (not Long) java.lang.Comparable) Long)
                   (or (and (not java.io.Serializable) java.lang.Comparable) (and (not String) java.lang.Comparable) String)
                   (or (and (not String) java.lang.Comparable) String)
                   (or (not java.io.Serializable) (and java.io.Serializable (not Long)) (and (not Short) Long) Short)
                   (or (and (not Boolean) (not Double)) (and (not Boolean) Double) Boolean) 
                   ]
              :let [bdd-1 (bdd td)
                    serialized-1 (dnf bdd-1)
                    bdd-2 (bdd serialized-1)
                    serialized-2 (dnf bdd-2)]]
        (is (bdd-type-subtype? serialized-1 serialized-2)
            (cl-format false "failed 1 <: 2, dnf serialization failed on ~A: ~A != ~A"
                       td
                       serialized-1
                       serialized-2))
        (is (bdd-type-subtype? serialized-2 serialized-1)
            (cl-format false "failed 2 <: 1, dnf serialization failed on ~A: ~A != ~A"
                       td
                       serialized-1
                       serialized-2))))))
                   
(deftest t-dnf
  ;; convert bdd to dnf
  ;; convert dnf back to bdd
  ;; compare them
  (testing "dnf by serialization out and in"
    (with-bdd-hash []
      (let [bdd (bdd '(and
                       (not Long)
                       (and (not Long)
                            (not Boolean))))]
        (is (member '(not Long) (dnf bdd)))
        (is (member '(not Boolean) (dnf bdd))))
               
      (doseq [n (range num-random-samples)
              :let [bdd1 (gen-random)
                    serialized-1 (dnf bdd1)
                    bdd2 (bdd serialized-1)
                    serialized-2 (dnf bdd2)
                    ]]
        (is (bdd-type-subtype? serialized-1 serialized-2)
            (cl-format false "failed: serialized-1 <: serialized-2, dnf serialization failed on ~a, ~A != ~A"
                       bdd1
                       serialized-1
                       serialized-2
                       ))
        (is (bdd-type-subtype? serialized-2 serialized-1)
            (cl-format false "failed: serialized-2 <: serialized-1, dnf serialization failed on ~a, ~A != ~A"
                       bdd1
                       serialized-1
                       serialized-2
                       ))))))

(deftest t-itenf
  ;; convert bdd to itenf
  ;; convert itenf back to bdd
  ;; compare them
  (testing "itenf by serialization out and in"
    (with-bdd-hash []
      (doseq [_ (range num-random-samples)
              :let [bdd1 (gen-random)
                    dnf-1 (dnf bdd1)
                    serialized (itenf bdd1)
                    bdd2 (bdd serialized)
                    dnf-2 (dnf bdd2)
                    ]]
        (is (= dnf-1 dnf-2) (cl-format false "itenf serialization failed on ~a : ~a, ~A != ~A"
                                       bdd1 serialized
                                       dnf-1 dnf-2
                                       ))))))

;; (deftest t-eq
;;   (testing "that bdds which are equal are also eq"
;;     (with-bdd-hash []
;;       (doseq [_ (range num-random-samples)
;;               :let [bdd-1 (gen-random)
;;                     bdd-2 (bdd (itenf bdd-1))
;;                     bdd-3 (bdd (dnf bdd-1))]]
;;         (is (identical? bdd-1 bdd-2))
;;         (is (identical? bdd-1 bdd-3))))))
            
(deftest t-bdd-type-disjoint-1
  (testing "disjoint checks for types"
    (with-bdd-hash []
      (let [type1 '(and Number (not (= 0)) (not (member a b c 1 2 3)))
            type2 'java.io.Serializable
            bdd1 (bdd  type1)
            bdd2 (bdd type2)]
        (is (bdd-and bdd1 bdd2)) ;; not false
        (is (= :empty-set (dnf (bdd-and bdd1 (bdd-not bdd2)))))

        (is (not (bdd-disjoint? bdd1 bdd2)))
        (is (bdd-disjoint? bdd1 (bdd-not bdd2)))

        (is (not (bdd-type-disjoint? type1 type2)))
        (is (bdd-type-disjoint? type1 (list 'not type2)))))))

(deftest t-bdd-type-disjoint-2
  (when (and (resolve 'java.lang.CharSequence)
             (resolve 'java.io.Serializable)
             (resolve 'java.lang.Comparable))
    (testing "bdd-type-disjoint?"
    (with-bdd-hash []
      (is (not (bdd-type-disjoint? 'java.io.Serializable '(and clojure.lang.Symbol (not (member a b))))))
      (is (not (bdd-type-disjoint? 'java.lang.CharSequence 'String)))
      (is (not (bdd-type-disjoint? 'java.io.Serializable 'java.lang.Comparable)))
      (is (bdd-type-disjoint? 'Integer 'String))
      (is (not (bdd-type-disjoint? 'java.lang.Comparable '(not java.io.Serializable))))
      (is (not (bdd-type-disjoint? '(and java.lang.Comparable (not clojure.lang.Symbol)) 'java.lang.Object)))

      ;; (bdd-type-disjoint? (and A1 A2 .. An) S)
      ;; if Ai is non empty subset of S
      (is (not (bdd-type-disjoint? '(and Long (not (member 2 3 4))) 'java.lang.Comparable)))

      (is (not (bdd-type-disjoint? '(and java.lang.Number (not (= 0)) (not (member a b c 1 2 3)))
                          'java.io.Serializable)))
      (is (not (bdd-type-disjoint? 'java.io.Serializable
                          '(and java.lang.Number (not (= 0)) (not (member a b c 1 2 3))))))
      ))))

(deftest t-bdd-canonicalize-type
  (testing "bdd-canonicalize-type"
    (is (member (bdd-canonicalize-type (list 'and
                                            '(not Long)
                                            '(and (not Long)
                                                  (not Boolean))))
               '((and (not Long) (not Boolean))
                 (and (not Boolean) (not Long)))))))
