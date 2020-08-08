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
  (:require [clojure-rte.bdd :refer :all]
            [clojure.test :refer :all]))

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
      (is (not (bdd-typep "hello" (bdd-not (bdd 'Long)))))
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
               (bdd-not a)))))
    ))

(deftest t-and-not
  (testing "bdd and-not"
    (with-bdd-hash []
      (for [a [true false]
            b [true false]]
        (is (= (not (and a b))
               (bdd-and a b)))))
    ))


  
 ;; Bdd.withNewBddHash{
 ;;      (1 to 10).foreach { _ =>
 ;;        assert(Or(BddTrue, BddFalse) == BddTrue)
 ;;        assert(Or(BddFalse, BddTrue) == BddTrue)
 ;;        assert(Or(BddTrue, BddFalse) == BddTrue)
 ;;        assert(Or(BddFalse, BddTrue) == BddTrue)
 ;;        assert(Or(BddTrue, BddTrue) == BddTrue)
 ;;        assert(Or(BddFalse, BddFalse) == BddFalse)
 ;;      }
 ;;      AndNot(1,2)
 ;;      AndNot(1,2,3)
 ;;      AndNot(1,2,3,4)
 ;;      AndNot(1,2,3,4,5)
 ;;      assert(Try(AndNot(1)).isFailure)
 ;;      assert(Try(AndNot()).isFailure)
 ;;    }
  ;;   Bdd.withNewBddHash {
  ;;     println(And(1, 2, 3))
  ;;     println(And(1, Or(2, 3, Xor(1, 2, 4), AndNot(3, 4))))
  ;;   }
  ;;   Bdd.withNewBddHash {
  ;;     val bdd3 = Bdd(3)
  ;;     val bdd2 = Bdd(2)
  ;;     Bdd(1, bdd2, bdd3)
  ;;     Bdd(1, bdd2, bdd3)
  ;;     Or(1, 2, -3, And(-1, 4), And(2, Not(Or(1, 3))))

  ;;     Not(Or(Xor(1, And(-2, -3)),
  ;;            AndNot(2, 3)))
  ;;     Not(Or(-2,
  ;;            3,
  ;;            Xor(1, 2, And(-2, -3, 4)),
  ;;            AndNot(2, 3)))
  ;;   }
  ;; }
