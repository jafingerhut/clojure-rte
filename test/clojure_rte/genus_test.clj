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

(ns clojure-rte.genus-test
  (:require [clojure-rte.core :refer :all :exclude [-main]]
            [clojure-rte.genus :refer :all]
            [clojure-rte.util :refer [call-with-collector]]
            [clojure.test :refer :all]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.genus-test))

(deftest t-disjoint?
  (when (and (resolve 'java.lang.CharSequence)
             (resolve 'java.io.Serializable)
             (resolve 'java.lang.Comparable))
    (testing "disjoint?"

      (is (not (disjoint? 'java.io.Serializable '(and clojure.lang.Symbol (not (member a b))) (constantly true))) "case-1")
      (is (not (disjoint? 'java.lang.CharSequence 'String)) "case-2")
      (is (not (disjoint? 'java.io.Serializable 'java.lang.Comparable)) "case-3")
      (is (disjoint? 'Integer 'String) "case-4")
      (is (not (disjoint? 'java.lang.Comparable '(not java.io.Serializable))) "case-5")
      (is (not (disjoint? '(and java.lang.Comparable (not clojure.lang.Symbol)) 'java.lang.Object)) "case-6")

      ;; (disjoint? (and A1 A2 .. An) S)
      ;; if Ai is non empty subset of S
      (is (not (disjoint? '(and Long (not (member 2 3 4))) 'java.lang.Comparable)) "case-7")

      ;; disjoint? is not smart enough to determine the following,
      ;; must use bdd-type-disjoint? instead
      ;; (is (not (disjoint? '(and java.lang.Number (not (= 0)) (not (member a b c 1 2 3)))
      ;;                     'java.io.Serializable
      ;;                     (constantly true))) "case-8")

      ;; disjoint? is not smart enough to determine the following,
      ;; must use bdd-type-disjoint? instead
      ;; (is (not (disjoint? 'java.io.Serializable
      ;;                     '(and java.lang.Number (not (= 0)) (not (member a b c 1 2 3)))
      ;;                     (constantly true))) "case-9")

      ;; disjoint? is not smart enough to determine the following,
      ;; must use bdd-type-disjoint? instead
      ;; (is (not (disjoint? '(and Long (not (= 0)) (not (= 1)) (not (member a b c 1 2 3)))
      ;;                     'java.io.Serializable
      ;;                     (constantly true))))

      )))

(deftest t-disjoint-not?
  (when (and (resolve 'java.lang.Number)
             (resolve 'clojure.lang.ISeq))
    (testing "disjoint not"
      (is (disjoint? '(not Boolean) '(not Object) (constantly false))) ;; currently broken
      (is (disjoint? 'Long 'Boolean (constantly false)))
      (is (not (disjoint? '(not Long) '(not Boolean) (constantly true))))
      (is (not (disjoint? 'clojure.lang.ISeq '(not java.lang.Number)))))))

(deftest t-disjoint-2-14
  (if (and (resolve 'java.lang.Comparable)
           (resolve 'clojure.lang.IMeta))   
    (testing "disjoint 2 14"
      ;; interface vs interface - never disjoint
      (is (not (disjoint? 'java.lang.Comparable 'clojure.lang.IMeta)))
      (is (not (disjoint? 'clojure.lang.IMeta 'java.lang.Comparable)))

      ;; final vs interface is superclass
      (is (not (disjoint? 'Integer 'java.lang.Comparable)))
      (is (not (disjoint? 'java.lang.Comparable 'Integer)))
      )))

(deftest t-disjoint-2
  (testing "disjoint 2"

    (is (disjoint? 'Integer 'String)) ; final vs final
    (is (disjoint? 'String 'Integer)) ; final vs final

    ;; final vs interface not a superclass
    (is (disjoint? 'Integer 'java.lang.CharSequence))
    (is (disjoint? 'java.lang.CharSequence 'Integer))

    ;; abstract vs abstract
    (is (disjoint? 'Number 'clojure.lang.ASeq))
    (is (disjoint? 'clojure.lang.ASeq 'Number))

    ;; abstract vs interface
    (is (not (disjoint? 'clojure.lang.IHashEq 'clojure.lang.ASeq)))
    (is (not (disjoint? 'clojure.lang.ASeq 'clojure.lang.IHashEq)))

    ;;clojure.lang.PersistentList java.lang.Object  java.lang.Number
    (is (not (disjoint? 'clojure.lang.PersistentList 'java.lang.Object  )))
    (is (not (disjoint? 'java.lang.Object  'java.lang.Number)))
    (is (disjoint? 'clojure.lang.PersistentList 'java.lang.Number))

    (is (disjoint? 'Long '(not Long)))
    (is (disjoint? '(not Long) 'Long))

    (is (disjoint? 'Long '(not java.io.Serializable)))
    (is (disjoint? '(not java.io.Serializable) 'Long))
    ))

(deftest t-typep
  (testing "typep"
    (is (typep 3 :sigma))
    (is (not (typep 3 :empty-set)))
    (is (typep 3 '(or Long Double)))
    (is (not (typep 3 '(and Long Double))))
    (is (typep 3 '(and Long (not Double))))
    (is (typep 3 '(not String)))
    (is (typep 3 '(= 3)))
    (is (typep 3 '(member 1 2 3 4)))
    (is (typep 3 '(satisfies integer?)))))

(deftest t-type-max
  (testing "type-max"
    (is (= 'Number (type-max '(Number Integer))))
    (is (= 'Number (type-max '(Integer Number))))))

(deftest t-type-min
  (testing "type-min"
    (is (= 'Integer (type-min '(Number Integer))))
    (is (= 'Integer (type-min '(Integer Number))))))

(deftest t-map-type-partitions
  (testing "map-type-partitions"
    (is (not (contains? (set
                         (call-with-collector (fn [collect]
                                                (map-type-partitions ['Long 'Integer 'Object]
                                                                     (fn [left right]
                                                                       (collect [left right]))))))
                        ['(Object) ()])) "should not contain 1")
    (is (= (set
            (call-with-collector (fn [collect]
                                   (map-type-partitions ['Long 'Integer 'Object]
                                                        (fn [left right]
                                                          (collect [left right]))))))
           #{[() '(Object)]
             ['(Integer) ()]
             ['(Long) ()]
             ['(Object) '(Integer Long)]})
        "expected content"
)))

(deftest t-disjoint-member
  (testing "disjoint member"
    (is (disjoint? '(member 1 2 3) '(member 5 6 )) "line 139")
    (is (not (disjoint? '(member 1 2 3) '(member 3 6 ))) "line 140")
    (is (disjoint? '(= 1) '(= 2)) "line 141")
    (is (disjoint? '(= 1) '(member 2 3 4)) "line 142")
    (is (disjoint? '(member 2 3 4) '(= 1)) "line 143")
    (is (not (disjoint? '(= 3) '(member 2 3 4))) "line 144")
    (is (not (disjoint? '(member 2 3 4) '(= 3))) "line 145")

    (is (disjoint? '(member 1 2 3) '(not Long))  "line 147")
    (is (not (disjoint? '(member 1 "2" 3) '(not Long))) "line 148")

    (is (not (disjoint? '(member a b c 1 2 3) '(not (member 1 2 3)))) "line 150")

    (is (disjoint? '(member 1 2 3) '(not (member a b c 1 2 3))) "line 152")
    
    (is (disjoint? 'Long '(not Long)) "line 154")
    (is (disjoint? '(not Long) 'Long) "line 155")

    (is (disjoint? 'String '(member 1 2 3)))
    (is (disjoint? '(member 1 2 3) 'String))

    (is (not (disjoint? 'String '(not (member 1 2 3))
                        (constantly true))))
    (is (not (disjoint? 'Long '(not (member 1 2 3))
                        (constantly true))))
    (is (not (disjoint? 'Object '(not (member 1 2 3))
                        (constantly true))))
    (is (not (disjoint? 'Object '(not (= 0))
                        (constantly true))))
    (is (not (disjoint? 'Long '(not (= 0))
                        (constantly true))))
    (is (not (disjoint? 'java.lang.CharSequence '(not (member a b c a b c))
                        (constantly true))))
    (is (not (disjoint? '(member 3 2)
                        '(member 3 4)
                        (constantly true))))
    (is (not (disjoint? '(member 3 2)
                        '(not (member 3 4))
                        (constantly true))))
    (is (not (disjoint? '(member [1 2 3] [1 2] [1] [])
                        '(not (member [1 2 3] [2 1 3]))
                        (constantly true))))
    (is (not (disjoint? '(and String (not (member a b c 1 2 3)))
                        'java.lang.Comparable
                        (constantly true))))
    ))

(deftest t-subtype?
  (testing "subtype?"
    ;; adding failing test, TODO need to fix
    (is (subtype? 'Long '(not Double) (constantly false)))

    (is (not (subtype? '(not Long) '(not Boolean) (constantly true))))
    (is (not (subtype? '(not Boolean) '(not Long) (constantly true))))
    
    (is (not (subtype? 'Long '(member 1 2 3) (constantly true))))
    (is (not (subtype? 'Long '(member 1 1.2 1.3) (constantly true))))
    (is (not (subtype? 'Long '(member 1.1 1.2 1.3) (constantly true))))

    (is (not (subtype? 'Long '(not (member 1 2 3)) (constantly true))))
    (is (not (subtype? 'Long '(not (member 1 1.2 1.3)) (constantly true))))
    (is (subtype? 'Long '(not (member 1.1 1.2 1.3)) (constantly false)))
    
    (is (subtype? '(member 1 2) '(member 0 1 2 3)))
    (is (not (subtype? '(member 0 1 2 3) '(member 1 2))))
    (is (subtype? '(member 1 2) 'Long) "line 154")
    (is (not (subtype? '(member 1 2) 'String)) "line 155")
    (is (not (subtype? '(member 1 2) '(not Long))) "line 156")
    (is (subtype? '(member 1 2) '(not String)) "line 157")))
  

(deftest t-inhabited
  (testing "inhabited?"
    (with-compile-env ()

      (is (inhabited? 'Long))
      (is (inhabited? '(not Long)))
      (is (inhabited? 'Object))
      (is (not (inhabited? '(not Object))))
      (is (inhabited? '(rte (:+ Number))))
      (is (not (inhabited? '(rte (:and (:+ Number)
                                       (:+ String)))))))))

(deftest t-expand-satisfies
  (testing "expand-satisfies"
    (is (= (expand-satisfies nil)
           nil) "test 0")
    (is (= (expand-satisfies 'x)
           'x) "test 1")

    (is (= (expand-satisfies '(satisfies))
           '(satisfies)) "test 2")

    (is (= (expand-satisfies '(satisfies no-such-function))
           '(satisfies no-such-function)) "test 3")

    (is (= (expand-satisfies '(satisfies list?))
           'clojure.lang.IPersistentList) "test 4")

    (is (= (expand-satisfies '(satisfies integer?))
           '(or Integer Long clojure.lang.BigInt BigInteger Short Byte))
        "test 5")
    
    (is (= (expand-satisfies '(satisfies rational?))
           '(or
             (or Integer Long clojure.lang.BigInt BigInteger Short Byte)
             clojure.lang.Ratio
             BigDecimal))
        "test 6")))

(defn test-predicate [x]
  (> x 10))

(deftest t-satisfies-ns
  (testing "satifies in namespace"
    (is (not (typep 3 '(satisfies clojure-rte.genus-test/test-predicate)))
        "test 0")

    (is (typep 13 '(satisfies clojure-rte.genus-test/test-predicate))
        "test 1")

    ;; cannot use satisfies with undecorated function name
    ;;   from local name space.
    (is (thrown? Exception (typep 13 '(satisfies test-predicate))))))
  
    
