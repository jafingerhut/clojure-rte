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



(ns clojure-rte.type-test
  (:require [clojure-rte.type :refer :all]
            [clojure-rte.util :refer [call-with-collector]]
            [clojure.test :refer :all]))

(deftest t-disjoint?
  (derive ::Canine ::Animal)
  (derive ::Wolf ::Canine)
  (derive ::Fox ::Canine)
  (derive ::Dog ::Canine)
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)
  (derive ::Cat-Lion ::Cat)
  (derive ::Cat-Lion ::Lion)
  (testing "disjoint?"
    (is (not (disjoint? ::Fox ::Animal)))
    (is (not (disjoint? ::Cat ::Lion)))
    (is (disjoint? ::Wolf ::Fox))
    (is (= (set (type-intersection ::Cat ::Lion))
           #{::Cat-Lion}))))


(deftest t-disjoint-2
  (testing "disjoint 2"
    (is (disjoint? 'Integer 'String)) ; final vs final
    (is (disjoint? 'String 'Integer)) ; final vs final

    ;; interface vs interface - never disjoint
    (is (not (disjoint? 'java.lang.constant.Constable 'java.lang.constant.ConstantDesc)))
    (is (not (disjoint? 'java.lang.constant.ConstantDesc 'java.lang.constant.Constable)))

    ;; final vs interface not a superclass
    (is (disjoint? 'Integer 'java.lang.CharSequence))
    (is (disjoint? 'java.lang.CharSequence 'Integer))

    ;; final vs interface is superclass
    (is (not (disjoint? 'Integer 'java.lang.constant.Constable)))
    (is (not (disjoint? 'java.lang.constant.Constable 'Integer)))

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
    (is (= (set
            (call-with-collector (fn [collect]
                                   (map-type-partitions ['Long 'Integer 'Object]
                                                        (fn [left right]
                                                          (collect [left right]))))))
           #{[() '(Object)]
             ['(Integer) ()]
             ['(Long) ()]
             ['(Object) '(Integer Long)]
             ['(Object) ()]}))))
