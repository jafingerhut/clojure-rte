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

(deftest t-type-intersection
  (derive ::Canine ::Animal)
  (derive ::Wolf ::Canine)
  (derive ::Fox ::Canine)
  (derive ::Dog ::Canine)
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)
  (derive ::Cat-Lion ::Cat)
  (derive ::Cat-Lion ::Lion)

  (testing "type-intersection"

    (is (= #{::Cat-Lion}  (type-intersection ::Cat ::Lion)))
    (is (= #{::Cat-Lion}  (type-intersection ::Lion ::Cat)))
    (is (= #{}  (type-intersection ::Wolf ::Fox)))
    (is (= #{::Wolf}  (type-intersection ::Wolf ::Animal)))
    (is (= #{::Wolf}  (type-intersection ::Animal ::Wolf)))))

(deftest t-type-min
  (derive ::Canine ::Animal)
  (derive ::Wolf ::Canine)
  (derive ::Fox ::Canine)
  (derive ::Dog ::Canine)
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)
  (derive ::Cat-Lion ::Cat)
  (derive ::Cat-Lion ::Lion)

  (testing "type-min"
    (= ::Lion (type-min [::Lion ::Animal]))))

(deftest t-type-max
  (derive ::Canine ::Animal)
  (derive ::Wolf ::Canine)
  (derive ::Fox ::Canine)
  (derive ::Dog ::Canine)
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)
  (derive ::Cat-Lion ::Cat)
  (derive ::Cat-Lion ::Lion)

  (testing "type-max"
    (is (= ::Animal (type-max [::Lion ::Animal])))))

(deftest t-type-min
  (derive ::Canine ::Animal)
  (derive ::Wolf ::Canine)
  (derive ::Fox ::Canine)
  (derive ::Dog ::Canine)
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)
  (derive ::Cat-Lion ::Cat)
  (derive ::Cat-Lion ::Lion)

  (testing "type-min"
    (is (= ::Lion (type-min [::Lion ::Animal])))))

(deftest t-map-type-partitions
  (testing "map-type-partitions"
    (is (= (set
            (call-with-collector (fn [collect]
                                   (map-type-partitions ['Long 'Integer 'Object]
                                                        (fn [left right]
                                                          (collect [left right]))))))
           #{[() '(Object Integer Long)]
             ['(Object) ()]
             ['(Integer) ()]
             ['(Long) ()]}))))
