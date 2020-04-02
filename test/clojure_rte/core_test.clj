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
    (is (nullable :epsilon) 14)
    (is (not (nullable :empty-set)) 13)
    (is (nullable '(:and :epsilon :epsilon)) 12)
    (is (nullable '(:or :epsilon :empty-set)) 11)
    (is (nullable '(:cat :epsilon :epsilon)) 10)
    (is (not (nullable '(:cat :epsilon :empty-set))) 9)
    (is (not (nullable '(:cat :empty-set :epsilon))) 8)
    (is (nullable '(:cat :epsilon (:* :epsilon))) 7)
    (is (nullable '(:+ :epsilon)) 6)
    (is (not (nullable '(:cat :empty-set (:* :empty-set)))) 5)
    (is (not (nullable '(:cat :empty-set :epsilon))) 4)
    (is (not (nullable :empty-set)) 3)
    (is (not (nullable '(:+ :empty-set))) 2)
    (is (nullable '(:? :epsilon)) 1)))


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

(deftest t-disjoint?
  (testing "disjoint"
   (derive ::Feline ::Animal)
   (derive ::Cat ::Feline)
   (derive ::Lion ::Feline)
   (is (isa? ::Lion ::Animal))
   (is (not (isa? ::Lion ::Cat)))))


(deftest t-canonicalize-pattern-once
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)

  (testing "canonicalize-pattern-once"
    ;; type
    (is (= ::Lion (canonicalize-pattern-once ::Lion)) "canonicalize :type")

    ;; :*
    (is (= (canonicalize-pattern-once '(:* (:* x)))
           '(:* x)) "a** -> a*")
    (is (= '(:* ::Lion) (canonicalize-pattern-once '(:* ::Lion))) "canonicalize :type *")
    (is (= :epsilon (canonicalize-pattern-once '(:* :epsilon))) ":epsilon* -> :epsilon")
    (is (= :epsilon (canonicalize-pattern-once '(:* :empty-set))) ":empty-set* -> :epsilon")
    (is (= '(:* :sigma) (canonicalize-pattern-once '(:* :sigma))) ":sigma* -> :sigma*")
    (is (= '(:* :sigma) (canonicalize-pattern-once '(:* (:* :sigma)))) ":sigma** -> :sigma*")
    (is (= '(:* :sigma) (canonicalize-pattern-once '(:* (:* (:* :sigma))))) ":sigma*** -> :sigma*")

    ;; :cat
    (is (= ::Lion (canonicalize-pattern-once '(:cat ::Lion))) "unary :cat")
    (is (= '(:cat ::Lion ::Lion) (canonicalize-pattern-once '(:cat ::Lion ::Lion))) "binary :cat")
    (is (= '(:cat ::Lion ::Lion ::Lion) (canonicalize-pattern-once '(:cat ::Lion ::Lion ::Lion))) "3-ary :cat")
    (is (= ::Lion (canonicalize-pattern-once '(:cat (:cat ::Lion)))) "recursive cat")
    (is (= '(:cat ::Lion ::Lion) (canonicalize-pattern-once '(:cat (:cat ::Lion) (:cat ::Lion)))) "recursive cat")
    (is (= '(:cat ::Lion ::Lion) (canonicalize-pattern-once '(:cat ::Lion (:cat ::Lion)))) "recursive cat 2")
    (is (= '(:cat ::Lion ::Lion) (canonicalize-pattern-once '(:cat (:cat ::Lion) ::Lion))) "recursive cat 3")
    (is (= '(:cat ::Lion ::Lion ::Lion) (canonicalize-pattern-once '(:cat (:cat ::Lion) (:cat ::Lion) (:cat ::Lion)))) "recursive cat")
    ))
