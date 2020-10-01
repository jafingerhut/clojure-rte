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

(ns clojure-rte.rte-tester
  (:require [clojure-rte.tester  :as tester]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.core :refer [rte-to-dfa canonicalize-pattern nullable]]
            ))

(defn rte-components [pattern]
  (cond
    (and (seq? pattern)
         (empty pattern))
    ()

    (seq? pattern)
    (let [[keyword & operands] pattern]
      (case keyword
        (:* :+ :? :not
            :and :or :cat :permute) operands
        ;; case else
        ()))

    :else
    ()))

(def ^:dynamic *rte-keywords*
  [:type
   :? :+ :* :not
   :and :or 
   :cat :permute
   :sigma :empty-set :epsilon])

(defn gen-rte
  ([size types]
   (let [key (rand-nth *rte-keywords*)] 
     (gen-rte key size types)))
  ([key size types]
    (case key
      (:type) (rand-nth types)
      (:sigma :empty-set :epsilon) key
      (:permute) (gen-rte :cat size types)
      (:and :or :cat) (cons key (map (fn [k] (gen-rte (dec size) types))
                                              (range size)))
      (:? :+ :* :not) (list key (gen-rte (dec size) types)))))


(def ^:dynamic *test-types*
  '((satisfies integer?)
    (satisfies int?)
    (satisfies rational?)
    (satisfies ratio?)
    (satisfies string?)
    (satisfies keyword?)
    (satisfies symbol?)
    (satisfies decimal?)
    (satisfies float?)
    (satisfies seq?)
    java.io.Serializable
    java.lang.CharSequence
    java.lang.Comparable
    java.lang.Number
    java.lang.Object
    clojure.lang.IMeta
    (= 1)
    (= 0)
    (= a)
    (= [1 2 3])
    (= [])
    (member [1 2 3] [1 2] [1] [])
    (member [1 2 3] [2 1 3])
    (member a b c "a" "b" "c")
    (member a b)
    (member 1 2 3)
    (member 2 3 4)
    (member "a" "b" "c")
    (member "a" "b" "c" 1 2 3)
    (member 1 "a")
    ))

(defn test-rte-to-dfa [num-tries size verbose]
  (tester/random-test num-tries rte-to-dfa
                      (fn [] (gen-rte size *test-types*))
                      rte-components
                      verbose))

(defn test-canonicalize-pattern [num-tries size verbose]
  (tester/random-test num-tries canonicalize-pattern
                      (fn [] (gen-rte size *test-types*))
                      rte-components verbose))


(defn test-rte-not-nullable
  "Run some tests to assure that if an rte r is nullable if and only
  if (:not r) is not nullable."
  [num-tries size verbose]
  (tester/random-test num-tries
                      (fn [rte]
                        (if (nullable rte)
                          (assert (not (nullable (list :not rte)))
                                  (cl-format false
                                             "rte ~A is nullable but its complement (:not ...) is not nullable"
                                             rte))
                          (assert (nullable (list :not rte))
                                  (cl-format false
                                             "rte ~A is not nullable but its complement (:not ...) is nullable"
                                             rte))))                          
                      (fn [] (gen-rte size *test-types*))
                      rte-components
                      verbose))

;; this test is not yet correctly implemented,
;;    need a good way to compare two rtes for equivalence
;; (defn test-rte-not-not-canonicalize
;;   "Run some tests to assure that an rte r is equivalent to
;;   (:not (:not r))"
;;   [num-tries size verbose]
;;   (tester/random-test
;;    num-tries
;;    (fn [rte]
;;      (let [not-1 (canonicalize-pattern `(:not ~rte))
;;            not-2 (canonicalize-pattern `(:not ~not-1))
;;            a-and-not-b (canonicalize-pattern `(:and ~rte (:not ~not-2)))
;;            b-and-not-a (canonicalize-pattern `(:and ~not-2 (:not ~rte)))
;;            ]
;;        (assert (= :empty-set a-and-not-b)
;;                (cl-format false "expecting :empty-set, got a-and-not-b=~A" a-and-not-b))
;;        (assert (= :empty-set b-and-not-a)
;;                (cl-format false "expecting :empty-set, got b-and-not-a=~A" b-and-not-a))))
;;    (fn [] (gen-rte size *test-types*))
;;    rte-components
;;    verbose))

(defn test-rte-canonicalize-nullable
  "Run some tests to assure that if an rte r is nullable if and only
  if (canonicalize-pattern r) is nullable."
  [num-tries size verbose]
  (tester/random-test num-tries
                      (fn [rte]
                        (let [can (canonicalize-pattern rte)]
                          (if (nullable rte)
                            (assert (nullable can)
                                    (cl-format false
                                               "rte ~A is nullable but its canonicalization ~A is not"
                                               rte can))
                            (assert (not (nullable can))
                                    (cl-format false
                                               "rte ~A is not nullable but its canonicalization ~A is nullable"
                                               rte can)))))
                      (fn [] (gen-rte size *test-types*))
                      rte-components
                      verbose))
