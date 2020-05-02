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
            [clojure-rte.core :refer [rte-to-dfa canonicalize-pattern]]
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
  '(integer? int? rational? ratio? string? keyword? symbol? decimal? float? seq?
             java.io.Serializable
             java.lang.CharSequence
             java.lang.Comparable
             java.lang.Number
             java.lang.Object
             clojure.lang.IMeta
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
