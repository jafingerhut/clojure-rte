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

(ns clojure-rte.rte-core
  (:require [clojure.set :refer [union]]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.cl-compat :as cl]
            [clojure-rte.util :refer [with-first-match call-with-collector
                                      defn-memoized
                                      first-repeat
                                      fixed-point
                                      visit-permutations rte-constantly rte-identity
                                      partition-by-pred
                                      print-vals sort-operands member
                                      map-eagerly mapcat-eagerly filter-eagerly
                                      concat-eagerly remove-eagerly]]
            [clojure-rte.genus :as gns]
            [clojure-rte.dfa :as dfa ]
            [clojure-rte.rte-construct]
            [clojure-rte.memoize]
            [clojure-rte.api]
            [clojure-rte.rte-case]
            [clojure-rte.genus-rte]
            ))

;; (defn funny-function [n f]
;;   (let [v (repeat n 12)
;;         w (filter f v)
;;         y (count w)]
;;     (fn [x]
;;       (list x y (count w) (count v)))))

;; (defn test-out-of-memory [n]
;;   (loop [a ()
;;          n 0]
;;     (when (= 0 (mod n 1000))
;;       (println n))
;;     (recur (cons (funny-function n (constantly false)) a)
;;            (inc n))))
