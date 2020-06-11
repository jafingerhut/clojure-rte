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

;; TODO
;; *  since there is no (not ...) type in clojure, the (:not ) rte must
;;    be especially handled in the automaton construction.
;;
;; * fully implement (satisfies)
;;
;; * allow (= ...) type to be used in an RTE.
;;   E.g., (rte-match '(:* (:or (= true) (= false))) [true true false false false])
;;
;; * test multiple (rte ) types used together,   rte-trace of pattern containing (rte) type
;;
;; * optimize (rte ) types used together, with intersection and and-not operations.
;;
;; * there are many functions in clojure core such as
;;      (defn double?
;;        "Return true if x is a Double"
;;        {:added "1.9"}
;;        [x] (instance? Double x))
;;   I would like to parse these and build entries in *rte-known* programmatically
;; 

(ns clojure-rte.core
  (:require [clojure.set :refer [union]]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.cl-compat :refer [cl-cond]]
            [clojure-rte.util :refer [with-first-match call-with-collector
                                      visit-permutations rte-constantly rte-identity
                                      partition-by-pred
                                      sort-operands member]]
            [clojure-rte.type :as ty]
            [clojure-rte.rte]
            [clojure-rte.memoize]
            [clojure-rte.api]
            [clojure-rte.type-extend]
            )
  (:gen-class))

