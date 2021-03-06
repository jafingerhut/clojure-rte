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

(ns clojure-rte.all-test
   (:require  clojure.test
              clojure-rte.api-test
              clojure-rte.bdd-test
              clojure-rte.cl-compat-test
              clojure-rte.dfa-test
              clojure-rte.dot-test
              clojure-rte.rte-test
              clojure-rte.rte-case-test
              clojure-rte.rte-tester-test
              clojure-rte.genus-test
              clojure-rte.util-test             
              )
  (:gen-class))

(defn rte-run-all-test
  "Run all the tests defined in the clojure-rte project"
  []
  (clojure.test/run-tests 'clojure-rte.api-test
                          'clojure-rte.bdd-test
                          'clojure-rte.cl-compat-test
                          'clojure-rte.dfa-test
                          'clojure-rte.dot-test
                          'clojure-rte.rte-test
                          'clojure-rte.rte-case-test
                          'clojure-rte.rte-tester-test
                          'clojure-rte.genus-test
                          'clojure-rte.util-test
                          ))

(defn -main []
  (rte-run-all-test))



