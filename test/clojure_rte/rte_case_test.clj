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

(ns clojure-rte.rte-case-test
  (:require [clojure.test :refer :all]
            [clojure-rte.core :refer :all]
            [clojure-rte.rte-case :refer :all]
))

(deftest t-rte-case
  (testing "rte-case"
    (is (= 0 (rte-match
              ;; I don't know why it is necessary to prefix clojure-rte.core/rte-case-helper
              ;; otherwise the loader complains:
              ;; java.lang.RuntimeException: Unable to resolve symbol: rte-case-helper in this context
              (clojure-rte.core/rte-case-helper

               '[[0 (:and (:* Long) (:not (:or)))]
                 [1 (:and (:* Boolean) (:not (:or (:* Long))))]
                 [2 (:and (:* String) (:not (:or (:* Boolean) (:* Long))))]
                 [3
                  (:and
                   (:* :sigma)
                   (:not (:or (:* String) (:* Boolean) (:* Long))))]])
              [1 2 3]))
        "case-0")

    (is (= 1 (rte-match
              (clojure-rte.core/rte-case-helper
               '[[0 (:and (:* Long) (:not (:or)))]
                 [1 (:and (:* Boolean) (:not (:or (:* Long))))]
                 [2 (:and (:* String) (:not (:or (:* Boolean) (:* Long))))]
                 [3
                  (:and
                   (:* :sigma)
                   (:not (:or (:* String) (:* Boolean) (:* Long))))]])
              [true false]))
        "case-1")

    (is (= 2 (rte-match
              (clojure-rte.core/rte-case-helper
               '[[0 (:and (:* Long) (:not (:or)))]
                 [1 (:and (:* Boolean) (:not (:or (:* Long))))]
                 [2 (:and (:* String) (:not (:or (:* Boolean) (:* Long))))]
                 [3
                  (:and
                   (:* :sigma)
                   (:not (:or (:* String) (:* Boolean) (:* Long))))]])
              ["hello" "world"]))
        "case-2")

    (is (= 3 (rte-match
              (clojure-rte.core/rte-case-helper
               '[[0 (:and (:* Long) (:not (:or)))]
                 [1 (:and (:* Boolean) (:not (:or (:* Long))))]
                 [2 (:and (:* String) (:not (:or (:* Boolean) (:* Long))))]
                 [3
                  (:and
                   (:* :sigma)
                   (:not (:or (:* String) (:* Boolean) (:* Long))))]])
              [false "world"]))
        "case-3")))
    
