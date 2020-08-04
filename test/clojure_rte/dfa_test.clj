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


(ns clojure-rte.dfa-test
  (:require [clojure-rte.core :refer :all]
            [clojure-rte.dfa :refer :all]
            [clojure.test :refer :all]))

(deftest t-split-eqv-class
  (testing "split-eqv-class"
    (is (= (split-eqv-class #{1 2 3 4 5 6 7} even?)
           #{#{1 3 5 7}
             #{2 4 6}}))
    (is (= (split-eqv-class #{1 2 3 4 5 6 7 8 9} #(mod % 3))
           #{#{2 5 8}
             #{1 4 7}
             #{3 6 9}}))))

(deftest t-find-eqv-class
  (testing "find-eqv-class"
    (is (= (find-eqv-class [#{1 2 3} #{4 5 6} #{7 8 9}]
                           7)
           #{7 8 9}))
    (is (= (find-eqv-class [#{1 2 3} #{4 5 6} #{7 8 9}]
                           8)
           #{7 8 9}))
    (is (= (find-eqv-class [#{1 2 3} #{4 5 6} #{7 8 9}]
                           2)
           #{1 2 3}))
    (is (= (find-eqv-class #{#{1 2 3} #{4 5 6} #{7 8 9}}
                           2)
           #{1 2 3}))))


(deftest t-minimize
  (testing "minimize"
    ;; first example, a dfa already minimum
    (let [dfa1 (rte-to-dfa '(:or (:* Number)
                                 (:cat String Number)
                                 (:* Double)))
          dfa2 (minimize dfa1)]
      (is (= 6 (count (states-as-seq dfa1))))
      (is (= 6 (count (states-as-seq dfa2)))))))

(deftest t-minimize-runs
  (testing "that minimize runs"
    (doseq [rte ['(:* Long)
                 '(:or (rte (:* Number)) 
                                (rte (:cat Double Number))
                                (rte (:* Double)))
                 '(:or (rte (:* Number)) 
                                (rte (:cat String Number))
                                (rte (:* Double)))]]
      (rte-to-dfa rte)
      (minimize (rte-to-dfa rte)))))

(deftest t-trim-runs
  (testing "that trim runs"
    (doseq [rte ['(:* Long)
                 '(:or (rte (:* Number)) 
                                (rte (:cat Double Number))
                                (rte (:* Double)))
                  '(:or (rte (:* Number)) 
                                (rte (:cat String Number))
                                (rte (:* Double)))]]
      (rte-to-dfa rte)
      (trim (rte-to-dfa rte))
      (trim       (minimize (rte-to-dfa rte))))))
