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

(ns clojure-rte.cl-compat-test
  (:require [clojure-rte.cl-compat :refer [cl-cond cl-prog1 cl-prog2 cl-progn call-with-escape with-escape]]
            [clojure-rte.util :refer [call-with-collector]]
            [clojure.test :refer :all]))


(deftest t-cl-prog1
  (testing "cl-prog1"
    (is (= 1 (cl-prog1 1 2)))
    (is (= 1 (cl-prog1 1 2 3)))
    (is (= 1 (cl-prog1 1 2 3 4)))
    (is (= '(4 3 2 1)
           (call-with-collector (fn [collect]
                                  (cl-prog1 (collect 1)
                                            (collect 2)
                                            (collect 3)
                                            (collect 4))))))))

(deftest t-cl-prog2
  (testing "cl-prog2"
    (is (= 2 (cl-prog2 1 2)))
    (is (= 2 (cl-prog2 1 2 3)))
    (is (= 2 (cl-prog2 1 2 3 4)))
    (is (= '(4 3 2 1)
           (call-with-collector (fn [collect]
                                  (cl-prog2 (collect 1)
                                            (collect 2)
                                            (collect 3)
                                            (collect 4))))))))

(deftest t-cl-progn
  (testing "cl-progn"
    (is (= 1 (cl-progn 1)))
    (is (= 2 (cl-progn 1 2)))
    (is (= 3 (cl-progn 1 2 3)))
    (is (= 4 (cl-progn 1 2 3 4)))
    (is (= '(4 3 2 1)
           (call-with-collector (fn [collect]
                                  (cl-progn (collect 1)
                                            (collect 2)
                                            (collect 3)
                                            (collect 4))))))))
(deftest t-cl-cond
  (testing "cl-cond"
    (let [a 100 b 200]
      (is (= 42 (cl-cond
                 (a 42))) "cond 1")
      (is (= 42 (cl-cond
                 ((= a 1) 41)
                 ((= a 100) 42))) "cond 2")
      (is (not (cl-cond
                 ((= a 1) 41)
                 ((= a 200) 42))) "cond 2b")
      (is (= 100 (cl-cond
                  ((= a 1) 41)
                  (a)
                  (true -1))) "cond 3")

      (is (= -2 (cl-cond
                  ((= a 1) 41)
                  (true -1 -2))) "cond 4")

      (is (= '(3 2 1) (call-with-collector (fn [collect]
                                             (cl-cond
                                              ((= a 1) 41)
                                              (true (collect 1) (collect 2) (collect 3)))))) "cond 5")
      (is (= nil (cl-cond)) "cond nil")
      (is (= 12 (cl-cond (true 12)
                         (nil))) "cond nil termination")
      )))

(deftest t-call-with-escape
  (testing "call-with-escape"
    (is (= 42 (call-with-escape (fn [ret]
                                  (ret 42)))))
    (is (= 43 (call-with-escape (fn [ret]
                                  43))))
    (is (= 44 (call-with-escape (fn [ret]
                                  (ret 44)
                                  45))))
    (is (= 47 (call-with-escape (fn [ret1]
                                  (call-with-escape (fn [ret2]
                                                      (ret2 46)))
                                  (ret1 47)))))
    (is (= 48 (call-with-escape (fn [ret1]
                                  (call-with-escape (fn [ret2]
                                                      (ret1 48)))
                                  (ret1 49)))))))

(deftest t-with-escape
  (testing "with-escape"
    (is (= 42 (with-escape ret
                (ret 42))))
    (is (= 43 (with-escape ret
                43)))
    (is (= 44 (with-escape ret
                (ret 44)
                45)))
    (is (= 47 (with-escape ret1
                (with-escape ret2
                  (ret2 46))
                (ret1 47))))
    (is (= 48 (with-escape ret1
                (with-escape ret2
                  (ret1 48))
                (ret1 49))))))
    
    
