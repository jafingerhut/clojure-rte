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
            [clojure-rte.core :refer :all :exclude [-main]]
            [clojure-rte.rte-case :refer :all]
))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.rte-case-test))

(deftest t-rte-case
  (testing "rte-case"
    (is (= 1 (rte-case '(1 2 3)
                       (:* String) 0
                       (:* Number) 1
                       (:* Long) 2)))
    (is (= 1 (rte-case '(1 2 3)
                       (:* String) 0
                       (:* Long) 1
                       (:* Number) 2)))
    (is (= 2 (rte-case '(1 2 3)
                       (:* String) 0
                       (:* Boolean) 1
                       (:* Long) 2)))

    (is (= 3 (rte-case '(1 "2" false)
                       (:* String) 0
                       (:* Boolean) 1
                       (:* Long) 2
                       (:cat (:* Number) (:+ String) (:* Boolean)) 3
                       (:sigma 4))))))

(deftest t-rte-case-clauses-to-dfa
  (testing "rte-case-clauses-to-dfa"
    (is (= 0 (rte-match
              ;; I don't know why it is necessary to prefix clojure-rte.core/rte-case-clauses-to-dfa
              ;; otherwise the loader complains:
              ;; java.lang.RuntimeException: Unable to resolve symbol: rte-case-clauses-to-dfa in this context
              (clojure-rte.core/rte-case-clauses-to-dfa

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
              (clojure-rte.core/rte-case-clauses-to-dfa
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
              (clojure-rte.core/rte-case-clauses-to-dfa
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
              (clojure-rte.core/rte-case-clauses-to-dfa
               '[[0 (:and (:* Long) (:not (:or)))]
                 [1 (:and (:* Boolean) (:not (:or (:* Long))))]
                 [2 (:and (:* String) (:not (:or (:* Boolean) (:* Long))))]
                 [3
                  (:and
                   (:* :sigma)
                   (:not (:or (:* String) (:* Boolean) (:* Long))))]])
              [false "world"]))
        "case-3")))
    
(deftest t-destructuring-case
  (testing "destructuring-case"
    (is (= 1 (destructuring-case '(true ["hello" 3] true)
                                 [[a [b c] & d]  {a Boolean b String d Boolean}]
                                 1

                                 [[a b]          {a Boolean b (or String Boolean)}]
                                 2))
        "test 1")

    (is (= 1 (destructuring-case '(true ["hello" 3] true)

                                 [[a b]          {a Boolean b (or String Boolean)}]
                                 2
                                 
                                 [[a [b c] & d]  {a Boolean b String d Boolean}]
                                 1


                                 ))
        "test 2")

    (is (= nil (destructuring-case '(true [3 3] true)
                                   [[a [b c] & d]  {a Boolean b String d Boolean}]
                                   1

                                   [[a b]          {a Boolean b (or String Boolean)}]
                                   2))
        "test 3")

    (is (= 1
           (destructuring-case '(true ["hello" xyz] true false true)
                               [[^Boolean a [^String b c] & ^Boolean d]  {}]
                               1 ;; this is returned

                               [[a b]          {a Boolean b (or String Boolean)}]
                               2))
        "test 4")
    (is (= 2
           (destructuring-case '(true ["hello" xyz] true false 1 2 3)
                               [[^Boolean a [^String b c] & ^Boolean d]  {}]
                               1

                               [[^Boolean a [^String b c] & d]  {}]
                               2 ;; this is returned
                               ))
        "test 5")
    (is (= nil
           (destructuring-case '(true ["hello" xyz] true false 1 2 3)
                               [[^Boolean a [^String b c] & ^Boolean d]  {d Number}]
                               1 ;; this is NOT returned

                               [[^Boolean a [^String b c] & ^Number d]  {d Boolean}]
                               2 ;; this is NOT returned
                               ))
        "test 6")

    (is (= 1 (destructuring-case '(true ["3" 3] true)
                                 [[a [b c] & d]  {[a d] Boolean b String}]
                                 1

                                 [[a b]          {a Boolean b (or String Boolean)}]
                                 2))
        "test 7")

    (is (= 1 (destructuring-case '(true ["3" 3] true)
                                 [[a b]          {a Boolean b (or String Boolean)}]
                                 2

                                 [[a [b c] & d]  {[a d] Boolean b String}]
                                 1
                                 ))
        "test 8")

    (is (= 1 (destructuring-case '(true ["3" 3] true)
                                 [[a b]          {a Boolean b (or String Boolean)}]
                                 2

                                 [[a [b c] & d]  {[a d] Boolean a (not Number) b String}]
                                 1
                                 ))
        "test 9")
    (is (= 1 (destructuring-case '(true ["3" 3] true)
                                 [[a b]          {a Boolean b (or String Boolean)}]
                                 2

                                 [[a [b c] & d]  {[a d] (not Number) a Boolean b String}]
                                 1
                                 ))
        "test 10")))

(deftest t-destructuring-fn-1
  (testing "destructuring-fn simple form"
    (is (= 1 
           (let [f 
                 (destructuring-fn
                  [[a [b c] & d]  {a Boolean b String d Boolean}]
                  1)]
                  
             (apply f  '(true ["hello" 3] true))))
        "test 1")))

(deftest t-destructuring-fn-many
  (testing "destructuring-fn-many"
    (is (= 1
           (let [f
                 (destructuring-fn-many
                  ([[a b]          {a Boolean b (or String Boolean)}]
                   2)
                  ([[a [b c] & d]  {a Boolean b String d Boolean}]
                   1))]
             (apply f  '(true ["hello" 3] true))))

        "test 2")))

(deftest t-destructuring-fn-many-2
  ;; some of these were/are getting index invalid
  ;; we run them here to assure that we don't get index invalid.
  (let [f

        (destructuring-fn-many
         ([[a b]          {a Boolean b (or String Boolean)}]
          2)
         ([[a [b c] & d]  {a Boolean b String d Boolean}]
          1))]
     (apply f  '(true ["hello" 3] true))
    )

  (let [f
        (fn [& fn-var-41956]
          (let [v41977 fn-var-41956]
            (rte-case v41977
                      (:cat (and :sigma Boolean) (and :sigma (or String Boolean)))
                      (let [[a b] v41977] (do 2))

                      (:cat
                       (and :sigma Boolean)
                       (rte (:cat (and :sigma String) (and :sigma :sigma)))
                       (:* (:cat (and :sigma Boolean))))
                      (let [[a [b c] & d] v41977] (do 1))

                      (:* :sigma)
                      nil)))]
    (apply f  '(true ["hello" 3] true)))

  (let [f
        (fn [& fn-var-41956]
          (let [v41977 fn-var-41956]
            (([(fn [] (let [[a b] v41977] (do 2)))
               (fn [] (let [[a [b c] & d] v41977] (do 1)))
               (fn [] nil)]
              (let [dfa (memoized-rte-case-clauses-to-dfa
                         '[[0
                            (:and
                             (:cat
                              (and :sigma Boolean)
                              (and :sigma (or String Boolean)))
                             (:not (:or)))]
                           [1
                            (:and
                             (:cat
                              (and :sigma Boolean)
                              (rte (:cat (and :sigma String) (and :sigma :sigma)))
                              (:* (:cat (and :sigma Boolean))))
                             (:not
                              (:or
                               (:cat
                                (and :sigma Boolean)
                                (and :sigma (or String Boolean))))))]
                           [2
                            (:and
                             (:* :sigma)
                             (:not
                              (:or
                               (:cat
                                (and :sigma Boolean)
                                (rte (:cat (and :sigma String) (and :sigma :sigma)))
                                (:* (:cat (and :sigma Boolean))))
                               (:cat
                                (and :sigma Boolean)
                                (and :sigma (or String Boolean))))))]])]
                (rte-match
                 dfa
                 v41977))))))]
    (apply f  '(true ["hello" 3] true)))
  )

(deftest t-destructuring-fn
  (testing "destructuring-fn"
    (is (= nil
           (let [f (destructuring-fn
                    ([[a [b c] & d]  {a Boolean b String d Boolean}]
                     1)

                    ([[a b]          {a Boolean b (or String Boolean)}]
                     2))]
             (apply f  '(true [3 3] true))))
        "test 3")

    (is (= 1
           (let [f (destructuring-fn
                    ([[^Boolean a [^String b c] & ^Boolean d]  {}]
                     1 ;; this is returned
                     )
                    ([[a b]          {a Boolean b (or String Boolean)}]
                     2))]
             (apply f '(true ["hello" xyz] true false true))))
        "test 4")
    (is (= 2
           (let [f (destructuring-fn
                    ([[^Boolean a [^String b c] & ^Boolean d]  {}]
                     1
                     )
                    ([[^Boolean a [^String b c] & d]  {}]
                     2 ;; this is returned
                     ))]
             (apply f '(true ["hello" xyz] true false 1 2 3))))
        "test 5")
    (is (= nil
           (let [f (destructuring-fn
                    ([[^Boolean a [^String b c] & ^Boolean d]  {d Number}]
                     1 ;; this is NOT returned
                     )
                    ([[^Boolean a [^String b c] & ^Number d]  {d Boolean}]
                     2 ;; this is NOT returned
                     ))]
             (apply f '(true ["hello" xyz] true false 1 2 3))))
        "test 6")

    (is (= 1 
           (let [f (destructuring-fn
                    ([[a [b c] & d]  {[a d] Boolean b String}]
                     1)
                    
                    ([[a b]          {a Boolean b (or String Boolean)}]
                     2))]
             (apply f '(true ["3" 3] true))))
        "test 7")

    (is (= 1
           (let [f (destructuring-fn
                    ([[a b]          {a Boolean b (or String Boolean)}]
                     2)
                    ([[a [b c] & d]  {[a d] Boolean b String}]
                     1))]
             (apply f '(true ["3" 3] true))))
        "test 8")

    (is (= 1
           (let [f (destructuring-fn
                    ([[a b]          {a Boolean b (or String Boolean)}]
                     2)
                    ([[a [b c] & d]  {[a d] Boolean a (not Number) b String}]
                     1))]
             (apply f '(true ["3" 3] true))))
        "test 9")
    (is (= 1 
           (let [f (destructuring-fn
                    ([[a b]          {a Boolean b (or String Boolean)}]
                     2)
                    ([[a [b c] & d]  {[a d] (not Number) a Boolean b String}]
                     1))]
             (apply f  '(true ["3" 3] true))))
        "test 10")
    (is (= 1 
           (let [f (destructuring-fn
                    ([[a b]          {a Boolean b (or String Boolean)}]
                     2)
                    ([[a [b c] & ^Boolean d]  {[a d] (not Number) a Boolean b String}]
                     1))]
             (apply f  '(true ["3" 3] true))))
        "test 11")
    (is (= 1 
           (let [f (destructuring-fn
                    ([[a b]          {a Boolean b (or String Boolean)}]
                     2)
                    ([[a [b c] & ^Boolean d]  {[a d] (not Number) a Boolean b String}]
                     1))]
             (apply f  '(true ["3" 3] true false true))))
        "test 12")
    (is (= 3 
           (let [f (destructuring-fn
                    ([[a b]          {a Boolean b (or String Boolean)}]
                     2)
                    ;; TODO, not sure if [[& ^Boolean d] {d (not number)}] works properly.
                    ;;  still need to debug this test case
                    ([[a [b c] & ^Boolean d]  {[a d] (not Number) a Boolean b String}]
                     1)
                    ([[& others] {}]
                     3))]
             (apply f  '(true ["3" 3] true "miss" false true))))
        "test 13")))

(deftest t-destructuring-fn-400
  (testing "special case which was failing 400"
    (let [f (destructuring-fn
             ([[a b]          {a Boolean b (or String Boolean)}]
              2)
             ([[a [b c] & ^Boolean d]  {[a d] (not Number) a Boolean b String}]
              1)
             ([[& others] {}]
              3))]
      (is (= 2 (f true "hello")) "test 1")
      (is (= 2 (f true false)) "test 2")
      (is (= 1 (f "string" [true false])) "test 3")
      (is (= 1 (f "string" [true false] true)) "test 3")
      (is (= 1 (f "string" [true false] true false)) "test 3")

      (is (= 3 (f "string" [true false] true 1 false)) "test 4")
      (is (= 3 (f 3 [true false] true false)) "test 5")
      (is (= 3 (f '(1 2 3))) "test 6"))))

(deftest t-destructuring-fn-374
  (testing "special case which was failing 374"
    (is (= 1
           (let [f (destructuring-fn
                    ([[^Boolean a]  {}]
                     1)
                    ([[a]          {a String}]
                     2))]
             (apply f '(true  ))))
        "test-374")))

(deftest t-destructuring-fn-385
  (testing "special case which was failing 385"
    (is (= 1
           (destructuring-case '(true  )
                               [[^Boolean a]  {}]
                               1
                               [[a]          {a String}]
                               2))
        "test-385")))

(deftest t-rte-match-376
  (testing "special case which was failing 376"
    (is (= 1
           (with-compile-env ()
             (rte-match
              (memoized-rte-case-clauses-to-dfa
               '[[0
                  (:and (:cat Boolean (or String Boolean))
                        (:not (:or)))]
                 [1
                  (:and
                   (:cat
                    Boolean
                    (rte (:cat String :sigma))
                    (:* Boolean))
                   (:not
                    (:cat Boolean (or String Boolean))))]
                 [2
                  (:and
                   (:* :sigma)
                   (:not
                    (:or
                     (:cat
                      (and :sigma Boolean)
                      (rte
                       (:cat String :sigma))
                      (:* Boolean))
                     (:cat
                      Boolean
                      (or String Boolean)))))]])
              '(true ["hello" 3] true)))))))

(deftest t-destructuring-case-402
  (testing "special case which was failing 402"
    (is (= 1 (destructuring-case [1 2 3 4]
                                 [[a b] {}]
                                 2

                                 [[a b & d] {}]
                                 1
                                 )))))

(deftest t-destructuring-fn-401
  (testing "special case which was failing 401"
    (let [f (destructuring-fn
             ([[a b]          {a Boolean b (or String Boolean)}]
              2)
             ([[a [b c] & d]  {d Boolean
                               a (and (not Number) Boolean)
                               b String}]
              1)
             ([[& others] {}]
              3))]
      (is (= 2 (f true "hello")) "test 1")
      (is (= 2 (f true false)) "test 2")
      (is (= 1 (f "string" [true false])) "test 3")
      (is (= 1 (f "string" [true false] true)) "test 3")
      (is (= 1 (f "string" [true false] true false)) "test 3")

      (is (= 3 (f "string" [true false] true 1 false)) "test 4")
      (is (= 3 (f 3 [true false] true false)) "test 5")
      (is (= 3 (f '(1 2 3))) "test 6"))))
