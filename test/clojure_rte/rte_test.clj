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

(ns clojure-rte.rte-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.cl-compat :refer [cl-cond]]
            [clojure-rte.util :refer [sort-operands remove-once call-with-collector visit-permutations]]
            [clojure-rte.type :refer [disjoint? typep inhabited?]]
            [clojure-rte.core :refer :all :exclude [-main]]
            [clojure-rte.rte-tester :refer :all]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.rte-test))

(deftest t-nullable
  (testing "nullable"
    (is (not (nullable :sigma)) "nullable sigma")
    (is (nullable :epsilon) "14")
    (is (not (nullable :empty-set)) "13")
    (is (nullable '(:and :epsilon :epsilon)) "12")
    (is (nullable '(:or :epsilon :empty-set)) "11")
    (is (nullable '(:cat :epsilon :epsilon)) "10")
    (is (not (nullable '(:cat :epsilon :empty-set))) "9")
    (is (not (nullable '(:cat :empty-set :epsilon))) "8")
    (is (nullable '(:cat :epsilon (:* :epsilon))) "7")
    (is (nullable '(:+ :epsilon)) "6")
    (is (not (nullable '(:cat :empty-set (:* :empty-set)))) "5")
    (is (not (nullable '(:cat :empty-set :epsilon))) "4")
    (is (not (nullable :empty-set)) "3")
    (is (not (nullable '(:+ :empty-set))) "2")
    (is (nullable '(:? :epsilon)) "1")))



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
    (is (= #{} (first-types :epsilon)))

    (is (= (first-types '(:cat :sigma ::Lion ::Wolf))
           #{:sigma}) "first types cat sigma")
    ))

(deftest t-canonicalize-pattern-subtypes
  (testing "canonicalize-pattern with subtypes"
    (is (= 'Number (canonicalize-pattern '(:or Integer Number))) "Number")
    (is (= :sigma (canonicalize-pattern '(:or Number (:not Number)))) "sigma")

    (is (= 'Integer (canonicalize-pattern '(:and Integer Number))) "Integer")
    (is (= :empty-set (canonicalize-pattern '(:and Number (:not Number)))) "empty-set 1")

    ;; intersection of disjoint types
    (is (= :empty-set (canonicalize-pattern '(:and String Integer))) "empty-set 2")
    ))

(deftest t-canonicalize-pattern-14
  (when (and (resolve 'java.lang.Comparable)
             (resolve 'clojure.lang.IMeta))
    
    (testing "canonicalize-pattern 14"
      (is (= '(:or (:and
                    clojure.lang.IMeta
                    clojure.lang.IReduceInit
                    java.io.Serializable)
                   (:and
                    clojure.lang.IMeta
                    clojure.lang.IReduceInit
                    java.lang.Comparable))
             (canonicalize-pattern '(:and (:or java.io.Serializable java.lang.Comparable)
                                          clojure.lang.IMeta clojure.lang.IReduceInit)))
          "and-distribute"))))

(deftest t-canonicalize-pattern-14b
  (when (and (resolve 'java.lang.Comparable))
    (testing "canonicalize-pattern 14b"
      (is (= (canonicalize-pattern '(:permute java.lang.Comparable java.io.Serializable java.lang.Comparable ))
           (canonicalize-pattern '(:or (:cat java.lang.Comparable java.io.Serializable java.lang.Comparable )
                                       (:cat java.lang.Comparable java.lang.Comparable  java.io.Serializable)
                                       (:cat java.io.Serializable java.lang.Comparable java.lang.Comparable )
                                       (:cat java.io.Serializable java.lang.Comparable  java.lang.Comparable)
                                       (:cat java.lang.Comparable  java.io.Serializable java.lang.Comparable)
                                       (:cat java.lang.Comparable java.lang.Comparable java.io.Serializable)))) "permute 3 args"))))


(deftest t-canonicalize-pattern
  (testing "canonicalize-pattern"

    ;; syntax errors
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:*))))
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:not))))
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:?))))
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:+))))

    ;; type
    (is (= 'Number (canonicalize-pattern-once 'Number)) "canonicalize :type")

    ;; :*
    (is (= (canonicalize-pattern-once '(:* (:* Number)))
           '(:* Number)) "a** -> a*")
    (is (= '(:* Number) (canonicalize-pattern-once '(:* Number))) "canonicalize :type *")
    (is (= :epsilon (canonicalize-pattern-once '(:* :epsilon))) ":epsilon* -> :epsilon")
    (is (= :epsilon (canonicalize-pattern-once '(:* :empty-set))) ":empty-set* -> :epsilon")
    (is (= '(:* :sigma) (canonicalize-pattern-once '(:* :sigma))) ":sigma* -> :sigma*")
    (is (= '(:* :sigma) (canonicalize-pattern-once '(:* (:* :sigma)))) ":sigma** -> :sigma*")
    (is (= '(:* :sigma) (canonicalize-pattern-once '(:* (:* (:* :sigma))))) ":sigma*** -> :sigma*")

    ;; :cat
    (is (= (canonicalize-pattern '(:cat)) :epsilon))
    (is (= 'Number (canonicalize-pattern-once '(:cat Number))) "unary :cat")
    (is (= '(:cat Number Number) (canonicalize-pattern-once '(:cat Number Number))) "binary :cat")
    (is (= '(:cat Number Number Number) (canonicalize-pattern-once '(:cat Number Number Number))) "3-ary :cat")
    (is (= 'Number (canonicalize-pattern-once '(:cat (:cat Number)))) "recursive cat")
    (is (= '(:cat Number Number) (canonicalize-pattern-once '(:cat (:cat Number) (:cat Number)))) "recursive cat")
    (is (= '(:cat Number Number) (canonicalize-pattern-once '(:cat Number (:cat Number)))) "recursive cat 2")
    (is (= '(:cat Number Number) (canonicalize-pattern-once '(:cat (:cat Number) Number))) "recursive cat 3")
    (is (= '(:cat Number Number Number) (canonicalize-pattern-once '(:cat (:cat Number) (:cat Number) (:cat Number)))) "recursive cat")
    (is (= 'Long
           (canonicalize-pattern '(:cat :epsilon Long)))
        "cat epsilon x")
    (is (= 'Long
           (canonicalize-pattern '(:cat Long :epsilon)))
        "cat x epsilon")
    (is (= :empty-set
           (canonicalize-pattern '(:cat :empty-set Long)))
        "cat epsilon x")
    (is (= :empty-set
           (canonicalize-pattern '(:cat Long :empty-set)))
        "cat x epsilon")
    (is (= (canonicalize-pattern '(:cat Number (:* :sigma) (:* :sigma) String))
           '(:cat Number (:* :sigma) String)) "cat sigma* sigma*")

    ;; :not
    (is (= :epsilon (canonicalize-pattern-once '(:not :sigma))) "not sigma")
    (is (= :empty-set (canonicalize-pattern-once '(:not (:* :sigma)))) "not sigma*")
    (is (= (canonicalize-pattern-once '(:+ :sigma))
           (canonicalize-pattern-once '(:not :epsilon))) "not epsilon")
    (is (= '(:* :sigma)
           (canonicalize-pattern-once '(:not :empty-set))) "not empty-set")
    (is (= '(:not Number)
           (canonicalize-pattern-once '(:not Number))) "not type")
    (is (= 'Number
           (canonicalize-pattern-once '(:not (:not Number)))) "not no type")
    (is (= '(:not Number)
           (canonicalize-pattern-once '(:not (:not (:not Number))))) "not not not type")
    ;; :not :and
    (is (= (canonicalize-pattern-once '(:not (:and java.io.Serializable java.lang.Comparable)))
           (canonicalize-pattern-once '(:not (:and java.lang.Comparable java.io.Serializable)))) "not and 1")
    (is (= '(:or (:not java.io.Serializable) (:not java.lang.Comparable))
           (canonicalize-pattern-once '(:not (:and java.lang.Comparable java.io.Serializable)))) "not and 2") ;;  (:not (:and A B)) --> (:or (:not A) (:not B))
    (is (= '(:or (:not java.io.Serializable) (:not java.lang.Comparable))
           (canonicalize-pattern-once '(:not (:and java.io.Serializable java.lang.Comparable)))) "not and 3")

    ;; :not :or
    (is (= (canonicalize-pattern '(:not (:or java.io.Serializable java.lang.Comparable)))
           (canonicalize-pattern '(:not (:or java.lang.Comparable java.io.Serializable)))) "not or 1")
    (is (= '(:and (:not java.io.Serializable)
                  (:not java.lang.Comparable))
           (canonicalize-pattern '(:not (:or java.lang.Comparable java.io.Serializable)))) "not or 2") ;;  (:not (:and A B)) --> (:or (:not A) (:not B))
    (is (= '(:and (:not java.io.Serializable)
                  (:not java.lang.Comparable))
           (canonicalize-pattern-once '(:not (:or java.io.Serializable java.lang.Comparable)))) "not or 3")

    (is (= '(:not java.io.Serializable)
           (canonicalize-pattern '(:not (:or java.io.Serializable java.io.Serializable)))) "not or 4")

    ;; and
    (is (= (canonicalize-pattern '(:and)) :sigma))    
    (is (= 'java.io.Serializable
           (canonicalize-pattern '(:and java.io.Serializable
                                        java.io.Serializable))) "and remove duplicate 1")
    (is (= '(:and java.io.Serializable java.lang.Comparable)
           (canonicalize-pattern-once '(:and java.io.Serializable java.lang.Comparable java.io.Serializable java.lang.Comparable))) "and remove duplicate 2")

    

    (is (= :empty-set
           (canonicalize-pattern '(:and  java.io.Serializable :empty-set java.lang.Comparable))) "and empty-set")
    (is (= '(:and java.io.Serializable java.lang.Comparable)
           (canonicalize-pattern '(:and  java.io.Serializable (:* :sigma) java.lang.Comparable))) "and sigma*")

    ;; or
    (is (= (canonicalize-pattern '(:or)) :empty-set))
    (is (= 'java.io.Serializable
           (canonicalize-pattern '(:or java.io.Serializable java.io.Serializable))) "or remove duplicate 1 b")
    (is (= '(:or java.io.Serializable java.lang.Comparable)
           (canonicalize-pattern '(:or java.lang.Comparable java.io.Serializable java.lang.Comparable java.io.Serializable java.lang.Comparable))) "or remove duplicate 2 b")
    (is (= '(:or  java.io.Serializable java.lang.Comparable)
           (canonicalize-pattern '(:or  java.io.Serializable :empty-set java.lang.Comparable))) "or empty-set")
    (is (= '(:* :sigma)
           (canonicalize-pattern '(:or  java.io.Serializable (:* :sigma) java.lang.Comparable))) "or sigma*")

    ;; permute
    (is (= (canonicalize-pattern '(:permute)) :epsilon) "permute 0 arg")

    (is (= (canonicalize-pattern '(:permute java.lang.Comparable))
           'java.lang.Comparable) "permute 1 arg")
    (is (= (canonicalize-pattern '(:permute java.lang.Comparable java.io.Serializable))
           (canonicalize-pattern '(:or (:cat java.lang.Comparable java.io.Serializable)
                                       (:cat java.io.Serializable java.lang.Comparable)))) "permute 2 args")
    ))

(deftest t-derivative
  (testing "derivative"
    (is (= (derivative :empty-set java.lang.Comparable)
           :empty-set) "derivative empty-set w.r.t A")

    ;; :sigma
    (is (= (derivative :sigma :sigma)
           :epsilon) "derivative sigma wrt sigma")
    (is (= (derivative :sigma :epsilon)
           :sigma) "derivative sigma wrt epsilon")
    (is (= :epsilon
           (derivative :sigma java.lang.Comparable)) "derivative sigma wrt A")

    ;; :epsilon
    (is (= (derivative :epsilon :epsilon)
           :epsilon))
    (is (= (derivative :epsilon java.lang.Comparable)
           :empty-set))
    (is (= (derivative :epsilon :empty-set)
           :empty-set))
    (is (= (derivative :epsilon :sigma)
           :empty-set))

    ;; :empty-set
    (is (= (derivative :empty-set :epsilon)
           :empty-set))
    (is (= (derivative :empty-set java.lang.Comparable)
           :empty-set))
    (is (= (derivative :empty-set :empty-set)
           :empty-set))
    (is (= (derivative :empty-set :sigma)
           :empty-set))

    ;; type
    (is (= (derivative java.lang.Comparable java.lang.Comparable)
           :epsilon))
    (is (= (derivative 'Number 'String)
           :empty-set) "derivative disjoint types")

    ;; or
    
    (is (= (derivative '(:or Double String)
                       'Double)
           :epsilon))

    ;; cat
    (is (thrown? clojure.lang.ExceptionInfo
                 (derivative '(:cat (:or java.io.Serializable java.lang.Comparable) Long)
                             'java.io.Serializable))
        "derivative wrt overlpping type not possible")
    
    (is (= (derivative '(:cat (:or Double String) Long)
                       'Double)
           'Long) "derivative cat with reduction 1")

    (is (= (derivative '(:cat (:or Double String) Long)
                       'String)
           'Long) "derivative cat with reduction 2")

    (is (= (derivative '(:cat Number Number Number)
                       'Number)
           '(:cat Number Number)) "line 237")

    (is (= (derivative '(:cat (:or java.io.Serializable Number) Number Number)
                       'Number)
           '(:cat Number Number))  "line 277")
    (is (= (derivative '(:cat (:or String Number) Number Number)
                       'String)
           '(:cat Number Number))  "line 280")
    ))

(deftest t-rte-to-dfa

  (testing "rte-to-dfa"
    (with-compile-env ()
      (is (rte-to-dfa '(:cat :epsilon (:+ (:* :epsilon)) :sigma)) "dfa 1")
      ;; (is (thrown? clojure.lang.ExceptionInfo
      ;;              (rte-to-dfa '(:permute ::Wolf (:? (:+ :empty-set)) (:+ (:* (:and)))))) "dfa 2")
      )))



(deftest t-syntax
  (testing "syntax"
    (with-compile-env ()
      (is (thrown? clojure.lang.ExceptionInfo (rte-compile '(:* :epsilon :epsilon))))
      (is (thrown? clojure.lang.ExceptionInfo (rte-compile '(:? :epsilon :epsilon))))
      (is (thrown? clojure.lang.ExceptionInfo (rte-compile '(:+ :epsilon :epsilon)))))))

(deftest t-mdtd
  (testing "mdtd"
    (with-compile-env ()
      (is (= (set (mdtd #{:sigma 'java.lang.Exception 'clojure.lang.ExceptionInfo}))
             #{`(~'not java.lang.Exception)
               `(~'and java.lang.Exception (~'not clojure.lang.ExceptionInfo))
               'clojure.lang.ExceptionInfo})))))

(deftest t-boolean-types
  (testing "rte-match with Boolean types"
    (with-compile-env []
      (is (rte-match '(:cat (or Boolean Long)) [42]) "test 1")
      (is (rte-match '(:* (or Boolean Long)) [])  "test 2")
      (is (rte-match '(:* (or Boolean Long)) [42 ])  "test 3")
      (is (rte-match '(:* (or Boolean Long)) [42 43])  "test 4")
      (is (rte-match '(:* (or Boolean Long)) [42 43 false])  "test 5")

      (is (rte-match '(:* (and Number Long (not (= 0)))) [])  "test 6")
      (is (rte-match '(:* (and Number Long (not (= 0)))) [42])  "test 7")
      (is (rte-match '(:* (and Number Long (not (= 0)))) [42 43 ])  "test 8")
      (is (not (rte-match '(:* (:and Number Long (:not (:and :sigma (= 0))))) [42 43 0 44])) "test 9a")
      (is (not (rte-match '(:* (and Number Long (not (= 0)))) [42 43 0 44]))  "test 9b"))))

(deftest t-?
  (testing "rte :?"
    (with-compile-env []
      (is (rte-match '(:? Long) [42]))
      (is (rte-match '(:? Long) []))
      (is (not (rte-match '(:? Long) [42 43])))
      (is (not (rte-match '(:? Long) ["hello"]))))))

(deftest t-+
  (testing "rte :+"
    (with-compile-env []
      (is (rte-match '(:+ Long) [42]))
      (is (rte-match '(:+ Long) [42 43]))
      (is (rte-match '(:+ Long) [42 43 44]))
      (is (not (rte-match '(:+ Long) [])))
      (is (not (rte-match '(:+ Long) ["hello"])))
      (is (not (rte-match '(:+ Long) ["hello" "hello"])))
      (is (not (rte-match '(:+ Long) ["hello" 42])))
      (is (not (rte-match '(:+ Long) [42 "hello"]))))))

(deftest t-permute
  (testing "rte :permute"
    (with-compile-env []
      (is (rte-match '(:permute) []))
      (is (not (rte-match '(:permute) [1])))

      (is (rte-match '(:permute Long) [42]))
      (is (not (rte-match '(:permute Long) [42 43])))
      (is (not (rte-match '(:permute Long) [])))

      (is (rte-match '(:permute Long String) [42 "hello"]))
      (is (rte-match '(:permute Long String) ["hello" 42]))
      (is (not (rte-match '(:permute Long String) [42 "hello" 42])))
      (is (not (rte-match '(:permute Long String) ["hello" 42 42])))
      (is (not (rte-match '(:permute Long String) [])))
      
      (is (rte-match '(:permute Long String Boolean) [42 "hello" false]))
      (is (rte-match '(:permute Long String Boolean) [42 false "hello"]))
      (is (rte-match '(:permute Long String Boolean) [false 42 "hello"]))
      (is (rte-match '(:permute Long String Boolean) [false "hello" 42]))
      (is (rte-match '(:permute Long String Boolean) ["hello" 42 false]))
      (is (rte-match '(:permute Long String Boolean) ["hello" false 42]))
      (is (not (rte-match '(:permute Long String Boolean) [42 "hello"])))
      (is (not (rte-match '(:permute Long String Boolean) [42 false "hello" 42])))
      (is (not (rte-match '(:permute Long String Boolean) [])))
      (is (not (rte-match '(:permute Long String Boolean) [false false "hello"])))
      (is (not (rte-match '(:permute Long String Boolean) ["hello" "hello" 42])))
      (is (not (rte-match '(:permute Long String Boolean) [false]))))))

(deftest t-exp
  (testing "exp"
    (with-compile-env ()
      (is (rte-match '(:exp 3 Long) [42 42 42]))
      (is (not (rte-match '(:exp 3 Long) [42 42 42 42])))
      (is (not (rte-match '(:exp 3 5 Long) [42 42])))
      (is (rte-match '(:exp 3 5 Long) [42 42 42]))
      (is (rte-match '(:exp 3 5 Long) [42 42 42 42]))
      (is (rte-match '(:exp 3 5 Long) [42 42 42 42 42]))
      (is (not (rte-match '(:exp 3 5 Long) [42 42 42 42 42 42])))
      (map (fn [n] 
             (let [data (repeat 12 n)
                   pattern `(:cat (:exp ~n (:? Long)) (:exp ~n Long))
                   rte (rte-compile pattern)]

               (is (rte-match rte data) (format "n=%s" n))))
           (range 10)))))

(deftest t-contains-every
  (testing ":contains-every"
    (with-compile-env ()
      (is (rte-match '(:contains-every Long String Boolean) [[] [] [] 42 "hello" true [] [] []]))
      (is (rte-match '(:contains-every Long String Boolean) [[] "hello" [] 42 [] true []]))
      (is (rte-match '(:contains-every Long String Boolean) [[] true [] 42 [] "hello" []]))
      (is (not (rte-match '(:contains-every Long String Boolean) [[] true [] true [] "hello" []]))))))

(deftest t-contains-any
  (testing ":contains-any"
    (with-compile-env ()
      (is (rte-match '(:contains-any Long String Boolean) [[] [] [] 42 "hello" true [] [] []]))
      (is (rte-match '(:contains-any Long String Boolean) [[] [] [] 42 [] [] []]))
      (is (rte-match '(:contains-any Long String Boolean) [[] "hello" [] 42 []]))
      (is (rte-match '(:contains-any Long String Boolean) [[] true []]))
      (is (not (rte-match '(:contains-any Long String) [[] true [] false []]))))))


(deftest t-contains-none
  (testing ":contains-none"
    (with-compile-env ()
      (is (not (rte-match '(:contains-none Long String Boolean) [[] [] [] 42 "hello" true [] [] []])))
      (is (not (rte-match '(:contains-none Long String Boolean) [[] [] [] 42 [] [] []])))
      (is (not (rte-match '(:contains-none Long String Boolean) [[] "hello" [] 42 []])))
      (is (not (rte-match '(:contains-none Long String Boolean) [[] true []])))
      (is (rte-match '(:contains-none Long String) [[] true [] false []])))))

(deftest t-typep-rte
  (testing "typep rte"
    (with-compile-env ()
      (is (typep [3 3.0 "hello" "world" 3 3.0]
                 '(rte (:cat (:+ (:cat Long Double String))
                             (:+ (:cat String Long Double)))))))))

(deftest t-rte-trace
  (testing "rte trace"
    (with-compile-env ()
      (is (rte-trace (rte-compile '(:* (rte Long))))  "test 2")
      (is (rte-trace (rte-compile '(:* (rte (:* Long)))))  "test 6")
      (is (rte-trace (rte-compile '(:cat (:+ (:cat Long Double String))
                                         (:+ (:cat String Long Double)))))  "test 7")

      (is (rte-trace  '(:* (rte Long)))  "test 12")
      (is (rte-trace  '(:* (rte (:* Long))))  "test 16")
      (is (rte-trace  '(:cat (:+ (:cat Long Double String))
                             (:+ (:cat String Long Double))))  "test 17")
      )))

(deftest t-with-rte-1
  (with-rte [::a (:permute Long Long String)]
    (is (resolve-rte-tag ::a)))
  )

(deftest t-with-rte-2
  (with-rte [::a (:permute Long Long String)]
    (is (resolve-rte-tag ::a))
    (rte-compile '(:cat ::a ::a))))

(deftest t-with-rte-3
  (with-rte [::a (:permute Long Long String)]
      (is (resolve-rte-tag ::a))
      (let [rte (rte-compile '(:cat ::a ::a))]
        (is (rte-match rte [2 2 "hello"
                            4 4 "world"]) "case 1")))
    )

(deftest t-with-rte
  (with-rte [::a (:permute Long Long String)]
    (is (resolve-rte-tag ::a))
    (let [rte (rte-compile '(:cat ::a ::a))]
      (is (rte-match rte [2 2 "hello"
                            4 4 "world"]) "case 1")
      (is (rte-match rte [2 "hello" 2
                            4 4 "world"]) "case 2")
      (is (rte-match rte [2 "hello" 2
                            "world" 4 4]) "case 3")
      (is (not (rte-match rte [2 "hello" 2 2 2
                                 "world" 4 4])) "case 4")
      (is (not (rte-match rte [2 "hello" "hello"
                                 "world" 4 4])) "case 5")))
  )

(deftest t-with-rte-4
  (testing "with-rte 4"
    (with-rte [::x (:+ Long)
               ::y (:+ Double)]

      (let [pat (rte-compile '(:cat ::x  ::y))]
        ;; the same as (rte-compile '(:cat (:+ Long) (:+ Double)))
        (is (rte-match pat [1 2 3 1.2 3.4 5.6 7.8]))
        (is (not (rte-match pat [[1 2 3] [1.2 3.4 5.6 7.8]])))
        ))

    (let [pat (rte-compile '(:cat (rte (:+ Long)) (rte (:+ Double))))]
      (is (not (rte-match pat [1 2 3 1.2 3.4 5.6 7.8])))
      (is (rte-match pat [[1 2 3] [1.2 3.4 5.6 7.8]])))))

(deftest t-with-rte-5
  (testing "with-rte 5"
    (with-rte [::x (:+ Long)
               ::y (:+ Double)]

      (let [pat (rte-compile '(:cat ::x  ::y))]
        ;; the same as (rte-compile '(:cat (:+ Long) (:+ Double)))
        (is (rte-match pat [1 2 3 1.2 3.4 5.6 7.8]))
        (is (not (rte-match pat [[1 2 3] [1.2 3.4 5.6 7.8]])))
        ))

    (with-rte [::x (:+ String)
               ::y (:+ Double)]

      (let [pat (rte-compile '(:cat ::x  ::y))]
        ;; the same as (rte-compile '(:cat (:+ Long) (:+ Double)))
        (is (rte-match pat ["1" "2" "3" 1.2 3.4 5.6 7.8]))
        (is (not (rte-match pat [["1" "2" "3"] [1.2 3.4 5.6 7.8]])))
        ))
    

    (let [pat (rte-compile '(:cat (rte (:+ Long)) (rte (:+ Double))))]
      (is (not (rte-match pat [1 2 3 1.2 3.4 5.6 7.8])))
      (is (rte-match pat [[1 2 3] [1.2 3.4 5.6 7.8]])))))

(deftest t-rte-inhabited
  (testing "rte inhabited?"
    (with-compile-env ()

      (is (rte-inhabited? (rte-to-dfa '(:and (:* Long) (:* Double)))))
      (is (rte-vacuous? (rte-to-dfa '(:and (:+ Long) (:+ Double)))))

      (is (rte-inhabited? '(:and (:* Long) (:* Double))))
      (is (rte-vacuous? '(:and (:+ Long) (:+ Double)))))))

(deftest t-rte-with-rte
  (testing "recursive rte"
    (with-compile-env ()

      (is (not (disjoint? '(rte (:* Number))
                          '(rte (:* Double)))))
      (is (not (disjoint? '(rte (:* Number))
                          '(rte (:* String)))))
      (is (disjoint? '(rte (:+ Number)) 
                     '(rte (:+ String))))
      (is (rte-compile '(:or (rte (:* Number)) 
                             (rte (:cat Double Number))
                             (rte (:* Double))))))))

(deftest t-pattern-with-=-and-class
  (testing "pattern with ="
    (with-compile-env ()

      (is (rte-match '(:or Long (= 42)) [42]))
      (is (rte-match '(:or Long (= "42")) [0]))
      (is (rte-match '(:or Long (= "42")) ["42"])))))

(deftest t-pattern-with-=
  (testing "pattern with ="
    (with-compile-env ()
      (is (rte-match '(= 42) [42]))
      (is (not (rte-match '(= 42) [43])))
      (is (not (rte-match '(= 42) [42 42])))
      (is (not (rte-match '(= 42) [])))

      (is (rte-match '(:* (= 42)) []))
      (is (rte-match '(:* (= 42)) [42]))
      (is (rte-match '(:* (= 42)) [42 42 42 42]))

      (is (not (rte-match '(:+ (= 42)) [])))
      (is (rte-match '(:+ (= 42)) [42 ]))
      (is (rte-match '(:+ (= 42)) [42 42 42]))
      (is (not (rte-match '(:+ (= 42)) [42 42 42 43])))

      (is (rte-match '(:or (= 43 ) (= 42)) [42]))
      (is (rte-match '(:or (= 43 ) (= 42)) [43]))
      (is (not (rte-match '(:or (= 43 ) (= 42)) [0])))
      (is (rte-match '(:* (:or (= 43 )(= 42))) []))
      (is (rte-match '(:* (:or (= 43 )(= 42))) [42 42 42 43 42 43]))
      (is (not (rte-match '(:* (:or (= 43 ) (= 42))) [42 42 42 43 42 0 43])))

      )))

(deftest t-rte-combine-labels
  (testing "rte-combine-labels"
    (with-compile-env ()
      (is (= '(or Long String)
             (rte-combine-labels 'Long
                                 'String)))
      (is (= '(or Long String Double)
             (rte-combine-labels '(or Long String)
                                 'Double)))
      (is (= '(or Double Long String)
             (rte-combine-labels 'Double
                                 '(or Long String))))
      (is (= '(or Double String Long String)
             (rte-combine-labels '(or Double String)
                                 '(or Long String)))))))

(deftest t-rte-combine-labels
  (testing "and/not conversion"
    (with-compile-env ()
      (is (not= '(:not (= 0))
                (canonicalize-pattern '(not (= 0)))) "test 0")
      (is (not (rte-match '(:* (and Number Long (not (= 0)))) [0])) "test 1")
      (is (rte-match '(:* (and Number Long (not (= 0)))) [1]) "test 2")
      (is (not (rte-match '(:* (and  Long (not (= 0)))) [0])) "test 3")
      (is (rte-match '(:* (and Long (not (= 0)))) [1]) "test 4")
      (is (not (rte-match '(:* (and  Number (not (= 0)))) [0])) "test 5")
      (is (rte-match '(:* (and  Number (not (= 0)))) [1]) "test 6"))))

(deftest t-invalid-type
  (testing "for invalid type within rte"
    (with-compile-env ()
      (is (thrown? Exception (canonicalize-pattern '(not (:or String Number)))) "test 0")
      (is (thrown? Exception (canonicalize-pattern '(and (:or String Number)
                                                         (:or :sigma)))) "test 1")
      (is (thrown? Exception (canonicalize-pattern '(or (:and String Number)))) "test 2"))))

    
