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
            [clojure-rte.dfa :as dfa]
            [clojure.pprint :refer [cl-format]]
            [clj-async-profiler.core :as prof] ;; this requirement is only temporary while trying to debug the out-of-memory error
            ;; [clojure-rte.dot :as dot]
            [clojure-rte.rte-core :refer [dfa-to-rte rte-to-dfa canonicalize-pattern nullable with-compile-env]]
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
  (tester/random-test num-tries (fn [rte]
                                  (with-compile-env []
                                    (rte-to-dfa rte)))
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
  if (canonicalize-pattern r) is also nullable."
  [num-tries size verbose]
  (tester/random-test num-tries
                      (fn [rte]
                        (with-compile-env []
                          ;;(cl-format true "canonicalizing:~%")
                          ;; TODO doall this lazy seq
                          (let [can (canonicalize-pattern rte)]
                            ;;(cl-format true "canonicalized: ~A~%" can)
                            (if (nullable rte)
                              (assert (nullable can)
                                      (cl-format false
                                                 "rte ~A is nullable but its canonicalization ~A is not"
                                                 rte can))
                              (assert (not (nullable can))
                                      (cl-format false
                                                 "rte ~A is not nullable but its canonicalization ~A is nullable"
                                                 rte can))))))
                      (fn [] (gen-rte size *test-types*))
                      rte-components
                      verbose))

(defn test-rte-not-1
  "Assert that the same result occurs from complementing a dfa
  or building a Dfa from a complemented rte."
  [rte]
  (println [:test-rte-not-1 rte])
  (with-compile-env []
    ;; is (not rte) equivalent to (complement dfa) ?
    (let [dfa (rte-to-dfa rte)
          dfa-complement (dfa/complement dfa)
          dfa-not-rte (rte-to-dfa (list :not rte))]
      ;;(dot/dfa-to-dot dfa :view true :title "dfa")
      ;;(dot/dfa-to-dot dfa-complement :view true :title "dfa-complement")
      ;;(dot/dfa-to-dot dfa-not-rte :view true :title "dfa-not-rte")
      
      (assert (dfa/dfa-equivalent dfa
                                  dfa)
              (cl-format false
                         "dfa not equivalent with self rte=~A" rte))

      (assert (dfa/dfa-equivalent dfa-not-rte
                                  dfa-not-rte)
              (cl-format false
                         "dfa of :not, not equivalent with self rte=~A" (list :not rte)))

      (assert (dfa/dfa-equivalent dfa-complement
                                  dfa-not-rte)
              (cl-format false
                         "!dfa != (dfa (not rte)), when rte=~A" rte))
      
      (let [extracted-rte-map (dfa-to-rte dfa-complement)
            extracted-rte (get extracted-rte-map true :empty-set)
            ]
        
        (assert (dfa/dfa-equivalent dfa
                                    ;; dfa-to-rte returns a map
                                    ;;   we have to find the value corresponding to the key=true
                                    (rte-to-dfa (list :not extracted-rte)))
                (cl-format false
                           "(rte (dfa (not rte))) != dfa, when rte=~A" rte))))))

(defn test-rte-not
  "Testing several functions, dfa/complement, dfa-to-rte, dfa-equivalent"
  [num-tries size verbose]
  (tester/random-test num-tries
                      test-rte-not-1                            
                      (fn [] (gen-rte size *test-types*))
                      rte-components
                      verbose))
;; (defn -main []
;;   (prof/profile 
;;    (test-rte-canonicalize-nullable 500 ;; 500 ; num-tries
;;                                    4 ; size
;;                                    true ;verbose
;;                                    ))
;;   )

