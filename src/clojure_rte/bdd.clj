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

(ns clojure-rte.bdd
  "Definition of Bdd."
  (:require [clojure-rte.cl-compat :refer [cl-cond]]
            [clojure-rte.util :refer [fixed-point member group-by-mapped print-vals call-with-collector]]
            [clojure-rte.type :as ty]
            [clojure.pprint :refer [cl-format]]
            [clojure.set :refer [union difference intersection]]
))

(defrecord Bdd
  [label positive negative])

(defmethod print-method Bdd [bdd w]
  (.write w (cond
              (and (= true (:positive bdd))
                   (= false (:negative bdd)))
              (cl-format false "#<Bdd ~A>" (:label bdd))

              (and (= false (:positive bdd))
                   (= true (:negative bdd)))
              (cl-format false "#<Bdd not ~A>" (:label bdd))

              :else
              (cl-format false "#<Bdd ~A ~A ~A>" (:label bdd) (:positive bdd) (:negative bdd)))))

(defn itenf
  "Serialize a Bdd to if-then-else-normal-form (itenf)"
  [bdd]
  (case bdd
    (true) :sigma
    (false) :empty-set
    (letfn [(pretty-not [arg]
              (case arg
                (:sigma) :empty-set
                (:empty-set) :sigma
                (list 'not arg)))
            (pretty-or [a b]
              (cond
                (= a :sigma) :sigma
                (= b :sigma) :sigma
                (= a :empty-set) b
                (= b :empty-set) a
                (= a b) a
                :else (list 'or a b)))
            (pretty-and [a b]
              (cond
                (= a :sigma) b
                (= b :sigma) a
                (= a :empty-set) :empty-set
                (= b :empty-set) :empty-set
                (= a b) a
                :else (list 'and a b)))]
      
      (let [l (:label bdd)
            p (itenf (:positive bdd))
            n (itenf (:negative bdd))]
        (assert (not (= nil p)))
        (assert (not (= nil n)))
        (pretty-or (pretty-and l p)
                   (pretty-and (pretty-not l) n))))))

(defn dnf
  "Serialize a Bdd to dnf disjunctive normal form.
  This dnf form is cleaned up so that an (and ...) or (or ...) clause contains
  no subtype/supertype pairs.  This subtype relation is determined by
  (ty/subtype? a b (constantly false)).
  
  "
  [bdd]
  (letfn [(pretty-and [args]
            (cond
              (empty? args) :sigma
              (empty? (rest args)) (first args)
              :else (cons 'and args)))
          (pretty-or [args]
            (cond
              (empty? args) :empty-set
              (empty? (rest args)) (first args)
              :else (cons 'or args)))
          (supertypes [sub types]
            (filter (fn [super]
                      (and (not (= sub super))
                           (ty/subtype? sub super (constantly false)))) types))
          (check-supers [args]
            (let [args (distinct args)
                  complements (for [a args
                                    b args
                                    :when (or (= a (list 'not b))
                                              (= b (list 'not a)))]
                                [a b])]
              (cond
                ;; does the list contain A and (not A) ?
                (not (empty? complements))
                '(:sigma)

                ;; does the list contain A and B where A is subtype B
                :else
                (remove (fn [sub]
                          (not (empty? (supertypes sub args))))
                        args))))]

    (pretty-or
     (check-supers
      (call-with-collector
       (fn [collect]
         (letfn [(walk [node parents]
                   (let [my-label (:label node)
                         ;; two lazy sequences created by filter.  the filter loops are
                         ;; never called unless (empty? ...) is called below.
                         disjoints (filter (fn [x] (ty/disjoint? x my-label (constantly false))) parents)
                         subtypes  (filter (fn [x] (ty/subtype?  x my-label (constantly false))) parents)]
                     
                     (cond
                       (= true node)
                       ;; we know parents ( ... A ... B ...) that B is not subtype of A, but maybe B subtype A
                       ;;   we need to remove the supertypes
                       ;;   E.g., (Long java.io.Comparable java.io.Serializable) -> (Long)
                       (collect (pretty-and (loop [tail parents
                                                   done '()]
                                              (if (empty? tail)
                                                done
                                                (recur (filter (fn [b]
                                                                 (ty/subtype? b (first tail) (constantly false))) (rest tail))
                                                       (cons (first tail) done))))))
                       
                       (= false node)
                       nil ;; do not collect, and prune recursion
                       
                       (not (empty? disjoints))
                       (walk (:negative node)
                             parents)
                       
                       (not (empty? subtypes))
                       (walk (:positive node)
                             parents)
                       
                       :else
                       (do (walk (:positive node)
                                 (cons (:label node) parents))
                           (walk (:negative node)
                                 (cons (list 'not (:label node)) parents))))))]
           (walk bdd '()))))))))

(def ^:dynamic *bdd-hash* (atom false))
(def ^:dynamic *label-to-index* (atom false))

(defn call-with-bdd-hash
  ""
  [thunk]
  (binding [*label-to-index* (atom {})
            *bdd-hash* (atom {})]
    (thunk)))

(defmacro with-bdd-hash
  ""
  [[] & body]
  `(call-with-bdd-hash (fn [] ~@body)))

(defn type-index [type-designator]
  ;; TODO make sure two differnet symbols representing the same class
  ;; result in the same index.  eg. Double vs java.lang.Double
  (or (@*label-to-index* type-designator)
      (do (swap! *label-to-index* assoc type-designator (count @*label-to-index*))
          (@*label-to-index* type-designator))))

(declare bdd-and)
(declare bdd-or)
(declare bdd-not)

(defn bdd
  "Programmatic Bdd constructor."
  ([type-designator]
   (cond
     (sequential? type-designator)
     (case (first type-designator)
       (and) (reduce bdd-and (map bdd (rest type-designator)))
       (or)  (reduce bdd-or (map bdd (rest type-designator)))
       (not) (apply bdd-not (map bdd (rest type-designator)))
       (bdd type-designator true false))

     (= :sigma type-designator)
     true

     (= :empty-set type-designator)
     false

     :else
     (bdd type-designator true false)))
  ([type-designator positive negative]
   (assert (map? @*bdd-hash*) "attempt to allocate a Bdd outside dynamically extend of call-with-bdd-hash")
   (assert (map? @*label-to-index*) "attempt to allocate a Bdd outside dynamically extend of call-with-bdd-hash")
   (assert (ty/typep positive '(or Boolean clojure_rte.bdd.Bdd)))
   (assert (ty/typep negative '(or Boolean clojure_rte.bdd.Bdd)))
   (assert (ty/valid-type? type-designator) (format "invalid type-designator %s" type-designator))

   (cond
     (identical? positive negative)
     positive
     :else
     (let [try-bdd (Bdd. type-designator positive negative)
           cached-bdd (@*bdd-hash* try-bdd)]
       (or cached-bdd
           (do (swap! *bdd-hash* assoc try-bdd try-bdd)
               (assert (or (instance? Boolean positive)
                           (< (type-index type-designator)
                              (type-index (:label positive))))
                       (format "parent %s must be < positive %s" type-designator (:label positive)))
               (assert (or (instance? Boolean negative)
                           (< (type-index type-designator)
                              (type-index (:label negative))))
                       (format "parent %s must be < negative %s" type-designator (:label negative)))
               
               try-bdd))))))

(defn binary-op
  "Bdd abstract binary operation."
  [op bdd1 bdd2]
  (if (= (:label bdd1) (:label bdd2))
    (bdd (:label bdd1)
         (op (:positive bdd1) (:positive bdd2))
         (op (:negative bdd1) (:negative bdd2)))
    (let [label-index-1 (type-index (:label bdd1))
          label-index-2 (type-index (:label bdd2))]
      (assert (integer? label-index-1) (format "expecting integer got %s" (type label-index-1)))
      (assert (integer? label-index-2) (format "expecting integer got %s" (type label-index-2)))
      (if (< label-index-1 label-index-2)
        (bdd (:label bdd1)
             (op (:positive bdd1) bdd2)
             (op (:negative bdd1) bdd2))
        (bdd (:label bdd2)
             (op bdd1 (:positive bdd2))
             (op bdd1 (:negative bdd2)))))))
  
(defn bdd-and
  "Perform a Boolean AND on 0 or more Bdds."
  ([] true)
  ([bdd] bdd)
  ([bdd1 bdd2]
   (cond
     (= false bdd1) false
     (= false bdd2) false
     (= true bdd1) bdd2
     (= true bdd2) bdd1
     (identical? bdd1 bdd2) bdd1
     :else (binary-op bdd-and bdd1 bdd2)))
  ([bdd1 bdd2 & bdds]
   (reduce bdd-and (apply cons bdd1 bdd2 bdds))))

(defn bdd-or
  "Perform a Boolean OR on 0 or more Bdds."
  ([] false)
  ([bdd] bdd)
  ([bdd1 bdd2]
   (cond
     (= false bdd1) bdd2
     (= false bdd2) bdd1
     (= true bdd1) true
     (= true bdd2) true
     (identical? bdd1 bdd2) bdd1
     :else (binary-op bdd-or bdd1 bdd2)))
  ([bdd1 bdd2 & bdds]
   (reduce bdd-or (apply cons bdd1 bdd2 bdds))))

(defn bdd-and-not
  "Perform a relative complement operation on two (or more) Bdds.
  This is not implemented for the 0-ary nor 1-ary case."
  ([bdd1 bdd2]
   (cond
     (identical? bdd1 bdd2) false
     (= bdd1 false) false
     (= bdd2 true) false
     (= bdd2 false) bdd1
     (= bdd1 true) (bdd (:label bdd2)
                        (bdd-and-not true (:positive bdd2))
                        (bdd-and-not true (:negative bdd2)))
     :else (binary-op bdd-and-not bdd1 bdd2)))
  ([bdd1 bdd2 & bdds]
   (reduce bdd-and (apply cons bdd1 bdd2 bdds))))

(defn bdd-not
  "Perform a Boolean not of a given Bdd"
  [bdd1]
  (bdd-and-not true bdd1))

(defn gen-random
  "Generate a random Bdd"
  ([] (gen-random 15))
  ([max-depth]
   (if (<= max-depth 0)
     (rand-nth '(true false))
     (let [r (rand-int 4)]
       (cond
         (= r 0)
         (rand-nth '(true false))
         
         (= r 1)
         (bdd (rand-nth '(Long Double String Boolean Character Short
                               java.io.Serializable java.lang.Comparable)))
         
         :else
         (let [bdd-1 (gen-random (dec max-depth))
               bdd-2 (gen-random (dec max-depth))
               r (rand-int 3)]
           (cond
             (= r 0)
             (bdd-and bdd-1 bdd-2)
             (= r 1)
             (bdd-or bdd-1 bdd-2)
             :else
             (bdd-and-not bdd-1 bdd-2))))))))

(defn bdd-typep
  "Given a value in question, and a Bdd representing a type designator,
  determine whether the value is an alement of the designated type."
  [value bdd]
  (cond
    (= true bdd) true
    (= false bdd) false
    :else (bdd-typep value
                     (if (ty/typep value (:label bdd))
                         (:positive bdd)
                         (:negative bdd)))))

(defn bdd-disjoint?
  "Given two Bdds, determine whether it can be proven that the intersection of the
   types they represent is empty.
   If it cannot be proven that they are disjoint, false is returned."
  [bdd1 bdd2]
  (= :empty-set
     (dnf (bdd-and bdd1 bdd2))))

(defn bdd-type-disjoint?
  "Given two type designators, use Bdds to determine whether they are disjoint.
  If it cannot be proven that they are disjoint, false is returned."
  [type-designator-1 type-designator-2]
  (= :empty-set
     (dnf (bdd (list 'and type-designator-1 type-designator-2)))))

(defn bdd-type-subtype?
  "Given two type designators, use Bdds to determine whether one is a subtype of the other.
  If it cannot be proven, false is returned."
  [subtype-designator supertype-designator]
  (let [bdd-sub (bdd subtype-designator)
        bdd-sup (bdd supertype-designator)]
    (= :empty-set
       (dnf (bdd-and-not bdd-sub bdd-sup)))))



  
