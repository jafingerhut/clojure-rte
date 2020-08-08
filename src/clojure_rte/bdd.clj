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
    (let [l (:label bdd)
          p (itenf (:positive bdd))
          n (itenf (:negative bdd))]
      (assert (not (= nil p)))
      (assert (not (= nil n)))
      (cond
        (and (= p :sigma)
             (= n :empty-set))
        l

        (and (= p :empty-set)
             (= n :sigma))
        (list 'not l)

        (= p :sigma)
        `(~'or ~l
          (~'and (~'not ~l) ~n))

        (= p :empty-set)
        `(~'and (~'not ~l) ~n)

        (= n :sigma)
        `(~'or (~'and ~l ~p)
          (~'not ~l))

        (= n :empty-set)
        `(~'and ~l ~p)

        :else
        `(~'or (~'and ~l ~p)
          (~'and (~'not ~l) ~n))))))

(defn dnf
  "Serialize a Bdd to dnf disjunctive normal form."
  [bdd]
  (cons 'or
        (call-with-collector
         (fn [collect]
           (letfn [(walk [node parents]
                     (cond
                       (= true node)
                       (collect (cons 'and (reverse parents)))
                       
                       (= false node)
                       "nothing"
                       
                       :else
                       (do (walk (:positive node)
                                 (cons (:label node) parents))
                           (walk (:negative node)
                                 (cons (list 'not (:label node)) parents)))))]
             (walk bdd '()))))))

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
  ""
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
   (assert (ty/valid-type? type-designator))

   (cond
     (= positive negative)
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
  ([] true)
  ([bdd] bdd)
  ([bdd1 bdd2]
   (cond
     (= false bdd1) false
     (= false bdd2) false
     (= true bdd1) bdd2
     (= true bdd2) bdd1
     (= bdd1 bdd2) bdd1
     :else (binary-op bdd-and bdd1 bdd2)))
  ([bdd1 bdd2 & bdds]
   (reduce bdd-and (apply cons bdd1 bdd2 bdds))))

(defn bdd-or
  ([] false)
  ([bdd] bdd)
  ([bdd1 bdd2]
   (cond
     (= false bdd1) bdd2
     (= false bdd2) bdd1
     (= true bdd1) true
     (= true bdd2) true
     (= bdd1 bdd2) bdd1
     :else (binary-op bdd-or bdd1 bdd2)))
  ([bdd1 bdd2 & bdds]
   (reduce bdd-or (apply cons bdd1 bdd2 bdds))))

(defn bdd-and-not
  ([bdd1 bdd2]
   (cond
     (= bdd1 bdd2) false
     (= bdd1 false) false
     (= bdd2 true) false
     (= bdd2 false) bdd1
     (= bdd1 true) (bdd (:label bdd2)
                        (bdd-and-not true (:positive bdd2))
                        (bdd-and-not true (:negative bdd2)))
     :else (binary-op bdd-and-not bdd1 bdd2)))
  ([bdd1 bdd2 & bdds]
   (reduce bdd-and (apply cons bdd1 bdd2 bdds))))

(defn bdd-not [bdd1]
  (bdd-and-not true bdd1))

(defn bdd-random
  "Generate a random Bdd"
  ([] (bdd-random 15))
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
         (let [bdd-1 (bdd-random (dec max-depth))
               bdd-2 (bdd-random (dec max-depth))
               r (rand-int 3)]
           (cond
             (= r 0)
             (bdd-and bdd-1 bdd-2)
             (= r 1)
             (bdd-or bdd-1 bdd-2)
             :else
             (bdd-and-not bdd-1 bdd-2))))))))

(defn bdd-typep [value bdd]
  (cond
    (= true bdd) true
    (= false bdd) false
    :else (bdd-typep value
                     (if (ty/typep value (:label bdd))
                         (:positive bdd)
                         (:negative bdd)))))
