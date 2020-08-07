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
            [clojure-rte.util :refer [fixed-point member group-by-mapped print-vals]]
            [clojure-rte.type :as ty]
            [clojure.set :refer [union difference intersection]]
))

(defrecord Bdd
  [label positive negative])

(defmethod print-method Bdd [bdd w]
  (.write w (format "#<Bdd %s>" (:label bdd))))

(def ^:dynamic *bdd-hash* false)
(def ^:dynamic *label-to-index* false)

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
  (or (@*label-to-index* type-designator)
      (do (swap! *label-to-index* assoc type-designator (count @*label-to-index*))
          (@*label-to-index* type-designator))))

(defn bdd
  ""
  ([type-designator]
   (bdd type-designator true false))
  ([type-designator positive negative]
   (assert (map? @*bdd-hash*) "attempt to allocate a Bdd outside dynamically extend of call-with-bdd-hash")
   (assert (map? @*label-to-index*) "attempt to allocate a Bdd outside dynamically extend of call-with-bdd-hash")
   (assert (ty/typep positive '(or Boolean Bdd)))
   (assert (ty/typep negative '(or Boolean Bdd)))
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

(defn bdd-op
  ""
  [op bdd1 bdd2]
  (if (= (:label bdd1) (:label bdd2))
    (bdd (:label bdd1)
         (op (:positive bdd1) (:positive bdd1))
         (op (:negative bdd1) (:negative bdd1)))
    (let [label-index-1 (type-index (:label bdd1))
          label-index-2 (type-index (:label bdd2))]
      (assert (ty/typep Long label-index-1))
      (assert (ty/typep Long label-index-2))
      (if (< label-index-1 label-index-2)
        (bdd (:label bdd1)
             (op (:positive bdd1) bdd2)
             (op (:negative bdd1) bdd2))
        (bdd (:label bdd2)
             (op bdd1 (:positive bdd2))
             (op bdd1 (:negative bdd2)))))))
  
(defn bdd-and
  ([bdd1 bdd2]
   (cond
     (= false bdd1) false
     (= false bdd2) false
     (= true bdd1) bdd2
     (= true bdd2) bdd1
     (= bdd1 bdd2) bdd1
     :else (bdd-op bdd-and bdd1 bdd2)))
  ([bdd1 bdd2 & bdds]
   (reduce bdd-and (apply cons bdd1 bdd2 bdds))))

(defn bdd-or
  ([bdd1 bdd2]
   (cond
     (= false bdd1) bdd1
     (= false bdd2) bdd2
     (= true bdd1) true
     (= true bdd2) true
     (= bdd1 bdd2) bdd1
     :else (bdd-op bdd-or bdd1 bdd2)))
  ([bdd1 bdd2 & bdds]
   (reduce bdd-or (apply cons bdd1 bdd2 bdds))))

(defn bdd-and-not
  ([bdd1 bdd2]
   (cond
     (= bdd1 bdd2) false
     (= bdd1 false) false
     (= bdd2 true) false
     (and (= bdd1 true)
          (= bdd2 false)) true
     (= bdd1 true) (bdd (:label bdd2)
                        (bdd-and-not true (:positive bdd2))
                        (bdd-and-not true (:negative bdd2)))
     :else (bdd-op bdd-and-not bdd1 bdd2)))
  ([bdd1 bdd2 & bdds]
   (reduce bdd-and (apply cons bdd1 bdd2 bdds))))

(defn bdd-not [bdd1]
  (bdd-and-not true bdd1))

(defn bdd-random []
  (let [r (rand)]
    (cond
      (< r 0.25)
      (if (< (rand) 0.5)
        true
        false)
      (< r 0.5)
      (bdd (rand-nth '(Long Double String Boolean Character Short
                            java.io.Serializable java.lang.Comparable)))
      :else
      (let [bdd-1 (bdd-random)
            bdd-2 (bdd-random)
            r (rand)]
        (cond
          (> r 0.66)
          (bdd-and bdd-1 bdd-2)
          (> r 0.33)
          (bdd-or bdd-1 bdd-2)
          :else
          (bdd-and-not bdd-1 bdd-2))))))        

(defn bdd-typep [value bdd]
  (cond
    (= true bdd) true
    (= false bdd) false
    :else (bdd-typep value
                     (if (ty/typep value (:label bdd))
                         (:positive bdd)
                         (:negative bdd)))))
