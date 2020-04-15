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

(ns clojure-rte.type
  (:require   [clojure.set :refer [union intersection]]
              [clojure-rte.util :refer [call-with-collector]]
              [clojure-rte.cl-compat :refer [cl-cond]]
  ))

(defn typep 
  "Like instance? except that the arguments are reversed, and the
  given type need not be a class.
  This function also handles CL style type designators such as
  (not A)
  (and A B)
  (or A B)
  (satisfies A B)
  (= obj)
  (member a b c)"
  [a-value a-type]

  (cond 
    (= :sigma a-type)
    true

    (= :empty-set a-type)
    false
    
    (and (symbol? a-type)
         (resolve a-type)
         (class? (resolve a-type)))
    (isa? (type a-value) (resolve a-type))
  
    (not (seq? a-type))
    false

    :else
    (let [[name & others] a-type]
      (case name
        (not) (not (apply typep a-value others))
        (and) (every? (fn [t1]
                        (typep a-value t1)) others)
        (or) (some (fn [t1]
                     (typep a-value t1)) others)
        (satisfies) ((resolve (first others)) a-value)
        (=) (= (first others) a-value)
        (member) (some #{a-value} others)
        (throw (ex-info (format "(2) invalid type designator %s, %s not in %s"
                                a-type name '(not and or satisfies = member))

                    {:type :invalid-type-designator
                     :type-designator a-type
                     }))))))

(defn type-intersection 
  "Return the set of the subtypes of the two types, ie. the set of types
  which are both a subtype of t1 and of t2.  If the types don't
  intersect, #{} is returned."
  [t1 t2]
  (intersection (conj (or (descendants t1) #{}) t1)
                (conj (or (descendants t2) #{}) t2)))

(defn disjoint? 
  "Predicate to determine whether the two types overlap."
  [t1 t2]
  (cond
    (= :empty-set t1)
    true

    (= :empty-set t2)
    true
    
    (= :sigma t1)
    false

    (= :sigma t2)
    false

    :else
    (and (not (isa? t1 t2))
         (not (isa? t2 t1))
         (let [descendants-1 (descendants t1)
               descendants-2 (descendants t2)]
           (and (not-any? (fn [a2] (contains? descendants-1 a2)) descendants-2)
                (not-any? (fn [a1] (contains? descendants-2 a1)) descendants-1))))))

(defn type-min 
  "Find an element of the given sequence which is a subtype
  of some other type and is not =.  not necessarily the global minimum."
  [atoms]
  (some (fn [sub]
          (some (fn [super]
                  (and (not (= sub super))
                       (isa? sub super)
                       sub)) atoms)) atoms))

(defn type-max 
  "Find an element of the given sequence which is a supertype
  of some other type and is not =.  not necessarily the global maximum"
  [atoms]
  (some (fn [sub]
          (some (fn [super]
                  (and (not (= sub super))
                       (isa? sub super)
                       super)) atoms)) atoms))

(defn map-type-partitions 
  "Iterate through all the ways to partition types between a right and left set.
  Some care is made to prune branches which are provably empty."
  [items binary-fun]
  
  (letfn [(remove-supertypes [types]
            ;; Given a list of symbols designating types, return a new list
            ;; excluding those which are supertypes of others in the list.
            (let [supers (call-with-collector
                          (fn [collect]
                            (doseq [t1 types
                                    t2 types]
                              (if (and (not (= t1 t2))
                                       (isa? t1 t2))
                                (collect t2)))))]
              (for [x types
                    :when (not (some #{x} supers))]
                x)))
          (remove-subtypes [types]
            ;; Given a list of symbols designating types, return a new list
            ;; excluding those which are subypes of others in the list.
            (let [supers (call-with-collector
                          (fn [collect]
                            (doseq [t1 types
                                    t2 types]
                              (if (and (not (= t1 t2))
                                       (isa? t2 t1))
                                (collect t2)))))]
              (for [x types
                    :when (not (some #{x} supers))]
                x)))
          (type-reduce [left right]
            (loop [left left
                   right right]
              (cond
                (and (< 1 (count left))
                     (some #{:sigma} left))
                (recur (remove #{:sigma} left) right)

                :else [left right])))
          (recurring [items left right]
            (cl-cond
             ((some #{:sigma} right)
              )
             ((and left
                   (some (fn [t2]
                           (disjoint? t2 (first left))) (rest left)))
              )
             ((and left right
                   ;; exists t2 in right such that t1 < t2
                   ;; then t1 & !t2 = nil
                   (some (fn [t2] (isa? (first left) t2))  right))
              ;; prune
              )

             ((and left right
                   ;; exists t2 in right such that t1 < t2
                   ;; then t1 & !t2 = nil
                   (some (fn [t1] (isa? t1 (first right)))  left))
              ;; prune
              )

             ((empty? items)
              (let [[left right] (type-reduce (remove-supertypes left) (remove-subtypes right))]
                (binary-fun left right)))
             ;; TODO consider subsets A < B
             ;;    then A and ! B is empty
             ;;    A & B is A
             ;;    !A and !B is !B

             (:else
              (let [new-type (first items)]
                (case new-type
                  (nil)
                  (:sigma)
                  (do
                    (recurring (rest items) (cons new-type left) (remove (fn [t2] (disjoint? t2 new-type)) right))
                    (if (some (fn [t2] (disjoint? new-type t2)) left)
                      (recurring (rest items) left right) ;;   Double & !Float, we can omit Float in right
                      (recurring (rest items) left (cons new-type right)))))))))]
    (recurring items () ())))
