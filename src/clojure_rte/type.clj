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
  (:require [clojure.set :refer [union intersection]]
            [clojure-rte.util :refer [call-with-collector]]
            [clojure-rte.cl-compat :refer [cl-cond cl-prog1]]
            [clojure.reflect :as refl]
  ))

(defmulti typep 
  "Like instance? except that the arguments are reversed, and the
  given type need not be a class.
  This function also handles CL style type designators such as
  (not A)
  (and A B)
  (or A B)
  (satisfies A B)
  (= obj)
  (member a b c)"
  (fn [a-value type-designator]
    (if (sequential? type-designator)
      (first type-designator)
      type-designator)))

(defmulti valid-type?
  "Look at a type-designator and determine whether it is syntactically correct"
  (fn [type-designator]
    (if (sequential? type-designator)
      (first type-designator)
      type-designator)))


(defmethod typep :sigma [_ _]
  true)

(defmethod valid-type? :sigma [_]
  true)

(defmethod typep :empty-set [_ _]
  false)

(defmethod valid-type? :empty-set [_]
  true)

(defmethod typep :default [a-value a-type]
  (if (and (symbol? a-type)
           (resolve a-type)
           (class? (resolve a-type)))
    (isa? (type a-value) (resolve a-type))
    (throw (ex-info (format "typep: invalid type %s" a-type)
                    {:type :invalid-type-designator
                     :a-type a-type
                     :a-value a-value
                     }))))

(defmethod valid-type? :default [type-designator]
  (and (symbol? type-designator)
       (resolve type-designator)
       (class? (resolve type-designator))))

(defmethod typep 'not [a-value [a-type t]]
  (not (typep a-value t)))

(defmethod valid-type? 'not [[_ type-designator]]
  (valid-type? type-designator))

(defmethod typep 'and [a-value [a-type & others]]
  (every? (fn [t1]
            (typep a-value t1)) others))

(defmethod valid-type? 'and [_ & others]
  (every? valid-type? others))


(defmethod typep 'or [a-value [a-type & others]]
  (some (fn [t1]
          (typep a-value t1)) others))

(defmethod valid-type? 'or [_ & others]
  (every? valid-type? others))

(defmethod typep 'satisfies [a-value [a-type f]]
  (if (fn? f)
    (f a-value)
    ((resolve f) a-value)))

(defmethod valid-type? 'satisfies [[_ f]]
  (or (fn? f)
      (and (symbol? f)
           (resolve f))))

(defmethod typep '= [a-value [a-type value]]
  (= value a-value))

(defmethod valid-type? '= [[_ _]]
  true)

(defmethod typep 'member [a-value [a-type & others]]
  (some #{a-value} others))

(defmethod valid-type? 'member [[_ & _]]
  true)

(defn type-intersection 
  "Return the set of the subtypes of the two types, ie. the set of types
  which are both a subtype of t1 and of t2.  If the types don't
  intersect, #{} is returned."
  [t1 t2]
  (intersection (conj (or (descendants t1) #{}) t1)
                (conj (or (descendants t2) #{}) t2)))

(def disjoint-hooks (atom {}))
(defn new-disjoint-hook [key hook-fn]
  (swap! disjoint-hooks (fn [_]
                          (assoc @disjoint-hooks key hook-fn)))
  (keys @disjoint-hooks))

(new-disjoint-hook
 :primary
 (letfn [(derived? [t]
           (and (or (keyword? t)
                    (qualified-symbol? t))
                (or (ancestors t)
                    (descendants t))))]
   (fn [t1 t2]
     (cond
       (= :empty-set t1)
       true
       
       (= :empty-set t2)
       true

       (= t1 t2)
       false

       (= :epsilon t1)
       false
       
       (= :epsilon t2)
       false
       
       (= :sigma t1)
       false
       
       (= :sigma t2)
       false
       
       (isa? t1 t2)
       false
       
       (isa? t2 t1)
       false

       (and (derived? t1)
            (derived? t2))
       (empty? (type-intersection t1 t2))
       
       :else
       :dont-know))))

(defn disjoint?
  "Predicate to determine whether the two types overlap."
  [t1 t2]
  (case (reduce (fn [_ key]
                  (case ((key @disjoint-hooks) t1 t2)
                    (true) (reduced true)
                    (false) (reduced false)
                    nil))
                :initial
                (cons :primary (remove #{:primary} (keys @disjoint-hooks))))
    (true) true
    (false) false
    (throw (ex-info (format "disjoint? cannot decide %s vs %s" t1 t2)
                    {:type :not-yet-implemented
                     :type-designators [t1 t2]}))))

(new-disjoint-hook
 :derived
 (fn [t1 t2]
   :dont-know))

(letfn [(class-designator? [t]
          (and (symbol? t)
               (resolve t)
               (class? (resolve t))))
        (not? [t]
          (and (sequential? t)
               (= 'not (first t))))
        (isa [sub super]
          (isa? (resolve sub) (resolve super)))
        (class-type [t]
          (let [c (resolve t)
                r (refl/type-reflect c)
                flags (:flags r)]
            (cond
              (= c Object)
              :abstract
              (contains? flags :interface)
              :interface
              (contains? flags :final)
              :final
              (contains? flags :abstract)
              :abstract
              (= flags #{:public})
              :final
              
              :else
              (throw (ex-info (format "disjoint? type %s flags %s not yet implemented" t flags)
                              {:type :invalid-type-flags
                               :a-type t
                               :flags flags})))))]

  (new-disjoint-hook
   :classes
   (fn [t1 t2]
     (if (and (class-designator? t1)
              (class-designator? t2))
       (case [(class-type t1) (class-type t2)]
         ((:interface :interface)
          (:interface :abstract)
          (:abstract :interface))
         false ;; not disjoint
         
         ((:final :final)
          (:final :interface)
          (:final :abstract))
         (not (isa t1 t2))

         ((:interface :final)
          (:abstract :final))
         (not (isa t2 t1))

         ((:abstract :abstract))
         (not (or (isa t1 t2)
                  (isa t2 t1))))

       :dont-know)))

  (new-disjoint-hook
   :not
   (fn [t1 t2]
     (cond
       (and (not? t1)
            (= t2 (second t1)))
       true
       
       (and (not? t2)
            (= t1 (second t2)))
       true
       
       ;; if t1 < t2, then t1 disjoint from (not t2)
       (and (class-designator? t1)
            (not? t2)
            (class-designator? (second t2))
            (isa t1 (second t2)))
       true

       :else
       :dont-know))))

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
             ((and (some #{:sigma} left)
                   right)
              (recurring items (remove #{:sigma} left) right))
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
