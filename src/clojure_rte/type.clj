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
  (:require [clojure.set :refer [intersection]]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.util :refer [call-with-collector]]
            [clojure-rte.cl-compat :refer [cl-cond]]
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
  (fn [_value type-designator]
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
                    {:error-type :invalid-type-designator
                     :a-type a-type
                     :a-value a-value
                     }))))

(defmethod valid-type? :default [type-designator]
  (and (symbol? type-designator)
       (resolve type-designator)
       (class? (resolve type-designator))))

(defmethod typep 'not [a-value [_a-type t]]
  (not (typep a-value t)))

(defmethod valid-type? 'not [[_ type-designator]]
  (valid-type? type-designator))

(defmethod typep 'and [a-value [_a-type & others]]
  (every? (fn [t1]
            (typep a-value t1)) others))

(defmethod valid-type? 'and [_ & others]
  (every? valid-type? others))


(defmethod typep 'or [a-value [_a-type & others]]
  (some (fn [t1]
          (typep a-value t1)) others))

(defmethod valid-type? 'or [_ & others]
  (every? valid-type? others))

(defmethod typep 'satisfies [a-value [_a-type f]]
  (if (fn? f)
    (f a-value)
    ((resolve f) a-value)))

(defmethod valid-type? 'satisfies [[_ f]]
  (or (fn? f)
      (and (symbol? f)
           (resolve f))))

(defmethod typep '= [a-value [_type value]]
  (= value a-value))

(defmethod valid-type? '= [[_ _]]
  true)

(defmethod typep 'member [a-value [_type & others]]
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
(defn new-disjoint-hook 
  "Establish (or override) a named hook for use in disjoint?
  This is necessary because clojure multmethods do not support
  call-next-method.  We need several _methods_ to be called until
  one fails to return :dont-know.
  (new-disjoint-hook ...) establishes a new hook, which must designate
  a binary function which returns true, false, or :dont-know."
  ;; TODO - also specify relative key for use in topological sort, to determine the order the hooks should run.
  [key hook-fn]
  (swap! disjoint-hooks (fn [_]
                          (assoc @disjoint-hooks key hook-fn)))
  (keys @disjoint-hooks))

(new-disjoint-hook
 :primary
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

     :else
     :dont-know)))

(def disjoint?-false (fn [_ _] false))
(def disjoint?-true  (fn [_ _] true))
(def disjoint?-false-warn (fn [t1 t2]
                            (cl-format true "disjoint? cannot decide ~A vs ~A -- assuming not disjoint~%" t1 t2)
                            false))
(def ^:dynamic *disjoint?-default*
  "Default to return when disjoint-ness cannot be determined.  This value is a binary
  function which is called with the two type designators in question:
  [t1 t2]"
  disjoint?-false-warn)
                
(defn disjoint?
  "Predicate to determine whether the two types overlap."
  ([t1 t2]
   (disjoint? t1 t2 *disjoint?-default*))
  ([t1 t2 default]
   (binding [*disjoint?-default* default]
     (case (reduce (fn [_ key-tag]
                     (case ((key-tag @disjoint-hooks) t1 t2)
                       (true) (reduced true)
                       (false) (reduced false)
                       (case ((key-tag @disjoint-hooks) t2 t1)
                         (true) (reduced true)
                         (false) (reduced false)
                         nil)))
                   :initial
                   (cons :primary (remove #{:primary} (keys @disjoint-hooks))))
       (true) true
       (false) false
       (default t1 t2)))))

(defn class-designator? [t]
  (and (symbol? t)
       (resolve t)
       (class? (resolve t))))


(new-disjoint-hook
 :and
 (letfn [(and? [t]
           (and (sequential? t)
                (= 'and (first t))))]
   (fn [t1 t2]
     (cond (and (and? t2)
                (some (fn [t]
                        (disjoint? t1 t)) (rest t2)))
           true

           (and (and? t1)
                (some #{t2} (rest t1)))
           false

           (and (and? t1)
                (class-designator? t2)
                (= (resolve t2) java.lang.Object)
                (some class-designator? (rest t1)))
           false

           :else
           :dont-know))))

(new-disjoint-hook
 :derived
 (fn [_t1 _t2]
   :dont-know))


(def subtype-hooks (atom {}))
(defn new-subtype-hook 
  "Establish (or override) a named hook for use in subtype?
  This is necessary because clojure multmethods do not support
  call-next-method.  We need several _methods_ to be called until
  one fails to return :dont-know.
  (new-disjoint-hook ...) establishes a new hook, which must designate
  a binary function which returns true, false, or :dont-know."
  ;; TODO - also specify relative key for use in topological sort, to determine the order the hooks should run.
  [key hook-fn]
  (swap! subtype-hooks (fn [_]
                          (assoc @subtype-hooks key hook-fn)))
  (keys @subtype-hooks))

(def inhabited?-false (fn [_] false))
(def inhabited?-true (fn [_] true))
(def inhabited?-error 
  (fn [type-designator]       
    (throw (ex-info (format "inhabited? cannot decide %s" type-designator)
                    {:error-type :not-yet-implemented
                     :type-designator [type-designator]}))))

(def ^:dynamic *inhabited?-default*
  "doc string"
  inhabited?-error
)

(def inhabited-hooks (atom {}))
(defn new-inhabited-hook 
  "docstring"
  ;; TODO - also specify relative key for use in topological sort, to determine the order the hooks should run.
  [key hook-fn]
  (swap! inhabited-hooks (fn [_]
                          (assoc @inhabited-hooks key hook-fn)))
  (keys @inhabited-hooks))



(new-inhabited-hook
 :primary
 (fn [type-designator]
   (if (class-designator? type-designator)
     true
     :dont-know)))

(defn inhabited?
  "doc string"
  ([type-designator]
   (inhabited? type-designator *inhabited?-default*))
  ([type-designator default]
   (binding [*inhabited?-default* default]
     (case (reduce (fn [_ key-tag]
                     (case ((key-tag @inhabited-hooks) type-designator)
                       (true) (reduced true)
                       (false) (reduced false)
                       nil))
                   :initial
                   (cons :primary (remove #{:primary} (keys @inhabited-hooks))))
       (true) true
       (false) false
       (default type-designator)))))

(defn vacuous? [type-designator]
  (not (inhabited? type-designator)))

(def subtype?-false (fn [_ _] false))

(def subtype?-true (fn [_ _] true))

(def subtype?-error
  (fn [sub-designator super-designator]
    (throw (ex-info (format "subtype? cannot decide %s vs %s" sub-designator super-designator)
                    {:error-type :not-yet-implemented
                     :type-designators [sub-designator super-designator]}))))

(def ^:dynamic *subtype?-default*
  "Default to return when subtype-ness cannot be determined.  This value is a binary
  function which is called with the two type designators in question:
  [sub-designator super-designator]"
  subtype?-error)
                         
(defn subtype?
  "Determine whether sub-designator specifies a type which is a subtype
  of super-designator. Sometimes this decision cannot be made/computed, in
  which case the given default value is interpreted as a binary
  function which is called, and its value returned.  The default value
  of default is a function which raises an exception.  This default
  value is controlled by the dynamic variable *subtype?-default* It is
  assumed that the arguments have already been valided by
  class-designator?"
  ([sub-designator super-designator]
   (subtype? sub-designator super-designator *subtype?-default*))
  ([sub-designator super-designator default]
   (binding [*subtype?-default* default]
     (case (reduce (fn [_ key-tag]
                     (case ((key-tag @subtype-hooks) sub-designator super-designator)
                       (true) (reduced true)
                       (false) (reduced false)
                       nil))
                   :initial
                   (cons :primary (remove #{:primary} (keys @subtype-hooks))))
       (true) true
       (false) false
       (default sub-designator super-designator)))))

(new-subtype-hook
 :primary
 (fn [sub-designator super-designator]
   
   (cond (and (class-designator? super-designator)
              (= Object (resolve super-designator)))
         true
         
         (and (class-designator? sub-designator)
              (class-designator? super-designator))
         (isa? (resolve sub-designator) (resolve super-designator))

         :else
         :dont-know)))

(letfn [(not? [t]
          (and (sequential? t)
               (= 'not (first t))))
        (and? [t]
          (and (sequential? t)
               (= 'and (first t))))
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
                              {:error-type :invalid-type-flags
                               :a-type t
                               :flags flags})))))]

  (new-subtype-hook
   :and
   (fn [t1 t2]
     (if (and (and? t1)
              (some #{t2} (rest t1)))
       false
       :dont-know)))


  (new-disjoint-hook
   :subtype
   (fn [sub super]
     (cond (and (subtype? sub super subtype?-false)
                (inhabited? sub inhabited?-false))
           false

           :else
           :dont-know)))

  (new-disjoint-hook
   :not-disjoint
   (fn [t1 t2]
     (cond (and (not? t2)
                (disjoint? t1 (second t2) disjoint?-false))
           false

           (and (not? t2)
                (class-designator? t1)
                (class-designator? (second t2))
                (= :interface (class-type t1))
                (= :interface (class-type (second t2)))
                (not (= (resolve t1) (resolve (second t2)))))
           false
           
           (and (not? t1)
                (not? t2)
                (class-designator? (second t1))
                (class-designator? (second t2))
                (= :interface (class-type (second t1)))
                (= :interface (class-type (second t2)))
                (not (= (resolve t1) (resolve t2))))
           false
           
           :else :dont-know)))

  (new-disjoint-hook
   :classes
   (fn [t1 t2]
     (if (and (class-designator? t1)
              (class-designator? t2))
       (if (= (resolve t1)
              (resolve t2))
         false
         (case [(class-type t1) (class-type t2)]
           ((:interface :interface)
            (:interface :abstract)
            (:abstract :interface))
           false ;; not disjoint
           
           ((:final :final)
            (:final :interface)
            (:final :abstract))
           (not (subtype? t1 t2 subtype?-error))

           ((:interface :final)
            (:abstract :final))
           (not (subtype? t2 t1 subtype?-error))

           ((:abstract :abstract))
           (not (or (subtype? t1 t2 subtype?-false)
                    (subtype? t2 t1 subtype?-error)))))

       :dont-know)))

  (new-inhabited-hook
   :not
   (fn [t1]
     (if (and (not? t1)
              (class-designator? (second t1)))
       (not (= (resolve (second t1))
               Object))
       :dont-know)))
       
       
  (new-disjoint-hook
   :not
   (fn [t1 t2]
     (cond
       (and (not? t1)
            (= t2 (second t1)))
       true
       
       (and (not? t1)
            (disjoint? (second t1) t2))
       false

       ;; if t1 < t2, then t1 disjoint from (not t2)
       (and ;;(class-designator? t1)
            (not? t2)
            ;;(class-designator? (second t2))
            (subtype? t1 (second t2) subtype?-false))
       true

       (and (class-designator? t1)
            (not? t2)
            (class-designator? (second t2))
            (not (= (resolve (second t2)) (resolve t1)))
            (isa? (resolve (second t2)) (resolve t1)))
       false

       :else
       :dont-know))))

(defn type-min 
  "Find an element of the given sequence which is a subtype
  of some other type and is not =.  not necessarily the global minimum."
  [atoms]
  (some (fn [sub]
          (when (class-designator? sub)
            (let [csub (resolve sub)]
              (some (fn [super]
                      (when (class-designator? super)
                        (let [csuper (resolve super)]
                          (and (not (= csub csuper))
                               (isa? csub csuper)
                               sub)))) atoms)))) atoms))

(defn type-max 
  "Find an element of the given sequence which is a supertype
  of some other type and is not =.  not necessarily the global maximum"
  [atoms]
  (some (fn [sub]
          (when (class-designator? sub)
            (let [csub (resolve sub)]
              (some (fn [super]
                      (when (class-designator? super)
                        (let [csuper (resolve super)]
                          (and (not (= csub csuper))
                               (isa? csub csuper)
                               super)))) atoms)))) atoms))

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
                              (when (and (class-designator? t1)
                                         (class-designator? t2))
                                (let [c1 (resolve t1)
                                      c2 (resolve t2)]
                                  (when (and (not (= c1 c2))
                                             (isa? c1 c2))
                                    (collect t2)))))))]
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
                              (when (and (class-designator? t1)
                                         (class-designator? t2))
                                (let [c1 (resolve t1)
                                      c2 (resolve t2)]
                                  (when (and (not (= c1 c2))
                                             (isa? c2 c1))
                                    (collect t2)))))))]
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
             ((and (not-empty left) (not-empty right)
                   ;; exists t2 in right such that t1 < t2
                   ;; then t1 & !t2 = nil
                   (some (fn [t2] (subtype? (first left) t2 subtype?-false))  right))
              ;; prune
              )

             ((and (not-empty left) (not-empty right)
                   ;; exists t2 in right such that t1 < t2
                   ;; then t1 & !t2 = nil
                   (some (fn [t1] (subtype? t1 (first right) subtype?-false))  left))
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
                  (nil) (recurring (rest items) left right)
                  (:sigma) (recurring (rest items) (cons new-type left) right)
                  (do
                    (recurring (rest items) (cons new-type left) (remove (fn [t2] (disjoint? t2 new-type)) right))
                    (if (some (fn [t2] (disjoint? new-type t2)) left)
                      (recurring (rest items) left right) ;;   Double & !Float, we can omit Float in right
                      (recurring (rest items) left (cons new-type right)))))))))]
    (recurring items () ())))
