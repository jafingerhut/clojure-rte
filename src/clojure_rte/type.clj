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
  given type designator need not be a class.  The given type 
  designator may be a (1) class, (2) a symbol resolving to a class, or
  (3) a CL style type designator such as
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
  (cond
    (class? a-type)
    (instance? a-type a-value)
    
    (not (symbol? a-type))
    (throw (ex-info (format "typep: invalid type of %s, expecting a symbol or class , got %s" a-type (type a-type))
                    {:error-type :invalid-type-designator
                     :a-type a-type
                     :a-value a-value
                     }))

    (not (resolve a-type))
    (throw (ex-info (format "typep: invalid type %s, no resolvable value" a-type)
                    {:error-type :invalid-type-designator
                     :a-type a-type
                     :a-value a-value
                     }))

    (not (class? (resolve a-type)))
    (throw (ex-info (format "typep: invalid type of %s, does not resolve to a class, got %s of type %s"
                            a-type (resolve a-type) (type (resolve a-type)))
                    {:error-type :invalid-type-designator
                     :a-type a-type
                     :a-value a-value
                     }))
    :else
    (isa? (type a-value) (resolve a-type))))

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

(defn disjoint?-false-warn [t1 t2]
  (cl-format true "disjoint? cannot decide ~A vs ~A -- assuming not disjoint~%" t1 t2)
  false)

(def ^:dynamic *disjoint?-default*
  "Default to return when disjoint-ness cannot be determined.  This value is a binary
  function which is called with the two type designators in question:
  [t1 t2]"
  disjoint?-false-warn)

(defn inhabited?-error [type-designator]       
  (throw (ex-info (format "inhabited? cannot decide %s" type-designator)
                  {:error-type :not-yet-implemented
                   :type-designator [type-designator]})))

(def ^:dynamic *inhabited?-default*
  "doc string"
  inhabited?-error
)

(defn subtype?-error [sub-designator super-designator]
  (throw (ex-info (format "subtype? cannot decide %s vs %s" sub-designator super-designator)
                  {:error-type :not-yet-implemented
                   :type-designators [sub-designator super-designator]})))

(def ^:dynamic *subtype?-default*
  "Default to return when subtype-ness cannot be determined.  This value is a binary
  function which is called with the two type designators in question:
  [sub-designator super-designator]"
  subtype?-error)

(def sort-method-keys
  "Given a multimethod object, return a list of method keys.
  The :primary method comes first in the return list and the :default
  method has been filtered away."
  (memoize (fn [f]
             (cons :primary (remove #{:primary :default} (keys (methods f)))))))

(defmulti -disjoint?
  "This function should never be called.
  Applications may install methods via (defmethod -disjoint? ...).
  The method accepts two arguments which are type-designators,
  [t1 t2],  pontentially application specific.
  The method should examine the designated types to determine whether
  the designated types are disjoint, i.e., whether they have no
  element in common, i.e., whether their intersection is empty.
  The method must return true, false, or :dont-know.
  The function, disjoint?, will call (-disjoint? t1 t2)
  and also (-disjoint? t2 t1) if necessary, therefore
  the methods need only check one or the other.
  When disjoint? (the public calling interface) is called,
  the methods of -disjoint? are called in some order
  (:primary first) until one method returns true or false,
  in which case disjoint? returns that value.
  If no method returns true or false, then the function
  *disjoint?-default* is called, and its value returned.
  If disjoint? is called with a 3rd argument, then
  *disjoint?-default* is dynamically bound to that value."
  (fn [t1 t2]
    (throw (ex-info "-disjoint? should not be called directly"
                    {:error-type :should-not-be-called-directly
                     :t1 t1
                     :t2 t2}))))

(defn disjoint?
  "Predicate to determine whether the two types overlap."
  ([t1 t2]
   (disjoint? t1 t2 *disjoint?-default*))
  ([t1 t2 default]
   {:pre [(fn? default)]
    :post [(fn [v] (#{true false :dont-know} v))]}
   (binding [*disjoint?-default* default]
     (loop [[k & ks] (sort-method-keys -disjoint?)]
       (case ((k (methods -disjoint?)) t1 t2)
         (true) true
         (false) false
         (case ((k (methods -disjoint?)) t2 t1)
           (true) true
           (false) false
           (if ks
             (recur ks)
             (default t1 t2))))))))

(defmethod -disjoint? :primary [t1 t2]
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
    :dont-know))

(defn class-designator? [t]
  (and (symbol? t)
       (resolve t)
       (class? (resolve t))))

(letfn [(and? [t]
          (and (sequential? t)
               (= 'and (first t))))]

  (defmethod -disjoint? :and [t1 t2]
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
          :dont-know)))

(letfn [(=? [t]
          (and (sequential? t)
               (= '= (first t))
               (= (count t) 2)))
        (not? [t]
          (and (sequential? t)
               (= 'not (first t))))
        (member? [t]
          (and (sequential? t)
               (= 'member (first t))))]

  (defmethod -disjoint? := [t1 t2]
    (cond (=? t1)
          (not (typep (second t1) t2))
          
          ;; (= ...) is finite, types are infinite
          ;; (disjoint? '(not (= 1 2 3)) 'Long)
          (and (not? t1)
               (=? (second t1))
               (class-designator? t2))
          false
          
          :else
          :dont-know))
          
  (defmethod -disjoint? :member [t1 t2]
    (cond (member? t1)
          (every? (fn [e1]
                    (not (typep e1 t2))) (rest t1))

          ;; (member ...) is finite, types are infinite
          ;; (disjoint? '(not (member 1 2 3)) 'Long)
          (and (not? t1)
               (member? (second t1))
               (class-designator? t2))
          false
          
          :else
          :dont-know)))

(defmulti -subtype?
  "This function should never be called.
  Applications may install methods via (defmethod -subtype? ...).
  The method accepts two arguments which are type-designators,
  [sub-designator super-designator],  pontentially application specific.
  The method should examine the designated types to determine whether
  they have a subtype relation, and return true, false, or :dont-know.
  When subtype? (the public calling interface) is called,
  the methods of -subtype? are called in some order
  (:primary first) until one method returns true or false,
  in which case subtype? returns that value.
  If no method returns true or false, then the function
  *subtype?-default* is called, and its value returned.
  If subtype? is called with a 3rd argument, then
  *inhabited?-default* is dynamically bound to that value."
  (fn [sub super]
    (throw (ex-info "-subtype? should not be called directly"
                    {:error-type :should-not-be-called-directly
                     :sub sub
                     :super super}))))

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
   {:pre [(fn? default)]
    :post [(fn [v] (#{true false :dont-know} v))]}
   (binding [*subtype?-default* default]
     (loop [[k & ks] (sort-method-keys -subtype?)]
       (let [s ((k (methods -subtype?)) sub-designator super-designator)]
         (case s
           (true false) s
           (if ks
             (recur ks)
             (default sub-designator super-designator))))))))

(defmulti -inhabited?
  "This function should never be called.
  Applications may install methods via (defmethod -inhabited? ...).
  The method accepts one argument which is a type-designator,
  pontentially application specific.
  The method should examine the type designator and return
  true, false, or :dont-know.
  When inhabited? (the public calling interface) is called,
  the methods of -inhabited? are called in some order
  (:primary first) until one method returns true or false,
  in which case inhabited? returns that value.
  If no method returns true or false, then the function
  *inhabited?-default* is called, and its value returned.
  If inhabited? is called with a 3rd argument, then
  *inhabited?-default* is dynamically bound to that value."
  (fn [type-designator]
    (throw (ex-info "-inhabited? should not be called directly"
                    {:type-designator type-designator
                     :error-type :should-not-be-called-directly}))))

(defn inhabited?
  "Given a type-designator, perhaps application specific,
  determine whether the type is inhabited, i.e., not the
  empty type."
  ([type-designator]
   (inhabited? type-designator *inhabited?-default*))
  ([type-designator default]
   {:pre [(fn? default)]
    :post [(fn [v] (#{true false :dont-know} v))]}
   (binding [*inhabited?-default* default]
     (loop [[k & ks] (sort-method-keys -inhabited?)]
       (case ((k (methods -inhabited?)) type-designator)
         (true) true
         (false) false
         (if ks
           (recur ks)
           (default type-designator)))))))

(defmethod -inhabited? :primary [type-designator]
  (if (class-designator? type-designator)
    true
    :dont-know))

(defn vacuous? 
  "Determine whether the specified type is empty, i.e., not inhabited."
  [type-designator]
  (not (inhabited? type-designator)))

(defmethod -subtype? :primary [sub-designator super-designator]
  (cond (and (class-designator? super-designator)
             (= Object (resolve super-designator)))
        true
        
        (and (class-designator? sub-designator)
             (class-designator? super-designator))
        (isa? (resolve sub-designator) (resolve super-designator))
        
        :else
        :dont-know))

(letfn [(not? [t]
          (and (sequential? t)
               (= 'not (first t))))
        (and? [t]
          (and (sequential? t)
               (= 'and (first t))))
        (member? [t]
          (and (sequential? t)
               (= 'member (first t))))
        (=? [t]
          (and (sequential? t)
               (= '= (first t))))
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

  (defmethod -subtype? := [sub super]
    (cond (=? sub)
          (subtype? (cons 'member (rest sub)) super)

          (=? super)
          (subtype? sub (cons 'member (rest super)))

          :else
          :dont-know))

  (defmethod -subtype? :member [sub super]
    (cond (member? sub)
          (every? (fn [e1]
                    (typep e1 super)) (rest sub))

          ;; (subtype? 'Long '(member 1 2 3))
          (and (member? super)
               (class-designator? sub)) ;; assuming a class is infinite
          false

          ;; (subtype? 'Long '(not (member 1 2 3))) ==> false
          ;; (subtype? 'Long '(not (member 1.1 2 3))) ==> false
          ;; (subtype? 'Long '(not (member 1.1 2.2 3.3))) ==> true
          (and (not? super)
               (class-designator? sub)
               (member? (second super)))
          (every? (fn [e2]
                    (not (typep e2 sub))) (rest (second super)))

          :else
          :dont-know))
  
  (defmethod -subtype? :and [t1 t2]
    (if (and (and? t1)
             (some #{t2} (rest t1)))
      false
      :dont-know))

  (defmethod -disjoint? :subtype [sub super]
    (cond (and (subtype? sub super (constantly false))
               (inhabited? sub (constantly false)))
          false
          
          :else
          :dont-know))

  (defmethod -disjoint? :not-disjoint [t1 t2]
    (cond (and (not? t2)
               (disjoint? t1 (second t2) (constantly false)))
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
          
          :else :dont-know))

  (defmethod -disjoint? :classes [t1 t2]
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
          (not (or (subtype? t1 t2 (constantly false))
                   (subtype? t2 t1 subtype?-error)))))
      
      :dont-know))

  (defmethod -inhabited? :not [t1]
    (if (and (not? t1)
             (class-designator? (second t1)))
      (not (= (resolve (second t1))
              Object))
      :dont-know))

  (defmethod -inhabited? :member [t1]
    (if (member? t1)
      (boolean (rest t1))
      :dont-know))


  (defmethod -inhabited? := [t1]
    (if (=? t1)
      true
      :dont-know))
       
  (defmethod -disjoint? :not [t1 t2]
    (cond
      (and (not? t2)
           (= t1 (second t2)))
      true
      
      (and (not? t2)
           (disjoint? (second t2) t1 (constantly false)))
      false
      
      ;; if t1 < t2, then t1 disjoint from (not t2)
      ;; (disjoint? '(member 1 2 3) '(not (member a b c 1 2 3)))
      (and (not? t2)
           (subtype? t1 (second t2) (constantly false))
           (not (subtype? (second t2) t1 (constantly true))))
      true

      ;; (disjoint? '(member a b c 1 2 3) '(not (member 1 2 3)))
      (and (not? t2)
           (subtype? (second t2) t1 (constantly false))
           (not (subtype? t1 (second t2) (constantly true))))
      false

      (and (class-designator? t1)
           (not? t2)
           (class-designator? (second t2))
           (not (= (resolve (second t2)) (resolve t1)))
           (isa? (resolve (second t2)) (resolve t1)))
      false
      
      (and (class-designator? t1)
           (not? t2)
           (class-designator? (second t2))
           ;; and neither is final
           (not (= :final (class-designator? (resolve t1))))
           (not (= :final (class-designator? (resolve (second t2))))))
      false

      ;; I don't know the general form of this, so make it a special case for the moment.
      ;; (ty/disjoint? '(and Long (not (member 2 3 4))) 'java.lang.Comparable)
      ;;                      A   (not B)                     C
      ;; should return false
      ;; TODO generalize this special case.
      ;; If B < A and A !< B    and A < C and C !< A
      ;;   then (and A !B) is NOT disjoint from C
      (and (and? t1) ;; t1 of the form (and ...)
           (= 3 (count t1)) ;; t1 of the form (and x y)
           (not? (first (rest (rest t1))))  ;; t1 of the form (and x (not y))
           (let [[_ A [_ B]] t1
                 C t2]
             (and (subtype? B A (constantly false))
                  (not (subtype? A B (constantly true)))
                  (subtype? A C (constantly false))
                  (not (subtype? C A (constantly true))))))
      false

      
      ;; (ty/disjoint? '(and String (not (member a b c 1 2 3))) 'java.lang.Comparable)
      ;;                       A    (not B)                     C
      ;;  since A and B are disjoint
      ;;  we may ask (disjoint? A C)
      (and (and? t1) ;; t1 of the form (and ...)
           (= 3 (count t1)) ;; t1 of the form (and x y)
           (not? (first (rest (rest t1))))  ;; t1 of the form (and x (not y))
           (let [[_ A [_ B]] t1
                 C t2]
             (disjoint? A B)))
      (disjoint? (second t1) t2)
      
      :else
      :dont-know)))

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
                   (some (fn [t2] (subtype? (first left) t2 (constantly false)))  right))
              ;; prune
              )

             ((and (not-empty left) (not-empty right)
                   ;; exists t2 in right such that t1 < t2
                   ;; then t1 & !t2 = nil
                   (some (fn [t1] (subtype? t1 (first right) (constantly false)))  left))
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

