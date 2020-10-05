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

(ns clojure-rte.genus-extend
  "This is an empty namespace to fool lein to load this file as part of
  the clojure-rte.core ns.")

(in-ns 'clojure-rte.core)

(defmethod gns/typep 'rte [a-value [_a-type pattern]]
  (and (sequential? a-value)
       (rte-match pattern a-value)))

(defmethod gns/valid-type? 'rte [[_ pattern]]
  (boolean (rte-compile pattern)))

(letfn [(rte? [t]
          (and (sequential? t)
               (= 'rte (first t))))
        (not? [t]
          (and (sequential? t)
               (= 'not (first t))))        
        (and? [t]
          (and (sequential? t)
               (= 'and (first t))))]
  
  (defmethod gns/-inhabited? :rte [t1]
    (if (rte? t1)
      (boolean (rte-inhabited? (rte-compile (second t1))))
      :dont-know))
       
  (defmethod gns/-disjoint? :rte [t1 t2]
    (cond (and (rte? t1)
               (rte? t2))
          (let [[_ pat1] t1
                [_ pat2] t2]
            (rte-vacuous? (rte-compile `(:and ~pat1 ~pat2))))

          ;; (disjoint? (rte ...) clojure.lang.IPersistentVector )
          (and (rte? t1)
               (gns/class-designator? t2)
               (or (isa? (gns/find-class t2) clojure.lang.Seqable)
                   (isa? (gns/find-class t2) clojure.lang.Sequential)))
          false

          ;; (disjoint? (not (rte ...)) clojure.lang.IPersistentVector )
          (and (not? t1)
               (rte? (second t1))
               (gns/class-designator? t2)
               (or (isa? (gns/find-class t2) clojure.lang.Seqable)
                   (isa? (gns/find-class t2) clojure.lang.Sequential)))
          true
          
          (and (rte? t1)
               (not? t2)
               (rte? (second t2)))
          (let [[_ pat1] t1
                [_ [_ pat2]] t2]
            (rte-vacuous? (rte-compile `(:and ~pat1 (:not ~pat2)))))
          
          (and (rte? t1)
               (gns/class-designator? t2)
               (isa? (gns/find-class t2) java.lang.CharSequence))
          (let [[_ pat1] t1]
            (rte-vacuous? (rte-compile `(:and ~pat1 (:* java.lang.Character)))))
          
          (and (rte? t1)
               (gns/class-designator? t2)
               (not (isa? (gns/find-class t2) clojure.lang.Sequential)))
          true
          
          (and (not? t1)
               (rte? (second t1))
               (gns/class-designator? t2)
               (not (isa? (gns/find-class t2) clojure.lang.Sequential)))
          false
          
          (and (rte? t1)
               (not? t2)
               (gns/class-designator? (second t2))
               (not (isa? (gns/find-class (second t2)) clojure.lang.Sequential)))
          false
          
          :else :dont-know))

  (defmethod gns/-subtype? :rte [sub-designator super-designator]
    (cond (and (rte? sub-designator)
               (rte? super-designator))
          (let [[_ pat-sub] sub-designator
                [_ pat-super] super-designator]
            (rte-vacuous? (rte-compile `(:and ~pat-sub (:not ~pat-super)))))
          
          (and (rte? super-designator)
               (gns/class-designator? sub-designator)
               (isa? (gns/find-class sub-designator) java.lang.CharSequence))
          (gns/subtype? '(rte (:* java.lang.Character)) super-designator)
          
          (and (rte? sub-designator)
               (gns/class-designator? super-designator)
               (isa? (gns/find-class super-designator) java.lang.CharSequence))
          (gns/subtype? sub-designator '(rte (:* java.lang.Character)))
          
          (and (rte? super-designator)
               (gns/class-designator? sub-designator)
               (not (isa? (gns/find-class sub-designator) clojure.lang.Sequential)))
          false
          
          (and (rte? sub-designator)
               (gns/class-designator? super-designator)
               (not (isa? (gns/find-class super-designator) clojure.lang.Sequential)))
          false
          
          (and (rte? super-designator)
               (and? sub-designator)
               (some (fn [and-operand]
                       (rte? and-operand)
                       (gns/subtype? and-operand super-designator
                                    (constantly false))) (rest sub-designator)))
          true
          
          :else :dont-know)))

