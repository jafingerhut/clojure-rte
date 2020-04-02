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

(ns clojure-rte.core
  (:require [clojure.set :refer [union intersection]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def *rte-hash* {})
(def *traversal-functions*
  {:client (fn [pattern functions]
             (traverse-pattern pattern functions))
   :type (fn [pattern functions]
           ((:client functions) pattern functions))
   :* (fn [pattern functions]
        (cons :* ((:client functions) pattern functions)))
   :and (fn [patterns functions]
          (cons :and (map (fn [expr]
                            ((:client functions) expr functions)) patterns)))
   :or (fn [patterns functions]
         (cons :or (map (fn [expr]
                           ((:client functions) expr functions)) patterns)))
   :not (fn [pattern functions]
          (cons :not ((:client functions) pattern functions)))
   :cat (fn [patterns functions]
          (cons :cat (map (fn [expr]
                            ((:client functions) expr functions)) patterns)))
   :sigma (fn [pattern functions]
            ((:client functions) pattern functions))
   :empty-set (fn [pattern functions]
                ((:client functions) pattern functions))
   :epsilon (fn [pattern functions]
              ((:client functions) pattern functions))
   })

(defn rte-list? [obj]
  (or (list? obj)
      (= clojure.lang.Cons (type obj))))

(defn traverse-pattern [pattern functions]
  (println (format "traverse-pattern pattern=%s" pattern))
  (letfn [(if-atom []
            (case pattern
              (:epsilon :empty-set :sigma)
              ((functions pattern) pattern functions)

              ((:type functions) pattern functions)))
          (if-nil []
            (println "nil list")
            ((:type functions) () functions))
          (if-singleton-list []
            (println "here singleton list")
            (let [[keyword] pattern]
              (case keyword
                (:or)  (traverse-pattern :empty-set functions)
                (:and) (traverse-pattern :sigma functions)
                (:cat) (traverse-pattern :epsilon functions)
                (:not
                 :?
                 :+
                 :permute
                 :rte) (throw (Exception.
                               (format "invalid pattern %s" pattern)))
                ;; case-else
                ((:type functions) pattern functions))))
          (if-long-list []
            (println "long list")
            (let [[token & operands] pattern]
              (println (format "  cond-else token=%s operands=%s" token operands))
              (case token
                (:rte) (do (assert (= 1 (count operands))
                                   (format "invalid pattern %s" pattern))
                           (let [[name] operands]
                             (assert (contains? *rte-hash* name)
                                     (format "invalid rte name %s in pattern %s"
                                             name pattern))
                             (traverse-pattern (*rte-hash* name) functions)))

                (:permute)
                (throw (Exception.
                        (format "not yet implemented %s" pattern)))
                
                (:or
                 :and
                 :cat)
                (if (= 1 (count operands))
                  (traverse-pattern (first operands) functions)
                  ((functions token) operands functions))
                
                (:not
                 :*)
                (do (assert (= 1 (count operands))
                            (format "invalid pattern %s" pattern))
                    ((functions token) (first operands) functions))
                
                (:+)
                (do (assert (= 1 (count operands))
                            (format "invalid pattern %s" pattern))
                    (traverse-pattern `(:cat ~(first operands) (:* ~(first operands))) functions))
                
                (:?)
                (do (assert (= 1 (count operands))
                            (format "invalid pattern %s" pattern))
                    (traverse-pattern `(:or :epsilon
                                            ~(first operands)) functions))
                ;;case-else
                ((:type functions) pattern functions))))]
    (cond (not (rte-list? pattern))
          (if-atom)
          
          (= pattern ())
          (if-nil)
          
          (not (rest pattern)) ;; singleton list
          (if-singleton-list)
          
          ;; cond-else (:keyword args) or list-expr
          :else (if-long-list))))

(defn rte-constantly [x]
  (fn [_ _]
    x))
(defn rte-identity [x y]
  x)
  
(defn typep [a-value a-type]
  (isa? (type a-value) a-type))

(defn disjoint? [t1 t2]
  (let [ancestors-1 (ancestors t1)
        ancestors-2 (ancestors t2)]
    (and (not-any? (fn [a2] (contains? ancestors-1 a2)) ancestors-2)
         (not-any? (fn [a1] (contains? ancestors-2 a1)) ancestors-1))))

(defn nullable [expr]
  (traverse-pattern expr
                    (assoc *traversal-functions*
                           :empty-set (rte-constantly false)
                           :epsilon (rte-constantly true)
                           :type (fn [operand functions]
                                   (some (fn [x]
                                           (typep x operand))
                                         [() []]))
                           :* (rte-constantly true)
                           :cat (fn [operands functions]
                                  (every? nullable operands))
                           :and (fn [operands functions]
                                  (every? nullable operands))
                           :or (fn [operands functions]
                                 (println "some nullable operands?")
                                 (some nullable operands))
                           :not (fn [operand functions]
                                  (not (nullable operand))))))

(defn first-types [expr]
  (letfn [(mr [operands functions]
            (reduce (fn [acc next]
                      (union acc (first-types next))) #{} operands))]
  (traverse-pattern expr
                    (assoc *traversal-functions*
                           :epsilon (rte-constantly #{})
                           :empty-set (rte-constantly #{})
                           :sigma (rte-constantly #{:sigma})
                           :type (fn [operand functions]
                                   #{operand})
                           :or mr
                           :and mr
                           :not (fn [operand functions]
                                  (first-types operand))
                           :cat (fn [[head & tail] functions]
                                  (cond (nullable head)
                                        (union (first-types head)
                                               (first-types (cons :cat tail)))

                                        :else
                                        (first-types head)))
                           :* (fn [operand functions]
                                (first-types operand))))))

(defn canonicalize-pattern-once [re]
  (traverse-pattern re
                    (assoc *traversal-functions*
                           :type rte-identity
                           :empty-set rte-identity
                           :epsilon rte-identity
                           :* (fn [operand functions]
                                (case (canonicalize-pattern operand)
                                  :empty-word :epsilon
                                  :empty-set  :epsilon
                                  :sigma      re
                                  (if (rte-list? operand)
                                    (let [[head & tail] operand]
                                      (if (= head :*)
                                        operand
                                        re))
                                    re))))))

(defn canonicalize-pattern [pattern]
  (loop [old-pattern []
         new-pattern pattern]
    (if (= old-pattern new-pattern)
      old-pattern
      (recur new-pattern (canonicalize-pattern-once new-pattern)))))

(defn derivative [expr wrt]
  (letfn [(walk [patterns]
            (map (fn [p]
                   (derivative (canonicalize-pattern p) wrt))
                 patterns))]
    (canonicalize-pattern
     (traverse-pattern expr
                       (assoc *traversal-functions*
                              :epsilon (rte-constantly :empty-set)
                              :empty-set (rte-constantly :empty-set)
                              :type (fn [type functions]
                                      (cond (= wrt type)
                                            :epsilon

                                            (disjoint? wrt type)
                                            :empty-set

                                            :else
                                            (throw (Exception.
                                                    (format "cannot compute derivative of %s wrt %s because the types are partially intersecting" type wrt)))))
                              :or (fn [operands functions]
                                    (cons :or (walk operands)))
                              :and (fn [operands functions]
                                     (cons :and (walk operands)))
                              :not (fn [operand functions]
                                     (cons :not (walk (list operand))))
                              :cat (fn [[head & tail] functions]
                                     (letfn [(term1 []
                                               `(:cat ~(derivative head wrt)
                                                      ~@tail))
                                             (term2 []
                                               (derivative `(:cat ~@tail) wrt))]
                                       (cond
                                         (nullable head) ;; nu = :epsilon
                                         `(:or ~(term1) ~(term2))
                                         :else
                                         (term1))))
                              :* (fn [operand functions]
                                   `(:cat ,(derivative operand wrt) (:* ,operand))))))))
                                   

