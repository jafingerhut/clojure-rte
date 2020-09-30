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

(ns clojure-rte.parse-function
  "Function parser to reverse engineer the type-predicate/type mapping
  E.g., list? -> clojure.lang.IPersistentList
  and   integer? -> (or Integer Long clojure.lang.BigInt BigInteger Short Byte)"
)

(in-ns 'clojure-rte.core)

(defn- get-fn-source [fn-name]
  (let [src-str (source-fn fn-name)]
    (cond
      (= nil src-str)
      nil

      :else
      (read-string src-str))))

(declare type-predicate-to-type-designator)

(defn extract-type-from-expression [var-1 expr]
  (destructuring-case
   expr

   ;; (instance? type-designator x)
   [[_i type-designator var-2] {_i (= instance?)
                                type-designator clojure.lang.Symbol
                                var-2 clojure.lang.Symbol
                                }]
   (if (= var-2 var-1)
     type-designator
     nil)

   ;; (symbol? x)
   [[type-predicate var-2] {type-predicate clojure.lang.Symbol
                            var-2 clojure.lang.Symbol}]
   (if (= var-1 var-2)
     (type-predicate-to-type-designator type-predicate)
     nil)
   
   ;; (or ...)
   [[_or & exprs] {_or (= or)}]
   (cons 'or (map (fn [ex]
                    (extract-type-from-expression var-1 ex))
                  exprs))

   [[& _] {}]
   nil))

(defn type-predicate-to-type-designator [type-predicate]
  (assert (symbol? type-predicate))
  (destructuring-case
   (get-fn-source type-predicate)

   [[] {}]
   nil

   [[defn name doc-string attr-map [var-1]
     expr]
    {defn       (= defn)
     name       clojure.lang.Symbol
     doc-string String
     attr-map   clojure.lang.IPersistentMap
     var-1      clojure.lang.Symbol
     }]
   (extract-type-from-expression var-1 expr)

   [[& _] {}]
   nil))

(defn expand-satisfies [type-designator]
  (cl-cond
   ((not (sequential? type-designator))
    type-designator)

   ((empty? type-designator)
    type-designator)

   ((not= 'satisfies (first type-designator))
    type-designator)

   ((empty? (rest type-designator))
    type-designator)

   ((not (empty? (rest (rest type-designator))))
    type-designator)

   ((type-predicate-to-type-designator (second type-designator)))

   (:else
    type-designator)))
   
