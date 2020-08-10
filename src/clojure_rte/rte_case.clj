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

(ns clojure-rte.rte-case
  "This is an empty namespace to fool lein to load this file as part of
  the clojure-rte.core ns.")

(in-ns 'clojure-rte.core)

(defn rte-case-helper
  "Helper function for macro-expanding rte-case"
  [pairs]
  (reduce dfa/synchronized-union
          (map (fn [[index rte]]
                 (rte-to-dfa rte index))
               pairs)))

(def memoized-rte-case-helper (memoize rte-case-helper))

(defmacro rte-case
  ""
  [sequence & clauses]
  (letfn [(compile-clauses [clauses]
            (loop [remaining-clauses clauses
                   index 0
                   used-rtes ()
                   acc-int-rte-pairs []
                   acc-fns []]
              (cond
                (empty? remaining-clauses)
                [acc-fns acc-int-rte-pairs]

                (empty? (rest remaining-clauses))
                (recur (cons '(:* :sigma) remaining-clauses)
                       index
                       used-rtes
                       acc-int-rte-pairs
                       acc-fns)

                :else
                (let [[rte consequent & more] remaining-clauses]
                  (recur more
                         (inc index)
                         (cons rte used-rtes)
                         (conj acc-int-rte-pairs [index `(:and ~rte (:not (:or ~@used-rtes)))])
                         (conj acc-fns `(fn [] ~consequent)))))))]
    
    (let [[fns int-rte-pairs] (compile-clauses clauses)]
      `((~fns (rte-match (memoized-rte-case-helper '~int-rte-pairs) ~sequence))))))


