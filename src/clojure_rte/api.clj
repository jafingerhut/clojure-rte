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

(ns clojure-rte.api
  "This is an empty namespace to fool lein to load this file as part of
  the clojure-rte.core ns.")

(in-ns 'clojure-rte.core)

(defn dispatch [obj caller]
  (cond (instance? Dfa obj)
        :Dfa

        (sequential? obj)
        :sequential

        :else
        (throw (ex-info (format "invalid argument for %s, expecting, sequential? or Dfa, not %s"
                                caller obj)
                        {:obj obj
                         }))))

(defmulti rte-trace
  "Given a compiled rte, find a sequence of types which satisfy the corresponding pattern."
  (fn [rte]
    (dispatch rte 'rte-trace)))
  
(defmethod rte-trace :sequential
  [pattern]
  (rte-trace (rte-compile pattern)))

(defmethod rte-trace :Dfa
  [dfa]
  (let [state-vec (:states dfa)]
    (letfn [(recurring [state path lineage]
              (cond
                (:accepting (state-vec state)) path
                (some #{state} lineage) false
                :else (some (fn [[type dst-state]]
                              (recurring dst-state (conj path type) (conj lineage state)))
                            (:transitions (state-vec state))))
              )]
      (recurring 0 [] ()))))

(defmulti rte-inhabited?
  (fn [rte]
    (dispatch rte 'rte-inhabited?)))

(defmethod rte-inhabited? :sequential [pattern]
  (rte-inhabited? (rte-compile pattern)))

(defmethod rte-inhabited? :Dfa [dfa]
  (some :accepting (:states dfa)))

(defn rte-vacuous? [dfa]
  (not (rte-inhabited? dfa)))

(defmulti rte-match
  "Given an rte pattern or finite automaton generated by rte-to-dfa (or rte-compile), 
   determine whether the given sequence, items, matches the regular type expression.

   If the caller wishes to check more than one sequence against the same
   pattern, it is probably better to call rte-compile, to get an automaton, and
   use that same automaton in several calls to rte-match to avoid
   multiple conversions/look-ups, as the correspondence of pattern
   to compiled Dfa is maintained via the memoize function."

  (fn [rte _items]
   (dispatch rte 'rte-match)))

(defmethod rte-match :sequential
  [pattern items]
  (rte-match (rte-compile pattern) items))

(defmethod rte-match :Dfa
  [dfa items]
  (let [state-vec (:states dfa)]
    (letfn [(consume [state-index item]
              (let [state-obj (state-vec state-index)]
                (cl-cond
                 ((:sync-state state-obj)
                  (reduced (:accepting state-obj)))
                 ((some (fn [[type next-state-index]]
                          (if (ty/typep item type)
                            next-state-index
                            false))
                        (:transitions state-obj)))
                 (:else (reduced false)))))]
      (let [final-state (reduce consume 0 items)]
        ;; final-state may be integer disgnating the state which was
        ;;  reached on iterating successfully through the input
        ;;  sequence, items.  Or final-state may true or false, if the
        ;;  iteration finished without iterating through the entire
        ;;  sequence.  Two such cases, we found ourselves in a
        ;;  sync-state, or we encountered a item for which no transition
        ;;  was possible.
        (case final-state 
          (true) true
          (false) false
          (:accepting (state-vec final-state)))))))

