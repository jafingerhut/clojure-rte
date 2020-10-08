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
  the clojure-rte.core ns."
  (:require [clojure-rte.genus-rte])
)

(in-ns 'clojure-rte.core)

(defn- rte-case-clauses-to-dfa
  "Helper function for macro-expanding rte-case.
  Returns a Dfa which is the union of the input clauses."
  [pairs]
  (reduce (fn [a b]
            (dfa/synchronized-union a b))
          (map (fn [[index rte]]
                 (rte-to-dfa rte index))
               pairs)))

(def memoized-rte-case-clauses-to-dfa rte-case-clauses-to-dfa )
;;(def memoized-rte-case-clauses-to-dfa (memoize rte-case-clauses-to-dfa))

(defn ensure-fns-index
  "Internal function used in macro expansion of rte-case, to assure the index is in range
  or print a reasonable error message."
  [index num-fns]
  (cond (not (integer? index))
        (throw (ex-info (cl-format false "rte-match returned non-integer ~A" index)
                        {}))
        (< index 0)
        (throw (ex-info (cl-format false "rte-match returned negative integer ~A" index)
                        {}))

        (<= num-fns index)
        (throw (ex-info (cl-format false "rte-match returned index out of range ~A <= ~A"
                                   num-fns index)
                        {}))
        :else
        index))

(defmacro rte-case
  "Takes an expression, and a set of clauses.
  The expression should evaluate to an object which is `sequential?`.
  Each clause is of the form
     rte consequent.
  The rte is NOT evaluated, and should not be quoted.  It should be
  a syntacticaly correct regular type expression.
  If the first rte matches the sequence, then the consequent is evaluated
  and its value is returned.   If the rte fails to match the rte, then
  the second clause is examined.   The semantics are that the rte's are
  considered in order, top to bottom, and the first one which matches
  causes the corresponding consequent to be evaluated.
  However, the sequence is traverse only once, so the matching process
  is more efficient than a sequence of consecutive calls to
  rte-match."
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

                :else
                (let [[rte consequent & more] remaining-clauses]
                  (recur more
                         (inc index)
                         (cons rte used-rtes)
                         (conj acc-int-rte-pairs [index (canonicalize-pattern `(:and ~rte (:not (:or ~@used-rtes))))])
                         (conj acc-fns `(fn [] ~consequent)))))))]
    (if (odd? (count clauses))
      (throw (IllegalArgumentException. (str "rte-case, odd number of clauses is not supported. No matching clause: " (last clauses)))))
    
    (let [[fns int-rte-pairs] (compile-clauses clauses)
          num-fns (count fns)
          var (gensym "index")]
      `((~fns (ensure-fns-index (rte-match (memoized-rte-case-clauses-to-dfa '~int-rte-pairs) ~sequence)
                                ~num-fns))))))

(defn- lambda-list-to-rte
  "Helper function for destructuring-case macro.
  Returns an rte either of one of the following forms:
    (:cat ... (:* ...)) -- if the given lambda-list contains &
    (:cat ...) -- if the given lambda list only has required parametes."
  [lambda-list types-map]
  (assert (map? types-map))
  (letfn [(pretty-and [a b]
            (cond
              (= :sigma a)  b
              (= :sigma b)  a
              (= a b)       a
              :else         (list 'and a b)))]
    (loop [required lambda-list
           others ()
           prefix-rte []
           suffix-rte []
           parsed []]
      (cond (and (empty? required)
                 (empty? others))
            ;; finished parsing
            (if (empty? suffix-rte)
              `(:cat ~@prefix-rte)
              `(:cat ~@prefix-rte (:* ~@suffix-rte)))

            (and (not (empty? required))
                 (= '& (first required)))
            ;; found & in the correct place
            (recur nil ; required
                   (rest required) ; rest
                   prefix-rte ; prefix-rte
                   suffix-rte ; suffix-rte
                   (conj parsed '&))

            (not (empty? required))
            (let [var (first required)]
              (recur (rest required)
                     others
                     (conj prefix-rte
                           (cond (symbol? var)
                                 ;; parsing required section, found a var
                                 (pretty-and (get (meta var) :tag :sigma)
                                             (get types-map var :sigma))
                                 (vector? var)
                                 ;; parsing required section, found a vector
                                 (list 'rte
                                       (lambda-list-to-rte (first required) types-map))

                                 :else ;; found non-symbol non-vector
                                 (throw (ex-info (cl-format false "invalid lambda-list ~A, at ~A"
                                                            lambda-list required)
                                                 {:error-type "cannot parse prefix"
                                                  :lambda-list lambda-list
                                                  :parsed parsed
                                                  :unparsed required}))))
                     suffix-rte
                     (conj parsed var)))

            (and (not (empty? others))
                 (not (empty? suffix-rte)))
            (throw (ex-info (cl-format false "invalid lambda-list ~A, at ~A"
                                       lambda-list others)
                            {:error-type "cannot parse suffix"
                             :lambda-list lambda-list
                             :parsed parsed
                             :unparsed others}))

            (and (not (empty? others))
                 (symbol? (first others)))
            (let [var (first others)]
              (recur required
                     (rest others)
                     prefix-rte
                     (conj suffix-rte
                           (pretty-and (get (meta var) :tag :sigma)
                                       (get types-map var :sigma)))
                     (conj parsed var)))

            (not (empty? others))
            (throw (ex-info (cl-format false "invalid lambda-list ~A, at ~A"
                                       lambda-list others)
                            {:error-type "cannot parse suffix"
                             :lambda-list lambda-list
                             :parsed parsed
                             :unparsed others}))))))
            
            

(defmacro destructuring-case
  "After evaluating the expression (only once) determine whether its return value
  conforms to any of the given lambda lists and type restrictions.  If so,
  bind the variables as if by let, and evaluate the corresponding form."
  [expr & pairs]
  (cond
    (not= 0 (mod (count pairs) 2))
    (throw (ex-info (cl-format false "destructuring-case expects multiple of 2 number of arguments after the first: not ~A, ~A"
                               (count pairs) (apply list 'destructuring-case expr pairs))
                    {:error-type :invalid-destructuring-case-call-site
                     :expr expr
                     :pairs pairs}))

    :else
    (let [var (gensym "v")]
      (letfn [(expand-multi-restrictions [types-map]
                (assert (map? types-map))
                (merge types-map
                       (into {} (for [key (keys types-map)
                                      :when (sequential? key)
                                      :let [type-1 (get types-map key)]
                                      var key
                                      :let [type-2 (get types-map var)]]
                                  (if type-2
                                    [var (list 'and type-1 type-2)]
                                    [var type-1])))))
              (conv-1-case-clause [[[lambda-list types-map] consequence]]
                (assert (map? types-map)
                        (cl-format false "destructuring-case expecting a map, not ~A" types-map))
                [(lambda-list-to-rte lambda-list (expand-multi-restrictions types-map))
                 `(let [~lambda-list ~var]
                    ~consequence)])]
        (let [pairs (partition 2 pairs)
              cases (mapcat conv-1-case-clause pairs)]
          `(let [~var ~expr]
             (rte-case ~var ~@cases (:* :sigma) nil)))))))

(defmacro destructuring-fn-many
  "Internal macro used in the exapansion of destructuring-fn"
  [& args]
  (cond (empty? args)
        nil

        (and (not (symbol? (first args)))
             (not (= nil (first args))))
        `(destructuring-fn-many nil ~@args)

        :else
        (let [var (gensym "fn-var-")
              [name & given-clauses] args
              clauses (mapcat (fn [[structured-lambda-list & exprs]]
                                `(~structured-lambda-list (do ~@exprs))) given-clauses)
              ]
          `(fn
             ~@(if name (list name) nil) ;; either name or nothing
             [& ~var]
             (destructuring-case ~var
                                 ~@clauses)))))

(defmacro destructuring-fn
  "params => positional-params* , or positional-params* & next-param
  positional-param => binding-form
  next-param => binding-form
  name => symbol
  constr-map => a map to constrain variables by type.
  
  Defines a function.  When the function is called,  the
  clause whose parameters match the argument in structure and type
  will be evaluated;  otherwise nil is returned."
  {:forms '[(destructuring-fn name? [[params* ] constr-map] exprs*)
            (destructuring-fn name? ([[params*] constr-map ] exprs*)+)]}
  [& args]
  (destructuring-case
   args
   [[] {}]
   (throw (IllegalArgumentException. 
           "destructuring-fn, empty argument list not supported"))

   [[name & others] {name (not (or (satisfies symbol?)
                                   (= nil))) }]
   `(destructuring-fn nil ~@args)

   [[name] {}]
   (throw (IllegalArgumentException. 
           "destructuring-fn, invalid function body or clauses clauses"))

   [[name lambda-list & others] {lambda-list (satisfies vector?)}]
   `(destructuring-fn-many
     ~@(if name (list name) nil) ;; either name or nothing
     (~lambda-list
      ~@others))
   
   [[name & clauses] {clauses (and (satisfies list?)
                                   (not (= ()))
                                   (rte (:* (:cat (satisfies vector?) (:* :sigma)))))}]
   `(destructuring-fn-many
     ~@(if name (list name) nil) ;; either name or nothing
     ~@clauses)

   [[& others] {}]
   (throw (IllegalArgumentException. 
           (cl-format false
                      "destructuring-fn, invalid argument list: ~A"
                      args)))))
