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
  But in the final clause the rte is optional.  In the case of missing
  rte in the final clause, the rte, :sigma, is assumed, analagous
  to the final/default clause of the  clojure.core/cond.
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
    
    (let [[fns int-rte-pairs] (compile-clauses clauses)
          num-fns (count fns)
          var (gensym "index")]
      `((~fns (ensure-fns-index (rte-match (memoized-rte-case-helper '~int-rte-pairs) ~sequence)
                                ~num-fns))))))

(defn lambda-list-to-rte
  "Helper function for destructuring-case macro."
  [lambda-list types-map]
  (assert (map? types-map))
  (letfn [(pretty-and [a b]
            (let [args (list a b)]
              (cond
                (empty? args) :sigma
                (empty? (rest args)) (first args)
                :else (cons 'and args))))]
    (let [[prefix _ suffix] (partition-by (fn [x] (= x '&)) lambda-list)
          prefix-rte (for [var prefix]
                       (cond (and (sequential? var)
                                  (empty? var))
                             nil

                             (sequential? var)
                             (list 'rte (lambda-list-to-rte var types-map))
                             
                             (symbol? var)
                             (pretty-and (get (meta var) :tag :sigma)
                                         (get types-map var :sigma))
                             
                             :else
                             (throw (ex-info (cl-format false "invalid lambda-list ~A" lambda-list)
                                             {:error-type "cannot parse prefix"}))))
          suffix-rte (cond
                       (empty? suffix)
                       nil

                       (empty? (rest suffix))
                       (lambda-list-to-rte suffix types-map)
                       
                       :else
                       (throw (ex-info (cl-format false "invalid lambda-list ~A" lambda-list)
                                       {:error-type "cannot parse suffix"})))]
      (if (not (empty? suffix))
        `(:cat ~@prefix-rte (:* ~suffix-rte))
        `(:cat ~@prefix-rte)))))

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
      (letfn [(expand-multi-restructions [types-map]
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
                [(lambda-list-to-rte lambda-list (expand-multi-restructions types-map))
                 `(let [~lambda-list ~var]
                    ~consequence)])]
        (let [pairs (partition 2 pairs)
              cases (mapcat conv-1-case-clause pairs)]
          `(let [~var ~expr]
             (rte-case ~var ~@cases (:* :sigma) nil)))))))

(defmacro destructuring-fn-many
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
  (cond (empty? args)
        (throw (IllegalArgumentException. 
                    "destructuring-fn, empty argument list not supported"))

        (and (not (symbol? (first args)))
             (not (= nil (first args))))
        `(destructuring-fn nil ~@args)

        :else
        (let [[name & clauses] args]
          (cond
            (empty? clauses)
            nil

            (vector? (first clauses))
            ;; either first clause is a vector like [a b c]
            `(destructuring-fn-many
              ~@(if name (list name) nil) ;; either name or nothing
              ~clauses ;; i.e., with a set of parens around it.
              )

            (every? (fn [clause]
                      (and (list? clause)
                           (not (empty? clause))
                           (vector? (first clause))))
                    clauses)
            ;; or all the clauses are lists whose first element is always a vector
            `(destructuring-fn-many
              ~@(if name (list name) nil) ;; either name or nothing
              ~@clauses)

            :else
            (throw (IllegalArgumentException. 
                    (cl-format false
                               "destructuring-fn, invalid argument list: ~A"
                               args)))))))


        
          
        
  
