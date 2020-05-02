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

;; TODO
;; *  since there is no (not ...) type in clojure, the (:not ) rte must
;;    be especially handled in the automaton construction.
;;
;; * fully implement (satisfies)
;;
;; * allow (= ...) type to be used in an RTE.
;;   E.g., (rte-match '(:* (:or (= true) (= false))) [true true false false false])
;;
;; * test multiple (rte ) types used together,   rte-trace of pattern containing (rte) type
;;
;; * optimize (rte ) types used together, with intersection and and-not operations.
;;
;; * there are many functions in clojure core such as
;;      (defn double?
;;        "Return true if x is a Double"
;;        {:added "1.9"}
;;        [x] (instance? Double x))
;;   I would like to parse these and build entries in *rte-known* programmatically
;; 

(ns clojure-rte.core
  (:require [clojure.set :refer [union]]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.cl-compat :refer [cl-cond]]
            [clojure-rte.util :refer [with-first-match call-with-collector
                                      visit-permutations rte-constantly rte-identity
                                      partition-by-pred
                                      sort-operands member]]
            [clojure-rte.type :as ty]
            )
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& _args]
  (println "Hello, World!"))

(declare traverse-pattern)
(declare canonicalize-pattern)

(def ^:dynamic *rte-known*
  "Dynamic variable whose value is a map.
  The map associates symbols with rte expansions.
  Any tag in this table may be used in place of a type name
  in an rte pattern."
  {'int? '(:or Long Integer Short Byte)
   'integer? '(:or int? clojure.lang.BigInt BigInteger)
   'ratio? 'clojure.lang.Ratio
   'decimal? 'BigDecimal
   'rational? '(:or integer? ratio? decimal?)
   'number? 'Number
   'float? '(:or Double Float)
   'real? '(:or rational? number? decimal? float?)
   'string? 'String
   'keyword? 'clojure.lang.Keyword
   'symbol? 'clojure.lang.Symbol
   'seq? 'clojure.lang.ISeq
   })
      
(defn call-with-rte
  "Call the given 0-ary function with 0 or more rte keys bound to rte patterns.
   with-rte is a macro API to this function.
   E.g.,
   (call-with-rte [::a '(:permute Long Long String)
                   ::b '(:permute Double Double String)]
     (fn []
      (rte-match '(:cat ::a ::b) [1 \"hello\" 2
                                  \"world\" 1.0 2.0])))"
  [bindings thunk]
  (binding [*rte-known* (apply assoc *rte-known* bindings)]
    (thunk)))
      
(defmacro with-rte
  "Evaluate the given body in a dynamic extend where 0 or more keys bound to
   un-quoted rte patterns.
   E.g.,
   (with-rte [::a (:permute Long Long String)
              ::b (:permute Double Double String)]
     (rte-match '(:cat ::a ::b) [1 \"hello\" 2
                                \"world\" 1.0 2.0])))"
  [bindings & body]
  `(call-with-rte '~bindings (fn [] ~@body)))

(defn resolve-rte-tag
  "Look up a tag in *rte-known*, or return the given tag
   if not found"
  [tag]

  (cl-cond   
   ((*rte-known* tag))
   ((and (symbol? tag)
         (resolve tag)
         (class? (resolve tag)))
    tag)
   ((not-empty (or (descendants tag)
                   (ancestors tag))) tag)
   ((ty/valid-type? tag) tag)
   (:else
    (println (format "resolve-rte-tag: warning unknown type %s" tag))
    tag))
)

(def ^:dynamic *traversal-functions*
  "Default callbacks for walking an rte tree.
  A function which wants to perform a recursive action on an
  rte pattern, must call traverse-pattern, passing 
  (assoc *traversal-functions*
         key value key value   ...)
  as second argument, thus overriding the callbacks
  when special nodes are encountered in the rte pattern.
  For example, when (:* ...) is encountered, the function
  (*traversal-functions* :*) is called with two arguments,
  1) the list of operands given to (:* ...), and 2) the value
  value of functions, i.e., the extended value of
  *traversal-functions*"
  {:client (fn [pattern functions]
             (traverse-pattern pattern functions))
   :type (fn [tag functions]
           ((:client functions) (resolve-rte-tag tag) functions))
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

(def supported-nontrivial-types
  "Which types are currently supported?  This list denotes the
  type names which appear as (something maybe-args), which are
  supported by RTE.  The goal is to support all those supported
  by typep, but that's not yet implemented."
 '(rte))

(defn traverse-pattern
  "Workhorse function for walking an rte pattern.
   This function is the master of understanding the syntax of an rte
   pattern.  Any function which needs to perform a recursive operation
   such as derivative, nullable, first-types, or canonicalize-pattern
   may call traverse-pattern with an augmented map of
   *traversal-functions*, indicating the callbacks for each rte
   keyword such as :* :+ :cat etc.  The philosophy is that no other
   function needs to understand how to walk an rte pattern."
  [pattern functions]

  (letfn [(if-atom []
            (case pattern
              (:epsilon :empty-set :sigma)
              ((functions pattern) pattern functions)
              ((:type functions) pattern functions)))
          (if-nil []
            ((:type functions) () functions))
          (if-singleton-list []
            (let [[keyword] pattern]
              (case keyword
                (:or)  (traverse-pattern :empty-set functions)
                (:and) (traverse-pattern :sigma functions)
                (:cat) (traverse-pattern :epsilon functions)
                (:permute) (traverse-pattern :epsilon functions)
                (:not
                 :*
                 :?
                 :+
                 :exp
                 :rte) (throw (ex-info (format "invalid pattern %s, expecting exactly one operand" pattern)
                                       {:error-type :rte-syntax-error
                                        :keyword keyword
                                        :pattern pattern
                                        :functions functions
                                        :cause :unary-keyword
                                        }))
                ;; case-else
                (if (some #{(first pattern)} supported-nontrivial-types)
                  ((:type functions) pattern functions)
                  (throw (ex-info (format "0-ary type %s not yet implemented" pattern)
                                  {:error-type :type-not-yet-implemented
                                   :pattern pattern
                                   :functions functions
                                   }))))))
          (if-exactly-one-operand []
            (let [[token operand] pattern]
              (case token
                (:rte)
                (throw (ex-info (format "not yet implemented: derivative of %s" pattern)
                                {:error-type :rte-not-yet-implemented
                                 :keyword keyword
                                 :pattern pattern
                                 :functions functions
                                 }))
                
                (:or :and :cat :permute)
                (traverse-pattern operand functions)
                
                (:not :*)
                ((functions token) operand functions)
                
                (:exp)
                (let [[n operand] operand
                      operand (traverse-pattern operand functions)
                      repeated-operand (map (fn [_]
                                              operand) (range n))]
                  (assert (>= n 0))
                  (traverse-pattern `(:cat ~@repeated-operand) functions))

                (:+)
                (traverse-pattern `(:cat ~operand
                                         (:* ~operand)) functions)
                
                (:?)
                (traverse-pattern `(:or :epsilon
                                        ~operand) functions)

                ;;case-else
                (if (some #{(first pattern)} supported-nontrivial-types)
                  ((:type functions) pattern functions)
                  (throw (ex-info (format "unary type %s not yet implemented" pattern)
                                  {:error-type :type-not-yet-implemented
                                   :pattern pattern
                                   :functions functions
                                   }))))))
          (if-multiple-operands []
            (let [[token & operands] pattern]
              (case token
                (:permute)
                (cons :or (call-with-collector (fn [collect]
                                                 (visit-permutations
                                                  (fn [perm]
                                                    (collect (cons :cat perm))) operands))))

                (:or
                 :and
                 :cat)
                ((functions token) operands functions)

                (:not :* :+ :? :rte :exp)
                (throw (ex-info (format "invalid pattern %s, expecting exactly one operand" pattern)
                                {:error-type :rte-syntax-error
                                 :keyword keyword
                                 :pattern pattern
                                 :functions functions
                                 :cause :unary-keyword
                                 }))

                ;;case-else
                (if (some #{(first pattern)} supported-nontrivial-types)
                  ((:type functions) pattern functions)
                  (throw (ex-info (format "variadic type %s not yet implemented" pattern)
                                  {:error-type :type-not-yet-implemented
                                   :pattern pattern
                                   :functions functions
                                   }))))))]
    (cond (not (seq? pattern))
          (if-atom)

          (empty? pattern)
          (if-nil)

          (empty? (rest pattern)) ;; singleton list, (:and), (:or) etc
          (if-singleton-list)

          (empty? (rest (rest pattern))) ;; (:and x) (:+ x)
          (if-exactly-one-operand)

          ;; cond-else (:keyword args) or list-expr ;; (:and x y) (:+ x y)
          :else (if-multiple-operands))))

(defn nullable 
  "Determine whether the given rational type expression is nullable.
  I.e., does the empty-word satisfy the expression."
  [expr]
  (traverse-pattern expr
                    (assoc *traversal-functions*
                           :empty-set (rte-constantly false)
                           :epsilon (rte-constantly true)
                           :sigma   (rte-constantly false)
                           :type (rte-constantly false)
                           :* (rte-constantly true)
                           :cat (fn [operands _functions]
                                  (every? nullable operands))
                           :and (fn [operands _functions]
                                  (every? nullable operands))
                           :or (fn [operands _functions]
                                 (some nullable operands))
                           :not (fn [operand _functions]
                                  (not (nullable operand))))))

(defn first-types 
  "Return a possibly empty set of types (i.e., object which can be
  passed to isa?) which specify the possible set of first value values
  in any sequence satisfying this rational type expression."
  [expr]
  (letfn [(mr [operands _functions]
            (reduce (fn [acc next]
                      (union acc (first-types next))) #{} operands))]
  (traverse-pattern expr
                    (assoc *traversal-functions*
                           :epsilon (rte-constantly #{})
                           :empty-set (rte-constantly #{})
                           :sigma (rte-constantly #{:sigma})
                           :type (fn [operand _functions]
                                   #{operand})
                           :or mr
                           :and mr
                           :not (fn [operand _functions]
                                  (first-types operand))
                           :cat (fn [[head & tail] _functions]
                                  (cond (nullable head)
                                        (union (first-types head)
                                               (first-types (cons :cat tail)))

                                        :else
                                        (first-types head)))
                           :* (fn [operand _functions]
                                (first-types operand))))))

(defn seq-matcher
  "Return a function, a closure, which can be used to determine whether
  its argument is a sequence whose first element is identically the
  given obj."
  [target]
  (fn [obj]
    (and (seq? obj)
         (not-empty obj)
         (= target (first obj)))))

(def cat? 
  "Predicate determining whether its object is of the form (:cat ...)"
  (seq-matcher :cat))
(def *?
  "Predicate determining whether its object is of the form (:* ...)"
  (seq-matcher :*))
(def not? 
  "Predicate determining whether its object is of the form (:not ...)"
  (seq-matcher :not))
(def and?
  "Predicate determining whether its object is of the form (:and ...)"
  (seq-matcher :and))
(def or? 
  "Predicate determining whether its object is of the form (:or ...)"
  (seq-matcher :or))

(defn canonicalize-pattern-once 
  "Rewrite the given rte patter to a canonical form.
  This involves recursive re-writing steps for each sub form,
  including searches for syntatical and semantical reductions.
  The API for canonicalizing a pattern is canonicalize-pattern,
  which finds a fixed-point of canonicalize-pattern-once, i.e.,
  keeps calling canonicalize-pattern-once until it finally
  stops changing."
  [re]
  (traverse-pattern re
                    (assoc *traversal-functions*
                           :type (fn [tag _functions]
                                   (resolve-rte-tag tag))
                           :empty-set rte-identity
                           :epsilon rte-identity
                           :sigma rte-identity
                           :* (fn [operand _functions]
                                (let [operand (canonicalize-pattern operand)]
                                  (case operand
                                    :epsilon    :epsilon ;; (:* :epsilon) --> :epsilon
                                    :empty-set  :epsilon ;; (:* :empty-set) --> :epsilon
                                    (if (*? operand)
                                      operand ;; (:* (:* something)) --> (:* something)
                                      (list :* (canonicalize-pattern operand))))))
                           :cat (fn [operands _functions]
                                  (let [operands (map canonicalize-pattern operands)]
                                    (assert (< 1 (count operands))
                                            (format "traverse-pattern should have already eliminated this case: re=%s count=%s operands=%s" re (count operands) operands))
                                    (cl-cond
                                      ;; (:cat x (:cat a b) y) --> (:cat x a b y)
                                      ((some cat? operands)
                                      (cons :cat (mapcat (fn [obj]
                                                           (if (cat? obj)
                                                             (rest obj)
                                                             (list obj))) operands)))

                                      ;; (:cat x "empty-set" y) --> :emptyset
                                      ((member :empty-set operands)
                                      :empty-set)

                                      ;; (:cat x :epsilon y) --> (:cat x y)
                                      ((member :epsilon operands)
                                      (cons :cat (remove #{:epsilon} operands)))

                                      ;; (:cat x (:* :sigma) (:* :sigma) y) --> (:cat x y)
                                      ((let [[head tail] (split-with (complement #{'(:* :sigma)}) operands)]
                                         ;; tail the first tail starting with  (:* :sigma)
                                         (if (and tail
                                                  (rest tail)
                                                  (= '(:* :sigma) (first (rest tail))))
                                          (cons :cat (concat head '((:* :sigma)) (drop-while #{'(:* :sigma)} tail)))
                                          false)))

                                      (:else
                                       (cons :cat operands)))))
                           :not (fn [operand _functions]
                                  (let [operand (canonicalize-pattern operand)]
                                    (case operand
                                      (:sigma) :epsilon
                                      ((:* :sigma)) :empty-set
                                      (:epsilon) (canonicalize-pattern '(:+ :sigma))
                                      (:empty-set) '(:* :sigma)
                                      (cond
                                        (not? operand) ;; (:not (:not A)) --> A
                                        (second operand)

                                        (and? operand) ;;  (:not (:and A B)) --> (:or (:not A) (:not B))
                                        (cons :or (map (fn [obj]
                                                         (list :not obj)) (rest operand)))

                                        (or? operand) ;;   (:not (:or A B)) --> (:and (:not A) (:not B))
                                        (cons :and (map (fn [obj]
                                                          (list :not obj)) (rest operand)))

                                        :else
                                        ;; TODO in CL this expands to
                                        ;; (:or :empty-word
                                        ;;      (not pattern) ;; not type does not exist in clojure
                                        ;;      (:cat t (:+ t)))
                                        ;; so we need to take care of this when when build the automaton
                                        (list :not operand))
                                      )))
                           :and (fn [operands _functions]
                                  (assert (< 1 (count operands))
                                          (format "traverse-pattern should have already eliminated this case: re=%s count=%s operands=%s" re (count operands) operands))
                                  (let [operands (dedupe (sort-operands (map canonicalize-pattern operands)))]
                                    (cl-cond
                                     ((some and? operands)
                                      (cons :and (mapcat (fn [obj]
                                                           (if (and? obj)
                                                             (rest obj)
                                                             (list obj))) operands)))

                                     ((member :empty-set operands)
                                      :empty-set)

                                     ((member '(:* :sigma) operands)
                                      (cons :and (remove (fn [obj]
                                                           (= '(:* :sigma) obj)) operands)))

                                     ((some or? operands)
                                      ;; (:and (:or A B) C D) --> (:or (:and A C D) (:and B C D))
                                      (with-first-match or? operands
                                        (fn [or-item]
                                          (let [others (remove (fn [x] (= or-item x)) operands)]
                                            (cons :or (map (fn [x] (list* :and x others)) (rest or-item)))))))

                                     ;; (:and x (:not x)) --> :empty-set
                                     ((let [nots (filter not? operands)
                                            others (remove not? operands)]
                                        (when (some (fn [item]
                                                      (some #{(list :not item)} nots)) others)
                                          :empty-set)))

                                     ;; (:and of disjoint types) --> :empty-set
                                     ((let [atoms (filter (complement seq?) operands)
                                            ]
                                        (when (some (fn [i1]
                                                      (some (fn [i2]
                                                              (and (not (= i1 i2))
                                                                   (ty/disjoint? i1 i2))) atoms)) atoms)
                                          :empty-set)))
                                     
                                     ;; (:and subtype supertype x y z) --> (:and subtype x y z)
                                     ((let [atoms (filter (complement seq?) operands)
                                            max (ty/type-max atoms)
                                            ]
                                        (when max
                                          (cons :and (remove #{max} operands)))))
                                     
                                     (:else
                                      (cons :and operands))

                                     )))
                           :or (fn [operands _functions]
                                 (assert (< 1 (count operands))
                                         (format "traverse-pattern should have already eliminated this case: re=%s count=%s operands=%s" re (count operands) operands))
                                 (let [operands (dedupe (sort-operands (map canonicalize-pattern operands)))]
                                   (cl-cond
                                    ((some or? operands)
                                     (cons :or (mapcat (fn [obj]
                                                         (if (or? obj)
                                                           (rest obj)
                                                           (list obj))) operands)))

                                    ((member '(:* :sigma) operands)
                                     '(:* :sigma))

                                    ((member :empty-set operands)
                                     (cons :or (remove #{:empty-set} operands)))

                                    ;; (:or x (:not x)) --> :sigma
                                    ((let [nots (filter not? operands)
                                           others (remove not? operands)]
                                       (when (some (fn [item]
                                                     (some #{(list :not item)} nots)) others)
                                         :sigma)))

                                    ;; (:or subtype supertype x y z) --> (:and supertype x y z)
                                    ((let [atoms (filter (complement seq?) operands)
                                           min (ty/type-min atoms)
                                           ]
                                       (when min
                                         (cons :or (remove #{min} operands)))))

                                    (:else
                                     (cons :or operands))
                                    ))))))

(defn canonicalize-pattern 
  "find the fixed point of canonicalize-pattern-once"
  [pattern]

  (loop [old-pattern []
         new-pattern pattern]
    (if (= old-pattern new-pattern)
      old-pattern
      (recur new-pattern (canonicalize-pattern-once new-pattern)))))

(defn compute-compound-derivative
  "wrt may be a compound type designator such as (and A (not B)).
  So to compute the derivative of B wrt (and A (not B)) we get :empty-set
  because the types are disjoint."
  [expr wrt]

  ;;(assert (not (sequential? expr)) (cl-format false "not expecting sequence expr= ~A:" expr))
  (assert (sequential? wrt) (cl-format false "expecting sequence, not ~A:" wrt))
  (assert (= 'and (first wrt)))
  (let [[_ & and-args] wrt]
    (cond
      (some #{`(~'not ~expr)} and-args)
      :empty-set

      (some #{expr} and-args)
      :epsilon

      :else
      (throw (ex-info (format "not yet implemented: derivative of %s wrt %s"
                              expr wrt)
                      {:error-type :rte-not-yet-implemented
                       :pattern expr
                       :wrt wrt
                       })))))

(defn derivative 
  "Compute the Brzozowski rational expression derivative of the given
  rte pattern with respect to the given type wrt."
  [expr wrt]
  (letfn [(walk [patterns]
            (map (fn [p]
                   (derivative (canonicalize-pattern p) wrt))
                 patterns))]
    (canonicalize-pattern
     (cond
       (= :empty-set expr)
       :empty-set
       
       (= :epsilon wrt)
       expr ;; deriv of anything with respect to :epsilon is that thing.

       (= wrt expr)
       :epsilon

       :else
       (traverse-pattern expr
                         (assoc *traversal-functions*
                                :epsilon (rte-constantly :empty-set)
                                :empty-set (rte-constantly :empty-set)     
                                :sigma (fn [_type _functions]
                                         :epsilon)
                                :type (fn [type _functions]
                                        (cond 
                                              
                                              (ty/disjoint? wrt type)
                                              :empty-set

                                              (ty/subtype? wrt type ty/subtype?-false)
                                              :epsilon
                                              
                                              (and (sequential? wrt)
                                                   (= 'and (first wrt)))
                                              (compute-compound-derivative type wrt)

                                              :else
                                              (throw (ex-info (format "cannot compute derivative of overlapping types because %s is not a subtype of %s" wrt type)
                                                              {:error-type :derivative-undefined
                                                               :wrt wrt
                                                               :expr expr
                                                               :sub-types [{:type `(~'and ~wrt ~expr)}
                                                                           {:type `(~'and ~wrt (~'not ~expr))}]
                                                               }))
                                              ))
                                :or (fn [operands _functions]
                                      (cons :or (walk operands)))
                                :and (fn [operands _functions]
                                       (cons :and (walk operands)))
                                :not (fn [operand _functions]
                                       (cons :not (walk (list operand))))
                                :cat (fn [[head & tail] _functions]
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
                                :* (fn [operand _functions]
                                     `(:cat ~(derivative operand wrt) (:* ~operand)))))))))

(defn mdtd 
  "Given a set of type designators, return a newly computed list of type
  designators which implement the Maximal Disjoint Type Decomposition.
  I.e., the computed list designates a set whose union is the same as
  the given set, but all the elements are mutually disjoint."
  [type-set]
  ;; find a disjoint type
  (letfn [(independent? [t1]
            (every? (fn [t2]
                      (or (= t1 t2)
                          (ty/disjoint? t1 t2))) type-set))
          (rte? [t]
            (and (sequential? t)
                 (= 'rte (first t))))
          (count-if [pred items]
            (reduce (fn [acc item]
                      (if (pred item)
                        (inc acc)
                        acc)) 0 items))
          (collect-left-right [collect left right]
            (cond
              (and (empty? right)
                   (= 1 (count left)))
              (collect (first left))

              (and (empty? left)
                   (= 1 (count right)))
              (collect (list 'not (first right)))

              (and (empty? right)
                   (empty? left))
              :sigma

              (> (+ (count-if rte? left)
                    (count-if rte? right)) 1)
              (let [[left-rtes left] (partition-by-pred rte? left)
                    [right-rtes right] (partition-by-pred rte? right)
                    left-patterns (map second left-rtes)
                    right-patterns (map second right-rtes)]
                (cond (empty? left-rtes)
                      (let [new-rte (canonicalize-pattern `(:or ~@right-patterns))]
                        (collect-left-right collect
                                            left
                                            (cons (list 'rte new-rte) right)))

                      (empty? right-rtes)
                      (let [new-rte (canonicalize-pattern `(:and ~@left-patterns))]
                        (collect-left-right collect
                                            (cons (list 'rte new-rte) left)
                                            right))

                      :else
                      (let [new-rte (canonicalize-pattern
                                     `(:and ~@left-patterns
                                            (:not (:or ~@right-patterns))))]
                        (collect-left-right collect
                                            (cons (list 'rte new-rte) left)
                                            right))))

              :else
              (let [right (map (fn [x]
                                 (list 'not x)) right)]
                (collect `(~'and ~@left ~@right)))))]

    (let [independent (filter independent? type-set)
          dependent (remove (set independent) type-set)]
      (concat independent (call-with-collector
                           (fn [collect]
                             (ty/map-type-partitions
                              (seq dependent)
                              (fn [left right]
                                (collect-left-right collect left right)))))))))

(defn find-all-derivatives 
  "Start with the given rte pattern, and compute its derivative with
  respect to all the values returned by first-types.  Continue
  computing the derivatives of each of the derivatives returned with
  respect to all of their first-types.  Continue this process until no
  more derivatives can be found.  Warning, the given pattern might not
  be an element of the return value.  I.e., the 0'th derivative is not
  guaranteed to be among the values returned."
  [pattern]
  (loop [to-do-patterns (list pattern)
         done #{}
         triples [] 
         ]
    (if (empty? to-do-patterns)
      [ triples (seq done)]
      (let [[pattern & to-do-patterns] to-do-patterns]
        (if (done pattern)
          (recur to-do-patterns done triples)
          (letfn [(xx [[acc-triples acc-derivs] wrt-type]
                    (let [triple [pattern wrt-type (derivative pattern wrt-type)]]
                      [(conj acc-triples triple)
                       (if (done (triple 2))
                         acc-derivs
                         (conj acc-derivs (triple 2)))]
                      )
                    )]
            (let [firsts (first-types pattern)
                  disjoined (mdtd (conj firsts :sigma))
                  [new-triples new-derivatives] (reduce xx [[] ()] disjoined)]
              (recur (concat new-derivatives to-do-patterns)
                     (conj done pattern)
                     (concat triples new-triples)))))))))

(defn rte-to-dfa 
  "Use the Brzozowski derivative aproach to compute a finite automaton
  representing the given rte patten.  The finite automaton is in the
  form of an array of states.  The n'th state is array[n].  Each state
  is a map with the keys:

  :index -- the index of this state in the array
  :accepting - Boolean true/false indicating whether this state is a
      final/accepting state.
  :pattern -- the derivative value representing an rte pattern matching
      any tail of the input sequence which is accepting from this point
      onward.
  :transitions -- A list of pairs, each pair is a 2 element array of the form
      [type next-state], e.g., [clojure.lang.Keyword 1]
      which means if the value at the head of the sequence is of type
      clojure.lang.Keyword, then go to state 1.  The type is some value
      compatible with isa?.  the state index is some index of the state
      array representing the finite atomaton."
  [pattern]

  (let [pattern (canonicalize-pattern pattern)
        [triples derivatives] (find-all-derivatives pattern)
        derivatives (cons pattern (remove #{pattern} derivatives))
        index-map (zipmap derivatives (range (count derivatives)))
        triples (map (fn [[primative wrt deriv]]
                       [(index-map primative) wrt (index-map deriv)]
                       ) triples)
        grouped (group-by (fn [trip]
                            (trip 0)) triples)]
    (into [] (map (fn [deriv index]
                    (let [transitions (if (and (grouped index)
                                               (apply = (map (fn [[_src _wrt dst]]
                                                               dst) (grouped index))))
                                        ;; if all transitions have same dst, then don't draw
                                        ;; multiple transitions, just draw with with label = :sigma
                                        (list [:sigma ((first (grouped index)) 2)])
                                        (map (fn [[_src wrt dst]]
                                               [wrt dst]) (grouped index)))]
                      {:index index
                       :initial (= 0 index)
                       :accepting (nullable deriv)
                       :pattern deriv
                       :sync-state (and (some #{[:sigma index]} transitions) true)
                       :transitions transitions}))
                  derivatives (range (count derivatives))))))

(def rte-compile
  "Compile an rte pattern into a finite automaton."
  (memoize rte-to-dfa))

(defn rte-trace
  "Given a compiled rte, find a sequence of types which satisfy the corresponding pattern."
  [rte]
  (letfn [(recurring [state path lineage]
            (cond
              (:accepting (rte state)) path
              (some #{state} lineage) false
              :else (some (fn [[type dst-state]]
                            (recurring dst-state (conj path type) (conj lineage state)))
                          (:transitions (rte state))))
            )]
    (recurring 0 [] ())))

(defn rte-inhabited? [dfa]
  (some :accepting dfa))

(defn rte-vacuous? [dfa]
  (not (rte-inhabited? dfa)))

(defn rte-execute 
  "Given a finite automaton generated by rte-to-dfa (or rte-compile), determine
   whether the given sequence, items, matches the regular type expression."
  [dfa items]
  (letfn [(consume [state-index item]
            (let [state-obj (dfa state-index)]
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
        (:accepting (dfa final-state))))))

(defn rte-match 
  "Given a rte pattern, not a finite automaton, determine whether the
  given sequence, items, matches the regular type expression.  If the
  caller wishes to check more than one sequence against the same
  pattern, it is better to call rte-compile, to get an automaton, and
  use that same automaton in several calls to rte-execute.
  rte-execute has time complexity O(n) where n is the length of the
  sequence, while rte-compile has worst case exponential complexity.
  rte-match contains a call to both rte-compile and rte-execute."
  [pattern items]
  (rte-execute (rte-compile pattern) items))

(defmethod ty/typep 'rte [a-value [_a-type pattern]]
  (and (sequential? a-value)
       (rte-match pattern a-value)))

(defmethod ty/valid-type? 'rte [[_ pattern]]
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
  
  (ty/new-disjoint-hook
   :rte
   (fn [t1 t2]
     (cond (and (rte? t1)
                (rte? t2))
           (let [[_ pat1] t1
                 [_ pat2] t2]
             (rte-vacuous? (rte-compile `(:and ~pat1 ~pat2))))

           (and (rte? t1)
                (not? t2)
                (rte? (second t2)))
           (let [[_ pat1] t1
                 [_ [_ pat2]] t2]
             (rte-vacuous? (rte-compile `(:and ~pat1 (:not ~pat2)))))

           (and (rte? t1)
                (ty/class-designator? t2)
                (isa? (resolve t2) java.lang.CharSequence))
           (let [[_ pat1] t1]
             (rte-vacuous? (rte-compile `(:and ~pat1 (:* java.lang.Character)))))

           (and (rte? t1)
                (ty/class-designator? t2)
                (not (isa? (resolve t2) clojure.lang.Sequential)))
           true

           (and (not? t1)
                (rte? (second t1))
                (ty/class-designator? t2)
                (not (isa? (resolve t2) clojure.lang.Sequential)))
           false

           (and (rte? t1)
                (not? t2)
                (ty/class-designator? (second t2))
                (not (isa? (resolve (second t2)) clojure.lang.Sequential)))
           false

           :else :dont-know)))

  (ty/new-subtype-hook
   :rte
   (fn [sub-designator super-designator]
     (cond (and (rte? sub-designator)
                (rte? super-designator))
           (let [[_ pat-sub] sub-designator
                 [_ pat-super] super-designator]
             (rte-vacuous? (rte-compile `(:and ~pat-sub (:not ~pat-super)))))

           (and (rte? super-designator)
                (ty/class-designator? sub-designator)
                (isa? (resolve sub-designator) java.lang.CharSequence))
           (ty/subtype? '(rte (:* java.lang.Character)) super-designator)

           (and (rte? sub-designator)
                (ty/class-designator? super-designator)
                (isa? (resolve super-designator) java.lang.CharSequence))
           (ty/subtype? sub-designator '(rte (:* java.lang.Character)))

           (and (rte? super-designator)
                (ty/class-designator? sub-designator)
                (not (isa? (resolve sub-designator) clojure.lang.Sequential)))
           false

           (and (rte? sub-designator)
                (ty/class-designator? super-designator)
                (not (isa? (resolve super-designator) clojure.lang.Sequential)))
           false
           
           (and (rte? super-designator)
                (and? sub-designator)
                (some (fn [and-operand]
                        (rte? and-operand)
                        (ty/subtype? and-operand super-designator
                                     ty/subtype?-false)) (rest sub-designator)))
           true

           :else :dont-know))))
