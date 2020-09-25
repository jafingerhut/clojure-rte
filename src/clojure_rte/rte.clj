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

(ns clojure-rte.rte
  "This is an empty namespace to fool lein to load this file as part of
  the clojure-rte.core ns.")

(in-ns 'clojure-rte.core)

(declare traverse-pattern)
(declare canonicalize-pattern)

(def ^:dynamic *rte-known*
  "Dynamic variable whose value is a map.
  The map associates symbols with rte expansions.
  Any tag in this table may be used in place of a type name
  in an rte pattern."
  ;; TODO - is it interesting to allow parameterized types here?
  ;;    E.g., these types would work similar to CL deftype,
  ;;    we'd need a macro expander based on quasi-quote.
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
    (throw (ex-info (format "resolve-rte-tag: warning unknown type %s" tag)
                    {:error-type :unknown-type
                     :type tag }))
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


(defmulti registered-type? identity)
(defmethod registered-type? :default [_] false)
(defmethod registered-type? '= [_] true)
(defmethod registered-type? 'rte [_] true)
(defmethod registered-type? 'member [_] true)


(defn supported-nontrivial-types []
  "Which types are currently supported?  This list denotes the
  type names which appear as (something maybe-args), which are
  supported by RTE.  The goal is to support all those supported
  by typep, but that may not yet be the case."
  (clojure.set/difference (set (keys (methods registered-type?)))  #{:default}))

(defmulti rte-expand
  "macro-like facility for rte" (fn [pattern _functions] (first pattern)))

(defn invalid-pattern [pattern functions]
  (throw (ex-info (format "invalid pattern %s" pattern)
                  {:error-type :rte-expand-error
                   :keyword (first pattern)
                   :pattern pattern
                   :functions functions
                   })))

(defmethod rte-expand :default [pattern functions]
  (invalid-pattern pattern functions))

(defmethod rte-expand :? [pattern functions]
  (apply (fn
           ([] (invalid-pattern pattern functions))
           ([operand] `(:or :epsilon ~operand))
           ([_ & _] (invalid-pattern pattern functions)))
         (rest pattern)))

(defmethod rte-expand :+ [pattern functions]
  (apply (fn
           ([] (invalid-pattern pattern functions))
           ([operand] `(:cat ~operand (:* ~operand)))
           ([_ & _] (invalid-pattern pattern functions)))
         (rest pattern)))

(defmethod rte-expand :permute [pattern functions]
  (apply (fn
           ([] :epsilon)
           ([operand] operand)
           ([_ & _]
            (let [operands (for [operand (rest pattern)]
                             (traverse-pattern operand functions))]
              (cons :or (call-with-collector (fn [collect]
                                               (visit-permutations
                                                (fn [perm]
                                                  (collect (cons :cat perm))) operands)))))))
         (rest pattern)))

(defmethod rte-expand :contains-any [pattern functions]
  (apply (fn
           ([] :epsilon)
           ([operand] operand)
           ([_ & _]
            (let [operands (for [operand (rest pattern)]
                             (traverse-pattern operand functions))]
              `(:cat (:* :sigma)
                     (:or ~@operands)
                     (:* :sigma)))))
         (rest pattern)))

(defmethod rte-expand :contains-every [pattern functions]
  (apply (fn
           ([] :epsilon)
           ([operand] operand)
           ([_ & _]
            (let [wrapped (for [operand (rest pattern)
                                 :let [traversed (traverse-pattern operand functions)]]
                             `(:cat (:* :sigma) ~traversed (:* :sigma)))]
              `(:and ~@wrapped))))
         (rest pattern)))

(defmethod rte-expand :contains-none [pattern _functions]
  ;; TODO, not sure what (:contains-none) should mean with no arguments.
  ;;    as implemented it is equivalent to (:not :epsilon) which seems wierd.
  `(:not (:contains-any ~@(rest pattern))))

(defmethod rte-expand :exp [pattern functions]
  (letfn [(expand [n m pattern]
            (assert (>= n 0) (format "pattern %s is limited to n >= 0, not %s" pattern n))
            (assert (<= n m) (format "pattern %s is limited to n <= m, got %s > %s" pattern n m))
            (let [operand (traverse-pattern pattern functions)
                  repeated-operand (repeat n operand)
                  optional-operand (repeat (- m n) `(:? ~operand))
                  ]
              (traverse-pattern `(:cat ~@repeated-operand ~@optional-operand) functions)))]
    (apply (fn
             ([] (invalid-pattern pattern functions))
             ([_] (invalid-pattern pattern functions))
             ([n pattern]
              (expand n n pattern))
             ([n m pattern] 
              (expand n m pattern))
             ([_ _ _ & _] 
              (invalid-pattern pattern functions)))
           (rest pattern))))


(defn traverse-pattern
  "Workhorse function for walking an rte pattern.
   This function is the master of understanding the syntax of an rte
   pattern.  Any function which needs to perform a recursive operation
   such as derivative, nullable, first-types, or canonicalize-pattern
   may call traverse-pattern with an augmented map of
   *traversal-functions*, indicating the callbacks for each rte
   keyword such as :* :cat etc.  The philosophy is that no other
   function needs to understand how to walk an rte pattern."
  [pattern functions]

  (letfn [(if-atom []
            (case pattern
              (:epsilon :empty-set :sigma)
              ((functions pattern) pattern functions)
              ((:type functions) pattern functions)))
          (if-nil []
            ((:type functions) () functions))
          (convert-type-designator-to-rte [obj]
            ;; e.g convert (and a b c) => (:and a b c)
            ;;             (or a b c) => (:or a b c)
            ;;             (not a) => (:and :sigma (:not a))
            (if (not (sequential? obj))
              obj
              ;; TODO if and,or,not need to verify that it is a valid.
              ;;  i.e., (or (:and ...)) is not allowed.
              (case (first obj)
                (or) (cons :or (rest obj))
                (and) (cons :and (rest obj))
                (not) `(:and :sigma (:not ~(rest obj)))
                obj)))
          (if-singleton-list [] ;; (:or)  (:and)
            (let [pattern (convert-type-designator-to-rte pattern)
                  [keyword] pattern]
              (case keyword
                (:or)  (traverse-pattern :empty-set functions)
                (:and) (traverse-pattern :sigma functions)
                (:cat) (traverse-pattern :epsilon functions)
                (:not
                 :*) (throw (ex-info (format "invalid pattern %s, expecting exactly one operand" pattern)
                                       {:error-type :rte-syntax-error
                                        :keyword keyword
                                        :pattern pattern
                                        :functions functions
                                        :cause :unary-keyword
                                        }))
                ;; case-else
                (cond
                  (and (sequential? keyword)
                       (registered-type? (first keyword)))
                  ((:type functions) pattern functions)

                  :else
                  (traverse-pattern (rte-expand pattern functions) functions)))))
          (if-exactly-one-operand [] ;; (:or Long) (:* Long)
            (let [pattern (convert-type-designator-to-rte pattern)
                  [token operand] pattern]
              (case token
                (:or :and :cat)
                (traverse-pattern operand functions)
                
                (:not :*)
                ((functions token) operand functions)

                ;;case-else
                (if (registered-type? (first pattern))
                  ((:type functions) pattern functions)
                  (traverse-pattern (rte-expand pattern functions) functions)))))
          (if-multiple-operands []
            (let [pattern (convert-type-designator-to-rte pattern)
                  [token & operands] pattern]
              (case token
                (:or
                 :and
                 :cat)
                ((functions token) operands functions)

                (:not :*)
                (throw (ex-info (format "invalid pattern %s, expecting exactly one operand" pattern)
                                {:error-type :rte-syntax-error
                                 :keyword keyword
                                 :pattern pattern
                                 :functions functions
                                 :cause :unary-keyword
                                 }))

                ;;case-else
                (if (registered-type? token)
                  ((:type functions) pattern functions)
                  (traverse-pattern (rte-expand pattern functions) functions)))))]
    (cond (not (seq? pattern))
          (if-atom)

          (empty? pattern)
          (if-nil)

          (empty? (rest pattern)) ;; singleton list, (:and), (:or) etc
          (if-singleton-list)

          (empty? (rest (rest pattern))) ;; (:and x)
          (if-exactly-one-operand)

          ;; cond-else (:keyword args) or list-expr ;; (:and x y)
          :else (if-multiple-operands))))

(defn nullable 
  "Determine whether the given rational type expression is nullable.
  I.e., does the empty-word satisfy the expression."
  [expr]
  (boolean
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
                                   (not (nullable operand)))))))

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

                                              (ty/subtype? wrt type (constantly false))
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

(defn rte-combine-labels ""
  [label1 label2]
  (letfn [(or? [lab]
            (and (sequential? lab)
                 (= (first lab) 'or)))]      
    (cond
      (and (or? label1)
           (or? label2)) `(~@label1 ~@(rest label2))
      (and (or? label1)
           (not (or? label2))) `(~@label1 ~label2)
      (and (not (or? label1))
           (or? label2)) `(~(first label2) ~label1 ~@(rest label2))
      :else `(~'or ~label1 ~label2))))

(defn rte-to-dfa
  "Use the Brzozowski derivative aproach to compute a finite automaton
  representing the given rte patten.  The finite automaton is in the
  form of an array of States.  The n'th State is array[n]."
  ([pattern]
   (rte-to-dfa pattern true))
  ([pattern exit-value]

  (let [given-pattern pattern
        pattern (canonicalize-pattern pattern)
        [triples derivatives] (find-all-derivatives pattern)
        derivatives (cons pattern (remove #{pattern} derivatives))
        index-map (zipmap derivatives (range (count derivatives)))
        triples (map (fn [[primative wrt deriv]]
                       [(index-map primative) wrt (index-map deriv)]
                       ) triples)
        grouped (group-by (fn [trip]
                            (trip 0)) triples)]
    (dfa/map->Dfa
     {:pattern given-pattern
      :canonicalized pattern
      :exit-map (constantly exit-value)
      :combine-labels rte-combine-labels
      :states
      (into [] (map (fn [deriv index]
                      (let [transitions (if (and (grouped index)
                                                 (apply = (map (fn [[_src _wrt dst]]
                                                                 dst) (grouped index))))
                                          ;; if all transitions have same dst, then don't draw
                                          ;; multiple transitions, just draw with with label = :sigma
                                          (list [:sigma ((first (grouped index)) 2)])
                                          (map (fn [[_src wrt dst]]
                                                 [wrt dst]) (grouped index)))]
                        (dfa/map->State {:index index
                                     :initial (= 0 index)
                                     :accepting (nullable deriv)
                                     :pattern deriv
                                     :transitions transitions})))
                    derivatives (range (count derivatives))))}))))

