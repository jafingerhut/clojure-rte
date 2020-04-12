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
;; *  need a way to match a single object which itself is an rte type
;;    thus matching hierarchical structure
;;
;; *  since there is no (not ...) type in clojure, the (:not ) rte must
;;    be especially handled in the automaton construction.
;;
;; *  need a way to declare rte "variables".   and a way to distinguish
;;    a hierarchical rte, i.e., an element of a sequence which itself
;;    is a sequence matching an rte, vs simply a variable whose value
;;    is substituted into an rte expression.
;;
;; * fully implement (satisfies)

(ns clojure-rte.core
  (:require [clojure.set :refer [union]]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.cl-compat :refer [cl-cond cl-prog1 cl-prog2 cl-progn]]
            [clojure-rte.util :refer [with-first-match remove-once call-with-collector
                                      visit-permutations rte-constantly rte-identity
                                      sort-operands member]]
            [clojure-rte.type :refer [typep type-intersection disjoint?
                                      map-type-partitions type-min type-max]]
            )
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(declare traverse-pattern)
(declare canonicalize-pattern)

(def ^:dynamic *rte-known*
  "Dynamic variable whose value is a map.
  The map associates symbols with rte expansions.
  Any tag in this table may be used in place of a type name
  in an rte pattern."
  {'integer? '(:or Integer
                   Long
                   clojure.lang.BigInt
                   BigInteger
                   Short    
                   Byte)
   'int? '(:or Long Integer Short Byte)
   'rational? '(:or integer? ratio? decimal?)
   'ratio? 'clojure.lang.Ratio
   'string? 'String
   'number? 'Number
   'keyword? 'clojure.lang.Keyword
   'symbol? 'clojure.lang.Symbol
   'decimal? 'BigDecimal
   'float? '(:or Double Float)
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
   ((not (empty? (or (descendants tag)
                     (ancestors tag)))) tag)
   (:else
    (println (format "warning unknown type %s" tag))
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
                                       {:type :rte-syntax-error
                                        :keyword keyword
                                        :pattern pattern
                                        :functions functions
                                        :cause :unary-keyword
                                        }))
                ;; case-else
                ((:type functions) pattern functions))))
          (if-exactly-one-operand []
            (let [[token operand] pattern]
              (case token
                (:rte)
                (throw (ex-info (format "not yet implemented: derivative of %s" pattern)
                                {:type :rte-not-yet-implemented
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
                ((:type functions) pattern functions))))
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
                                {:type :rte-syntax-error
                                 :keyword keyword
                                 :pattern pattern
                                 :functions functions
                                 :cause :unary-keyword
                                 }))

                ;;case-else
                ((:type functions) pattern functions))))]
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
                           :type (fn [operand functions]
                                   ;; not not converts nil to false
                                   (not (not (some (fn [x]
                                                     (typep x operand))
                                                   [() []]))))
                           :* (rte-constantly true)
                           :cat (fn [operands functions]
                                  (every? nullable operands))
                           :and (fn [operands functions]
                                  (every? nullable operands))
                           :or (fn [operands functions]
                                 (some nullable operands))
                           :not (fn [operand functions]
                                  (not (nullable operand))))))

(defn first-types 
  "Return a possibly empty set of types (i.e., object which can be
  passed to isa?) which specify the possible set of first value values
  in any sequence satisfying this rational type expression."
  [expr]
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

(defn seq-matcher
  "Return a function, a closure, which can be used to determine whether
  its argument is a sequence whose first element is identically the
  given obj."
  [target]
  (fn [obj]
    (and (seq? obj)
         (not (empty? obj))
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
                           :type (fn [tag functions]
                                   (resolve-rte-tag tag))
                           :empty-set rte-identity
                           :epsilon rte-identity
                           :sigma rte-identity
                           :* (fn [operand functions]
                                (let [operand (canonicalize-pattern operand)]
                                  (case operand
                                    :epsilon    :epsilon ;; (:* :epsilon) --> :epsilon
                                    :empty-set  :epsilon ;; (:* :empty-set) --> :epsilon
                                    (if (*? operand)
                                      operand ;; (:* (:* something)) --> (:* something)
                                      (list :* (canonicalize-pattern operand))))))
                           :cat (fn [operands functions]
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
                           :not (fn [operand functions]
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
                           :and (fn [operands functions]
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
                                            max (type-max atoms)
                                            ]
                                        (when (some (fn [i1]
                                                      (some (fn [i2]
                                                              (and (not (= i1 i2))
                                                                   (disjoint? i1 i2))) atoms)) atoms)
                                          :empty-set)))
                                     
                                     ;; (:and subtype supertype x y z) --> (:and subtype x y z)
                                     ((let [atoms (filter (complement seq?) operands)
                                            max (type-max atoms)
                                            ]
                                        (when max
                                          (cons :and (remove #{max} operands)))))
                                     
                                     (:else
                                      (cons :and operands))

                                     )))
                           :or (fn [operands functions]
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
                                           min (type-min atoms)
                                           ]
                                       (when min
                                         (cons :or (remove #{min} operands)))))                                     

                                    (:else
                                     (cons :or operands))

                                    )
                                   )))))

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
  So co compute the derivative of B wrt (and A (not B)) we get :empty-set
  because the types are disjoint."
  [expr wrt]

  (assert (not (seq? expr)) (cl-format false "not expecting sequence expr= ~A" expr))
  (assert (seq? wrt) (cl-format false "expecting sequence, not ~A" wrt))
  (assert (= 'and (first wrt)))
  (cond
    (some #{`(~'not ~expr)} (rest wrt))
    :empty-set

    :else
    (throw (ex-info (format "not yet implemented: derivative of %s wrt %s"
                            expr wrt)
                    {:type :rte-not-yet-implemented
                     :pattern expr
                     :wrt wrt
                     }))))    

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
                                :sigma (fn [type functions]
                                         :epsilon)
                                :type (fn [type functions]
                                        (cond (and (seq? wrt)
                                                   (= 'and (first wrt)))
                                              (compute-compound-derivative type wrt)
                                              
                                              (disjoint? wrt type)
                                              :empty-set

                                              (isa? wrt type)
                                              :epsilon
                                              
                                              :else
                                              (do
                                                (throw (ex-info "providing smaller types"
                                                                {:type :split-type
                                                                 :sub-types [{:type `(~'and ~wrt ~expr)}
                                                                             {:type `(~'and ~wrt (~'not ~expr))}]
                                                                 })))
                                              ))
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
                          (disjoint? t1 t2))) type-set))]

    (let [independent (filter independent? type-set)
          dependent (remove (set independent) type-set)]
      (concat independent (call-with-collector
                           (fn [collect]
                             (map-type-partitions
                              (seq dependent)
                              (fn [left right]
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

                                  :else
                                  (let [right (map (fn [x]
                                                     (list 'not x)) right)]
                                    (collect `(~'and ~@left ~@right))))))))))))

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
            (let [disjoined (mdtd (conj (first-types pattern) :sigma))
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
                    {:index index
                     :accepting (nullable deriv)
                     :pattern deriv
                     :transitions (map (fn [[src wrt dst]]
                                         [wrt dst]) (grouped index))})
                  derivatives (range (count derivatives))))))

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


(defn rte-compile 
  "Compile an rte pattern into a finite automaton."
  [pattern]
  (rte-to-dfa pattern))

(defn rte-execute [dfa items]
  "Given a finite automaton generated by rte-to-dfa (or rte-compile), determine
   whether the given sequence, items, matches the regular type expression."
  (loop [items (seq items)
         state 0]

    (if (empty? items)
      (:accepting (dfa state))
      (let [[head & tail] items
            state-obj (dfa state)]
        (if-let [next-state (some (fn [[type next-state]]
                                    (if (typep head type)
                                      next-state
                                      false))
                                  (:transitions state-obj))]
          (recur tail next-state)
          false)))))

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
