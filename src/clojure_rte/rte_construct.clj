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

(ns clojure-rte.rte-construct
  "This is an empty namespace to fool lein to load this file as part of
  the clojure-rte.rte-core ns.")

(in-ns 'clojure-rte.genus)
(declare rte?)

(in-ns 'clojure-rte.rte-core)


(def heavy-logging (atom false))
(def andy-attempted-fix (atom false))
(def call-count-canonicalize-pattern-once (atom 0))
(def call-count-canonicalize-pattern (atom 0))
(def call-count-traverse-pattern (atom 0))
(def traverse-pattern-call-count-by-depth (atom {}))
(def traverse-pattern-call-stack (atom []))
(def arg-freq-canonicalize-pattern-once (atom {}))

(defn assoc-inc [m k]
  (assoc m k (inc (get m k 0))))

(defn print-current-call-stack []
  (let [my-exc (try
                 (throw (ex-info "only for creating stack trace" {}))
                 (catch Throwable e
                   e))]
    (clojure.repl/pst my-exc 100)
    (. *err* (flush))))

(defn print-pattern-abbrev [pat]
  (binding [*print-length* 50]
    (let [sz (if (sequential? pat) (count pat) 0)]
      (print (if (zero? sz)
               ""
               (str "(seq len " sz ")"))
             pat))))

(defn print-most-freq-first [freqs]
  (let [call-counts (for [[v cnt] freqs]
                      {:count cnt, :value v,
                       :value-size (if (sequential? v) (count v) 0)})
        call-counts (sort-by (fn [m]
                               [(- (:count m)) (- (:value-size m))])
                             call-counts)]
    (doseq [m call-counts]
      (print "  " (:count m) ":")
      (print-pattern-abbrev (:value m))
      (println))))

(defn reset-stats! []
  (reset-util-stats!)
  (reset! call-count-canonicalize-pattern-once 0)
  (reset! call-count-canonicalize-pattern 0)
  (reset! call-count-traverse-pattern 0)
  (reset! arg-freq-canonicalize-pattern-once {})
  (reset! call-count-traverse-pattern 0)
  (reset! traverse-pattern-call-count-by-depth {}))

(defn get-stats []
  {:call-count-canonicalize-pattern-once
   call-count-canonicalize-pattern-once
   :call-count-canonicalize-pattern
   call-count-canonicalize-pattern
   :call-count-traverse-pattern
   call-count-traverse-pattern
   :arg-freq-canonicalize-pattern-once
   arg-freq-canonicalize-pattern-once})

(defn print-stats [pattern]
  (let [c1 @call-count-canonicalize-pattern-once
        c2 @call-count-canonicalize-pattern
        c3 @call-count-traverse-pattern
        args @arg-freq-canonicalize-pattern-once
        ]
    (println "cp calls=" c2 "  cpo calls=" c1 " tp calls=" c3 " pattern" pattern)
    (println "    cpo arg frequencies:")
    (print-most-freq-first args)
    (println "    --------------------")
    (flush)
    ;;(print-current-call-stack)
    ))

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
  {
   'real? 'Number
   })

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
           ((:client functions) tag functions))
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
(defmethod registered-type? :default
  [type-designator]
  (cond
    (not (sequential? type-designator))
    false
    (empty? type-designator)
    false
    :else
    (registered-type? (first type-designator))))
(defmethod registered-type? '= [_] true)
(defmethod registered-type? 'rte [_] true)
(defmethod registered-type? 'member [_] true)
(defmethod registered-type? 'satisfies [_] true)

(defn supported-nontrivial-types []
  "Which types are currently supported?  This list denotes the
  type names which appear as (something maybe-args), which are
  supported by RTE.  The goal is to support all those supported
  by typep, but that may not yet be the case."
  (clojure.set/difference (set (keys (methods registered-type?)))  #{:default}))

(defmulti rte-expand
  "macro-like facility for rte" (fn [pattern _functions] (first pattern)))

(defn invalid-pattern [pattern functions culprit]
  (throw (ex-info (format "[134] invalid pattern %s" pattern)
                  {:error-type :rte-expand-error
                   :keyword (first pattern)
                   :culprit culprit
                   :pattern pattern
                   :functions functions
                   })))

(defmethod rte-expand :default [pattern functions]
  (invalid-pattern pattern functions :default))

(defmethod rte-expand :? [pattern functions]
  (apply (fn
           ([] (invalid-pattern pattern functions '[:? []]))
           ([operand] `(:or :epsilon ~operand))
           ([_ & _] (invalid-pattern pattern functions '[:? [_ & _]]))) 
         (rest pattern)))

(defmethod rte-expand :+ [pattern functions]
  (apply (fn
           ([] (invalid-pattern pattern functions '[:+ []]))
           ([operand] `(:cat ~operand (:* ~operand)))
           ([_ & _] (invalid-pattern pattern functions '[:+ [_ & _]])))
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
             ([] (invalid-pattern pattern functions '[:exp []]))
             ([_] (invalid-pattern pattern functions '[:exp [_]]))
             ([n pattern]
              (expand n n pattern))
             ([n m pattern] 
              (expand n m pattern))
             ([_ _ _ & _] 
              (invalid-pattern pattern functions '[:exp [_ _ _ & _]])))
           (rest pattern))))

(declare traverse-pattern-impl)

(defn traverse-pattern
  [given-pattern functions]
  (swap! call-count-traverse-pattern inc)
  (swap! traverse-pattern-call-stack conj given-pattern)
  (let [depth (count @traverse-pattern-call-stack)]
    (swap! traverse-pattern-call-count-by-depth assoc-inc depth))
  (when @heavy-logging
    (when-not (or (keyword? given-pattern)
                  (and (sequential? given-pattern)
                       (= 2 (count given-pattern))
                       (= :* (first given-pattern))
                       (= :sigma (second given-pattern))))
      (print "traverse-pattern")
      (doseq [[depth pat] (reverse
                           (map-indexed (fn [depth-1 pat] [(inc depth-1) pat])
                                        @traverse-pattern-call-stack))]
        (print (str " (depth " depth ", " (get @traverse-pattern-call-count-by-depth depth) " calls): "))
        (print-pattern-abbrev pat)
        (println))
      (flush)
      (print-current-call-stack)))
  (let [ret (traverse-pattern-impl given-pattern functions)]
    (swap! traverse-pattern-call-stack pop)
    (when (and @heavy-logging
               (not= ret given-pattern))
      (println "traverse-pattern returns different pattern:")
      (print "   given-pattern:")
      (pr given-pattern)
      (println)
      (print "   non-= ret val:")
      (pr ret)
      (println))
    ret))

(defn traverse-pattern-impl
  "Workhorse function for walking an rte pattern.
   This function is the master of understanding the syntax of an rte
   pattern.  Any function which needs to perform a recursive operation
   such as derivative, nullable, first-types, or canonicalize-pattern
   may call traverse-pattern with an augmented map of
   *traversal-functions*, indicating the callbacks for each rte
   keyword such as :* :cat etc.  The philosophy is that no other
   function needs to understand how to walk an rte pattern."
  [given-pattern functions]
  (letfn [(if-atom [pattern]
            (cond
              (member pattern '(:epsilon :empty-set :sigma))
              ((functions pattern) pattern functions)

              (*rte-known* pattern)
              (traverse-pattern (*rte-known* pattern) functions)

              :else
              ((:type functions) pattern functions)))
          (if-nil [_]
            ((:type functions) () functions))
          (verify-type [obj]
            (if (gns/valid-type? obj)
              obj
              (throw (ex-info (cl-format false "[219] invalid type designator ~A" obj)
                              {:error-type :invalid-type-designator
                               :obj obj
                               :given-pattern given-pattern}))))
          (convert-type-designator-to-rte [obj]
            ;; e.g convert (and a b c) => (:and a b c)
            ;;             (or a b c) => (:or a b c)
            ;;             (not a) => (:and :sigma (:not a))
            ;; We also verify that it is a valid.
            ;;  i.e., (or (:and ...)) is not allowed, which probably means the user forgot a :
            (if (not (sequential? obj))
              obj
              (case (first obj)
                (or) (cons :or (rest (verify-type obj)))
                (and) (cons :and (rest (verify-type obj)))
                (not) `(:and (:not ~@(rest (verify-type obj))) :sigma)
                obj)))
          (if-singleton-list [pattern] ;; (:or)  (:and)
            (let [[keyword] pattern]
              (case keyword
                (:or)  (traverse-pattern :empty-set functions)
                (:and) (traverse-pattern '(:* :sigma) functions)
                (:cat) (traverse-pattern :epsilon functions)
                (:not
                 :*) (throw (ex-info (format "[264] invalid pattern %s, expecting exactly one operand" pattern)
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
          (if-exactly-one-operand [pattern] ;; (:or Long) (:* Long)
            (let [[token operand] pattern]
              (case token
                (:or :and :cat)
                (traverse-pattern operand functions)
                
                (:not :*)
                ((functions token) operand functions)

                (satisfies)
                (if (not= pattern (gns/expand-satisfies pattern))
                  (traverse-pattern (gns/expand-satisfies pattern) functions)
                  ((:type functions) pattern functions))
                
                ;;case-else
                (if (registered-type? (first pattern))
                  ((:type functions) pattern functions)
                  (traverse-pattern (rte-expand pattern functions) functions)))))
          (if-multiple-operands [pattern]
            (let [[token & operands] pattern]
              (case token
                (:or
                 :and
                 :cat)
                ((functions token) operands functions)

                (:not :*)
                (throw (ex-info (format "[301] invalid pattern %s, expecting exactly one operand" pattern)
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
    (let [pattern (convert-type-designator-to-rte given-pattern)]
      (cond (not (seq? pattern))
            (if-atom pattern)

            (empty? pattern)
            (if-nil pattern)

            (empty? (rest pattern)) ;; singleton list, (:and), (:or) etc
            (if-singleton-list pattern)

            (empty? (rest (rest pattern))) ;; (:and x)
            (if-exactly-one-operand pattern)

            ;; cond-else (:keyword args) or list-expr ;; (:and x y)
            :else (if-multiple-operands pattern)))))

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

(defmethod gns/canonicalize-type 'rte
  [type-designator]
  (cons 'rte (map canonicalize-pattern (rest type-designator))))

(defn -canonicalize-pattern-once 
  "Rewrite the given rte patter to a canonical form.
  This involves recursive re-writing steps for each sub form,
  including searches for syntatical and semantical reductions.
  The API for canonicalizing a pattern is canonicalize-pattern,
  which finds a fixed-point of canonicalize-pattern-once, i.e.,
  keeps calling canonicalize-pattern-once until it finally
  stops changing."
  [re]
  (swap! call-count-canonicalize-pattern-once inc)
  (swap! arg-freq-canonicalize-pattern-once #(assoc % re (inc (get % re 0))))
  ;;(println "can-once:" re)
  ;;(flush)
  ;;(assert false)
  (traverse-pattern re
                    (assoc *traversal-functions*
                           :type (fn [tag _functions]
                                   (gns/canonicalize-type tag))
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
                                  (let [orig-operands operands
                                        operands (doall (map canonicalize-pattern operands))]
                                    (when @heavy-logging
                                      (print "(:cat traversal-function) on"
                                             (count operands) "operands:")
                                      (if (= operands orig-operands)
                                        (println " no-change")
                                        (let [old-new (map-indexed
                                                       vector
                                                       (map vector orig-operands
                                                            operands))
                                              changed (remove (fn [[idx [before after]]]
                                                                (= before after))
                                                              old-new)
                                              num-changed (count changed)]
                                          (println " " num-changed " changed")
                                          (doseq [[idx [before after]]
                                                  (take 20 changed)]
                                            (print "  # " idx ": before: ")
                                            (pr before)
                                            (if (= before after)
                                              (print " after-no-change:")
                                              (do
                                                (print " after-different: ")
                                                (pr after)))
                                            (println)))))
                                    (assert (< 1 (count operands))
                                            (format "traverse-pattern should have already eliminated this case: re=%s count=%s operands=%s" re (count operands) operands))
                                    (cl/cl-cond
                                     ;; (:cat A (:* X) (:* X) B)
                                     ;;  --> (:cat A (:* X) B)
                                     ((let [equal-and-*? (fn [a b]
                                                           (and (= a b)
                                                                (*? a)))
                                            ptr (first-repeat operands equal-and-*?)]
                                        (if (empty? ptr)
                                          false
                                          (if @andy-attempted-fix
                                            (cons :cat (dedupe-by-f equal-and-*?
                                                                    operands))
;;                                            (cons :cat (remove-second-of-first-pair-satisfying
;;                                                        equal-and-*?
;;                                                        operands))
                                            (let [prefix (cl/ldiff operands ptr)]
                                              (cons :cat (doall (concat prefix (rest ptr)))))
))))

                                     ;; (:cat x (:cat a b) y) --> (:cat x a b y)
                                     ((some cat? operands)
                                      (cons :cat
                                            (doall (mapcat (fn [obj]
                                                             (if (cat? obj)
                                                               (rest obj)
                                                               (list obj)))
                                                           operands))))

                                     ;; (:cat x "empty-set" y) --> :emptyset
                                     ((member :empty-set operands)
                                      :empty-set)

                                     ;; (:cat x :epsilon y) --> (:cat x y)
                                     ((member :epsilon operands)
                                      (cons :cat (doall (remove #{:epsilon} operands))))

                                     (:else
                                      (cons :cat operands)))))
                           :not (fn [operand _functions]
                                  (let [operand (canonicalize-pattern operand)]
                                    (case operand
                                      (:sigma) '(:or (:cat :sigma :sigma (:* :sigma)) :epsilon)
                                      ((:* :sigma)) :empty-set
                                      (:epsilon) '(:cat :sigma (:* :sigma))
                                      (:empty-set) '(:* :sigma)
                                      (cond
                                        (not? operand) ;; (:not (:not A)) --> A
                                        (second operand)

                                        (and? operand) ;;  (:not (:and A B)) --> (:or (:not A) (:not B))
                                        (cons :or (doall (map (fn [obj]
                                                         (list :not obj)) (rest operand))))

                                        (or? operand) ;;   (:not (:or A B)) --> (:and (:not A) (:not B))
                                        (cons :and (doall (map (fn [obj]
                                                          (list :not obj)) (rest operand))))

                                        :else
                                        ;; TODO in CL this expands to
                                        ;; (:or :empty-word
                                        ;;      (not pattern) ;; not type does not exist in clojure
                                        ;;      (:cat t (:+ t)))
                                        ;; so we need to take care of this when when build the automaton
                                        (list :not operand))
                                      )))
                           :and (fn [operands _functions]
                                  (let [operands (dedupe (sort-operands (doall (map canonicalize-pattern operands))))]
                                    (cl/cl-cond
                                     ;; TODO - (:and :epsilon ...)
                                     ;;    if any of the :and arguments is not nullable,
                                     ;;    then the result is :empty-set
                                     ;;    otherwise the result is :epsilon

                                     ;; TODO (:and (:cat A B (:* :sigma))
                                     ;;            (:cat A B ))
                                     ;;  --> (:and (:cat A B))


                                     ((some and? operands)
                                      (cons :and (doall (mapcat (fn [obj]
                                                           (if (and? obj)
                                                             (rest obj)
                                                             (list obj))) operands))))

                                     ((member :empty-set operands)
                                      :empty-set)

                                     ((member '(:* :sigma) operands)
                                      (cons :and (doall (remove (fn [obj]
                                                           (= '(:* :sigma) obj)) operands))))

                                     ((some or? operands)
                                      ;; (:and (:or A B) C D) --> (:or (:and A C D) (:and B C D))
                                      (with-first-match or? operands
                                        (fn [or-item]
                                          (let [others (doall (remove (fn [x] (= or-item x)) operands))]
                                            (cons :or (doall (map (fn [x] (list* :and x others)) (rest or-item))))))))

                                     ;; (:and x (:not x)) --> :empty-set
                                     ((let [nots (doall (filter not? operands))
                                            others (doall (remove not? operands))]
                                        (when (some (fn [item]
                                                      (some #{(list :not item)} nots)) others)
                                          :empty-set)))

                                     ;; (:and of disjoint types) --> :empty-set
                                     ((let [atoms (doall (filter (complement seq?) operands))
                                            ]
                                        (when (some (fn [i1]
                                                      (some (fn [i2]
                                                              (and (not (= i1 i2))
                                                                   (gns/disjoint? i1 i2))) atoms)) atoms)
                                          :empty-set)))
                                     
                                     ;; (:and subtype supertype x y z) --> (:and subtype x y z)
                                     ((let [atoms (doall (filter (complement seq?) operands))
                                            max (gns/type-max atoms)
                                            ]
                                        (when max
                                          (cons :and (doall (remove #{max} operands))))))
                                     
                                     (:else
                                      (cons :and operands))

                                     )))
                           :or (fn [operands _functions]
                                 (assert (< 1 (count operands))
                                         (format "traverse-pattern should have already eliminated this case: re=%s count=%s operands=%s" re (count operands) operands))
                                 (let [operands (dedupe (sort-operands (doall (map canonicalize-pattern operands))))]
                                   (cl/cl-cond
                                    ;; TODO (:or (:cat A B (:* :sigma))
                                    ;;           (:cat A B ))
                                    ;;  --> (:or (:cat A B (:* :sigma)))
                                    
                                    ;; (:or A :epsilon B (:cat X (:* X)) C)
                                    ;;   --> (:or A :epsilon B (:* X) C ) --> (:or A B (:* X) C)
                                    ((and (member :epsilon operands)
                                          (some (fn [obj]
                                                  (and (cat? obj)
                                                       (= 3 (count obj))
                                                       (let [[_ x y] obj]
                                                         (cond (and (*? x)
                                                                    (= y (second x)))
                                                               ;; (:or x A B C)
                                                               (cons :or (cons x (doall (remove (fn [o] (or (= o :epsilon)
                                                                                                     (= o obj))) operands))))

                                                               (and (*? y)
                                                                    (= x (second y)))
                                                               ;; (:or y A B C)
                                                               (cons :or (cons y (doall (remove (fn [o] (or (= o :epsilon)
                                                                                                     (= o obj))) operands))))
                                                               
                                                               :else
                                                               false))))
                                                operands)))

                                    ((some or? operands)
                                     (cons :or (doall (mapcat (fn [obj]
                                                         (if (or? obj)
                                                           (rest obj)
                                                           (list obj))) operands))))

                                    ((member '(:* :sigma) operands)
                                     '(:* :sigma))

                                    ((member :empty-set operands)
                                     (cons :or (doall (remove #{:empty-set} operands))))

                                    ;; (:or x (:not x)) --> :sigma
                                    ((let [nots (doall (filter not? operands))
                                           others (doall (remove not? operands))]
                                       (when (some (fn [item]
                                                     (some #{(list :not item)} nots)) others)
                                         '(:* :sigma))))

                                    ;; (:or subtype supertype x y z) --> (:and supertype x y z)
                                    ((let [atoms (doall (filter (complement seq?) operands))
                                           min (gns/type-min atoms)
                                           ]
                                       (when min
                                         (cons :or (doall (remove #{min} operands))))))

                                    (:else
                                     (cons :or operands))
                                    ))))))

(def ^:dynamic canonicalize-pattern-once 
  ;;(memoize
   -canonicalize-pattern-once
  ;; )
  )

(defn log-worthy-pattern? [pattern]
  (and (not= pattern :sigma)
       (not= pattern :epsilon)
       (not= pattern '(:* :sigma))
       ))

(defn canonicalize-pattern 
  "find the fixed point of canonicalize-pattern-once"
  [pattern]
  (swap! call-count-canonicalize-pattern inc)
  (let [c1 @call-count-canonicalize-pattern-once
        do-log? (and @heavy-logging
                     (log-worthy-pattern? pattern))]
    (when do-log?
      ;;(and (not (zero? c1)) (zero? (mod c1 100000)))
      (print-stats pattern))
    (fixed-point pattern canonicalize-pattern-once = do-log?)))

(def patterns-to-do-heavy-logging
  #{
    '(:cat (:not (:? (member a b c "a" "b" "c"))) (:cat (:not (:cat :empty-set)) (:? (:cat (:and))) (:and (:or (:cat)) (:+ (:cat)))) (:+ (:or (:* (:and)) (:? :sigma))) :epsilon)
    })

(def last-pattern (atom nil))

(defn canonicalize-pattern-top [pattern]
  (reset! last-pattern pattern)
  (let [do-heavy-logging? (contains? patterns-to-do-heavy-logging pattern)]
    (when do-heavy-logging?
      (println "Enabling heavy logging...")
      (reset! heavy-logging true))
    (let [ret (canonicalize-pattern pattern)]
      (when do-heavy-logging?
        (reset! heavy-logging false))
      ret)))

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
                                              (gns/disjoint? wrt type)
                                              :empty-set

                                              (gns/subtype? wrt type (constantly false))
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
                          (gns/disjoint? t1 t2))) type-set))
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

              (> (+ (count-if gns/rte? left)
                    (count-if gns/rte? right)) 1)
              (let [[left-rtes left] (partition-by-pred gns/rte? left)
                    [right-rtes right] (partition-by-pred gns/rte? right)
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
                             (gns/map-type-partitions
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
  (cond
    (and (gns/or? label1)
         (gns/or? label2)) `(~@label1 ~@(rest label2))
    (and (gns/or? label1)
         (not (gns/or? label2))) `(~@label1 ~label2)
    (and (not (gns/or? label1))
         (gns/or? label2)) `(~(first label2) ~label1 ~@(rest label2))
    :else `(~'or ~label1 ~label2)))

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
    (dfa/extend-with-sink-state
     (dfa/map->Dfa
      {:pattern given-pattern
       :canonicalized pattern
       :exit-map (constantly exit-value)
       :combine-labels rte-combine-labels
       :states
       (into {}
             (map (fn [deriv index]
                    (let [transitions (if (and (grouped index)
                                               (apply = (map (fn [[_src _wrt dst]]
                                                               dst) (grouped index))))
                                        ;; if all transitions have same dst, then don't draw
                                        ;; multiple transitions, just draw with with label = :sigma
                                        (list [:sigma ((first (grouped index)) 2)])
                                        (map (fn [[_src wrt dst]]
                                               [wrt dst]) (grouped index)))]
                      [index
                       (dfa/map->State {:index index
                                        :initial (= 0 index)
                                        :accepting (nullable deriv)
                                        :pattern deriv
                                        :transitions transitions})]))
                  derivatives (range (count derivatives))))})))))

(defn dfa-to-rte
  "Accepts an object of type Dfa, and returns a map which associates
  exit values of the dfa with canonicalized rte patterns of the accepting
  langauge.  If there are no accepting states in the Dfa, an empty map {}
  is returned."
  [dfa]
  (assert (instance? (dfa/record-name) dfa) (cl-format false "dfa-to-rte: expecting Dfa, not ~A ~A" (type dfa) dfa))
  (into {} (for [[exit-value label] (dfa/extract-rte dfa)]
             [exit-value (canonicalize-pattern label)])))
