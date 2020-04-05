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


(ns clojure-rte.core
  (:require [clojure.set :refer [union intersection]]
            [clojure.pprint :refer [cl-format]]
            )
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(declare traverse-pattern)
(declare canonicalize-pattern)

(def ^:dynamic *rte-hash* {'integer? '(:or Integer
                                          Long
                                          clojure.lang.BigInt
                                          BigInteger
                                          Short
                                          Byte)
                           'int? '(:or Long Integer Short Byte)
                           })

(defn resolve-rte-tag [tag]
  (cl-cond
   ((*rte-hash* tag))
   (:else
    tag)))

(def ^:dynamic *traversal-functions*
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

(defn with-first-match [pred items continuation]
  (loop [items items]
    (cond (empty? items)
          nil

          (pred (first items))
          (continuation (first items))

          :else
          (recur (rest items)))))

(defn remove-once [target items]
  (loop [items items
         acc ()]
    (cond
      (empty? items)
      (reverse acc)

      (= (first items) target)
      (concat (reverse acc) (rest items))

      :else
      (recur (rest items) (cons (first items) acc)))))

(defn call-with-collector [unary-client]
  "This function calls your given function which an argument which can be
   called to collect values.  The return value of call-with-collector is
   the list of items collected, in reverse order."
  (with-local-vars [data '()]
    (unary-client (fn [obj]
                    (var-set data (cons obj @data))))
    @data))

(defn visit-permutations [unary-client items]
  (letfn [(visit-with-tail [remaining tail]
            (if (empty? remaining)
              (unary-client tail)
              (doseq [item remaining]
                (visit-with-tail (remove-once item remaining)
                                 (cons item tail)))))]
    (visit-with-tail items '())))

(defn traverse-pattern [pattern functions]
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
                 :rte) (throw (ex-info (format "invalid pattern %s, expecting exactly one operand" pattern)
                                       {:type :rte-syntax-error
                                        :keyword keyword
                                        :pattern pattern
                                        :functions functions
                                        :cause :unary-keyword
                                        }))
                ;; case-else
                ((:type functions) pattern functions))))
          (if-at-least-one-operand []
            (let [[token & operands] pattern]
              (case token
                (:rte) (do (assert (= 1 (count operands))
                                   (format "invalid pattern %s" pattern))
                           (let [[name] operands]
                             (assert (contains? *rte-hash* name)
                                     (format "invalid rte name %s in pattern %s"
                                             name pattern))
                             (traverse-pattern (*rte-hash* name) functions)))

                (:permute)
                (cons :or (call-with-collector (fn [collect]
                                                 (visit-permutations
                                                  (fn [perm]
                                                    (collect (cons :cat perm))) operands))))

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
    (cond (not (seq? pattern))
          (if-atom)

          (empty? pattern)
          (if-nil)

          (empty? (rest pattern)) ;; singleton list
          (if-singleton-list)

          ;; cond-else (:keyword args) or list-expr
          :else (if-at-least-one-operand))))

(defn rte-constantly [x]
  (fn [_ _]
    x))

(defn rte-identity [x y]
  x)

(defn typep [a-value a-type]
  (isa? (type a-value) a-type))

(defn type-intersection [t1 t2]
  (intersection (conj (descendants t1) t1)
                (conj (descendants t2) t2)))

(defn disjoint? [t1 t2]
  (and (not (isa? t1 t2))
       (not (isa? t2 t1))
       (let [descendants-1 (descendants t1)
             descendants-2 (descendants t2)]
         (and (not-any? (fn [a2] (contains? descendants-1 a2)) descendants-2)
              (not-any? (fn [a1] (contains? descendants-2 a1)) descendants-1)))))

(defn nullable [expr]
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

(defn seq-matcher [target]
  (fn [obj]
    (and (seq? obj)
         (= target (first obj)))))

(def cat? (seq-matcher :cat))
(def *? (seq-matcher :*))
(def not? (seq-matcher :not))
(def and? (seq-matcher :and))
(def or? (seq-matcher :or))

(defn sort-operands [operands]
  (letfn [(cmp [a b]
            (cond
              (= a b)       0
              (= a ())      1
              (= b ())     -1

              (not (= (type a) (type b)))
              (compare (.getName (type a))
                       (.getName (type b)))

              (and (seq? a)
                   (seq? b))
              (loop [a a
                     b b]
                (cond
                  (= a b)   0
                  (= a ())   1
                  (= b ())  -1
                  (= (first a) (first b))   (recur (rest a) (rest b))

                  :else     (cmp (first a) (first b))))

              (seq? a)        1
              (seq? b)       -1

              :else
              (compare a b)))]
    (sort cmp  operands)))

(defn member [obj items]
  (some #{obj} items))

(defmacro cl-cond [[if1 then1] & others]
  ;; implementation from
  ;; https://stackoverflow.com/questions/4128993/consolidated-cond-arguments-in-clojure-cl-style
  (when (or if1 then1 others)
    (let [extra-clauses# (if others `(cl-cond ~@others))]
      (if then1
        `(if ~if1 ~then1 ~extra-clauses#)
        `(or ~if1 ~extra-clauses#)))))

;; find an element of the given sequence which is a subtype
;; of some other type and is not =.  not necessarily the global minimum.
(defn type-min [atoms]
  (some (fn [sub]
          (some (fn [super]
                  (and (not (= sub super))
                       (isa? sub super)
                       sub)) atoms)) atoms))

;; find an element of the given sequence which is a supertype
;; of some other type and is not =.  not necessarily the global maximum
(defn type-max [atoms]
  (some (fn [sub]
          (some (fn [super]
                  (and (not (= sub super))
                       (isa? sub super)
                       super)) atoms)) atoms))

(defn canonicalize-pattern-once [re]
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
                                    (cond
                                      ;; (:cat x (:cat a b) y) --> (:cat x a b y)
                                      (some cat? operands)
                                      (cons :cat (mapcat (fn [obj]
                                                           (if (cat? obj)
                                                             (rest obj)
                                                             (list obj))) operands))

                                      ;; (:cat x "empty-set" y) --> :emptyset
                                      (member :empty-set operands)
                                      :empty-set

                                      ;; (:cat x :epeilon y) --> (:cat x y)
                                      (member :epsilon operands)
                                      (cons :cat (remove #{:epsilon} operands))

                                      :else
                                      (cons :cat operands))))
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

(defn canonicalize-pattern [pattern]
  ;; find the fixed point of canonicalize-pattern-once
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
                              :sigma (fn [type functions]
                                       (cond (= wrt :sigma)
                                             :epsilon         

                                             (= wrt :epsilon)    
                                             :empty-set
                                             
                                             :else
                                             (throw (ex-info (format "cannot compute derivative of :sigma wrt %s because they intersecting" wrt)
                                                            {:type :derivative-error
                                                             :derivative {:expr expr
                                                                          :type type
                                                                          :wrt wrt
                                                                          :functions functions}
                                                             :cause :intersecting-types
                                                             }))))
                              :type (fn [type functions]
                                      (cond (= wrt type)
                                            :epsilon

                                            (disjoint? wrt type)
                                            :empty-set

                                            (isa? wrt type)
                                            :epsilon
                                            
                                            :else
                                            (throw (ex-info (format "cannot compute derivative of %s wrt %s because the types are intersecting at %s" type wrt (type-intersection type wrt))
                                                            {:type :derivative-error
                                                             :derivative {:expr expr
                                                                          :type type
                                                                          :wrt wrt
                                                                          :functions functions
                                                                          :intersection (type-intersection type wrt)}
                                                             :cause :intersecting-types
                                                             
                                                             }))))
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
                                   `(:cat ~(derivative operand wrt) (:* ~operand))))))))

(defn find-all-derivatives [pattern]
  (loop [to-do-patterns (list pattern)
         done #{}
         triples [] 
         ]
    (if (empty? to-do-patterns)
      [ triples (seq done)]
      (let [pattern (first to-do-patterns)
            to-do-patterns (rest to-do-patterns)]
        (if (done pattern)
          (recur to-do-patterns done triples)
          (let [[new-triples new-derivatives]
                (reduce (fn [[acc-triples acc-derivs] wrt-type]
                          (let [deriv (derivative pattern wrt-type)]
                            [(conj acc-triples [pattern wrt-type deriv])
                             (if (done deriv)
                               acc-derivs
                               (cons deriv acc-derivs))]
                            ))
                        [[] ()] (first-types pattern))]
            (recur (concat new-derivatives to-do-patterns)
                   (conj done pattern)
                   (concat triples new-triples))))))))

(defn rte-to-dfa [pattern]
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

(defn rte-compile [pattern]
  (rte-to-dfa pattern))

(defn rte-execute [dfa items]
  (loop [items (seq items)
         state 0]

    (if (empty? items)
      (:accepting (dfa state))
      (let [[head & tail] items
            state-obj (dfa state)]
        (if-let [next-state (some (fn [[type next-state]]
                                    (let [type (if (and (symbol? type)
                                                        (resolve type)
                                                        (class? (resolve type)))
                                                 (resolve type)
                                                 type)]
                                      (if (typep head type)
                                        next-state
                                        false)))
                                  (:transitions state-obj))]
          (recur tail next-state)
          false)))))

(defn rte-match [pattern items]
  (rte-execute (rte-compile pattern) items))

(defn simplify [unary error-case gen-components]
  (try (do (unary error-case)
           error-case)
       (catch Exception e
         (do
           (cl-format true "e=~A~%" e)
           (or (some (fn [component]
                       (simplify unary component)) (gen-components error-case))
               error-case)))))

(defn random-test [num-tries unary-test-fun arg-generator gen-components]
  (loop [num-tries num-tries]
    (if (< 0 num-tries)
      (let [data (arg-generator)]
        (cl-format true "~d: trying ~A~%" num-tries data)
        (unary-test-fun data)
        (recur (dec num-tries))))))

(defn gen-rte [size types]
  (let [key (rand-nth [:type
                   :? :+ :* :not
                   :and :or 
                   :cat :permute
                   :sigma :empty-set :epsilon])] 
    (case key
      (:type) (rand-nth types)
      (:sigma :empty-set :epsilon) key
      (:and :or :cat :permute) (cons key (map (fn [k] (gen-rte (dec size) types))
                                                  (range size)))
      (:? :+ :* :not) (list key (gen-rte (dec size) types)))))

(defn rte-components [pattern]
  (cond
    (and (seq? pattern)
         (empty pattern))
    ()

    (seq? pattern)
    (let [[keyword & operands] pattern]
      (case keyword
        (:* :+ :? :not
            :and :or :cat :permute) operands
        ;; case else
        ()))

    :else
    ()))

(defn test-rte-to-dfa [num-tries size]
  (random-test num-tries rte-to-dfa
               (fn [] (gen-rte size '(::Fox ::Wolf ::Cat ::Lion ::Cat-Lion)))
               rte-components))

(defn test-canonicalize-pattern [num-tries size]
  (random-test num-tries canonicalize-pattern
               (fn [] (gen-rte size '(::Fox ::Wolf ::Cat ::Lion ::Cat-Lion)))
               rte-components))
