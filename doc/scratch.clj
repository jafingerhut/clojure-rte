;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL input for repl-session1.txt when OutOfMemoryException occurred

(require 'clojure-rte.rte-tester)
(require '[clojure-rte.rte-core :as co])
(def rng-seed 5)
(reset! co/heavy-logging true)
(reset! co/andy-attempted-fix true)
(clojure-rte.rte-tester/-main rng-seed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL input for repl-session2.txt when no OutOfMemoryException
;; occurred when calling canonicalize-pattern on the same input that
;; caused the OOM during repl-session1.txt, but did not when that was
;; the only parameter ever given to canonicalize-pattern.

(require 'clojure-rte.rte-tester)
(require '[clojure-rte.rte-core :as co])

(reset! co/heavy-logging true)

(def problem-pattern '(:cat (:not (:? (member a b c "a" "b" "c"))) (:cat (:not (:cat :empty-set)) (:? (:cat (:and))) (:and (:or (:cat)) (:+ (:cat)))) (:+ (:or (:* (:and)) (:? :sigma))) :epsilon))
(pr problem-pattern)
(co/reset-stats!)
(co/canonicalize-pattern-top problem-pattern)

(def problem-pattern '(:or (:and (:and (:or (:cat)) :epsilon) :empty-set :empty-set) (:or :epsilon (:not (:* :epsilon)) (:and (:+ :epsilon) (:cat (:+ (:and))))) :epsilon :sigma))

----------------------------------------------------------------------
seed5-run3-pat148-only,txt line 5878 and a few after that
This file is output of first group of forms above
----------------------------------------------------------------------
traverse-pattern returns different pattern:
   given-pattern:(:cat (:not (:? (member a b c "a" "b" "c"))) (:cat (:not (:cat :empty-set)) (:? (:cat (:and))) (:and (:or (:cat)) (:+ (:cat)))) (:+ (:or (:* (:and)) (:? :sigma))) :epsilon)
   non-= ret val:(:cat (:and (:cat :sigma (:* :sigma)) (:not (= a))) (:* :sigma) (:* :sigma) :epsilon (:* :sigma) :epsilon)
fixed-point # 1  loop= 0 good-enough= false new-value= (:cat (:and (:cat :sigma (:* :sigma)) (:not (= a))) (:* :sigma) (:* :sigma) :epsilon (:* :sigma) :epsilon)
traverse-pattern (depth 1, 2 calls): (seq len 7) (:cat (:and (:cat :sigma (:* :sigma)) (:not (= a))) (:* :sigma) (:* :sigma) :epsilon (:* :sigma) :epsilon)
----------------------------------------------------------------------
given-pattern:
(:cat (:not (:? (member a b c "a" "b" "c")))
      (:cat (:not (:cat :empty-set))
            (:? (:cat (:and)))
            (:and (:or (:cat)) (:+ (:cat))))
      (:+ (:or (:* (:and)) (:? :sigma)))
      :epsilon)

The given-pattern above should cause the last branch of the cond
expression in traverse-pattern-impl to be run, which calls the locally
defined function if-multiple-operands

if-multiple-operands should then evaluate the expression ((functions token) operands functions)

non-= ret val:
(:cat (:and (:cat :sigma (:* :sigma))
            (:not (= a)))
      (:* :sigma)
      (:* :sigma)
      :epsilon
      (:* :sigma)
      :epsilon)

----------------------------------------------------------------------
seed5-pat148-only1,txt line 5510 and a few after that
This file is output of second group of forms above
----------------------------------------------------------------------
traverse-pattern returns different pattern:
   given-pattern:(:cat (:not (:? (member a b c "a" "b" "c"))) (:cat (:not (:cat :empty-set)) (:? (:cat (:and))) (:and (:or (:cat)) (:+ (:cat)))) (:+ (:or (:* (:and)) (:? :sigma))) :epsilon)
   non-= ret val:(:cat (:and (:cat :sigma (:* :sigma)) (:not (= a))) (:* :sigma) :epsilon)
fixed-point # 1  loop= 0 good-enough= false new-value= (:cat (:and (:cat :sigma (:* :sigma)) (:not (= a))) (:* :sigma) :epsilon)
traverse-pattern (depth 1, 2 calls): (seq len 4) (:cat (:and (:cat :sigma (:* :sigma)) (:not (= a))) (:* :sigma) :epsilon)
----------------------------------------------------------------------
non-= ret val:
(:cat (:and (:cat :sigma (:* :sigma))
            (:not (= a)))
      (:* :sigma)
      :epsilon)
----------------------------------------------------------------------



(def maybe-troublesome-pattern '(:cat :epsilon (:not (:and :empty-set (:cat :epsilon))) (:or clojure.lang.IMeta :empty-set (:* (:or (:and)))) (:and :sigma (:+ :sigma) (:cat (:+ (:cat)) :sigma))))

(def prev-stats (atom nil))
(def same-stats (atom 0))

(defn repeat-same-call [pattern n]
  (reset! prev-stats nil)
  (reset! same-stats 0)
  (dotimes [i n]
    (co/reset-stats!)
    (doall (co/canonicalize-pattern pattern))
    (let [s (co/get-stats)]
      (if (= @prev-stats s)
        (swap! same-stats inc)
        (do
          (println "i=" i " stats different than previous:")
          (co/print-stats pattern)
          (reset! prev-stats s)))))
  (println "same stats count=" @same-stats))

(repeat-same-call maybe-troublesome-pattern 500)

;; Call tree for function canonicalize-pattern

canonicalize-pattern (file rte_construct.clj) directly calls:
  (fixed-point parameter value is pattern,
               parameter f is canonicalize-pattern-once
               parameter good-enough is clojure.core/=)

fixed-point (file util.clj) directly calls:
  f is canonicalize-pattern-once
  good-enough is clojure.core/=
  no calls to functions returning lazy values, unless f or good-enough do that

canonicalize-pattern-once is a dynamic var with initial value
-canonicalize-pattern-once, and as far as I can tell it seems it is
never bound to any other value.

-canonicalize-pattern-once (file rte_construct.clj) directly calls:
  clojure.core/assoc
  traverse-pattern

  This function reads the value of a dynamic var
  *traversal-functions*, which is a map whose keys are keywords, and
  whose values are functions.  -canonicalize-pattern-once assoc's many
  new key/value pairs into this map, and passes that map to
  traverse-pattern.

  *traversal-functions* has these keys in its default/root binding,
  which I have sorted using Emacs here, differently than the order
  they appear in the source code.

  :*
  :and
  :cat
  :client
  :empty-set
  :epsilon
  :not
  :or
  :sigma
  :type

  Here are the keys assoc'd into the map in -canonicalize-pattern-once:

  :*
  :and
  :cat
  :empty-set
  :epsilon
  :not
  :or
  :sigma
  :type

  That is the same set of keys in the default value of
  *traversal-functions*, except no new value is assoc'd in for the
  key :client

traverse-pattern (file rte_construct.clj) directly calls:
  always convert-type-designator-to-rte first
  then conditionally one of:
  + (functions :epsilon)
  + (functions :empty-set)
  + (functions :sigma)
  + traverse-pattern (recursively)
  + (:type functions)
  

(defn test-post-condition
  [in]
  {:pre [(keyword? in)]
   :post [(fn [v] (#{true false :dont-know} v))]}
  (case in
    :a true
    :b false
    :c :dont-know
    :unknown-input))

(defn test-post-condition
  [in]
  {:pre [(keyword? in)]
   :post [(contains? #{true false :dont-know} %)]}
  (case in
    :a true
    :b false
    :c :dont-know
    :unknown-input))

(test-post-condition :a)
(test-post-condition :b)
(test-post-condition :c)
(test-post-condition :d)

(defn filter
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [f (first s) r (rest s)]
       (if (pred f)
         (cons f (filter pred r))
         (filter pred r))))))

(defn dedupe
  "Returns a lazy sequence removing consecutive duplicates in coll.
  Returns a transducer when no collection is provided."
  {:added "1.7"}
  ([]
   (fn [rf]
     (let [pv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (let [prior @pv]
              (vreset! pv input)
              (if (= prior input)
                result
                (rf result input))))))))
  ([coll] (sequence (dedupe) coll)))


(defn dedupe-by-f
  "Returns a lazy sequence removing consecutive 'duplicates' in coll.
  Two consecutive items x and y are only considered duplicates if (f x
  y) returns a logical true value.  Only the first of several
  consecutive duplicates is kept.  (dedupe-by-f = coll) is equivalent
  to Clojure's (dedupe coll). Returns a transducer when no collection
  is provided."
  ([f]
   (fn [rf]
     (let [pv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (let [prior @pv]
              (vreset! pv input)
              (if (f prior input)
                result
                (rf result input))))))))
  ([f coll] (sequence (dedupe-by-f f) coll)))

(dedupe-by-f = [1 2 2 2 3 2 4 4 5])

(defn =-and-*? [x y]
  (and (= x y)
       (sequential? x)
       (= :* (first x))))

(dedupe-by-f =-and-*? [1 2 2 2 3 2 4 4 5])

(dedupe-by-f =-and-*?
             '[(:* 1) (:* 2) (:* 2) (:* 2) (:* 3) (:* 2) (:* 4) (:* 4) (:* 5)])


(defn remove-second-of-first-pair-satisfying
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [x (first s) r (next s)]
       (if r
         (if (pred x (first r))
           (cons x (rest r))
           (cons x (remove-second-of-first-pair-satisfying pred r)))
         s)))))

(remove-second-of-first-pair-satisfying = [1 2 2 2 3 2 4 4 5])

(remove-second-of-first-pair-satisfying =-and-*? [1 2 2 2 3 2 4 4 5])

(remove-second-of-first-pair-satisfying =-and-*? [1 2 nil nil])

(remove-second-of-first-pair-satisfying = [1 2 nil nil])
(remove-second-of-first-pair-satisfying = [1 2 nil nil nil])
(remove-second-of-first-pair-satisfying = [1 2 1 1 nil nil nil])

(remove-second-of-first-pair-satisfying =-and-*?
             '[(:* 1) (:* 2) (:* 2) (:* 2) (:* 3) (:* 2) (:* 4) (:* 4) (:* 5)])
