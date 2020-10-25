;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL input for repl-session1.txt when OutOfMemoryException occurred

(require 'clojure-rte.rte-tester)
(def rng-seed 5)
(clojure-rte.rte-tester/-main rng-seed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL input for repl-session2.txt when no OutOfMemoryException
;; occurred when calling canonicalize-pattern on the same input that
;; caused the OOM during repl-session1.txt, but did not when that was
;; the only parameter ever given to canonicalize-pattern.

(require 'clojure-rte.rte-tester)
(require '[clojure-rte.rte-core :as co])

(def problem-pattern
  '(:cat (:not (:? (member a b c a b c))) (:cat (:not (:cat :empty-set)) (:? (:cat (:and))) (:and (:or (:cat)) (:+ (:cat)))) (:+ (:or (:* (:and)) (:? :sigma))) :epsilon))
(println problem-pattern)
(co/reset-stats!)
(co/canonicalize-pattern problem-pattern)


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
