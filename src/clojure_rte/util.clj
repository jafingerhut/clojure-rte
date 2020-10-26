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

(ns clojure-rte.util
  (:require [clojure.pprint :refer [cl-format pprint]]))

(defn with-first-match 
  "Find the first element in the given sequence, items,
   for which the predicate, pred, returns Boolean true.
   If such is found, call the continuation with the
   element.  This type of 'finder' avoids the problem of
   deciding whether nil was the value found.  The continuation
   is only called on the found value."
  [pred items continuation]

  (loop [items items]
    (cond (empty? items)
          nil

          (pred (first items))
          (continuation (first items))

          :else
          (recur (rest items)))))

(defn remove-once 
  "Non-destructively remove the first element of the sequence which is
   = to the target value.  The list is unrolled as much as necessary,
   to remove the target value, and then the leading values are
   prepended, via concat, to the beginning of the remaining sequence.
   The tail of the sequence after finding the target is not examined,
   in case it is lazy.  If the taget does not appear in the list, a
   copy of the sequence is returned.  If the target item appears more
   than once, we have no way of knowing, and only the first such
   occurance is removed."
  [target items]

  (loop [items items
         acc ()]
    (cond
      (empty? items)
      (reverse acc)

      (= (first items) target)
      (concat (reverse acc) (rest items))

      :else
      (recur (rest items) (cons (first items) acc)))))

(defn call-with-collector
  "This function calls your given function which an argument which can be
   called to collect values.  The return value of call-with-collector is
   the list of items collected, in reverse order.  E.g.,
   (call-with-collector (fn [collect] 
                            ...body...))

   Within the body, collect is a unary function which can be called
   zero or more times.  The arguments are collected and returned as a
   in reverse order as if they were cons-ed onto an internal list.
   The caller is responsible for reversing the list if necessary."
  [unary-client]

  (with-local-vars [data '()]
    (unary-client (fn [obj]
                    (var-set data (cons obj @data))))
    @data))

(defn visit-permutations 
  "Call the given unary-client function once on each permutation
   of the given sequence of items.  Warning, there are n! many
   such permutations, so this function will be extremely slow
   if the (count items) is large.  If you want to return a list 
   of permutations, use visit-permutations in conjunction 
   with call-with-collector.
   (call-with-collector
     (fn [collect]
       (visit-permutations collect items)))"
  [unary-client items]
  
  (letfn [(visit-with-tail [remaining tail]
            (if (empty? remaining)
              (unary-client tail)
              (doseq [item remaining]
                (visit-with-tail (remove-once item remaining)
                                 (cons item tail)))))]
    (visit-with-tail items '())))

(defn rte-constantly
  "Return a binary function, similar to constanty, but the binary
   function ignores its second argument.  This function is useful as a
   callback function used to extend *traversal-functions*, as each
   such callback function must be a binary function."
  [x]
  (fn [_ _]
    x))

(defn rte-identity 
  "Similar to clojure.core.identity, except that this version is
   binary and always ignors its second argument.  This function is
   useful as a callback function used to extend *traversal-functions*,
   as each such callback function must be a binary function."
  [x _y]
  
  x)

(def problematic-operands (atom ()))

(defn sort-operands
 "Sort the given list of operands into deterministic order, making it possible
  to easily find identical elements, and to write test cases."
  [operands]
  (letfn [(cmp [a b]
            (cond
              (and (sequential? a)
                   (sequential? b))
              (loop [aa a
                     bb b]
                (cond
                  (and (empty? aa)
                       (empty? bb)) (compare (.getName (type a))
                                             (.getName (type b)))
                  (empty? aa) -1
                  (empty? bb) 1

                  (= (first aa) (first bb))   (recur (rest aa) (rest bb))
                  :else     (cmp (first aa) (first bb))))

              (= a b) (compare (.getName (type a))
                               (.getName (type b)))

              (sequential? a)
              -1

              (sequential? b)
              1
              
              (not (= (.getName (type a))
                      (.getName (type b))))
              (compare (.getName (type a))
                       (.getName (type b)))

              :else
              (compare a b)))]
    (try (sort cmp operands)
         (catch Exception e
           ;; were were getting a complaint from TIM sort.
           ;; however the error message did not give the offending list.
           ;; this code attempts to save the list in case TIM sort fails so we
           ;; can debug it.
           (swap! problematic-operands (fn [_] operands))
           (printf "saving problematic operands in *problematic-operands*\n")
           (println [:type (type operands)
                     :types (seq (map type operands))
                     :operands operands                     
                     :seq (seq operands)])
           (throw e)))))

(defn member
  "Like cl:member.  Determines whether the given target is an element of the given sequence."
  [target items]
  (boolean (cond
             (nil? target) (some nil? items)
             (false? target) (some false? items)
             :else (some #{target} items))))

(defn partition-by-pred 
  "Apply the predicate to every element of the sequence and return a vector of two
  values, each of which is a vector, the left vector is the set of the items
  in the original sequence for which the predicate returned a Boolean true value,
  the right vector are the other values from the given sequence."  
  [pred items]
  (let [g (group-by (fn [i]
                      (boolean (pred i))) items)]
    [(g true) (g false)]))

(defn find-simplifier [obj simplifiers]
  (if (empty? simplifiers)
    obj
    (loop [[f & fs] simplifiers]
      (let [new-obj (f obj)]
        (cond
          (not= new-obj obj)
          new-obj

          (empty? fs)
          obj

          :else
          (recur fs))))))

(def fixed-point-call-count (atom 0))

(defn reset-util-stats! []
  (reset! fixed-point-call-count 0))

(defn fixed-point
  "Find the fixed point of the given function starting at the given value.
  Convergence is detected when the function good-enough returns Boolean true
  when called with two consecutive values. The older of the two values
  is returned in this case."
  ([value f good-enough]
   (fixed-point value f good-enough false))
  ([value f good-enough heavy-logging]
   (swap! fixed-point-call-count inc)
   (let [call-count @fixed-point-call-count]
     (when heavy-logging
       (println "fixed-point #" call-count " value=" value))
     (loop [value value
            loop-count 0]
       (let [new-value (f value)
             same? (good-enough value new-value)]
         (when heavy-logging
           (println "fixed-point #" call-count " loop=" loop-count
                    "good-enough=" same?
                    (if same?
                      ""
                      (str "new-value= " new-value))))
         (if same?
           value
           (recur new-value (inc loop-count))))))))

(defn print-vals-helper ""
  [pairs]
  (let [N (count pairs)]
    (loop [val nil
           n 1
           pairs pairs]
      (if (empty? pairs)
        val
        (let [[[thunk1 e1] & pairs] pairs]
          (cl-format true "~A/~A ~A~%~T -> "
                     n N e1)
          (let [v1 (thunk1)]

          (cl-format true "~A~%"
                     v1)
          (recur v1 (inc n) pairs)))))))

(defmacro print-vals ""
  [& args]
  (let [pairs (into [] (map (fn [arg]
                              `[(fn [] ~arg) '~arg]) args))]
    `(print-vals-helper [~@pairs])))

(defn group-by-mapped
  "Like group-by but allows a second function to be mapped over each
  of the values in the computed hash map."
  [f1 f2 coll]
  (into {} (map (fn [[key value]]
                  [key (set (map f2 value))]) (group-by f1 coll))))


(defn mapc
  "Like map but does not accumulate the return values.  Returns its second argument."
  [f seq]
  (doseq [i seq]
    (f i))
  seq)
  
(defn assert-type 
  "Check the given value asserting it to be of the given type.
  If the type check is satisfied, the value is returned, otherwise
  an exception is throw.  This function can be used inline to
  assert types without changing the control flow."
  [type value]
  (if (instance? type value)
    value
    (throw (ex-info (cl-format false "value ~A expected to be ~A, not ~A"
                               value type (type value))
                    {:expected-type type
                     :actual-type (type value)
                     :value value}))))

(defn first-repeat
  "Return the first tail, (a b ...) of data which satisfies (test a b)"
  [data test]
  (loop [tail data]
    (cond (empty? tail)
          ()

          (empty? (rest tail))
          ()

          (test (first tail) (second tail))
          tail

          :else
          (recur (rest tail)))))

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
