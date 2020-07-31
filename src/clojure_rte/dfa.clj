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

(ns clojure-rte.dfa
  "Definition of records State and Dfa.")

(in-ns 'clojure-rte.core)


(defrecord State 
  ;; :index -- the index of this state in the array
  ;; :accepting - Boolean true/false indicating whether this state is a
  ;;     final/accepting state.
  ;; :pattern -- the derivative value representing an rte pattern matching
  ;;     any tail of the input sequence which is accepting from this point
  ;;     onward.
  ;; :sync-state -- Boolean
  ;; :pattern -- 
  ;; :transitions -- A list of pairs, each pair is a 2 element array of the form
  ;;     [type next-state], e.g., [clojure.lang.Keyword 1]
  ;;     which means if the value at the head of the sequence is of type
  ;;     clojure.lang.Keyword, then go to state 1.  The type is some value
  ;;     compatible with isa?.  the state index is some index of the state
  ;;     array representing the finite atomaton.
  [index accepting pattern transitions])

(defmethod print-method State [v w]
  (.write w (format "#<State %d>" (:index v))))

(defrecord Dfa [pattern canonicalized states exit-map combine-labels])

(defn delta
  "Given a state and target-label, find the destination state (object of type State)"
  [dfa source-state target-label]
  (let [[_ index] (first (filter (fn [[label dst-index]] (= label target-label))
                                 (:transitions source-state)))]
    ((:states dfa) index)))
  
(defmethod print-method Dfa [v w]
  (.write w (format "#<Dfa %d states>" (count (:states v)))))

(defn %partition-by
  "Given a set of objects, return a set of subsets thereof which is a partition of
  the given set.   Every element in any some set has the same value under f, and
  the value under f is different for any distinct subsets.  f is not called
  if the size of the given set is 1."
  [objects f]
  (if (= 1 (count objects))
    #{ objects}
    (set (map set (vals (group-by f objects))))))

(defn find-eqv-class
  "Given a sequence of sequences, find the leaf level
  sequence which contains the given element"
  [partition target]
  (first (filter (fn [eqv-class]
                   (member target eqv-class)) partition)))

(defn find-hopcroft-partition
  ""
  [dfa]
  (let [[finals non-finals] (map (group-by :accepting (:states dfa)) [true false])
        pi-0 (conj (%partition-by finals
                                  (fn [state]
                                    ((:exit-map dfa) (:index state))))
                   non-finals)]
    (letfn [(refine [partition]
              (letfn [(phi [source-state label]
                        ;; find the element of partition, itself an equivalence class, which
                        ;;   which contains the destination state of the transition
                        ;;   whose source and label are given
                        (find-eqv-class partition (delta dfa source-state label)))
                      (Phi [s]
                        (assert (:combine-labels dfa))
                        (map (fn [[eqv-class transitions]]
                               ;; trans is a seq of transitions, each [label dst-index]
                               (reduce (:combine-labels dfa)
                                       (map (fn [[label _]] label) transitions)))
                             (group-by (fn [[label _]] (phi s label)) (:transitions s))))
                      (repartition [eqv-class]
                        (%partition-by eqv-class Phi))]
                (mapcat repartition partition)))]
      (fixed-point pi-0 refine =))))

(defn min-state
  "Compute the minimimum :index of the given set of states"
  [eqv-class] ;; a set of states
  (reduce min (map :index eqv-class)))

(defn tabulate
  ""
  [n f]
  (into [] (map f (range n))))

(defn minimize
  "Accepts an object of type Dfa, and returns a new object of type Dfa
  implementing the minimization of the state machine according to the
  Hopcroft minimization algorithm."
  [dfa]
  (let [pi-minimized (find-hopcroft-partition dfa)
        ids (map min-state pi-minimized)
        partitions-map (zipmap ids pi-minimized)
        ids-map (zipmap pi-minimized ids)]

    (assert (sequential? pi-minimized))
    (letfn [(new-id [state]
              (assert (instance? State state))
              (ids-map (find-eqv-class pi-minimized state)))]
      (let [new-fids (mapcat (fn [id eqv-class]
                               ;; does there exists an s in eqv-class such that (:accepting s)
                               (if (some :accepting eqv-class)
                                 (list id)
                                 nil))
                             ids pi-minimized)
            new-proto-delta (distinct (mapcat (fn [q]
                                                (let [new-src-id (new-id q)]
                                                  (map (fn [[label dst-id]]
                                                         [new-src-id
                                                          label
                                                          (new-id ((:states dfa) dst-id))]
                                                         ) (:transitions q))))
                                              (:states dfa)))
            grouped (group-by (fn [[new-src-id _ _]] new-src-id) new-proto-delta)
            new-exit-map (into {}
                               (mapcat (fn [id eqv-class]
                                         (if (some :accepting eqv-class)
                                           (list [id
                                                  ((:exit-map dfa) (:index (first eqv-class)))])
                                           nil))
                                       ids pi-minimized))
            ]
            
    (map->Dfa
     {:pattern (:pattern dfa)
      :canonicalized (:cononicalized dfa)
      :exit-map (into {} (map (fn [id]
                                [id ((:exit-map dfa) id)]) new-fids)) ;; map each of new-fids to the old value returned from the exit-map
      :combine-labels (:combine-labels dfa)
      :states
      (tabulate (inc (reduce max ids ))
                (fn [id]
                  (let [transitions (grouped id)]
                    (if (not transitions)
                      nil
                      (map->State
                       {:index id
                        :accepting (member id new-fids)
                        :transitions (map (fn [[src-id label dst-id]]
                                            [label dst-id])
                                          transitions)})))))})))))

(defn call-with-index-of-nil ""
  [f ar default]
  (let [len (count ar)]
    (loop [i 0]
      (cond
        (= i len) default
        (nil? (ar i)) (f i)
        :else (recur (inc i))))))

(defn renumber
  ""
  ([dfa]
   (call-with-index-of-nil
    (fn [i]
      (renumber dfa i (dec (count (:states dfa)))))
    (:states dfa)
    dfa))
  ([dfa new-id old-id] ;; old-id is being removed
   (renumber
    (map->Dfa
     {:pattern (:pattern dfa)
      :canonicalized (:cononicalized dfa)
      :exit-map (into {} (map (fn [[key value]]
                                (if (= key old-id)
                                  [new-id value]
                                  [key value])) (:exit-map dfa)))
      :combine-labels (:combine-labels dfa)
      :states
      (tabulate (dec (count (:states dfa)))
                (fn [id]
                  (let [old-state ((:states dfa) (if (= id new-id)
                                                   old-id
                                                   new-id))]
                    (map->State
                     {:index id
                      :accepting (:accepting old-state)
                      :transitions (map (fn [[label dst-id]]
                                          [label (if (= old-id dst-id)
                                                   new-id
                                                   dst-id)])
                                        (:transitions old-state))}))))}))))
  

