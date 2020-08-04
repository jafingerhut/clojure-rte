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
  "Definition of records State and Dfa."
  (:require [clojure-rte.cl-compat :refer [cl-cond]]
            [clojure-rte.util :refer [fixed-point member group-by-mapped print-vals]]
            [clojure-rte.type :as ty]
            [clojure.set :refer [union difference intersection]]
))

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

;; TODO - need to assert every time a Dfa gets created that no two states have the same :index
(defrecord Dfa [pattern canonicalized states exit-map combine-labels])

(defn record-name
  []
  Dfa)

(defn state-by-index
  "Return the State object of the Dfa whose :index is the given index."
  [dfa index]
  ((:states dfa) index))

(defn states-as-seq
  "Return a sequence of states which can be iterated over"
  [dfa]
  (cl-cond
   ((map? (:states dfa))
    (vals (:states dfa)))
   ((sequential? (:states dfa))
    (:states dfa))
   (:else
    (throw (ex-info (format "invalid :states = %s" (:states dfa)))))))

(defn serialize-state
  "Serialize a State for debugging"
  [state]
   [:index (:index state)
            :accepting (:accepting state)
            :pattern (:pattern state)
            :transitions (:transitions state)])

(defn serialize-dfa
  "Serialize a Dfa for debugging"
  [dfa]
  (map serialize-state (states-as-seq dfa)))

(defn delta
  "Given a state and target-label, find the destination state (object of type State)"
  [dfa source-state target-label]
  (let [[_ index] (first (filter (fn [[label dst-index]] (= label target-label))
                                 (:transitions source-state)))]
    (state-by-index dfa index)))
  
(defmethod print-method Dfa [v w]
  (.write w (format "#<Dfa %d states>" (count (states-as-seq v)))))

(defn split-eqv-class
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
  "Apply the Hopcroft partition algorithm to the states of the given
  Dfa to return a set of eqv-classes.  This set of eqv-classes is a partition
  of the initial set of states.
  Each eqv-class is a set of states."
  [dfa]
  (let [[finals non-finals] (map (group-by :accepting (states-as-seq dfa)) [true false])
        pi-0 (conj (split-eqv-class finals
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
                        (split-eqv-class eqv-class Phi))]
                (mapcat repartition partition)))]
      (fixed-point pi-0 refine =))))

(defn min-state
  "Compute the minimimum :index of the given set of states"
  [eqv-class] ;; a set of states
  (reduce min (map :index eqv-class)))

(defn find-sink-states
  "Find the set (as sequence) of all sink states in the given Dfa.
  A sink state is not a final state,
  and all its transitions point to itself."
  [dfa]
  (filter (fn [q]
            (and (not (:accepting q))
                 (every? (fn [[label dst]]
                           (= dst (:index q)))
                         (:transitions q))))
          (states-as-seq dfa)))

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
    (letfn [(pretty-or [rest-args]
              (cl-cond
               ((empty? rest-args)
                :sigma)
               ((empty? (rest rest-args))
                (first rest-args))
               (:else
                (conj rest-args  :or))))
            (new-id [state]
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
                                                          (new-id (state-by-index dfa dst-id))]
                                                         ) (:transitions q))))
                                              (states-as-seq dfa)))
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
         (assoc dfa :exit-map (into {} (map (fn [id]
                                               [id ((:exit-map dfa) id)])
                                             new-fids)) ;; map each of new-fids to the old value returned from the exit-map
                     :states
                     (into {} (mapcat (fn [id ]
                                        (let [transitions (grouped id)]
                                          (if (not transitions)
                                            nil ;; contribute nothing to the mapcat for this iteration.
                                            [[id (map->State
                                                  {:index id
                                                   :initial (= 0 id)
                                                   :pattern (pretty-or (map :pattern (partitions-map id)))
                                                   :accepting (member id new-fids)
                                                   :transitions (map rest transitions)})]]
                                            ))) ids))
                     ))))))

(defn trim
  "Creates a new Dfa from the given Dfa containing only accessible and co-accessible
  states.  Warning, this removes the sink state if there is one.  The result is
  that the computed Dfa may not any longer be complete."
  [dfa]
  (let [transition-pairs (mapcat (fn [q]
                                     (map (fn [[_ dst-id]]
                                            [(:index q) dst-id])
                                          (:transitions q)))
                                 (states-as-seq dfa))
        forward-map (group-by-mapped first second transition-pairs)
        backward-map (group-by-mapped second first transition-pairs)]
    (letfn [(trace-fb [states fb-map]
              (loop [states states
                     done #{}]
                (if (empty? states)
                  done
                  (let [next-states (mapcat (fn [id]
                                              (if (member id done)
                                                nil
                                                (fb-map id))) states)
                        new-next-states (difference (set next-states) done)]
                    (recur new-next-states (union done states))))))
            (trace-forward [states]
              (trace-fb states forward-map))
            (trace-backward [states]
              (trace-fb states backward-map))]
      (let [
            ;; Trace forward from initial state, collecting all states reached.
            ;; These are the accessible states.
            accessible (trace-forward #{0})
            final-accessible (clojure.set/intersection accessible
                                                       (set (map :index (filter :accepting (states-as-seq dfa)))))
            ;; trace backward starting from the set of all final states which are
            ;; co-accessible, collecting all states.  
            co-accessible (trace-backward final-accessible)
            useful (intersection co-accessible accessible)
            new-fids (filter (fn [id] (:accepting (state-by-index dfa id)))
                             useful)
            ]
        ;; now build a new Dfa, omitting any state not in the co-accessible list
        ;; any transition going to a state which has being removed, gets
        ;; diverted to the sink state.
        (map->Dfa
         (assoc dfa
          :exit-map (into {} (map (fn [id]
                                    [id ((:exit-map dfa) id)])
                                  new-fids)) ;; map each of new-fids to the old value returned from the exit-map
          :states
          (into {} (map (fn [id]
                          (let [state (state-by-index dfa id)]
                            [id (map->State
                                 (assoc state
                                        :index id
                                        :accepting (member id new-fids)
                                        :initial (= id 0)
                                        :transitions (filter (fn [[label dst-id]]
                                                               (member dst-id co-accessible))
                                                             (:transitions state))))]))
                        useful))))
))))

(defn intersect-labels
  ""
  [label-1 label-2]
  (letfn [(and? [label]
            (and (sequential? label)
                 (= 'and (first label))))
          (pretty-and [rest-args]
            (cond
              (empty? rest-args)
              :empty-set
              
              (empty? (rest rest-args))
              (first rest-args)
              
              (= (distinct rest-args) rest-args)
              (concat (list 'and) rest-args)
              
              :else
              (pretty-and (distinct rest-args))))]
              
    (cl-cond
     ((= label-1 :sigma)
      label-2)
     ((= label-2 :sigma)
      label-1)
     ((= label-1 :empty-set)
      :empty-set)
     ((= label-2 :empty-set)
      :empty-set)
     ((= label-1 label-2)
      label-1)
     ((and (and? label-1)
           (and? label-2))
      (pretty-and (concat (rest label-1) (rest label-2))))
     ((and? label-1)
      (pretty-and (conj (rest label-1) label-2)))
     ((and? label-2)
      (pretty-and (conj (rest label-2) label-1)))
     (:else
      (pretty-and (list label-1 label-2))))))
   

(defn synchronized-product
  [dfa-1 dfa-2 f-arbitrate-accepting f-arbitrate-exit-value]
  "Assuming that the given Dfas are complete, we compute the syncronized cross product SXP
  of the two Dfas."
  (letfn [
          (find-reachable [state-pairs]
            (loop [state-pairs state-pairs
                   done #{}]
              (if (empty? state-pairs)
                done
                (let [next (set (for [[id-1 id-2] state-pairs
                                      [_ dst-1] (:transitions (state-by-index dfa-1 id-1))
                                      [_ dst-2] (:transitions (state-by-index dfa-2 id-2))]
                                  [dst-1 dst-2]))]
                  (recur (difference next done)
                         (union state-pairs done))))))]
    (let [sxp-pairs (sort (fn [[a b] [x y]]
                            (cond
                              (= a x)
                              (< b y)
                              :else
                              (< a x)))
                          (find-reachable #{[0 0]}))
          state-ident-map  (zipmap sxp-pairs (range))
          ident-state-map  (zipmap (range) sxp-pairs)
          accepting-ids (for [[id [id-1 id-2]] ident-state-map
                              :when (f-arbitrate-accepting (:accepting (state-by-index dfa-1 id-1))
                                                           (:accepting (state-by-index dfa-2 id-2)))]
                          id)
          ]
      (assert (= 0 (state-ident-map [0 0])))
      (assert (= [0 0] (ident-state-map 0)))

      (map->Dfa (assoc dfa-1
                       :exit-map (into {} (for [[id [id-1 id-2]] ident-state-map
                                                :when (member id accepting-ids)]
                                            [id (f-arbitrate-exit-value
                                                 (:exit-value (state-by-index dfa-1 id-1))
                                                 (:exit-value (state-by-index dfa-2 id-2)))]))
                       
                       :states (into {} (for [[id [id-1 id-2]] ident-state-map]
                                          [id (map->State {:index id
                                                           :initial (print-vals id (= 0 id))
                                                           :accepting (member id accepting-ids)
                                                           :transitions
                                                           (for [[label-1 dst-1] (:transitions (state-by-index dfa-1 id-1))
                                                                 [label-2 dst-2] (:transitions (state-by-index dfa-2 id-2))
                                                                 :let [label-sxp (intersect-labels label-1 label-2)]
                                                                 :when (not (ty/disjoint? label-1 label-2 (constantly true)))]
                                                             [label-sxp (state-ident-map [dst-1 dst-2])])})])))))))
