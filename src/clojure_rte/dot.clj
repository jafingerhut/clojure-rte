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

(ns clojure-rte.dot
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.string]
            [clojure.set]
            [clojure-rte.cl-compat :refer [cl-cond]]
            [clojure-rte.core]
            [clojure-rte.dfa :as dfa]
            [clojure-rte.bdd :as bdd]
            [clojure-rte.util :refer [member print-vals mapc]]
            [clojure.java.shell :refer [sh]]))

(def ^:dynamic *dot-path*
  "Full path to the graphviz dot program"
  (let [m (sh "which" "dot")]
    (cond
      (= 0 (:exit m)) (clojure.string/trim (:out m))
      :else "dot")))

(def ^:dynamic *dot-tmp-dir*
  "Directory to put temporary files"
  (let [m (sh "mktemp" "-d")
        tmp-dir (str (clojure.string/trim (:out m)) "/.")]
    (sh "mkdir" "-p" tmp-dir)
    tmp-dir))

(defn dfa-to-dot 
  "Create (and possibly display) a graphical image rendering the automaton
  represented by the given dfa.  dfa is a value as returned from function
  rte-compile, or rte-to-dfa.
  For Mac OS, the :view option may be used to display the image
  interactively."
  [dfa & {:keys [title view abbrev draw-sink verbose state-legend]
          :or {title "no-title"
               draw-sink false
               abbrev true
               view false
               verbose false
               state-legend true}}]
  (cond
    view (let [png-file-name (str *dot-tmp-dir* "/" title ".png")
               dot-string (dfa-to-dot dfa :draw-sink draw-sink :title title :view false :abbrev abbrev
                                      :state-legend state-legend)]
           (if verbose
             (println [:title title :dfa dfa :draw-sink draw-sink
                       :dot-string dot-string :state-legend state-legend]))
           (sh *dot-path* "-Tpng" "-o" png-file-name
               :in dot-string)
           (when (= "Mac OS X" (System/getProperty "os.name"))
             (let [stat (sh "open" "-a" "Preview" png-file-name)]
               (if (not (= 0 (:exit stat)))
                 (println dot-string))
               stat)))
    :else
    (let [sink-states (dfa/find-sink-states dfa)
          visible-states (if draw-sink
                         (dfa/states-as-seq dfa)
                         (remove (fn [q] (member q sink-states)) (dfa/states-as-seq dfa)))
          visible-state-ids (map :index visible-states)
          transition-labels (distinct (for [q visible-states
                                            [label dst-id] (:transitions q)
                                            :when (member dst-id visible-state-ids)]
                                        label))          
          abbrevs (zipmap transition-labels (range (count transition-labels)))
          indices (clojure.set/map-invert abbrevs)]
      (with-out-str
        (cl-format *out* "digraph G {~%")
        (when title
          (cl-format *out* "// ~a~%" title))
        (cl-format *out* "  rankdir=LR;~%")
        (cl-format *out* "  fontname=courier;~%")
        (when abbrev
          (cl-format *out* "  label=\"~a\\l\"~%"
                     (clojure.string/join
                      "" (concat (map (fn [index]
                                        (cl-format false "\\lt~a= ~a" index (indices index)))
                                      (range (count (keys indices))))
                                 ["\\l"]
                                 (if state-legend
                                   (for [q visible-states
                                         :when (boolean (:pattern q))]
                                     (cl-format false "\\lq~a= ~a"
                                                (:index q) (:pattern q)))
                                   "")))))
        (cl-format *out* "  graph [labeljust=l,nojustify=true];~%")
        (cl-format *out* "  node [fontname=Arial, fontsize=25];~%")
        (cl-format *out* "  edge [fontname=Helvetica, fontsize=20];~%")

        (doseq [q (dfa/states-as-seq dfa)]
          (cl-cond
           ((and (member q sink-states)
                 (not draw-sink)))
           (:else
            (when (:accepting q)
              (cl-format *out* "   q~D [shape=doublecircle] ;~%" (:index q))
              (cl-format *out* "   X~D [label=\"~A\", shape=rarrow]~%" ;; or plaintext ?
                         (:index q) ((:exit-map dfa) (:index q)))
              (cl-format *out* "   q~D -> X~D ;~%" (:index q) (:index q)))
            (when (:initial q)
              (cl-format *out* "   H~D [label=\"\", style=invis, width=0]~%" (:index q))
              (cl-format *out* "   H~D -> q~D;~%" (:index q) (:index q)))
            (doseq [[type-desig next-state] (:transitions q)]
              (cl-cond
               ((and (member (dfa/state-by-index dfa next-state) sink-states)
                     (not draw-sink)))
               (abbrev
                (cl-format *out* "   q~D -> q~D [label=\"t~a\"];~%" (:index q) next-state (abbrevs type-desig)))
               (:else
                (cl-format *out* "   q~D -> q~D [label=\"~a\"];~%" (:index q) next-state type-desig)))))))
        
        (cl-format *out* "}~%")))))


(defn bdd-to-dot 
  "Create (and possibly display) a graphical image rendering the given Bdd
  For Mac OS, the :view option may be used to display the image
  interactively."
  [bdd & {:keys [title view verbose pen-width draw-false-leaf]
          :or {title "no-title"
               view false
               verbose false
               pen-width 2
               draw-false-leaf true}}]
  (cond
    view (let [png-file-name (str *dot-tmp-dir* "/" title ".png")
               dot-string (bdd-to-dot bdd :title title :view false
                                      :pen-width pen-width :draw-false-leaf draw-false-leaf)]
           (when verbose
             (println [:title title :view view
                       :pen-width pen-width :draw-false-leaf draw-false-leaf]))
           (let [stat (sh *dot-path* "-Tpng" "-o" png-file-name
                           :in dot-string)]
             (if (not (= 0 (:exit stat)))
               (println stat)))
           (when (= "Mac OS X" (System/getProperty "os.name"))
             (let [stat (sh "open" "-a" "Preview" png-file-name)]
               (if (not (= 0 (:exit stat)))
                 (println dot-string))
               stat)))
    :else
    (letfn [(draw-connection [direction bdd node-to-index]
              (cond
                (and (not draw-false-leaf)
                     (= (direction bdd) false))
                nil

                (= :positive direction)
                (cl-format *out* "~D -> ~D [style=~A,color=~A,penwidth=~D]~%"
                           (node-to-index bdd) (node-to-index (direction bdd))
                           "solid" "green" pen-width)

                (= :negative direction)
                (cl-format *out* "~D -> ~D [style=~A,color=~A,penwidth=~D,arrowhead=~s,arrowtail=~s,dir=~s]~%"
                           (node-to-index bdd) (node-to-index (direction bdd))
                           "dashed" "red" pen-width "normal" "odot" "both")))
            
            (write-leaf [leaf node-to-index]
              (cond
                (= true leaf)
                (cl-format *out* "~D [shape=~A,label=~S,fontname=~S]~%"
                           (node-to-index leaf)
                           "box"
                           "T"
                           "sans-serif")
                (= false leaf)
                (when draw-false-leaf
                  (cl-format *out* "~D [shape=~A,label=~S]~%"
                          (node-to-index leaf)
                          "box"
                          "&perp;"))))
            
            (top-sort [groups]
              ;; find all the labels which no bdd references,
              ;; these are the independent nodes.
              (loop [labels (set (keys groups))
                     groups groups
                     acc []]
                (if (empty? labels)
                  acc
                  (let [referenced-labels (set (for [[label seq] groups
                                                     bdd seq
                                                     direction '(:positive :negative)
                                                     :let [child (direction bdd)]
                                                     :when (not (boolean? child))]
                                                 (:label child)))
                        unreferenced-labels (clojure.set/difference labels referenced-labels)
                        ]
                    (recur referenced-labels
                           (apply dissoc groups unreferenced-labels)
                           (concat acc unreferenced-labels))))))]

      (with-out-str
        (cl-format *out* "digraph G {~%")
        (when title
          (cl-format *out* "// ~a~%" title))
        (cl-format *out* "  fontname=courier;~%")
        (let [all-nodes (set (tree-seq (fn [node]
                                              (not (member node '(true false))))
                                            (fn [bdd]
                                              (for [child '(:positive :negative)
                                                    :when (or draw-false-leaf
                                                              (child bdd))]
                                                (child bdd)))
                                            bdd))
              node-to-index (zipmap all-nodes (range))
              apex-node (first all-nodes)
              leaf-nodes (clojure.set/intersection #{true false} all-nodes)
              internal-nodes (clojure.set/difference all-nodes leaf-nodes)
              groups (group-by :label internal-nodes)
              sorted-labels (top-sort groups)]

          (if (member apex-node '(true false))
            (write-leaf apex-node node-to-index) ;; always draw the leaf if there's only one, independent of draw-false-leaf
            (do
              (if draw-false-leaf
                (doseq [leaf leaf-nodes]
                  (write-leaf leaf node-to-index))
                (write-leaf true node-to-index))
              (doseq [label sorted-labels]
                (cl-format *out* "{rank=same")
                (doseq [bdd (groups label)]
                  (cl-format *out* " ~D" (node-to-index bdd)))
                (cl-format *out* "}~%")
                (doseq [bdd (groups label)]
                  (cl-format *out* "~D [shape=~A,label=~S,penwidth=~D]~%"
                             (node-to-index bdd)
                             "ellipse"
                             (cl-format false "~A" label)
                             pen-width)
                  (draw-connection :positive bdd node-to-index )
                  (draw-connection :negative bdd node-to-index )))
              
        )))
        (cl-format *out* "}~%")))))
