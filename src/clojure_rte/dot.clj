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
            [clojure-rte.core :refer [dfa-states-as-seq]]
            [clojure-rte.util :refer [member print-vals]]
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
  [dfa & {:keys [title view abbrev draw-sink]
          :or {title "no-title"
               draw-sink false
               abbrev true
               view false}}]
  (cond
    view (let [png-file-name (str *dot-tmp-dir* "/" title ".png")
               dot-string (dfa-to-dot dfa :title title :view false :abbrev abbrev)]
           (sh *dot-path* "-Tpng" "-o" png-file-name
               :in dot-string)
           (when (= "Mac OS X" (System/getProperty "os.name"))
             (sh "open" "-a" "Preview" png-file-name)))
    :else
    (let [transition-labels (distinct (mapcat (fn [q]
                                                (map first (:transitions q)))
                                              (dfa-states-as-seq dfa)))
          sink-states (filter (fn [q]
                                (and (= 1 (count (:transitions q)))
                                     (every? (fn [[label dst]] (and (= :sigma label)
                                                                    (not (:accepting q))
                                                                    (= dst (:index q))))
                                             (:transitions q))))
                              (dfa-states-as-seq dfa))
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
                     (clojure.string/join "" (concat (map (fn [index]
                                                            (cl-format false "\\lt~a= ~a" index (indices index)))
                                                          (range (count (keys indices))))
                                                     ["\\l"]
                                                     (map (fn [q] (cl-format false "\\lq~a= ~a"
                                                                             (:index q) (:pattern q))) (dfa-states-as-seq dfa))
))))
        (cl-format *out* "  graph [labeljust=l,nojustify=true];~%")
        (cl-format *out* "  node [fontname=Arial, fontsize=25];~%")
        (cl-format *out* "  edge [fontname=Helvetica, fontsize=20];~%")

        (doseq [q (dfa-states-as-seq dfa)]
          (cl-cond
           ((and (member q sink-states)
                 (not draw-sink)))

           (:else
            (when (:accepting q)
              (cl-format *out* "   q~D [shape=doublecircle] ;~%" (:index q)))
            (when (:initial q)
              (cl-format *out* "   H~D [label=\"\", style=invis, width=0]~%" (:index q))
              (cl-format *out* "   H~D -> q~D;~%" (:index q) (:index q)))
            (doseq [[type-desig next-state] (:transitions q)]
              (print-vals (:states dfa))
              (cl-cond
               ((and (member ((:states dfa) next-state) sink-states)
                     (not draw-sink)))
               (abbrev
                (cl-format *out* "   q~D -> q~D [label=\"t~a\"];~%" (:index q) next-state (abbrevs type-desig)))
               (:else
                (cl-format *out* "   q~D -> q~D [label=\"~a\"];~%" (:index q) next-state type-desig)))))))
        
        (cl-format *out* "}~%")))))
