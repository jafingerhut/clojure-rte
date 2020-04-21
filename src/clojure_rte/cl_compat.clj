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

(ns clojure-rte.cl-compat)

(defn cl-prog1 [val & _]
  val)

(defn cl-prog2 [_ val & _]
  val)

(defn cl-progn [& others]
  (last others))

(defmacro cl-cond
  "Like CL:cond.  Each operand of the cl-cond is a list of length at least 1.
   The same semantics as clojure cond, in that the return value is
   determined by the first test which returns non-false.  The
   important semantic difference is that an agument has 1, then the
   specified form is both the test and the return value, and it is
   evaluated at most once.
   Implementation from:
   https://stackoverflow.com/questions/4128993/consolidated-cond-arguments-in-clojure-cl-style"
  ([] nil)
  ([[if1 & then1] & others]
   (when (or if1 then1 others)
     (let [extra-clauses# (when others `(cl-cond ~@others))]
       (if then1
         `(if ~if1 (do ~@then1) ~extra-clauses#)
         `(or ~if1 ~extra-clauses#))))))

