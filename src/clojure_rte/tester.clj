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

(ns clojure-rte.tester
  (:require [clojure.pprint :refer [cl-format]])
)


(defn simplify [unary error-case gen-components]
  (try (do (unary error-case)
           error-case)
       (catch Exception e
         (do
           (cl-format true "e=~A~%" e)
           (or (some (fn [component]
                       (simplify unary component gen-components)) (gen-components error-case))
               error-case)))))

(defn random-test [num-tries unary-test-fun arg-generator gen-components]
  (loop [num-tries num-tries]
    (if (< 0 num-tries)
      (let [data (arg-generator)]
        ;;(cl-format true "~d: trying ~A~%" num-tries data)
        (unary-test-fun data)
        (recur (dec num-tries))))))
