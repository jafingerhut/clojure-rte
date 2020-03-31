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

(ns clojure-rte.core
  (:require [clojure.set :refer [union]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn nullable [expr]
  (cond (sequential? expr)
        (let [[token & args] expr]
          (case token
            :or (some nullable args)
            :and (every? nullable args)
            :cat (every? nullable args)
            :* true
            :? true
            :+ (nullable (first args))
            :not (not (nullable (first args)))
            false))
        :else
        (if (= :epsilon expr)
          true
          (if (= :empty-set expr)
            false
            false))))

(defn first-types [expr]
  (cond (sequential? expr)
        (let [[token & args] expr]
          (if (nil? args)
            (case token
              :or   (first-types :empty-set)
              :and  (first-types :sigma)
              :cat  (first-types :empty-word)
              #{token})
            (case token
              :or  (reduce union (map first-types args))
              :and (reduce union (map first-types args))
              :cat (cond
                     (nullable (first args))
                     (union (first-types (first args))
                            (first-types `(:cat ~@(rest args))))
                     
                     :else
                     (first-types (first args)))
              :* (first-types (first args))
              :? (first-types `(:or :epsilon ~(first args)))
              :+ (first-types `(:cat ~(first args) (:* ~(first args))))
              :not (first-types (first args))

              #{ `(~token ~@args)})))
        :else
        (case expr
          :sigma #{:sigma}
          :empty-set #{}
          :epsilon #{}
          #{expr})))

