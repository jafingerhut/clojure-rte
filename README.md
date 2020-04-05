# clojure-rte

This package impements rational type expressions (RTEs) for clojure.
The implementation is based on a similar package for Common Lisp.
The theory of how RTEs work can be found here:
  Introduction:   https://www.lrde.epita.fr/wiki/Publications/newton.16.els
  Phd Thesis:     https://www.lrde.epita.fr/wiki/Publications/newton.18.phd

## Installation

Download from git@gitlab.lrde.epita.fr:jnewton/clojure-rte.git
   or https://gitlab.lrde.epita.fr/jnewton/clojure-rte.git

## Usage


## Options

* `(:* ...)` --- matches 0 or more times

* `(:+ ...)` --- matches 1 or more times

* `(:? ...)` --- match 0 or 1 time

* `(:cat ...)` --- matches a sequence of patterns

* `(:and ...)` --- simulaneously matches all of the given patterns

* `(:or ...)` --- matches any of the given patterns

* `(:permute ...)` --- matches a sequence in any order

* `:empty-set` --- identity for `:or`

* `:epsilon` --- matching nothing once, identity for `:cat`

* `:sigma` --- matches anything once, identity for `:and`


## Examples

```clojure
(rte-match '(:cat (:* (:cat clojure.lang.Keyword java.lang.Long))
                                  (:? String))
           '(:x 1 :y 2 :z 42)) ;; --> true

(let [rte (rte-compile '(:cat (:* (:cat clojure.lang.Keyword java.lang.Long))
                                  (:? String)))]
  (rte-execute '(:x 1 :y 2 :z 42)) ;; --> true
  (rte-execute '(:x 1 :y 2 :z 42 "Hello")) ;; --> true
  (rte-execute '(:x 1 :y 2 :z 42 "Hello" "World")) ;; --> false
)
```


### Bugs

The rte keyword `:not` is partially implemented, but know to be buggy.
The intention is something like the following.

```clojure
(rte-match '(:cat (:* (:cat clojure.lang.Keyword (:not java.lang.Long)))
                                  (:? String))
           '(:x 1 :y 2 :z 42))
;; --> false
```

This does not yet work.



## License
```
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
```
