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

* `(:cat ...)` --- matches a sequence of patterns
** Example 

```clojure
(let [rte (rte-compile '(:cat integer? String))]
  (rte-execute rte [1 "hello"]) ;; true
  (rte-execute rte [1.0 "hello"]) ;; false
  (rte-execute rte [1 2]) ;; false
  (rte-execute rte [1 2 "hello"]) ;; false
  )
```

* `(:+ ...)` --- matches 1 or more times
** Example 

```clojure
(let [rte (rte-compile '(:+ integer?))]
  (rte-execute rte [1 2 3 4 5]) ;; true
  (rte-execute rte [1]) ;; true
  (rte-execute rte []) ;; false
  (rte-execute rte [1.0 2 3.0 4 5]) ;; false
  )
```

* `(:* ...)` --- matches 0 or more times
** Example 

```clojure
(let [rte (rte-compile '(:* integer?))]
  (rte-execute rte [1 2 3 4 5]) ;; true
  (rte-execute rte [1]) ;; true
  (rte-execute rte []) ;; true
  (rte-execute rte [1.0 2 3.0 4 5]) ;; false
  )
```

* `(:? ...)` --- match 0 or 1 time
** Example 

```clojure
(let [rte (rte-compile '(:? integer?))]
  (rte-execute rte [1]) ;; true
  (rte-execute rte []) ;; true
  (rte-execute rte [1 2]) ;; false
  )

(let [rte (rte-compile '(:cat integer? (:? String)))]
  (rte-execute rte [1 2 3 4 5]) ;; true
  (rte-execute rte [1 2 3 4 5 "hello"]) ;; true
  (rte-execute rte [1 2 3 4 5 "hello" "world"]) ;; false
  )
```

* `(:and ...)` --- simulaneously matches all of the given patterns
** Example ---  Keyword followed by 1 or two integers, repeated any number of times which is a multiple of 3 total items

```clojure
(let [rte (rte-compile '(:and (:* (:cat Keyword integer? (:? Integer)))
                              (:+ (:sigma :sigma :sigma))))]
  (rte-execute rte [:x 1 :x 2 :x 3]) ;; true
  (rte-execute rte [:x 1 2 :y 2 3]) ;; true
  (rte-execute rte [:x 1 :y 3]) ;; false
  )
```


* `(:or ...)` --- matches any of the given patterns
** Example  ---   either 0 or more integers, or 1 or more strings

```clojure
(let [rte (rte-compile '(:or (:* integer?) (:+ String)))]
  (rex-execute rte []) ;; true, 0 integers
  (rex-execute rte [1 2 3]) ;; true
  (rex-execute rte ["hello" "world"]) ;; true one or more strings
  )
```


* `(:permute ...)` --- matches a sequence in any order
** Example --- two integers and a string in any order

```clojure
(let [rte (rte-compile '(:permute integer? integer? String))]
  (rex-execute rte [1 2 "hello"]) ;; true
  (rex-execute rte [1 "hello" 2]) ;; true
  (rex-execute rte ["hello" 1 2]) ;; true
  (rex-execute rte ["hello" 2]) ;; false
  (rex-execute rte [1 2]) ;; false
  )
```


* `:empty-set` --- identity for `:or`
** Example 

```clojure
(let [rte (rte-compile '(:* (:or integer? String)))]
  (rex-execute rte [1]) ;; true
  (rex-execute rte [1 "hello"]) ;; true
  (rex-execute rte ["hello" "world" 1 2 "hello" 3 "world"]) ;; true

```


* `:epsilon` --- matching nothing once, identity for `:cat`

** Example 

```clojure
```


* `:sigma` --- matches anything once, identity for `:and`

** Example -- any number of repetitions of integer anything String

```clojure
(let [rte (rte-compile '(:* (:cat integer? :sigma String)))]
  (rex-execute rte []) ;; true
  (rex-execute rte [1]) ;; false
  (rex-execute rte [1 2]) ;; false
  (rex-execute rte [1 "hello" 2]) ;; true
  (rex-execute rte [1 "hello" 2 3 "world" 4 1 "hello" 2 3 "world" 4]) ;; true
  )
```



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

The rte keyword `:not` is partially implemented, but known to be buggy.
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
