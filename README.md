# clojure-rte

This package implements rational type expressions (RTEs) for the [Clojure](https://clojure.org) programming language.
The implementation is based on a similar package for [Common Lisp](https://lisp-lang.org).
The theory of how RTEs work can be found here: [Type-Checking of Heterogeneous Sequences in Common Lisp](https://www.lrde.epita.fr/wiki/Publications/newton.16.els) and [Representing and Computing with Types in Dynamically Typed Languages](https://www.lrde.epita.fr/wiki/Publications/newton.18.phd)

An important aspect of this implementation is that a regular type
expression pattern is represented internally (after compilation with
`rte-compile`) as a symbolic finite automaton.

<img src="img/symbolic-finite-automaton.png" alt="Symbolic Finite Automaton" width="300"/>

This means that the
time complexity of matching a sequence, `rte-execute`, against a
pattern is `O(n)` where `n` is the length of the sequence.  I.e., the
time to perform the match is not a function of the complexity of the
pattern; it is only a function of the sequence length.  On the
contrary, the time to compile the pattern depends on the complexity of
the pattern, and may be exponential in worst case.  For this reason,
the programmer is encouraged to arrange that expressions be compiled
either once, and used many times, or if possible to compile the
expressions at compile-time/load-time via:

```clojure
(def *my-rte* (rte-compile ...))
```

## Installation

Download from [LRDE](https://www.lrde.epita.fr/wiki/Home) GitLab 

```
git clone git@gitlab.lrde.epita.fr:jnewton/clojure-rte.git)
```
 or
```
git clone https://gitlab.lrde.epita.fr/jnewton/clojure-rte.git
```

## Usage

RTE allows the Clojure programmer to specify regular patterns of types
in sequences.  The pattern language resembles that of regular
expressions for strings, so the user very quickly has an intuition of
how it works.

Whereas with string regular expressions, the leaf level thing you
specify is a character such as `"a*b+"` which means a string of
characters consisting of 0 or more `'a'` characters followed by 1 or
more `'b'` characters; with regular type expressions you specify
type-designators, and repetition information about those types.
Rather than using post-fix notation we use lisp-friendly prefix
notation.  `(:* String)` means a sequence of objects consisting of 0
or more objects of type `String`.  The concept of *followed-by* is made
explicit by the `:cat` operator such as: `(:cat (:* Long) (:+String))`
a sequence consisting of 0 or more objects of type `Long`
followed (in the same sequence) by one or more objects of type `String`.

Which kinds of type designators can be used?  You may use 

- Any type name which is accepted by the `isa?` function.
- Any name which is registered in `*rte-known*`.

The dynamic variable `*rte-known*` is intended for applications to
extend but comes equipped with several useful *quasi-types*. For example,
`integer?` means `(:or Integer
                   Long
                   clojure.lang.BigInt
                   BigInteger
                   Short
                   Byte)`, and `int?` means `(:or Long Integer Short Byte)`.


## Options

RTE supports the following keywords `:cat`, `:+`, `:*`, `:?`, `:exp`, `:and`, `:or`, `:permute`, 
`:empty-set`, `:sigma`, `:epsilon`, and `:not`.


* `(:cat ...)` --- Takes 0 or more operands.  Matches a sequence of patterns.

Example 

```clojure
(let [rte (rte-compile '(:cat integer? String))]
  (rte-execute rte [1 "hello"]) ;; true
  (rte-execute rte [1.0 "hello"]) ;; false
  (rte-execute rte [1 2]) ;; false
  (rte-execute rte [1 2 "hello"]) ;; false
  )
```

* `(:+ ...)` --- Takes exactly one operand.  Matches 1 or more times.

Example 

```clojure
(let [rte (rte-compile '(:+ integer?))]
  (rte-execute rte [1 2 3 4 5]) ;; true
  (rte-execute rte [1]) ;; true
  (rte-execute rte []) ;; false
  (rte-execute rte [1.0 2 3.0 4 5]) ;; false
  )
```

* `(:* ...)` --- Takes exactly one operand.  Matches 0 or more times.

Example 

```clojure
(let [rte (rte-compile '(:* integer?))]
  (rte-execute rte [1 2 3 4 5]) ;; true
  (rte-execute rte [1]) ;; true
  (rte-execute rte []) ;; true
  (rte-execute rte [1.0 2 3.0 4 5]) ;; false
  )
```

* `(:? ...)` --- Takes exactly one operand. Matches 0 or 1 time.

Example 

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

* `(:exp (n ...))` --- This has a syntax different than the rest.  `:exp` takes exactly one argument which is a parenthesized pair, indicating the number of repititons, and a pattern.

Example --- to match a sequence 0 to 5 Integers,

```clojure
(let [rte (rte-compile '(:exp (5 (:? integer?))))]
   (rte-execute rte []) ;; true
   (rte-execute rte [1 2 3]) ;; true
   (rte-execute rte [1 2 3 3 4 5 6]) ;; false
   )
```

* `(:and ...)` ---  Takes 0 or more operands.  Simultaneously matches all of the given patterns.

Example ---  Keyword followed by 1 or two integers, repeated any number of times which is a multiple of 3 total items.

```clojure
(let [rte (rte-compile '(:and (:* (:cat Keyword integer? (:? integer?)))
                              (:+ (:sigma :sigma :sigma))))]
  (rte-execute rte [:x 1 :x 2 :x 3]) ;; true
  (rte-execute rte [:x 1 2 :y 2 3]) ;; true
  (rte-execute rte [:x 1 :y 3]) ;; false
  )
```


* `(:or ...)` --- matches any of the given patterns.

Example  ---  Takes 0 or more operands.  Either 0 or more integers, or 1 or more strings.

```clojure
(let [rte (rte-compile '(:or (:* integer?) (:+ String)))]
  (rex-execute rte []) ;; true, 0 integers
  (rex-execute rte [1 2 3]) ;; true
  (rex-execute rte ["hello" "world"]) ;; true one or more strings
  )
```


* `(:permute ...)` ---  Takes 0 or more operands.  Matches a sequence in any order.

Example --- two integers and a string in any order.

```clojure
(let [rte (rte-compile '(:permute integer? integer? String))]
  (rex-execute rte [1 2 "hello"]) ;; true
  (rex-execute rte [1 "hello" 2]) ;; true
  (rex-execute rte ["hello" 1 2]) ;; true
  (rex-execute rte ["hello" 2]) ;; false
  (rex-execute rte [1 2]) ;; false
  )
```


* `:empty-set` --- identity for `:or`.

Example any number of integers or strings in any order.

```clojure
(let [rte (rte-compile '(:* (:or integer? String)))]
  (rex-execute rte [1]) ;; true
  (rex-execute rte [1 "hello"]) ;; true
  (rex-execute rte ["hello" "world" 1 2 "hello" 3 "world"]) ;; true

```



* `:sigma` --- matches anything once, identity for `:and`.

Example -- any number of repetitions of integer anything String.

```clojure
(let [rte (rte-compile '(:* (:cat integer? :sigma String)))]
  (rex-execute rte []) ;; true
  (rex-execute rte [1]) ;; false
  (rex-execute rte [1 2]) ;; false
  (rex-execute rte [1 "hello" 2]) ;; true
  (rex-execute rte [1 "hello" 2 3 "world" 4 1 "hello" 2 3 "world" 4]) ;; true
  )
```

* `:epsilon` --- matching nothing once, identity for `:cat`.  This is probably
not useful to the end user.  However, internally `(:? x)` expands to `(:or x :epsilon)`.

* `(:not ...)` --- Takes exactly one operand.  Matches any sequence
except ones which match the pattern.  This can be confusing.

Example -- `String` matches a singleton sequence whose element is a string.  So `(:not String)` matches any sequence except one of length 1 consisting of a string, including matching the empty sequence.

```clojure
(rte-match '(:not String] []) ;; true
(rte-match '(:not String] ["hello"]) ;; false
(rte-match '(:not String] ["hello" "world"]) ;; true
```

If you want to match a sequence like  `[:x 100 :y 200 :z 300]`  but not if any of the values after the keyword is a String, use the following.
`(rte-match '(:* (:cat Keyword (:and :sigma (:not String)))) ...)`, because `(:and :sigma (:not String))` will match any singleton sequence whose element is NOT a string.


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

## Algebra of RTEs

Given two RTEs it is possible to ask questions of habitation and
vacuity.  For example, given two patterns, one might ask, is there a
sequence which matches neither of the patterns?  Or to ask whether
every sequence which matches one also matches the other.

* `rte-trace` takes a compiled RTE, return value of `rte-compile` and
  returns a sequence of type designators indicating the values of a
  sequence which will satisfy the original pattern.

```clojure
(rte-trace (rte-compile '(:and (:cat :sigma :sigma)
                               (:* integer?))))

==> [Byte Byte]

(rte-trace (rte-compile '(:and (:cat :sigma :sigma)
                               (:* (:and :sigma (:not Byte)))
                               (:* integer?))))

==> [Long Long]
```

* If you have two patterns, and you'd like to know what sequence matches one but not the other.
For example to find a sequence which contains 1 or more `integer?` but does not contain 0 or more `Number`.

```clojure
(let [pattern1 '(:+ integer?)
      pattern2 '(:* Number)]
  (rte-trace (rte-compile `(:and ~pattern1 (:not ~pattern2)))))

==> [Byte]
```

Another example.  `(:* (:cat keyword? :sigma))` matches a sequence of alternating `Keyword` anything pairs such as `[:x 100 :y "hello"]`. `(:cat (:* :sigma) keyword? (:* :sigma))` matches any sequence which contains at least one `Keyword` somewhere.   So does there exist a sequence which matches the first but not the secone?

```clojure
(let [pattern1' (:* (:cat keyword? :sigma))
      pattern2 '(:cat (:* :sigma) keyword? (:* :sigma))]
  (rte-trace (rte-compile `(:and ~pattern1 ~pattern2))))

==> []
```

We see that indeed the empty sequence matches `pattern1` but does not match `pattern2`.


## Not yet implemented

There are several important extensions we would like to implement.

1. The `:rte` keyword such as `(:rte (:* integer?))` which means a singleton sequence whose element is a sequence of integers.  This keyword is used to designate hierarchical structure.  `(:* (:rte (:* integer?)))` means a sequence of 0 or more sequences whose elements are integers. `(:* (:or (:rte (:* String)) (:rte (:* integer?))))` is a sequence of sequences each of which contains only strings or only Integers, e.g., 
```clojure
[[1 2 3]
 ["hello" "world"]
 [10 20 30 40 50]]
```
but not
```clojure
[[1 2 3]
 ["hello" "world"]
 [10 "20" 30 "40" 50]]
```

2. Internal to the RTE code we have implemented a type designator
  notation (a DSL) inspired by that of Common Lisp.  In Common Lisp they are
  referred to as [type
  specifiers](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node44.html).
  We would like to make these *type designators* part of the public interface to RTE.
  In order to do so we must fully implement, test, and document the DSL.
  
  A *type designator* is defined by the following recursive definition.  If `A` and `B` are type designators and `f` is a symbol whose global value `(resolve 'f)` is a unary predicate function,
  then

  - Any symbol designates a type, provided it can be resolved with the function `resolve`, and the resulting value is true according to the `class?` predicate.  I.e., if the predicate `(fn [x] (and (symbol? x) (resolve x) (class? (resolve x))))` is returns true.

  - `(and A B)` is a type designator, designating the set of values which are simultaneously of type `A` and `B`. `(and ...)` may have arbitrarily many operands. `(and A)` means `A`, and `(and)` means the empty set of all possible.

  - `(or A B)` is a type designator, designating the set of values which are of type `A` or of type `B`, or perhaps of both. `(or ...)` may have arbitrarily many operands.  `(or A)` means `A`, and `(or)` means the empty set of values.

  - `(not A)` is a type designating, designating the set of values which are *not* of type `A`.

  - `(satisfies f)` is a type designator, designating the set of values, `x` for which `(f x)` returns Boolean *true*.  It is assumed that `f` may be called with any value, always returns, and has no side effects.

  - `(= x)`  is a type designator, designating the set of all values which are equal `=` to its literal operand.  For example `(= 42)` is the set of all values equal to 42, which include among others the `java.lang.Long 42`, the `java.lang.Short 42`, and the  `java.lang.Byte 42`.

  - `(member x y z ...)` is a type designator equivalent to `(or (= x) (= y) (= z) ...)`.
  
## Contributors

[Jim Newton](https://www.lrde.epita.fr/wiki/User:Jnewton)

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

<!--  LocalWords:  Clojure LRDE GitLab gitlab https rte src img RTEs
 -->
<!--  LocalWords:  DSL
 -->
