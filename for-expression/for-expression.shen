\* for-expression.shen --- scala-like 'for expressions' for shen

Copyright (C) 2013, Kjetil S. Matheussen

*** License:

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

 - Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

 - Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



About:
======

'for' is a macro that provides scala-like 'for expressions'.
http://www.scala-lang.org/node/111 (also known as 'for comprehensions')



Example 1:
==========

The last element is returned.

(for 1)

=> 1



Example 2:
==========

A generator.

(for A <- [1 2]
     A)

=> [1 2]



Example 3:
==========

Two generators.

(for A <- [1 2]
     B <- [a b]
     [A B])

=> [[1 a] [1 b] [2 a] [2 b]]



Example 4:
==========

A guard.

(for A <- [1 2]
     B <- [a b]
     if (= A 1)
     [A B])
=> [[1 a] [1 b]]



Example 5:
==========

Similarly to example 4, this example uses
two expressions and a guard. But
by moving the guard one step up, the
expression computes slightly more efficiently.
The result is the same.

(for A <- [1 2]
     if (= A 1)
     B <- [a b]
     [A B])
=> [[1 a] [1 b]]




Example 6:
==========

Local variable

(for A <- [1 2]
     B <- [a b]
     C = [A B]
     C)
=> [[1 a] [1 b] [2 a] [2 b]]


Example 6:
==========

Local variable with pattern matching

(for A <- [1 2]
     B <- [a b]
     [C D] = [A B]
     C)
=> [1 1 2 2]


Example 7: 
==========

Pattern matching.

(for [A B] <- [[1 2][3 4]]
     (* A B))
=> [2 12]


Example 8:
==========

Pattern matching with strings.
(Pattern matching also works with tuples
and vectors)

(for (@s A B) <- ["ab" "cd" "de"]
     [A B])
=>
[["a" "b"] ["c" "d"] ["d" "e"]]

*\


(load "../pattern-matching/pattern-matching.shen")


(package for-expression [for create-pattern-matching-variables]


(define create-for-expression
  [A]                        []    -> A
  [A]                        Empty -> [cons A Empty]
  [Matcher Arrow Value | As] Empty -> (let RecFunc (gensym (protect RecFunc))
                                           Rest    (gensym (protect Rest))
                                           [let RecFunc [/. RecFunc Rest
                                                            [if [empty? Rest]
                                                                Empty
                                                                (append [let] (create-pattern-matching-variables Matcher [head Rest])
                                                                        [(create-for-expression As [RecFunc RecFunc [tail Rest]])])]]
                                             [RecFunc RecFunc Value]])
  where (= Arrow <-)
  [Matcher Eq Value | As]    Empty -> (append [let] (create-pattern-matching-variables Matcher Value)
                                              [(create-for-expression As Empty)])
  where (= Eq =)
  [if Test | As]             Empty -> [if Test
                                          (create-for-expression As Empty)
                                          Empty]
  As                         _     -> (do (print As)
                                          (print " - ")
                                          (error "for expression: syntax error")))

(defmacro for-macro
  [for | Rest] -> (create-for-expression Rest []))

)
