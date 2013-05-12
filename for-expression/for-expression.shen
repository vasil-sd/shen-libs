\* Copyright 2013 Kjetil Matheussen
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *


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
