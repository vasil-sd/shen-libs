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


'(create-pattern-matching-variables Pattern Value)' returns an optimized
list of variable names and values based on the provided 'Pattern'
and 'Value' arguments. The returned list can be used directly in
a shen 'let' block.

The function matches combinations of lists, tuples, vectors and strings.

Note that the same task could possibly also be achieved by using
the shen.<define> function, which is part of the shen parser.
But 'create-pattern-matching-variables' is documented.


Example 1:

  (create-pattern-matching-variables [cons A []]
                                     [cons a []]))
->
  [A (head (cons a NIL))]
   | |
   | |
   | +--- Value
   | 
   +--- Variable name


Example 2:

  (create-pattern-matching-variables [cons A [cons B C]]
                                     [cons a [cons b c]])
->
  [TempMatchVariable1168 (cons a (cons b c))
   A                     (head TempMatchVariable1168)
   TempMatchVariable1169 (tail TempMatchVariable1168)
   B                     (head TempMatchVariable1169)
   C                     (tail TempMatchVariable1169)]
   |                     |
   |                     |
   |                     +--- Values
   | 
   +--- Variable names




*\

(package pattern-matching [create-pattern-matching-variables]

(define create-cons-match-code
  M Ms HeadF TailF Value -> (let Try (append (create-pattern-matching-variables M [HeadF Value])
                                             (create-pattern-matching-variables Ms [TailF Value]))
                              (if (and (cons? Value)
                                       (> (length Try) 2))
                                  (let TempMatchVariable (gensym (protect TempMatchVariable))
                                    (append [TempMatchVariable Value]
                                            (create-pattern-matching-variables M [HeadF TempMatchVariable])
                                            (create-pattern-matching-variables Ms [TailF TempMatchVariable])))
                                  Try)))

(define create-pattern-matching-variables
  []              _  -> []
  [@v [vector 0]] _  -> []
  [cons M Ms] Value -> (create-cons-match-code M Ms head  tail  Value)
  [@p M Ms]   Value -> (create-cons-match-code M Ms fst   snd   Value)
  [@s M Ms]   Value -> (create-cons-match-code M Ms hdstr tlstr Value)
  [@v M Ms]   Value -> (create-cons-match-code M Ms hdv   tlv   Value)
  Var         _      -> [] where (= Var _)
  Var         Value -> [Var Value])

)


\*
Various Tests:

(map PPRINT
     (create-pattern-matching-variables [cons A []]
                                        [cons a []]))
(map PPRINT
     (create-pattern-matching-variables [cons A [cons B C]] 
                                        [cons a [cons b c]]))
(map PPRINT
     (create-pattern-matching-variables [cons A [cons B C]]
                                        hepp))

(map PPRINT
     (create-pattern-matching-variables [cons [@v A [@v B [@v [vector 0]]]] C]
                                        [cons [@v a b <>] c]))
(map PPRINT
     (create-pattern-matching-variables [cons [@s A [@s B1 B2]] C]
                                        [cons "ab gakkgakk" c]))
(map PPRINT
     (create-pattern-matching-variables [cons [@s A [@s B1 B2]] C] 
                                        [cons "" c]))
(map PPRINT
     (create-pattern-matching-variables [cons [@p A B] C]
                                        [cons [@p a b] c]))
(map PPRINT
     (create-pattern-matching-variables [cons [@p A [@p B1 B2]] C]
                                        [cons [@p a b1 b2] c]))
(map PPRINT
     (create-pattern-matching-variables [cons [@p [cons A1 A2] B] C]
                                        [cons [@p [cons a1 a2] b] c]))

(map PPRINT
     (create-pattern-matching-variables [cons A [cons [cons B []] _]]
                                        [cons a [cons [cons b []] _]]))

*\

