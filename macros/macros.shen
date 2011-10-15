\* macros.shen --- collection of simple macros for various purposes

Copyright (C) 2011  Eric Schulte

*** License:

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*** Commentary:

*** Code: *\
(trap-error
 (require string)
 (require sequence)
 (/. E
     (load "../string/string.shen")
     (load "../sequence/sequence.shen")))

\*** Documentation Strings ***\
(defmacro def-macro
  \* function definition with documentation strings *\
  [def Name Docstring | Body] ->
    [package null []
      [set *functions*
            [adjoin [@p Name Docstring]
                    [trap-error [value *functions*]
                                [/. _ [set *functions* []]]]]]
      [define | [Name | Body]]])

(define apropos
  {string --> [(@p symbol string)]}
  \* Return a list of functions matching a search term *\
  Str -> (filter (/. Pair (or (substr? Str (make-string "~S" (fst Pair)))
                              (substr? Str (snd Pair))))
                 (value *functions*)))

(define tassoc
  \* Like `assoc' for lists of tuples. *\
  _ [] -> []
  X [A|AS] -> (if (= X (fst A)) A (tassoc X AS)))

(define documentation
  \* Return the documentation string for a function *\
  Func -> (let Doc (tassoc Func (value *functions*))
            (if (tuple? Doc)
                (snd Doc)
                (make-string "Documentation for `~S' not found" Func))))

\*** Pattern matching in anonymous lambdas ***\
(defmacro l-macro
  \* Thanks to vasil from the Qi-lang mailing list for this function definition *\
  [/.* | PatternsActions] ->
  (let TmpName (intern (str (gensym tmpname)))
       DBody   (tl (tl (compile shen-<define> [ TmpName | PatternsActions])))
    [/. | (append (hd DBody) (tl DBody))]))

\* For extended comments in source code *\
(defmacro comment-macro
  [comment | _] -> []
  [comment   _] -> [])
