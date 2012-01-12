\* macros.shen --- collection of simple macros for various purposes

Copyright (C) 2011,  Eric Schulte

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

*** Commentary:

*** Code: *\
(trap-error
 (do (require string)
     (require sequence))
 (/. E
     (do (load "../string/string.shen")
         (load "../sequence/sequence.shen"))))

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
