\* Copyright (c) 03-12-12,  Willi O Riha
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*\

(defmacro let-macro
  [let [@p X Y] Z | W]
		-> (let Var (gensym (protect V))
			  [let Var Z | (subst [snd Var] Y (subst [fst Var] X W))])
  [let [X | Y] Z W] 
		-> (let Z' (gensym (protect V)) \* changed 29-09-12 *\
				Z' Z
				(recursive-let-list 1 [X | Y] Z' W)))

(define recursive-let-list
  _ [] _ W -> W
  N [cons V Vs] Z' W -> [let V [nth N Z'] (recursive-let-list (+ N 1) Vs Z' W)])  

(defmacro trap-macro
  [t'rap Exp Str] -> [trap-error Exp [/. (protect E) [error [@s [error-to-string (protect E)] Str]]]]) 
  
(defmacro max-macro
  [max X] -> X
  [max W X Y | Z] -> [max W [max X Y | Z]])

(defmacro min-macro
  [min X] -> X
  [min W X Y | Z] -> [min W [min X Y | Z]])

(defmacro gcd-macro
  [gcd X] -> [abs X]
  [gcd W X Y | Z] -> [gcd W [gcd X Y | Z]])
   
(defmacro lcm-macro
  [lcm X] -> [abs X]
  [lcm W X Y | Z] -> [lcm W [lcm X Y | Z]])

(defmacro round-macro
  [round N] -> [maths-round0 N]
  [round N1 N2] -> [maths-round' N1 N2])

(declare sqrt [number --> number])

(defmacro string->int-macro
  [string->int Str] -> [string-oct-hex-dec->decimal Str]
  [string->int Str B] -> [radixB->decimal Str B])
