\* Copyright (c) 08-08-12,  Willi O Riha
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
			  [let Var Z | (subst [snd Var] Y (subst [fst Var] X W))]))

(defmacro max-macro
   [max M N ] -> [aux.max' M N]
   [max | Ns] -> [aux.maxL (cf Ns)])
  
(defmacro min-macro
   [min M N ] -> [aux.min' M N]
   [min | Ns] -> [aux.minL (cf Ns)])

(defmacro gcd-macro
   [gcd M N ] -> [maths.gcd' M N]
   [gcd | Ns] -> [maths.gcdL (cf Ns)])

(defmacro lcm-macro
   [lcm | Ns] -> [maths.lcmL (cf Ns)])
   
(define cf
   [] -> []
   [N | Ns] -> [cons N (cf Ns)]) 

(defmacro round-macro
   [round N] -> [maths.round0 N]
   [round N1 N2] -> [maths.round' N1 N2])

(declare sqrt [number --> number])

(defmacro string-list-macro
   string->list -> explode)

(defmacro strlen-macro
   strlen -> string-length)
   
(defmacro string->integer-macro
   [string->integer Str] -> [string-oct-hex-dec->decimal Str]
   [string->integer Str B] -> [radixB->decimal Str B])

