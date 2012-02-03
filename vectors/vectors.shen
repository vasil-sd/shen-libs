\* vectors.shen --- Vector utilities for shen

Copyright (C) 2012,  Dmitry Cherkassov 

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

This library implements a number of vector utilities 

*** Code: *\

(package vectors- [vector-ref list->vector vector->list]

\*---------------------------------------------------------------------------------*\
(define vector-ref
\* returns n'th element of the vector and throws exception if (n > length) of list *\
  {(vector A) --> number --> A}
  V N -> (<-vector V N) where (>= (limit V) N)
  _ _ -> (error "Out of bounds exception"))

\*---------------------------------------------------------------------------------*\
(define list-vect-help
  {(list A) --> (vector A) --> number --> (vector A)}
  []    V _    -> V
  [A|B] V N -> (list-vect-help B (vector-> V N A) (+ N 1)))

(define list->vector
\* makes list->vector conversion *\
  {(list A) --> (vector A)}
  L -> (list-vect-help L (vector (length L)) 1))

\*---------------------------------------------------------------------------------*\
(define vector->list-help
  {(vector A) --> number --> number --> (list A) --> (list A)}
  _ End End Acc -> (reverse Acc)
  V I End Acc -> (vector->list-help V (+ I 1) End [(<-vector V I) | Acc]))

(define vector->list
  \* makes list->vector conversion *\
  {(vector A) --> (list A)}
  V -> (vector->list-help V 1 (+ 1 (limit V)) []))

)