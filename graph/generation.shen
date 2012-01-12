\* generation.shen --- functions for graph generation

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
(load "./graph.shen")
(load "../sequence/sequence.shen")
(load "../macros/macros.shen")

(define preferential-attachment
  {number --> number --> graph}
  N M -> (snd (reduce
               (/.* X (@p Ds (@p Vs Es)) ->
                    (let Pnts (map (lambda X [X (pick Ds)]) (range 0 M))
                      (@p (append (flatten Pnts) Ds)
                          (@p [X|Vs] (append Pnts Es)))))
               (@p [0] (@p [0] [])) (range 1 (- N 1)))))

(define simple-preferential-attachment
  {number --> graph}
  N -> (preferential-attachment N 1))

(define erdos-reni
  {number --> number --> graph}
  N Prb -> (let Verts (range 0 N)
                Edges (lambda X (map (lambda Y
                                       (if (< (/ (random 100) 100) Prb)
                                           [X Y] [])) Verts))
             (reduce (/.* X (@p Vs Es) -> (@p Vs (append (Edges X) Es)))
                     (@p Verts []) (range 1 (- N 1)))))
