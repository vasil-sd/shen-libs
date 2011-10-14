\* generation.shen --- functions for graph generation

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
