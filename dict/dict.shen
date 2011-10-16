\* dict.shen --- an ordered dictionary implementation

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

(1-) (set *dict* (dict))
<dictionary [] <fail! fail! fail! fail! fail! fail! ... etc>>

(2-) (dict? (value *dict*))
true

(3-) (dict-> (value *dict*) foo bar)
bar

(4-) (<-dict (value *dict*) foo)
bar

(5-) (dict-> (value *dict*) bar foo)
foo

(6-) (dict-> (value *dict*) bar baz)
baz

(7-) (contents (value *dict*))
[(@p foo bar) (@p bar baz)]

(8-) (keys (value *dict*))
[foo bar]

(9-) (vals (value *dict*))
[bar baz]

*** Code: *\
(package dict- [dict? dict dict-> <-dict contents key? keys vals dictionary]

(datatype dictionary
  Indexes : (list number);
  Store : (vector (list (@p A B)));
  ======================
  (absvector symbol Keys Store) : dictionary;)

(define dict?
  {A --> boolean}
  X -> (trap-error (= dictionary (<-address X 0)) (/. E false)))

(define make-dict
  {number --> dictionary}
  N -> (let Dict (absvector 3)
         (do (address-> Dict 0 dictionary)
             (address-> Dict 1 [])
             (address-> Dict 2 (vector N))
             Dict)))

(defmacro dict-macro
  [dict] -> [make-dict 1024]
  [dict N] -> [make-dict N])

(define indexes
  {dictionary --> (list number)}
  X -> (<-address X 1))

(define store
  {dictionary --> (list (@p A B))}
  X -> (<-address X 2))

(define update-entry
  {A --> B --> (list (@p A B)) --> (list (@p A B))}
  Key Val [] -> [(@p Key Val)]
  Key Val [(@p Key _) | Entry] -> [(@p Key Val) | Entry]
  Key Val [Z | Entry] -> [Z | (update-entry Key Val Entry)])

(define dict->
  {dictionary --> A --> B --> B}
  Dict Key Val ->
    (let Vector (store Dict)
         N (hash Key (limit Vector))
         Entry (trap-error (<-vector Vector N) (/. E []))
      (do (vector-> Vector N (update-entry Key Val Entry))
          (vector-> Dict 1 (adjoin N (<-vector Dict 1)))
          Val)))

(define tassoc
  \* Like `assoc' for lists of tuples. *\
  _ [] -> []
  X [A|AS] -> (if (= X (fst A)) A (tassoc X AS)))

(define <-dict
  {dictionary --> A --> B}
  Dict Key -> (let Vector (store Dict)
                   Entry (trap-error (<-vector Vector (hash Key (limit Vector)))
                                     (/. E (error "key not found~%")))
                (snd (tassoc Key Entry))))

(define contents
  {dictionary --> (list (@p A B))}
  Dict -> (contents- (reverse (indexes Dict)) (store Dict)))

(define contents-
  {(list number) --> (vector (list (@p A B))) --> (list (@p A B))}
  [] _ -> []
  [I|IS] Store -> (append (<-vector Store I) (contents- IS Store)))

(define keys
  {dictionary --> (list A)}
  Dict -> (map (function fst) (contents Dict)))

(define vals
  {dictionary --> (list B)}
  Dict -> (map (function snd) (contents Dict)))

(define key?
  {dictionary --> A --> boolean}
  Dict Key -> (let Store (store Dict)
                (trap-error (do (<-vector Store (hash Key (limit Store))) true)
                            (/. E false))))

)
