\* dict.shen --- an ordered dictionary implementation

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
(datatype dictionary
  Indexes : (list number);
  Store : (vector (list (@p A B)));
  ======================
  (absvector symbol Keys Store) : dictionary;)

(package dict- [dict? dict dict-> <-dict contents key? keys vals
                make-dict dictionary]

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
  _ [] -> (error "key not found~%")
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
                (trap-error
                 (do (tassoc Key (<-vector Store (hash Key (limit Store))))
                     true)
                 (/. E false))))

)
