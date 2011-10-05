\* Copyright 2010-2011 Ramil Farkhshatov

defstruct is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

defstruct is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with defstruct.  If not, see <http://www.gnu.org/licenses/>.


Description
===========
defstruct allows defining typed records stored for efficiency as vectors.

The following example

  (defstruct person
    (name string)
    (last-name string)
    (gender symbol)
    (age number))

defines a constructor:

  mk-person : (string --> string --> symbol --> number --> person)

setters:

  person-name-> : (person --> string --> person)
  person-last-name-> : (person --> string --> person)
  person-gender-> : (person --> symbol --> person)
  person-age-> : (person --> number --> person)

accessors:

  <-person-name : (person --> string)
  <-person-last-name : (person --> string)
  <-person-gender : (person --> symbol)
  <-person-age: (person --> number)

*\

(package defstruct- [defstruct]

(datatype struct-type
  X : symbol; Y : symbol;
  =======================
  [X Y] : slot;
  
  X : slot;
  __________________
  (head X) : symbol;)

(define capitalize
  {string --> string}
  (@s "a" S) -> (@s "A" S)
  (@s "b" S) -> (@s "B" S)
  (@s "c" S) -> (@s "C" S)
  (@s "d" S) -> (@s "D" S)
  (@s "e" S) -> (@s "E" S)
  (@s "f" S) -> (@s "F" S)
  (@s "g" S) -> (@s "G" S)
  (@s "h" S) -> (@s "H" S)
  (@s "i" S) -> (@s "I" S)
  (@s "j" S) -> (@s "J" S)
  (@s "k" S) -> (@s "K" S)
  (@s "l" S) -> (@s "L" S)
  (@s "m" S) -> (@s "M" S)
  (@s "n" S) -> (@s "N" S)
  (@s "o" S) -> (@s "O" S)
  (@s "p" S) -> (@s "P" S)
  (@s "q" S) -> (@s "Q" S)
  (@s "r" S) -> (@s "R" S)
  (@s "s" S) -> (@s "S" S)
  (@s "t" S) -> (@s "T" S)
  (@s "u" S) -> (@s "U" S)
  (@s "v" S) -> (@s "V" S)
  (@s "w" S) -> (@s "W" S)
  (@s "x" S) -> (@s "X" S)
  (@s "y" S) -> (@s "Y" S)
  (@s "z" S) -> (@s "Z" S)
  S -> S)

(define sym-capitalize
  {symbol --> symbol}
  X -> (intern (capitalize (str X))))

(define slot-type
  Type I Stype Acc -> (let A (intern "A")
                           B (intern "B")
                        [A : Type;
                         _______________________
                         [<-vector A I] : Stype;
                           
                         A : Type; B : Stype;
                         _______________________
                         [vector-> A I B] : Type;
                         | Acc]))

(define slots-types
  _ [] _ Acc -> Acc
  Type [[Sname Stype] | Slots] N Acc ->
  (slots-types Type Slots (+ N 1) (slot-type Type N Stype Acc)))

(define slots-defs
  {(list slot) --> (list symbol) --> (list symbol)}
  [] Acc -> Acc
  [[Name Type] | Slots] Acc -> (slots-defs
                                 Slots
                                 [(sym-capitalize Name) : Type; | Acc]))

(define slots-datatype-names
  {(list slot) --> (list symbol) --> (list symbol)}
  [] Acc -> (reverse Acc)
  [[Name Type] | Slots] Acc -> (slots-datatype-names
                                 Slots
                                 [(sym-capitalize Name) | Acc]))

(define datatypes
  Type Slots -> (let Names (map (/. X (sym-capitalize (head X))) Slots)
                     Defs (slots-defs Slots [])
                     Types (slots-types Type Slots 1 [])
                  (append [datatype Type] 
                          Defs
                          [_________
                            (append [@v | Names] [[vector 0]]) : Type;
                            | Types])))

(define accessors
  _ [] _ Acc -> Acc
  Type [[Sname Stype] | Slots] I Acc ->
  (accessors Type
             Slots
             (+ I 1)
             (let X (intern "X")
                  N (intern (cn "<-" (cn (str Type) (cn "-" (str Sname)))))
               [[define N
                  { Type --> Stype }
                  X -> [<-vector X I]]
                | Acc])))

(define setters
  _ [] _ Acc -> Acc
  Type [[Sname Stype] | Slots] I Acc ->
  (setters Type
           Slots
           (+ I 1)
           (let X (intern "X")
                Y (intern "Y")
             [[define (intern (cn (str Type) (cn "-" (cn (str Sname) "->"))))
                { Type --> Stype --> Type }
                X Y -> [vector-> X I Y]]
              | Acc])))

(define constructor-type
  {symbol --> (list slot) --> ast}
  Type [] Acc -> (reverse [} Type --> | Acc])
  Type [[Sname Stype] | Slots] [] -> (constructor-type Type Slots [Stype {])
  Type [[Sname Stype] | Slots] Acc -> 
    (constructor-type Type Slots [Stype --> | Acc]))

(define constr-init
  [] -> [vector 0]
  [N | Names] -> [@v N (constr-init Names)])

(define constructor
  Type Slots -> (let Types (constructor-type Type Slots [])
                     Names (map (/. X (sym-capitalize (head X))) Slots)
                     Init (constr-init Names)
                     Constr (intern (cn "mk-" (str Type)))
                  (append [define Constr] Types Names [-> Init])))

(define struct-aux
  Name Slots -> (append [(datatypes Name Slots) (constructor Name Slots)]
                        (setters Name Slots 1 [])
                        (accessors Name Slots 1 [])))

(defmacro defstruct-macro
  [defstruct Name | Slots] -> [package null [] | (struct-aux Name Slots)]))
