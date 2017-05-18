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

  person-name : (person --> string)
  person-last-name : (person --> string)
  person-gender : (person --> symbol)
  person-age: (person --> number)

*\

(package defstruct [defstruct]

(datatype struct-type
  X : symbol; Y : symbol;
  =======================
  [X Y] : slot;

  X : slot;
  __________________
  (head X) : symbol;)

(define char-upcase
  {string --> string}
  "a" -> "A"
  "b" -> "B"
  "c" -> "C"
  "d" -> "D"
  "e" -> "E"
  "f" -> "F"
  "g" -> "G"
  "h" -> "H"
  "i" -> "I"
  "j" -> "J"
  "k" -> "K"
  "l" -> "L"
  "m" -> "M"
  "n" -> "N"
  "o" -> "O"
  "p" -> "P"
  "q" -> "Q"
  "r" -> "R"
  "s" -> "S"
  "t" -> "T"
  "u" -> "U"
  "v" -> "V"
  "w" -> "W"
  "x" -> "X"
  "y" -> "Y"
  "z" -> "Z"
  S -> S)

(define string-capitalize
  {string --> string}
  (@s C S) -> (@s (char-upcase C) S)
  S -> S)

(define sym-capitalize
  {symbol --> symbol}
  X -> (intern (string-capitalize (str X))))

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
                  N (intern (cn (str Type) (cn "-" (str Sname))))
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
  Name Slots -> (error "Structure name must be a symbol.")
                where (not (symbol? Name))
  Name [] -> (error "At least one slot must be defined in a structure.")
  Name Slots -> (append [(datatypes Name Slots) (constructor Name Slots)]
                        (setters Name Slots 1 [])
                        (accessors Name Slots 1 [])))

(defmacro defstruct-macro
  [defstruct Name | Slots] -> [package null [] | (struct-aux Name Slots)]))
