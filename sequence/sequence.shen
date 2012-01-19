\* sequence.shen --- Sequence utilities for shen

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

This library implements a number of sequence utilities commonly
found in functional languages.

*** Code: *\
(datatype nested
  ____________
  A : nested;
  ___________________
  (list A) : nested;)

(package sequence- [take drop take-while drop-while range flatten
                    filter complement seperate zip indexed reduce
                    mapcon partition partition-with unique frequencies
                    shuffle pick remove-first interpose subset?
                    cartesian-product]

(define take-aux
  {number --> (list A) --> (list A) --> (list A)}
  _ [] Acc -> (reverse Acc)
  0 _ Acc -> (reverse Acc)
  N [A | Rest] Acc -> (take-aux (- N 1) Rest [A | Acc]))

(define take
  \* take and return N elements from the front of a list *\
  {number --> (list A) --> (list A)}
  N L -> (take-aux N L []))

(define drop
  \* drop N elements from the front of a list *\
  {number --> (list A) --> (list A)}
  _ [] -> []
  0 AS -> AS
  N [A|AS] -> (drop (- N 1) AS))

(define take-while-aux
  {(A --> boolean) --> (list A) --> (list A) --> (list A)}
  _ [] Acc -> (reverse Acc)
  Fn [A | Rest] Acc -> (take-while-aux Fn Rest [A | Acc]) where (Fn A)
  _ _ Acc -> (reverse Acc))

(define take-while
  \* take elements of a list while they satisfy a function *\
  {(A --> boolean) --> (list A) --> (list A)}
  Fn L -> (take-while-aux Fn L []))

(define drop-while
  \* drop elements of a list while they satisfy a function *\
  {(A --> boolean) --> (list A) --> (list A)}
  _ [] -> []
  Fn [A|AS] -> (if (Fn A) (drop-while Fn AS) [A|AS]))

(define range
  \* return a list of integers from X to Y *\
  {number --> number --> [number]}
  X X -> [X]
  X Y -> [X|(range (if (> X Y) (- X 1) (+ X 1)) Y)])

(define flatten-aux
  {nested --> (list A) --> (list A)}
  [] Acc -> Acc
  [X | Rest] Acc -> (flatten-aux Rest (flatten-aux X Acc)) where (cons? X)
  [X | Rest] Acc -> (flatten-aux Rest [X | Acc]))

(define flatten
  \* flatten a list of elements *\
  {nested --> (list A)}
  [] -> []
  L -> (reverse (flatten-aux L [])))

(define filter-aux
  {(A --> boolean) --> (list A) --> (list A) --> (list A)}
  Test [] Acc -> (reverse Acc)
  Test [X | Y] Acc -> (filter-aux Test Y [X | Acc]) where (Test X)
  Test [X | Y] Acc -> (filter-aux Test Y Acc))

(define filter
  \* return those elements of a list which satisfy a given function *\
  {(A --> boolean) --> (list A) --> (list A)}
  Test X -> (filter-aux Test X []))

(define complement
  \* return a function which is the complement of the given function *\
  {(A --> boolean) --> (A --> boolean)}
  Fn -> (/. A (not (Fn A))))

(define separate-aux
  {(A --> boolean) --> (list A) --> (list A) --> (list A)
    --> ((list A) * (list A))}
  Test [] A1 A2 -> (@p A1 A2)
  Test [A | Rest] A1 A2 -> (separate-aux Test Rest [A | A1] A2) where (Test A)
  Test [A | Rest] A1 A2 -> (separate-aux Test Rest A1 [A | A2]))

(define separate
  \* separate a list into those that do and don't satisfy a boolean function *\
  {(A --> boolean) --> (list A) --> ((list A) * (list A))}
  Test L -> (separate-aux Test L [] []))

(define zip-aux
  {(list A) --> (list B) --> (list (A * B)) --> (list (A * B))}
  _ [] Acc -> (reverse Acc)
  [] _ Acc -> (reverse Acc)
  [A | A-rest] [B | B-rest] Acc -> (zip-aux A-rest B-rest [(@p A B) | Acc]))

(define zip
  \* combine two lists returning a list of tuples of their elements *\
  {(list A) --> (list B) --> [(A * B)]}
  A B -> (zip-aux A B []))

(define indexed-
  {number --> (list A) --> (list (number * A)) --> (list (number * A))}
  _ [] Acc -> (reverse Acc)
  N [A | AS] Acc -> (indexed- (+ N 1) AS [(@p N A) | Acc]))

(define indexed
  \* return an indexed version of a list *\
  {(list A) --> (list (number * A))}
  AS -> (indexed- 0 AS []))

(define reduce
  \* reduce a function over a list *\
  {(A --> B --> B) --> B --> (list A) --> B}
  Fn B [A] -> (Fn A B)
  Fn B [A|AS] -> (reduce Fn (Fn A B) AS))

(define mapcon
  \* like map but concatenate the results *\
  {(A --> (list B)) --> (list A) --> (list B)}
  _ [] -> []
  Fn [A|AS] -> (append (Fn A) (mapcon Fn AS)))

(define partition
  \* group the elements of a list breaking into sublists of size N *\
  {number --> (list A) --> (list (list A))}
  _ [] -> []
  N AS -> [(take N AS)|(partition N (drop N AS))])

(define partition-with
  \* partition into sublists every time a function returns a new value *\
  {(Fn --> A --> B) --> (list A) -->  (list (list A))}
  _ [] -> []
  Fn [A|AS] -> (let Touchstone (Fn A)
                    Head (cons A (take-while (/. X (= Touchstone (Fn X))) AS))
                 [Head|(partition-with Fn (drop (length Head) [A|AS]))]))

(define unique-
  {(list A) --> (list A) --> (list A)}
  BS [] -> (reverse BS)
  BS [A|AS] -> (if (element? A BS)
                   (unique- BS AS)
                   (unique- [A|BS] AS)))

(define unique
  \* remove all duplicate elements from a list *\
  {(list A) --> (list A)}
  AS -> (unique- [] AS))

(define frequencies-
  {[(number * A)] --> (list A) --> [(number * A)]}
  ACC [] -> ACC
  ACC [A|AS] -> (frequencies- [(@p A (+ 1 (occurrences A AS)))|ACC] (remove A AS)))

(define frequencies
  \* returns the number of occurences of each unique element in a list *\
  {(list A) --> [(number * A)]}
  AS -> (frequencies- [] AS))

(define shuffle
  \* return a random permutation of a list *\
  {(list A) --> (list A)}
  [] -> []
  AS -> (let Index (+ 1 (random (length AS)))
          [(nth Index AS)|(shuffle (append (take (- Index 1) AS)
                                           (drop Index AS)))]))

(define pick
  \* return a random element of a list *\
  {(list A) --> A}
  AS -> (nth (+ 1 (random (length AS))) AS))

(define remove-first
  \* remove the first occurance of argument 1 in argument 2 *\
  {A --> (list A) --> (list A)}
  _ [] -> []
  X [A|AS] -> (if (= X A) AS [A|(remove-first X AS)]))

(define interpose
  \* insert the first arg between every two elements of the second arg *\
  {A --> (list A) --> (list A)}
  _ [] -> []
  _ [A] -> [A]
  S [A|AS] -> [A S|(interpose S AS)])

(define subset?
  \* check if arg1 is a subset of arg2 *\
  {(list A) --> (list A) --> boolean}
  [] _ -> true
  _ [] -> false
  [X|XS] YS -> (if (element? X YS)
                   (subset? XS YS)
                   false))

(define cartesian-product
  \* return the cartesian product of two lists *\
  {(list A) --> (list B) --> (list (A * B))}
  [] _ -> []
  [A|AS] BS -> (append (map (@p A) BS) (cartesian-product AS BS)))

)
