\* sequence.shen --- Sequence utilities for shen

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

(define take
  \* take and return N elements from the front of a list *\
  {number --> (list A) --> (list A)}
  _ [] -> []
  0 _  -> []
  N [A|AS] -> [A|(take (- N 1) AS)])

(define drop
  \* drop N elements from the front of a list *\
  {number --> (list A) --> (list A)}
  _ [] -> []
  0 AS -> AS
  N [A|AS] -> (drop (- N 1) AS))

(define take-while
  \* take elements of a list while they satisfy a function *\
  {(A --> boolean) --> (list A) --> (list A)}
  _ [] -> []
  Fn [A|AS] -> (if (Fn A) [A|(take-while Fn AS)] []))

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

(define flatten
  \* flatten a list of elements *\
  {nested --> nested}
  [] -> []
  [A|AS] -> (if (cons? A)
                (flatten (append A AS))
                [A|(flatten AS)]))

(define filter
  \* return those elements of a list which satisfy a given function *\
  {(A --> boolean) --> (list A) --> (list A)}
  _  [] -> []
  Fn [A|AS] -> (if (Fn A)
                   [A|(filter Fn AS)]
                   (filter Fn AS)))

(define complement
  \* return a function which is the complement of the given function *\
  {(A --> boolean) --> (A --> boolean)}
  Fn -> (/. A (not (Fn A))))

(define seperate
  \* seperate a list into those that do and don't satisfy a boolean function *\
  {(A --> boolean) --> (list A) --> ((list A) * (list A))}
  Fn AS -> (@p (filter Fn AS) (filter (complement Fn) AS)))

(define zip
  \* combine two lists returning a list of tuples of their elements *\
  {(list A) --> (list B) --> [(A * B)]}
  _ [] -> []
  [] _ -> []
  [A|AS] [B|BS] -> [(@p A B)|(zip AS BS)])

(define indexed-
  {number --> (list A) --> [(number * A)]}
  _ [] -> []
  N [A|AS] -> [(@p N A)|(indexed- (+ N 1) AS)])

(define indexed
  \* return an indexed version of a list *\
  {(list A) --> [(number * A)]}
  AS -> (indexed- 0 AS))

(define reduce
  \* reduce a functoin over a list *\
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
