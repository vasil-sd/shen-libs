\* sequence.shen --- Sequence utilities for shen
 *
 * Copyright (C) 2011  Eric Schulte
 * 
 *** License:
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GNU Emacs; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 * 
 *** Commentary:
 *
 * This library implements a number of sequence utilities commonly
 * found in functional languages.
 *
 *** Code:
 *\

(define take
  \* take and return N elements from the front of list [A] *\
  {number --> [A] --> [A]}
  _ [] -> []
  0 _  -> []
  N [A|AS] -> [A|(take (- N 1) AS)])

(define drop
  \* drop N elements from the front of list [A] *\
  {number --> [A] --> [A]}
  _ [] -> []
  0 AS -> AS
  N [A|AS] -> (drop (- N 1) AS))

(define take-while
  \* take elements of a list while they satisfy a function *\
  {(A --> boolean) --> [A] --> [A]}
  _ [] -> []
  Fn [A|AS] -> (if (Fn A) [A|(take-while Fn AS)] []))

(define drop-while
  \* drop elements of a list while they satisfy a function *\
  {(A --> boolean) --> [A] --> [A]}
  _ [] -> []
  Fn [A|AS] -> (if (Fn A) (drop-while Fn AS) [A|AS]))

(define range
  \* return a list of integers from X to Y *\
  {number --> number --> [number]}
  X X -> [X]
  X Y -> [X|(range (if (> X Y) (- X 1) (+ X 1)) Y)])

(define flatten
  \* flatten a list of elements *\
  {[A] --> [B]}
  [] -> []
  [A|AS] -> (if (cons? A)
                (flatten (append A AS))
                [A|(flatten AS)]))

(define filter
  \* return those elements of [A] which satisfy a given function *\
  {(A --> boolean) --> [A] --> [A]}
  _  [] -> []
  Fn [A|AS] -> (if (Fn A)
                   [A|(filter Fn AS)]
                   (filter Fn AS)))

(define complement
  \* return a function which is the complement of the given function *\
  {(A --> boolean) --> (A --> boolean)}
  Fn -> (/. A (not (Fn A))))

(define seperate
  \* return the tuple: (@p (filter Fn [A]) (filter (complement Fn) [A])) *\
  {(A --> boolean) --> [A] --> [[A]]}
  Fn AS -> (@p (filter Fn AS) (filter (complement Fn) AS)))

(define zip
  \* combine two lists returning a list of tuples of their elements *\
  {[A] --> [B] --> [(@p A B)]}
  _ [] -> []
  [] _ -> []
  [A|AS] [B|BS] -> [(@p A B)|(zip AS BS)])

(define indexed
  \* return an indexed version of a list *\
  {[A] --> [(@p number A)]}
  AS -> (indexed- 0 AS))

(define indexed-
  {number -> [A] -> [(@p number A)]}
  _ [] -> []
  N [A|AS] -> [(@p N A)|(indexed- (+ N 1) AS)])

(define reduce
  \* reduce a functoin over a list *\
  {(A --> B --> B) --> [A] --> B --> B}
  Fn B [A] -> (Fn A B)
  Fn B [A|AS] -> (reduce Fn (Fn A B) AS))

(define mapcon
  \* like map but concatenate the results *\
  {(A --> B) --> [A] --> [B]}
  _ [] -> []
  Fn [A|AS] -> (append (Fn A) (mapcon Fn AS)))

(define partition
  \* group the elements of a list breaking into sublists of size N *\
  {number --> [A] --> [[A]]}
  _ [] -> []
  N AS -> [(take N AS)|(partition N (drop N AS))])

(define partition-with
  \* partition into sublists every time a function returns a new value *\
  {(Fn --> A --> B) --> [[A]]}
  _ [] -> []
  Fn [A|AS] -> (let Touchstone (Fn A)
                    Head (cons A (take-while (/. X (= Touchstone (Fn X))) AS))
                 [Head|(partition-with Fn (drop (length Head) [A|AS]))]))

(define unique
  \* remove all duplicate elements from a list *\
  {[A] --> [A]}
  AS -> (unique- [] AS))

(define unique-
  {[A] --> [A] --> [A]}
  BS [] -> (reverse BS)
  BS [A|AS] -> (if (element? A BS)
                   (unique- BS AS)
                   (unique- [A|BS] AS)))

(define frequencies
  \* returns the number of occurences of each unique element in a list *\
  {[A] --> [(@p number A)]}
  AS -> (frequencies- [] AS))

(define frequencies-
  {[(@p number A)] --> [A] --> [(@p number A)]}
  ACC [] -> ACC
  ACC [A|AS] -> (frequencies- [(@p A (+ 1 (occurrences A AS)))|ACC] (remove A AS)))

(define shuffle
  \* return a random permutation of a list *\
  {[A] --> [A]}
  [] -> []
  AS -> (let Index (+ 1 (random (length AS)))
          [(nth Index AS)|(shuffle (append (take (- Index 1) AS)
                                           (drop Index AS)))]))

(define rand-elt
  \* return a random element of a list *\
  {[A] --> A}
  [] -> NIL
  AS -> (nth (+ 1 (random (length AS))) AS))

(define remove-first
  \* remove the first occurance of argument 1 in argument 2 *\
  {A --> [A]}
  _ [] -> []
  X [A|AS] -> (if (= X A) AS [A|(remove-first X AS)]))

(define interpose
  \* insert the first arg between every two elements of the second arg *\
  {A --> [A] --> [A]}
  _ [] -> []
  _ [A] -> [A]
  S [A|AS] -> [A S|(interpose S AS)])