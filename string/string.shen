\* string.shen --- String utilities for shen

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

This library implements a number of common string utilities.

*** Code: *\
(trap-error
 (require sequence)
 (/. E (load "../sequence/sequence.shen")))

(package string- [takestr dropstr substr length-str index-str
                  reverse-str starts-with substr? replace-str
                  join split trim-left trim-right chomp trim]

(define takestr
  \* return the n-length prefix of string *\
  {number --> string --> string}
  _ "" -> ""
  0 _  -> ""
  N (@s S SS) -> (@s S (takestr (- N 1) SS)))

(define dropstr
  \* drop the n-length prefix of string *\
  {number --> string --> string}
  _ "" -> ""
  0 S -> S
  N (@s S SS) -> (dropstr (- N 1) SS))

(define substr
  \* Return a substring of a string. *\
  {string --> number --> number --> string}
  String Start End -> (dropstr Start (takestr End String)))

(define length-str
  \* return the length of a string *\
  {string --> number}
  "" -> 0
  (@s _ SS) -> (+ 1 (length-str SS)))

(define index-str
  \* return the index of arg2 in arg1 or -1 if it doesn't appear *\
  {string --> string --> number}
  "" _ -> 0
  _ "" -> -1
  (@s T TS) S -> (if (= T S) 0 (+ 1 (index-str TS S))))

(define reverse-str
  {string --> string}
  "" -> ""
  (@s C RST) -> (@s (reversestr RST) C))

(define starts-with
  \* check if the first string starts with the second *\
  {string --> string --> boolean}
  "" _ -> true
  _ "" -> false
  (@s T TS) (@s S SS) -> (if (= S T) (starts-with SS TS) false))

(define substr?
  \* check if the second argument is a substring of the first *\
  {string --> string --> boolean}
  "" _ -> true
  _ "" -> false
  S T -> (if (starts-with S T) true (substr? S (tlstr T))))

(define replace-str
  \* replace all instance of arg1 with arg2 in arg3 *\
  {string --> string --> string --> string}
  _ _ "" -> ""
  S R T -> (if (starts-with T S)
               (@s R (dropstr (length-str S) T))
               (@s (pos T 0) (replace-str S R (tlstr T)))))

(define join
  \* join a list of strings with a string seperator *\
  {string --> [string] --> string}
  S TS -> (reduce (/. S ACC (@s ACC S)) "" (interpose S TS)))

(define split-
  {string --> string --> string --> [string] --> [string]}
  _ "" Holder Acc -> (append (reverse Acc) [Holder])
  S T  Holder Acc -> (if (starts-with T S)
                         (split- S (dropstr (length-str S) T) "" [Holder|Acc])
                         (split- S (tlstr T) (@s Holder (pos T 0)) Acc)))

(define split
  \* split a string on every occurance of a substring *\
  {string --> string --> [string]}
  S T -> (split- S T "" []))

(define trim-left
  \* remove all trailing and leading whitespace from a string *\
  {string --> string}
  (@s S STR) -> (trim-left STR) where (or (= S " ") (= S "\n"))
  STR -> STR)

(define trim-right
  \* remove all trailing whitespace from a string *\
  {string --> string}
  STR -> (reverse-str (trim-left (reverse-str STR))))

(define chomp
  \* remove all trailing whitespace from a string *\
  {string --> string}
  STR -> (reverse-str (trim-left (reverse-str STR))))

(define trim
  \* remove all trailing and leading whitespace from a string *\
  {string --> string}
  STR -> (trim-left (trim-right STR)))

)
