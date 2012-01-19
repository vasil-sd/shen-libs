\* string.shen --- String utilities for shen

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

This library implements a number of common string utilities.

*** Code: *\
(trap-error
 (require sequence)
 (/. E (load "../sequence/sequence.shen")))

(package string- [takestr dropstr substr length-str index-str
                  reverse-str starts-with substr? replace-str
                  join split trim-left trim-right chomp trim
                  \* symbols included from sequence *\
                  take drop take-while drop-while range flatten
                  filter complement seperate zip indexed reduce
                  mapcon partition partition-with unique frequencies
                  shuffle pick remove-first interpose subset?
                  cartesian-product]

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
  S TS -> (reduce (/.  ACC  (@s ACC S)) "" (interpose S TS)))

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
