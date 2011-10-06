\* regexp.shen --- regular expressions for shen
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
 * This library implements regular expressions for shen.  String regular
 * expressions are compiled to shen functions which accept a string argument
 * and return an re-state object.
 * 
 * See the bottom portion of this file for external functions which may be
 * used as an access point for compiling and using regular expressions.
 *
 * Some examples are included below.
 *
 * Character classes.
 * 
 *   (1-) (match-strings (re-search "[:digit:]+" "Lorem ipsum dolor sit, 26."))
 *   ["26"]
 * 
 *   (2-) (match-strings (re-search "\\d+" "Lorem ipsum dolor sit, 26."))
 *   ["26"]
 * 
 *   (3-) (match-strings (re-search "\\w+" "Lorem ipsum dolor sit amet, 26, mattis eget."))
 *   ["Lorem"]
 *   
 * Alternatives grouped with [...]'s.
 * 
 *   (4-) (match-strings (re-search "d[olr]+" "Lorem ipsum dolor sit, 26."))
 *   ["dolor"]
 * 
 * Nested regular expressions and alternatives with (...|...).
 * 
 *   (5-) (do-matches (/. X (hd (match-strings X))) "(ipsum|eget)"
 *                     "Lorem ipsum dolor sit amet, 26, mattis eget.")
 *   ["ipsum" "eget"]
 *
 *
 *** Code:
 *\
(load "../sequence/sequence.shen")
(load "../string/string.shen")

\*******************************************************************************
 * re-state holds the state and match data of regular expressions
 *\
(datatype re-state
    state : (@p string integer);
  matches : [integer];
  _________________________________
  (@p state matches) : re-state;)

(define state
  {re-state --> (@p string integer)}
  Re-state -> (fst Re-state))

(define index
  {re-state --> integer}
  (@p (@p _ Index) _) -> Index)

(define matches
  {re-state --> [(@p integer integer)]}
  Re-state -> (snd Re-state))

(define new-state
  {string --> re-state}
  String -> (@p (@p String 0) []))

(define next
  {re-state --> string}
  (@p (@p String Index) _) -> (trap-error (pos String Index) (/. _ eos)))

(define increment
  {re-state --> re-state}
  (@p (@p String Index) Matches) -> (@p (@p String (+ 1 Index)) Matches))

(define match-strings
  {re-state --> [strings]}
  false -> []
  (@p (@p S _) Ms) -> (map (/. M (substr S (nth 1 M) (nth 2 M)))
                           (partition 2 (reverse Ms))))

(define starting-at
  {re-state --> integer --> re-state}
  (@p (@p String _) Matches) Index -> (@p (@p String Index) Matches))

\*******************************************************************************
 * compilation of a regular expression from a string representation
 *\
(define re-or
  \* Create a disjunction of regular expressions *\
  []     -> (lambda _ false)
  [R|Rs] -> (lambda State
              (let Result (R State)
                (if (not (= false Result))
                    Result
                    ((re-or Rs) State)))))

(define re-and
  \* Create a conjunction of regular expressions *\
  []     -> (lambda X X)
  [R|Rs] -> (lambda State
              (let Result (R State)
                (if (not (= false Result)) 
                    ((re-and Rs) Result)
                    false))))

(define re-repeat
  \* Repeatedly apply a regular expression until it fails *\
  R -> (lambda State
         (if (= eos (next State))
             State
             (let Result (R State)
               (if (not (= false Result))
                   ((re-repeat R) Result)
                   State)))))

(define with-match
  \* Wrap a regular expression into a match. *\
  E -> (lambda State
         (let Beg (index State)
              Result (E State)
           (if (not (= false Result))
               (@p (state Result) [(index Result) Beg|(matches Result)])
               false))))

(define compile-1
  \* Condense (...|...) and [...] control constructs *\
  [] -> []
  ["("|Rs] -> (let Inside   (take-while (complement (= ")")) Rs)
                   Leftover (tl (drop-while (complement (= ")")) Rs))
                   Split    (remove ["|"] (partition-with (= "|") Inside))
                (cons (with-match
                       (re-or (map (/. X (re-and (compile-2 (compile-1 X))))
                                   Split)))
                      (compile-1 Leftover)))
  ["["|Rs] -> (let Inside (take-while (complement (= "]")) Rs)
                   Leftover (tl (drop-while (complement (= "]")) Rs))
                (cons (re-or Inside) (compile-1 Leftover)))
  [R|Rs]  -> (cons R (compile-1 Rs)))

(define compile-2
  \* Handle + and * control constructs *\
  []         -> []
  [R "+"|Rs] -> (cons R (compile-2 [R "*"|Rs]))
  [R "*"|Rs] -> (cons (re-repeat R) (compile-2 Rs))
  [R|Rs]     -> (cons R (compile-2 Rs)))

(define re-compile
  \* Compile a string to a regular expression *\
  {string --> regexp}
  Expr -> (re-and (compile-2 (compile-1 (parse Expr)))))

(define parse
  \* Convert a string regexp into a list of compiled regular expressions and strings *\
  {string --> [A]}
  "" -> []
  \* Character Classes *\
  (@s "\\d" Str)       -> [(to-re re-digit?)              |(parse Str)]
  (@s "[:digit:]" Str) -> [(to-re re-digit?)              |(parse Str)]
  (@s "\\D" Str)       -> [(to-re (complement re-digit?)) |(parse Str)]
  (@s "\\s" Str)       -> [(to-re re-space?)              |(parse Str)]
  (@s "[:space:]" Str) -> [(to-re re-space?)              |(parse Str)]
  (@s "\\S" Str)       -> [(to-re (complement re-space?)) |(parse Str)]
  (@s "\\w" Str)       -> [(to-re (complement re-space?)) |(parse Str)]
  (@s "[:word:]" Str)  -> [(to-re (complement re-space?)) |(parse Str)]
  (@s "\\W" Str)       -> [(to-re re-space?)              |(parse Str)]
  \* Control characters *\
  (@s "(" Str) -> ["("|(parse Str)] (@s ")" Str) -> [")"|(parse Str)]
  (@s "[" Str) -> ["["|(parse Str)] (@s "]" Str) -> ["]"|(parse Str)]
  (@s "+" Str) -> ["+"|(parse Str)] (@s "*" Str) -> ["*"|(parse Str)]
  (@s "|" Str) -> ["|"|(parse Str)]
  \* Escape'd and Regular Characters *\
  (@s "\\" C Str) -> [(to-re (= C))|(parse Str)]
  (@s C Str)      -> [(to-re (= C))|(parse Str)])

(define to-re
  \* Convert a boolean string matcher into a regular expression *\
  {(string --> boolean) --> regexp}
  X -> (lambda S (if (X (next S)) (increment S) false)))

(define re-digit?
  {string --> boolean}
  X -> (element? X ["0"  "1"  "2"  "3"  "4" "5"  "6"  "7"  "8"  "9"]))

(define re-space?
  {string --> boolean}
  X -> (element? X [" "  "	"  "
"]))

\*******************************************************************************
 * External functions
 *\
(define re-search
  \* Parse and search for a regular expression in a string. *\
  {string --> string --> match-data}
  Re String -> (re-search-from Re String 0))

(define re-search-from
  \* Parse and search for a regular expression in a string starting at arg3. *\
  {string --> string --> integer --> string}
  Re Str Ind -> (re-search- (with-match (re-compile Re))
                            (starting-at (new-state Str) Ind)))

(define re-search-
  \* Continue searching forward in for arg1 in arg2 until success or eos *\
  {regex --> re-state --> match-data}
  Re State -> (let Try (Re State)
                (if (= false Try)
                    (if (= eos (next State))
                        false
                        (re-search- Re (increment State)))
                    Try)))

(define do-matches
  \* Call a function on every match of arg2 in arg3 *\
  {(match-data --> A) --> string --> string --> [A]}
  Fn Re Str -> (do-matches- Fn (with-match (re-compile Re)) (new-state Str)))

(define do-matches-
  \* Call arg1 on the match data from every match of arg2 in arg3 *\
  {(match-data --> A) --> regexp --> re-state --> [A]}
  Fn Re State -> (let Try (Re State)
                   (if (= false Try)
                       (if (= eos (next State))
                           []
                           (do-matches- Fn Re (increment State)))
                       (cons (Fn Try)
                             (do-matches- Fn Re (@p (state Try) []))))))

(define replace-regexp
  \* replace all matches of arg1 with arg2 in arg3 *\
  {string --> string --> string --> string}
  Regexp Replace String ->
  (reduce (/. Match Acc
              (@s (substr Acc 0 (nth 2 Match))
                  Replace
                  (substr Acc (nth 1 Match) (lengthstr Acc))))
          String (reverse (do-matches (function snd) Regexp String))))
