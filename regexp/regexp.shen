\* regexp.shen --- regular expressions for shen

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

This library implements regular expressions for shen.  String regular
expressions are compiled to shen functions which accept a string argument
and return an re-state object.

See the bottom portion of this file for external functions which may be
used as an access point for compiling and using regular expressions.

Some examples are included below.

Character classes.

  (1-) (match-strings (re-search "[:digit:]+" "Lorem ipsum dolor sit, 26."))
  ["26"]

  (2-) (match-strings (re-search "\\d+" "Lorem ipsum dolor sit, 26."))
  ["26"]

  (3-) (match-strings (re-search "\\w+" "Lorem ipsum dolor sit amet, 26, mattis eget."))
  ["Lorem"]

Alternatives grouped with [...]'s.

  (4-) (match-strings (re-search "d[olr]+" "Lorem ipsum dolor sit, 26."))
  ["dolor"]

Nested regular expressions and alternatives with (...|...).

  (5-) (do-matches (/. X (hd (match-strings X))) "(ipsum|eget)"
                    "Lorem ipsum dolor sit amet, 26, mattis eget.")
  ["ipsum" "eget"]

Finally it is also possible to express regular expressions using S-exprs rather
than strings, for example

  (1-) (match-strings (re-search [: d [+ [| o l r]]] "Lorem ipsum dolor sit, 26."))
  ["dolor"]

The syntax for S-expr regular expressions is as follows.
 [: ...] ---------------- consequtive regular expressions
 [| ...] ---------------- regular expression alternatives
 [* ...] and [+ ...] ---- repeating regular expressions
 [*? R1 R2] ------------- compile R1 as a lazy regular expression followed by R2

*** Code: *\
(trap-error
 (require string)
 (require sequence)
 (/. E
     (load "../string/string.shen")
     (load "../sequence/sequence.shen")))

(package regexp- [new-state state index matches next increment
                  match-strings starting-at successful? re re-search
                  re-search-from do-matches replace-regexp]
\*******************************************************************************
 * re-state holds the state and match data of regular expressions
 *\
(datatype re-state
  String : string;
  Index : number;
  Matches : [number];
  =============================
  (@p (@p String Index) Matches) : re-state;

  Simple-state : string;
  =======================
  Simple-state : re-state;

  _________________
  false : re-state;)

(define new-state
  {re-state --> re-state}
  (@p (@p String Index) Matches) -> (@p (@p String Index) Matches)
  String -> (@p (@p String 0) []) where (string? String))

(define state
  {re-state --> (string * number)}
  (@p (@p String Index) Matches) -> (@p String Index)
  String -> (@p String 0) where (string? String))

(define index
  {re-state --> number}
  X -> (snd (state X)))

(define matches
  {re-state --> [number]}
  (@p (@p String Index) Matches) -> Matches
  String -> [] where (string? String))

(datatype re-next
  Re-string : string;
  ===================
  Re-string : re-next;

  ___________________
  eos : re-next;)

(define next
  {re-state --> re-next}
  (@p (@p String Index) Matches) -> (trap-error (pos String Index) (/. _ eos))
  String -> (next (new-state String)) where (string? String)
  What -> (print (make-string "what the ~S" What)))

(define increment
  {re-state --> re-state}
  (@p (@p String Index) Matches) -> (@p (@p String (+ 1 Index)) Matches)
  String -> (increment (new-state String)) where (string? String))

(define match-strings
  {re-state --> [string]}
  false -> []
  String -> [] where (string? String)
  (@p (@p S I) [A B|MS]) -> [(substr S A B)|(match-strings (@p (@p S I) MS))])

(define starting-at
  {re-state --> number --> re-state}
  (@p (@p String _) Matches) Index -> (@p (@p String Index) Matches)
  String Index -> (starting-at (new-state String) Index) where (string? String))

(define successful?
  {re-state --> boolean}
  false -> false
  _ -> true)

\*******************************************************************************
 * compilation of a regular expression from a string representation
 *\
(define re-or
  \* Create a disjunction of regular expressions *\
  []     -> (lambda _ false)
  [R|Rs] -> (lambda State
              (let Result ((re R) State)
                (if (successful? Result)
                    Result
                    ((re-or Rs) State)))))

(define re-and
  \* Create a conjunction of regular expressions *\
  []     -> (lambda X X)
  [R|Rs] -> (lambda State
              (let Result ((re R) State)
                (if (successful? Result)
                    ((re-and Rs) Result)
                    false))))

(define re-repeat
  \* Repeatedly apply a regular expression until it fails *\
  R -> (let RC (re R)
         (lambda State
           (let Result (RC State)
             (if (successful? Result)
                 ((re-repeat RC) Result)
                 State)))))

(define with-match
  \* Wrap a regular expression into a match. *\
  E -> (lambda State
         (let Beg (index State)
              Result ((re E) State)
           (if (successful? Result)
               (@p (state Result) [(index Result) Beg|(matches Result)])
               false))))

(define re-repeat-lazy
  R1 [] -> (lambda State State)
  R1 R2  -> (let RC1 (re R1)
                 RC2 (re R2)
              (lambda State
                (let Result1 (RC2 State)
                  (if (successful? Result1)
                      Result1
                      (let Result2 (RC1 State)
                        (if (successful? Result2)
                            ((re-repeat-lazy R1 R2) Result2)
                            false)))))))

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
  [R "+" "?" |Rs] -> (cons R (compile-2 [R "*" "?"|Rs]))
  [R "+"|Rs] -> (cons R (compile-2 [R "*"|Rs]))
  [R1 "*" "?" R2|Rs] -> (cons (re-repeat-lazy R1 R2) (compile-2 Rs))
  [R1 "*" "?"|Rs] -> (cons (re-repeat-lazy R1 []) (compile-2 Rs))
  [R "*"|Rs] -> (cons (re-repeat R) (compile-2 Rs))
  [R|Rs]     -> (cons R (compile-2 Rs)))

(define parse
  \* Convert a string regexp into a list of compiled regular expressions and strings *\
  {string --> (list A)}
  "" -> []
  \* Character Classes *\
  (@s "\\d" Str)       -> [(to-re re-digit?)              |(parse Str)]
  (@s "[:digit:]" Str) -> [(to-re re-digit?)              |(parse Str)]
  (@s "\\D" Str)       -> [(to-re (complement re-digit?)) |(parse Str)]
  (@s "\\s" Str)       -> [(to-re re-space?)              |(parse Str)]
  (@s "[:space:]" Str) -> [(to-re re-space?)              |(parse Str)]
  (@s "\\S" Str)       -> [(to-re (complement re-space?)) |(parse Str)]
  (@s "\\a" Str)       -> [(to-re re-alpha?)              |(parse Str)]
  (@s "[:alpha:]" Str) -> [(to-re re-alpha?)              |(parse Str)]
  (@s "\\A" Str)       -> [(to-re (complement re-alpha?)) |(parse Str)]
  (@s "\\w" Str)       -> [(to-re re-word?)               |(parse Str)]
  (@s "[:word:]" Str)  -> [(to-re re-word?)               |(parse Str)]
  (@s "\\W" Str)       -> [(to-re (complement re-word?))  |(parse Str)]
  \* Beginning and end markers *\
  \** (@s "^" Str) ->         [beginning-of-string? | (parse Str)] **\
  (@s "$" Str) ->         [end-of-string?       | (parse Str)]
  \* Control characters *\
  (@s "(" Str) -> ["("|(parse Str)] (@s ")" Str) -> [")"|(parse Str)]
  (@s "[" Str) -> ["["|(parse Str)] (@s "]" Str) -> ["]"|(parse Str)]
  (@s "+" Str) -> ["+"|(parse Str)] (@s "*" Str) -> ["*"|(parse Str)]
  (@s "|" Str) -> ["|"|(parse Str)] (@s "?" Str) -> ["?"|(parse Str)]
  \* Escape'd and Regular Characters *\
  (@s "\\" C Str) -> [(to-re (= C))|(parse Str)]
  (@s C Str)      -> [(to-re (= C))|(parse Str)])

(define to-re
  \* Convert a boolean string matcher into a regular expression *\
  {(string --> boolean) --> regexp}
  X -> (lambda S (let Next (next S)
                   (if (and (not (= eos Next)) (X Next))
                       (increment S) false))))

(define beginning-of-string?
  {re-state --> re-state}
  S -> (if (= 0 (index S)) S false))

(define end-of-string?
  {re-state --> re-state}
  S -> (if (= eos (next S)) S false))

(define re-digit?
  {string --> boolean}
  X -> (element? X ["0"  "1"  "2"  "3"  "4" "5"  "6"  "7"  "8"  "9"]))

(define re-alpha?
  {string --> boolean}
  X -> (element? X ["z" "y" "x" "w" "v" "u" "t" "s" "r" "q" "p" "o" "n"
                    "m" "l" "k" "j" "i" "h" "g" "f" "e" "d" "c" "b" "a"
                    "Z" "Y" "X" "W" "V" "U" "T" "S" "R" "Q" "P" "O" "N"
                    "M" "L" "K" "J" "I" "H" "G" "F" "E" "D" "C" "B" "A"]))

(define re-word?
  {string --> boolean}
  X -> (or (re-digit? X) (re-alpha? X)))

(define re-space?
  {string --> boolean}
  X -> (element? X [" "  "	"  "
"]))

\*******************************************************************************
 * External functions
 *\
(define re
  \* Compile a string or S-expr to a regular expression *\
  Str        -> (re-and (compile-2 (compile-1 (parse Str)))) where (string? Str)
  digit      -> (to-re re-digit?)
  space      -> (to-re re-space?)
  alpha      -> (to-re re-alpha?)
  word       -> (to-re re-word?)
  Sym        -> (re-and (map (function re) (explode (str Sym)))) where (symbol? Sym)
  [- A B]    -> (re-range A B)
  [| |RS]    -> (re-or RS)
  [bar! |RS] -> (re-or RS)
  [: |RS]    -> (re-and RS)
  [m |RS]    -> (re-match RS)
  [+ R]      -> (re [: R [* R]])
  [* R]      -> (re-repeat R)
  [+? A B]   -> [: A (re-repeat-lazy A B)]
  [+? A]     -> [: A (re-repeat-lazy A [])]
  [*? A B]   -> (re-repeat-lazy A B)
  [*? A]     -> (re-repeat-lazy A [])
  CS         -> (re [:|CS]) where (cons? CS)
  X          -> X)

(define re-search
  \* Parse and search for a regular expression in a string. *\
  {string --> string --> match-data}
  Re String -> (re-search-from Re String 0))

(define re-search-from
  \* Parse and search for a regular expression in a string starting at arg3. *\
  {string --> string --> number --> string}
  Re Str Ind -> (re-search- (with-match (re Re))
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
  {(match-data --> A) --> string --> string --> (list A)}
  Fn Re Str -> (do-matches- Fn (with-match (re Re)) (new-state Str)))

(define do-matches-
  \* Call arg1 on the match data from every match of arg2 in arg3 *\
  {(match-data --> A) --> regexp --> re-state --> (list A)}
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
                  (substr Acc (nth 1 Match) (length-str Acc))))
          String (reverse (do-matches (function snd) Regexp String))))

)
