\* Copyright (c) 05-07-12, Mark Tarver, Eric Schulte, Willi O Riha 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*\

(load "ustring.shen")

(package string-  [ustring? 
				  uppercase? lowercase? whitespace? digit? letter?
				  <ustr >ustr digit-string? substring? 	    
				  string-length list->string substring string-map
				  <=str >str >=str <str string-reverse delete-substring
				  decimal->radixB radixB->decimal] 
				   
					\* STRING PREDICATES *\
(define every?
   \* returns true iff predicate P applies to all unit strings of a string *\
   { (string --> boolean) --> string --> boolean }
   P "" -> true
   P (@s S Str) -> (and (P S) (every? P Str)))

(define any?
   \* returns true iff predicate P applies to at least one unit strings of a string *\
   { (string --> boolean) --> string --> boolean }
   P "" -> false
   P (@s S Str) -> (if (P S) true (any? P Str)))

(define digit-string?
   \* returns true iff string consists entirely of digitis *\
   { string --> boolean } 
   Str -> (every? digit? Str))
		
(define prefix?
   \* returns true iff 1st string is a prefix of 2nd *\
   { string --> string --> boolean }
   "" _ -> true
   (@s S Str1) (@s S Str2) -> (prefix? Str1 Str2)
   _ _ -> false)
 
(define prefix-length
   \* returns length of the longest common prefix of the two arguments *\
   { string --> string --> number} 
   Str1 Str2 -> (prefix-length-h Str1 Str2 0))

(define suffix-length
   \* returns length of the longest common suffix of the two arguments *\
   { string --> string --> number} 
   Str1 Str2 -> (prefix-length (string-reverse Str1) (string-reverse Str2)))
   
(define prefix-length-h
   { string --> string --> number --> number}
   (@s S Str1) (@s S Str2) N -> (prefix-length-h Str1 Str2 (+ N 1))
   _ _  N -> N)

(define suffix?
   \* returns true iff 1st string is a suffix of 2nd *\
   { string --> string --> boolean }
   Str1 Str2 -> (prefix? (string-reverse Str1) (string-reverse Str2)))
      
(define substring?
   \* returns true iff 1st string is a substring of 2nd *\
   { string --> string --> boolean }
   "" _ -> true
   _ "" -> false
   Str1 Str2 -> true where (prefix? Str1 Str2) 
   Str1 (@s _ Str2) -> (substring? Str1 Str2))
   
(define string-map
   \* applies function F to each unit string of a string *\
   {(string --> string) --> string --> string}
   F Str -> (map-h F Str ""))

(define map-h
   {(string --> string) --> string --> string --> string}
   _ "" Result -> Result
   F (@s S Str) Result -> (map-h F Str (@s Result (F S))))
   
(define downcase
   \* returns string with all upper-case letters converted to lower case *\
   { string --> string }
   Str -> (string-map (function ustring-downcase) Str))
   
(define upcase
   \* returns string with all lower-case letters converted to upper case *\
   { string --> string }
   Str -> (string-map (function ustring-upcase) Str))
					
							\* STRING COMPARISON *\
(define <=str 
   { string --> string --> boolean } 
   "" _ -> true
   _ "" -> false
   (@s S1 Str1) (@s S2 Str2) -> (if (= S1 S2) (<=str Str1 Str2) (<ustr S1 S2)))

(define >=str
   { string --> string --> boolean } 
   _  "" -> true
   "" _  -> false
   (@s S1 Str1) (@s S2 Str2) -> (if (= S1 S2) (>=str Str1 Str2) (>ustr S1 S2)))
   
(define <str
   { string --> string --> boolean } 
   Str1 Str2 -> (not (>=str Str1 Str2)))
   
(define >str
   { string --> string --> boolean } 
   Str1 Str2 -> (not (<=str Str1 Str2)))

(define ref
   { number --> string --> string }
   N Str -> (pos Str N)) 
      
(define string-length
   \* returns the length of the string *\
   { string --> number }
   Str -> (length-h Str 0))
 
(define length-h
   { string --> number --> number }
   "" Len -> Len
   (@s _ Str) Len -> (length-h Str (+ Len 1)))
   
(define index
   \* returns the 'index' of Str1 in Str2, or -1 if not a substring *\
   { string --> string --> number }
   Str1 Str2 -> (index-h Str1 Str2 0))

(define index-h
   { string --> string --> number --> number }
    Str1 Str2 N -> N  where (prefix? Str1 Str2)
    _ "" _ -> -1
    Str1 (@s _ Str2) N -> (index-h Str1 Str2 (+ N 1)))

(define index-last  \* added 12-02-12 *\
   \* returns the last 'index' of Str1 string in Str2, or -1 if not a substring *\
   { string --> string --> number }
   Str1 Str2 -> (index-last-h Str1 Str2 -1 0))

(define index-last-h		
   { string --> string --> number --> number --> number }
    Str1 Str2 Last N -> (index-last-h Str1 (tlstr Str2) N (+ N 1))  where (prefix? Str1 Str2)
    _ "" Last _ -> Last
    Str1 (@s _ Str2) Last N -> (index-last-h Str1 Str2 Last (+ N 1)))
	
(define insert   \* added 13-02-12 *\
   \* inserts a string after the n-prefix *\
   { number --> string --> string --> string } 
   N Str1 Str2 -> (let Split(split N Str2)
					   (@s (fst Split) Str1 (snd Split))))

(define split   \* added 13-02-12 *\
   \* splits a string into an n-prefix and the remaining suffix *\
   { number --> string --> (string * string) } 
   N Str -> (split-h N Str ""))
     
(define split-h
   { number --> string --> string --> (string * string) }
   _ "" Result -> (@p Result "")
   N Str  Result -> (@p Result Str) where (<= N 0)
   N (@s S Str) Result -> (split-h (- N 1) Str (@s Result S)))

(define take
   \* returns the n-length prefix of string *\
   { number --> string --> string }  
   N Str -> (fst(split N Str)))
       
(define drop
   \* drops the n-length prefix of string *\
   { number --> string --> string }
   N Str -> (snd(split N Str)))
   
(define take-right
   \* returns the n-length suffix of string *\
   { number --> string --> string }
   N Str -> (drop (- (string-length Str) N) Str))

(define drop-right
   \* drops the n-length suffix of string *\
   { number --> string --> string }
   N Str -> (take (- (string-length Str) N) Str))
   
(define substring
   \* returns substring Str[M..N] *\
   { number --> number --> string --> string }
   M N Str -> (drop M (take (+ N 1) Str)))

(define pad
   \* pads Str (on the left) to length N with unit strings S  *\
   { string --> number --> string --> string }
   S N Str -> (take-right N (@s (n-copy N S) Str)))
   
(define count
   \* returns the no. of occurrences of Str1 in Str2 *\
   { string --> string --> number }
   Str1 Str2 -> (count-h Str1 Str2 (string-length Str1) 0))

(define count-h
   { string --> string --> number --> number --> number }
    _ "" _ Count -> Count
    Str1 Str2 N Count -> (count-h Str1 (drop N Str2) N (+ Count 1))
														where (prefix? Str1 Str2)
    Str1 (@s _ Str2) N Count -> (count-h Str1 Str2 N Count))    

(define replace-all
   \* replaces all occurrences of Str2 with Str1 in Str3  *\ 
   { string --> string --> string --> string }
   _  "" _  -> (error "empty substring in 'replace-all'!~%")
   Str1 Str2 Str3 -> (replace-all-h Str1 Str2 Str3 (string-length Str2) ""))

(define replace-all-h
  { string --> string --> string --> number --> string --> string }
   _ _ "" _ Result -> Result
   Str1 Str2 Str3 N Result -> (replace-all-h Str1 Str2 (drop N Str3) N (cn Result Str1))
															where (prefix? Str2 Str3)
   Str1 Str2 (@s S Str) N Result -> (replace-all-h Str1 Str2 Str N (cn Result S)))
  
(define replace
   \* replaces the I-th occurrence of Str2 with Str1 in Str3  *\ 
   { string --> string --> number --> string --> string }
   _  "" _ _  -> (error "empty substring in 'replace'!~%")
   Str1 Str2 I Str3 -> (replace-h Str1 Str2 Str3 I ""))

(define replace-h
  { string --> string --> string --> number --> string --> string}
   _ _ "" _ Result -> Result
   Str1 Str2 Str3 1 Result -> (@s Result Str1 (drop (string-length Str2) Str3))
															where (prefix? Str2 Str3)
   Str1 Str2 Str3 I Result -> (replace-h Str1 Str2 (tlstr Str3) (- I 1) (cn Result (hdstr Str3)))
															where (prefix? Str2 Str3)
   Str1 Str2 (@s S Str) I Result -> (replace-h Str1 Str2 Str I (cn Result S)))
   
(define delete-all
   \* deletes all occurrences of Str1 in Str2  *\ 
   { string --> string --> string } 
   "" Str2 -> Str2  
   Str1 Str2 -> (replace-all "" Str1 Str2))

(define delete-substring
   \* deletes substring Str[M..N]  *\ 
   { number --> number --> string --> string }
   M N Str -> Str where (> M N)
   M N Str -> (cn (take M Str)(drop (+ N 1) Str)))
				      
(define n-copy 
   \* makes n copies of a string *\  
   { number --> string --> string }
   N Str -> (n-copy-h N Str ""))
	 
(define n-copy-h
   { number --> string -->  string --> string }
   N Str Copy -> Copy  where (<= N 0)
   N Str Copy -> (n-copy-h(- N 1) Str (@s Copy Str)))

(define trim-left
   \* drops the longest prefix whose unit strings all satisfy predicate P *\
   { (string --> boolean) --> string --> string }
   _ "" -> ""
   P (@s S Str) -> (@s S Str)  where (not (P S))
   P (@s _ Str) -> (trim-left P Str)) 

(define trim-right
   { (string --> boolean) --> string --> string }          
   P Str -> (string-reverse (trim-left P (string-reverse Str))))
     
(define trim
   { (string --> boolean) --> string --> string }          
   P Str -> (string-reverse (trim-left P (string-reverse (trim-left P Str)))))  
   	 
(define string-reverse
   \* reverses a string *\
   {string --> string}
   Str -> (reverse-h Str ""))

(define reverse-h
   {string --> string --> string}
   "" Rev -> Rev
   (@s S Str) Rev -> (reverse-h Str (@s S Rev))) 

(define tokenise
   { (string --> boolean) --> string -->  (list string)}
   F Str -> (tokenise-h F Str "" []))

(define tokenise-h
   { (string --> boolean) --> string -->  string --> (list string) --> (list string)}
   _ "" Str L -> (reverse [Str | L])
   F (@s S1 Str) S2 L -> (tokenise-h F Str "" [S2 | L])  where (F S1)
   F (@s S1 Str) S2 L -> (tokenise-h F Str (@s S2 S1) L))  
   
(define interpose
  \* inserts the first arg between every two elements of the second arg *\
  { string --> string --> string }
  J Str -> (interpose-h J Str ""))  
  
(define interpose-h
  { string --> string --> string --> string }
  _ "" Result -> Result 
  _ Str Result -> (@s Result Str) where (ustring? Str) 
  J (@s S Str) Result -> (interpose-h J Str (@s Result S J)))

(define join
   \* inserts the first arg between every two elements of the second arg *\
   { string --> (list string) --> string}
   J StrL -> (join-h J StrL ""))
   
(define join-h
   { string --> (list string) --> string --> string}
   _ [] Result -> Result 
   _ [Str] Result -> (@s Result Str)
   J [Str | StrL] Result -> (join-h J StrL (@s Result Str J)))
   
(define filter
   { (string --> boolean) --> string --> string }
   P Str -> (filter-h P Str ""))
   
(define filter-h
   { (string --> boolean) --> string --> string --> string }
   P "" Result -> Result
   P (@s S Str) Result -> (if (P S) 
							  (filter-h P Str (@s Result S))
							  (filter-h P Str Result)))

(define count-ustrings
   {(string --> boolean) --> string --> number }
   P Str -> (reduce (/. S N (if (P S) (+ N 1) N)) 0 Str))
 
(define reduce  \* foldr *\   \* not tail recursive *\
   \* right-left reduction of a function over a string *\
   {(string --> B --> B) --> B --> string --> B}
   F B "" -> B
   F B (@s S Str) -> (F S (reduce F B Str)))

(define reduce'  \* tail recursive *\
  \* right-left reduction of a function over a string *\
  {(string --> B --> B) --> B --> string --> B}
  F B Str -> (reduce-h F B (string-reverse Str) B))
  
(define reduce-h 
  {(string --> B --> B) --> B --> string --> B --> B}
  F B "" Result -> Result
  F B (@s S Str) Result -> (reduce-h F B Str (F S Result)))

(define foldl
  \* left-right reduction of a function over a string *\
  {(string --> B --> B) --> B --> string --> B}
  F B "" -> B
  F B (@s S Str) -> (foldl F (F S B) Str))
 
(define list->string
   { (list string) --> string } 
   L -> (list->str-h L ""))
   
(define list->str-h
   { (list string) --> string --> string }
   [] Str -> Str
   [S | L] Str -> (list->str-h L (@s Str S))) 
      
			\* NUMBER CONVERSIONS *\
			
(define >number
   { string --> number }
   Str -> (let Ex (index "e" Str)
             (if (< Ex 0) 
			     (->decimal Str)
			     (let Tl (drop (+ Ex 1) Str)
				      Exp (exptL Tl)
				      ScaleF (pow10 Exp)
					  Y (take Ex Str) 
					  (* (->decimal Y) ScaleF)))))
				  
(define ->decimal
   { string --> number }
   "" -> (error "empty string~%")
   Str -> (let Pt (index "." Str)
			 (if (< Pt 0) 
			     (str->int Str)
			      (let Hd (simplify(take Pt Str))
				       Tl (drop (+ Pt 1) Str)
					   Sgn (take 1 Hd)
					   IntPart (str->uint (drop 1 Hd))
					   FracPart (->fraction Tl)
					   Res (+ IntPart FracPart)
					   (if (= Sgn "+") Res (- 0 Res))))))
					   					   
(define exptL
   { string --> number }
   "" -> (error "exponent missing!~%")
   "-" -> (error "exponent missing!~%")
   (@s "-" Str) -> (- 0 (str->uint Str))
   (@s S _) -> (error "illegal character '~A' in exponent~%" S) where (not (digit? S))
   Str -> (str->uint Str))

(define ->fraction
   { string --> number }
   "" -> (error "fractional part missing!~%")
   Str -> (->fraction-h Str))
   
\* accumulates fraction from string implicitly reversing it *\			 
(define ->fraction-h
   { string --> number }
   "" -> 0
   (@s S Str) -> (let D (str->digit S)
                        (if (< D 0) 
						   (error "illegal character '~A' in number~%" S)
                           (/ (+ (->fraction-h Str) D) 10))))

(define str->int
   { string --> number }
   "" -> (error "not a number!~%")
   (@s "-" Str) -> (- 0 (str->int Str))
   (@s "+" Str) -> (str->int Str)
   Str -> (str->uint Str))

(define str->uint
   { string --> number }
   Str -> (str->uint-h Str 0))
   
(define str->uint-h
   { string --> number --> number }
   "" N 		-> N
   (@s S Str) N -> (let D (str->digit S)
                        (if (< D 0) (error "illegal digit '~A' in decimal number~%" S)
						     (str->uint-h Str(+ (* N 10) D )))))

(define str->digit
   { string --> number }
   Str -> (let D (string->n Str)
               (if (ustring-inrange? D 48 57) (- D 48) -1)))
 
(define trim0s
   { string --> string }
   (@s "0" Str) -> (trim0s Str)
    Str -> Str)
       
(define simplify
   { string --> string }
   (@s "-" "-" Str) -> (simplify Str)
   (@s "+" "-" Str) -> (simplify (@s "-" Str))
   (@s "-" "+" Str) -> (simplify (@s "-" Str))
   (@s "+" Str) -> (simplify Str)
   (@s "-" Str) -> (@s "-" (trim0s Str))
   Str -> (@s "+" (trim0s Str)))

				\* RADIX CONVERSION *\
				
(define radixBdigit->decimal    \* auxiliary *\
   \* S is a 'digit' occurring in a radix-B number,
      if S is a bona fide digit then the fn returns the decimal value of S,
	  where "0" -> 0 "a" or "A" -> 10, "b" -> 11 etc
	  if S is an illegal digit, e.g. "3" in a binary number, -1 is returned *\
   { string --> number --> number }
   S B -> (let D (adjust(string->n (ustring-downcase S)))
               (if (and (<= 0 D) (< D B)) D -1)))

(define adjust  \* auxiliary *\
   { number --> number }
   N -> (- N 48) where (< N 58)
   N -> (- N 87))

(define digit->str   \* auxiliary *\
   { number --> string }
   N -> (n->string (+ N 48)) where (< N 10)
   N -> (n->string (+ N 87)))
   
(define radixB->decimal
   \* if Str represents an integer in radix-B number system the decimal equivalent is returned
      otherwise an error message is displayed *\
   { string --> number --> number }
   _ B -> (error "radix must be greater than '1'~%") where (< B 2)
   (@s "0" Str) 16 -> (hex' Str)
   Str B -> (radixB->decimal-h Str B 0))

(define hex'  \*auxiliary *\
   { string --> number }
   (@s "x" Str) -> (radixB->decimal-h Str 16 0)
   Str -> (radixB->decimal-h Str 16 0))
   
(define radixB->decimal-h
   { string --> number --> number --> number }
   "" _ N 		  -> N
   (@s S Str) B N -> (let D (radixBdigit->decimal S B)
                         (if (< D 0) 
						     (error "illegal digit '~A' in radix '~A' integer~%" S B) 
						     (radixB->decimal-h Str B(+ (* N B) D )))))

(define oct-hex->decimal  \* auxiliary *\
   { string --> number }
   (@s "x" Str) -> (radixB->decimal-h Str 16 0)
   Str -> (radixB->decimal-h Str 8 0))

(define oct-hex-dec->decimal
   \* converts to decimal, octal or hex *\
   { string --> number }
   (@s "0" Str) -> (oct-hex->decimal Str)
   Str -> (radixB->decimal-h Str 10 0))
      				
(define decimal->radixB
   \* converts a decimal integer to radix-B *\
   { number --> number --> string }
   _  B -> (error "radix must be greater than 1~%") where (<= B 1)
   X  B -> (decimal->radixB-h (abs X) B ""))
   
(define decimal->radixB-h
   { number --> number --> string --> string}
   0 B S -> S
   X B S -> (let DivMod (/-pos X B)
                 (decimal->radixB-h (fst DivMod) B (@s (digit->str (snd DivMod)) S))))					

(define radixB->radixC
   \* converts from radix-B to radix-C *\
   { string --> number --> number --> string }
   Str B C -> (decimal->radixB (radixB->decimal Str B) C))
   
\* auxiliary *\					  				  	
(define pow10+
    { number --> number }
	0  -> 1
	N  -> (* (pow10+ (- N 1)) 10))

\* auxiliary *\
(define pow10
   {number --> number }
   N -> (pow10+ N)  where (>= N 0)
   N -> (/ 1 (pow10+ (- 0 N))))

\* from maths-lib *\

(define abs
   { number --> number }
   X -> (if (>= X 0) X (- 0 X)))
   
(define /-pos		
   { number --> number --> (number * number) }
   _ 0 -> (error "division by 0!~%")
   A B -> (@p 0 A) where (> B A) 
   _ B -> (error "divisor must be an integer!~%") where (not(integer? B))
   A B -> (let Pow2 (pow-2div B A 1)  
			   (div-w A (* Pow2 B) Pow2 0)))

(define div-w		
   { number --> number --> number --> number --> (number * number) }
   A B 1 Q -> (if (<= B A) (@p (+ Q 1) (- A B)) (@p Q A))
   A B P Q -> (if (<= B A) (div-w (- A B) (/ B 2) (/ P 2) (+ Q P))
						   (div-w A (/ B 2) (/ P 2) Q)))  

(define pow-2div
   \* returns the smallest power of 2, s.t. B*Pow2 > A *\
   { number --> number --> number --> number }
   B A Pow2 -> (if (<= B A) (pow-2div (* B 2) A (* Pow2 2)) Pow2))
   
)
								   	