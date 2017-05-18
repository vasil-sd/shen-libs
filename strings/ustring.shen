\* Copyright (c) 16-02-12, Willi O Riha 
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

(define ustring?
   \* returns true iff argument is a unit string *\
   { string --> boolean }
   (@s _ "") -> true
     _       -> false)
 
(package string [whitespace? digit? letter? <ustr >ustr <=ustr >=ustr]
  
   				   \* PREDICATES *\
(define digit?
   \* returns true iff argument is one of "0", "1",  "9" *\
   { string --> boolean }
   S -> (ustring-inrange? (string->n S) 48 57))
   
(define letter?
   \* returns true iff argument is a letter *\
   { string --> boolean }
   S -> (or (lowercase? S) (uppercase? S)))
   
(define uppercase?
   \* returns true iff argument is an upper-case letter *\
   { string --> boolean }
   S -> (ustring-inrange? (string->n S) 65 90))

(define lowercase?
   \* returns true iff argument is a lower-case letter *\
   { string --> boolean }
   S -> (ustring-inrange? (string->n S) 97 122))

(define whitespace?
   \* returns true iff argument is a 'white space' *\
   { string --> boolean }
   S -> (element? S ["c#9;" "c#10;" "c#11;" "c#12;" "c#13;" " "]))

(define ustring-inrange?  \* auxiliary *\
   { number --> number --> number --> boolean }
   N Lb Ub -> (and (>= N Lb) (<= N Ub)))

   							\* FUNCTIONS *\
(define ustring-upcase 
   { string --> string }
   S -> (n->string (- (string->n S) 32)) where (lowercase? S)
   S -> S)

(define ustring-downcase  
   { string --> string }
   S -> (n->string (+ (string->n S) 32)) where (uppercase? S)
   S -> S)
   					
							\* COMPARISON *\
(define <ustr
   { string --> string --> boolean } 
   S1 S2 -> (< (string->n S1) (string->n S2)))

(define <=ustr
   { string --> string --> boolean } 
   S1 S2 -> (<= (string->n S1) (string->n S2)))
   
(define >ustr 
   { string --> string --> boolean } 
   S1 S2 -> (> (string->n S1) (string->n S2)))
   
(define >=ustr 
   { string --> string --> boolean } 
   S1 S2 -> (>= (string->n S1) (string->n S2)))

) 
    
	