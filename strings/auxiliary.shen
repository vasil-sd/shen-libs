\* Copyright (c) 22-03-12,  Willi O Riha
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

(define !=
   { A --> A --> boolean }
   X Y -> (not (= X Y)))
 
(package aux []
   
(define maxL
   { (list number) --> number }
   [X] -> X
   [X | Y] -> (max' X (maxL Y))
   [] -> (error "not defined!~%"))
   \* [] -> 1.7976931348623157e308 *\

(define minL
   { (list number) --> number }
   [X] -> X
   [X | Y] -> (min' X (minL Y))
   [] -> (error "not defined!~%"))
   \* [] -> 0.22250738585072014e-307 *\

(define max'
   {number --> number --> number}
   M N -> (if (> M N) M N))

(define min'
   {number --> number --> number}
   M N -> (if (< M N) M N))

)

   