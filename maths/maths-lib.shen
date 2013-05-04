\* Copyright (c) 03-12-12,  Willi O Riha
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
			\* changed export list *\
(package maths [!= even? odd? natural? positive? negative? zero?
				 divisible-by? prime? /mod /rem /%
				 div trunc-div div-eucl mod rem %
				 gcd lcm max min sign abs ceiling floor trunc fmod
				 maths-round0 maths-round'
				 int-part frac-part modf frexp ldexp	
				 sin cos tan asin acos atan atan2
				 square power exp sinh cosh tanh
				 dms->degs degs->dms rad->degs degs->rad
				 expt sqrt log log2 log10 log' 
				 pi pi/2 pi/4 pi*2 log2 e log2e log10e 
				 one/pi two/pi two/sqrt-pi sqrt2 one/sqrt2]

(define sign
   { number --> number }
   X -> 1  where (> X 0)
   X -> -1 where (< X 0)
   _ -> 0)
   
(define abs
   { number --> number }
   X -> (if (>= X 0) X (- 0 X)))
   
\* ====================================================== *\
 
(define floor
   { number --> number }
   X -> (if (>= X 0) (floor-pos X) (floor-neg X))) 

(define floor-pos
   { number --> number }
   X  -> 0  where (< X 1)    \* not really necesary *\
   X  -> (floor-h X (pow-2 1 X) 0))

(define floor-neg
   { number --> number }
   X -> (let F  (floor-pos (- 0 X)) 
             F1 (- 0 F)
            (if (= F1 X) F1 (- F1 1))))

(define floor-h
   { number --> number --> number --> number }
   \* X P Q -> Q where (< P 1) *\
   X 1 Q -> (if (<= 1 X) (+ Q 1) Q)
   X P Q -> (if (<= P X) 
				(floor-h (- X P) (rsh P) (+ Q P))
				(floor-h X (rsh P) Q)))   

(define ceiling
   { number --> number }
   X -> (- 0 (floor (- 0 X))))
   
(define trunc   \* same as int-part *\
   { number --> number }
   X -> (let FloorAbsX (floor-pos(abs X))
			 (if (>= X 0) FloorAbsX (- 0 FloorAbsX))))
			 
\* ====================================================== *\
 
(define maths-round0	
   { number --> number }
   X -> (if (round-down? X) (floor X) (ceiling X)))
   
(define round-down?  
   { number --> boolean }
   X -> (let FLX (floor X)
             FRX (- X FLX)
             (or(< FRX 0.5) (and (= FRX 0.5) (even? FLX)))))
   
(define int-part     \* same as trunc *\
   { number --> number } 
   X -> (if (>= X 0) (floor X) (ceiling X)))

(define frac-part
   { number --> number }
   X -> (- X (int-part X)))
   
(define modf
   { number --> (number * number) }
   X -> (let Int (int-part X)
			 (@p Int (- X Int)))) 
					 
(define maths-round'
   { number --> number --> number }
   X N -> (let Scale (power 10 N)
			   (/ (maths-round0 (* X Scale)) Scale)))

\* auxiliary fn:
	precond: P > 0, X > 0. (if P <= 0 -> infinite loop!)
    usual call: (pow-2 1 X) which returns 
	     the largest power of 2 not greater than X *\		   
(define pow-2
   { number --> number --> number }
    P X -> (let P2 (* P 2)
                (if (<= P2 X) (pow-2 P2 X) P)))

\* floating modulus - similar to rem *\    
(define fmod
   { number --> number --> number }
   X Y -> (let YA (abs Y)
               Q (/ X YA)
               (if (> X 0) (- X (* (floor Q) YA)) 
                           (- X (* (ceiling Q) YA)))))
\* ====================================================== *\						   
(define frexp
   { number --> (number * number) }
   0 -> (@p 0 0)
   X -> (if (> X 0) (frexp-pos X) (frexp-neg X)))

(define frexp-neg 
   { number --> (number * number) }
   X -> (let Tmp (frexp-pos (- 0 X))
             (@p (- 0 (fst Tmp)) (snd Tmp))))
			 
(define frexp-pos
   { number --> (number * number) }
   X -> (if (> X 0.5) (div-2 X 0) (mult-2 X 0)))
			
(define mult-2  \* (0 X) -> (1 2*X) -> ... -> (Exp Y), where Y >= 0.5 *\
   { number --> number --> (number * number) }
   X Exp -> (if (< X 0.5) (mult-2 (* X 2)(- Exp 1)) (@p X Exp)))
   
(define div-2   \* (0 X) -> (1 X/2) -> ... -> (Exp Y), where Y < 1 *\
   { number --> number --> (number * number) }     \* corrected >= *\
   X Exp -> (if (>= X 1.0) (div-2 (/ X 2)(+ Exp 1)) (@p X Exp)))
   
(define ldexp
   { number --> number --> number }
   X Exp -> (* X (power 2 Exp)))
\* ====================================================== *\
   
(define square	
   { number --> number }
   X -> (* X X) )

(define power-pos
  { number --> number --> number }
  X 1 -> X
  X N -> (let Y (square (power-pos X (rsh N)))
              (if (even? N) Y (* X Y))))
            
(define power	\* fast integer power *\
  { number --> number --> number }
  X 0 -> 1 
  X N -> (error "power - exponent must be an integer~%") where (not (integer? N))  
  X N -> (let P (power-pos X (abs N))
              (if (> N 0) P (/ 1.0 P))))
\* ====================================================== *\
(datatype global
   if (element? Global [tolerance e log2e log10e log2 log10 pi pi/2 pi/4 pi*2
					    pi/180 l80/pi one/pi two/pi two/sqrt-pi sqrt2 one/sqrt2])
   ________________
   (value Global) : number; )
                  
\* this constant may be changed - it works for double precision ! *\
(set tolerance 1e-15)
				\* used *\
(set e          2.71828182845904523536) 
(set log10      2.30258509299404568402)
(set log2		0.69314718055994530942)
(set pi         3.14159265358979323846)
(set pi/2		1.57079632679489661923)
(set pi/4	    0.78539816339744830962)
(set pi*2       6.28318530717958647692)
				\* not used *\
(set one/pi		0.31830988618379067154)
(set two/pi		0.63661977236758134308)
(set log2e		1.44269504088896340736)
(set log10e		0.43429448190325182765)
(set sqrt2		1.41421356237309504880)
(set one/sqrt2	0.70710678118654752440)
(set two/sqrt-pi 1.12837916709551257390)
				\* conversion constants *\
(set pi/180     0.017453292519943295769)  	\* 1 degree in radians *\
(set l80/pi    57.295779513082320876798)  	\* 1 radian in degrees *\
                  
(define small-enough?
   { number --> boolean }
   X -> (< (abs X) (value tolerance)))

\*====================================================== *\

(define rad->degs   			\* radians -> degrees *\
   {number --> number }
   X -> (* X (value l80/pi)))
   
(define degs->rad			\* degrees -> radians *\
   { number --> number }
   X -> (* X (value pi/180)))

(define dms->degs			\* [degs mins secs] -> degrees *\
   {(list number) --> number }
   [] -> (error "dms->degs - no arguments given!")
   [_ _ _ _ | _] -> (error "too many arguments in dms->degs")
   [Degs | Y] -> (let Tmp (dms->degs-pos [(abs Degs) | Y])
					  (if (negative? Degs ) (- 0 Tmp) Tmp)))
					  
(define dms->degs-pos		\* [degs mins secs] -> degrees  where degs >= 0 *\
   {(list number) --> number } 
   [Degs] -> Degs
   [Degs | _] -> (error "dms->degs - type error in Degs")	 where (not (integer? Degs))
   [Degs Mins] -> (+ Degs (/ Mins 60))  			  where (range-ok? Mins)
   [Degs Mins _] -> (error "dms->degs - type error in Mins") where (not (integer? Mins))
   [Degs Mins Secs] -> (+ Degs (/(+ Mins (/ Secs 60)) 60)) where (and (range-ok? Mins)(range-ok? Secs)) 
   X -> (error "dms->degs - range error in Mins/Secs"))
   
(define degs->dms			\* degrees -> [degs mins secs] *\
   { number --> (list number) }
   X -> (let Degs (trunc X)
             Tmp  (abs (* (- X Degs) 60))
			 Mins (floor Tmp)
			 Secs (* (- Tmp Mins) 60)
			 [Degs Mins (maths-round' Secs 10)]))	 \* added 14-12-11 *\
			 
(define range-ok?  \* auxiliary *\
   { number --> boolean }  
   M -> (and (>= M 0)(< M 60)))   
\*====================================================== *\

(define exp
   { number --> number }
   X -> (let  X1 (ceiling X)
              X2 (- X X1)
              (if (< X2 -0.5) 
			      (/ 1.0 (exp (- 0 X)))
                  (exp-large X))))
           
(define exp-large
   { number --> number }
   X -> (let X1 (ceiling X)
              P (power (value e) X1)
              (* P (exp-h (- X X1)))))

(define exp-h
   { number --> number }
   X -> (exp-sum 1 1.0 1.0 X))
  
(define exp-sum 
   { number --> number --> number --> number --> number }
   I P Sum X -> Sum  where (small-enough? P) 
   I P Sum X -> (let NewP (/ (* P X) I)
                     (exp-sum (+ I 1) NewP (+ Sum NewP) X)))
                
(define sinh
   { number --> number }
   X -> (let ExpX (exp X)
             (/ (- ExpX (/ 1 ExpX)) 2)))

(define cosh
   { number --> number }
   X -> (let ExpX (exp X)
             (/ (+ ExpX (/ 1 ExpX)) 2))) 
           
(define tanh
   { number --> number }
   X -> (sign X)     where (> (abs X) 20) 
   X -> (let Exp2X (exp (+ X X))
             (/ (- Exp2X 1) (+ Exp2X 1))))                      
\*====================================================== *\  

(define expt
   { number --> number --> number }
   0 0 -> 1
   0 Y -> (error "expt undefined!~%")  where (< Y 0) 
   X Y -> (error "expt undefined!~%")  where (and (not (integer? Y))(< X 0))
   X Y -> (power X Y) where (integer? Y)
   X Y -> (exp (* Y (log X))))                
\*====================================================== *\

(define sqrt
   { number --> number }
   X -> (error "sqrt(x) for x < 0!~%")   where (< X 0)
   X -> (sqrt-scale X 1))
        
(define sqrt-scale
   { number --> number --> number }
   X F -> (* (sqrt-h X) F)    where (< X 1)
   X F -> (sqrt-scale (/ X 100) (* F 10)))
  
(define sqrt-h 
   { number --> number }
   X -> (sqrt-iter X (/ X 2) X))
   
(define sqrt-iter
   { number --> number --> number --> number}
   X0 X A -> X      where (small-enough? (- X X0))
   X0 X A -> (sqrt-iter X (mean A X0) A))

(define mean
   { number --> number --> number }
   X Y -> (/ (+ Y (/ X Y)) 2))    
\* =====================================================*\
	
(define log
   { number --> number }
   X -> (error "log(x) for x < 0!~%")   where (<= X 0)
   X -> (let Sgn (if (< X 0.5) -1 1)
			  X1 (if (< X 0.5) (/ 1 X) X)       
              (* (log-scale X1 0) Sgn)))
               
(define log-scale
    { number --> number --> number }
    A K -> (+ (log-h A) K)    where (< A 1)
    A K -> (log-scale (/ A (value e)) (+ K 1)))
  
(define log-h
   { number --> number }
   X -> (let X1 (/ (- X 1) (+ X 1))
             X2 (* X1 X1)
             (log-sum 3 X1 X1 X2)))
        
(define log-sum 
   { number --> number --> number --> number --> number }
   I P Sum X2 -> (* Sum 2)  where (small-enough? P)
   I P Sum X2 -> (let Ptmp (* P X2)
                 (log-sum (+ I 2) Ptmp (+ Sum (/ Ptmp I)) X2)))

(define log10
   { number --> number }
   X -> (/ (log X) (value log10)))

(define log2
   { number --> number }
   X -> (/ (log X) (value log2)))
   
(define log'
   { number --> number --> number }
   X B -> (/ (log X) (log B)))  
\* ===================================================== *\

(define sin
   { number --> number }
   X -> (let Sgn (sign X)
            X1  (fmod (abs X) (value pi*2))
            Sgn1 (if (> X1 (value pi)) (- 0 Sgn) Sgn) 
             X2  (if (> X1 (value pi)) (- X1 (value pi)) X1)
             X3  (if (> X2 (value pi/2)) 
                     (- (value pi) X2) X2)
                     (* (sin-h X2) Sgn1)))

(define sin-h
   { number --> number }
   X -> (sin-sum 3 X X -1 (* X X)))
 
(define sin-sum 
   { number --> number --> number --> number --> number --> number }
   I P Sum Sgn X2 ->  Sum  where  (small-enough? P)
   I P Sum Sgn X2 -> (let NewP (/ (/ (* P X2) I) (- I 1))
                         (sin-sum (+ I 2) NewP (+ Sum (* NewP Sgn)) (- 0 Sgn) X2)))

(define cos
   { number --> number }
   X -> (sin (- (value pi/2) X)))
  
(define tan
   { number --> number }
   X -> (/(sin X) (cos X)))
\* ====================================================== *\

(define asin
   { number --> number }
   X -> (error "asin(x) for |x| > 1!~%")   where (> (abs X) 1)
   X -> (let Sgn (sign X)
             X1  (abs X)
             (if (< X1 0.7) 
                 (* (asin-h X1) Sgn) 
                 (* (- (value pi/2) (asin-h (sqrt (- 1.0 (square X1))))) Sgn))))
				   
(define asin-h
   { number --> number } 
   X -> (asin-sum 3 X X (* X X)))
   
(define asin-sum
   { number --> number --> number --> number --> number } 
   I P Sum X2 ->  Sum  where  (small-enough? P)
   I P Sum X2 -> (let Ptmp (/ (* (* P X2) (- I 2)) (- I 1))
                      (asin-sum (+ I 2) Ptmp (+ Sum (/ Ptmp I)) X2)))
                              
(define acos
   { number --> number }
   X -> (error "acos(x) for |x| > 1!~%")   where (> (abs X) 1)
   X -> (- (value pi/2) (asin X)))   
\* ====================================================== *\

(define atan 
   { number --> number } 
   X -> (let Sgn (sign X)
              X1 (abs X)
             (if (> X1 1) 
			      (* (atan-gt1 X1) Sgn)
                  (* (atan-lt1 X1) Sgn))))

(define atan-h
   { number --> number } 
   X -> (atan-sum 3 X X -1 (* X X)))
   
(define atan-sum 
   { number --> number --> number --> number --> number --> number }
   I P Sum Sgn X2 ->  Sum  where  (small-enough? P)
   I P Sum Sgn X2 -> (let Ptmp  (* P X2) 
                         (atan-sum (+ I 2) Ptmp (+ Sum (* (/ Ptmp I) Sgn)) (- 0 Sgn) X2)))

(define atan-lt1
   { number --> number } 
   X -> (let Sgn (sign X)
              X1 (abs X)
             (if (> X1 0.4142) 
                 (* (- (value pi/4) (atan-h (atan-transf X1)) ) Sgn)
                 (*(atan-h X1) Sgn)) ) )

(define atan-gt1
   { number --> number } 
   X -> (- (value pi/2) (atan-lt1 (/ 1 X))))

(define atan-transf 
   { number --> number } 
   X -> (/ (- 1 X) (+ 1 X)))        
 
(define atan2
   { number --> number --> number }
   0 0 -> (error "atan2 - undefined")
   Y 0 -> (value pi/2) where (> Y 0)
   Y 0 -> (- 0 (value pi/2)) where (< Y 0)
   Y X -> (atan (/ Y X)) where (> X 0)
   Y X -> (+ (atan (/ Y X)) (value pi)) where (>= Y 0) 
   Y X -> (- (atan (/ Y X)) (value pi))) \* ( < Y 0) *\
\* ====================================================== *\  
\*================== integer functions ===================*\

 (define even?
   { number --> boolean }
   N -> true where (integer? (/ N 2))
   _ -> false)
 
(define odd?
   { number --> boolean }
   N -> true where (integer? (/ (- N 1) 2))
   N -> false)

(define natural?
   { number --> boolean }
   X -> (and (>= X 0) (integer? X)))

(define positive?
   { number --> boolean }
   N -> (> N 0))

(define negative?
   { number --> boolean }
   N -> (< N 0))
   
(define zero?
   { number --> boolean }
   N -> (= N 0))  
      
\* ====================  shift function ====================== *\

(define rsh
  {number --> number}
  X -> (rsh-h X (/ X 2)))

(define rsh-h
  {number --> number --> number}
   X X/2 -> X/2   where (integer? X/2)
   X _ -> (rsh (- X 1)))    
\* ====================  integer division ====================== *\

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

(define divisible-by?			
   { number --> number --> boolean }
   A B ->  (= (mod-pos A B) 0))
   
(define prime?
   { number --> boolean }
   2 -> true
   N -> false where (even? N)
   N -> (prime'? N 3 (round(sqrt N))))
   
(define prime'?
   { number --> number --> number --> boolean }
   _ K Limit -> true where (> K Limit)
   N K  _    -> false where (divisible-by? N K)
   N K Limit -> (prime'? N (+ K 2) Limit))

(define mod-pos	
   { number --> number --> number } 
   A B -> (let S (pow-2div B A 1)  
			   (div-ww A (* S B) S )))

(define div-ww	
   { number --> number --> number --> number }
   A B 1 -> (if (<= B A) (- A B)  A)
   A B P -> (if (<= B A) (div-ww (- A B) (/ B 2) (/ P 2))
						 (div-ww A (/ B 2) (/ P 2))))  

\* ====================== /mod div mod ========================= *\
 
(define /mod 	\* remainder has same sign as divisor *\
   { number --> number --> (number * number) }
   A B -> (let QR (/-pos (abs A) (abs B))
               (/mod-h B (fst QR) (snd QR) (sign A) (sign B)))) 

(define /mod-h		
   { number --> number --> number --> number --> number --> (number * number) } 
   B Q R  1  1 -> (@p Q R)       	
   B Q R -1 -1 -> (@p Q (- 0 R))
   B Q 0  _  _ -> (@p (- 0 Q) 0)
   B Q R -1  1 -> (@p (- -1 Q)(- B R))  
   B Q R  _  _ -> (@p (- -1 Q)(+ B R)))
			  
(define div
   { number --> number --> number }
   A B -> (let QR (/-pos (abs A) (abs B))
               (div-h (fst QR) (snd QR) (sign A) (sign B))))
   
(define div-h		
   { number --> number --> number --> number --> number }
   Q _ SA SA -> Q
   Q 0  _ _  -> (- 0 Q)
   Q _ _  _  -> (- -1 Q))
   			   
(define mod			\* has same sign as divisor B *\
   { number --> number --> number }
   A B -> (let QR (/-pos (abs A) (abs B))
               (mod-h B (snd QR) (sign A) (sign B))))
   
(define mod-h		
   { number --> number --> number --> number --> number }
   B 0  _  _ -> 0
   B R  1  1 -> R
   B R -1 -1 -> (- 0 R)
   B R  1 -1 -> (+ B R)
   B R -1  1 -> (- B R))
   		   
\* ================== /rem trunc-div rem ===================== *\
          
(define /rem		\* remainder has same sign as dividend *\
   { number --> number --> (number * number) }
   A B -> (let QR (/-pos (abs A) (abs B))
               (/rem-h(fst QR) (snd QR) (sign A) (sign B)))) 

(define /rem-h		
   { number --> number --> number --> number --> (number * number) }
   Q R  1  1 -> (@p Q R)       	
   Q R -1 -1 -> (@p Q (- 0 R))
   Q R  1 -1 -> (@p (- 0 Q) R)  
   Q R  _  _ -> (@p (- 0 Q)(- 0 R)))
			  
(define trunc-div 
   { number --> number --> number }
   A B -> (let QR (/-pos (abs A) (abs B))
			   (if (= (sign A) (sign B)) (fst QR) (- 0 (fst QR)))))
			        				
(define rem			\* has same sign as dividend A *\
   { number --> number --> number }  
   A B -> (let QR (/-pos (abs A) (abs B))
			   (if (>= A 0) (snd QR) (- 0 (snd QR)))))

\* ====================== /% div-eucl % =========================== *\
 	
(define /%		
   { number --> number --> (number * number) }
   A B -> (let QR (/-pos (abs A) (abs B)) 
			  (/%-h B (fst QR) (snd QR) (sign A) (sign B))))

(define /%-h		
   { number --> number --> number --> number --> number --> (number * number) }
   _ Q R  1  1 -> (@p Q R)
   _ Q R  1 -1 -> (@p (- 0 Q) R)
   _ Q 0 -1  1 -> (@p (- 0 Q) 0)
   _ Q 0  _  _ -> (@p Q 0)					  \* -1 -1 *\
   B Q R -1  1 -> (@p (- -1 Q) (- B R))
   B Q R  _  _ -> (@p (+  1 Q) (- 0 (+ B R))))  \* -1 -1 *\
   
(define div-eucl			
   { number --> number --> number }
   A B -> (let QR (/-pos (abs A) (abs B))
			   (adjust-Q (sign A) (sign B)(fst QR))))

(define adjust-Q
   { number --> number --> number --> number}
   1   1 Q -> Q
   -1 -1 Q -> (+ Q 1)
   _  _  Q -> (- 0 Q))
   
(define %			\* always +ve *\
   { number --> number --> number }
   A B ->  (let QR (/-pos (abs A) (abs B))
				(if (> A 0) (snd QR) (- (abs B) (snd QR)))))   

(define gcd
   { number --> number --> number }
   X 0 -> (abs X)
   X Y -> (gcd Y (mod X Y))) 
   
(define lcm
   { number --> number --> number }
   M N -> (abs(/ (* M N) (gcd M N)))) 

				\* moved from auxiliary - now deleted *\
(define !=
   { A --> A --> boolean }
   X Y -> (not (= X Y)))

(define max
   {number --> number --> number}
   M N -> (if (> M N) M N))

(define min
   {number --> number --> number}
   M N -> (if (< M N) M N))

)
