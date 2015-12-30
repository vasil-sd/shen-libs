(define bytevector
  N -> (vector N))

(define <-bytevector-u8
  V I -> (<-vector V (+ I 1)))

(define bytevector-u8->
  B I X -> (vector-> B (+ I 1) X))

(define bytevector<-list'
  [] V _ -> V
  [X | Xs] V I -> (do (bytevector-u8-> V I X)
                      (bytevector<-list' Xs V (+ I 1))))

(define bytevector<-list
  List -> (let N (length List)
               V (bytevector N)
            (bytevector<-list' List V 0)))

(define bytevector-length
  V -> (limit V))

(define bytevector-copy
  V -> (let N (bytevector-length V)
            New (bytevector N)
            . (bytevector-replace! New 0 V 0 N)
         New))

(define bytevector-replace!'
  Dst Dst-i Src Src-i N N -> true
  Dst Dst-i Src Src-i I N ->
  (do (bytevector-u8-> Dst Dst-i (<-bytevector-u8 Src Src-i))
      (bytevector-replace!' Dst (+ Dst-i 1) Src (+ Src-i 1) (+ I 1) N)))

(define move-subbytevector!-
  V I1 I2 N N -> true
  V I1 I2 I N -> (do (bytevector-u8-> V I1 (<-bytevector-u8 V I2))
                     (move-subbytevector+!' V (- I1 1) (- I2 1) (+ I 1) N)))

(define move-subbytevector!
  V I I N -> true
  V I1 I2 N -> (bytevector-replace!' V I1 V I2 0 N) where (< I1 I2)
  V I1 I2 N -> (move-subbytevector!- V (- (+ I1 N) 1) (- (+ I2 N) 1) 0 N))

(define bytevector-replace!
  V Dst-off V Src-off Len -> (move-subbytevector! V Dst-off Src-off Len)
  Dst Dst-off Src Src-off Len -> (bytevector-replace!'
                                  Dst Dst-off Src Src-off 0 Len))

(define bytevector-u16le->
  B I X -> (do (vector-> B (+ I 1) (binary.int#1 X))
               (vector-> B (+ I 2) (binary.int#2 X))))

(define bytevector-u32le->
  B I X -> (do (vector-> B (+ I 1) (binary.int#1 X))
               (vector-> B (+ I 2) (binary.int#2 X))
               (vector-> B (+ I 3) (binary.int#3 X))
               (vector-> B (+ I 4) (binary.int#4 X))))
