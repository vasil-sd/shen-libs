(DEFUN binary.arithmetic-shift (X S)
  (ASH X S))

(DEFUN binary.bitwise-and (X Y)
  (LOGAND X Y))

(DEFUN binary.bitwise-ior (X Y)
  (LOGIOR X Y))

(DEFUN binary.int#n (X N)
  (IF (ZEROP N) 
      (LOGAND X 255)
      (LOGAND (ASH X (* -8 N)) 255)))

(DEFUN binary.int#0 (X)
  (LOGAND X 255))

(DEFUN binary.int#1 (X)
  (LOGAND (ASH X -8) 255))

(DEFUN binary.int#2 (X)
  (LOGAND (ASH X -16) 255))

(DEFUN binary.int#3 (X)
  (LOGAND (ASH X -24) 255))

(DEFUN bytevector (N)
  (MAKE-ARRAY N :ELEMENT-TYPE '(UNSIGNED-BYTE 8)))

(DEFUN <-bytevector-u8 (V I)
  (AREF V I))

(DEFUN bytevector-u8-> (V I X)
  (SETF (AREF V I) X))

(DEFUN bytevector-length (V)
  (LENGTH V))

(DEFUN bytevector-replace! (Dst Dst-i Src Src-i N)
  (REPLACE Dst Src :START1 Dst-i :END1 (+ Dst-i N) :START2 Src-i))

(DEFUN bytevector-u16le-> (V I X)
  (SETF (AREF V I) (binary.int#1 X))
  (SETF (AREF V (+ I 1)) (binary.int#2 X)))

(DEFUN bytevector-u32le-> (V I X)
  (SETF (AREF V I) (binary.int#1 X))
  (SETF (AREF V (+ I 1)) (binary.int#2 X))
  (SETF (AREF V (+ I 2)) (binary.int#3 X))
  (SETF (AREF V (+ I 3)) (binary.int#4 X)))

(DEFUN bytevector<-list (list)
  (LET* ((n (LENGTH list))
         (v (bytevector n)))
    (DO ((i 0 (+ i 1))
         (x list (CDR x)))
        ((NULL x) v)
      (bytevector-u8-> v i (CAR x)))))
