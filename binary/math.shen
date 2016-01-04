(package binary [mod div power even?]
(define arithmetic-shift'
  X 0 -> X
  X S -> (arithmetic-shift' (* X 2) (- S 1)))

(define arithmetic-shift
  X 0 -> X
  X S -> (arithmetic-shift-+ X S) where (> S 0)
  X S -> (div X (power 2 (- 0 S))))

(define bitwise-and'
  0 _ R _ -> R
  _ 0 R _ -> R
  X Y R L -> (bitwise-and' (div X 2) (div Y 2) R (* L 2))
             where (or (even? X) (even? Y))
  X Y R L -> (bitwise-and' (div X 2) (div Y 2) (+ R L) (* L 2)))

(define bitwise-and
  X Y -> (bitwise-and' X Y 0 1))

(define bitwise-ior'
  0 0 R _ -> R
  X Y R L -> (bitwise-ior' (div X 2) (div Y 2) R (* L 2))
             where (and (even? X) (even? Y))
  X Y R L -> (bitwise-ior' (div X 2) (div Y 2) (+ R L) (* L 2)))

(define bitwise-ior
  X Y -> (bitwise-ior' X Y 0 1))

(define int#n
  X 0 -> (mod X 256)
  X N -> (let Y (power 256 N)
           (div (mod X (* Y 256)) Y)))

(define int#0
  X -> (mod X 256))

(define int#1
  X -> (div (mod X 65536) 256))

(define int#2
  X -> (div (mod X 16777216) 65536))

(define int#3
  X -> (div (mod X 4294967296) 16777216))

)
