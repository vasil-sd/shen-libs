(package binary [mod div power even?]

(define uint8? X -> (<= X 255))
(define uint16? X -> (<= X 65535))
(define uint32? X -> (<= X 4294967296))
(define uint64? X -> (<= X 18446744073709551615))

(define int8? X -> (and (>= X -128) (<= X 127)))
(define int16? X -> (and (>= X -32768) (<= X 32767)))
(define int32? X -> (and (>= X -2147483648) (<= X 2147483647)))
(define int64? X -> (and (>= X -9223372036854775808)
                         (<= X 9223372036854775807)))

(define uint-size
  X -> (error "Positive value expected") where (< X 0) 
  X -> 1 where (uint8? X)
  X -> 2 where (uint16? X)
  X -> 4 where (uint32? X)
  \\X -> 8 where (uint64? X)
  X -> (error "Integer overflow: ~A > 2**32" X))

(define sint-size
  X -> 1 where (int8? X)
  X -> 2 where (int16? X)
  X -> 4 where (int32? X)
  \\X -> 8 where (int64? X)
  X -> (error "Integer overflow: ~A > 2**32" X))

(define arithmetic-shift
  X S -> (* X (power 2 S)) where (>= S 0)
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

(define int#1
  X -> (mod X 256))

(define int#2
  X -> (div (mod X 65536) 256))

(define int#3
  X -> (div (mod X 16777216) 65536))

(define int#4
  X -> (div (mod X 4294967296) 16777216))

)
