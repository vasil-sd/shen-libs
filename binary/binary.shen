(package binary [mod div power even?]

(define bytevector
  N -> (vector N))

(define bytevector->
  V I X -> (vector-> V I X))

(define <-bytevector
  V I -> (<-vector V I))

(defstruct buf
  (size number)
  (dsize number)
  (buf bytevector))

(define mkbuf
  Dsize -> (mk-buf 0 Dsize (bytevector Dsize)))

(define move-subbytevector
  I1 I1 V -> V
  I1 I2 V -> (do (bytevector-> V I2 (<-bytevector V (- I2 1)))
                 (move-subbytevector I1 (- I2 1) V)))

(define copy-subbytevector
  I N To From -> To where (> I N)
  I N To From -> (do (bytevector-> To I (<-bytevector From I))
                     (copy-subbytevector (+ I 1) N To From)))

(define min
  X Y -> X where (< X Y)
  _ Y -> Y)

(define realloc-bytevector
  Used-size New-size V -> (let N (min New-size (limit V))
                               R (bytevector New-size)
                            (copy-subbytevector 1 Used-size R V)))

(define realloc-size'
  Size N B -> N where (<= Size N)
  Size N B -> (realloc-size' Size (+ N (buf-dsize B)) B))

(define realloc-size
  Size B -> (realloc-size' Size (buf-dsize B) B))

(define maybe-realloc-buf
  Dsize B -> (let S (buf-size B)
                  N (realloc-size (+ Dsize S) B)
               (if (> N (limit (buf-buf B)))
                   (buf-buf-> B (realloc-bytevector S N (buf-buf B)))
                   B)))

(define uint-size
  X -> (error "Positive value expected") where (< X 0) 
  X -> 1 where (< X 256) 
  X -> 2 where (< X 65536)
  X -> 4 where (< X 4294967296)
  \\X -> 8 where (< X 18446744073709551615)
  X -> (error "Integer overflow: ~A > 2**32" X))


(define sint-size
  X -> 1 where (and (>= X -128) (<= X 127))
  X -> 2 where (and (>= X -32768) (<= X 32767))
  X -> 4 where (and (>= X -2147483648) (<= X 2147483647))
  \\X -> 8 where (and (>= X -9223372036854775808) (<= X 9223372036854775807))
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

(define bytevector-u8->
  B Off X -> (do (bytevector-> B Off (int#1 X))
                  B))

(define bytevector-u16le->
  B Off X -> (do (bytevector-> B Off (int#1 X))
                 (bytevector-> B (+ Off 1) (int#2 X))
                 B))

(define bytevector-u32le->
  B Off X -> (do (bytevector-> B Off (int#1 X))
                 (bytevector-> B (+ Off 1) (int#2 X))
                 (bytevector-> B (+ Off 2) (int#3 X))
                 (bytevector-> B (+ Off 3) (int#4 X))
                 B))

(define bytevector-replace
  N N _ _ To -> To
  I N From Off To -> (do (bytevector-> To Off (<-bytevector From I))
                         (bytevector-replace (+ I 1) N From (+ Off 1) To)))

(define put-u8
  X B -> (do (maybe-realloc-buf 1 B)
             (bytevector-u8-> (buf-buf B) (+ (buf-size B) 1) X)
             (buf-size-> B (+ (buf-size B) 1))
             B))

(define put-i8
  X B -> (put-u8 X B))

(define put-u16
  X B -> (let . (maybe-realloc-buf 2 B)
              I (buf-size B)
              . (bytevector-u16le-> (buf-buf B) (+ I 1) X)
              . (buf-size-> B (+ I 2))
           B))

(define put-i16
  X B -> (put-u16 X B))

(define put-u32
  X B -> (let . (maybe-realloc-buf 4 B)
              I (buf-size B)
              . (bytevector-u32le-> (buf-buf B) (+ I 1) X)
              . (buf-size-> B (+ I 4))
           B))

(define put-i32
  X B -> (put-u32 X B))

(define put-uint
  X B -> (put-u8 X B) where (< X 256)
  X B -> (put-u16 X B) where (< X 65536)
  X B -> (put-u24 X B) where (< X 166777216)
  X B -> (put-u32 X B) where (< X 4294967296)
  X _ -> (error "Integer overflow: ~A > 2**32" X))

(define put-char
  X B -> (put-u8 X B) where (<= X 255)
  X B -> (error "Non latin-1 is not yet supported."))

(define put-string'
  S I B -> (trap-error (do (put-char (string->n (pos S I)) B)
                           (put-string' S (+ I 1) B))
                       (/. E B)))

(define put-string
  S B -> (put-string' S 0 B))

(define put-bytevector
  X B -> (let N (limit X)
              . (maybe-realloc-buf N B)
              I (buf-size B)
              . (bytevector-replace 1 (+ N 1) X (+ I 1) (buf-buf B))
              . (buf-size-> B (+ I N))
           B))

(define put-buf
  X B -> (let N (buf-size X)
              . (maybe-realloc-buf N B)
              I (buf-size B)
              . (bytevector-replace 1 (+ N 1) (buf-buf X) (+ I 1) (buf-buf B))
              . (buf-size-> B (+ I N))
           B))

(define bytevector-move
  V N _ N -> V
  V I D N -> (do (bytevector-> V I (<-bytevector V (+ I D)))
                 (bytevector-move V (+ I 1) D N)))

(define buf-cut
  Off Size B -> (let S (+ (- (buf-size B) Size) 1)
                     . (bytevector-move (buf-buf B) Off Size S)
                     . (buf-size-> B (- (buf-size B) Size))
                  B))

(define string-from-bytevector'
  N N V S -> S
  I N V S -> (let S (cn S (n->string (<-bytevector V I)))
               (string-from-bytevector' (+ I 1) N V S)))

(define string-from-bytevector
  I N V -> (string-from-bytevector' I N V ""))

(define string-from-buf
  B -> (string-from-bytevector 1 (+ (buf-size B) 1) (buf-buf B)))

)

(define binary.test-scratch
  -> (let A (binary.mkbuf 2)
          . (binary.put-u8 0 A)
       A))

(define binary.test-put-vec
  -> (let A (binary.mkbuf 2)
          V (binary.bytevector 3)
          . (binary.bytevector-> V 1 3)
          . (binary.bytevector-> V 2 6)
          . (binary.bytevector-> V 3 9)
          . (binary.put-u8 10 A)
          . (binary.put-u8 15 A)
          . (output "A: ~A~%V: ~A~%" A V)
          . (binary.put-bytevector V A)
       A))

(define binary.test-put-buf
  -> (let A (binary.mkbuf 2)
          B (binary.mkbuf 8)
          . (binary.put-u8 10 A)
          . (binary.put-u8 15 A)
          . (binary.put-u8 7 B)
          . (output "A: ~A~%B: ~A~%" A B)
          . (binary.put-buf B A)
       A))

(define binary.test-write
  -> (let B (binary.mkbuf 16)
          . (binary.put-u8 10 B)
          . (binary.put-u8 15 B)
          . (binary.put-u8 255 B)
          . (binary.put-u8 128 B)
          . (binary.put-u8 200 B)
          . (binary.put-u8 35 B)
          . (binary.put-u16 365 B)
          . (output "buf: ~A~%" (binary.buf-buf B))
          . (write-to-file "buf.bin" (binary.string-from-buf B))
       _))


(define str-from-list'
  [] S -> S
  [X | Y] S -> (str-from-list' Y (cn S (n->string X))))

(define str-from-list
  L -> (str-from-list' L ""))

(define write-binary-to-file
  File L -> (let F (open file File out)
                 . (pr (str-from-list L) F)
                 . (close F)
              true))
