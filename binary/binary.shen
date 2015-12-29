(package binary [mod div power even?]

(define bytevector
  N -> (vector N))

(define bytevector->
  V I X -> (vector-> V (+ I 1) X))

(define <-bytevector
  V I -> (<-vector V (+ I 1)))

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
  (do (bytevector-> Dst Dst-i (<-bytevector Src Src-i))
      (bytevector-replace!' Dst (+ Dst-i 1) Src (+ Src-i 1) (+ I 1) N)))

(define move-subbytevector!-
  V I1 I2 N N -> true
  V I1 I2 I N -> (do (bytevector-> V I1 (<-bytevector V I2))
                     (move-subbytevector+!' V (- I1 1) (- I2 1) (+ I 1) N)))

(define move-subbytevector!
  V I I N -> true
  V I1 I2 N -> (bytevector-replace!' V I1 V I2 0 N) where (< I1 I2)
  V I1 I2 N -> (move-subbytevector!- V (- (+ I1 N) 1) (- (+ I2 N) 1) 0 N))

(define bytevector-replace!
  V Dst-off V Src-off Len -> (move-subbytevector! V Dst-off Src-off Len)
  Dst Dst-off Src Src-off Len -> (bytevector-replace!'
                                  Dst Dst-off Src Src-off 0 Len))

(define min
  X Y -> X where (< X Y)
  _ Y -> Y)

(defstruct bytestream
  (size number)
  (blocks (list bytevector)))

(define mkstream
  -> (mk-bytestream 0 []))

(define bytestream-add-block!
  Block S -> (let N (bytevector-length Block)
                  . (bytestream-blocks-> S [Block | (bytestream-blocks S)])
                  . (bytestream-size-> S (+ N (bytestream-size S)))
               S))

(define stream-put-blocks
  V [] Off -> V
  V [B | Bs] Off -> (let Bn (bytevector-length B)
                         . (bytevector-replace! V Off B 0 Bn)
                      (stream-put-blocks V Bs (+ Off Bn))))

(define bytevector-bytestream->
  V S Off -> (stream-put-blocks V (reverse (bytestream-blocks S)) Off))

(define bytevector-from-bytestream
  S -> (bytevector-bytestream-> (bytevector (bytestream-size S)) S 0))

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

(define put-u8
  X B -> (let V (bytevector 1)
              . (bytevector-> V 0 X)
           (bytestream-add-block! V B)))

(define put-i8
  X B -> (put-u8 X B))

(define put-u16
  X B -> (let V (bytevector 2)
              . (bytevector-u16le-> V 0 X)
           (bytestream-add-block! V B)))

(define put-i16
  X B -> (put-u16 X B))

(define put-u32
  X B -> (let V (bytevector 4)
              . (bytevector-u32le-> V 0 X)
           (bytestream-add-block! V B)))

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

(define str-bytelen'
  S I -> (trap-error (do (pos S I)
                         (str-bytelen' S (+ I 1)))
                     (/. E I)))

(define str-bytelen
  S -> (str-bytelen' S 0))

(define bytevector-substring->'
  V _ _ _ N N -> V
  V V-off S S-off I N ->
  (do (bytevector-> V V-off (string->n (pos S S-off)))
      (bytevector-substring->' V (+ V-off 1) S (+ S-off 1) (+ I 1) N)))

(define bytevector-substring->
  V V-off S S-off N -> (bytevector-substring->' V V-off S S-off 0 N))

(define bytevector-string->
  V Off S -> (bytevector-substring-> V Off S 0 (str-bytelen S)))

(define put-string
  S B -> (let N (str-bytelen S)
              V (bytevector-substring-> (bytevector N) 0 S 0 N)
           (bytestream-add-block! V B)))

(define put-bytevector
  X B -> (bytestream-add-block! (bytevector-copy X) B))

(define put-bytestream'
  [] Stream -> Stream
  [B | Bs] Stream -> (bytestream-add-block! B Stream))

(define put-bytestream
  X B -> (put-bytestream' (reverse (bytestream-blocks X)) B))

(define string-from-bytevector'
  N N V S -> S
  I N V S -> (let S (cn S (n->string (<-bytevector V I)))
               (string-from-bytevector' (+ I 1) N V S)))

(define string-from-bytevector
  I N V -> (string-from-bytevector' I N V ""))

(define string-from-buf
  B -> (string-from-bytevector 1 (+ (buf-size B) 1) (buf-buf B)))

(define str-from-list'
  [] S -> S
  [X | Y] S -> (str-from-list' Y (cn S (n->string X))))

(define str-from-list
  L -> (str-from-list' L ""))

(define write-subbytevector'
  V Off N N F -> true
  V Off I N F -> (do (write-byte (<-bytevector V Off) F)
                     (write-subbytevector' V (+ Off 1) (+ I 1) N F)))

(define write-subbytevector
  V Off N F -> (write-subbytevector' V Off 0 N F))

(define write-bytestream'
  [] _ -> true
  [B | Bs] F -> (do (write-subbytevector B 0 (bytevector-length B) F)
                    (write-bytestream' Bs F)))

(define write-bytestream
  Stream F -> (write-bytestream' (reverse (bytestream-blocks Stream)) F))

(define call-with-output-file
  File Proc -> (let F (open File out)
                    R (trap-error (Proc F)
                                  (/. E (do (close F)
                                            (error (error-to-string E)))))
                    . (close F)
                 R))

(define bytevector-to-file
  File V -> (call-with-output-file
             File
             (write-subbytevector V 0 (bytevector-length V))))

(define bytestream-to-file
  File Stream -> (call-with-output-file
                  File (/. F (write-bytestream Stream F)))))

(define binary.test-byte
  -> (let A (binary.mkstream)
          . (binary.put-u8 0 A)
       A))

(define binary.test-put-vec
  -> (let A (binary.mkstream)
          V (binary.bytevector 3)
          . (binary.bytevector-> V 0 3)
          . (binary.bytevector-> V 1 6)
          . (binary.bytevector-> V 2 9)
          . (binary.put-u8 10 A)
          . (binary.put-u8 15 A)
          . (output "A: ~A~%V: ~A~%" A V)
          . (binary.put-bytevector V A)
       (binary.bytevector-from-bytestream A)))

(define binary.test-put-stream
  -> (let A (binary.mkstream)
          B (binary.mkstream)
          . (binary.put-u8 10 A)
          . (binary.put-u8 15 A)
          . (binary.put-u8 7 B)
          . (output "A: ~A~%B: ~A~%" A B)
          . (binary.put-bytestream B A)
       (binary.bytevector-from-bytestream A)))

(define binary.test-write
  -> (let B (binary.mkstream)
          . (binary.put-u8 10 B)
          . (binary.put-u8 15 B)
          . (binary.put-u8 255 B)
          . (binary.put-u8 128 B)
          . (binary.put-u8 200 B)
          . (binary.put-u8 35 B)
          . (binary.put-u16 365 B)
          V (binary.bytevector-from-bytestream B)
          . (output "buf: ~A~%" B)
          . (output "buf: ~A~%" V)
          . (binary.bytestream-to-file "buf.bin" B)
       _))
