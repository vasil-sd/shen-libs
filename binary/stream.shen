(package binary [bytevector bytevector-length bytevector-copy
                 bytevector-replace! <-bytevector-u8 bytevector-u8->
                 bytevector-u16le-> bytevector-u32le->]

(defstruct bytestream
  (size number)
  (blocks (list bytevector)))

(define mkstream
  -> (mk-bytestream 0 []))

(define bytestream-add-block!
  Block Stream -> (let N (bytevector-length Block)
                       Blocks (bytestream-blocks Stream)
                       Size (bytestream-size Stream)
                       . (bytestream-blocks-> Stream [Block | Blocks])
                       . (bytestream-size-> Stream (+ N Size))
                    Stream))

(define stream-blocks-to-bytevector
  V [] Off -> V
  V [B | Bs] Off -> (let Bn (bytevector-length B)
                         . (bytevector-replace! V Off B 0 Bn)
                      (stream-blocks-to-bytevector V Bs (+ Off Bn))))

(define bytevector-bytestream->
  V Stream Off -> (let Blocks (reverse (bytestream-blocks Stream))
                    (stream-blocks-to-bytevector V Blocks Off)))

(define bytevector-from-bytestream
  Stream -> (let V (bytevector (bytestream-size Stream))
              (bytevector-bytestream-> V Stream 0)))

(define put-u8
  X Stream -> (let V (bytevector 1)
                   . (bytevector-u8-> V 0 X)
                (bytestream-add-block! V Stream)))

(define put-i8
  X Stream -> (put-u8 X Stream))

(define put-u16
  X Stream -> (let V (bytevector 2)
                   . (bytevector-u16le-> V 0 X)
                (bytestream-add-block! V Stream)))

(define put-i16
  X Stream -> (put-u16 X Stream))

(define put-u32
  X Stream -> (let V (bytevector 4)
                   . (bytevector-u32le-> V 0 X)
                (bytestream-add-block! V Stream)))

(define put-i32
  X Stream -> (put-u32 X Stream))

(define put-uint
  X Stream -> (put-u8 X Stream) where (< X 256)
  X Stream -> (put-u16 X Stream) where (< X 65536)
  X Stream -> (put-u24 X Stream) where (< X 166777216)
  X Stream -> (put-u32 X Stream) where (< X 4294967296)
  X _ -> (error "Integer overflow: ~A > 2**32" X))

(define str-bytelen'
  S I -> (trap-error (do (pos S I)
                         (str-bytelen' S (+ I 1)))
                     (/. E I)))

(define str-bytelen
  S -> (str-bytelen' S 0))

(define bytevector-substring->'
  V _ _ _ N N -> V
  V V-off S S-off I N ->
  (do (bytevector-u8-> V V-off (string->n (pos S S-off)))
      (bytevector-substring->' V (+ V-off 1) S (+ S-off 1) (+ I 1) N)))

(define bytevector-substring->
  V V-off S S-off N -> (bytevector-substring->' V V-off S S-off 0 N))

(define bytevector-string->
  V Off S -> (bytevector-substring-> V Off S 0 (str-bytelen S)))

(define put-string
  S Stream -> (let N (str-bytelen S)
                   V (bytevector-substring-> (bytevector N) 0 S 0 N)
                (bytestream-add-block! V Stream)))

(define put-bytevector
  X Stream -> (bytestream-add-block! (bytevector-copy X) Stream))

(define put-bytestream'
  [] Stream -> Stream
  [B | Bs] Stream -> (do (bytestream-add-block! B Stream)
                         (put-bytestream' Bs Stream)))

(define put-bytestream
  X Stream -> (put-bytestream' (reverse (bytestream-blocks X)) Stream))

(define string-from-bytevector'
  N N V S -> S
  I N V S -> (let S (cn S (n->string (<-bytevector-u8 V I)))
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
  V Off I N F -> (do (write-byte (<-bytevector-u8 V Off) F)
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
          V (bytevector 3)
          . (bytevector-u8-> V 0 3)
          . (bytevector-u8-> V 1 6)
          . (bytevector-u8-> V 2 9)
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
