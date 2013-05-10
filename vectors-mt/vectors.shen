(package vector [vector-shell vector-copy vector-extend vector-map vector-map!
                 vector-tail vector-list vector->string cnvector string->vector
                 vector->list list->vector vector-prefix? vector-suffix?
                 vector-every? vector-any? vector-subst vector-reverse
                 vector-dense vector-== vector-index-defined?
                 vector-index-undefined?
                 file->vectorstring file->vectornum vector-append]

(define vector-shell
  {(vector A) --> (vector B)}
    V -> (vector (limit V)))

(define vector-append
  {(vector A) --> (vector A) --> (vector A)}
   V1 V2 -> (list->vector (append (vector->list V1) (vector->list V2))))

(define vector-copy
  {(vector A) --> (vector A)}
   Vector -> (vector-extend Vector 0))

(define vector-dense
  {(vector A) --> (vector A)}
    Vector -> (list->vector (vector->list Vector)))

(define vector-subst
  {A --> A --> (vector A) --> (vector A)}
   X Y V -> (vector-subst' X Y V (limit V)))

(define vector-subst'
  {A --> A --> (vector A) --> number --> (vector A)}
   _ _ V 0 -> V
   X Y V N -> (vector-subst' X Y (trap-error (let VN (<-vector V N)
                                                 (if (= VN Y)
                                                     (vector-> V N X)
                                                     V)) (/. E V)) (- N 1)))

(define vector-extend
  {(vector A) --> number --> (vector A)}
    Vector N -> (vector-extend' Vector (vector (+ (limit Vector) N)) (limit Vector) 1))

(define vector-extend'
  {(vector A) --> (vector A) --> number --> number --> (vector A)}
   Vector Copy N N -> (copy-to-vector Vector Copy N)
   Vector Copy Limit N -> (vector-extend' Vector
                                          (copy-to-vector Vector Copy N)
                                          Limit
                                          (+ N 1)))

(define copy-to-vector
  {(vector A) --> (vector A) --> number --> (vector A)}
   Vector Copy N -> (trap-error (vector-> Copy N (<-vector Vector N)) (/. E Copy)))

(define vector-map
  {(A --> B) --> (vector A) --> (vector B)}
  F Vector -> (vector-map' F Vector (vector-shell Vector) (limit Vector) 1))

(define vector-map'
  {(A --> B) --> (vector A) --> (vector B) --> number --> number --> (vector B)}
   F Vector FVector N N -> (copy-to-FVector F Vector FVector N)
   F Vector FVector Limit N -> (vector-map' F Vector
                                        (copy-to-FVector F Vector FVector N)
                                        Limit
                                        (+ N 1)))

(define copy-to-FVector
  {(A --> B) --> (vector A) --> (vector B) --> number --> (vector B)}
  F Vector FVector N -> (trap-error (vector-> FVector N (F (<-vector Vector N))) (/. E FVector)))

(define vector-map!
  {(A --> A) --> (vector A) --> (vector A)}
  F Vector -> (vector-map!' F Vector (limit Vector) 1))

(define vector-map!'
  {(A --> A) --> (vector A) --> number --> number --> (vector A)}
   F Vector N N -> (copy-to-FVector F Vector Vector N)
   F Vector Limit N -> (vector-map!' F (copy-to-FVector F Vector Vector N) Limit (+ N 1)))

(define vector->list
   {(vector A) --> (list A)}
   Vector -> (vector->list' Vector (limit Vector) []))

(define vector->list'
  {(vector A) --> number --> (list A) --> (list A)}
  Vector 0 L -> L
  Vector N L -> (vector->list' Vector
                               (- N 1)
                               (trap-error [(<-vector Vector N) | L] (/. E L))))

(define list->vector
  {(list A) --> (vector A)}
   L -> (list->vector' L (vector (length L)) 1))

(define list->vector'
  {(list A) --> (vector A) --> number --> (vector A)}
   [] Vector _ -> Vector
   [X | Y] Vector N -> (list->vector' Y (vector-> Vector N X) (+ N 1)))

(define vector-==
  {(vector A) --> (vector B) --> boolean}
   V1 V2 -> (== (vector->list V1) (vector->list V2)))

(define vector->string
   {(vector A) --> string}
    Vector -> (vector->string' Vector 1 (limit Vector) ""))

(define vector->string'
  {(vector A) --> number --> number --> string --> string}
  Vector N N String -> (cnvector String Vector N)
  Vector N Limit String -> (vector->string' Vector
                                    (+ N 1)
                                    Limit
                                    (cnvector String Vector N)))

(define cnvector
  {string --> (vector A) --> number --> string}
  String Vector N -> (trap-error (let X (<-vector Vector N)
                                      VString (cases (symbol? X) (str X)
                                                     (boolean? X) (str X)
                                                     (number? X) (str X)
                                                     true (make-string "~A" X))
                                      (cn String VString)) (/. E String)))

(define string->vector
  {string --> (vector string)}
   "" -> <>
   (@s S Ss) -> (@v S (string->vector Ss)))

(define vector-prefix?
  {(vector A) --> (vector B) --> boolean}
   V1 V2 -> (let L1 (limit V1)
                 L2 (limit V2)
                 (if (> L1 L2)
                     false
                     (vector-prefix'? V1 V2 L1)))                      )

(define vector-prefix'?
  {(vector A) --> (vector B) --> number --> boolean}
  _ _ 0 -> true
  V1 V2 N -> (vector-prefix'? V1 V2 (- N 1))
                      where (cases (vector-index-undefined? V1 N) (vector-index-undefined? V2 N)
                                   (vector-index-undefined? V2 N) false
                                   true (== (<-vector V1 N) (<-vector V2 N)))
  _ _ _ -> false)

(define vector-index-undefined?
  {(vector A) --> number --> boolean}
   V N -> (trap-error (do (<-vector V N) false) (/. E true)))

(define vector-index-defined?
  {(vector A) --> number --> boolean}
   V N -> (trap-error (do (<-vector V N) true) (/. E false)))

(define vector-suffix?
  {(vector A) --> (vector B) --> boolean}
   V1 V2 -> (let L1 (limit V1)
                 L2 (limit V2)
                 (if (> L2 L1)
                     (vector-suffix'? V1 V2 L1 L2)
                     false))                      )

(define vector-suffix'?
  {(vector A) --> (vector B) --> number --> number --> boolean}
  _ _ 0 _ -> true
  V1 V2 L1 L2 -> (vector-suffix'? V1 V2 (- L1 1) (- L2 1))
                      where (trap-error (== (<-vector V1 L1) (<-vector V2 L2)) (/. E false))
  _ _ _ _ -> false)

(define vector-every?
  {(A --> boolean) --> (vector A) --> boolean}
   F V -> (vector-every'? F V (limit V)))

(define vector-every'?
  {(A --> boolean) --> (vector A) --> number --> boolean}
   _ _ 0 -> true
   F V N -> (vector-every'? F V (- N 1))  where (trap-error (F (<-vector V N)) (/. E true))
   _ _ _ -> false)

(define vector-any?
  {(A --> boolean) --> (vector A) --> boolean}
   F V -> (vector-any'? F V (limit V)))

(define vector-any'?
  {(A --> boolean) --> (vector A) --> number --> boolean}
   _ _ 0 -> false
   F V N -> true  where (trap-error (F (<-vector V N)) (/. E false))
   F V N -> (vector-any'? F V (- N 1)))

(define vector-reverse
  {(vector A) --> (vector A)}
    V -> (let Limit (limit V)
              (vector-reverse' V (vector Limit) 1 Limit)))

(define vector-reverse'
  {(vector A) --> (vector A) --> number --> number --> (vector A)}
   _ RV _ 0 -> RV
   V RV NV NVR -> (vector-reverse' V
                                   (trap-error (vector-> RV NVR (<-vector V NV)) (/. E RV))
                                   (+ NV 1)
                                   (- NVR 1)))

(define file->vectornum
  {string --> number --> (vector number)}
   File N -> (let Stream (open file File in)
                 (file->vectornum-help File Stream (read-byte Stream) (vector N) 1 N)))

(define file->vectornum-help
  {string --> (stream in) --> number --> (vector number) --> number --> number --> (vector number)}
   File Stream -1 Vector Index N -> Vector
   File Stream Byte Vector Index N
    -> (file->vectornum-help
        File
        Stream
        (read-byte Stream)
        (trap-error (vector-> Vector Index Byte)
                    (/. E (vector-> (vector-extend Vector N) Index Byte)))
        (+ Index 1) N))

(define file->vectorstring
  {string --> number --> (vector string)}
   File N -> (let Stream (open file File in)
                 (file->vectorstring-help Stream (read-byte Stream) (vector N) 1 N)))

(define file->vectorstring-help
  {(stream in) --> number --> (vector string) --> number --> number --> (vector string)}
   Stream -1 Vector Index N -> Vector
   Stream Byte Vector Index N
    -> (file->vectorstring-help
        Stream
        (read-byte Stream)
        (trap-error (vector-> Vector Index (n->string Byte))
                    (/. E (vector-> (vector-extend Vector N)
                                    Index
                                    (n->string Byte))))
        (+ Index 1) N))
)