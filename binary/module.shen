(register-module [[depends: "maths" "defstruct"]
                  [desc: "Binary serialization."]
                  [load-fn: binary.load]
                  [translate: "math.shen" "bytevector.shen" "binary.shen"]])

\* load native definitions of binary functions for efficiency *\
(define binary.load-native
  "Common Lisp" _ -> (do (load "bytevector.shen")
                         ((protect LOAD) (cn (value *home-directory*)
                                             "native.lisp")))
  _ _ -> (do (load "math.shen")
             (load "bytevector.shen")))

(define binary.load
  -> (do (binary.load-native (language) (implementation))
         (load "stream.shen")
         true))
