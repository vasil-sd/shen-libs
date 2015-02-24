(register-module [[name: binary]
                  [depends: maths defstruct]
                  [desc: "Buffer for storing bytevector"]
                  [load-fn: binary.load]
                  [dump: "binary.shen"]])

\* load native definitions of binary functions for efficiency *\
(define binary.load-native
  "Common Lisp" _ -> true
  _ _ -> true)

(define binary.load
  Dir -> (do (load "binary.shen")
             (binary.load-native (value *language*)
                                 (value *implementation*))
             true))
