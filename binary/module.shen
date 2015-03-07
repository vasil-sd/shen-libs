(register-module [[depends: "maths" "defstruct"]
                  [desc: "Binary serialization."]
                  [load-fn: binary.load]
                  [translate: "binary.shen"]])

\* load native definitions of binary functions for efficiency *\
(define binary.load-native
  "Common Lisp" _ -> true
  _ _ -> true)

(define binary.load
  -> (do (load "binary.shen")
         (binary.load-native (language) (implementation))
         true))
