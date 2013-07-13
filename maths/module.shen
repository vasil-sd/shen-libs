(register-module [[name: maths]
                  [author: "Willi O Riha"]
                  [load-fn: maths-load]
                  [dump: "macro-def.shen" "maths-lib.shen"]])

\* load native definitions of math functions for efficiency *\
(define maths-load-native
  _ _ -> true)

(define maths-load
  Dir -> (let Tc (if (value shen.*tc*) + -)
           (trap-error (do (tc -)
                           (load "macro-def.shen")
                           (tc +)
                           (load "maths-lib.shen")
                           (maths-load-native (value *language*)
                                              (value *implementation*))
                           (tc Tc)
                           true)
                       (/. E (do (tc Tc)
                                 (error (error-to-string E)))))))
