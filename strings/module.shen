(register-module [[name: strings]
                  [author: "Willi O Riha"]
                  [load-fn: strings-load]
                  [dump: "macro-def.shen" "auxiliary.shen" "str-lib.shen"]])

\* load native definitions of string functions for efficiency *\
(define strings-load-native
  _ _ -> true)

(define strings-load
  Dir -> (let Tc (if (value shen.*tc*) + -)
           (trap-error (do (tc -)
                           (load "macro-def.shen")
                           (tc +)
                           (load "auxiliary.shen")
                           (load "str-lib.shen")
                           (strings-load-native (value *language*)
                                                (value *implementation*))
                           (tc Tc)
                           true)
                       (/. E (do (tc Tc)
                                 (error (error-to-string E)))))))
