(register-module [[name: strings]
                  [author: "Willi O Riha"]
                  [load-fn: strings-load]
                  [dump: "macro-def.shen" "auxiliary.shen" "str-lib.shen"]])

\* load native definitions of string functions for efficiency *\
(define strings-load-native
  _ _ -> true)

(define strings-load
  Dir -> (do (load-with-tc - "macro-def.shen")
             (load "auxiliary.shen")
             (load "str-lib.shen")
             (strings-load-native (value *language*)
                                  (value *implementation*))))
