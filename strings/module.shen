(register-module [[author: "Willi O Riha"]
                  [load-fn: strings-load]
                  [translate: "macro-def.shen" "auxiliary.shen" "ustring.shen"
                              "str-lib.shen"]])

\* load native definitions of string functions for efficiency *\
(define strings-load-native
  _ _ -> true)

(define strings-load
  -> (do (load/tc - "macro-def.shen")
         (load "auxiliary.shen")
         (load "ustring.shen")
         (load "str-lib.shen")
         (strings-load-native (language) (implementation))))
