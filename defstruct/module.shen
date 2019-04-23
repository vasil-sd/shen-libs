(register-module [[name: defstruct]
                  [author: "Ramil Farkshatov"]
                  [license: "GPLv3+"]
                  [desc: "A macro for defining typed structures."]
                  [load-fn: defstruct-load]
                  [translate: "defstruct.shen"]])

(define defstruct-load
  {A --> boolean}
  _ -> (load/tc - "defstruct.shen"))
