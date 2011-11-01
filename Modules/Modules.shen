(package module-
         [name depends load load-fn dump dump-fn path use-module
          *modules-paths* find-module use-modules dump-module register-module
          forget-module load-native dump-native]

(datatype module-desc
  X : symbol;
  ==============================
  [name : X] : module-desc-item;

  X : (list symbol);
  ================================
  [depends : | X] : module-desc-item;

  X : (list string);
  =============================
  [load : | X] : module-desc-item;

  X : (list string);
  =============================
  [dump : | X] : module-desc-item;

  X : symbol;
  ================================
  [load-fn : X] : module-desc-item;

  X : symbol;
  ================================
  [dump-fn : X] : module-desc-item;

  if (not (element? X [name depends load dump load-fn dump-fn]))
  X : symbol; Y : string;
  ===========================
  [X : Y] : module-desc-item;

  __________________
  [] : module-desc;

  Y : module-desc-item; F : module-desc;
  ======================================
  [Y | F] : module-desc;

  X : module-desc;
  __________________________
  (reverse X) : module-desc;

  X : module-desc; Y : module-desc;
  _________________________________
  (append X Y) : module-desc;

  X : symbol;
  ____________________________________
  (module-fn X) : (string --> boolean);)

(datatype module-types

  __________________
  [] : entry;

  X : symbol; F : module-desc;
  ============================
  [X | F] : entry;

  _______________________________________
  (value *modules*) : (list entry);

  X : (list entry);
  ________________________________________
  (set *modules* X) : (list entry);

  ________________________________________
  (value *loaded-modules*) : (list symbol);

  X : (list symbol);
  ________________________________________
  (set *loaded-modules* X) : (list symbol);

  ________________________________________
  (value *modules-paths*) : (list string);

  X : (list string);
  ________________________________________
  (set *modules-paths* X) : (list string);

  __________________________________
  (value *home-directory*) : string;
  )

(set *loaded-modules* [])
(set *modules* [])
(set *modules-paths* ["root/"])

(define load-native
  {string --> boolean}
  S -> (error "No native loader is defined yet.")
  _ -> false)

(define module-loaded?
  {symbol --> boolean}
  M -> (element? M (value *loaded-modules*)))

(define module-deps
  {module-desc --> (list symbol)}
  [] -> []
  [[depends : | M] | R] -> M
  [_ | R] -> (module-deps R))

(define module-str-list
  {symbol --> module-desc --> (list string)}
  _ [] -> []
  load [[load : | F] | _] -> F
  dump [[dump : | F] | _] -> F
  T [_ | R] -> (module-str-list T R))

(define module-str
  {symbol --> module-desc --> string}
  _ [] -> ""
  K [[K : F] | _] -> F
  K [_ | R] -> (module-str K R))

(define module-sym
  {symbol --> module-desc --> symbol}
  _ [] -> null
  name [[name : F] | _] -> F
  load-fn [[load-fn : F] | _] -> F
  dump-fn [[dump-fn : F] | _] -> F
  T [_ | R] -> (module-sym T R))

(define module-load-fn
  {symbol --> (string --> boolean)}
  X -> (function X) where (not (= (arity X) 1))
  X -> (error "There is no function ~S~%"))

(define module-dump-fn
  {symbol --> (symbol --> symbol --> string --> boolean)}
  X -> (function X) where (not (= (arity X) 3))
  X -> (error "There is no function ~S~%"))

(define module-entry-key
  {entry --> symbol}
  [Key | Def] -> Key)

(define list-modules
  {A --> (list symbol)}
  _ -> (map module-entry-key (value *modules*)))

(define find-module-aux
  {symbol --> (list entry) --> module-desc}
  _ [] -> []
  M [[M | Def] | R] -> Def
  M [_ | R] -> (find-module-aux M R))

(define find-module
  {symbol --> module-desc}
  M -> (find-module-aux M (value *modules*)))

(define set-module-path-if-absent
  {string --> module-desc --> module-desc}
  P D -> (append D [[path : P]]) where (= (module-str path D) "")
  P D -> D)

(define forget-module-manifest
  {symbol --> (list entry) --> (list entry) --> (list entry)}
  M [] Acc -> (set *modules* Acc)
  M [[M | _] | L] Acc -> (forget-module-manifest M L Acc)
  M [X | L] Acc -> (forget-module-manifest M L [X | Acc]))

(define add-module!
  {symbol --> module-desc --> symbol}
  null Def -> (error "Module name is not specified.~%")
  Name Def -> (let D (set-module-path-if-absent (value *home-directory*) Def)
                (do (forget-module-manifest Name (value *modules*) [])
                    (set *modules* [[Name | D] | (value *modules*)])
                    Name)))

(define register-module
  {module-desc --> symbol}
  [] -> (error "Wrong module definition.~%")
  Def -> (error "Module load files or load-fn method is not defined.~%")
         where (and (empty? (module-str-list load Def))
                    (= (module-sym load-fn Def) null))
  Def -> (add-module! (module-sym name Def) Def))

(define module-known?
  {symbol --> boolean}
  M -> false where (= (find-module M) [])
  _ -> true)

(define in-directory
  {string --> (string --> A) --> (error --> A) --> A}
  S F E -> (let Pwd (value *home-directory*)
             (trap-error (let Path (cd S)
                              Ret (F Path)
                              Path2 (cd Pwd)
                           Ret)
                         (/. Err (do (cd Pwd)
                                     (E Err))))))

(define manifest-exists?
  {string --> boolean}
  M P -> (in-directory (cn P (str M))
                       (/. P (let P (open file (cn (str M) ".shen") in)
                                  R (close P)
                                true))
                       (/. E false)))

(define load-manifest*
  {symbol --> string --> boolean}
  M D -> (fail) where (not (manifest-exists? M D))
  M D -> (in-directory (cn D (str M))
                       (/. P (do (load (cn (str M) ".shen"))
                                 (if (module-known? M)
                                     true
                                     (fail))))
                       (/. E (do (error "~A/~A.shen: ~S"
                                        D M (error-to-string E))
                                 false))))

(define load-manifest
  {symbol --> (list string) --> boolean}
  M [] -> false
  M [P | Path] <- (load-manifest* M P)
  M [P | Path] -> (load-manifest M Path))

(define resolve-deps-aux*
  {(list symbol) --> symbol --> module-desc --> (list symbol)
   --> (list symbol)}
  Acc M [] Deps -> (if (load-manifest M (value *modules-paths*))
                       (resolve-deps-aux* Acc M (find-module M) Deps)
                       (error "Unable to find module ~S~%" M))
  Acc M Desc Deps -> (let D (module-deps Desc)
                       (resolve-deps-aux [M | Acc] (append Deps D))))

(define resolve-deps-aux
  {(list symbol) --> (list symbol) --> (list symbol)}
  Acc [] -> Acc
  Acc [D | Deps] -> (resolve-deps-aux [D | Acc] Deps) where (element? D Acc)
  Acc [D | Deps] -> (resolve-deps-aux* Acc D (find-module D) Deps))

(define resolve-deps
  {(list symbol) --> (list symbol)}
  Deps -> (resolve-deps-aux [] Deps))

(define walk-tree*
  {(list symbol) --> (symbol --> boolean) --> (list symbol) --> boolean
   --> boolean}
  _ _ _ false -> false
  [] _ _ Res -> Res
  [M | Mods] Fn Acc Res -> (walk-tree* Mods Fn Acc Res) where (element? M Acc)
  [M | Mods] Fn Acc Res -> (walk-tree* Mods Fn [M | Acc] (Fn M)))

(define walk-tree
  {(list symbol) --> (symbol --> boolean) --> boolean}
  Mods Fn -> (walk-tree* (resolve-deps Mods) Fn [] true))

(define load-module-files
  {(list string) --> boolean}
  [] -> true
  [F | Files] -> (do (load F)
                     (load-module-files Files)))

(define load-module**
  {symbol --> symbol --> (list string) --> boolean}
  _ null [] -> false
  M null Files -> (load-module-files Files)
  M Fn _ -> ((module-load-fn Fn) (value *home-directory*)))

(define load-module*
  {symbol --> module-desc --> boolean}
  _ [] -> false
  M _ -> true where (module-loaded? M)
  M Desc -> (let F (module-sym load-fn Desc)
                 L (module-str-list load Desc)
              (in-directory
                (module-str path Desc)
                (/. _ (if (load-module** M F L)
                          (do (set *loaded-modules*
                                   [M | (value *loaded-modules*)])
                              true)
                          false))
                (/. E (do (error (error-to-string E))
                          false)))))

(define load-module
  {symbol --> boolean}
  M -> (load-module* M (find-module M)))

(define use-modules
  {(list symbol) --> boolean}
  M -> (walk-tree M load-module))

(define dump-native
  {symbol --> symbol --> string --> string --> boolean}
  Lang Impl Dir F -> (error "No native loader is defined yet.")
  _ _ _ _ -> false)

(define dump-module-files
  {symbol --> symbol --> string --> (list string) --> boolean}
  Lang Impl Dir [] -> true
  Lang Impl Dir [F | Files] -> (do (load F)
                                   (dump-native Lang Impl Dir F)
                                   (dump-module-files Lang Impl Dir Files)))

(define dump***
  {symbol --> symbol --> string --> symbol --> symbol --> (list string)
   --> (list string) --> boolean}
  _ _ _ _ null [] [] -> false
  Lang Impl Dir M null [] L-files -> (dump-module-files Lang Impl Dir L-files)
  Lang Impl Dir M null D-files _ -> (dump-module-files Lang Impl Dir D-files)
  Lang Impl Dir M Fn _ _ -> ((module-dump-fn Fn) Lang Impl Dir))

(define dump**
  {symbol --> symbol --> string --> symbol --> module-desc --> boolean}
  _ _ _ _ [] -> false
  Lang Impl Dir M Desc -> (let F (module-sym dump-fn Desc)
                               D (module-str-list dump Desc)
                               L (module-str-list load Desc)
                            (in-directory
                              (module-str path Desc)
                              (/. _ (dump*** Lang Impl Dir M F D L))
                              (/. E (do (error (error-to-string E))
                                        false)))))

(define dump*
  {symbol --> string --> symbol --> boolean}
  Lang Impl Dir M -> (dump** Lang Impl Dir M (find-module M)))

(define dump-module
  {symbol --> symbol --> symbol --> string --> boolean}
  M Lang Impl Dir -> (walk-tree [M] (dump* Lang Impl Dir)))

(define forget-module
  {symbol --> boolean --> boolean}
  M true -> (do (forget-module-manifest M (value *modules*) [])
                (forget-module M false))
  M false -> (set *loaded-modules* (remove M (value *loaded-modules*))))
)
