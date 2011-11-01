(package module-
         [name depends load load-fn dump dump-fn path use-module
          *modules-paths* find-module use-module dump-module register-module
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
  {symbol --> (symbol --> string --> boolean)}
  X -> (function X) where (not (= (arity X) 2))
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

(define add-module!
  {symbol --> module-desc --> symbol}
  null Def -> (error "Module name is not specified.~%")
  Name Def -> (let D (set-module-path-if-absent (value *home-directory*) Def)
                (do (set *modules* [[Name | D] | (value *modules*)])
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

(define load-module-manifest
  {symbol --> (list string) --> boolean}
  M [] -> false
  M [P | Path] -> (let Pwd (value *home-directory*)
                    (trap-error
                      (do (cd (cn P (str M)))
                          (load (cn (str M) ".shen"))
                          (cd Pwd)
                          (module-known? M))
                      (/. E (do (cd Pwd)
                                false)))))

(define resolve-deps-aux*
  {(list symbol) --> symbol --> module-desc --> (list symbol)
   --> (list symbol)}
  Acc M [] Deps -> (if (load-module-manifest M (value *modules-paths*))
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

(define walk-tree-aux
  {(list symbol) --> (symbol --> boolean) --> (list symbol) --> boolean
   --> boolean}
  _ _ _ false -> false
  [] _ _ Res -> Res
  [M | Mods] Fn Acc Res -> (walk-tree-aux Mods Fn Acc Res)
                           where (element? M Acc)
  [M | Mods] Fn Acc Res -> (walk-tree-aux Mods Fn [M | Acc] (Fn M)))

(define walk-module-tree
  {(list symbol) --> (symbol --> boolean) --> boolean}
  Mods Fn -> (walk-tree-aux (resolve-deps Mods) Fn [] true))

(define load-module-files
  {(list string) --> boolean}
  [] -> true
  [F | Files] -> (do (load F)
                     (load-module-files Files)))

(define load-module-aux*
  {symbol --> symbol --> (list string) --> boolean}
  _ null [] -> false
  M null Files -> (load-module-files Files)
  M Fn _ -> ((module-load-fn Fn) (value *home-directory*)))

(define load-module-aux
  {symbol --> module-desc --> boolean}
  _ [] -> false
  M _ -> true where (module-loaded? M)
  M Desc -> (let F (module-sym load-fn Desc)
                 L (module-str-list load Desc)
                 Pwd (value *home-directory*)
              (do (cd (module-str path Desc))
                  (let Res (load-module-aux* M F L)
                    (do (cd Pwd)
                        (set *loaded-modules* [M | (value *loaded-modules*)])
                        Res)))))

(define load-module
  {symbol --> boolean}
  M -> (load-module-aux M (find-module M)))

(define use-module
  {symbol --> boolesn}
  M -> (walk-module-tree [M] load-module))

(define dump-native
  {symbol --> string --> string --> boolean}
  Target Dir F -> (error "No native loader is defined yet.")
  _ _ _ -> false)

(define dump-module-files
  {symbol --> string --> (list string) --> boolean}
  Target Dir [] -> true
  Target Dir [F | Files] -> (do (load F)
                                (dump-native Target Dir F)
                                (dump-module-files Target Dir Files)))

(define dump-module-aux*
  {symbol --> string --> symbol --> symbol --> (list string)
   --> (list string) --> boolean}
  _ _ _ null [] [] -> false
  Target Dir M null [] L-files -> (dump-module-files Target Dir L-files)
  Target Dir M null D-files _ -> (dump-module-files Target Dir D-files)
  Target Dir M Fn _ _ -> ((module-dump-fn Fn) Target Dir))

(define dump-module-aux
  {symbol --> string --> symbol --> module-desc --> boolean}
  _ _ _ [] -> false
  Target Dir M Desc -> (let F (module-sym dump-fn Desc)
                            D (module-str-list dump Desc)
                            L (module-str-list load Desc)
                            Pwd (value *home-directory*)
                         (do (cd (module-str path Desc))
                             (let Res (dump-module-aux* Target Dir M F D L)
                               (do (cd Pwd)
                                   Res)))))

(define dump-module*
  {symbol --> string --> symbol --> boolean}
  Target Dir M -> (dump-module-aux Target Dir M (find-module M)))

(define dump-module
  {symbol --> symbol --> string --> boolean}
  M Target Dir -> (walk-module-tree [M] (dump-module* Target Dir)))

(define forget-module-manifest
  {symbol --> (list entry) --> (list entry) --> (list entry)}
  M [] Acc -> (set *modules* Acc)
  M [[M | _] | L] Acc -> (forget-module-manifest M L Acc)
  M [X | L] Acc -> (forget-module-manifest M L [X | Acc]))

(define forget-module
  {symbol --> boolean --> boolean}
  M true -> (do (forget-module-manifest M (value *modules*) [])
                (forget-module M false))
  M false -> (set *loaded-modules* (remove M (value *loaded-modules*))))
)
