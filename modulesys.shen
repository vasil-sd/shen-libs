\* modulesys - public domain module system for Shen

  ## Description

  Module system is a tool for managing Shen libraries.

  ## Basic usage

  * `(module.use [Mod1 ...])` or `(use-modules [Mod1 ...])`
  loads given modules with all their dependencies. Any module already loaded
  won't be loaded twice.

  * `(module.reload Mod1)`
  reloads given module.

  * `(module.files-to-translate Mod Language Implementation)`
  returns a list of module Mod files to translate which can be passed to
  a language dependent translator. Note that it loads module with all its
  dependencies first.

  * `(module.add-path Dir)`
  adds directory to a list where modules are searched.

  ## Module definition

  Sample contents of `mod1/module.shen` where `mod1` is module name:

    (register-module [[load: "file1" "file2"]
                      [depends: "mod3" mod4]])
*\

(package module [use-modules load/tc register-module

                 name depends translate-depends load translate load-fn
                 unload-fn translate-fn]

(set *paths* [])
(set *list* [])
(set *db* (trap-error (shen.dict 256) (/. _ (vector 256))))
(set *fields* [path load translate depends translate-depends load-fn unload-fn
               translate-fn])

(define add-path
  X -> (set *paths* [X | (value *paths*)])
       where (not (element? X (value *paths*))))

(define rm-path
  X -> (set *paths* (remove X (value *paths*))))

(define normalize-id
  X -> X where (string? X)
  X -> (str X) where (symbol? X))

(define normalize-ids
  X -> (map (function normalize-id) X) where (cons? X)
  X -> [(normalize-id X)])

(define add-field
  M Field Data Db -> (do (put M fields (adjoin Field (get M fields Db)) Db)
                         (put M Field Data Db)
                         true))

(define add-module-field
  M [Field : X] -> (add-field M Field X (value *db*))
                   where (element? Field [load-fn unload-fn translate-fn])
  M [depends : | Xs] -> (add-field M depends (normalize-ids Xs) (value *db*))
  M [Field : | Xs] -> (add-field M Field Xs (value *db*))
  _ _ -> false)

(define nil-load
  -> false)

(set *nil-load* nil-load)
(set *nil-translate* (/. _ _ []))

(define init-module-data
  M -> (do (put M path (value *home-directory*) (value *db*))
           (put M load [] (value *db*))
           (put M translate [] (value *db*))
           (put M depends [] (value *db*))
           (put M translate-depends [] (value *db*))
           (put M load-fn (value *nil-load*) (value *db*))
           (put M unload-fn (value *nil-load*) (value *db*))
           (put M translate-fn (value *nil-translate*) (value *db*))
           (put M fields (value *fields*) (value *db*))
           true))

(define rm-module-data'
  [] _ _ -> true
  [X | Xs] M D -> (do (unput M X D)
                      (rm-module-data' Xs M)))

(define rm-module-data
  M -> (rm-module-data' (get M fields (value *db*)) M (value *db*)))

(define add-module-data
  _ [] -> true
  M [X | Xs] -> (do (add-module-field M X)
                    (add-module-data M Xs)))

(define register
  Def -> (let Name (value *current-module*)
           (and (init-module-data Name)
                (add-module-data Name Def))))

(define register-module
  Def -> (register Def))

(define call-module-unload
  M -> (let F (get M unload-fn (value *db*))
         (if (= F (value *nil-load*))
             true
             (F))))

(define forget-module
  M -> true where (not (element? (normalize-id M) (value *list*)))
  M -> (let M-id (normalize-id M)
            . (call-module-unload M-id)
            . (set *list* (remove M-id (value *list*)))
         (rm-module-data M-id)))

(define manifest-exists?
  F -> (trap-error (do (close (open (cn F "/module.shen") in))
                       true)
                   (/. E false)))

(define in-directory
  Dir Proc Err -> (let Prev (value *home-directory*)
                    (trap-error (let Ret (Proc (cd Dir))
                                     . (cd Prev)
                                  Ret)
                                (/. E (do (cd Prev)
                                          (Err E))))))

(define find-module-dir
  M [] -> (error "Unable to locate module ~A" M)
  M [D | Ds] -> (let Dir (cn D (cn "/" M))
                  (if (manifest-exists? Dir)
                      Dir
                      (find-module-dir M Ds))))

(define load-manifest'
  M S -> (let . (set *current-module* M)
              . (load/tc - "module.shen")
              . (set *current-module* "")
           S))

(define module-error
  S M R E -> (do (rm-module-data M)
                 (set *current-module* "")
                 (error "~A ~S: ~S" S M (error-to-string E))
                 R))

(define load-manifest
  M Ds -> (in-directory (find-module-dir M Ds)
                        (load-manifest' M)
                        (module-error "Loading manifest" M "")))

(define module-trans-deps
  M -> (let D (get M translate-depends (value *db*))
         (if (empty? D)
             (get M depends (value *db*))
             D)))

(define resolve-deps'
  {string --> (list string) --> get-deps-fn --> (string --> boolean)
   --> (list string) --> (list string)}
  _ [] _ _ Acc -> Acc
  P [D | Ds] Get Pred Acc -> (resolve-deps' P Ds Get Pred Acc) where (Pred D)
  P [D | Ds] Get Pred Acc -> (let Ps [P "." | (value *paths*)]
                                  Dir (load-manifest D Ps)
                                  Acc [D | Acc]
                                  Acc (resolve-deps' Dir (Get D) Get Pred Acc)
                               (resolve-deps' P Ds Get Pred Acc)))

(define remove-dups'
  [] Acc -> (reverse Acc)
  [X | Xs] Acc -> (remove-dups' Xs Acc) where (element? X Acc)
  [X | Xs] Acc -> (remove-dups' Xs [X | Acc]))

(define remove-dups
  X -> (remove-dups' X []))

(define resolve-deps
  Deps Get Pred -> (remove-dups (resolve-deps' "." Deps Get Pred [])))

(define load-module-files
  [] -> true
  [F | Fs] -> (do (load F)
                  (load-module-files Fs)))

(define load-module-sources
  M -> (let F (get M load-fn (value *db*))
            R (if (= F (value *nil-load*))
                  (load-module-files (get M load (value *db*)))
                  ((function F) load))
            . (set *list* [M | (value *list*)])
         R))

(define load-module
  M -> (in-directory (get M path (value *db*))
                     (/. _ (load-module-sources M))
                     (module-error "Failed loading" M false)))

(define load-modules
  [] -> true
  [M | Ms] -> (do (load-module M)
                  (load-modules Ms)))

(define use
  Ms -> (let Mods (resolve-deps (normalize-ids Ms)
                                (/. M (get M depends (value *db*)))
                                (/. X (element? X (value *list*))))
         (load-modules Mods)))

(define use-modules
  Ms -> (use Ms))

(define reload
  M -> (do (forget-module M)
           (use [M])))

(define fullpath
  P Files -> (map (/. X (cn P X)) Files))

(define ls-module-trans-files
  M Lang Impl Acc ->
  (in-directory
   (get M path (value *db*))
   (/. Dir (let F (get M translate-fn (value *db*))
             (append Acc (fullpath Dir
                                   (if (= F (value *nil-translate*))
                                       (let L (get M translate (value *db*))
                                         (if (empty? L)
                                             (get M load (value *db*))
                                             L))
                                       ((function F) Lang Impl))))))
   (module-error "Failed translating" M [])))

(define collect-trans-files
  [] _ _ Acc -> Acc
  [M | Ms] Lang Impl Acc ->
  (collect-trans-files Ms Lang Impl (ls-module-trans-files M Lang Impl Acc)))

(define files-to-translate
  M Lang Impl -> (let M-id (normalize-id M)
                   . (use [M])
                   Mods (resolve-deps [M-id]
                                      (function module-trans-deps)
                                      (/. _ false))
                   (collect-trans-files Mods Lang Impl [])))

(define load/tc
  Tc File -> (let Old-tc (if (tc?) + -)
                  . (tc Tc)
                  R (trap-error (load File)
                                (/. E (do (tc Old-tc)
                                          (error (error-to-string E)))))
                  . (tc Old-tc)
               R))

(datatype module-types
  X : string;
  ______________
  X : module-id;

  X : symbol;
  ______________
  X : module-id;

  X : module-id;
  _____________
  X : module-list;

  X : (list module-id);
  _________________
  X : module-list;
  )


(declare files-to-translate
         [module-id --> string --> string --> [list string]])
(declare use [module-list --> boolean])
(declare use-modules [module-list --> boolean])
(declare reload [module-id --> boolean])
(declare load/tc [symbol --> string --> symbol]))
