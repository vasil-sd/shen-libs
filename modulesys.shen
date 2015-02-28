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

                 name depends translate-depends depends load translate load-fn
                 unload-fn translate-fn]

(synonyms load-fn (--> boolean)
          translate-fn (string --> string --> (list string))
          get-deps-fn (string --> (list string)))
          
(datatype module-def
  X : string;
  ==============
  X : module-id;

  X : symbol;
  ==============
  X : module-id;

  X : module-id;
  ______________________________
  [name : X] : module-def-field;

  if (element? X [depends translate-depends])
  Y : (list module-id);
  =============================
  [X : | Y] : module-def-field;

  if (element? X [load translate])
  Y : (list string);
  ============================
  [X : | Y] : module-def-field;

  if (element? X [load-fn unload-fn])
  Y : load-fn;
  ===========================
  [X : Y] : module-def-field;

  if (element? X [translate-fn])
  Y : translate-fn;
  ===========================
  [X : Y] : module-def-field;

  if (not (element? X [translate-depends depends load translate load-fn
                       unload-fn translate-fn]))
  X : symbol; Y : string;
  ===========================
  [X : Y] : module-def-field;

  ________________
  [] : module-def;

  X : module-def-field; Y : module-def;
  =====================================
  [X | Y] : module-def;)

(datatype db
  _________________________
  (value *db*) : module-db;

  _________________________
  (vector 256) : module-db;

  _________________________
  (value *paths*) : (list string);

  __________________________________
  (value *current-module*) : string;

  __________________________________
  (value *home-directory*) : string;

  __________________________________
  (value *language*) : string;

  __________________________________
  (value *implementation*) : string;

  _________________________________
  (value *modules*) : (list string);

  _____________________________
  (value *nil-load*) : load-fn;

  ______________________________________
  (value *nil-translate*) : translate-fn;

  if (not (element? X [translate-depends depends load translate load-fn
                       unload-fn translate-fn]))
  M : string; X : symbol; Y : string; D : module-db;
  __________________________________________________
  (put M X Y D) : string;

  if (not (element? X [translate-depends depends load translate load-fn
                       unload-fn translate-fn]))
  M : string; X : symbol; D : module-db;
  ______________________________________
  (get M X D) : string;

  if (element? X [translate-depends depends load translate])
  M : string; Y : (list string); D : module-db;
  _____________________________________________
  (put M X Y D) : (list string);

  if (element? X [translate-depends depends load translate])
  M : string; D : module-db;
  ____________________________
  (get M X D) : (list string);

  if (element? X [load-fn unload-fn])
  M : string; Y : load-fn; D : module-db;
  _______________________________________
  (put M X Y D) : load-fn;

  if (element? X [load-fn unload-fn])
  M : string; X : symbol; D : module-db;
  ______________________________________
  (get M X D) : load-fn;

  if (element? X [translate-fn])
  M : string; Y : translate-fn; D : module-db;
  _______________________________________
  (put M X Y D) : translate-fn;

  if (element? X [translate-fn])
  M : string; D : module-db;
  __________________________
  (get M X D) : translate-fn;

  M : string; X : symbol; D : module-db;
  _____________________________________
  (unput M X D) : string;)

(set *paths* [])
(set *modules* [])
(set *db* (vector 256))

(define add-path
  {string --> (list string)}
  X -> (set *paths* [X | (value *paths*)])
       where (not (element? X (value *paths*))))

(define rm-path
  {string --> (list string)}
  X -> (set *paths* (remove X (value *paths*))))

(define normalize-id
  {module-id --> string} 
  X -> X where (string? X)
  X -> (str X) where (symbol? X))

(define normalize-ids
  {(list module-id) --> (list string)}
  X -> (map (function normalize-id) X))

(define add-module-field
  {string --> module-def-field --> boolean}
  M [load : | X] -> (do (put M load X (value *db*)) true)
  M [translate : | X] -> (do (put M translate X (value *db*)) true)
  M [depends : | X] -> (do (put M depends (normalize-ids X) (value *db*))
                           true)
  M [translate-depends : | X] -> (do (put M translate-depends
                                          (normalize-ids X) (value *db*))
                                true)
  M [load-fn : X] -> (do (put M load-fn X (value *db*)) true)
  M [unload-fn : X] -> (do (put M unload-fn X (value *db*)) true)
  M [translate-fn : X] -> (do (put M translate-fn X (value *db*)) true)
  _ _ -> true)

(define nil-load
  {--> boolean}
  -> false)

(set *nil-load* nil-load)
(set *nil-translate* (/. _ _ []))

(define init-module-data
  {string --> boolean}
  M -> (do (put M path (value *home-directory*) (value *db*))
           (put M load [] (value *db*))
           (put M translate [] (value *db*))
           (put M depends [] (value *db*))
           (put M translate-depends [] (value *db*))
           (put M load-fn (value *nil-load*) (value *db*))
           (put M unload-fn (value *nil-load*) (value *db*))
           (put M translate-fn (value *nil-translate*) (value *db*))
           true))

(define rm-module-data
  {string --> boolean}
  M -> (do (unput M path (value *db*))
           (unput M load (value *db*))
           (unput M translate (value *db*))
           (unput M depends (value *db*))
           (unput M translate-depends (value *db*))
           (unput M load-fn (value *db*))
           (unput M unload-fn (value *db*))
           (unput M translate-fn (value *db*))
           true))

(define add-module-data
  {string --> module-def --> boolean}
  _ [] -> true
  M [X | Xs] -> (do (add-module-field M X)
                    (add-module-data M Xs)))

(define register
  {module-def --> boolean}
  Def -> (let Name (value *current-module*)
           (and (init-module-data Name)
                (add-module-data Name Def))))

(define register-module
  {module-def --> boolean}
  Def -> (register Def))

(define call-module-unload
  {string --> boolean}
  M -> (let F (get M unload-fn (value *db*))
         (if (= F (value *nil-load*))
             true
             (F))))

(define forget-module
  {module-id --> boolean}
  M -> (let M-id (normalize-id M)
            . (call-module-unload M-id)
            . (remove M-id (value *modules*))
         (rm-module-data M-id)))

(define manifest-exists?
  {string --> boolean}
  F -> (trap-error (do (close (open (cn F "/module.shen") in))
                       true)
                   (/. E false)))

(define in-directory
  {string --> (string --> A) --> (exception --> A) --> A}
  Dir Proc Err -> (let Prev (value *home-directory*)
                    (trap-error (let Ret (Proc (cd Dir))
                                     . (cd Prev)
                                  Ret)
                                (/. E (do (cd Prev)
                                          (Err E))))))

(define find-module-dir
  {string --> (list string) --> string}
  M [] -> (error "Unable to locate module ~A" M)
  M [D | Ds] -> (let Dir (cn D (cn "/" M))
                  (if (manifest-exists? Dir)
                      Dir
                      (find-module-dir M Ds))))

(define load-manifest'
  {string --> string --> string}
  M S -> (let . (set *current-module* M)
              . (load "module.shen")
              . (set *current-module* "")
           S))

(define module-error
  {string --> string --> A --> exception -->  A}
  S M R E -> (do (rm-module-data M)
                 (set *current-module* "")
                 (error "~A ~S: ~S" S M (error-to-string E))
                 R))

(define load-manifest
  {string --> (list string) --> string}
  M Ds -> (in-directory (find-module-dir M Ds)
                        (load-manifest' M)
                        (module-error "Loading manifest" M "")))

(define module-trans-deps
  {string --> (list string)}
  M -> (let D (get M translate-depends (value *db*))
         (if (empty? D)
             (get M depends (value *db*))
             D)))

(define resolve-deps'
  {string --> (list string) --> get-deps-fn --> (string --> boolean)
   --> (list string) --> (list string)}
  _ [] _ _ Acc -> Acc
  P [D | Ds] Get Pred Acc -> (resolve-deps' P Ds Get Pred Acc)
                             where (element? D Acc)
  P [D | Ds] Get Pred Acc -> (resolve-deps' P Ds Get Pred Acc) where (Pred D)
  P [D | Ds] Get Pred Acc -> (let Ps [P "." | (value *paths*)]
                                  Dir (load-manifest D Ps)
                                  Acc [D | Acc]
                               (resolve-deps' Dir (Get D) Get Pred Acc)))

(define resolve-deps
  {(list string) --> get-deps-fn --> (string --> boolean) --> (list string)}
  Deps Get Pred -> (resolve-deps' "." Deps Get Pred []))

(define load-module-files
  {(list string) --> boolean}
  [] -> true
  [F | Fs] -> (do (load F)
                  (load-module-files Fs)))

(define load-module-sources
  {string --> boolean}
  M -> (let F (get M load-fn (value *db*))
            R (if (= F (value *nil-load*))
                  (load-module-files (get M load (value *db*)))
                  (F))
            . (set *modules* [M | (value *modules*)])
         R))

(define load-module
  {string --> boolean}
  M -> (in-directory (get M path (value *db*))
                     (/. _ (load-module-sources M))
                     (module-error "Failed loading" M false)))

(define load-modules
  {(list string) --> boolean}
  [] -> true
  [M | Ms] -> (do (load-module M)
                  (load-modules Ms)))

(define use
  {(list module-id) --> boolean}
  Ms -> (let Ms' (map (function normalize-id) Ms)
             Mods (resolve-deps Ms'
                                (/. M (get M depends (value *db*)))
                                (/. X (element? X (value *modules*))))
         (load-modules Mods)))

(define use-modules
  {(list module-id) --> boolean}
  Ms -> (use Ms))

(define reload
  {module-id --> boolean}
  M -> (do (forget-module M)
           (use [M])))

(define fullpath
  P Files -> (map (/. X (cn P X)) Files))

(define ls-module-trans-files
  {string --> string --> string --> (list string) --> (list string)}
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
                                       (F Lang Impl))))))
   (module-error "Failed translating" M [])))

(define collect-trans-files
  {(list string) --> string --> string --> (list string) --> (list string)}
  [] _ _ Acc -> Acc
  [M | Ms] Lang Impl Acc ->
  (collect-trans-files Ms Lang Impl (ls-module-trans-files M Lang Impl Acc)))

(define files-to-translate
  {module-id --> string --> string --> (list string)}
  M Lang Impl -> (let M-id (normalize-id M)
                   . (use [M])
                   Mods (resolve-deps [M-id]
                                      (function module-trans-deps)
                                      (/. _ false))
                   (collect-trans-files Mods Lang Impl [])))

(define load/tc
  {symbol --> string --> symbol}
  Tc File -> (let Old-tc (if (tc?) + -)
                  . (tc Tc)
                  R (trap-error (load File)
                                (/. E (do (tc Old-tc)
                                          (error (error-to-string E)))))
                  . (tc Old-tc)
               R))
)
