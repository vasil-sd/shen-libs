\* Copyright 2010-2011 Ramil Farkhshatov

defstruct is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

defstruct is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with defstruct.  If not, see <http://www.gnu.org/licenses/>.

## Description

Module system is a convenient tool for managing libraries.

## Usage syntax

* `(use-modules [mod1 mod2 ...])`
loads given modules with all their dependencies. Any module already loaded
won't be loaded twice.

* `(reload-module mod1)`
reloads given module.

* `(list-modules registered)`
returns a list of registered modules.

* `(list-modules loaded)`
returns a list of loaded modules.

* `(dump-module mod language implementation target-dir)`
dumps module `mod` and its dependencies to given implementation of given
language to supplied directory.

* `(set *modules-paths* [dir1 dir2])`
sets list of directories where modules are searched.

## Module definition

Sample contents of `mod1/module.shen` where `mod1` is module name:

  (register-module [[name: mod1]
                    [load: "file1" "file2"]
                    [depends: mod3 mod4]])

*\

(package module
         [name depends load load-fn unload-fn dump dump-fn path loaded
          registered *modules-paths* find-module use-modules dump-module
          register-module reload-module list-modules dump-native
          module-str-list module-sym module-str module-load-fn module-dump-fn
          module-deps module-dump-deps register-dumper all in]

(synonyms load-fn (string --> boolean)
          dump-fn (symbol --> (symbol --> (string --> (string --> boolean))))
          native-dump-fn (string --> (string --> (string --> boolean)))
          dep-fn (module-desc --> (list symbol)))

(datatype module-desc
  X : symbol;
  ==============================
  [name : X] : module-desc-item;

  X : (list symbol);
  ===================================
  [depends : | X] : module-desc-item;

  X : (list symbol);
  ===================================
  [dump-depends : | X] : module-desc-item;

  X : (list string);
  =============================
  [load : | X] : module-desc-item;

  X : (list string);
  =============================
  [dump : | X] : module-desc-item;

  X : symbol;
  _________________________________
  [load-fn : X] : module-desc-item;

  X : load-fn >> P;
  ______________________________________
  [load-fn : X] : module-desc-item >> P;

  X : symbol;
  ___________________________________
  [unload-fn : X] : module-desc-item;

  X : load-fn >> P;
  ________________________________________
  [unload-fn : X] : module-desc-item >> P;

  X : symbol;
  _________________________________
  [dump-fn : X] : module-desc-item;

  X : dump-fn >> P;
  ______________________________________
  [dump-fn : X] : module-desc-item >> P;

  if (not (element? X [name dump-depends depends load dump load-fn unload-fn
                       dump-fn]))
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

  ___________
  [] : entry;

  X : symbol; F : module-desc;
  ============================
  [X | F] : entry;

  _________________________________
  (value *modules*) : (list entry);

  X : (list entry);
  _________________________________
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
  (value *home-directory*) : string;)

(datatype native-types
  __________________
  [] : dumper-entry;

  I : symbol; L : symbol; F : native-dump-fn;
  ===========================================
  [I L | F] : dumper-entry;

  ________________________________________
  (value *dumpers*) : (list dumper-entry);

  X : (list dumper-entry);
  _______________________________________
  (set *dumpers* X) : (list dumper-entry);)

(set *loaded-modules* [])
(set *modules* [])
(set *modules-paths* [])
(set *dumpers* [])

(define module-loaded?
  {symbol --> boolean}
  M -> (element? M (value *loaded-modules*)))

(define module-deps
  {module-desc --> (list symbol)}
  [] -> []
  [[depends : | M] | R] -> M
  [_ | R] -> (module-deps R))

(define module-dump-deps*
  {module-desc --> (list symbol)}
  [] -> []
  [[dump-depends : | M] | R] -> M
  [_ | R] -> (module-dump-deps* R))

(define module-dump-deps
  {module-desc --> (list symbol)}
  X -> (let D (module-dump-deps* X)
         (if (empty? D)
             (module-deps X)
             D)))

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
  T [_ | R] -> (module-sym T R))

(define null-load-fn
  {string --> boolean}
  _ -> false)

(define module-load-fn
  {symbol --> module-desc --> load-fn}
  _ [] -> null-load-fn
  load-fn [[load-fn : F] | _] -> F where (= (arity F) 1)
  load-fn [[load-fn : F] | _] -> (error "Wrong load function ~S.~%" F)
  unload-fn [[unload-fn : F] | _] -> F where (= (arity F) 1)
  unload-fn [[unload-fn : F] | _] -> (error "Wrong unload function ~S.~%" F)
  T [_ | R] -> (module-load-fn T R))

(define null-dump-fn
  {symbol --> symbol --> string --> string --> boolean}
  _ _ _ _ -> false)

(define module-dump-fn
 {symbol --> module-desc --> dump-fn}
  _ [] -> null-dump-fn
  dump-fn [[dump-fn : F] | _] -> F where (= (arity F) 4)
  dump-fn [[dump-fn : F] | _] -> (error "Wrong dump function ~S.~%" F)
  T [_ | R] -> (module-dump-fn T R))

(define module-entry-key
  {entry --> symbol}
  [Key | Def] -> Key)

(define list-modules
  {symbol --> (list symbol)}
  loaded -> (value *loaded-modules*)
  registered -> (map module-entry-key (value *modules*))
  _ -> (error "(list-modules loaded) or (list-modules registered)~%"))

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
  Def -> (add-module! (module-sym name Def) Def))

(define module-known?
  {symbol --> boolean}
  M -> false where (= (find-module M) [])
  _ -> true)

(define in-directory
  {string --> (string --> A) --> (exception --> A) --> A}
  S F E -> (let Pwd (value *home-directory*)
             (trap-error (let Path (cd S)
                              Ret (F Path)
                              Path2 (cd Pwd)
                           Ret)
                         (/. Err (do (cd Pwd)
                                     (E Err))))))

(define manifest-exists?
  {symbol --> string --> boolean}
  M P -> (in-directory (cn P (str M))
                       (/. _ (let P (open "module.shen" in)
                                  R (close P)
                                true))
                       (/. E false)))

(define load-manifest-file
  {symbol --> string --> boolean}
  M P -> false where (not (manifest-exists? M P))
  M P -> (in-directory (cn P (str M))
                       (/. _ (do (load "module.shen")
                                 (module-known? M)))
                       (/. E (error "~A/module.shen: ~S"
                                    P
                                    (error-to-string E)))))

(define load-manifest
  {symbol --> (list string) --> boolean}
  M [] -> false
  M [P | Path] <- (fail-if (/. X (not X)) (load-manifest-file M P))
  M [P | Path] -> (load-manifest M Path))

(define resolve-deps-aux
  {dep-fn --> (list symbol) --> (list symbol) --> (list symbol)}
  _ [] Acc -> Acc
  F [M | R] Acc -> (resolve-deps-aux F R Acc) where (element? M Acc)
  F [M | R] Acc -> (if (load-manifest M (value *modules-paths*))
                       (let Deps (F (find-module M))
                            Acc (resolve-deps-aux F Deps Acc)
                         (resolve-deps-aux F R [M | Acc]))
                       (error "Unable to find module ~S~%" M)))

(define resolve-deps
  {dep-fn --> (list symbol) --> (list symbol)}
  F Deps -> (reverse (resolve-deps-aux F Deps [])))

(define walk-tree*
  {(list symbol) --> (symbol --> boolean) --> (list symbol) --> boolean
   --> boolean}
  _ _ _ false -> false
  [] _ _ Res -> Res
  [M | Mods] Fn Acc Res -> (walk-tree* Mods Fn Acc Res) where (element? M Acc)
  [M | Mods] Fn Acc Res -> (walk-tree* Mods Fn [M | Acc] (Fn M)))

(define walk-tree
  {dep-fn --> (list symbol) --> (symbol --> boolean) --> boolean}
  F Mods Fn -> (walk-tree* (resolve-deps F Mods) Fn [] true))

(define load-module-files
  {(list string) --> boolean}
  [] -> true
  [F | Files] -> (do (load F)
                     (load-module-files Files)))

(define load-module*
  {symbol --> load-fn --> (list string) --> boolean}
  _ null-load-fn [] -> true
  M null-load-fn Files -> (load-module-files Files)
  M Fn _ -> (Fn (value *home-directory*)))

(define load-module
  {symbol --> module-desc --> boolean}
  _ [] -> false
  M _ -> true where (module-loaded? M)
  M Desc -> (let F (module-load-fn load-fn Desc)
                 L (module-str-list load Desc)
              (in-directory
                (module-str path Desc)
                (/. _ (if (load-module* M F L)
                          (do (set *loaded-modules*
                                   [M | (value *loaded-modules*)])
                              true)
                          false))
                (/. E (error (error-to-string E))))))

(define use-modules
  {(list symbol) --> boolean}
  M -> (let L (/. X (load-module X (find-module X)))
         (walk-tree (function module-deps) M L)))

(define null-native-dump-fn
  {string --> string --> string --> boolean}
  _ _ _ -> false)

(define find-dumper
  {symbol --> symbol --> (list dumper-entry) --> native-dump-fn}
  Lang Impl [] -> null-native-dump-fn
  Lang Impl [[Lang Impl | F] | Dumpers] -> F
  Lang Impl [_ | Dumpers] -> (find-dumper Lang Impl Dumpers))

(define dump-native
  {symbol --> symbol --> string --> string --> string --> boolean}
  Ln Im Src Dst F <- (let D1 (find-dumper Ln all (value *dumpers*))
                          D2 (find-dumper Ln Im (value *dumpers*))
                       (if (= D2 null-native-dump-fn)
                           (if (= D1 null-native-dump-fn)
                               (error "No appropriate native loader found.")
                               (D1 Src F Dst))
                           (D2 Src F Dst))))

(define dump-module-files
  {symbol --> symbol --> string --> string --> (list string) --> boolean}
  L Im S D [] -> true
  L Im S D [F | Files] -> (let T1 (dump-native L Im S D F)
                            (dump-module-files L Im S D Files)))

(define dump***
  {symbol --> symbol --> string --> string --> symbol --> dump-fn
   --> (list string) --> (list string) --> boolean}
  _ _ _ _ _ null-dump-fn [] [] -> false
  Ln Im Src Dst M Fn _ _ -> (Fn Ln Im Src Dst) where (not (= Fn null-dump-fn))
  Ln Im Src Dst M _ [] L-files -> (dump-module-files Ln Im Src Dst L-files)
  Ln Im Src Dst M _ D-files _ -> (dump-module-files Ln Im Src Dst D-files))

(define dump**
  {symbol --> symbol --> string --> symbol --> module-desc --> boolean}
  _ _ _ _ [] -> false
  Lang Impl Dir M Desc -> (let F (module-dump-fn dump-fn Desc)
                               Src (module-str path Desc)
                               D (module-str-list dump Desc)
                               L (module-str-list load Desc)
                            (dump*** Lang Impl Src Dir M F D L)))

(define dump*
  {symbol --> symbol --> string --> symbol --> boolean}
  Lang Impl Dir M -> (dump** Lang Impl Dir M (find-module M)))

(define dump-module
  {symbol --> symbol --> symbol --> string --> boolean}
  M Lang Impl Dir -> (let Dir' (cn (value *home-directory*) Dir)
                          D (function module-dump-deps)
                       (walk-tree D [M] (dump* Lang Impl Dir')))
                     where (module-loaded? M)
  M _ _ _ -> (error "Dump error: module ~S is not loaded.~%" M))

(define forget-module*
  {symbol --> module-desc --> load-fn --> boolean}
  M [] _ -> true
  M _ null-load-fn -> true
  M Desc Fn -> (in-directory (module-str path Desc)
                             Fn
                             (/. E (error (error-to-string E)))))

(define forget-module
  {symbol --> boolean}
  M -> (let D (find-module M)
            F (module-load-fn unload-fn D)
            L (set *loaded-modules* (remove M (value *loaded-modules*)))
            R (forget-module-manifest M (value *modules*) [])
         (forget-module* M D F))
       where (module-loaded? M)
  _ -> true)

(define reload-module
  {symbol --> boolean}
  M -> (do (forget-module M)
           (use-modules [M])))

(define register-dumper*
  {symbol --> symbol --> native-dump-fn --> (list dumper-entry)
   --> (list dumper-entry) --> (list dumper-entry)}
  L Impl Fn [] Acc -> (set *dumpers* [[L Impl | Fn] | Acc])
  L Impl Fn [[L Impl | _] | R] Acc -> (register-dumper* L Impl Fn R Acc)
  L Impl Fn [X | R] Acc -> (register-dumper* L Impl Fn R [X | Acc]))

(define register-dumper
  {symbol --> symbol --> native-dump-fn --> boolean}
  L Impl Fn -> (do (register-dumper* L Impl Fn (value *dumpers*) [])
                   true)))
