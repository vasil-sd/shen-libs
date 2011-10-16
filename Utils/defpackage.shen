\* Copyright 2010-2011 Vasil S. Diadov

package.shen is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

package.shen is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with package.shen.
If not, see <http://www.gnu.org/licenses/>.


Description
===========
package.shen contains macro for package managment for Shen.
Package management allows importing/exporting symbols across packages, to
introducing package aliases and so on.

Usage:

(defpackage Name ListOfExportImportSymbols | Code)

  where Name is package name (prefix for all symbols in package, except
  symbols listed in ListOfExportImportSymbols and symbols
  shielded by (not-in-package ...))
  defpackage may include another defpackage in Code. 

  Example:

  (defpackage mypkg- [a b]
    [a b c])

    result: [a b mypkg-c]

  

(subpackage Name ListOfExportImportSymbols | Code)

  where Name is subpackage name (which is concatenated with toplevel package 
  name to form new symbols prefix) and ListOfExportImportSymbols is added 
  to exceptions list inherited from upper package or subpackage.

  Example:

  (defpackage mypkg- [a b]
    (subpackage subpkg- [c]
      [a b c d]))

    result: [a b c mypkg-subpkg-d]

  
(from-package Name Symbols | Code)

  where Name is name or alias (within scope of (package-alias ...) form)
  of package to which Symbols belong, and Symbols are symbol names which will
  be prefixed by given package name.

  Example:
  (defpackage mypkg- [c]
    (from-package pkg1- [a b]
      [a b c d]))

    result: [pkg1-a pkg1-b c mypkg-d]
  
(not-in-package | Code)

  blocks symbol renaming rules within its scope.

  Example:

  (defpackage mypkg- [c]
    (subpackage pkg1- [d]
      [c d e f]
      (not-in-package [c d e f])))

   result:  [c d mypkg-pkg1-e mypkg-pkg1-f]
            [c d e f]


(package-alias [ Name1 Alias1
                 Name2 Alias2...] | Code)

  introduces aliases for packages. Every package alias is treated as package name.

  Example:

  (defpackage mypkg- [a b]
    (package-alias [big-package-name- bpn-
                    very-big-package-name- vbpn-]
      [bpn-a vbpn-b]
        
      (from-package vbpn- [q w e]
        [q w e r t y])))

    result: [big-package-name-a very-big-package-name-b]
            [very-big-package-name-q very-big-package-name-w 
              very-big-package-name-e mypkg-r mypkg-t mypkg-y]


End of description.
===================
*\


(package defpackage- [ defpackage subpackage from-package 
                       not-in-package package-alias 
                       load-in-package register-defpackage-macro]

(define register-defpackage-macro
  Symbol F -> (set *package-macros* (adjoin [Symbol F] (value *package-macros*))))

(define if-not-fail
  Result Cont -> (Cont Result) where (not (= Result (fail)))
  Fail _ -> Fail)

(define string-remove-prefix
  "" S -> S
  (@s C S1) (@s C S2) -> (string-remove-prefix S1 S2)
  _ _ -> (fail))

(define symbol-remove-prefix
  Pr Sym -> (if-not-fail 
              (string-remove-prefix (str Pr) (str Sym)) 
              (function intern)))

(define process-alias
  [] Name -> Name
  [[Name Alias] | _] Alias -> Name
  [ X | Y] Alias -> (process-alias Y Alias))

(define import-symbol
  [] Sym -> (fail)
  [[_] | Rest] Sym -> (import-symbol Rest Sym)
  [[Pkg Sym | _] | _] Sym -> (concat Pkg Sym)
  [[Pkg X | Y] | Z] Sym -> (import-symbol [[Pkg | Y] | Z] Sym))

(define rename-symbol
  Name Alias Sym -> 
    (if-not-fail (symbol-remove-prefix Alias Sym) (concat Name)))

(define packaged-symbol
  [] Sym -> (fail)
  [[Name Alias] | Rest] Sym <- (rename-symbol Name Alias Sym)
  [_ | Rest] Sym -> (packaged-symbol Rest Sym))

(define process-symbol 
  _ _ Imported _ Sym <- (import-symbol Imported Sym)
  _ _ _ Aliases Sym  <- (packaged-symbol Aliases Sym)
  _ Exceptions _ _ Sym -> Sym where (element? Sym Exceptions) 
  Prefix _ _ _ Sym -> (concat Prefix Sym) where (symbol? Prefix)
  _ _ _ _ Sym -> Sym)

(define find-package-macro
  Sym -> (let Macro (assoc Sym (value *package-macros*))
           (if (empty? Macro)
             (fail)
             (head (tail Macro)))))

(define apply-package-macro
  Prefix Exceptions Imported Aliases Sym Code -> 
    (if-not-fail 
      (find-package-macro Sym)
      (/. F (F Prefix Exceptions Imported Aliases Code))))

(define insert-macroexpanded-code
  [append Code] Program -> (append Code Program)
  [cons Code] Program -> (cons Code Program))

(define process-package
  Prefix Exceptions Imported Aliases [[Symbol | Body] | Code] <-
    (if-not-fail
      (apply-package-macro Prefix Exceptions Imported Aliases Symbol Body)
      (/. Result
        (insert-macroexpanded-code 
          Result
          (process-package Prefix Exceptions Imported Aliases Code))))
  
  Prefix Exceptions Imported Aliases [X | Y]->
  [(process-package Prefix Exceptions Imported Aliases X) | 
   (process-package Prefix Exceptions Imported Aliases Y) ]

  Prefix Exceptions Imported Aliases  Sym -> 
    (process-symbol Prefix Exceptions Imported Aliases Sym) 
    where (and 
            (symbol? Sym) 
            (not 
              (or 
                (shen-sysfunc? Sym) 
                (shen-prefix? (explode shen-) Sym) 
                (shen-singleunderline? Sym) 
                (shen-doubleunderline? Sym))))

  _ _ _ _  X -> X)
    

(define defpackage-macro-not-in-package
  _ _ _ _ Code -> [append (process-package [] [] [] [] Code)])

(define defpackage-macro-subpackage
  Prefix Exceptions Imported Aliases [Name Symbols | Code]->
    [append (process-package 
              (concat Prefix Name) 
              (append Exceptions (eval-without-macros Symbols)) 
              Imported
              Aliases 
              Code)])

(define defpackage-macro-from-package
  Prefix Exceptions Imported Aliases [Name Symbols | Code]->
    [append 
      (process-package 
        Prefix 
        Exceptions 
        [[(process-alias Aliases Name) | 
          (eval-without-macros Symbols)] | 
          Imported] 
        Aliases 
        Code)])

(define compose-names-and-aliases
  [] Acc -> Acc
  [X Y | Rest] Acc -> [[X Y] | (compose-names-and-aliases Rest Acc)])

(define defpackage-macro-package-alias
  Prefix Exceptions Imported Aliases [NamesAndAliases | Code]->
    [append 
      (process-package 
        Prefix 
        Exceptions 
        Imported 
        (compose-names-and-aliases 
          (eval-without-macros NamesAndAliases) 
          Aliases) 
        Code)])

(set *package-macros* [
  [not-in-package (function defpackage-macro-not-in-package)]
  [subpackage (function defpackage-macro-subpackage)]
  [from-package (function defpackage-macro-from-package)]
  [package-alias (function defpackage-macro-package-alias)]
])
  
(defmacro package-macro
  [defpackage Name Exceptions | Code] -> 
    [package null [] | 
      (process-package 
        Name 
        (eval-without-macros Exceptions) 
        [] 
        [] 
        Code)]

  [defpackage null [] | Code] -> 
    [package null [] | 
      (process-package [] [] [] [] Code)])

) \* end of package defpackage- *\
