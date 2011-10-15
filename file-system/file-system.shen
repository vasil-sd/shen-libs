\* file-system.shen --- cross-platform file system utilities

Copyright (C) 2011  Eric Schulte

*** License:

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*** Commentary:

The following functions are provided.

  return a path relative to a directory
  (join "file.ext" "directory")

  list the contents of a directory
  (dir-list "directory")

  does a file or directory exist at the given path
  (file-exists? "path")

*** Code: *\
(package file-system- [file-exists? file-directory? join-paths
                       directory-list directory-recur delete-file rename-file]

(define starts-with
  "" _ -> true
  _ "" -> false
  (@s A AS) (@s B BS) -> (if (= A B) (starts-with AS BS) false))

(set *path-separator*
      \* ideally a new *operating-system* global variable could be used here *\
      (cond
       ((starts-with "CLisp" (value *implementation*))
        (cond
         ((let OS (SOFTWARE-TYPE)
            (or (starts-with "x86_64-linux-gnu-gcc" OS)
                (starts-with "x86_32-linux-gnu-gcc" OS)))
          "/")
         (true "/")))
       (true "/")))

(define file-exists?
  \* check if a file exists *\
  {string --> boolean}
  Path -> (cond
           ((= "Common Lisp" (value *language*))
            (if (= NIL (trap-error (PROBE-FILE Path) (/. E NIL)))
                false true))))

(define file-directory?
  \* check if a path is a directory *\
  {string --> boolean}
  Path -> (cond
           ((starts-with "CLisp" (value *implementation*))
            (if (= NIL (trap-error (PROBE-DIRECTORY Path) (/. E NIL)))
                false true))))

(define join-paths
  \* return a path relative to a directory *\
  {string --> string --> string}
  Dir Path -> (cond
               ((= "Common Lisp" (value *language*))
                (FORMAT NIL "~a" (MAKE-PATHNAME
                                  (INTERN "DIRECTORY" "KEYWORD") Dir
                                  (INTERN "NAME" "KEYWORD") Path)))))

(define directory-list
  {string --> [string]}
  Path -> (cond
           ((starts-with "CLisp" (value *implementation*))
            (if (file-directory? Path)
                (map (lambda X (NAMESTRING X))
                     (append (DIRECTORY (@s Path "*/"))
                             (DIRECTORY (MAKE-PATHNAME
                                         (INTERN "NAME" "KEYWORD")
                                         (INTERN "WILD" "KEYWORD")
                                         (INTERN "TYPE" "KEYWORD")
                                         (INTERN "WILD" "KEYWORD")
                                         (INTERN "DEFAULTS" "KEYWORD") Path))))
                (error (make-string "~S does not name a directory" Path))))))

(define mapcon
  \* like map but concatenate the results *\
  {(A --> (list B)) --> (list A) --> (list B)}
  _ [] -> []
  Fn [A|AS] -> (append (Fn A) (mapcon Fn AS)))

(define directory-recur
  \* call a function on every file within a directory *\
  {(A --> B) --> String --> (list B)}
  Fn Path -> (cond
              ((file-exists? Path) [(Fn Path)])
              ((file-directory? Path)
               (mapcon (directory-recur Fn) (directory-list Path)))
              (true [])))

(define delete-file
  {string --> boolean}
  Path -> (cond
           ((= "Common Lisp" (value *language*))
            (if (= NIL (trap-error (DELETE-FILE Path) (/. E [])))
                false true))))

(define rename-file
  {string --> boolean}
  From To -> (cond
              ((= "Common Lisp" (value *language*))
               (trap-error (RENAME-FILE From To) (/. E false)))))

)
