\* file-system.shen --- cross-platform file system utilities

Copyright (C) 2011,  Eric Schulte

*** License:

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

 - Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

 - Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*** Commentary:

The following functions are provided.

  return a path relative to a directory
  (join "file.ext" "directory")

  list the contents of a directory
  (dir-list "directory")

  does a file or directory exist at the given path
  (file-exists? "path")

*** Code: *\
(package file-system- [file-exists? file-directory? split-paths join-paths
                       directory-list directory-recur delete-file rename-file]

(define starts-with
  "" _ -> true
  _ "" -> false
  (@s A AS) (@s B BS) -> (if (= A B) (starts-with AS BS) false))

(define length-str
  \* return the length of a string *\
  {string --> number}
  "" -> 0
  (@s _ SS) -> (+ 1 (length-str SS)))

(define mapcon
  \* like map but concatenate the results *\
  {(A --> (list B)) --> (list A) --> (list B)}
  _ [] -> []
  Fn [A|AS] -> (append (Fn A) (mapcon Fn AS)))

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

(define split-paths
  {string --> (list string)}
  Path -> (path-split- Path "" []))

(define path-split-
  "" Holder Acc -> (reverse (if (= Holder "") Acc [Holder|Acc]))
  (@s P Ps) Holder Acc ->
    (if (= (value *path-separator*) P)
        (path-split- Ps "" (if (= Holder "") Acc [Holder|Acc]))
        (path-split- Ps (@s Holder P) Acc)))

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

(define as-dir
  Path -> (if (= (value *path-separator*) (pos Path (- (length-str Path) 1)))
              Path
              (@s Path (value *path-separator*))))

(define join-paths
  \* return a path relative to a directory *\
  {string --> string --> string}
  Dir Path -> (@s (as-dir Dir) Path))

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

(define read-string
  {(stream in)--> string}
  Stream -> (let Byte (read-byte Stream)
  	      (if (or (= Byte -1) (= Byte 10))
  		  ""
  		  (cn (n->string Byte) (read-string-from-file Stream)))))

\* read list of strings from stream *\
(define read-strings-from-stream
  {(stream in) --> (list string)}
  Stream -> (let Str (read-string-from-file Stream)
	      (if (= Str "") 
		  []
		  [ Str | (read-text-stream Stream) ])))

\* read list of strings from file *\
(define read-text-file
  {string --> (list string)}
  Path -> (read-strings-from-stream (open file Path in)))


)
