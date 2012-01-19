\* packages.shen --- a simple package requirement facility

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

The following configures the load-path to load all packages in shen-libs/

(load "../file-system/file-system.shen")
(load "packages.shen")
(map (lambda P
       (if (= ".git" (hd (reverse (split-paths P))))
           false
           (set *package-path* (adjoin P (value *package-path*)))))
     (directory-list "../"))

after which packages may be loaded as follows

  (require sequence)

*** Code: *\
(package packages- [*packages* *package-path* loaded? require
                    \* symbols included from file-system *\
                    join-paths
                    \* symbols included from sequence *\
                    take drop take-while drop-while range flatten
                    filter complement seperate zip indexed reduce
                    mapcon partition partition-with unique frequencies
                    shuffle pick remove-first interpose subset?
                    cartesian-product]

(set *packages* [])
(set *package-path* [])

(define loaded?
  {symbol --> boolean}
  \* check if a package has been loaded *\
  Pkg -> (element? Pkg (value *packages*)))

(define require
  \* load a package if possible return boolean indiciating successful load *\
  {symbol --> boolean}
  Pkg -> (or (loaded? Pkg)
             (and (require- (@s (str Pkg) ".shen") (value *package-path*))
                  (do (set *packages* (adjoin Pkg (value *packages*))) true))))

(define require-
  {string --> (list string) --> boolean}
  _ [] -> false
  Path [Dir|Dirs] -> (if (try-load (join-paths Dir Path))
                         true
                         (require- Path Dirs)))

(define try-load
  {string --> boolean}
  Path -> (trap-error (do (load Path) true)
                      (lambda E false)))

)
