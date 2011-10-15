\* packages.shen --- a simple package requirement facility

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
