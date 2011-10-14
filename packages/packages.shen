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

These functions may be used to load external packages.

  ensure that a package is loaded
  (require package-name)

  check if a package has been loaded
  (loaded? package-name)

*** TODO:

Once Shen gets some file-system facilities it would make sense to
maintain a load path in which packages may be found.  When that
happens then packages should be represented as symbols rather than
paths.

*** Code: *\
(package packages- [loaded? require]

(set *packages* [])

(define loaded?
  {string --> boolean}
  \* check if a package has been loaded *\
  Pkg -> (element? Pkg (value *packages*)))

(define require
  \* load a package if possible return boolean indiciating successful load *\
  {string --> boolean}
  Pkg -> (if (loaded? Pkg)
             true
             (trap-error (do (load Pkg) true) (lambda E false))))
)
