\* html.shen --- html generation functions for shen

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

The standard lisp-to-html conversion tool suite.  Follows some of
the convertions of Clojure's hiccup.

  an example...

(8-) (html [ul#todo1.tasks.stuff [: [title "today"]]
          (map (lambda Str [li Str]) ["get milk" "dishes"])])
"<ul class='tasks stuff' id='todo1' title='today'>
 <li>get milk</li><li>dishes</li></ul>"

*** Code: *\
(trap-error
 (require string)
 (/. E (load "../string/string.shen")))

(package string- [html]

(define to-str
  \* return argument as a string, if already a string do not change *\
  X -> X where (string? X)
  X -> (str X))

(define gassoc
  X Y -> (hd (tl (assoc X Y))))

(define dassoc
  X Y -> (remove (assoc X Y) Y))

(define passoc
  [] Y -> Y
  [X XV] Y -> (let Orig (gassoc X Y)
                   New (if (cons? Orig) [XV|Orig] XV)
                [[X New]|(dassoc X Y)]))

(define html
  X -> X where (string? X)
  [Tag [: |Attrs] |Body] ->
    (let Tag-comps (css-parse-symbol Tag)
         Tag (gassoc tag Tag-comps)
         New-attrs (passoc (assoc class Tag-comps)
                           (passoc (assoc id Tag-comps) Attrs))
      (@s (make-string "<~S" Tag) (attributes New-attrs) ">"
          (html Body)
          (make-string "</~S>" Tag))) where (symbol? Tag)
  [Tag|Body] -> (html [Tag [:] Body]) where (symbol? Tag)
  [H|HS] -> (@s (html H) (html HS))
  [] -> "")

(define css-parse-symbol
  {symbol --> [[symbol A]]}
  Symbol -> (let String (str Symbol)
                 Class-split (split (str .) String)
                 Class (map (function intern) (tl Class-split))
                 Id-split (split (str #) (hd Class-split))
                 Tag (hd Id-split)
                 Id (tl Id-split)
              ((if (= [] Id) (/. X X) (cons [id (intern (hd Id))]))
               ((if (= [] Class) (/. X X) (cons [class Class])) 
                [[tag (intern Tag)]]))))

(define attributes
  [] -> ""
  [[K V]|AS] -> (@s " " (to-str K) "='"
                    (if (cons? V) (join " " (map (function str) V)) (to-str V))
                    "'" (attributes AS)))

)
