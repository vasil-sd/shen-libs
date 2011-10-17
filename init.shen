\* init.shen --- initialize the package managment system for lib-shen *\
(load "file-system/file-system.shen")
(load "packages/packages.shen")
(map (lambda P
       (if (= ".git" (hd (reverse (split-paths P))))
           false
           (set *package-path* (adjoin P (value *package-path*)))))
     (directory-list "./"))
