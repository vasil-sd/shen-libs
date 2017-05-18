(package backend-utils [kl-from-shen]

(define def-string-translate-fn-aux
  _ [] Acc -> Acc
  Name [[From To] | Defs] Acc
  -> (let S (protect S)
          A (intern "Acc")
          X [[@s From S] A -> [do [output "c: ~A~%" From] [Name S [cn A To]]]]
       (def-string-translate-fn-aux Name Defs (append X Acc))))

(define def-string-translate-fn
  Name Defs -> (let N (intern (cn (str Name) "*"))
                    C (protect C)
                    S (protect S)
                    'Acc' (intern "Acc")
                    A1 [[@s C S] 'Acc' -> [N S [cn 'Acc' C]]]
                    A ["" (protect Acc) -> (protect Acc)]
                    A (append A (def-string-translate-fn-aux N Defs A1))
                    X [define N | A]
                  X))

(defmacro def-string-translate
  [def-string-translate Name | Defs] -> (def-string-translate-fn Name Defs))

(define kl-from-shen
  X -> (let X (shen.walk (function macroexpand) X)
            X (if (shen.packaged? X)
                  (shen.package-contents X)
                  X)
         (shen.elim-def (shen.proc-input+ X))))

(define try
  Code Finally -> (let R (trap-error (thaw Code)
                                     (/. E (do (thaw Finally)
                                               (error (error-to-string E)))))
                       . (thaw Finally)
                    R))

(define str-from-sexpr
  How X -> (let M (value *maximum-print-sequence-size*)
                . (set *maximum-print-sequence-size* -1)
                S (trap-error (make-string How X)
                              (/. E (do (set *maximum-print-sequence-size* M)
                                        (error (error-to-string E)))))
                . (set *maximum-print-sequence-size* M)
             S))

(define with-file-output
  File Fn -> (let F (open File out)
               (try (freeze (Fn F))
                    (freeze (close F)))))

(define write-file
  X To -> (let . (with-file-output To (/. S (pr (if (string? X)
                                                    X
                                                    (make-string "~A" X))
                                                S)))
            true))

(define map-shen
  _ [] _ -> true
  Fn [X | Y] To -> (let X' (Fn (kl-from-shen X))
                        S (if (string? X')
                              X'
                              (make-string "~A" X'))
                        . (pr S To)
                     (map-shen Fn Y To)))

(define translate-to-file
  To Head Fn Tail X -> (with-file-output
                         To
                         (/. F
                             (do (Head F)
                                 (map-shen Fn X F)
                                 (Tail F)
                                 true))))
            )
