(package ffi [ffi call-ffi]
  
   (define ffi 
      Language OutSpec InSpec -> (push [Language OutSpec InSpec] *ffi*)) 

   (define push 
      X Y -> (set Y [X | (trap-error (value Y) (/. E []))])) 
      
   (defmacro call-ffi-macro 
     [call-ffi Foreign-Language Code] 
      -> (let Spec (assoc Foreign-Language (value *ffi*)) 
              (if (empty? Spec) 
                  (error "we don't know how to talk to ~A~%"  Foreign-Language) 
                  (process-ffi-call Spec Code))))
                  
   (define process-ffi-call 
     [_ (@p SyntaxOutF SendF) (@p SyntaxInF ReceiveF)] Code 
      -> [SyntaxInF [ReceiveF [SendF [SyntaxOutF (quote Code)]]]])
      
   (define quote 
     [X | Y] -> [cons (quote X) (quote Y)] 
     X -> X))