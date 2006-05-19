(define-macro (js . list)
   `(define *runtime-var-mapping* '(js . ,list)))
