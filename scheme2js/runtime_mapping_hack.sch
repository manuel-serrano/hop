;; $Id: runtime_mapping_hack.sch 112 2006-02-08 15:33:40Z flo $
(define-macro (js . list)
   `(define *runtime-var-mapping* '(js . ,list)))
