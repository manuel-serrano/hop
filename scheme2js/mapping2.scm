(module mapping2
   (import export)
   (include "mapping.sch")
   (export *default-runtime-var-mapping*
	   *call/cc-runtime-var-mapping*))

(define *default-runtime-var-mapping*
   (map (lambda (e) (create-Export e #t))
	(get-exports "runtime/mod-runtime.sch")))
(define *call/cc-runtime-var-mapping*
   (map (lambda (e) (create-Export e #t))
	(get-exports "runtime/mod-runtime-callcc.sch")))
