(module mapping2
   (import export-desc)
   (include "mapping.sch")
   (export *default-runtime-var-mapping*
	   *call/cc-runtime-var-mapping*))

(define *default-runtime-var-mapping*
   (map (lambda (e) (create-Export-Desc e #t))
	(get-exports "runtime/mod-runtime.sch")))
(define *call/cc-runtime-var-mapping*
   (map (lambda (e) (create-Export-Desc e #t))
	(get-exports "runtime/mod-runtime-callcc.sch")))
