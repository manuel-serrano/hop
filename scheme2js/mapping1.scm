(module mapping1
   (import export-desc)
   (include "mapping.sch")
   (export *default-constant-runtime-var-mapping*
	   *call/cc-constant-runtime-var-mapping*))

(define *default-constant-runtime-var-mapping*
   (map (lambda (e) (create-Export-Desc e #t))
	(get-exports "runtime/runtime.sch")))
(define *call/cc-constant-runtime-var-mapping*
   (map (lambda (e) (create-Export-Desc e #t))
	(get-exports "runtime/runtime-callcc.sch")))
