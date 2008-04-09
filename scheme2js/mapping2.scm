(module mapping2
   (include "mapping.sch")
   (export *default-runtime-var-mapping*
	   *call/cc-runtime-var-mapping*))

(define *default-runtime-var-mapping* (get-exports "runtime/mod-runtime.sch"))
(define *call/cc-runtime-var-mapping* (get-exports "runtime/mod-runtime-callcc.sch"))
