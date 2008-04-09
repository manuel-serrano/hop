(module mapping1
   (include "mapping.sch")
   (export *default-constant-runtime-var-mapping*
	   *call/cc-constant-runtime-var-mapping*))

(define *default-constant-runtime-var-mapping* (get-exports "runtime/runtime.sch"))
(define *call/cc-constant-runtime-var-mapping* (get-exports "runtime/runtime-callcc.sch"))
