(module mapping1
   (import export-desc
	   tools)
   (include "mapping.sch")
   (export *default-constant-runtime-var-mapping*
	   *call/cc-constant-runtime-var-mapping*))

(define *default-constant-runtime-var-mapping*
   (let ((ht (make-eq-hashtable)))
      (map (lambda (e)
	      (let ((desc (create-Export-Desc e #f #t)))
		 (hashtable-put! ht (Export-Desc-id desc) desc)))
	   (get-exports "runtime/runtime.sch"))
      ht))
(define *call/cc-constant-runtime-var-mapping*
   (let ((ht (make-eq-hashtable)))
      (map (lambda (e)
	      (let ((desc (create-Export-Desc e #f #t)))
		 (hashtable-put! ht (Export-Desc-id desc) desc)))
	   (get-exports "runtime/runtime-callcc.sch"))
      ht))
