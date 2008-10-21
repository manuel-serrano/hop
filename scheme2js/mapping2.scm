(module mapping2
   (import export-desc
	   tools)
   (include "mapping.sch")
   (export *default-runtime-var-mapping*
	   *call/cc-runtime-var-mapping*))

(define *default-runtime-var-mapping*
   (let ((ht (make-eq-hashtable)))
      (map (lambda (e)
	      (let ((desc (create-Export-Desc e #t)))
		 (hashtable-put! ht (Export-Desc-id desc) desc)))
	   (get-exports "runtime/mod-runtime.sch"))
      ht))
(define *call/cc-runtime-var-mapping*
   (let ((ht (make-eq-hashtable)))
      (map (lambda (e)
	      (let ((desc (create-Export-Desc e #t)))
		 (hashtable-put! ht (Export-Desc-id desc) desc)))
	   (get-exports "runtime/mod-runtime-callcc.sch"))
      ht))
