(module __hop_exports
   (library scheme2js)
   (export (hop-runtime-adder::procedure)))

;; this module is tightly linked to Scheme2Js. However the module itself is
;; small, and (more importantly) the changes inside Scheme2Js not intrusive.
;; I expect this module to follow potential changes in Scheme2Js.

(define (make-exports-hashtable exports)
   (let ((ht (make-hashtable)))
      (for-each (lambda (e)
		   (let ((desc (create-Export-Desc e #f)))
		      (hashtable-put! ht (Export-Desc-id desc) desc)))
		exports)
      ht))

(define-macro (read-Hop-runtime-exports)
   (let* ((module-clause (with-input-from-file "hop-runtime.sch"
			    read))
	  (macros (cdr (assq 'export-macros (cddr module-clause))))
	  (exports (cdr (assq 'export (cddr module-clause)))))
      `(begin
	  (define *exported-macros* ',macros)
	  (define *exported-vars* (make-exports-hashtable ',exports)))))

(read-Hop-runtime-exports)

(define (add-hop-runtime! m::WIP-Unit)
   (with-access::WIP-Unit m (imports macros)
      (set! imports (cons *exported-vars* imports))
      (set! macros (append macros *exported-macros*))))

(define (hop-runtime-adder)
   add-hop-runtime!)

