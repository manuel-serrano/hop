(module __hop_exports
   (library scheme2js)
   (export (hop-runtime-module)))

;; this module is tightly linked to Scheme2Js. However the module itself is
;; small, and (more importantly) the changes inside Scheme2Js not intrusive.
;; I expect this module to follow potential changes in Scheme2Js.

(define (make-exports-table exports)
   (let ((ht (make-hashtable)))
      (for-each (lambda (e)
		   (let ((desc (create-Export-Desc e #f #f)))
		      (hashtable-put! ht (Export-Desc-id desc) desc)))
		exports)
      ht))

(define (make-exported-macros-table macros)
   (let ((ht (make-hashtable)))
      (for-each (lambda (macro)
		   (add-macro-to-ht macro ht))
		macros)
      ht))

(define-macro (read-Hop-runtime-exports)
   (let* ((module-clause (with-input-from-file "hop-runtime.sch"
			    read))
	  (macros (cdr (assq 'export-macros (cddr module-clause))))
	  (exports (cdr (assq 'export (cddr module-clause)))))
      `(define *hop-module*
	  (instantiate::Compilation-Unit
	     (name 'hop)
	     (exported-macros (make-exported-macros-table ',macros))
	     (exports (make-exports-table ',exports))
	     ;; following fields are unused.
	     (imports '())
	     (top-level '())
	     ;; declared-module? is ignored.
	     (declared-module? #f)))))

(read-Hop-runtime-exports)

(define (hop-runtime-module) *hop-module*)
