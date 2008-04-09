(define-macro (get-exports file)
   (let* ((m-clause (with-input-from-file file read))
	  ;; skip 'module' and module-name
	  (a-list (cddr m-clause))
	  (exports (assq 'export a-list)))
      (list 'quote (cdr exports))))
