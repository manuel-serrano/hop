(module export-desc
   (import gen-js)
   (export
    (final-class Export-Desc
       (id::symbol read-only)
       (js-id::bstring read-only)
       (exported-as-const?::bool (default #f))
       (runtime?::bool (default #f) read-only)
       (peephole       (default #f) read-only)
       (higher?::bool  (default #f) read-only)
       (higher-params  (default #f) read-only) 
       (return-type    (default #f) read-only))
    (create-Export-Desc::Export-Desc info runtime?::bool)))


(define (entry-val sym l)
   (let ((try (assq sym (cdr l))))
      (and try
	   (cadr try))))

(define (normalize-export export)
   (cond
      ((symbol? export)
       (list export
	     (list 'JS (mangle-JS-sym export))))
      ((pair? export)
       (cond
	  ((assq 'JS (cdr export))
	   export)
	  (else
	   (let ((scheme-sym (car export)))
	      (cons* scheme-sym
		     (list 'JS (mangle-JS-sym scheme-sym))
		     (cdr export))))))
      (else
       (error "normalize-export" "bad import/export clause: " export))))

(define (normalize-js-id id)
   (cond
      ((symbol? id)
       (symbol->string id))
      ((string? id)
       id)
      (else 
       (error "exported variable"
	      "JS-clause must be either symbol or string"
	      id))))

(define (create-Export-Desc::Export-Desc info runtime?)
   (let* ((normalized (normalize-export info))
	  (scheme-sym (car normalized))
	  (js-id (normalize-js-id (entry-val 'JS normalized)))
	  (peephole (entry-val 'peephole normalized))
	  (higher? (entry-val 'call/cc? normalized))
	  (higher-params (entry-val 'call/cc-params normalized))
	  (return-type (entry-val 'type normalized))
	  (exported-as-const? (or (entry-val 'constant? normalized))))
      (instantiate::Export-Desc
	 (id scheme-sym)
	 (js-id js-id)
	 (exported-as-const? exported-as-const?)
	 (runtime? runtime?)
	 (peephole peephole)
	 (higher? higher?)
	 (higher-params higher-params)
	 (return-type return-type))))
