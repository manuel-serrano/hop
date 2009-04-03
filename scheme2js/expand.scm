(module expand
   (import verbose
	   error
	   tools)
   (export (my-expand x
		      additional-macros) ;; list of lists/ht of macros
	   (install-expander! id e)
	   ;; priority: lower -> later
	   (add-pre-expand! priority::bint f::procedure)
	   (pre-expand! x)
	   (identity-expander x e)
	   (add-macro-to-ht macro ht)
	   (lazy-macro macro ht)
	   (module-macro-ht))
   (eval (export add-pre-expand!)))

(define *pre-expanders* '())

(define *mutex* (make-mutex))

;; pre-expander is shared by all parallel computations.
(define (add-pre-expand! priority f)
   (define (insert L priority f)
      (cond
	 ((null? L)
	  (list (cons priority f)))
	 ((>= priority (caar L))
	  (cons (cons priority f) L))
	 (else
	  (cons (car L) (insert (cdr L) priority f)))))
   
   (mutex-lock! *mutex*)
   (set! *pre-expanders* (insert *pre-expanders* priority f))
   (mutex-unlock! *mutex*))

(define (pre-expand! x)
   (define (pre-expand!-inner x)
      (let loop ((x x)
		 (pre-expanders *pre-expanders*))
	 (if (null? pre-expanders)
	     x
	     (loop ((cdar pre-expanders) x)
		   (cdr pre-expanders)))))
   (mutex-lock! *mutex*)
   (let ((res (pre-expand!-inner x)))
      (mutex-unlock! *mutex*)
      res))

(define (module-macro-ht)
   (thread-parameter 'scheme2js-module-macro-ht))
(define (module-macro-ht-set! new-ht)
   (thread-parameter-set! 'scheme2js-module-macro-ht new-ht))
   

(define (lazy-macro macro ht)
   (define (destructure pat arg bindings)
      (cond
	 ((null? pat) bindings)
	 ((symbol? pat) `((,pat ,arg) ,@bindings))
	 ((pair? pat)
	  (destructure (car pat)
		       `(car ,arg)
		       (destructure (cdr pat) `(cdr ,arg) bindings)))))

   (define (deep-copy o)
      (cond
	 ((epair? o)
	  (econs (deep-copy (car o))
		 (deep-copy (cdr o))
		 (deep-copy (cer o))))
	 ((pair? o)
	  (cons (deep-copy (car o))
		(deep-copy (cdr o))))
	 ((vector? o)
	  (copy-vector o (vector-length o)))
	 ((string? o)
	  (string-copy o))
	 (else
	  o)))

   (define (macro->expander macro)
      (match-case macro
	 ((?- (?name . ?args) ?e0 . ?body)
	  (let ((L (gensym 'L)))
	     (eval `(lambda (x e)
		       (let ((,L (cdr x)))
			  (e
			   ;; macros might reference lists twice.
			   ;; by deep-copying the result, we are sure,
			   ;; that we don't share anything.
			   (,deep-copy
			    (let ,(destructure args L '())
			       ,e0 ,@body))
			   e))))))))
       
   (lambda (x e)
      (let ((name (car (cadr macro)))
	    (macro-expander (macro->expander macro)))
	 ;; replace this lazy fun by the actual macro-expander.
	 (hashtable-put! ht name macro-expander)
	 ;; execute the macro.
	 (macro-expander x e))))

(define (add-macro-to-ht macro ht)
   (match-case macro
      ((define-macro (?name . ?args) ?e0 . ?body)
       (hashtable-put! ht
		       name
		       (lazy-macro macro ht)))
      (else
       (scheme2js-error "add-macro-to-ht"
			"bad macro"
			macro
			macro))))
   
(define (prepare-additional-macros macros)
   ;; we want to have a list of hashtables.
   ;; however: try to minimize the number of hashtables.
   ;; the topmost hashtable will be modified, so it must not be one of the
   ;; given ones.
   (let loop ((ms (reverse macros))
	      (ht #f) ;; already exsting ht where we can add macros
	      (res '()))
      (cond
	 ((and (null? ms)
	       (not ht))
	  (cons (make-eq-hashtable) res))
	 ((null? ms)
	  res)
	 ((hashtable? (car ms))
	  ;; user-supplied ht. we are not allowed to add other macros.
	  (loop (cdr ms)
		#f
		(cons (car ms) res)))
	 ((null? (car ms))
	  ;; just ignore empty lists
	  (loop (cdr ms) ht res))
	 ((and (pair? (car ms))
	       (not ht))
	  ;; create new ht and try again
	  (let ((ht (make-eq-hashtable)))
	     (loop ms
		   ht
		   (cons ht res))))
	 ((pair? (car ms))
	  (for-each (lambda (macro)
		       (add-macro-to-ht macro ht))
		    (car ms))
	  (loop (cdr ms)
		ht
		res))
	 (else
	  (scheme2js-error "macro"
			   "bad additional macros form"
			   (car ms)
			   ms)))))

(define (my-expand x additional-macros)
   (verbose "expanding")
   (let* ((macro-mapping (prepare-additional-macros additional-macros))
	  (old-module-macro-ht (module-macro-ht))
	  (initial-expander (scheme2js-initial-expander macro-mapping)))
      (module-macro-ht-set! (car macro-mapping))
      (unwind-protect
	 (initial-expander x initial-expander)
	 (module-macro-ht-set! old-module-macro-ht))))


;; macros can not be global variable. (Parallel compilation could
;; yield bad results).
(define (scheme2js-initial-expander macros-hts)
   (lambda (x e)
      (let ((e1 (cond
		   ((symbol? x) symbol-expander)
		   ((not (pair? x)) identity-expander)
		   ((symbol? (car x))
		    (cond
		       ;; user-defined macros win over compiler-macros
		       ((any (lambda (ht)
				(hashtable-get ht (car x)))
			     macros-hts)
			=> (lambda (macro-e)
			      macro-e))
		       ;; compiler-macros
		       ((expander (car x))
			=> (lambda (expander)
			      expander))
		       (else
			application-expander)))
		   (else
		    application-expander)))
	    (pre-expanded-x (pre-expand! x)))
	 (e1 pre-expanded-x e))))

(define (symbol-expander x e)
   x)
	     
(define (identity-expander x e) x)

(define (application-expander x e)
   (map! (lambda (y) (e y e)) x))

;; compiler expanders are shared by all parallel compilations.
(define *expanders* '())

(define (expander? id)
   (and (assq id *expanders*) #t))

(define (expander id)
   (let ((tmp (assq id *expanders*)))
      (and tmp
	   (cdr tmp))))

(define (install-expander! id e)
   (cons-set! *expanders* (cons id e)))
