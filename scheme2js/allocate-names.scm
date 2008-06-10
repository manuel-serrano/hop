(module allocate-names
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   free-vars
	   config
	   nodes
	   var
	   verbose
	   gen-js)
   (export (gen-var-names tree)))

(define (gen-var-names tree)
   ;; ===================================================================
   ;; procedure starts here
   ;; ===================================================================
   (verbose "  generating names for vars")
   (free-vars tree)
   (overload traverse name-gen (Node
				Module
				Lambda
				Frame-alloc)
	     (overload allocate-name allocate-name (Var JS-Var)
		       (tree.traverse '() #f))))

(define *bad-js* ;; avoid variables with the following names.
   '("Object" "Array" "Math" "Number" "String" "undefined"))

(define-pmethod (Var-allocate-name escaping-ids local-ids-ht)
   (define (invalid-or-used? str)
      (and (or (not (valid-JS-str? str))
	       (member str *bad-js*)
	       (hashtable-get local-ids-ht str)
	       (any? (lambda (ht) (hashtable-get ht str)) escaping-ids)
	       ;; avoid clashes with runtime :
	       (string-prefix? "sc_" str)
	       (string-prefix? "SC_" str))
	   #t))

   (define (nice-mangle str)
      (let* ((str0 (string-replace str #\- #\_))
	     (str_ (string-replace str0 #\* #\@)))
	 (cond
	    ((string-suffix? "?" str_)
	     (if (or (string-prefix? "is_" str_)
		     (string-prefix? "has_" str_))
		 (substring str_ 0 (- (string-length str_) 1))
		 (string-append "is_"
				(substring str_
					   0
					   (- (string-length str_) 1)))))
	    ((string-suffix? "!" str_)
	     (string-append (substring str_ 0 (- (string-length str_) 1))
			    "_bang"))
	    (else
	     str_))))

   (define (suffix-mangle str)
      (define (replace s L)
	 (if (null? L)
	     s
	     (replace (string-replace s (caar L) (cdar L))
		      (cdr L))))
      
      (let ((str_ (replace str '((#\- . #\_)
				 (#\. . #\_)
				 (#\space . #\_)
				 (#\# . #\_)
				 (#\~ . #\_)
				 (#\* . #\_)
				 (#\: . #\_)))))
	 (if (bigloo-need-mangling? str_)
	     (bigloo-mangle str_)
	     str_)))

   ;; generate ids
   (cond
      (this.compiled
       ;; could only happen, if a formal has been replaced...
       'do-nothing)
      ((not (symbol? this.id))
       (error 'compile "Illegal variable identifier" this.id))
      ((or this.extern?
	   this.exported?)
       (set! this.compiled (mangle-JS-sym this.id)))
      ((and this.global?
	    (config 'statics-suffix))
       (let* ((suf (suffix-mangle (config 'statics-suffix)))
	      (short (string-append (nice-mangle (symbol->string this.id))
				    suf)))
	  (set! this.compiled
		(if (invalid-or-used? short)
		    (string-append (gen-JS-sym this.id)
				   suf)
		    short)))
       (if this.escapes?
	   (hashtable-put! (car escaping-ids) this.compiled #t)
	   (hashtable-put! local-ids-ht this.compiled #t)))
      (else
       (let ((short (nice-mangle (symbol->string this.id))))
	  (set! this.compiled
		(if (invalid-or-used? short)
		    (gen-JS-sym this.id)
		    short)))
       (if this.escapes?
	   (hashtable-put! (car escaping-ids) this.compiled #t)
	   (hashtable-put! local-ids-ht this.compiled #t)))))

(define-pmethod (JS-Var-allocate-name escaping-ids local-ids-ht)
   (unless this.compiled
      (set! this.compiled (if (symbol? this.js-id)
			      (symbol->string this.js-id)
			      this.js-id))
      (hashtable-put! (car escaping-ids) this.compiled #t)))

(define-pmethod (Node-name-gen escaping-ids local-ids-ht)
   (this.traverse2 escaping-ids local-ids-ht))

(define-pmethod (Module-name-gen escaping-ids ignored)
   (let ((new-escaping-ids (cons (make-hashtable) escaping-ids))
	 (local-ids-ht (make-hashtable)))
      (for-each (lambda (var)
		   (var.allocate-name new-escaping-ids local-ids-ht))
		this.runtime-vars)
      (for-each (lambda (var)
		   (var.allocate-name new-escaping-ids local-ids-ht))
		this.imported-vars)
      (for-each (lambda (var)
		   (set! var.global? #t)
		   (var.allocate-name new-escaping-ids local-ids-ht))
		this.declared-vars)
      (this.traverse2 new-escaping-ids local-ids-ht)))

(define-pmethod (Lambda-name-gen escaping-ids ignored)
   (let ((new-escaping-ids (cons (make-hashtable) escaping-ids))
	 (local-ids-ht (make-hashtable)))
      (for-each (lambda (var) (var.allocate-name new-escaping-ids local-ids-ht))
		this.scope-vars)
      (for-each (lambda (var) (var.allocate-name new-escaping-ids local-ids-ht))
		this.declared-vars)
      (this.traverse2 new-escaping-ids local-ids-ht)))

(define-pmethod (Frame-alloc-name-gen escaping-ids local-ids-ht)
   (for-each (lambda (var)
		(var.allocate-name escaping-ids local-ids-ht))
	     this.vars)
   (let ((storage-var this.storage-var))
      (this.traverse2 escaping-ids local-ids-ht)))
