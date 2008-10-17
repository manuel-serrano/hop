(module allocate-names
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   free-vars
	   verbose
	   gen-js)
   (static (class Name-Env
	      suffix
	      (counter::bint (default 0)))
	   (wide-class Global-Var::Local))
   (export (gen-var-names tree)))

(define *reserved-js* (make-hashtable))
(for-each (lambda (str)
	     (hashtable-put! *reserved-js* str #t))
	  '("as" "break" "case" "catch" "class" "const" "continue" "default"
		 "delete" "do" "else" "extends" "false" "finally" "for"
		 "function" "if" "import" "in" "instanceof" "is" "namespace"
		 "new" "null" "package" "private" "public" "return" "super"
		 "switch" "this" "throw" "true" "try" "typeof" "use" "var"
		 "void" "while" "with" "abstract" "debugger" "enum" "export"
		 "goto" "implements" "interface" "native" "protected"
		 "synchronized" "throws" "transient" "volatile" "Object"
		 "undefined"))
		 
(define (gen-var-names tree)
   (verbose "  generating names for vars")
   (free-vars tree)
   (let ((env (instantiate::Name-Env
		 (suffix (and (config 'statics-suffix)
			      (suffix-mangle (config 'statics-suffix)))))))
      (name-gen tree env #f)))

(define (nice-mangle::bstring str)
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

(define (valid? str)
   (valid-JS-str? str))

(define *dangerous-js*
   '("Object" "undefined" "Array" "Number" "String" "Date" "RegExp"))

(define (dangerous? str)
   (and (member str *dangerous-js*) #t))

(define (used? str used-ht)
   (and (hashtable-get used-ht str)))

(define-generic (allocate-name v::Var env used-ht)
   ;; generate ids
   (with-access::Var v (js-id escapes? id)
      (when (string-null? js-id)
	 (let loop ((short (nice-mangle (symbol->string id)))
		    (already-tried? #f))
	    (cond
	       ((and (valid? short)
		     (not (dangerous? short))
		     (not (used? short used-ht)))
		(set! js-id short))
	       (already-tried?
		(set! js-id (gen-JS-sym id)))
	       (else
		(with-access::Name-Env env (counter)
		   (set! counter (+fx counter 1))
		   (let ((next-try
			  (cond
			     ((or (string-prefix? "sc_" short)
				  (string-prefix? "SC_" short))
			      (string-append "v_" short "_"
					     (number->string counter)))
			     (else
			      (string-append short "_"
					     (number->string counter))))))
		      (loop next-try #t))))))
	 (hashtable-put! used-ht js-id #t))))

(define-method (allocate-name v::This-Var env used-ht)
   (hashtable-put! used-ht 'this #t)) ;; js-id is already up to date.

(define-method (allocate-name v::Exported-Var env used-ht)
   (with-access::Exported-Var v (desc js-id)
      (set! js-id (Export-Desc-js-id desc))
      (hashtable-put! used-ht js-id #t)))

(define-method (allocate-name v::Global-Var env used-ht)
   (with-access::Name-Env env (suffix)
      (if (not suffix)
	  (begin ;; no suffix -> treat it, as if it was a local var.
	     (shrink! v)
	     (allocate-name v env used-ht))
	  (with-access::Var v (id js-id escapes?)
	     (let* ((short (string-append (nice-mangle (symbol->string id))
					  suffix)))
		(set! js-id
		      (if (or (not (valid? short))
			      (used? short used-ht))
			  (string-append (gen-JS-sym id) suffix)
			  short)))
	     (hashtable-put! used-ht js-id #t)))))

(define-nmethod (Node.name-gen used-ht)
   (default-walk this used-ht))

(define-nmethod (Module.name-gen used-ht)
   (let ((used-ht (make-hashtable)))
      (with-access::Module this (runtime-vars imported-vars declared-vars)
	 (for-each (lambda (var)
		      (allocate-name var env used-ht))
		   runtime-vars)
	 (for-each (lambda (var)
		      (allocate-name var env used-ht))
		   imported-vars)
	 ;; first allocate Exported, as they have precedence over local vars.
	 ;; ex: (define export_var _some_val_) (let ((export-var ....)) ...)
	 ;;   if 'export-var' is handled before the exported one, it receives
	 ;;   'export_var' as JS-id. -> bad...
	 (for-each (lambda (var)
		      (when (Exported-Var? var)
			 (allocate-name var env used-ht)))
		   declared-vars)
	 ;; now that the Exported vars have been handled we can assign the
	 ;; names to the global vars.
	 (for-each (lambda (var)
		      (when (Local? var)
			 (widen!::Global-Var var)
			 (allocate-name var env used-ht)))
		   declared-vars)
	 (default-walk this used-ht))))

(define-nmethod (Lambda.name-gen used-ht)
   (with-access::Lambda this (scope-vars declared-vars free-vars)
      (let ((lambda-used-ht (make-hashtable)))
	 (for-each (lambda (var)
		      (hashtable-put! lambda-used-ht
				      (Var-js-id var)
				      #t))
		   free-vars)
	 
	 (for-each (lambda (var)
		      (allocate-name var env lambda-used-ht))
		   scope-vars)
	 (for-each (lambda (var)
		      (allocate-name var env lambda-used-ht))
		   declared-vars)
	 (default-walk this lambda-used-ht))))

(define-nmethod (Frame-alloc.name-gen used-ht)
   (with-access::Frame-alloc this (vars storage-var)
      (for-each (lambda (var)
		   (allocate-name var env used-ht))
		vars)
      (default-walk this used-ht)))
