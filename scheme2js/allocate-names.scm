(module allocate-names
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   verbose
	   gen-js)
   (static (class Name-Env
	      suffix
	      (counter::bint (default 0)))
	   (wide-class Global-Var::Var))
   (export (wide-class Named-Var::Var
	      (js-id::bstring read-only)))
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
   (find-free tree #f #f '()) ;; local version
   (let ((env (instantiate::Name-Env
		 (suffix (and (config 'statics-suffix)
			      (suffix-mangle (config 'statics-suffix)))))))
      (name-gen tree env #f)))

;; ------------- find-free --------------------------------
;; at this compilation-stage 'let's have been removed.
;; we thus only have to look at the 'declared'-vars and at Frame-allocs
(define-nmethod (Node.find-free surrounding-fun declared-vars-list)
   (default-walk this surrounding-fun declared-vars-list))
(define-nmethod (Execution-Unit.find-free surrounding-fun declared-vars-list)
   (with-access::Execution-Unit this (scope-vars declared-vars free-vars)
      (set! free-vars '())
      ;; scope-vars contains parameters to functions.
      (default-walk this this (list scope-vars declared-vars))))
(define-nmethod (Frame-push.find-free surrounding-fun declared-vars-list)
   (with-access::Frame-push this (frame-allocs)
      ;; the storage-vars allocate new vars that are supposed to be visible
      ;; inside the frame-pushes.
      (let* ((declared-vars (map Frame-alloc-vars frame-allocs)))
	 (default-walk this surrounding-fun
	    (append declared-vars declared-vars-list)))))
(define-nmethod (Ref.find-free surrounding-fun declared-vars-list)
   (with-access::Execution-Unit surrounding-fun (free-vars)
      (with-access::Ref this (var)
	 (unless (or (eq? (Var-kind var) 'this)
		     (any? (lambda (l) (memq var l)) declared-vars-list)
		     (memq var free-vars))
	    (cons-set! free-vars var)))))
;; --------------------------------------------------------
		
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

(define (allocate-name v::Var env used-ht)
   (unless (Named-Var? v)
      (with-access::Var v (kind)
	 (let ((kind (if (Global-Var? v)
			 'global
			 kind)))
	    (case kind
	       ((local)             (allocate-local-name v env used-ht))
	       ((this)              (allocate-this-name v used-ht))
	       ((imported exported) (allocate-exported-name v env used-ht))
	       ((global)            (allocate-global-name v env used-ht)))))))
	     
(define (allocate-local-name v::Var env used-ht)
   ;; generate ids
   (with-access::Var v (escapes? id)
      (let loop ((short (nice-mangle (symbol->string id)))
		 (already-tried? #f))
	 (cond
	    ((and (valid? short)
		  (not (dangerous? short))
		  (not (used? short used-ht)))
	     (widen!::Named-Var v (js-id short)))
	    (already-tried?
	     (widen!::Named-Var v (js-id (gen-JS-sym id))))
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
		   (loop next-try #t)))))))
   (hashtable-put! used-ht (Named-Var-js-id v) #t))

(define (allocate-this-name v::Var used-ht)
   (widen!::Named-Var v (js-id "this"))
   (hashtable-put! used-ht 'this #t))

(define (allocate-exported-name v::Var env used-ht)
   (with-access::Var v (export-desc)
      (let ((js-id (Export-Desc-js-id export-desc)))
	 (widen!::Named-Var v (js-id js-id))
	 (hashtable-put! used-ht js-id #t))))

(define (allocate-global-name v::Var env used-ht)
   (with-access::Name-Env env (suffix)
      (if (not suffix)
	  (begin ;; no suffix -> treat it, as if it was a local var.
	     (shrink! v)
	     (allocate-name v env used-ht))
	  (with-access::Var v (id escapes?)
	     (let* ((short (string-append (nice-mangle (symbol->string id))
					  suffix)))
		(widen!::Named-Var v
		   (js-id (if (or (not (valid? short))
				  (used? short used-ht))
			      (string-append (gen-JS-sym id) suffix)
			      short))))
	     (hashtable-put! used-ht (Named-Var-js-id v) #t)))))

(define-nmethod (Node.name-gen used-ht)
   (default-walk this used-ht))

(define-nmethod (Module.name-gen used-ht)
   (let ((used-ht (make-hashtable)))
      (with-access::Module this (runtime-vars imported-vars declared-vars
					      this-var)
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
		      (unless (eq? (Var-kind var) 'local)
			 (allocate-name var env used-ht)))
		   declared-vars)
	 ;; now that the Exported vars have been handled we can assign the
	 ;; names to the global vars.
	 (for-each (lambda (var)
		      (when (eq? (Var-kind var) 'local)
			 (widen!::Global-Var var)
			 (allocate-name var env used-ht)))
		   declared-vars)
	 (allocate-name this-var env used-ht)
	 (default-walk this used-ht))))

(define-nmethod (Lambda.name-gen used-ht)
   (with-access::Lambda this (scope-vars declared-vars this-var free-vars)
      (let ((lambda-used-ht (make-hashtable)))
	 (for-each (lambda (var)
		      (hashtable-put! lambda-used-ht
				      (Named-Var-js-id var)
				      #t))
		   free-vars)
	 
	 (for-each (lambda (var)
		      (allocate-name var env lambda-used-ht))
		   scope-vars)
	 (for-each (lambda (var)
		      (allocate-name var env lambda-used-ht))
		   declared-vars)
	 (allocate-name this-var env lambda-used-ht)
	 (default-walk this lambda-used-ht))))

(define-nmethod (Frame-alloc.name-gen used-ht)
   (with-access::Frame-alloc this (vars storage-var)
      (for-each (lambda (var)
		   (allocate-name var env used-ht))
		vars)
      (default-walk this used-ht)))
