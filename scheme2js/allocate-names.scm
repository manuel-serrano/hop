(module allocate-names
   (import config
	   tools
	   nodes
	   walk
	   free-vars
	   verbose
	   gen-js)
   (static (class Name-Env
	      suffix)
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
		 (suffix (config 'statics-suffix)))))
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

(define (invalid-or-used? str used-ht)
   (and (or (not (valid-JS-str? str))
	    (hashtable-get used-ht str))
	#t))

(define-generic (allocate-name v::Var env used-ht)

   ;; generate ids
   (with-access::Var v (compiled escapes? id)
      (when (string-null? compiled)
	 (let ((short (nice-mangle (symbol->string id))))
	    (set! compiled
		  (if (invalid-or-used? short used-ht)
		      (gen-JS-sym id)
		      short))
	    (hashtable-put! used-ht compiled #t)))))

(define-method (allocate-name v::JS-Var env used-ht)
   (with-access::JS-Var v (compiled js-id)
      (set! compiled js-id)
      (hashtable-put! used-ht compiled #t)))

(define-method (allocate-name v::Global-Var env used-ht)
   (with-access::Name-Env env (suffix)
      (if (not suffix)
	  (begin ;; no suffix -> treat it, as if it was a local var.
	     (shrink! v)
	     (allocate-name v env used-ht))
	  (with-access::Var v (id compiled escapes?)
	     (let* ((suf (suffix-mangle suffix))
		    (short (string-append (nice-mangle (symbol->string id))
					  suf)))
		(set! compiled
		      (if (invalid-or-used? short used-ht)
			  (string-append (gen-JS-sym id) suf)
			  short)))
	     (hashtable-put! used-ht compiled #t)))))

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
	 (for-each (lambda (var)
		      ;; might be either an Exported-Var or a Local
		      (when (Local? var)
			  (widen!::Global-Var var))
		      (allocate-name var env used-ht))
		   declared-vars)
	 (default-walk this used-ht))))

(define-nmethod (Lambda.name-gen used-ht)
   (with-access::Lambda this (scope-vars declared-vars free-vars)
      (let ((lambda-used-ht (make-hashtable)))
	 (for-each (lambda (var)
		      (hashtable-put! lambda-used-ht
				      (Var-compiled var)
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
