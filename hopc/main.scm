;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/hopc/main.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Fri Dec  2 17:02:03 2011 (serrano)                */
;*    Copyright   :  2004-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOPC entry point                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopc

   (library scheme2js hopscheme hop)

   (cond-expand
      (enable-threads (library pthread)))
   
   (import  hopc_parseargs
	    hopc_param)

   (main    main))

;*---------------------------------------------------------------------*/
;*    hop-verb ...                                                     */
;*---------------------------------------------------------------------*/
(define-expander hop-verb
   (lambda (x e)
      (match-case x
	 ((?- (and (? integer?) ?level) . ?rest)
	  (let ((v (gensym)))
	     `(let ((,v ,(e level e)))
		 (if (>=fx (hop-verbose) ,v)
		     (hop-verb ,v ,@(map (lambda (x) (e x e)) rest))))))
	 (else
	  `(hop-verb ,@(map (lambda (x) (e x e)) (cdr x)))))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   ;; set the Hop cond-expand identification
   (register-srfi! 'hopc)
   (for-each register-eval-srfi! (hop-srfis))
   ;; set the library load path
   (bigloo-library-path-set! (hop-library-path))
   ;; parse the command line
   (parse-args args)
   ;; access file
   (cond
      ((string? (hopc-access-file))
       (module-load-access-file (hopc-access-file)))
      ((file-exists? ".afile")
       (module-load-access-file ".afile")))
   ;; preload the hop library
   (eval `(library-load 'hop))
   ;; setup the client-side compiler
   (setup-client-compiler!)
   ;; setup the hop module resolvers
   (bigloo-module-extension-handler-set! (hop-module-extension-handler exp))
   (bigloo-module-resolver-set! (make-hop-module-resolver (bigloo-module-resolver)))
   ;; turn on debug to get line information
   (bigloo-debug-set! 1)
   ;; start the compilation stage
   (if (hopc-jsheap)
       ;; generate a js heap file from the source
       (jsheap)
       ;; compile the source file
       (compile-sources)))

;*---------------------------------------------------------------------*/
;*    setup-client-compiler! ...                                       */
;*---------------------------------------------------------------------*/
(define (setup-client-compiler!)
   ;; disable cache clearing otherwise parallel
   ;; invocations of hopc are impossible because one removes
   ;; the file of the other.
   (hop-clientc-clear-cache-set! #f)
   (init-hopscheme! :reader (lambda (p v) (hop-read p))
      :share (hopc-share-directory)
      :verbose (hop-verbose)
      :eval (lambda (e) (let ((op (open-output-string)))
			   (obj->javascript (eval e) op #f)
			   (close-output-port op)))
      :hop-compile (lambda (e p)
		      (obj->javascript e p #f))
      :features `(hop
		  ,(string->symbol (format "hop-~a" (hop-branch)))
		  ,(string->symbol (format "hop-~a" (hop-version))))
      :expanders `(labels match-case
			(define-tag . ,(eval 'hop-client-define-tag))))
   (init-clientc-compiler! :modulec hopscheme-compile-module
      :expressionc hopscheme-compile-expression
      :valuec hopscheme-compile-value 
      :macroe hopscheme-create-empty-macro-environment
      :filec hopscheme-compile-file
      :sexp->precompiled sexp->hopscheme
      :precompiled->sexp hopscheme->sexp
      :precompiled->JS-expression hopscheme->JS-expression
      :precompiled->JS-statement hopscheme->JS-statement
      :precompiled->JS-return hopscheme->JS-return))

;*---------------------------------------------------------------------*/
;*    compile-sources ...                                              */
;*---------------------------------------------------------------------*/
(define (compile-sources)

   (define (compile-javascript p)
      (let ((s (hopscheme-compile-file p '())))
	 (if (string? (hopc-destination))
	     (call-with-output-file (hopc-destination)
		(lambda (p) (display s p)))
	     (display s))))

   (define (compile-module exp)
      (match-case exp
	 ((module ?id . ?clauses)
	  ;; this produces a side effect
	  (hop-module-extension-handler exp)
	  ;; generate the new module clause
	  (let ((nclauses (filter (lambda (c)
				     (match-case c
					((<TILDE> . ?-) #f)
					(else #t)))
				  clauses)))
	     `(module ,id ,@nclauses)))
	 (else
	  (error "hopc" "Illegal module" exp))))
      
   (define (generate-bigloo in)
      
      (define (generate out)
	 (let loop ()
	    (let ((exp (hop-read in)))
	       (unless (eof-object? exp)
		  (match-case exp
		     ((module . ?-)
		      (pp (compile-module exp) out))
		     (else
		      (pp exp out)))
		  (loop)))))
      
      (if (string? (hopc-destination))
	  (call-with-output-file (hopc-destination) generate)
	  (generate (current-output-port))))
   
   (define (compile-bigloo in)
      (let* ((opts (hopc-bigloo-options))
	     (opts (cond
		      ((string? (hopc-destination))
		       (when (file-exists? (hopc-destination))
			  (delete-file (hopc-destination)))
		       (cons* "-o" (hopc-destination) opts))
		      ((and (pair? (hopc-sources))
			    (string? (car (hopc-sources))))
		       (let ((d (string-append
				 (prefix (car (hopc-sources))) ".o")))
			  (cons* "-o" d opts)))
		      (else
		       (cons "--to-stdout" opts))))
	     (cmd (format "| ~a - ~l -library hop -library hopscheme -fread-internal-src" (hopc-bigloo) opts))
	     (out (open-output-file cmd)))
	 (hop-verb 1 cmd "\n")
	 (unwind-protect
	    (let loop ()
	       (let ((exp (hop-read in)))
		  (unless (eof-object? exp)
		     (match-case exp
			((module . ?-)
			 (write (obj->string (compile-module exp)) out))
			(else
			 (write (obj->string exp) out)))
		     (loop))))
	    (close-output-port out))))
   
   (define (compile in)
      (if (eq? (hopc-pass) 'bigloo)
	  (generate-bigloo in)
	  (compile-bigloo in)))

   (cond
      ((eq? (hopc-pass) 'client-js)
       (for-each compile-javascript (hopc-sources)))
      ((pair? (hopc-sources))
       (for-each (lambda (s)
		    (call-with-input-file s compile))
		 (hopc-sources)))
      (else
       (compile (current-input-port)))))
				 
;*---------------------------------------------------------------------*/
;*    jsheap ...                                                       */
;*---------------------------------------------------------------------*/
(define (jsheap)

   (define (load-module mod path)
      (print "   ;; " mod)
      (display "   ")
      (write (with-input-from-file path read))
      (newline)
      (newline))
   
   (define (include-module module)
      (match-case module
	 (((and (? symbol?) ?sym) (and (? string?) ?path))
	  (load-module sym path))
	 ((? symbol?)
	  (load-module module (car ((bigloo-module-resolver) module (module-abase)))))
	 (else
	  (error "hopc" "Illegal module clause" module))))
	     
   (define (generate)
      (let ((exp (with-input-from-file (car (hopc-sources)) read)))
	 (match-case exp
	    ((module (and (? symbol?) ?name) . ?clauses)
	     (print "(heap " name)
	     (print "\n  (")
	     (for-each (lambda (c)
			  (match-case c
			     ((import . ?modules)
			      (for-each include-module modules))))
		clauses)
	     (print "  )\n")
	     (print "  ;; heap import")
	     (display "  (import  ")
	     (for-each (match-lambda
			  ((import . ?i)
			   (for-each (lambda (i)
					(display " ")
					(display i))
			      i)))
		clauses)
	     (print ")")
	     (print ")"))
	    (else
	     (error "hopc" "Illegal heap source file" exp)))))

   (if (string? (hopc-destination))
       (with-output-to-file (hopc-destination) generate)
       (generate)))
   
