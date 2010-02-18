;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/hopc/main.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Thu Feb 18 08:52:45 2010 (serrano)                */
;*    Copyright   :  2004-10 Manuel Serrano                            */
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
   (for-each register-eval-srfi! (hop-srfis))
   ;; set the library load path
   (bigloo-library-path-set! (hop-library-path))
   ;; parse the command line
   (parse-args args)
   ;; preload the hop library
   (eval `(library-load 'hop))
   ;; setup the client-side compiler
   (setup-client-compiler!)
   ;; turn on debug to get line information
   (bigloo-debug-set! 1)
   ;; start the compilation stage
   (compile-sources))

;*---------------------------------------------------------------------*/
;*    setup-client-compiler! ...                                       */
;*---------------------------------------------------------------------*/
(define (setup-client-compiler!)
   (init-hopscheme! :reader (lambda (p v) (hop-read p))
      :share (hopc-share-directory)
      :verbose (hop-verbose)
      :eval (lambda (e) (hop->json (eval e) #f #f))
      :hop-compile (lambda (e p) (display (hop->json e #f #f) p))
      :features `(hop
		  ,(string->symbol (format "hop-~a" (hop-branch)))
		  ,(string->symbol (format "hop-~a" (hop-version))))
      :expanders `(labels match-case
			(define-markup . ,(eval 'hop-client-define-markup))))
   (init-clientc-compiler! :modulec hopscheme-compile-module
      :expressionc hopscheme-compile-expression
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
	 (call-with-output-file (hopc-destination) (lambda (p) (display s p)))))

   (define (generate-bigloo in)
      
      (define (generate out)
	 (let loop ()
	    (let ((exp (hop-read in)))
	       (unless (eof-object? exp)
		  (pp exp out)
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
		     (write (obj->string exp) out)
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
       (for-each (lambda (s) (call-with-input-file s compile))
		 (hopc-sources)))
      (else
       (compile (current-input-port)))))
				 
