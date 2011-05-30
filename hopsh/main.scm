;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/hopsh/main.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Mon May 30 14:45:05 2011 (serrano)                */
;*    Copyright   :  2004-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOPSH entry point                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopsh

   (library scheme2js hopscheme hop)

   (cond-expand
      (enable-threads (library pthread)))
   
   (import  hopsh_parseargs
	    hopsh_param
	    hopsh_repl)

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
   ;; preload the hop library
   (eval `(library-load 'hop))
   ;; setup the client-side compiler
   (setup-client-compiler!)
   ;; parse the command line
   (parse-args args)
   (hop-verb 1 "Starting hopsh (v" (hop-version) "):\n")
   ;; setup the hop readers
   (bigloo-load-reader-set! hop-read)
   ;; start the hop main loop
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 (exit 2))
      (unwind-protect
	 (hopsh-repl)
	 (newline))))

;*---------------------------------------------------------------------*/
;*    setup-client-compiler! ...                                       */
;*---------------------------------------------------------------------*/
(define (setup-client-compiler!)
   ;; disable cache clearing otherwise parallel
   ;; invocations of hopc are impossible because one removes
   ;; the file of the other.
   (hop-clientc-clear-cache-set! #f)
   (init-hopscheme! :reader (lambda (p v) (hop-read p))
      :verbose (hop-verbose)
      :eval (lambda (e) (let ((op (open-output-string)))
			   (obj->javascript (eval e) op #f)
			   (close-output-port op)))
      :hop-compile (lambda (e p) (obj->javascript e p #f))
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

