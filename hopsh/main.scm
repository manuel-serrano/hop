;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopsh/main.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Fri Nov 28 11:27:42 2014 (serrano)                */
;*    Copyright   :  2004-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOPSH entry point                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopsh

   (library scheme2js hopscheme hop)

   (import  hopsh_parseargs
	    hopsh_param
	    hopsh_repl)

   (main    main))

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
      :eval (lambda (e)
	       (let ((op (open-output-string)))
		  (obj->javascript-expr (eval e) op)
		  (close-output-port op)))
      :hop-compile (lambda (obj op compile)
		      (hop->javascript obj op compile #f))
      :hop-register hop-register-value
      :hop-library-path (hop-library-path)
      :javascript-version (hop-javascript-version)
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

