;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/hophz/main.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Tue Dec 17 16:39:04 2013 (serrano)                */
;*    Copyright   :  2004-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOPHZ entry point                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hophz

   (library scheme2js hopscheme hopwidget hop sqlite)

   (import  hophz_parseargs
	    hophz_param
	    hophz_login
	    hophz_action)

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
   (multiple-value-bind (actions arguments)
      (parse-args args)
      (for-each action-exec actions)))

;*---------------------------------------------------------------------*/
;*    setup-client-compiler! ...                                       */
;*---------------------------------------------------------------------*/
(define (setup-client-compiler!)
   ;; disable cache clearing otherwise parallel
   ;; invocations of hopc are impossible because one removes
   ;; the file of the other.
   (hop-clientc-clear-cache-set! #f)
   (init-hopscheme! :reader (lambda (p v) (hop-read p))
      :verbose (hophz-verbose)
      :eval (lambda (e) (let ((op (open-output-string)))
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
