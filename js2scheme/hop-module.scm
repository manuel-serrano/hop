;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/hop-module.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar  8 11:35:48 2019                          */
;*    Last change :  Wed Sep 21 11:02:38 2022 (serrano)                */
;*    Copyright   :  2019-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop (Scheme) module parser used when a JS module imports         */
;*    a Hop module.                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_hop-module
   
   (include "token.sch"
	    "ast.sch")

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils)

   (export (hop-compile in #!key driver tmp #!rest args)))

;*---------------------------------------------------------------------*/
;*    hop-compile ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop-compile in #!key driver tmp #!rest args)
   (let ((path (input-port-name in)))
      (let loop ()
	 (let ((prgm (parse-export path (read in #t) args)))
	    (cond
	       ((eof-object? prgm)
		(raise
		   (instantiate::&io-parse-error
		      (proc "hop")
		      (msg "Cannot find hopscript function")
		      (obj path))))
	       ((not prgm)
		(loop))
	       (else
		prgm))))))

;*---------------------------------------------------------------------*/
;*    parse-export ...                                                 */
;*---------------------------------------------------------------------*/
(define (parse-export path exp args)
   
   (define (js-export id index)
      (co-instantiate
	    ((expo (instantiate::J2SExport
		      (loc (cer exp))
		      (id id)
		      (index index)
		      (alias id)
		      (decl decl)))
	     (decl (instantiate::J2SDeclExtern
		      (loc (cer exp))
		      (id id)
		      (writable #f)
		      (scope '%hop)
		      (vtype 'procedure)
		      (hidden-class #f)
		      (export expo)
		      (val (instantiate::J2SPragma
			      (type 'procedure)
			      (loc (cer exp))
			      (expr ""))))))
	 expo))

   (match-case exp
      ((cond-expand . ?-)
       (parse-export path (expand-once exp) args))
      ((define (hopscript %this this %scope %module) ??- ?exp)
       (match-case exp
	  ((js-export ?exports . ?body)
	   (when (>= (config-get args :verbose 0) 3)
	      (let ((margin (config-get args :verbmargin 0)))
		 (display "  " (current-error-port))
		 (display margin (current-error-port))
		 (display path (current-error-port))))
	   (let* ((fullexp (append exports '(default)))
		  (exports (map js-export fullexp (iota (length fullexp)))))
	      (instantiate::J2SProgram
		 (loc `(at ,path 0))
		 (endloc `(at ,path 0))
		 (exports exports)
		 (module path)
		 (mode 'hop)
		 (path path)
		 (nodes '()))))
	  (else
	   (raise
	      (instantiate::&io-parse-error
		 (proc "hop")
		 (msg "Cannot find export \"(js-export (id ...) ...)\"")
		 (obj exp)
		 (fname (cadr (cer exp)))
		 (location (caddr (cer exp))))))))
      ((define (and ?sig (hopscript . ?-)) . ?body)
       (raise
	  (instantiate::&io-parse-error
	     (proc "hop")
	     (msg "Wrong hopscript signature, expecting \"(hopscript %this this %scope %module)\"")
	     (obj sig)
	     (fname (cadr (cer exp)))
	     (location (caddr (cer exp))))))
      ((begin . ?rest)
       (any (lambda (exp) (parse-export path exp args)) rest))
      (else
       (if (eof-object? exp)
	   exp
	   #f))))
