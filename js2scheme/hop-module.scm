;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/hop-module.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar  8 11:35:48 2019                          */
;*    Last change :  Tue Mar 29 14:06:05 2022 (serrano)                */
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
      ((define (hopscript %this this %scope %module) ??- ?exp)
       (match-case exp
	  ((js-export ?exports . ?body)
	   (when (>= (config-get args :verbose 0) 3)
	      (let ((margin (config-get args :verbmargin 0)))
		 (display "  " (current-error-port))
		 (display margin (current-error-port))
		 (display path (current-error-port))))
	   (let ((exports (map js-export exports (iota (length exports)))))
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
		 (msg "Export missing")
		 (obj exp)
		 (fname (cadr (cer exp)))
		 (location (caddr (cer exp))))))))
      ((define (hopscript . ?-) . ?body)
       (raise
	  (instantiate::&io-parse-error
	     (proc "hop")
	     (msg "Wrong hopscript signature")
	     (obj exp)
	     (fname (cadr (cer exp)))
	     (location (caddr (cer exp))))))
      (else
       #f)))
