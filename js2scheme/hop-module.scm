;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/hop-module.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar  8 11:35:48 2019                          */
;*    Last change :  Fri Dec 17 12:10:55 2021 (serrano)                */
;*    Copyright   :  2019-21 Manuel Serrano                            */
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
   (parse-module (input-port-name in) (read in #t) args))

;*---------------------------------------------------------------------*/
;*    parse-module ...                                                 */
;*---------------------------------------------------------------------*/
(define (parse-module path module args)
   
   (match-case module
      ((module ?name . ?clauses)
       (when (>= (config-get args :verbose 0) 3)
	  (let ((margin (config-get args :verbmargin 0)))
	     (display "  " (current-error-port))
	     (display margin (current-error-port))
	     (display name (current-error-port))))
       (let ((exports (append-map (lambda (c)
				     (match-case c
					((export . ?exports)
					 (filter-map (lambda (e)
							(parse-export name e))
					    exports))
					(else
					 '())))
			 clauses)))
	  (let ((n 0))
	     (for-each (lambda (e)
			  (with-access::J2SExport e (index)
			     (set! index n)
			     (set! n (+fx n 1))))
		exports))
	  (instantiate::J2SProgram
	     (loc `(at ,path 0))
	     (endloc `(at ,path 0))
	     (exports exports)
	     (module name)
	     (mode 'hop)
	     (path path)
	     (nodes '()))))
      (else
       (raise
	  (instantiate::&io-parse-error
	     (proc "hop")
	     (msg "Illegal module")
	     (obj module)
	     (fname (cadr (cer module)))
	     (location (caddr (cer module))))))))

;*---------------------------------------------------------------------*/
;*    parse-export ...                                                 */
;*---------------------------------------------------------------------*/
(define (parse-export mod export)
   (match-case export
      ((?fun . ?args)
       (let ((id (id-of-id fun)))
	  (co-instantiate
		((expo (instantiate::J2SExport
			  (loc (cer export))
			  (id id)
			  (alias id)
			  (decl decl)
			  (from 'hop)))
		 (decl (instantiate::J2SDeclExtern
			  (loc (cer export))
			  (id id)
			  (writable #f)
			  (scope '%hop)
			  (vtype 'procedure)
			  (hidden-class #f)
;* 			  (exports (list expo))                        */
			  (val (instantiate::J2SPragma
				  (type 'procedure)
				  (loc (cer export))
				  (expr ""))))))
	     expo)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    id-of-id ...                                                     */
;*---------------------------------------------------------------------*/
(define (id-of-id id::symbol)
   (let* ((s (symbol->string! id))
	  (i (string-index s #\:)))
      (if i
	  (string->symbol (substring s 0 i))
	  id)))
	 
