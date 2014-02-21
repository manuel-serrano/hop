;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/js2scheme/compile.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 08:53:18 2013                          */
;*    Last change :  Tue Feb 11 18:57:14 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The js2scheme compiler driver                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_compile
   
   (import __js2scheme_ast
	   __js2scheme_stage
	   __js2scheme_dump
	   __js2scheme_parser
	   __js2scheme_syntax
	   __js2scheme_header
	   __js2scheme_symbol
	   __js2scheme_resolve
	   __js2scheme_return
	   __js2scheme_bestpractice
	   __js2scheme_this
	   __js2scheme_loopexit
	   __js2scheme_ronly
	   __js2scheme_property
	   __js2scheme_scheme
	   __js2scheme_stage)

   (export (j2s-compile in::input-port
	      #!key
	      (parser j2s-parser)
	      (driver (j2s-optim-driver))
	      tmp)
	   
	   (j2s-optim-driver)
	   (j2s-plain-driver)
	   (j2s-eval-driver)))

;*---------------------------------------------------------------------*/
;*    j2s-optim-driver ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-optim-driver)
   (list
      j2s-syntax-stage
      j2s-nodejs-header-stage
      j2s-hopscript-header-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-this-stage
      j2s-ronly-stage
      j2s-return-stage
      j2s-property-stage
      j2s-scheme-stage))

;*---------------------------------------------------------------------*/
;*    j2s-plain-driver ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-plain-driver)
   (list
      j2s-syntax-stage
      j2s-nodejs-header-stage
      j2s-hopscript-header-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-this-stage
      j2s-ronly-stage
      j2s-return-stage
      j2s-scheme-stage))

;*---------------------------------------------------------------------*/
;*    j2s-eval-driver ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-eval-driver)
   (list
      j2s-syntax-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-this-stage
      j2s-ronly-stage
      j2s-return-stage
      j2s-scheme-eval-stage))

;*---------------------------------------------------------------------*/
;*    j2s-compile ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-compile in::input-port
	   #!key
	   (parser j2s-parser)
	   (driver (j2s-optim-driver))
	   tmp)
   (unless (and (list? driver) (every (lambda (s) (isa? s J2SStage))))
      (bigloo-type-error "j2s-compile" "driver list" driver))
   (unless (procedure? parser)
      (error "j2s-compile" "procedure" parser))
   (let ((tmp (or tmp
		  (make-file-path (os-tmp) "J2S"
		     (basename (input-port-name in))))))
      (when (>=fx (bigloo-debug) 1) (make-directories tmp))
      (let ((ast (parser in)))
	 (if (eof-object? ast)
	     '()
	     (let loop ((ast ast)
			(driver driver)
			(count 0))
		(if (null? driver)
		    ast
		    (loop (stage-exec (car driver) ast  tmp count)
		       (cdr driver)
		       (+fx 1 count))))))))
