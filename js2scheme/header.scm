;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/header.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 29 06:46:36 2013                          */
;*    Last change :  Wed Jul 23 19:03:15 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    js2scheme compilation header stage                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_header

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_parser
	   __js2scheme_scheme)

   (export j2s-hopscript-stage
	   (generic j2s-hopscript-header::J2SProgram ::J2SProgram ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-hopscript-stage ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-hopscript-stage
   (instantiate::J2SStageProc
      (name "hopscript-header")
      (comment "HopScript Header (hopscript bootstrap)")
      (proc j2s-hopscript-header)))

;*---------------------------------------------------------------------*/
;*    j2s-hopscript-header ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (j2s-hopscript-header::J2SProgram ast::J2SProgram conf)
   (when (config-get conf :hopscript-header #t)
      (with-access::J2SProgram ast (nodes path loc mode)
	 (let* ((id (basename path))
		(path (or (config-get conf :module-path #f) path)))
	    (set! nodes (append (hopscript-header mode id path loc) nodes)))))
   ast)

;*---------------------------------------------------------------------*/
;*    hopscript-header ...                                             */
;*---------------------------------------------------------------------*/
(define (hopscript-header mode id path loc)
   
   (define (js-def-extern js expr)
      (instantiate::J2SDeclExtern
	 (loc loc)
	 (id js)
	 (name (j2s-scheme-id js))
	 (writable #f)
	 (global '%scope)
	 (bind #t)
	 (val (instantiate::J2SPragma
		 (loc loc)
		 (expr expr)))))

   (list
      (js-def-extern 'module '%module)
      (js-def-extern 'exports '(js-get %module 'exports %scope))
      (js-def-extern 'require '(nodejs-require %this %module))
      (js-def-extern 'Worker '(nodejs-worker %this %scope %module))
      (js-def-extern 'global '%this)
      (js-def-extern 'GLOBAL '%this)
      (js-def-extern 'process '(nodejs-process %worker %this))
      (js-def-extern 'console '(nodejs-require-core "console" %worker %this))
      (instantiate::J2SPragma
	 (loc loc)
	 (expr '(begin
		 (nodejs-eval %this %scope)
		 (nodejs-function %this %scope)
		 (nodejs-import! %this %scope
		    (nodejs-require-core "buffer" %worker %this) 'Buffer)
		 (nodejs-import! %this %scope
		    (nodejs-require-core "timers" %worker %this)))))
      (instantiate::J2SUndefined
	 (loc loc))))
