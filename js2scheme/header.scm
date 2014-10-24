;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/header.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 29 06:46:36 2013                          */
;*    Last change :  Thu Oct 23 21:29:47 2014 (serrano)                */
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
   
   (define (js-def-extern js bind writable expr)
      (instantiate::J2SDeclExtern
	 (loc loc)
	 (id js)
	 (name (j2s-scheme-id js))
	 (writable writable)
	 (global '%scope)
	 (bind bind)
	 (val (instantiate::J2SPragma
		 (loc loc)
		 (expr expr)))))

   (list
      (js-def-extern 'global #t #f '%this)
      (js-def-extern 'GLOBAL #t #f '%this)
      (js-def-extern 'module #t #t '%module)
      (js-def-extern 'exports #t #t '(js-get %module 'exports %scope))
      (js-def-extern 'require #t #f '(nodejs-require %this %module))
      (js-def-extern 'Worker #t #f '(nodejs-worker %this %scope %module))
      (js-def-extern '__filename #t #f '(js-get %module 'filename %scope))
      (js-def-extern '__dirname #t #f '(dirname (js-get %module 'filename %scope)))
      (js-def-extern '%__GLOBAL #f #f
	 ;; this will not be compiled as a global (see scheme.scm)
	 `(js-put! GLOBAL 'global GLOBAL #f %this))
      (js-def-extern 'process #t #t '(nodejs-process %worker %this))
      (if (string=? id "console.js")
	  (instantiate::J2SUndefined
	     (loc loc))
	  (js-def-extern 'console #t #f
	     '(nodejs-require-core "console" %worker %this)))
      (js-def-extern '%__INIT #f #f
	 ;; this will not be compiled as a global (see scheme.scm)
	 `(begin
	   (nodejs-eval %this GLOBAL)
	   (nodejs-function %this GLOBAL)
	   ,(unless (string=? id "buffer.js")
	       `(nodejs-import! %this GLOBAL
		   (nodejs-require-core "buffer" %worker %this) 'Buffer))
	   ,(unless (string=? id "timers.js")
	       `(nodejs-import! %this GLOBAL
		   (nodejs-require-core "timers" %worker %this)))))
      (instantiate::J2SUndefined
	 (loc loc))))
