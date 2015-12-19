;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/header.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 29 06:46:36 2013                          */
;*    Last change :  Thu Dec 17 07:47:20 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
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
      (with-access::J2SProgram ast (headers path loc)
	 (let* ((id (basename path))
		(path (or (config-get conf :module-path #f) path)))
	    (set! headers (hopscript-header id path loc conf)))))
   ast)

;*---------------------------------------------------------------------*/
;*    hopscript-header ...                                             */
;*---------------------------------------------------------------------*/
(define (hopscript-header::pair id path loc conf)
   
   (define (js-def-extern js bind writable expr)
      (instantiate::J2SDeclExtern
	 (loc loc)
	 (id js)
	 (writable writable)
	 (global '%scope)
	 (bind bind)
	 (val (instantiate::J2SPragma
		 (loc loc)
		 (expr expr)))))

   (list
      (js-def-extern 'global #t #t '%this)
      (js-def-extern 'GLOBAL #t #f '%this)
      (js-def-extern 'module #t #t '%module)
      (js-def-extern 'exports #t #t '(js-get %module 'exports %scope))
      (js-def-extern 'require #t #f `(nodejs-require %worker %this %module ',(config-get conf :language 'hopscript)))
      (js-def-extern 'HEAD #t #f `(nodejs-head %worker %this %scope %module))
      (js-def-extern 'Worker #t #t '(nodejs-worker %this %scope %module))
      (js-def-extern '__filename #t #f '(js-get %module 'filename %scope))
      (js-def-extern '__dirname #t #f '(js-string->jsstring (dirname (js-jsstring->string (js-get %module 'filename %scope)))))
      (js-def-extern '%__GLOBAL #f #f
	 ;; this will not be compiled as a global (see scheme.scm)
	 `(js-put! GLOBAL 'global GLOBAL #f %this))
      (js-def-extern 'process #t #t '(nodejs-process %worker %this))
      (if (or (string=? id "console.js") (string=? id "node_stdio.js"))
	  (instantiate::J2SUndefined
	     (loc loc))
	  (js-def-extern 'console #t #f
	     '(nodejs-require-core "console" %worker %this)))
      (if (string=? path "hop")
	  (instantiate::J2SUndefined
	     (loc loc))
	  (js-def-extern 'hop #t #t
	     '(nodejs-require-core "hop" %worker %this)))
      (js-def-extern '%__INIT #f #f
	 ;; this will not be compiled as a global (see scheme.scm)
	 `(begin
	   (nodejs-eval %this %scope)
	   (nodejs-function %this %scope)
	   ,(unless (string=? path "buffer")
	       `(nodejs-import! %this %scope
		   (nodejs-require-core "buffer" %worker %this) 'Buffer))
	   ,(unless (string=? path "timers")
	       `(nodejs-import! %this %scope
		   (nodejs-require-core "timers" %worker %this)))))
      (instantiate::J2SUndefined
	 (loc loc))))
