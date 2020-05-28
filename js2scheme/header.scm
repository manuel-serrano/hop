;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/header.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 29 06:46:36 2013                          */
;*    Last change :  Sun Apr 12 16:09:16 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    js2scheme compilation header stage                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_header

   (include "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_parser
	   __js2scheme_scheme
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-constant)

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
	    (set! headers (hopscript-header id path loc ast conf)))))
   ast)

;*---------------------------------------------------------------------*/
;*    hopscript-header ...                                             */
;*---------------------------------------------------------------------*/
(define (hopscript-header::pair id path loc prog conf)
   
   (define (js-def-extern js bind writable expr
	      #!key (type 'unknown) (hidden-class #t) (scope '%scope))
      (instantiate::J2SDeclExtern
	 (loc loc)
	 (id js)
	 (writable writable)
	 (_usage (if (not writable) (usage '()) (usage '(assig))))
	 (scope scope)
	 (bind bind)
	 (itype type)
	 (binder 'let-opt)
	 (hidden-class hidden-class)
	 (val (if (isa? expr J2SNode)
		  expr
		  (instantiate::J2SPragma
		     (type type)
		     (loc loc)
		     (expr expr))))))

   (let ((%require (js-def-extern '%require #t #f 
		     `(nodejs-require %worker %this %module
			 ,(config-get conf :language "hopscript"))
		     :type 'function :scope '%hop))
	 (%import-meta (js-def-extern '%import-meta #t #f 
			  `(nodejs-import-meta %worker %this %module ,path)
			  :type 'object :scope '%hop)))
      (list
	 %require
	 %import-meta
	 (js-def-extern 'global #t #t '%this :type 'object)
	 (js-def-extern 'GLOBAL #t #f '%this :type 'object)
	 (js-def-extern 'module #t #t '%module :type 'object :hidden-class #f)
	 (js-def-extern 'exports #t #t
	    `(js-get %module ,(& "exports" prog) %scope))
	 (js-def-extern 'require #t #f
	    (instantiate::J2SRef
	       (loc loc)
	       (decl %require)))
	 (js-def-extern 'HEAD #t #f
	    `(nodejs-head %worker %this %scope %module))
	 (js-def-extern 'SCRIPT #t #f
	    `(nodejs-script %worker %this %scope %module))
	 (js-def-extern 'Worker #t #t
	    '(nodejs-worker %this %scope %module))
	 (js-def-extern '__filename #t #f
	    `(js-get %module ,(& "filename" prog) %scope)
	    :type 'string)
	 (js-def-extern '__dirname #t #f
	    `(js-string->jsstring
	      (dirname
		 (js-jsstring->string
		    (js-get %module ,(& "filename" prog) %scope))))
	    :type 'string)
	 (js-def-extern '%__GLOBAL #f #f
	    ;; this will not be compiled as a global (see scheme.scm)
	    `(js-put! GLOBAL ,(& "global" prog) GLOBAL #f %this))
	 (js-def-extern 'process #t #t '(nodejs-process %worker %this)
	    :type 'object)
	 (js-def-extern 'Object #t #t
	    `(with-access::JsGlobalObject %this (js-object) js-object))
	 (js-def-extern 'Array #t #t
	    `(with-access::JsGlobalObject %this (js-array) js-array))
	 (js-def-extern 'String #t #t
	    `(with-access::JsGlobalObject %this (js-string) js-string))
	 (js-def-extern 'RegExp #t #t
	    `(with-access::JsGlobalObject %this (js-regexp) js-regexp))
	 (js-def-extern 'Proxy #t #t
	    `(with-access::JsGlobalObject %this (js-proxy) js-proxy))
	 (js-def-extern 'Math #t #t
	    `(with-access::JsGlobalObject %this (js-math) js-math))
	 (if (or (string=? id "console.js") (string=? id "node_stdio.js"))
	     (instantiate::J2SUndefined
		(type 'undefined)
		(loc loc))
	     (js-def-extern 'console #t #f
		'(nodejs-require-core "console" %worker %this) :type 'object))
	 (if (string=? path "hop")
	     (instantiate::J2SUndefined
		(type 'undefined)
		(loc loc))
	     (js-def-extern 'hop #t #t
		'(nodejs-require-core "hop" %worker %this)))
	 (js-def-extern '%__INIT #f #f
	    ;; this will not be compiled as a global (see scheme.scm)
	    `(begin
		(nodejs-eval %this %scope)
		(nodejs-function %this %scope)
		,(unless (string=? path "buffer")
		    `(nodejs-bind-export! %this %scope
			(nodejs-require-core "buffer" %worker %this)
			,(& "Buffer" prog)))
		,(unless (string=? path "timers")
		    `(nodejs-bind-export! %this %scope
			(nodejs-require-core "timers" %worker %this)
			,(& "clearImmediate" prog)
			,(& "clearInterval" prog)
			,(& "clearTimeout" prog)
			,(& "setImmediate" prog)
			,(& "setInterval" prog)
			,(& "setTimeout" prog)))
		,(unless (string=? path "console")
		    `(nodejs-bind-export! %this %this
			%scope
			,(& "console" prog)))))
	 (instantiate::J2SUndefined
	    (type 'undefined)
	    (loc loc)))))
