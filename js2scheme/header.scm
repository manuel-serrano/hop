;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/header.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 29 06:46:36 2013                          */
;*    Last change :  Wed Apr 16 14:07:41 2014 (serrano)                */
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
	   __js2scheme_parser)

   (export j2s-hopscript-header-stage
	   j2s-nodejs-header-stage
	   (generic j2s-hopscript-header::J2SProgram ::J2SProgram ::obj)
	   (generic j2s-nodejs-header::J2SProgram ::J2SProgram ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-hopscript-header-stage ...                                   */
;*---------------------------------------------------------------------*/
(define j2s-hopscript-header-stage
   (instantiate::J2SStage
      (name "hopscript-header")
      (comment "HopScript Header (global JS variables and module declaration)")
      (proc j2s-hopscript-header)))

;*---------------------------------------------------------------------*/
;*    j2s-nodejs-header-stage ...                                      */
;*---------------------------------------------------------------------*/
(define j2s-nodejs-header-stage
   (instantiate::J2SStage
      (name "nodejs-header")
      (comment "NodeJS pre-declarations")
      (proc j2s-nodejs-header)))

;*---------------------------------------------------------------------*/
;*    j2s-hopscript-header ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (j2s-hopscript-header::J2SProgram ast::J2SProgram args)
   (when (args-get args :hopscript-header #t)
      (with-access::J2SProgram ast (nodes path loc mode)
	 (let* ((id (basename path))
		(path (or (args-get args :module-path #f) path)))
	    (set! nodes (append (hopscript-header mode id path loc) nodes)))))
   ast)

;*---------------------------------------------------------------------*/
;*    hopscript-header ...                                             */
;*---------------------------------------------------------------------*/
(define (hopscript-header mode id path loc)
   
   (define (js-def-import-primitive js scm)
      (list (instantiate::J2SDeclExtern
	       (loc loc)
	       (id js)
	       (val (instantiate::J2SPragma
		       (loc loc)
		       (expr scm))))))

   (define (js-def-import-var js scm)
      (if (eq? mode 'hopscript)
	  (list (instantiate::J2SDeclExtern
		   (loc loc)
		   (id js)
		   (val (instantiate::J2SPragma
			   (loc loc)
			   (expr scm))))
	     (instantiate::J2SPragma
		(loc loc)
		(expr `(js-bind! %this %this ',js
			  :get (js-make-function %this (lambda (o) ,scm)
				  1
				  js)))))
	  (list (instantiate::J2SDeclExtern
		   (loc loc)
		   (id js)
		   (bind #t)
		   (val (instantiate::J2SPragma
			   (loc loc)
			   (expr scm)))))))

   (define (js-def-import-clone js scm)
      (let ((pragm (instantiate::J2SPragma
		      (loc loc)
		      (expr `(js-bind! %this %this ',js
				:get (lambda (o) ,js)))))
	    (decl (instantiate::J2SDeclInit
		     (loc loc)
		     (id js)
		     (val (instantiate::J2SPragma
			     (loc loc)
			     (expr scm))))))
	 (list decl pragm)
	 (list decl )))

   (define (js-def-import-pragma js name scm writable configurable enumerable)
      (let ((decl (instantiate::J2SDeclExtern
		     (loc loc)
		     (id js)
		     (name name)
		     (writable #f)
		     (val (instantiate::J2SPragma
			     (loc loc)
			     (expr scm))))))
	 (list decl
	    (instantiate::J2SPragma
	       (loc loc)
	       (expr `(js-bind! %this %this ',js
			 :value ,name
			 :writable ,writable
			 :configurable ,configurable
			 :enumerable ,enumerable))))))
   
   (define (js-def-import-special js scm)
      (js-def-import-pragma js js scm #f #f #f))
   
   (define (js-def-import-fun js scm #!key name)
      (js-def-import-pragma js name scm #t #t #f))
   
   `(;;; global-object
     ,@(js-def-import-primitive 'this
	  '%this)
     ,@(js-def-import-primitive '%module
	  `(%nodejs-module ,id ,path %this))
     ;; Global object properties
     ,(instantiate::J2SPragma
         (loc loc)
         (expr "end-of-header"))))

;*---------------------------------------------------------------------*/
;*    j2s-nodejs-header ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (j2s-nodejs-header::J2SProgram ast::J2SProgram args)
   (when (args-get args :nodejs-header #t)
      (with-access::J2SProgram ast (nodes module path loc)
	 (call-with-input-string (nodejs-header path)
	    (lambda (in)
	       (let ((prog (j2s-parser in '())))
		  (with-access::J2SProgram prog ((anodes nodes))
		     (set! nodes
			(append
			   anodes
			   nodes
			   (list (instantiate::J2SUnresolvedRef
				    (loc loc)
				    (id 'module)))))))))))
   ast)

;*---------------------------------------------------------------------*/
;*    nodejs-header ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-header path)
   (format "var module = #:%module;
var exports = module.exports;
var process = #:%nodejs-process( #:%this );
function require( name ) { return #:nodejs-require( name.toString(), #:%this ); }
var console = require( 'console' );
" path))
   
