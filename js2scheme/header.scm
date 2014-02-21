;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/js2scheme/header.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 29 06:46:36 2013                          */
;*    Last change :  Fri Feb 14 10:02:43 2014 (serrano)                */
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
	   (j2s-hopscript-header::J2SProgram ::J2SProgram)
	   (j2s-nodejs-header::J2SProgram ::J2SProgram)))

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
(define (j2s-hopscript-header::J2SProgram ast::J2SProgram)
   (with-access::J2SProgram ast (nodes module path loc mode path)
      (unless (pair? module)
	 (set! module '(module __node_module
			(library hop hopscript nodejs js2scheme)
			(eval (export %this) (export this)))))
      (set! nodes
	 (append (hopscript-header mode path (program-file-name ast) loc)
	    nodes)))
   ast)

;*---------------------------------------------------------------------*/
;*    hopscript-header ...                                             */
;*---------------------------------------------------------------------*/
(define (hopscript-header mode path filename loc)
   
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
		(expr `(js-bind! %this ',js
			  :get (js-make-function
				  (lambda (o) ,scm)
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
		      (expr `(js-bind! %this ',js
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
	       (expr `(js-bind! %this ',js
			 :value ,name
			 :writable ,writable
			 :configurable ,configurable
			 :enumerable ,enumerable))))))
   
   (define (js-def-import-special js scm)
      (js-def-import-pragma js js scm #f #f #f))
   
   (define (js-def-import-fun js scm #!key name)
      (js-def-import-pragma js name scm #t #t #f))
   
   `(;;; global-object
     ,@(js-def-import-primitive '%this '(js-clone (js-init-global-object!)))
     ,@(js-def-import-primitive 'this '%this)
     ,@(js-def-import-primitive '%module `(%nodejs-module ,path ,filename))
     ;; Global object properties
     ,(instantiate::J2SPragma
         (loc loc)
         (expr "end-of-header"))))

;*---------------------------------------------------------------------*/
;*    j2s-nodejs-header ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-nodejs-header::J2SProgram ast::J2SProgram)
   (with-access::J2SProgram ast (nodes module path loc)
      (let ((filename (program-file-name ast)))
	 (call-with-input-string (nodejs-header path filename)
	    (lambda (in)
	       (let ((prog (j2s-parser in)))
		  (with-access::J2SProgram prog ((anodes nodes))
		     (set! nodes (append anodes nodes)))
		  ast))))))

;*---------------------------------------------------------------------*/
;*    program-file-name ...                                            */
;*---------------------------------------------------------------------*/
(define (program-file-name ast::J2SProgram)
   (with-access::J2SProgram ast (nodes module path loc)
      (let ((imod (when (pair? module)
		     (let ((s (symbol->string (cadr module))))
			(when (string-prefix? "__nodejs_" s)
			   (substring s 9))))))
	 (or imod
	     (if (file-exists? path)
		 (file-name-canonicalize!
		    (make-file-name (pwd) path))
		 (pwd))))))
   
;*---------------------------------------------------------------------*/
;*    nodejs-header ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-header path filename)
   (format "var module = #:%module;
var exports = module.exports;
var process = #:%nodejs-process();
function require( name ) { return #:%nodejs-require( name, #:%module ); }
var console = require( 'console' );
" path filename))
   
