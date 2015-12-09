;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/ecmascript5.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  9 10:55:09 2015                          */
;*    Last change :  Wed Dec  9 15:29:01 2015 (serrano)                */
;*    Copyright   :  2015 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    ES5 transformations                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_ecmascript5

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_parser
	   __js2scheme_scheme)

   (export j2s-ecmascript5-stage
	   (generic j2s-ecmascript5::J2SProgram ::J2SProgram ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-ecmascript5-stage ...                                        */
;*---------------------------------------------------------------------*/
(define j2s-ecmascript5-stage
   (instantiate::J2SStageProc
      (name "ecmascript5-ecmascript5")
      (comment "Ecmascript5 transformations")
      (proc j2s-ecmascript5)))

;*---------------------------------------------------------------------*/
;*    j2s-ecmascript5 ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (j2s-ecmascript5::J2SProgram ast::J2SProgram conf)
   (when (config-get conf :hopscript-header #t)
      (with-access::J2SProgram ast (headers path loc)
	 (let* ((id (basename path))
		(path (or (config-get conf :module-path #f) path)))
	    (set! headers (ecmascript5-header id path loc conf)))))
   ast)

;*---------------------------------------------------------------------*/
;*    js-header ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (js-header)
   (file->string "ecmascript5.js"))

;*---------------------------------------------------------------------*/
;*    ecmascript5-header ...                                           */
;*---------------------------------------------------------------------*/
(define (ecmascript5-header::pair id path loc conf)
   (list (instantiate::J2SPragma
	    (loc loc)
	    (lang 'javascript)
	    (expr (js-header)))))




   
