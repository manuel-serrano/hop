;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/uint32.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  1 13:36:09 2017                          */
;*    Last change :  Sat Mar 18 07:29:29 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Map uint32 types to plain integers.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_uint32

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint)

   (export j2s-uint32-stage))

;*---------------------------------------------------------------------*/
;*    j2s-uint32-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-uint32-stage
   (instantiate::J2SStageProc
      (name "uint32")
      (comment "Type uint32s")
      (proc j2s-uint32!)))

;*---------------------------------------------------------------------*/
;*    j2s-uint32! ::obj ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-uint32! this args)
   (when (isa? this J2SProgram)
      (unless (config-get args :uint32 #f)
	 (j2s-uint32-program! this args))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-uint32-program! ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-uint32-program! this::J2SProgram args)
   (with-access::J2SProgram this (decls nodes)
      (let ((m64 (=fx (config-get args :long-size 0) 64)))
	 (for-each (lambda (n) (uint32! n m64)) decls)
	 (for-each (lambda (n) (uint32! n m64)) nodes)
	 this)))

;*---------------------------------------------------------------------*/
;*    uint->number ...                                                 */
;*---------------------------------------------------------------------*/
(define (uint->number type m64)
   (case type
      ;;((uint29) 'ufixnum)
      ((index length) (if m64 'ufixnum 'obj))
      (else type)))
	   
;*---------------------------------------------------------------------*/
;*    uint32! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (uint32! this::J2SNode m64)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    uint32! ::J2SDecl ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (uint32! this::J2SDecl m64)
   (call-default-walker)
   (with-access::J2SDecl this (vtype)
      (set! vtype (uint->number vtype m64)))
   this)

;*---------------------------------------------------------------------*/
;*    uint32! ::J2SExpr ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (uint32! this::J2SExpr m64)
   (call-default-walker)
   (with-access::J2SExpr this (type)
      (set! type (uint->number type m64)))
   this)

;*---------------------------------------------------------------------*/
;*    uint32! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (uint32! this::J2SFun m64)
   (call-default-walker)
   (with-access::J2SFun this (rtype)
      (set! rtype (uint->number rtype m64)))
   this)
