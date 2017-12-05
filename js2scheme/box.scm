;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/box.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  1 13:36:09 2017                          */
;*    Last change :  Tue Dec  5 09:35:43 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Box/unbox (respec. tag/untag) numerical values of escaping       */
;*    functions.                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_box

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint)

   (export j2s-box-stage))

;*---------------------------------------------------------------------*/
;*    j2s-box-stage ...                                                */
;*---------------------------------------------------------------------*/
(define j2s-box-stage
   (instantiate::J2SStageProc
      (name "box")
      (comment "Type box integers")
      (proc j2s-box!)))

;*---------------------------------------------------------------------*/
;*    j2s-box! ::obj ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-box! this args)
   (when (isa? this J2SProgram)
      (let ((bint (if (=fx (config-get args :long-size 0) 64) 'bint 'obj)))
	 (j2s-box-program! this bint))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-box-program! ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-box-program! this::J2SProgram bint)
   (with-access::J2SProgram this (decls nodes)
      (for-each (lambda (d) (box! d bint)) decls)
      (for-each (lambda (d) (box! d bint)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    box! ::J2SNode ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (box! this::J2SNode bint)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    box ...                                                          */
;*---------------------------------------------------------------------*/
(define (box type bint)
   (if (memq type '(int32 uint32)) bint type))

;*---------------------------------------------------------------------*/
;*    box-function! ...                                                */
;*---------------------------------------------------------------------*/
(define (box-function! this::J2SFun bint)
   (with-access::J2SFun this (params rtype)
      (set! rtype (box rtype bint))
      (for-each (lambda (arg)
		   (with-access::J2SDecl arg (utype vtype itype)
		      (set! utype (box utype bint))
		      (set! itype (box itype bint))
		      (set! vtype (box vtype bint))))
	 params)
      this))

;*---------------------------------------------------------------------*/
;*    box! ::J2SDeclFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (box! this::J2SDeclFun bint)
   (with-access::J2SDeclFun this (usage val id)
      (unless (only-usage? '(init call) usage)
	 (box-function! val bint))
      (with-access::J2SFun val (body)
	 (box! body bint))
      this))
	  
;*---------------------------------------------------------------------*/
;*    box! ::J2SFun ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (box! this::J2SFun bint)
   (box-function! this bint)
   (with-access::J2SFun this (body)
      (box! body bint))
   this)

;*---------------------------------------------------------------------*/
;*    box! ::J2SBinary ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (box! this::J2SBinary bint)
   (with-access::J2SBinary this (op type)
      (when (memq op '(>> >>> << ^ & BIT_OR))
	 (set! type 'int32)))
   (call-default-walker))
