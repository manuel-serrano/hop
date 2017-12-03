;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/box.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  1 13:36:09 2017                          */
;*    Last change :  Sun Dec  3 06:09:57 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Box/unbox (respec. tag/untag) numerical values.                  */
;*    -------------------------------------------------------------    */
;*    This stage replaces on 32 bit platforms INT32 function           */
;*    arguments and return values of escaping functions with INTEGER.  */
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
      (unless (or (config-get args :int64 #f)
		  (=fx (config-get args :long-size 0) 64))
	 ;; no boxing for 32bit integers on 64bit platforms
	 (j2s-box-program! this args))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-box-program! ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-box-program! this::J2SProgram args)
   (with-access::J2SProgram this (decls nodes)
      (for-each box! decls)
      (for-each box! nodes)
      this))

;*---------------------------------------------------------------------*/
;*    box! ::J2SNode ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (box! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    box ...                                                          */
;*---------------------------------------------------------------------*/
(define (box type)
   (if (memq type '(int32 uint32)) 'integer type))

;*---------------------------------------------------------------------*/
;*    box-function! ...                                                */
;*---------------------------------------------------------------------*/
(define (box-function! this::J2SFun)
   (with-access::J2SFun this (params rtype)
      (set! rtype (box rtype))
      (for-each (lambda (arg)
		   (with-access::J2SDecl arg (utype vtype itype)
		      (set! utype (box utype))
		      (set! itype (box itype))
		      (set! vtype (box vtype))))
	 params)
      this))

;*---------------------------------------------------------------------*/
;*    box! ::J2SDeclFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (box! this::J2SDeclFun)
   (with-access::J2SDeclFun this (usage val id)
      (unless (only-usage? '(init call) usage)
	 (box-function! val))
      (with-access::J2SFun val (body)
	 (box! body))
      this))
	  
;*---------------------------------------------------------------------*/
;*    box! ::J2SFun ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (box! this::J2SFun)
   (box-function! this)
   (with-access::J2SFun this (body)
      (box! body))
   this)

;*---------------------------------------------------------------------*/
;*    box! ::J2SAssig ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (box! this::J2SAssig)
   (with-access::J2SAssig this (lhs rhs type)
      (set! rhs (box! rhs))
      (unless (isa? lhs J2SRef)
	 (set! type (box type)))
      this))
