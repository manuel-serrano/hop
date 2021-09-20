;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/loopcnst.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 20 07:58:36 2021                          */
;*    Last change :  Mon Sep 20 08:11:07 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Loop constant lifting                                            */
;*    -------------------------------------------------------------    */
;*    This optimization moves before the loop constant expressions.    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_loopcnst

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_node-size
	   __js2scheme_alpha)

   (export j2s-loopcnst-stage))

;*---------------------------------------------------------------------*/
;*    j2s-loopcnst-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-loopcnst-stage
   (instantiate::J2SStageProc
      (name "loopcnst")
      (comment "Loop type cnstialization")
      (optional :optim-loopcnst)
      (proc j2s-loopcnst!)))

;*---------------------------------------------------------------------*/
;*    j2s-loopcnst! ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-loopcnst! this conf)
   (when (isa? this J2SProgram)
      (j2s-loopcnst-program! this conf))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-loopcnst-program! ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-loopcnst-program! this::J2SProgram conf)
   (with-access::J2SProgram this (headers decls nodes)
      (for-each (lambda (n) (loopcnst! n conf)) decls)
      (for-each (lambda (n) (loopcnst! n conf)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    loopcnst! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (loopcnst! this::J2SNode conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    loopcnst! ::J2SFor ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (loopcnst! this::J2SFor conf)
   (with-access::J2SFor this (init test incr body)
      this))

;*---------------------------------------------------------------------*/
;*    loopcnst! ::J2SWhile ...                                         */
;*---------------------------------------------------------------------*/
(define-method (loopcnst! this::J2SWhile conf)
   (with-access::J2SWhile this (test body)
      this))

