;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/letclass.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 06:35:14 2015                          */
;*    Last change :  Sat Oct  2 07:05:00 2021 (serrano)                */
;*    Copyright   :  2015-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Let class optimization. This optimization implements a single    */
;*    transformation: immutable declclasses bound to undefined are     */
;*    directly bound to classes but the classes are marked as neeeding */
;*    dead-zone checks.                                                */
;*    -------------------------------------------------------------    */
;*    This optimization cannot run before the "use" stage.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_letclass

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer
	   __js2scheme_alpha
	   __js2scheme_usage
	   __js2scheme_freevars)

   (export j2s-letclass-stage))

;*---------------------------------------------------------------------*/
;*    j2s-letclass-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-letclass-stage
   (instantiate::J2SStageProc
      (name "letclass")
      (comment "Class declarations optimization")
      (proc j2s-letclass)
      (optional :optim-letclass)))

;*---------------------------------------------------------------------*/
;*    j2s-letclass ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-letclass this args)
   (when (isa? this J2SProgram)
      (letclass! this args)
   this))

;*---------------------------------------------------------------------*/
;*    letclass! ...                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (letclass! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    letclass! ::J2SInit ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (letclass! this::J2SInit args)
   (with-access::J2SInit this (lhs rhs loc)
      (call-default-walker)
      (if (and (isa? lhs J2SRef) (isa? rhs J2SClass))
	  (with-access::J2SRef lhs (decl)
	     (when (isa? decl J2SDeclClass)
		(when (not (decl-usage-has? decl '(assig)))
		   (with-access::J2SDeclClass decl (val binder)
		      (when (isa? val J2SUndefined)
			 (with-access::J2SClass rhs (need-dead-zone-check)
			    (set! binder 'let)
			    (decl-usage-add! decl 'uninit)
			    (set! need-dead-zone-check #t)
			    (set! val rhs))))))
	     (J2SUndefined))
	  this)))


	  
