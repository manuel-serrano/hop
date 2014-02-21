;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/js2scheme/ronly.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 07:55:23 2013                          */
;*    Last change :  Wed Oct 23 17:28:58 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Mark read-only variables in the J2S AST.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_ronly
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage)

   (export j2s-ronly-stage
	   (generic j2s-ronly ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-ronly-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-ronly-stage
   (instantiate::J2SStage
      (name "read-only")
      (comment "Mark read-only variables")
      (proc j2s-ronly)))

;*---------------------------------------------------------------------*/
;*    j2s-ronly ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (j2s-ronly this)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-ronly ::J2SProgram ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-ronly this::J2SProgram)
   (with-access::J2SProgram this (nodes)
      (for-each (lambda (o) (ronly! o)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SNode)
   (default-walk! this))

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SAssig ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SAssig)
   (with-access::J2SAssig this (lhs rhs)
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	    (with-access::J2SDecl decl (ronly id)
	       (set! ronly #f))))
      (ronly! rhs))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SDecl ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SDecl)
   (with-access::J2SDecl this (ronly id global)
      (set! ronly (not global)))
   this)
   
;*---------------------------------------------------------------------*/
;*    ronly! ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SDeclInit)
   (call-next-method)
   (with-access::J2SDeclInit this (val)
      (ronly! val))
   this)
   


