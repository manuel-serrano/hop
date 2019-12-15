;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/objinit.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 10 14:08:36 2019                          */
;*    Last change :  Sat Dec 14 19:03:36 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    An optimization that annotated read-only literal objects         */
;*    (i.e., literal objects that are only used in get position).      */
;*                                                                     */
;*    This optimization is not sophisticated, it only checks           */
;*    literal objects assigned to get-only variables.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_objinit

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint)

   (export j2s-objinit-stage))

;*---------------------------------------------------------------------*/
;*    j2s-objinit-stage ...                                            */
;*---------------------------------------------------------------------*/
(define j2s-objinit-stage
   (instantiate::J2SStageProc
      (name "objinit")
      (comment "Annotate read-only literal objects")
      (proc j2s-objinit!)))

;*---------------------------------------------------------------------*/
;*    j2s-objinit! ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-objinit! this args)
   (when (isa? this J2SProgram)
     (objinit! this args))
   this)

;*---------------------------------------------------------------------*/
;*    objinit! ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (objinit! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    objinit! ::J2SAssig ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (objinit! this::J2SAssig arg)
   (with-access::J2SAssig this (lhs rhs)
      (when (and (isa? lhs J2SRef) (get-only-ref? lhs) (isa? rhs J2SObjInit))
	 (with-access::J2SObjInit rhs (ronly)
	    (set! ronly #t)))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    objinit! ::J2SDeclInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (objinit! this::J2SDeclInit arg)
   (with-access::J2SDeclInit this (val)
      (when (and (get-only-decl? this) (isa? val J2SObjInit))
	 (with-access::J2SObjInit val (ronly)
	    (set! ronly #t))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    get-only-ref? ...                                                */
;*---------------------------------------------------------------------*/
(define (get-only-ref? ref)
   (with-access::J2SRef ref (decl)
      (get-only-decl? decl)))

;*---------------------------------------------------------------------*/
;*    get-only-decl? ...                                               */
;*---------------------------------------------------------------------*/
(define (get-only-decl? decl)
   (not (decl-usage-has? decl
	   '(new ref assig set call delete
	     instanceof uninit rest eval))))

