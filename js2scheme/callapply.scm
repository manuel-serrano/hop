;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/callapply.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  6 14:30:50 2018                          */
;*    Last change :  Mon Aug  6 15:00:58 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Turns indirect CALL and APPLY method calls into direction        */
;*    function calls.                                                  */
;*    -------------------------------------------------------------    */
;*    This early optimization differs from the Scheme code generation  */
;*    optimization that is also able to remove CALL and APPLY calls    */
;*    as this one enable the other analyses to eliminate totally the   */
;*    closure generation.                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_callapply

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-callapply-stage))

;*---------------------------------------------------------------------*/
;*    j2s-callapply-stage ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-callapply-stage
   (instantiate::J2SStageProc
      (name "callapply")
      (comment "Remove CALL and APPLY method calls")
      (proc j2s-callapply!)
      (optional :optim-callapply)))

;*---------------------------------------------------------------------*/
;*    j2s-callapply! ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-callapply! this args)
   (when (isa? this J2SProgram)
      (callapply! this args))
   this)

;*---------------------------------------------------------------------*/
;*    callapply! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (callapply! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    callapply! ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (callapply! this::J2SCall args)

   (define (opt-call this::J2SCall fun::J2SRef)
      (with-access::J2SCall this (loc args)
	 (with-access::J2SRef fun (decl)
	    (if (isa? decl J2SDeclFun)
		(J2SMethodCall* fun
		   (if (pair? args) (list (car args)) (list (J2SUndefined)))
		   (if (pair? args) (cdr args) (list (J2SUndefined))))
		(call-default-walker)))))
   
   (define (opt-apply this::J2SCall fun::J2SRef)
      (with-access::J2SCall this (loc args)
	 (with-access::J2SRef fun (decl)
	    (if (isa? decl J2SDeclFun)
		(with-access::J2SArray (cadr args) (exprs)
		   (J2SMethodCall* fun (list (car args)) exprs))
		(call-default-walker)))))

   (with-access::J2SCall this (fun args thisarg)
      (if (isa? fun J2SAccess)
	  (with-access::J2SAccess fun (obj field)
	     (if (and (isa? field J2SString) (isa? obj J2SRef))
		 (with-access::J2SString field (val)
		    (cond
		       ((string=? val "call")
			(opt-call this obj))
		       ((and (string=? val "apply")
			     (pair? args) (pair? (cdr args)) (null? (cddr args))
			     (isa? (cadr args) J2SArray))
			(opt-apply this obj))
		       (else
			(call-default-walker))))
		 (call-default-walker)))
	  (call-default-walker))))
   
