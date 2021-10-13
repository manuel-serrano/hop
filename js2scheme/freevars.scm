;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/freevars.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 21 06:42:40 2021                          */
;*    Last change :  Tue Oct 12 07:22:35 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    FREE-VARS? is true if and only if the ast THIS uses              */
;*    free variables.                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_freevars
   (library web)
   
   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint
	   __js2scheme_alpha
	   __js2scheme_use
	   __js2scheme_node-size)

   (export (free-vars?::bool ::J2SNode)))

;*---------------------------------------------------------------------*/
;*    free-vars? ...                                                   */
;*---------------------------------------------------------------------*/
(define (free-vars? this::J2SNode)
   (let ((res (make-cell #f)))
      (free-vars this '() res)
      (cell-ref res)))

;*---------------------------------------------------------------------*/
;*    free-vars ...                                                    */
;*    -------------------------------------------------------------    */
;*    A predicate that is true IFF the ast uses free variables.        */
;*---------------------------------------------------------------------*/
(define-walk-method (free-vars this::J2SNode env res::cell)
   (unless (cell-ref res)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    free-vars ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (free-vars this::J2SRef env res::cell)
   (with-access::J2SRef this (decl loc)
      (with-access::J2SDecl decl (scope id)
	 (when (and (not (isa? decl J2SDeclExtern))
		    (not (memq scope '(%scope global)))
		    (not (memq decl env)))
	    (cell-set! res #t)))))

;*---------------------------------------------------------------------*/
;*    free-vars ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (free-vars this::J2SFun env res)
   (unless (cell-ref res)
      (with-access::J2SFun this (decl params thisp body)
	 (free-vars body (cons* decl thisp (append params env)) res))))

;*---------------------------------------------------------------------*/
;*    free-vars ::J2SLetBlock ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (free-vars this::J2SLetBlock env res)
   (unless (cell-ref res)
      (with-access::J2SLetBlock this (decls nodes rec)
	 (let* ((benv (append decls env))
		(denv (if rec benv env)))
	    (or (find (lambda (d)
			 (when (isa? d J2SDeclInit)
			    (with-access::J2SDeclInit d (val)
			       (free-vars val denv res))))
		   decls)
		(find (lambda (n) (free-vars n benv res)) nodes))))))
	     
;*---------------------------------------------------------------------*/
;*    free-vars ::J2SBlock ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (free-vars this::J2SBlock env res)
   (unless (cell-ref res)
      (with-access::J2SBlock this (decls nodes)
	 (let loop ((env env)
		    (nodes nodes))
	    (cond
	       ((null? nodes)
		#f)
	       ((isa? (car nodes) J2SDeclInit)
		(with-access::J2SDeclInit (car nodes) (val)
		   (let ((env (cons (car nodes) env)))
		      (unless (free-vars val (cons (car nodes) env) res)
			 (loop env (cdr nodes))))))
	       ((isa? (car nodes) J2SDecl)
		(loop (cons (car nodes) env) (cdr nodes)))
	       (else
		(find (lambda (n) (free-vars n env res)) nodes)))))))
   
;*---------------------------------------------------------------------*/
;*    free-vars ::J2SCatch ...                                         */
;*---------------------------------------------------------------------*/
(define-method (free-vars this::J2SCatch env res)
   (unless (cell-ref res)
      (with-access::J2SCatch this (param body)
	 (free-vars body (cons param env) res))))

