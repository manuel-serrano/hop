;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/js2scheme/syntax.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 14:30:38 2013                          */
;*    Last change :  Tue Jan 14 17:47:16 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript Return -> bind-exit                                   */
;*    -------------------------------------------------------------    */
;*    This module implements the JavaScript return removal. After      */
;*    this pass, return are no longer expected in the tree. They       */
;*    are replaced with either bind-exit calls or by tail returns.     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_syntax

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage)

   (export j2s-syntax-stage
	   (generic j2s-syntax ::obj)
	   (syntax-error ::J2SNode ::bstring)))

;*---------------------------------------------------------------------*/
;*    j2s-syntax-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-syntax-stage
   (instantiate::J2SStage
      (name "syntax")
      (comment "Post parsing syntactic checks")
      (proc j2s-syntax)))

;*---------------------------------------------------------------------*/
;*    j2s-syntax ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (j2s-syntax this)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-syntax ::J2SProgram ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-syntax this::J2SProgram)
   (with-access::J2SProgram this (nodes)
      (for-each (lambda (o) (syntax o #f #f)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    syntax ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (syntax this::J2SNode inloop? inswitch?)
   (default-walk this inloop? inswitch?))

;*---------------------------------------------------------------------*/
;*    syntax ::J2SWhile ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (syntax this::J2SWhile inloop? inswitch?)
   (with-access::J2SWhile this (test body)
      (walk test inloop? inswitch?)
      (walk body #t #f)))

;*---------------------------------------------------------------------*/
;*    syntax ::J2SFor ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (syntax this::J2SFor inloop? inswitch?)
   (with-access::J2SFor this (init test incr body)
      (walk init inloop? inswitch?)
      (walk test inloop? inswitch?)
      (walk incr inloop? inswitch?)
      (walk body #t inswitch?)))

;*---------------------------------------------------------------------*/
;*    syntax ::J2SForIn ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (syntax this::J2SForIn inloop? inswitch?)
   (with-access::J2SForIn this (lhs obj body)
      (walk lhs inloop? inswitch?)
      (walk obj inloop? inswitch?)
      (walk body #t inswitch?)))

;*---------------------------------------------------------------------*/
;*    syntax ::J2SSwitch ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (syntax this::J2SSwitch inloop? inswitch?)
   (with-access::J2SSwitch this (key cases)
      (walk key inloop? inswitch?)
      (for-each (lambda (case)
		   (with-access::J2SCase case (expr body)
		      (walk expr inloop? inswitch?)
		      (walk body inloop? #t)))
	 cases)))
   
;*---------------------------------------------------------------------*/
;*    syntax ::J2SContinue ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (syntax this::J2SContinue inloop? inswitch?)
   (unless inloop? 
      (syntax-error this "Illegal continue statement")))

;*---------------------------------------------------------------------*/
;*    syntax ::J2SBreak ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (syntax this::J2SBreak inloop? inswitch?)
   (unless (or inloop? inswitch?)
      (syntax-error this "Illegal break statement")))

;*---------------------------------------------------------------------*/
;*    syntax-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (syntax-error this::J2SNode msg)
   (with-access::J2SNode this (loc)
      (match-case loc
	 ((at ?fname ?loc)
	  (raise
	     (instantiate::&io-parse-error
		(proc "js-parser")
		(msg msg)
		(obj (j2s->list this))
		(fname fname)
		(location loc))))
	 (else
	  (raise
	     (instantiate::&io-parse-error
		(proc "js-parser")
		(msg msg)
		(obj (j2s->list this))))))))
