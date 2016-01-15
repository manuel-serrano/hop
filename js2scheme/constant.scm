;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/constant.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Fri Jan 15 08:26:50 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Init the this variable of all function in non-strict mode        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_constant

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)

   (export j2s-constant-stage
	   (generic j2s-constant ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-constant-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-constant-stage
   (instantiate::J2SStageProc
      (name "constant")
      (comment "Pre-allocated constants")
      (proc j2s-constant)
      (optional #t)))

;*---------------------------------------------------------------------*/
;*    j2s-constant ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (j2s-constant this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-constant ::J2SProgram ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-constant this::J2SProgram args)
   (with-access::J2SProgram this (nodes headers decls loc pcache-size cnsts)
      (let ((env (make-env)))
	 (for-each (lambda (n) (constant! n env)) headers)
	 (for-each (lambda (n) (constant! n env)) decls)
	 (for-each (lambda (n) (constant! n env)) nodes)
	 (set! cnsts (reverse! (cdr env)))))
   this)

;*---------------------------------------------------------------------*/
;*    make-env ...                                                     */
;*---------------------------------------------------------------------*/
(define (make-env)
   (cons 0 '()))

;*---------------------------------------------------------------------*/
;*    add-env! ...                                                     */
;*---------------------------------------------------------------------*/
(define (add-env! val env::pair)
   (let ((n (car env)))
      (set-car! env (+fx 1 n))
      (set-cdr! env (cons val (cdr env)))
      n))

;*---------------------------------------------------------------------*/
;*    add-expr! ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-expr! this env::pair)
   (let ((index (add-env! this env)))
      (with-access::J2SExpr this (loc)
	 (instantiate::J2SPragma
	    (loc loc)
	    (expr `(vector-ref %cnsts ,index))))))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SNode env::pair)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SString ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SString env)
   (add-expr! this env))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SRegExp ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SRegExp env)
   (add-expr! this env))

