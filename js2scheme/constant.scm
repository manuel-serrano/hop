;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/constant.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Sun Feb  7 07:25:14 2016 (serrano)                */
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
      (let ((env (env 0 '() (make-hashtable))))
	 (for-each (lambda (n) (constant! n env 0)) headers)
	 (for-each (lambda (n) (constant! n env 0)) decls)
	 (for-each (lambda (n) (constant! n env 0)) nodes)
	 (set! cnsts (reverse! (env-list env)))))
   this)

;*---------------------------------------------------------------------*/
;*    env ...                                                          */
;*---------------------------------------------------------------------*/
(define-struct env cnt list table)

;*---------------------------------------------------------------------*/
;*    add-env! ...                                                     */
;*---------------------------------------------------------------------*/
(define (add-env! this env::struct)
   (with-access::J2SLiteralValue this (val)
      (let* ((t (env-table env))
	     (old (hashtable-get t val)))
	 (or old
	     (let ((n (env-cnt env)))
		(hashtable-put! t val n)
		(env-cnt-set! env (+fx 1 n))
		(env-list-set! env (cons this (env-list env)))
		n)))))

;*---------------------------------------------------------------------*/
;*    add-expr! ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-expr! this env::struct)
   (let ((index (add-env! this env)))
      (with-access::J2SExpr this (loc)
	 (instantiate::J2SLiteralCnst
	    (type (j2s-type this))
	    (loc loc)
	    (index index)
	    (val this)))))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SNode env::struct nesting)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SString ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SString env nesting)
   (if (=fx nesting 0)
       (add-expr! this env)
       this))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SRegExp ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SRegExp env nesting)
   (if (=fx nesting 0)
       (add-expr! this env)
       this))

;*---------------------------------------------------------------------*/
;*    constant! ::J2STilde ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2STilde env nesting)
   (with-access::J2STilde this (stmt)
      (set! stmt (constant! stmt env (+fx nesting 1)))
      this))
   
;*---------------------------------------------------------------------*/
;*    constant! ::J2SDollar ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SDollar env nesting)
   (with-access::J2SDollar this (node)
      (set! node (constant! node env (-fx nesting 1)))
      this))
   
