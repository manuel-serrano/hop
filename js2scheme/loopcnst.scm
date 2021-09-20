;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/loopcnst.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 20 07:58:36 2021                          */
;*    Last change :  Mon Sep 20 08:12:26 2021 (serrano)                */
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
      (comment "Loop constant lifting")
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
   (with-access::J2SFor this (init test incr body loc)
      (let ((cnsts (append (collect-constants* test '())
		      (collect-constants* body '())
		      (collect-constants* incr '()))))
	 (if (null? cnsts)
	     (call-default-walker)
	     (let ((decls (map (lambda (c)
				  (J2SLetOpt/vtype (j2s-type c)
				     '(ref init) (gensym '%lcnst) c))
			     cnsts)))
		(set! test (replace-cnsts! test cnsts decls))
		(set! body (replace-cnsts! body cnsts decls))
		(J2SLetRecBlock #f decls this))))))

;*---------------------------------------------------------------------*/
;*    loopcnst! ::J2SWhile ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (loopcnst! this::J2SWhile conf)
   (with-access::J2SWhile this (test body loc)
      (let ((cnsts (append (collect-constants* test '())
		      (collect-constants* body '()))))
	 (if (null? cnsts)
	     (call-default-walker)
	     (let ((decls (map (lambda (c)
				  (J2SLetOptRo '(ref init) (gensym '%lcnst) c))
			     cnsts)))
		(set! test (replace-cnsts! test cnsts decls))
		(set! body (replace-cnsts! body cnsts decls))
		(J2SLetRecBlock #f decls this))))))

;*---------------------------------------------------------------------*/
;*    collect-constants* ::J2SNode ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-constants* this::J2SNode env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-constants* ::J2SLetBlock ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-constants* this::J2SLetBlock env)
   (with-access::J2SLetBlock this (decls nodes)
      (let ((nenv (append decls env)))
	 (append (append-map (lambda (d) (collect-constants* d nenv)) decls)
	    (append-map (lambda (n) (collect-constants* n nenv)) nodes)))))

;*---------------------------------------------------------------------*/
;*    collect-constants* ::J2SExpr ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-constants* this::J2SExpr env)
   (if (constant? this env)
       (list this)
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    collect-constants* ::J2SRef ...                                  */
;*---------------------------------------------------------------------*/
(define-method (collect-constants* this::J2SRef env)
   '())

;*---------------------------------------------------------------------*/
;*    collect-constants* ::J2SLiteral ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-constants* this::J2SLiteral env)
   '())

;*---------------------------------------------------------------------*/
;*    replace-cnsts! ...                                               */
;*---------------------------------------------------------------------*/
(define (replace-cnsts! this::J2SNode cnsts decls)
   (for-each (lambda (c d)
		(with-access::J2SNode c (%info)
		   (set! %info (cons 'loopcnst d))))
      cnsts decls)
   (replace-constants! this))

;*---------------------------------------------------------------------*/
;*    replace-constants! ::J2SNode ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (replace-constants! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    replace-constants! ::J2SExpr ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (replace-constants! this::J2SExpr)
   (with-access::J2SExpr this (%info loc type)
      (if (and (pair? %info) (eq? (car %info) 'loopcnst))
	  (J2SRef (cdr %info) :type type)
	  (call-default-walker))))
		   
;*---------------------------------------------------------------------*/
;*    constant? ::J2SExpr ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (constant? this::J2SExpr env)
   #f)

;*---------------------------------------------------------------------*/
;*    constant? ::J2SLiteral ...                                       */
;*---------------------------------------------------------------------*/
(define-method (constant? this::J2SLiteral env)
   #t)

;*---------------------------------------------------------------------*/
;*    constant? ::J2SRegExp ...                                        */
;*---------------------------------------------------------------------*/
(define-method (constant? this::J2SRegExp env)
   #f)

;*---------------------------------------------------------------------*/
;*    constant? ::J2SArray ...                                         */
;*---------------------------------------------------------------------*/
(define-method (constant? this::J2SArray env)
   #f)

;*---------------------------------------------------------------------*/
;*    constant? ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (constant? this::J2SRef env)
   (with-access::J2SRef this (decl)
      (unless (memq decl env)
	 (with-access::J2SDecl decl (escape)
	    (unless escape
	       (not (decl-usage-has? decl '(assig))))))))

;*---------------------------------------------------------------------*/
;*    constant? ::J2SUnary ...                                         */
;*---------------------------------------------------------------------*/
(define-method (constant? this::J2SUnary env)
   (with-access::J2SUnary this (expr)
      (constant? expr env)))
      
;*---------------------------------------------------------------------*/
;*    constant? ::J2SParen ...                                         */
;*---------------------------------------------------------------------*/
(define-method (constant? this::J2SParen env)
   (with-access::J2SParen this (expr)
      (constant? expr env)))
      
;*---------------------------------------------------------------------*/
;*    constant? ::J2SBinary ...                                        */
;*---------------------------------------------------------------------*/
(define-method (constant? this::J2SBinary env)
   (with-access::J2SBinary this (lhs rhs)
      (and (constant? lhs env) (constant? rhs env))))

;*---------------------------------------------------------------------*/
;*    constant? ::J2SCacheCheck ...                                    */
;*---------------------------------------------------------------------*/
(define-method (constant? this::J2SCacheCheck env)
   (with-access::J2SCacheCheck this (obj owner)
      (when (constant? obj env)
	 (isa? owner J2SRecord))))
