;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/cnstlift.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Tue Jul  7 19:27:03 2020                          */
;*    Last change :  Tue Jul  7 19:27:07 2020 (serrano)                */
;*    Copyright   :  2020 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Constant lifting optimization.                                   */
;*                                                                     */
;*    This optimization moves constant expressions that reference      */
;*    captured constant variable before the procedure creation.        */
;*    For instance:                                                    */
;*      function f( x ) {                                              */
;*         return f() {                                                */
;*            return "[" + x + "]";                                    */
;*         }                                                           */
;*      }                                                              */
;*    is transformed into:                                             */
;*      function f( x ) {                                              */
;*         let tmp = "[" + x + "]";                                    */
;*         return f() {                                                */
;*            return tmp;                                              */
;*         }                                                           */
;*      }                                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_cnstlift
   
   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-cnstlift-stage))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-cnstlift-stage
   (instantiate::J2SStageProc
      (name "cnstlift")
      (comment "Cnstlift optimization")
      (proc j2s-cnstlift!)
      (optional (lambda (conf)
		   (and (config-get conf :optim-cnstlift #f))))))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift! ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-cnstlift! this conf)
   (when (isa? this J2SProgram)
      (j2s-free-var this #f)
      (j2s-cnstlift-expression! this #f))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SNode vars)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SMethod ...                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SMethod vars)
   (with-access::J2SMethod this (function method)
      (with-access::J2SFun function (body loc)
	 (set! body (j2s-cnstlift-expression! body #f)))
      (with-access::J2SFun method (body loc)
	 (set! body (j2s-cnstlift-expression! body #f)))
      this))
      
;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SFun ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SFun vars)
   (let ((vars (make-cell '())))
      (with-access::J2SFun this (body loc)
	 (set! body (j2s-cnstlift-expression! body vars))
	 (if (pair? (cell-ref vars))
	     (let* ((lbl (gensym '%flift))
		    (be (J2SBindExit/type 'function lbl (J2SNop))))
		(with-access::J2SBindExit be (stmt)
		   (set! stmt
		      (J2SLetBlock (cell-ref vars)
			 (J2SReturn #t this be)))
		   be))
	     this))))
   
;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SExpr ...                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SExpr vars)
   (if (and (cell? vars) (pair? (cnst-expression-freevars this)))
       (with-access::J2SExpr this (loc type)
	  (let ((decl (J2SLetOptVUtype type '(ref) (gensym '%clift) this)))
	     (cell-set! vars (cons decl (cell-ref vars)))
	     (J2SRef decl :type type)))
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SLiteral ...                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SLiteral vars)
   this)
   
;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SRef ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SRef vars)
   this)

;*---------------------------------------------------------------------*/
;*    cnst-expression-freevars ...                                     */
;*    -------------------------------------------------------------    */
;*    Returns the list of free variables used in a constant            */
;*    expression. Returns NIL if the expression is not constant or     */
;*    if no free variable is used.                                     */
;*---------------------------------------------------------------------*/
(define (cnst-expression-freevars expr::J2SExpr)
   (let ((freevars (make-cell '()))
	 (iscnst (make-cell #t)))
      (cnst-expression expr iscnst freevars)
      (when (cell-ref iscnst)
	 (cell-ref freevars))))
	   
;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SNode ...                                    */
;*    -------------------------------------------------------------    */
;*    An expression is a constant expression iff:                      */
;*      - it uses only constants and immutable variables               */
;*      - at least one variable is free                                */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SNode iscnst freevars)
   (cell-set! iscnst #f)
   #f)

;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SUnary ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SUnary iscnst freevars)
   (with-access::J2SUnary this (expr)
      (when (cell-ref iscnst)
	 (cnst-expression expr iscnst freevars)))
   (cell-ref iscnst))
   
;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SBinary ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SBinary iscnst freevars)
   (with-access::J2SBinary this (lhs rhs)
      (when (cell-ref iscnst)
	 (cnst-expression lhs iscnst freevars)
	 (when (cell-ref iscnst)
	    (cnst-expression rhs iscnst freevars))))
   (cell-ref iscnst))

;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SLiteral ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SLiteral iscnst freevars)
   (cell-ref iscnst))
   
;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SRef iscnst freevars)
   (when (cell-ref iscnst)
      (with-access::J2SRef this (decl (free %info))
	 (with-access::J2SDecl decl (scope)
	    (cond
	       ((decl-usage-has? decl '(assig))
		(cell-set! iscnst #f))
	       ((not free)
		(when (eq? scope 'local)
		   (cell-set! iscnst #f)))
	       ((and (eq? scope 'local)
		     (not (memq decl (cell-ref freevars))))
		(cell-set! freevars (cons decl (cell-ref freevars))))))))
   (cell-ref iscnst))

;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SThis ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SThis iscnst freevars)
   (cell-set! iscnst #f)
   #f)

;*---------------------------------------------------------------------*/
;*    j2s-free-var ::J2SNode ...                                       */
;*    -------------------------------------------------------------    */
;*    Mark the free variable references                                */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-free-var this::J2SNode infun)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-free-var ::J2SRef ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-free-var this::J2SRef infun)
   (with-access::J2SRef this (decl (free %info))
      (with-access::J2SDecl decl (%info escape id)
	 (set! free (not (eq? infun %info))))))

;*---------------------------------------------------------------------*/
;*    j2s-free-var ::J2SLetBlock ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-free-var this::J2SLetBlock infun)
   (with-access::J2SLetBlock this (decls nodes)
      (for-each (lambda (d)
		   (with-access::J2SDecl d (%info) (set! %info infun)))
	 decls)
      (for-each (lambda (d) (j2s-free-var d infun)) decls)
      (for-each (lambda (n) (j2s-free-var n infun)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-free-var ::J2SFun ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-free-var this::J2SFun infun)
   (with-access::J2SFun this (params body)
      (for-each (lambda (p)
		   (with-access::J2SDecl p (%info)
		      (set! %info this)))
	 params)
      (j2s-free-var body this)))




