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
;*    This optimization does two differents lifting:                   */
;*                                                                     */
;*    1. function lifting:                                             */
;*      This optimization moves constant expressions that reference    */
;*      captured constant variables before the procedure creation.     */
;*      For instance:                                                  */
;*        function f( x ) {                                            */
;*           return f() {                                              */
;*              return "[" + x + "]";                                  */
;*           }                                                         */
;*        }                                                            */
;*      is transformed into:                                           */
;*        function f( x ) {                                            */
;*           let tmp = "[" + x + "]";                                  */
;*           return f() {                                              */
;*              return tmp;                                            */
;*           }                                                         */
;*        }                                                            */
;*    2. loop lifting:                                                 */
;*      This second optimization moves constant expressions out of     */
;*      loop.s                                                         */
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
      (optional :optim-cnstlift)))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift! ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-cnstlift! this conf)
   (when (isa? this J2SProgram)
      (let ((verb (make-cell '())))
	 (j2s-free-var this #f '())
	 (j2s-cnstlift-expression! this #f 'fun verb)
	 (when (and (>= (config-get conf :verbose 0) 2) (pair? (cell-ref verb)))
	    (fprintf (current-error-port)
	       (format " [~a: ~(,)]"
		  (let ((fst (car (cell-ref verb))))
		     (cadr (if (pair? fst) fst (cell-ref fst))))
		  (map (lambda (c)
			  (if (cell? c)
			      (format "~a*" (caddr (cell-ref c)))
			      (caddr c)))
		     (cell-ref verb)))))))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SNode vars mode verb)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SLetBlock ...                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SLetBlock vars mode verb)

   (define (decompose-bindexit stmt::J2SBindExit)
      (with-access::J2SBindExit stmt (stmt)
	 (with-access::J2SLetBlock stmt (decls nodes)
	    (if (and (pair? nodes)
		     (null? (cdr nodes))
		     (isa? (car nodes) J2SReturn))
		(with-access::J2SReturn (car nodes) (expr)
		   (unless (isa? expr J2SFun)
		      (error "cnstlift"
			 "unexpected function transformation"
			 (j2s->list stmt)))
		   (values decls expr))
		(error "cnstlift"
		   "unexpected function transformation"
		   (j2s->list stmt))))))

   (with-access::J2SLetBlock this (decls nodes)
      ;; compile the declaration and lift one level up the potential
      ;; variable declarations introduced by the function
      ;; transformation
      (for-each (lambda (decl)
		   (cond
		      ((isa? decl J2SDeclFun)
		       (with-access::J2SDeclFun decl (val)
			  (let ((nval (j2s-cnstlift-expression! val vars mode verb)))
			     (cond
				((isa? nval J2SBindExit)
				 (multiple-value-bind (ndecls fun)
				    (decompose-bindexit nval)
				    (set! decls (append ndecls decls))
				    (set! val fun)))
				((not (isa? nval J2SFun))
				 (error "cnstlift"
				    "unexpected function transformation"
				    (j2s->list nval)))))))
		      ((isa? decl J2SDeclInit)
		       (with-access::J2SDeclInit decl (val)
			  (set! val (j2s-cnstlift-expression! val vars mode verb))))))
	 decls)
      (set! nodes
	 (map (lambda (n) (j2s-cnstlift-expression! n vars mode verb))
	    nodes))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SFor ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SFor vars mode verb)
   (with-access::J2SFor this (init test incr body loc)
      (set! init (j2s-cnstlift-expression! init vars mode verb))
      (let* ((vtest (make-cell '()))
	     (vincr (make-cell '()))
	     (vbody (make-cell '()))
	     (ntest (j2s-cnstlift-expression! test vtest 'loop verb))
	     (nincr (j2s-cnstlift-expression! incr vincr 'loop verb))
	     (nbody (j2s-cnstlift-expression! body vbody 'loop verb))
	     (tmps (append (cell-ref vtest) (cell-ref vincr) (cell-ref vbody))))
	 (set! test ntest)
	 (set! incr nincr)
	 (set! body nbody)
	 (if (pair? tmps)
	     (J2SLetBlock tmps this)
	     this))))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SWhile ...                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SWhile vars mode verb)
   (with-access::J2SWhile this (test body loc)
      (let* ((vtest (make-cell '()))
	     (vbody (make-cell '()))
	     (ntest (j2s-cnstlift-expression! test vtest 'loop verb))
	     (nbody (j2s-cnstlift-expression! body vbody 'loop verb))
	     (tmps (append (cell-ref vtest) (cell-ref vbody))))
	 (set! test ntest)
	 (set! body nbody)
	 (if (pair? tmps)
	     (J2SLetBlock tmps this)
	     this))))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SForIn ...                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SForIn vars mode verb)
   (with-access::J2SForIn this (lhs obj body loc)
      (let* ((vbody (make-cell '()))
	     (nbody (j2s-cnstlift-expression! body vbody 'loop verb))
	     (tmps (cell-ref vbody)))
	 (set! lhs (j2s-cnstlift-expression! lhs vars mode verb))
	 (set! obj (j2s-cnstlift-expression! obj vars mode verb))
	 (set! body nbody)
	 (if (pair? tmps)
	     (J2SLetBlock tmps this)
	     this))))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2STry ...                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-cnstlift-expression! this::J2STry vars mode verb)
   (with-access::J2STry this (body catch finally)
      (set! body (j2s-cnstlift-expression! body vars mode verb))
      (set! catch (j2s-cnstlift-expression! catch #f mode verb))
      (set! finally (j2s-cnstlift-expression! finally #f mode verb))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SMethod ...                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SMethod vars mode verb)
   (with-access::J2SMethod this (function method)
      (with-access::J2SFun function (body loc)
	 (set! body (j2s-cnstlift-expression! body #f mode verb)))
      (with-access::J2SFun method (body loc)
	 (set! body (j2s-cnstlift-expression! body #f mode verb)))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SFun ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SFun vars mode verb)
   (with-access::J2SFun this (body loc generator)
      (if generator
	  this
	  (let ((vars (make-cell '())))
	     (set! body (j2s-cnstlift-expression! body vars mode verb))
	     (if (pair? (cell-ref vars))
		 (let* ((lbl (gensym '%flift))
			(be (J2SBindExit/type 'function lbl (J2SNop))))
		    (with-access::J2SBindExit be (stmt)
		       (set! stmt
			  (J2SLetBlock (cell-ref vars)
			     (J2SReturn #t this be)))
		       be))
		 this)))))
   
;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SExpr ...                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SExpr vars mode verb)
   (if (and (cell? vars)
	    (if (eq? mode 'fun)
		(pair? (cnst-expression-freevars this))
		(list? (cnst-expression-freevars this))))
       (with-access::J2SExpr this (loc type)
	  (let ((decl (J2SLetOptVUtype type '(ref) (gensym '%clift) this)))
	     (cell-set! verb (cons loc (cell-ref verb)))
	     (cell-set! vars (cons decl (cell-ref vars)))
	     (J2SRef decl :type type)))
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SLiteral ...                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SLiteral vars mode verb)
   this)
   
;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SRef ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SRef vars mode verb)
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
		(when (memq scope '(local letblock))
		   (cell-set! iscnst #f)))
	       ((and (memq scope '(local letblock))
		     (not (memq decl (cell-ref freevars))))
		(cell-set! freevars (cons decl (cell-ref freevars))))))))
   (cell-ref iscnst))

;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SAccess ...                                  */
;*    -------------------------------------------------------------    */
;*    An access is a constation expression, iff it is a string         */
;*    access and that string is read-only.                             */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SAccess iscnst freevars)
   (when (cell-ref iscnst)
      (with-access::J2SAccess this (obj field)
	 (unless (or (isa? obj J2SString)
		     (and (isa? obj J2SRef)
			  (with-access::J2SRef obj (decl type)
			     (and (eq? type 'string)
				  (not (decl-usage-has? decl '(assig)))))
			  (or (and (isa? field J2SString)
				   (with-access::J2SString field (val)
				      (string=? val "length")))
			      (memq (j2s-type field) '(integer number)))))
	    (cell-set! iscnst #f)
	    #f)))
   (cell-ref iscnst))

;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SCall ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SCall iscnst freevars)
   (with-access::J2SCall this (fun args)
      ;; to be improved for string methods if needed
      (cell-set! iscnst #f)
      #f))
   
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
(define-walk-method (j2s-free-var this::J2SNode infun env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-free-var ::J2SRef ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-free-var this::J2SRef infun env)
   (with-access::J2SRef this (decl (free %info))
      (with-access::J2SDecl decl (%info escape id)
	 (set! free (and (not (eq? infun %info)) (not (memq decl env)))))))

;*---------------------------------------------------------------------*/
;*    j2s-free-var ::J2SLetBlock ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-free-var this::J2SLetBlock infun env)
   (with-access::J2SLetBlock this (decls nodes)
      (for-each (lambda (d)
		   (with-access::J2SDecl d (%info) (set! %info infun)))
	 decls)
      ;; when traversing a declaration initialization, the list
      ;; of defined variables is added to the environment in order
      ;; to prevent lifting expressions that might contain a
      ;; variable declared in the current block but not yet initialized
      (for-each (lambda (d) (j2s-free-var d infun (append decls env))) decls)
      (for-each (lambda (n) (j2s-free-var n infun env)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-free-var ::J2SFun ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-free-var this::J2SFun infun env)
   (with-access::J2SFun this (params body)
      (for-each (lambda (p)
		   (with-access::J2SDecl p (%info)
		      (set! %info this)))
	 params)
      (j2s-free-var body this env)))

;*---------------------------------------------------------------------*/
;*    j2s-free-var ::J2SKont ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-free-var this::J2SKont infun env)
   (with-access::J2SKont this (param exn body)
      (with-access::J2SDecl param (%info)
	 (set! %info this))
      (with-access::J2SDecl exn (%info)
	 (set! %info this))
      (j2s-free-var body this env)))
