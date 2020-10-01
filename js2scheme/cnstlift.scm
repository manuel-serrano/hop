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
      (j2s-cnstlift-mode! this conf 'fun)
      (j2s-cnstlift-mode! this conf 'loop))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-mode! ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-cnstlift-mode! this conf mode)
   (let ((verb (make-cell '())))
      (j2s-cnstlift-expression! this #f mode 0 verb)
      (when (and (>= (config-get conf :verbose 0) 2) (pair? (cell-ref verb)))
	 (fprintf (current-error-port)
	    (format " [~a(~a): ~(,)]" 
	       (let ((fst (car (cell-ref verb))))
		  (cadr (if (pair? fst) fst (cell-ref fst))))
	       mode
	       (map (lambda (c)
		       (if (cell? c)
			   (format "~a*" (caddr (cell-ref c)))
			   (caddr c)))
		  (cell-ref verb)))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SNode
		       vars mode::symbol depth::long verb::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SFun ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SFun
		       vars mode depth verb)
   (with-access::J2SFun this (body loc generator params)
      (if generator
	  this
	  (let ((vars (make-cell '()))
		(ndepth (if (eq? mode 'fun) (+fx depth 1) depth)))
	     ;; mark the function parameters
	     (for-each (lambda (p)
			  (with-access::J2SDecl p ((vdepth %info))
			     (set! vdepth (cons 'depth ndepth))))
		params)
	     (set! body (j2s-cnstlift-expression! body vars mode ndepth verb))
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
;*    j2s-cnstlift-expression! ::J2SKont ...                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SKont
		       vars mode depth verb)
   (let ((ndepth (if (eq? mode 'fun) (+fx depth 1) depth)))
      (with-access::J2SKont this (param exn body)
	 (with-access::J2SDecl param ((vdepth %info))
	    (set! vdepth (cons 'depth ndepth)))
	 (with-access::J2SDecl exn ((vdepth %info))
	    (set! vdepth (cons 'depth ndepth)))
	 (j2s-cnstlift-expression! body vars mode ndepth verb))))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SLetBlock ...                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SLetBlock
		       vars mode depth verb)

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

   (with-access::J2SLetBlock this (decls nodes rec)
      ;; mark declaration depths
      (for-each (lambda (decl)
		   (with-access::J2SDecl decl ((vdepth %info))
		      (set! vdepth (cons 'depth depth))))
	 decls)
      ;; compile the declaration and lift one level up the potential
      ;; variable declarations introduced by the function
      ;; transformation
      (for-each (lambda (decl)
		   (cond
		      ((isa? decl J2SDeclFun)
		       (with-access::J2SDeclFun decl (val)
			  (let ((nval (j2s-cnstlift-expression! val vars mode depth verb)))
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
			  (set! val (j2s-cnstlift-expression! val vars mode depth verb))))))
	 decls)
      (set! nodes
	 (map (lambda (n) (j2s-cnstlift-expression! n vars mode depth verb))
	    nodes))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SFor ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SFor vars mode depth verb)
   (with-access::J2SFor this (init test incr body loc)
      (set! init (j2s-cnstlift-expression! init vars mode depth verb))
      (let* ((ndepth (if (eq? mode 'loop) (+fx depth 1) depth))
	     (vtest (make-cell '()))
	     (vincr (make-cell '()))
	     (vbody (make-cell '()))
	     (ntest (j2s-cnstlift-expression! test vtest mode ndepth verb))
	     (nincr (j2s-cnstlift-expression! incr vincr mode ndepth verb))
	     (nbody (j2s-cnstlift-expression! body vbody mode ndepth verb))
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
(define-walk-method (j2s-cnstlift-expression! this::J2SWhile vars mode depth verb)
   (with-access::J2SWhile this (test body loc)
      (let* ((ndepth (if (eq? mode 'loop) (+fx depth 1) depth))
	     (vtest (make-cell '()))
	     (vbody (make-cell '()))
	     (ntest (j2s-cnstlift-expression! test vtest mode ndepth verb))
	     (nbody (j2s-cnstlift-expression! body vbody mode ndepth verb))
	     (tmps (append (cell-ref vtest) (cell-ref vbody))))
	 (set! test ntest)
	 (set! body nbody)
	 (if (pair? tmps)
	     (J2SLetBlock tmps this)
	     this))))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SForIn ...                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SForIn vars mode depth verb)
   (with-access::J2SForIn this (lhs obj body loc)
      (let* ((ndepth (if (eq? mode 'loop) (+fx depth 1) depth))
	     (vbody (make-cell '()))
	     (nbody (j2s-cnstlift-expression! body vbody mode ndepth verb))
	     (tmps (cell-ref vbody)))
	 (set! lhs (j2s-cnstlift-expression! lhs vars mode depth verb))
	 (set! obj (j2s-cnstlift-expression! obj vars mode depth verb))
	 (set! body nbody)
	 (if (pair? tmps)
	     (J2SLetBlock tmps this)
	     this))))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2STry ...                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-cnstlift-expression! this::J2STry vars mode depth verb)
   (with-access::J2STry this (body catch finally)
      (set! body (j2s-cnstlift-expression! body vars mode depth verb))
      (set! catch (j2s-cnstlift-expression! catch #f mode depth verb))
      (set! finally (j2s-cnstlift-expression! finally #f mode depth verb))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SMethod ...                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SMethod vars mode depth verb)
   (with-access::J2SMethod this (function method)
      (with-access::J2SFun function (body loc)
	 (set! body (j2s-cnstlift-expression! body #f mode depth verb)))
      (with-access::J2SFun method (body loc)
	 (set! body (j2s-cnstlift-expression! body #f mode depth verb)))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SExpr ...                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SExpr vars mode depth verb)
   (if (and (cell? vars) (pair? (cnst-expression-freevars this depth)))
       (with-access::J2SExpr this (loc type)
	  (let ((decl (J2SLetOptVUtype type '(ref) (gensym '%clift) this)))
	     (cell-set! verb (cons loc (cell-ref verb)))
	     (cell-set! vars (cons decl (cell-ref vars)))
	     (J2SRef decl :type type)))
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SLiteral ...                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SLiteral vars mode depth verb)
   this)
   
;*---------------------------------------------------------------------*/
;*    j2s-cnstlift-expression! ::J2SRef ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cnstlift-expression! this::J2SRef vars mode depth verb)
   this)

;*---------------------------------------------------------------------*/
;*    cnst-expression-freevars ...                                     */
;*    -------------------------------------------------------------    */
;*    Returns the list of free variables used in a constant            */
;*    expression. Returns NIL if the expression is not constant or     */
;*    if no free variable is used.                                     */
;*---------------------------------------------------------------------*/
(define (cnst-expression-freevars expr::J2SExpr depth)
   (let ((freevars (make-cell '()))
	 (iscnst (make-cell #t)))
      (cnst-expression expr iscnst freevars depth)
      (when (cell-ref iscnst)
	 (cell-ref freevars))))
	   
;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SNode ...                                    */
;*    -------------------------------------------------------------    */
;*    An expression is a constant expression iff:                      */
;*      - it uses only constants and immutable variables               */
;*      - at least one variable is free                                */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SNode iscnst freevars depth)
   (cell-set! iscnst #f)
   #f)

;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SUnary ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SUnary iscnst freevars depth)
   (with-access::J2SUnary this (expr)
      (when (cell-ref iscnst)
	 (cnst-expression expr iscnst freevars depth)))
   (cell-ref iscnst))
   
;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SBinary ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SBinary iscnst freevars depth)
   (with-access::J2SBinary this (lhs rhs loc)
      (when (cell-ref iscnst)
	 (cnst-expression lhs iscnst freevars depth)
	 (when (cell-ref iscnst)
	    (cnst-expression rhs iscnst freevars depth)))
      (cell-ref iscnst)))

;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SLiteral ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SLiteral iscnst freevars depth)
   (cell-ref iscnst))
   
;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SRef iscnst freevars depth)
   
   (define (depth? o)
      (and (pair? o) (eq? (car o) 'depth)))
   
   (when (cell-ref iscnst)
      (with-access::J2SRef this (decl loc)
	 
	 (with-access::J2SDecl decl (scope (vdepth %info))
	    (cond
	       ((decl-usage-has? decl '(assig))
		(cell-set! iscnst #f))
	       ((not (depth? vdepth))
		(cell-set! iscnst #f))
	       ((>=fx (cdr vdepth) depth)
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
(define-walk-method (cnst-expression this::J2SAccess iscnst freevars depth)
   (when (cell-ref iscnst)
      (with-access::J2SAccess this (obj field)
	 (if (or (isa? obj J2SString)
		 (and (isa? obj J2SRef)
		      (with-access::J2SRef obj (decl type)
			 (and (eq? type 'string)
			      (not (decl-usage-has? decl '(assig)))))
		      (or (and (isa? field J2SString)
			       (with-access::J2SString field (val)
				  (string=? val "length")))
			  (memq (j2s-type field) '(integer number)))))
	     (call-default-walker)
	     (cell-set! iscnst #f))))
   (cell-ref iscnst))

;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SCall ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SCall iscnst freevars depth)
   (with-access::J2SCall this (fun args)
      ;; to be improved for string methods if needed
      (cell-set! iscnst #f)
      #f))
   
;*---------------------------------------------------------------------*/
;*    cnst-expression ::J2SThis ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (cnst-expression this::J2SThis iscnst freevars depth)
   (cell-set! iscnst #f)
   #f)

