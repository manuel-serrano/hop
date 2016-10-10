;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/type.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 18 18:44:55 2016                          */
;*    Last change :  Thu Oct  6 08:44:40 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Type inference                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_type

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint)

   (export j2s-type-stage))

;*---------------------------------------------------------------------*/
;*    j2s-type-stage ...                                               */
;*---------------------------------------------------------------------*/
(define j2s-type-stage
   (instantiate::J2SStageProc
      (name "type")
      (comment "transform generator in TYPE")
      (proc j2s-type!)))

;*---------------------------------------------------------------------*/
;*    j2s-type! ::obj ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-type! this args)
   (when (isa? this J2SProgram)
      (j2s-type-program! this args)
      (when (>= (config-get args :optim 0) 2)
	 (let loop ((i 1))
	    (if (j2s-hint! this args)
		(begin
;* 		   (j2s-type-program! this args)                       */
		   (j2s-call-hint! this args)
		   (when (>=fx (bigloo-debug) 4)
		      (fprintf (current-error-port) "\n       ~a:" i))
		   (j2s-type-program! this args)
		   (loop (+fx i 1))
		   this)
		this))
	 (type-reduce! this))
      this))

;*---------------------------------------------------------------------*/
;*    merge-type ...                                                   */
;*---------------------------------------------------------------------*/
(define (merge-type current new)
   (cond
      ((or (not new) (eq? current new)) current)
      ((not current) new)
      ((eq? current 'obj) 'obj)
      ((and (eq? current 'number) (eq? new 'integer)) 'number)
      ((and (eq? current 'object) (eq? new 'string)) 'obj)
      ((and (eq? current 'integer) (eq? new 'number)) 'number)
      ((and (eq? current 'string) (eq? new 'object)) 'obj)
      (else 'obj)))

;*---------------------------------------------------------------------*/
;*    ctx->list ...                                                    */
;*---------------------------------------------------------------------*/
(define (ctx->list env)
   (map (lambda (decl)
	   (with-access::J2SDecl (car decl) (id key)
	      (cons id (cdr decl))))
      env))

;*---------------------------------------------------------------------*/
;*    env-type ...                                                     */
;*---------------------------------------------------------------------*/
(define (env-type this::J2SDecl env::pair-nil)
   (let ((c (assq this env)))
      (when (pair? c)
	 (cdr c))))

;*---------------------------------------------------------------------*/
;*    env-extend ...                                                   */
;*---------------------------------------------------------------------*/
(define (env-extend::pair-nil decl::J2SDecl type env::pair-nil)
   (if type
       (let ((c (assq decl env)))
	  (cons (cons decl type) env))
       env))

;*---------------------------------------------------------------------*/
;*    env-merge ...                                                    */
;*---------------------------------------------------------------------*/
(define (env-merge::pair-nil env1::pair-nil env2::pair-nil)
   (let ((res env1))
      ;; left -> right
      (let ((res (filter (lambda (tydecl1)
			    (let* ((decl1 (car tydecl1))
				   (ty1 (cdr tydecl1))
				   (c2 (assq decl1 env2)))
			       (and ty1 c2 (eq? (cdr c2) ty1))))
		    env1)))
	 ;; right -> left
	 (for-each (lambda (tydecl2)
		      (let* ((decl2 (car tydecl2))
			     (ty2 (cdr tydecl2))
			     (c1 (assq decl2 env1)))
			 (when (pair? c1)
			    (unless (eq? (cdr c1) ty2)
			       (set! res (env-extend decl2 'obj res))))))
	    env2)
	 res)))
      
;*---------------------------------------------------------------------*/
;*    j2s-type-program! ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-type-program! this::J2SProgram args)
   (with-access::J2SProgram this (headers decls nodes)
      (when (>=fx (bigloo-debug) 4)
	 (display " " (current-error-port)))
      (let ((fix (make-cell #f)))
	 (let loop ((i 1))
	    (unless (cell-ref fix)
	       (when (>=fx (bigloo-debug) 4)
		  (fprintf (current-error-port) "~a." i)
		  (flush-output-port (current-error-port)))
	       (cell-set! fix #t)
	       (for-each (lambda (o) (type o '() #f fix)) headers)
	       (for-each (lambda (o) (type o '() #f fix)) decls)
	       (for-each (lambda (o) (type o '() #f fix)) nodes)
	       (loop (+fx i 1))))))
   this)

;*---------------------------------------------------------------------*/
;*    decl-type-set! ...                                               */
;*---------------------------------------------------------------------*/
(define (decl-type-set! this::J2SDecl etype fix::cell)
   (with-access::J2SDecl this (type)
      (when etype
	 (let ((ntype (merge-type type etype)))
	    (unless (eq? ntype type)
	       (cell-set! fix #f)
	       (set! type ntype))
	    ntype))))

;*---------------------------------------------------------------------*/
;*    expr-type-set! ...                                               */
;*---------------------------------------------------------------------*/
(define (expr-type-set! this::J2SExpr etype env::pair-nil fix::cell)
   (with-access::J2SExpr this (type)
      (when etype
	 (let ((ntype (merge-type type etype)))
	    (unless (eq? ntype type)
	       (cell-set! fix #f)
	       (set! type ntype))
	    ntype))
      (values type env #f)))

;*---------------------------------------------------------------------*/
;*    type* ...                                                        */
;*---------------------------------------------------------------------*/
(define (type* nodes env::pair-nil fun fix::cell)
   (let loop ((nodes nodes)
	      (etype #f)
	      (env env)
	      (ebreak #f))
      (if (null? nodes)
	  (values type env ebreak)
	  (multiple-value-bind (etype env break)
	     (type (car nodes) env fun fix)
	     (loop (cdr nodes) etype env (or ebreak break))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SNode ...                                               */
;*    -------------------------------------------------------------    */
;*    node x env x fix -> type x env x break                           */
;*                                                                     */
;*      node: J2SNode                                                  */
;*      env: binding list                                              */
;*      fix: cell                                                      */
;*      type: symbol                                                   */
;*      break: boolean                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SNode env::pair-nil fun fix::cell)
   ;; conservative guard
   (tprint "in node: " (typeof this))
   (values 'void '() #f))

;*---------------------------------------------------------------------*/
;*    type ::J2SExpr ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SExpr env::pair-nil fun fix::cell)
   ;; conservative guard
   (cond
      ((or (isa? this J2SPragma)
	   (isa? this J2SUnresolvedRef)
	   (isa? this J2SThis)
	   (isa? this J2STilde))
       (values #f env #f))
      ((isa? this J2SArrayAbsent)
       (values 'undefined '() #f))
      (else
       (begin
	  (tprint "in expr: " (typeof this))
	  (values 'obj '() #f)))))

;*---------------------------------------------------------------------*/
;*    type ::J2SStmt ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SStmt env::pair-nil fun fix::cell)
   ;; conservative guard
   (if (or (isa? this J2SBreak)
	   (isa? this J2SContinue))
       (values 'void env #f)
       (begin
	  (tprint "in stmt: " (typeof this))
	  (values 'void '() #f))))

;*---------------------------------------------------------------------*/
;*    type ::J2SHopRef ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SHopRef env::pair-nil fun fix::cell)
   (values #f env #f))

;*---------------------------------------------------------------------*/
;*    type ::J2SExprStmt ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SExprStmt env::pair-nil fun fix::cell)
   (with-access::J2SExprStmt this (stmt)
      (multiple-value-bind (type env break)
	 (type stmt env fun fix)
	 (values 'undefined env break))))

;*---------------------------------------------------------------------*/
;*    type ::J2STemplate ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type this::J2STemplate env::pair-nil fun fix::cell)
   (with-access::J2STemplate this (exprs)
      (multiple-value-bind (type env break)
	 (type* exprs env fun fix)
	 (values 'string env break))))

;*---------------------------------------------------------------------*/
;*    type ::J2SSequence ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SSequence env::pair-nil fun fix::cell)
   (with-access::J2SSequence this (exprs)
      (type* exprs env fun fix)))
      
;*---------------------------------------------------------------------*/
;*    type ::J2SArray ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SArray env::pair-nil fun fix::cell)
   (with-access::J2SArray this (exprs len)
      (multiple-value-bind (vtype venv _)
	 (type* exprs env fun fix)
	 (expr-type-set! this 'array venv fix))))

;*---------------------------------------------------------------------*/
;*    type ::J2SNull ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SNull env::pair-nil fun fix::cell)
   (expr-type-set! this 'null env fix))

;*---------------------------------------------------------------------*/
;*    type ::J2SUndefined ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SUndefined env::pair-nil fun fix::cell)
   (expr-type-set! this 'undefined env fix))

;*---------------------------------------------------------------------*/
;*    type ::J2SRef ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SRef env::pair-nil fun fix::cell)
   (with-access::J2SRef this (decl type)
      (with-access::J2SDecl decl ((typdecl type))
	 (expr-type-set! this (or typdecl (env-type decl env)) env fix))))

;*---------------------------------------------------------------------*/
;*    type ::J2SNumber ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SNumber env::pair-nil fun fix::cell)
   (with-access::J2SNumber this (val)
      (expr-type-set! this (if (fixnum? val) 'integer 'number) env fix)))

;*---------------------------------------------------------------------*/
;*    type ::J2SBool ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SBool env::pair-nil fun fix::cell)
   (expr-type-set! this 'bool env fix))

;*---------------------------------------------------------------------*/
;*    type ::J2SString ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SString env::pair-nil fun fix::cell)
   (expr-type-set! this 'string env fix))

;*---------------------------------------------------------------------*/
;*    type ::J2SNativeString ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SNativeString env::pair-nil fun fix::cell)
   (expr-type-set! this 'string env fix))

;*---------------------------------------------------------------------*/
;*    type ::J2SRegExp ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SRegExp env::pair-nil fun fix::cell)
   (expr-type-set! this 'regexp env fix))

;*---------------------------------------------------------------------*/
;*    type ::J2SLiteralCnst ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SLiteralCnst env::pair-nil fun fix::cell)
   (with-access::J2SLiteralCnst this (val)
      (multiple-value-bind (vtype venv break)
	 (type val env fun fix)
	 (expr-type-set! this vtype venv fix))))

;*---------------------------------------------------------------------*/
;*    type ::J2SObjInit ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SObjInit env::pair-nil fun fix::cell)
   (with-access::J2SObjInit this (inits)
      (multiple-value-bind (type env break)
	 (type* (filter-map (lambda (init)
			       (when (isa? init J2SDataPropertyInit)
				  (with-access::J2SDataPropertyInit init (val)
				     val)))
		   inits)
	    env fun fix)
	 (expr-type-set! this 'object env fix))))

;*---------------------------------------------------------------------*/
;*    type ::J2SAssig ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SAssig env::pair-nil fun fix::cell)
   (with-access::J2SAssig this (lhs rhs)
      ;; evaluate lhs first
      (multiple-value-bind (ltype lenv break)
	 (type lhs env fun fix)
	 ;; evaluat rhs second
	 (multiple-value-bind (rtype renv break)
	    (type rhs lenv fun fix)
	    (let ((nenv (if (isa? lhs J2SRef)
			    (with-access::J2SRef lhs (decl)
			       (env-extend decl (or rtype 'obj) renv))
			    renv)))
	       (values rtype nenv #f))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SPrefix ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SPrefix env::pair-nil fun fix::cell)
   (with-access::J2SPrefix this (lhs rhs)
      (multiple-value-bind (ltype lenv break)
	 (type lhs env fun fix)
	 (let ((nenv (if (isa? lhs J2SRef)
			 (with-access::J2SRef lhs (decl)
			    (env-extend decl 'number lenv))
			 lenv)))
	    (values 'number nenv #f)))))

;*---------------------------------------------------------------------*/
;*    type ::J2SPostfix ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SPostfix env::pair-nil fun fix::cell)
   (with-access::J2SPostfix this (lhs rhs)
      (multiple-value-bind (ltype lenv break)
	 (type lhs env fun fix)
	 (let ((nenv (if (isa? lhs J2SRef)
			 (with-access::J2SRef lhs (decl)
			    (env-extend decl 'number lenv))
			 lenv)))
	    (values 'number nenv #f)))))

;*---------------------------------------------------------------------*/
;*    type ::J2SFun ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SFun env::pair-nil fun fix::cell)
   (with-access::J2SFun this (body params)
      (let ((envp (filter-map (lambda (p)
				 (with-access::J2SDecl p (type usage)
				    (cond
				       ((eq? usage 'rest)
					(cons p 'array))
				       ((and type (not (eq? type 'obj)))
					(cons p type)))))
		     params)))
	 (multiple-value-bind (ftype fenv break)
	    (type body envp this fix)
	    ;; Until we have something smarter, all free variables
	    ;; of the functions are set to obj. Changing this requires
	    ;; to improve J2SCall.
	    (for-each (lambda (decl)
			 (when (assq decl fenv)
			    (decl-type-set! decl 'obj fix)))
	       env)
	    (filter-map (lambda (p)
				 (with-access::J2SDecl p (type usage)
				    (cond
				       ((eq? usage 'rest)
					(cons p 'array))
				       ((and type (not (eq? type 'obj)))
					(cons p type)))))
		     params)
	    (expr-type-set! this 'function env fix)))))

;*---------------------------------------------------------------------*/
;*    type ::J2SKont ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SKont env::pair-nil fun fix::cell)
   (with-access::J2SKont this (body param exn)
      (let ((envp (filter-map (lambda (p)
				 (with-access::J2SDecl p (type)
				    (when (and type (not (eq? type 'obj)))
				       (cons p type))))
		     (list param exn))))
	 (multiple-value-bind (ftype fenv break)
	    (type body envp #f fix)
	    ;; until we have something smarter, all free variables
	    ;; of the functions are set to obj
	    (for-each (lambda (decl)
			 (when (assq decl fenv)
			    (decl-type-set! decl 'obj fix)))
	       env)
	    (expr-type-set! this 'function env fix)))))

;*---------------------------------------------------------------------*/
;*    type ::J2SCall ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SCall env::pair-nil fun fix::cell)

   (define (type-args args)
      (let loop ((args args)
		 (env env))
	 (if (null? args)
	     env
	     (multiple-value-bind (_ env __)
		(type (car args) env fun fix)
		(loop (cdr args) env)))))

   (define (type-fun f env)
      (with-access::J2SFun f (rettype)
	 (expr-type-set! this rettype env fix)))

   (define (type-default f env)
      (multiple-value-bind (_ fenv)
	 (type f env fun fix)
	 (values #f fenv #f)))

   (with-access::J2SCall this ((f fun) args)
      (let ((env (type-args args)))
	 (cond
	    ((isa? f J2SFun)
	     (type-fun f env))
	    ((isa? f J2SRef)
	     (with-access::J2SRef f (decl)
		(cond
		   ((isa? decl J2SDeclFunCnst)
		    (with-access::J2SDeclFunCnst decl ((f val))
		       (type-fun f env)))
		   ((isa? decl J2SDeclFun)
		    (with-access::J2SDeclFun decl (ronly (f val))
		       (if ronly
			   (type-fun f env)
			   (values #f env #f))))
		   (else
		    (type-default f env)))))
	    ((isa? f J2SHopRef)
	     (with-access::J2SHopRef f ((htype type))
		(expr-type-set! this htype env fix)))
	    (else
	     (type-default f env))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SUnary ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SUnary env::pair-nil fun fix::cell)
   (with-access::J2SUnary this (op expr)
      (case op
	 ((~ -)
	  (expr-type-set! this 'number env fix))
	 ((!)
	  (expr-type-set! this 'bool env fix))
	 (else
	  (multiple-value-bind (_ env break)
	     (type expr env fun fix)
	     (values #f env #f))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SBinary ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SBinary env::pair-nil fun fix::cell)
   (with-access::J2SBinary this (op lhs rhs)
      (multiple-value-bind (lhstype lhsenv _)
	 (type lhs env fun fix)
	 (multiple-value-bind (rhstype rhsenv _)
	    (type rhs lhsenv fun fix)
	    (case op
	       ((+)
		(cond
		   ((and (eq? lhstype 'number) (eq? rhstype 'number))
		    (expr-type-set! this 'number rhsenv fix))
		   ((or (eq? lhstype 'string) (eq? rhstype 'string))
		    (expr-type-set! this 'string rhsenv fix))
		   (else
		    (values #f rhsenv #f))))
	       ((- * / %)
		(expr-type-set! this 'number rhsenv fix))
	       ((== === != !== < <= > >= instanceof)
		(expr-type-set! this 'bool env fix))
	       ((&&)
		(if (and lhstype rhstype)
		    (expr-type-set! this (merge-type lhstype rhstype) rhsenv fix)
		    (values #f rhsenv #f)))
	       ((OR)
		(if (and lhstype rhstype)
		    (expr-type-set! this (merge-type lhstype rhstype) rhsenv fix)
		    (values #f rhsenv #f)))
	       ((<< >> >>> ^ & BIT_OR)
		(expr-type-set! this 'number rhsenv fix))
	       (else
		(values #f rhsenv #f)))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SParen ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SParen env::pair-nil fun fix::cell)
   (with-access::J2SParen this (expr)
      (multiple-value-bind (etype eenv break)
	 (type expr env fun fix)
	 (expr-type-set! this etype eenv fix))))

;*---------------------------------------------------------------------*/
;*    type ::J2SAccess ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SAccess env::pair-nil fun fix::cell)

   (define (length? field)
      (cond
	 ((isa? field J2SLiteralCnst)
	  (with-access::J2SLiteralCnst field (val)
	     (length? val)))
	 ((isa? field J2SString)
	  (with-access::J2SString field (val)
	     (string=? val "length")))
	 (else
	  #f)))

   (with-access::J2SAccess this (obj field)
      (multiple-value-bind (tyobj tyenv )
	 (type obj env fun fix)
	 (multiple-value-bind (tyfield tyfield _)
	    (type field tyenv fun fix)
	    (if (and (memq tyobj '(array string)) (j2s-field-length? field))
		(expr-type-set! this 'integer tyenv fix)
		(values #f tyenv fix))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SCond ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SCond env::pair-nil fun fix::cell)
   (with-access::J2SCond this (test then else)
      (multiple-value-bind (itype ienv _)
	 (type test env fun fix)
	 (multiple-value-bind (ttype tenv _)
	    (type then ienv fun fix)
	    (multiple-value-bind (etype eenv _)
	       (type else ienv fun fix)
	       (let ((cenv (env-merge tenv eenv))
		     (ctype (if (eq? ttype etype) ttype #f)))
		  (expr-type-set! this ctype cenv fix)))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SNew ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SNew env::pair-nil fun fix::cell)
   (with-access::J2SNew this (clazz args)
      (multiple-value-bind (_ env _)
	 (type clazz env fun fix)
	 (multiple-value-bind (_ env break)
	    (type* args env fun fix)
	    (expr-type-set! this 'object env fix)))))

;*---------------------------------------------------------------------*/
;*    type ::J2SDecl ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SDecl env::pair-nil fun fix::cell)
   (with-access::J2SDecl this (type id)
      (values 'void (env-extend this type env) #f)))

;*---------------------------------------------------------------------*/
;*    type ::J2SDeclInit ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SDeclInit env::pair-nil fun fix::cell)
   (with-access::J2SDeclInit this (val id (dtype type))
      (set! dtype (merge-type dtype (type val env fun fix)))
      (values 'void (env-extend this dtype env) #f)))

;*---------------------------------------------------------------------*/
;*    type ::J2SDeclFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SDeclFun env::pair-nil fun fix::cell)
   (with-access::J2SDeclFun this (val (ftype type) id)
      (set! ftype (merge-type ftype 'function))
      (type val env fun fix)
      (values 'void (env-extend this 'function env) #f)))

;*---------------------------------------------------------------------*/
;*    type ::J2SDecFunCnst ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SDeclFunCnst env::pair-nil fun fix::cell)
   (with-access::J2SDeclFunCnst this (val (ftype type))
      (set! ftype (merge-type ftype 'function))
      (type val env fun fix)
      (values (env-extend this 'function env) #f)))

;*---------------------------------------------------------------------*/
;*    type ::J2SSeq ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SSeq env::pair-nil fun fix::cell)
   (with-access::J2SSeq this (nodes)
      (let loop ((nodes nodes)
		 (env env))
	 (if (null? nodes)
	     (values 'void env #f)
	     (begin
		(multiple-value-bind (_ senv break)
		   (type (car nodes) env fun fix)
		   (if break
		       (begin
			  (set-cdr! nodes '())
			  (values 'void env #t))       
		       (loop (cdr nodes) senv))))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SLetBlock ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type this::J2SLetBlock env::pair-nil fun fix::cell)
   (with-access::J2SLetBlock this (decls)
      (multiple-value-bind (_ denv dbreak)
	 (type* decls env fun fix)
	 (set! env denv)
	 (multiple-value-bind (_ benv break)
	    (call-next-method)
	    (values _ benv (or dbreak break))))))
	       
      
;*---------------------------------------------------------------------*/
;*    type ::J2SReturn ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SReturn env::pair-nil fun fix::cell)
   (with-access::J2SReturn this (expr)
      (multiple-value-bind (etype eenv ebreak)
	 (type expr env fun fix)
	 (when fun
	    ;; not a top-level form
	    (with-access::J2SFun fun (%info name rettype)
	       (set! rettype (merge-type rettype etype))
	       (values 'void eenv #t))))))
   
;*---------------------------------------------------------------------*/
;*    type ::J2SReturnYield ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SReturnYield env::pair-nil fun fix::cell)
   (with-access::J2SReturnYield this (expr)
      (multiple-value-bind (etype eenv ebreak)
	 (type expr env fun fix)
	 (values 'void eenv #t))))
   
;*---------------------------------------------------------------------*/
;*    type ::J2SIf ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SIf env::pair-nil fun fix)
   (with-access::J2SIf this (test then else)
      (multiple-value-bind (itype ienv _)
	 (type test env fun fix)
	 (let ((thenenv ienv)
	       (elseenv ienv))
	    (multiple-value-bind (tyop tydecl tytype tyexpr)
	       (j2s-expr-type-test test)
	       (case tyop
		  ((==) (set! thenenv (env-extend tydecl tytype ienv)))
		  ((!=) (set! elseenv (env-extend tydecl tytype ienv))))
	       (multiple-value-bind (ttype tenv tbreak)
		  (type then thenenv fun fix)
		  (multiple-value-bind (etype eenv ebreak)
		     (type else elseenv fun fix)
		     (let ((cenv (env-merge tenv eenv))
			   (cbreak (and tbreak ebreak)))
			(when (eq? tyop '!=)
			   ;; special optimization if( typeof X != type ) X = type
			   (when (eq? (env-type tydecl cenv) tytype)
			      (set! cenv (env-extend tydecl tytype cenv))))
			(values 'void cenv cbreak)))))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SSwitch ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SSwitch env::pair-nil fun fix::cell)
   (with-access::J2SSwitch this (key cases)
      (multiple-value-bind (ktype kenv _)
	 (type key env fun fix)
	 (let ((break #t))
	    (for-each (lambda (case)
			 (multiple-value-bind (ctype cenv cbreak)
			    (type case env fun fix)
			    (set! break (and cbreak break))
			    (set! kenv (env-merge kenv cenv))))
	       cases)
	    (values 'void kenv break)))))

;*---------------------------------------------------------------------*/
;*    type ::J2SCase ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SCase env::pair-nil fun fix::cell)
   (with-access::J2SCase this (expr body cascade)
      (multiple-value-bind (etype eenv)
	 (type expr env fun fix)
	 (type body eenv fun fix))))

;*---------------------------------------------------------------------*/
;*    type ::J2STry ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2STry env::pair-nil fun fix::cell)
   (with-access::J2STry this (body catch finally)
      (type catch env fun fix)
      (multiple-value-bind (_ env __)
	 (type body env fun fix)
	 (type finally env fun fix))))

;*---------------------------------------------------------------------*/
;*    type ::J2SCatch ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SCatch env::pair-nil fun fix::cell)
   (with-access::J2SCatch this (body)
      (type body env fun fix)))

;*---------------------------------------------------------------------*/
;*    type ::J2SLoop ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SLoop env::pair-nil fun fix::cell)
   (with-access::J2SLoop this (body)
      (let loop ((env env))
	 (let ((ofix (cell-ref fix)))
	    (cell-set! fix #f)
	    (multiple-value-bind (ltype lenv lbreak)
	       (type body env fun fix)
	       (if (cell-ref fix)
		   (loop lenv)
		   (begin
		      (cell-set! fix ofix)
		      (values 'void lenv lbreak))))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SWhile ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SWhile env::pair-nil fun fix::cell)
   (with-access::J2SWhile this (test body)
      (let loop ((env env))
	 (let ((ofix (cell-ref fix)))
	    (cell-set! fix #f)
	    (multiple-value-bind (_ env break)
	       (type test env fun fix)
	       (multiple-value-bind (ltype lenv lbreak)
		  (type body env fun fix)
		  (if (cell-ref fix)
		      (loop lenv)
		      (begin
			 (cell-set! fix ofix)
			 (values 'void lenv lbreak)))))))))
	 
;*---------------------------------------------------------------------*/
;*    type ::J2SFor ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SFor env::pair-nil fun fix::cell)
   (with-access::J2SFor this (init test incr body)
      (multiple-value-bind (_ env break)
	 (type init env fun fix)
	 (let loop ((env env)
		    (break break))
	    (let ((ofix (cell-ref fix)))
	       (cell-set! fix #f)
	       (multiple-value-bind (_ tenv tbreak)
		  (type test env fun fix)
		  (multiple-value-bind (_ lenv lbreak)
		     (type body tenv fun fix)
		     (multiple-value-bind (_ ienv ibreak)
			(type incr lenv fun fix)
			(let ((break (or break tbreak lbreak ibreak)))
			   (if (cell-ref fix)
			       (loop lenv break)
			       (begin
				  (cell-set! fix ofix)
				  (values 'void lenv break))))))))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SStmtExpr ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SStmtExpr env::pair-nil fun fix::cell)
   (with-access::J2SStmtExpr this (expr)
      (multiple-value-bind (_ env __)
	 (type expr env fun fix)
	 (values 'void env #f))))

;*---------------------------------------------------------------------*/
;*    type ::J2SNop ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SNop env::pair-nil fun fix::cell)
   (values 'void env #f))

;*---------------------------------------------------------------------*/
;*    type ::J2SThrow ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SThrow env::pair-nil fun fix::cell)
   (with-access::J2SThrow this (expr)
      (multiple-value-bind (_ env __)
	 (type expr env fun fix)
	 (values #f env #t))))

;*---------------------------------------------------------------------*/
;*    type ::J2SLabel ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type this::J2SLabel env::pair-nil fun fix::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    type-reduce! ::J2SNode ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-reduce! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    type-reduce! ::J2SIf ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-reduce! this::J2SIf)
   
   (define (reduce tyref tytype then else)
      (with-access::J2SRef tyref (type)
	 (cond
	    ((eq? type tytype) then)
	    ((and type (not (eq? type 'obj))) else)
	    (else this))))
   
   (with-access::J2SIf this (test then else)
      (multiple-value-bind (tyop tydecl tytype tyexpr)
	 (j2s-expr-type-test test)
	 (case tyop
	    ((==)
	     (reduce tyexpr tytype then else))
	    ((!=)
	     (reduce tyexpr tytype else then))
	    (else
	     this)))))
