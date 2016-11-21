;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/typing.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 16 06:12:13 2016                          */
;*    Last change :  Mon Nov 21 09:53:09 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    js2scheme type inference                                         */
;*    -------------------------------------------------------------    */
;*    This pass does not assume any type decoration in the AST, not    */
;*    even for literals and constants.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_typing

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint)

   (export j2s-typing-stage))

;*---------------------------------------------------------------------*/
;*    j2s-typing-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-typing-stage
   (instantiate::J2SStageProc
      (name "typing")
      (comment "Dataflow type inference")
      (proc j2s-typing!)))

;*---------------------------------------------------------------------*/
;*    j2s-typing! ::obj ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-typing! this args)
   (when (isa? this J2SProgram)
      (j2s-type-program! this args)
      this))

;*---------------------------------------------------------------------*/
;*    j2s-type-program! ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-type-program! this::J2SProgram args)
   (with-access::J2SProgram this (headers decls nodes)
      (when (>=fx (bigloo-debug) 4) (display " " (current-error-port)))
      (let ((fix (make-cell 0)))
	 (let loop ((i 0))
	    (let ((ofix (cell-ref fix)))
	       ;; type all the nodes
	       (typing-seq (append headers decls nodes) '() #f fix)
	       (when (>=fx (config-get args :optim 0) 2)
		  ;; type check resolution
		  (j2s-resolve! this args fix)
		  (when (j2s-hint! this args)
		     (unfix! fix "j2s-hint")))
	       (if (=fx (cell-ref fix) ofix)
		   (when (>=fx (bigloo-debug) 4)
		      (fprintf (current-error-port) "~a." i)
		      (flush-output-port (current-error-port)))
		   (loop (+fx i 1)))))))
   this)

;*---------------------------------------------------------------------*/
;*    dump-env ...                                                     */
;*---------------------------------------------------------------------*/
(define (dump-env env)
   (map (lambda (e)
	   (cons (with-access::J2SDecl (car e) (id key)
		    (format "~a:~a" id key))
	      (cdr e)))
      env))

;*---------------------------------------------------------------------*/
;*    type-integer? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (type-integer? type::symbol)
   (or (eq? type 'integer) (eq? type 'index)))
   
;*---------------------------------------------------------------------*/
;*    unfix! ...                                                       */
;*---------------------------------------------------------------------*/
(define (unfix! fix reason)
   ;; (tprint "--- UNFIX reason=" reason)
   (cell-set! fix (+fx 1 (cell-ref fix))))

;*---------------------------------------------------------------------*/
;*    return ...                                                       */
;*---------------------------------------------------------------------*/
(define (return ty::symbol env::pair-nil bk::pair-nil)
   (values ty env bk))

;*---------------------------------------------------------------------*/
;*    builtin-method-type ...                                          */
;*---------------------------------------------------------------------*/
(define (builtin-method-type name methods)
   (let ((c (assoc name methods)))
      (if (pair? c) (cdr c) 'any)))

;*---------------------------------------------------------------------*/
;*    string-method-type ...                                           */
;*---------------------------------------------------------------------*/
(define (string-method-type name)
   (builtin-method-type name
      '(("indexOf" . integer)
	("lastIndexOf" . integer)
	("charCodeAt" . number)
	("charAt" . string)
	("substring" . string)
	("substr" . string)
	("toLowerCase" . string)
	("toUpperCase" . string)
	("split" . array)
	("replace" . string)
	("naturalCompare" . integer)
	("localeCompare" . integer)
	("trim" . string))))

;*---------------------------------------------------------------------*/
;*    regexp-method-type ...                                           */
;*---------------------------------------------------------------------*/
(define (regexp-method-type name)
   (builtin-method-type name
      '(("test" . bool))))

;*---------------------------------------------------------------------*/
;*    number-method-type ...                                           */
;*---------------------------------------------------------------------*/
(define (number-method-type name)
   (builtin-method-type name
      '(("isInteger" . bool)
	("toString" . string))))

;*---------------------------------------------------------------------*/
;*    array-method-type ...                                            */
;*---------------------------------------------------------------------*/
(define (array-method-type name)
   (builtin-method-type name
      '(("indexOf" . integer)
	("lastIndexOf" . integer))))

;*---------------------------------------------------------------------*/
;*    expr-type-set! ...                                               */
;*    -------------------------------------------------------------    */
;*    Set the expression type and if needed update the fix stamp.      */
;*---------------------------------------------------------------------*/
(define (expr-type-set! this::J2SExpr env::pair-nil fix::cell ty::symbol
	   #!optional (bk '()))
   [assert (ty) (memq ty '(unknown any scm cmap void object global function
			   number integer index u8 string scmstring regexp
			   undefined null bool date array u8array tilde))]
   (with-access::J2SExpr this (type)
      (unless (or (eq? ty 'unknown) (eq? type ty) (eq? type 'any))
	 (let ((ntype (merge-types type ty)))
	    (unless (eq? ntype type)
	       (unfix! fix
		  (format "expr-type-set! ~a ~a/~a -> ~a" (j2s->list this) ty type
		     (merge-types type ty)))
	       (set! type ntype))))
      (return type env bk)))

;*---------------------------------------------------------------------*/
;*    merge-types ...                                                  */
;*---------------------------------------------------------------------*/
(define (merge-types left::symbol right::symbol)
   (cond
      ((eq? left right) left)
      ((eq? left 'unknown) right)
      ((and (type-integer? left) (type-integer? right)) 'integer)
      ((and (type-integer? left) (eq? right 'number)) 'number)
      ((and (eq? left 'number) (type-integer? right)) 'number)
      ((and (eq? left 'number) (eq? right 'index)) 'number)
      (else 'any)))

;*---------------------------------------------------------------------*/
;*    typnum? ...                                                      */
;*---------------------------------------------------------------------*/
(define (typnum? typ)
   (memq typ '(integer number)))

;*---------------------------------------------------------------------*/
;*    extend-env ...                                                   */
;*---------------------------------------------------------------------*/
(define (extend-env::pair-nil env::pair-nil decl::J2SDecl ty::symbol)
   (if (eq? ty 'unknown)
       env
       (cons (cons decl ty) env)))

;*---------------------------------------------------------------------*/
;*    env-lookup ...                                                   */
;*---------------------------------------------------------------------*/
(define (env-lookup::symbol env::pair-nil decl::J2SDecl)
   (let ((c (assq decl env)))
      (if (pair? c)
	  (cdr c)
	  'unknown)))

;*---------------------------------------------------------------------*/
;*    env-merge ...                                                    */
;*---------------------------------------------------------------------*/
(define (env-merge::pair-nil left::pair-nil right::pair-nil)
   
   (define (merge2 env1 env2)
      (filter-map (lambda (entry)
		     (let* ((decl (car entry))
			    (typl (cdr entry))
			    (typf (env-lookup env2 decl))
			    (typm (merge-types typl typf)))
			(unless (eq? typm 'unknown)
			   (cons decl typm))))
	 env1))
   
   (merge2 right (merge2 left right)))

(define (env-merge/debug::pair-nil left::pair-nil right::pair-nil)
   
   (define (merge2 env1 env2)
      (filter-map (lambda (entry)
		     (let* ((decl (car entry))
			    (typl (cdr entry))
			    (typf (env-lookup env2 decl))
			    (typm (merge-types typl typf)))
			(unless (eq? typm 'unknown)
			   (cons decl typm))))
	 env1))

   (tprint "left =" (dump-env left))
   (tprint "right=" (dump-env right))
   (tprint "merge=" (dump-env (merge2 right (merge2 left right))))
   
   (merge2 right (merge2 left right)))

;*---------------------------------------------------------------------*/
;*    typing* ...                                                      */
;*---------------------------------------------------------------------*/
(define (typing* nodes::pair-nil env::pair-nil fun fix::cell)
   (let loop ((nodes nodes)
	      (typ 'undefined)
	      (env env)
	      (bks '()))
      (if (null? nodes)
	  (return typ env bks)
	  (multiple-value-bind (typ env bk)
	     (typing (car nodes) env fun fix)
	     (loop (cdr nodes) typ env (append bk bks))))))

;*---------------------------------------------------------------------*/
;*    typing-args ...                                                  */
;*    -------------------------------------------------------------    */
;*    Types all the arguments and returns a new environment. The       */
;*    arguments are typed a as left to right sequence.                 */
;*---------------------------------------------------------------------*/
(define (typing-args::pair-nil args env::pair-nil fun fix::cell)
   (let loop ((args args)
	      (env env)
	      (bks '()))
      (if (null? args)
	  (values env bks)
	  (multiple-value-bind (_ enva bk)
	     (typing (car args) env fun fix)
	     (loop (cdr args) enva (append bk bks))))))

;*---------------------------------------------------------------------*/
;*    typing-seq ...                                                   */
;*---------------------------------------------------------------------*/
(define (typing-seq nodes::pair-nil env::pair-nil fun fix::cell)
   (let loop ((nodes nodes)
	      (env env)
	      (bks '())
	      (envr #f))
      (if (null? nodes)
	  (return 'void (or envr env) bks)
	  (multiple-value-bind (_ envn bk)
	     (typing (car nodes) env fun fix)
	     (if (pair? bk)
		 (loop (cdr nodes) envn (append bk bks) (or envr env))
		 (loop (cdr nodes) envn (append bk bks) envr))))))

;*---------------------------------------------------------------------*/
;*    filter-breaks ...                                                */
;*---------------------------------------------------------------------*/
(define (filter-breaks bks::pair-nil node)
   (filter (lambda (b::J2SStmt)
	      (or (isa? b J2SReturn)
		  (isa? b J2SThrow)
		  (with-access::J2SBreak b (target)
		     (not (eq? target node)))))
      bks))

;*---------------------------------------------------------------------*/
;*    typing ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SNode env::pair-nil fun fix::cell)
    ;; conservative guard
   (tprint "*** typing node: " (typeof this))
   (return 'any '() '()))

;*---------------------------------------------------------------------*/
;*    typing ::J2SExpr ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SExpr env::pair-nil fun fix::cell)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    typing ::J2SThis ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SThis env::pair-nil fun fix::cell)
   (expr-type-set! this env fix 'any))

;*---------------------------------------------------------------------*/
;*    typing ::J2SNull ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SNull env::pair-nil fun fix::cell)
   (expr-type-set! this env fix 'null))

;*---------------------------------------------------------------------*/
;*    typing ::J2SUndefined ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SUndefined env::pair-nil fun fix::cell)
   (expr-type-set! this env fix 'undefined))

;*---------------------------------------------------------------------*/
;*    typing ::J2SArrayAbsent ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SArrayAbsent env::pair-nil fun fix::cell)
   (expr-type-set! this env fix 'undefined))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SNumber ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SNumber env::pair-nil fun fix::cell)
   (with-access::J2SNumber this (val type)
      (expr-type-set! this env fix
	 (cond
	    ((not (fixnum? val)) 'number)
	    ((<fx val (bit-lsh 1 28)) 'integer)
	    (else 'integer)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SBool ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SBool env::pair-nil fun fix::cell)
   (expr-type-set! this env fix 'bool))

;*---------------------------------------------------------------------*/
;*    typing ::J2SString ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SString env::pair-nil fun fix::cell)
   (expr-type-set! this env fix 'string))

;*---------------------------------------------------------------------*/
;*    typing ::J2SNativeString ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SNativeString env::pair-nil fun fix::cell)
   (expr-type-set! this env fix 'scmstring))

;*---------------------------------------------------------------------*/
;*    typing ::J2SRegExp ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SRegExp env::pair-nil fun fix::cell)
   (expr-type-set! this env fix 'regexp))

;*---------------------------------------------------------------------*/
;*    typing ::J2SCmap ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCmap env::pair-nil fun fix::cell)
   (expr-type-set! this env fix 'cmap))

;*---------------------------------------------------------------------*/
;*    typing ::J2STemplate ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2STemplate env::pair-nil fun fix::cell)
   (with-access::J2STemplate this (exprs)
      (multiple-value-bind (env bk)
	 (typing-args exprs env fun fix)
	 (expr-type-set! this env fix 'string bk))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SArray ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SArray env::pair-nil fun fix::cell)
   (with-access::J2SArray this (exprs len)
      (multiple-value-bind (env bk)
	 (typing-args exprs env fun fix)
	 (expr-type-set! this env fix 'array bk))))

;*---------------------------------------------------------------------*/
;*    typing ::J2STilde ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2STilde env::pair-nil fun fix::cell)
   (expr-type-set! this env fix 'tilde))

;*---------------------------------------------------------------------*/
;*    typing ::J2SHopRef ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SHopRef env::pair-nil fun fix::cell)
   (with-access::J2SHopRef this (itype)
      (expr-type-set! this env fix itype)))

;*---------------------------------------------------------------------*/
;*    typing ::J2SUnresolvedRef ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SUnresolvedRef env::pair-nil fun fix::cell)
   (with-access::J2SUnresolvedRef this (id)
      (if (memq id '(console))
	  (expr-type-set! this env fix 'object)
	  (expr-type-set! this env fix 'any))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SRef ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SRef env::pair-nil fun fix::cell)
   
   (define (escape val::J2SFun)
      (with-access::J2SFun val (params)
	 (for-each (lambda (p::J2SDecl)
		      (with-access::J2SDecl p (itype id)
			 (unless (eq? itype 'any)
			    (set! itype 'any)
			    (unfix! fix "escape"))))
	    params)))
      
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (itype ronly id key)
	 (cond
	    ((isa? decl J2SDeclFun)
	     (with-access::J2SDeclFun decl (val) (escape val)))
	    ((isa? decl J2SDeclFunCnst)
	     (with-access::J2SDeclFunCnst decl (val) (escape val))))
	 (let ((etyp (env-lookup env decl)))
	    (when (eq? etyp 'unknown)
	       (when (or (isa? decl J2SDeclFunCnst)
			 (and ronly (isa? decl J2SDeclFun)))
		  (set! etyp 'function)))
	    (expr-type-set! this env fix etyp)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SSequence ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SSequence env::pair-nil fun fix::cell)
   (with-access::J2SSequence this (exprs)
      (typing* exprs env fun fix)))
      
;*---------------------------------------------------------------------*/
;*    typing ::J2SLiteralCnst ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SLiteralCnst env::pair-nil fun fix::cell)
   (with-access::J2SLiteralCnst this (val)
      (multiple-value-bind (tyv env bk)
	 (typing val env fun fix)
	 (expr-type-set! this env fix tyv bk))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SAssig ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SAssig env::pair-nil fun fix::cell)
   (with-access::J2SAssig this (lhs rhs)
      (multiple-value-bind (_ __ lbk)
	 (typing lhs env fun fix)
	 (cond
	    ((isa? lhs J2SRef)
	     ;; a variable assignment
	     (multiple-value-bind (tyr env rbk)
		(typing rhs env fun fix)
		(with-access::J2SRef lhs (decl)
		   (let ((nenv (extend-env env decl tyr)))
		      (expr-type-set! this nenv fix tyr (append lbk rbk))))))
	    (else
	     ;; a non variable assinment
	     (multiple-value-bind (tyr nenv rbk)
		(typing rhs env fun fix)
		(expr-type-set! this nenv fix tyr (append lbk rbk))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SPostfix ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SPostfix env::pair-nil fun fix::cell)
   (with-access::J2SPostfix this (lhs rhs)
      (multiple-value-bind (tyl envl bkl)
	 (typing lhs env fun fix)
	 (call-next-method)
	 (expr-type-set! this envl fix tyl bkl))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SPrefix ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SPrefix env::pair-nil fun fix::cell)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    typing ::J2SCond ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCond env::pair-nil fun fix::cell)
   (with-access::J2SCond this (test then else)
      (multiple-value-bind (tyi env bki)
	 (typing test env fun fix)
	 (multiple-value-bind (tyt envt bkt)
	    (typing then env fun fix)
	    (multiple-value-bind (tye enve bke)
	       (typing else env fun fix)
	       (let ((envc (env-merge envt enve))
		     (typc (merge-types tyt tye))
		     (bk (append bki bkt bke)))
		  (expr-type-set! this envc fix typc bk)))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SFun env::pair-nil fun fix::cell)
   (with-access::J2SFun this (body params rtype)
      (let ((envp (filter-map (lambda (p)
				 (with-access::J2SDecl p (itype utype usage)
				    (cond
				       ((eq? usage 'rest)
					(cons p 'array))
				       ((not (eq? itype 'unknown))
					(cons p itype))
				       ((not (eq? utype 'unknown))
					(cons p utype)))))
		     params)))
	 (multiple-value-bind (tyr env)
	    (typing body envp this fix)
	    (set! rtype (merge-types rtype tyr)))
	 (expr-type-set! this env fix 'function))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SCall ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCall env::pair-nil fun fix::cell)

   (define (type-fun callee args env bk)
      (with-access::J2SFun callee (rtype params)
	 (let loop ((params params)
		    (args args))
	    (when (pair? params)
	       (with-access::J2SDecl (car params) (itype)
		  (if (null? args)
		      (begin
			 (set! itype (merge-types itype 'undefined))
			 (loop (cdr params) '()))
		      (begin
			 (set! itype (merge-types itype (j2s-type (car args))))
			 (loop (cdr params) (cdr args)))))))
	 (expr-type-set! this env fix rtype bk)))

   (define (type-default callee env bk)
      (multiple-value-bind (_ env bk)
	 (typing callee env fun fix)
	 (expr-type-set! this env fix 'unknown bk)))

   (with-access::J2SCall this ((callee fun) args)
      (multiple-value-bind (env bk)
	 (typing-args args env fun fix)
	 (cond
	    ((isa? callee J2SFun)
	     (typing callee env fun fix)
	     (type-fun callee args env bk))
	    ((isa? callee J2SRef)
	     (with-access::J2SRef callee (decl)
		(cond
		   ((isa? decl J2SDeclFunCnst)
		    ;;(typing callee env fun fix)
		    (with-access::J2SDeclFunCnst decl (val)
		       (type-fun val args env bk)))
		   ((isa? decl J2SDeclFun)
		    ;;(typing callee env fun fix)
		    (with-access::J2SDeclFun decl (ronly val)
		       (if ronly
			   (type-fun val args env bk)
			   (begin
			      (typing callee env fun fix)
			      (values 'any env bk)))))
		   (else
		    (type-default callee env bk))))) 
	    ((isa? callee J2SHopRef)
	     (with-access::J2SHopRef callee (rtype)
		(expr-type-set! this env fix rtype bk)))
	    ((isa? callee J2SAccess)
	     (multiple-value-bind (_ env bk)
		(typing callee env fun fix)
		(with-access::J2SAccess callee (obj field)
		   (let* ((fn (j2s-field-name field))
			  (ty (if (string? fn)
				  (case (j2s-type obj)
				     ((string)
				      (string-method-type fn))
				     ((regexp)
				      (regexp-method-type fn))
				     ((number integer index)
				      (number-method-type fn))
				     ((array)
				      (array-method-type fn))
				     (else
				      'unknown))
				  'unknown)))
		      (expr-type-set! this env fix ty bk)))))
	    (else
	     (type-default callee env bk))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SNew ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SNew env::pair-nil fun fix::cell)

   (define (class-type clazz)
      (if (isa? clazz J2SUnresolvedRef)
	  (with-access::J2SUnresolvedRef clazz (id)
	     (case id
		((Array) 'array)
		((Date) 'date)
		((RegExp) 'regexp)
		(else 'object)))
	  'object))

   (with-access::J2SNew this (clazz args)
      (multiple-value-bind (_ env bk)
	 (typing clazz env fun fix)
	 (multiple-value-bind (env bka)
	    (typing-args args env fun fix)
	    (expr-type-set! this env fix (class-type clazz) (append bk bka))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SObjInit ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SObjInit env::pair-nil fun fix::cell)
   (with-access::J2SObjInit this (inits)
      (let ((args (filter-map (lambda (init)
				 (when (isa? init J2SDataPropertyInit)
				    (with-access::J2SDataPropertyInit init (val)
				       val)))
		     inits)))
	 (multiple-value-bind (env bk)
	    (typing-args args env fun fix)
	    (expr-type-set! this env fix 'object bk)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SUnary ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SUnary env::pair-nil fun fix::cell)
   (with-access::J2SUnary this (op expr)
      (multiple-value-bind (ty env bk)
	 (typing expr env fun fix)
	 (let ((tye (case op
		       ((+) (if (type-integer? ty) 'integer 'number))
		       ((~ -) (if (type-integer? ty) 'integer 'number))
		       ((!) 'bool)
		       (else 'any))))
	    (expr-type-set! this env fix tye bk)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SBinary ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SBinary env::pair-nil fun fix::cell)
   (with-access::J2SBinary this (op lhs rhs)
      (multiple-value-bind (typl envl bkl)
	 (typing lhs env fun fix)
	 (multiple-value-bind (typr envr bkr)
	    (typing rhs envl fun fix)
	    (let ((typ (case op
			  ((+)
			   (cond
			      ((and (type-integer? typl) (type-integer? typr))
			       'integer)
			      ((and (typnum? typl) (typnum? typr))
			       'number)
			      ((or (eq? typl 'string) (eq? typr 'string))
			       'string)
			      ((or (eq? typl 'unknown) (eq? typr 'unknown))
			       'unknown)
			      (else
			       'any)))
			  ((- * /)
			   (cond
			      ((and (type-integer? typl) (type-integer? typr))
			       'integer)
			      ((or (eq? typl 'unknown) (eq? typr 'unknown))
			       'unknown)
			      (else
			       'number)))
			  ((%)
			   (cond
			      ((and (type-integer? typl) (type-integer? typr))
			       'number)
			      ((or (eq? typl 'unknown) (eq? typr 'unknown))
			       'unknown)
			      (else
			       'number)))
			  ((== === != !== < <= > >= instanceof)
			   'bool)
			  ((&& OR)
			   (merge-types typr typl))
			  ((<< >> >>> ^ & BIT_OR)
			   (cond
			      ((and (type-integer? typl) (type-integer? typr))
			       'integer)
			      ((or (eq? typl 'unknown) (eq? typr 'unknown))
			       'unknown)
			      (else
			       'number)))
			  (else
			   'any))))
	       (expr-type-set! this env fix typ (append bkl bkr)))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SAccess ...                                           */
;*---------------------------------------------------------------------*/
(define-method (typing this::J2SAccess env::pair-nil fun fix::cell)

   (define (is-number-ref? expr::J2SNode)
      (when (isa? expr J2SUnresolvedRef)
	 (with-access::J2SUnresolvedRef expr (id)
	    (eq? id 'Number))))
   
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 (raise e))
      (with-access::J2SAccess this (obj field)
	 (multiple-value-bind (tyo envo bko)
	    (typing obj env fun fix)
	    (multiple-value-bind (tyf envf bkf)
	       (typing field envo fun fix)
	       (cond
		  ((and (memq tyo '(array string)) (j2s-field-length? field))
		   (expr-type-set! this envf fix 'integer (append bko bkf)))
		  ((eq? tyo 'string)
		   (let* ((fn (j2s-field-name field))
			  (ty (if (eq? (string-method-type fn) 'any)
				  'any 'function)))
		      (expr-type-set! this envf fix ty (append bko bkf))))
		  ((eq? tyo 'regexp)
		   (let* ((fn (j2s-field-name field))
			  (ty (if (eq? (regexp-method-type fn) 'any)
				  'any 'function)))
		      (expr-type-set! this envf fix ty (append bko bkf))))
		  ((eq? tyo 'number)
		   (let* ((fn (j2s-field-name field))
			  (ty (if (eq? (number-method-type fn) 'any)
				  'any 'function)))
		      (expr-type-set! this envf fix ty (append bko bkf))))
		  ((eq? tyo 'array)
		   (let* ((fn (j2s-field-name field))
			  (ty (if (eq? (array-method-type fn) 'any)
				  'any 'function)))
		      (expr-type-set! this envf fix ty (append bko bkf))))
		  ((is-number-ref? obj)
		   (let ((name (j2s-field-name field)))
		      (if (member name
			     '("POSITIVE_INFINITY" "NEGATIVE_INFINITY"))
			  (expr-type-set! this envf fix 'number (append bko bkf))
			  (expr-type-set! this envf fix 'any (append bko bkf)))))
		  ((eq? tyo 'unknown)
		   (expr-type-set! this envf fix 'unknown (append bko bkf)))
		  (else
		   (expr-type-set! this envf fix 'any (append bko bkf)))))))))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SPragma ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SPragma env::pair-nil fun fix::cell)
   (with-access::J2SPragma this (lang expr type)
      (if (eq? lang 'scheme)
	  (return type env '())
	  (multiple-value-bind (_ _ bk)
	     (typing expr env fun fix)
	     (return type env '())))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SParen ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SParen env::pair-nil fun fix::cell)
   (with-access::J2SParen this (expr)
      (typing expr env fun fix)))

;*---------------------------------------------------------------------*/
;*    typing ::J2SComprehension ...                                    */
;*---------------------------------------------------------------------*/
(define-method (typing this::J2SComprehension env::pair-nil fun fix::cell)
   (with-access::J2SComprehension this (decls iterables test expr)
      (multiple-value-bind (_ envi bki)
	 (typing-seq iterables env fun fix)
	 (let loop ((env envi)
		    (i 0))
	    (let ((ofix (cell-ref fix)))
	       (multiple-value-bind (_ envb bk)
		  (typing-seq (list test expr) env fun fix)
		  (if (=fx ofix (cell-ref fix))
		      (return 'array envb (filter-breaks (append bk bki) this))
		      (loop (env-merge env envb) (+fx i 1)))))))))
		  
;*---------------------------------------------------------------------*/
;*    typing ::J2SNop ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SNop env::pair-nil fun fix::cell)
   (return 'void env '()))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDecl ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDecl env::pair-nil fun fix::cell)
   (return 'void env '()))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDeclInit env::pair-nil fun fix::cell)
   (with-access::J2SDeclInit this (val ronly itype)
      (multiple-value-bind (typ env bk)
	 (typing val env fun fix)
	 (cond
	    ((eq? itype 'unknown)
	     (cond
		((eq? typ 'unknown)
		 (return 'void env bk))
		((eq? typ itype)
		 (return 'void env bk))
		((eq? 'any itype)
		 (return 'void env bk))
		(else
		 (unfix! fix "J2SDeclInit")
		 (when (eq? itype 'unknown) (set! itype typ))
		 (return 'void (extend-env env this typ) bk))))
	    ((eq? typ 'unknown)
	     (return 'void env bk))
	    (else
	     (return 'void (extend-env env this typ) bk))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDeclFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDeclFun env::pair-nil fun fix::cell)
   (with-access::J2SDeclFun this (val itype)
      (unless (eq? itype 'function)
	 (unfix! fix "J2SDeclFun")
	 (set! itype 'function))
      (multiple-value-bind (tyf env _)
	 (typing val env fun fix)
	 (return 'void (extend-env env this tyf) '()))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SStmtExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SStmtExpr env::pair-nil fun fix::cell)
   (with-access::J2SStmtExpr this (expr)
      (multiple-value-bind (typ env bk)
	 (typing expr env fun fix)
	 (return 'void env bk))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SSeq ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SSeq env::pair-nil fun fix::cell)
   (with-access::J2SSeq this (nodes)
      (typing-seq nodes env fun fix)))

;*---------------------------------------------------------------------*/
;*    typing ::J2SLabel ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SLabel env::pair-nil fun fix::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    typing ::J2SLetBlock ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SLetBlock env::pair-nil fun fix::cell)
   (with-access::J2SLetBlock this (decls nodes)
      (multiple-value-bind (_ denv bk)
	 (typing* decls env fun fix)
	 (multiple-value-bind (_ benv bks)
	    (typing-seq nodes denv fun fix)
	    (let ((nenv (filter (lambda (d) (not (memq (car d) decls))) benv)))
	       (return 'void nenv (append bk bks)))))))
	       
;*---------------------------------------------------------------------*/
;*    typing ::J2SReturn ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SReturn env::pair-nil fun fix::cell)
   (with-access::J2SReturn this (expr)
      (multiple-value-bind (tye enve bke)
	 (typing expr env fun fix)
	 (if fun
	     (with-access::J2SFun fun (rtype)
		(let ((tyr (merge-types rtype tye)))
		   (unless (eq? tyr rtype)
		      (unfix! fix (format "J2SReturn(~a) ~a/~a" tye tyr rtype))
		      (set! rtype tyr)))
		(values 'void enve (list this)))
	     (values 'void enve (list this))))))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SReturnYield ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SReturnYield env::pair-nil fun fix::cell)
   (with-access::J2SReturnYield this (expr)
      (multiple-value-bind (tye enve bke)
	 (typing expr env fun fix)
	 (values 'void enve (list this)))))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SIf ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SIf env::pair-nil fun fix::cell)
   (with-access::J2SIf this (test then else)
      (multiple-value-bind (tyi env bki)
	 (typing test env fun fix)
	 (multiple-value-bind (op decl typ)
	    (j2s-expr-type-test test)
	    (let ((envt env)
		  (enve env))
	       (case op
		  ((==) (set! envt (extend-env env decl typ)))
		  ((!=) (set! enve (extend-env env decl typ))))
	       (multiple-value-bind (tyt envt bkt)
		  (typing then envt fun fix)
		  (multiple-value-bind (tye enve bke)
		     (typing else enve fun fix)
		     (let ((bk (append bki bke bkt)))
			(return 'void (env-merge envt enve) bk)))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SSwitch ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SSwitch env::pair-nil fun fix::cell)
   (with-access::J2SSwitch this (key cases)
      (multiple-value-bind (typk envk bk)
	 (typing key env fun fix)
	 (let ((bks '()))
	    (for-each (lambda (cs)
			 (multiple-value-bind (t e b)
			    (typing cs '() fun fix)
			    (set! bks (append b bks))))
	       cases)
	    (return 'void '() bks)))))

;*---------------------------------------------------------------------*/
;*    type ::J2SCase ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCase env::pair-nil fun fix::cell)
   (with-access::J2SCase this (expr body cascade)
      (multiple-value-bind (typx envx bk)
	 (typing expr env fun fix)
	 (multiple-value-bind (typb envb bkb)
	    (typing body envx fun fix)
	    (return 'void envb (append bkb bk))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SBreak ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SBreak env::pair-nil fun fix::cell)
   (return 'void env (list this)))

;*---------------------------------------------------------------------*/
;*    typing ::J2SWhile ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SWhile env::pair-nil fun fix::cell)
   (with-access::J2SWhile this (test body)
      (let loop ((env env)
		 (i 0))
	 (let ((ofix (cell-ref fix)))
	    (multiple-value-bind (_ envb bk)
	       (typing-seq (list test body) env fun fix)
	       (if (=fx ofix (cell-ref fix))
		   (return 'void envb (filter-breaks bk this))
		   (loop (env-merge env envb) (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDo ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDo env::pair-nil fun fix::cell)
   (with-access::J2SDo this (test body)
      (let loop ((env env))
	 (let ((ofix (cell-ref fix)))
	    (multiple-value-bind (_ envb bk)
	       (typing-seq (list body test) env fun fix)
	       (if (=fx ofix (cell-ref fix))
		   (return 'void envb (filter-breaks bk this))
		   (loop (env-merge env envb))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SFor ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SFor env::pair-nil fun fix::cell)
   (with-access::J2SFor this (init test incr body)
      (let loop ((env env))
	 (let ((ofix (cell-ref fix)))
	    (multiple-value-bind (_ envb bk)
	       (typing-seq (list init test body incr) env fun fix)
	       (if (=fx ofix (cell-ref fix))
		   (return 'void envb (filter-breaks bk this))
		   (loop (env-merge env envb))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SForIn ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SForIn env::pair-nil fun fix::cell)
   (with-access::J2SForIn this (lhs obj body)
      (let loop ((env env))
	 (let ((ofix (cell-ref fix)))
	    (multiple-value-bind (_ envb bk)
	       (typing-seq (list obj body) env fun fix)
	       (if (=fx ofix (cell-ref fix))
		   (return 'void envb (filter-breaks bk this))
		   (loop (env-merge env envb))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2STry ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2STry env::pair-nil fun fix::cell)
   (with-access::J2STry this (body catch finally)
      (multiple-value-bind (_ __ bkb)
	 (typing body env fun fix)
	 (multiple-value-bind (_ __ bkh)
	    (typing catch env fun fix)
	    (multiple-value-bind (_ __ bkf)
	       (typing finally env fun fix)
	       (return 'void env (append bkb bkh bkf)))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SCatch ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCatch env::pair-nil fun fix::cell)
   (with-access::J2SCatch this (body)
      (typing body env fun fix)))

;*---------------------------------------------------------------------*/
;*    type ::J2SThrow ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SThrow env::pair-nil fun fix::cell)
   (with-access::J2SThrow this (expr)
      (multiple-value-bind (_ env bk)
	 (typing expr env fun fix)
	 (return 'void env (cons this bk)))))

;*---------------------------------------------------------------------*/
;*    j2s-resolve! ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-resolve! this::J2SProgram args fix)   
   (with-access::J2SProgram this (headers decls nodes)
      (set! headers (map! (lambda (o) (resolve! o fix)) headers))
      (set! decls (map! (lambda (o) (resolve! o fix)) decls))
      (set! nodes (map! (lambda (o) (resolve! o fix)) nodes))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SNode ...                                           */
;*    -------------------------------------------------------------    */
;*    Tries to resolve statically type checks using type information   */
;*    computed by the TYPING method.                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SNode fix::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SIf ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SIf fix::cell)
   (with-access::J2SIf this (test then else)
      (with-access::J2SExpr test (type)
	 (if (memq type '(any unknown))
	     (call-default-walker)
	     (multiple-value-bind (op decl typ ref)
		(j2s-expr-type-test test)
		(case op
		   ((==)
		    (with-access::J2SExpr ref (type)
		       (cond
			  ((eq? type typ)
			   (unfix! fix "resolve.J2SIf")
			   (resolve! then fix))
			  ((memq type '(unknown any))
			   (call-default-walker))
			  ((and (eq? type 'number) (memq typ '(integer index)))
			   (call-default-walker))
			  (else
			   (unfix! fix "return.J2SIf")
			   (resolve! else fix)))))
		   ((!=)
		    (with-access::J2SExpr ref (type)
		       (cond
			  ((eq? type typ)
			   (unfix! fix "return.J2SIf")
			   (resolve! else fix))
			  ((memq type '(unknown any))
			   (call-default-walker))
			  ((and (eq? type 'number) (memq typ '(integer index)))
			   (call-default-walker))
			  (else
			   (unfix! fix "return.J2SIf")
			   (resolve! then fix)))))
		   (else
		    (call-default-walker))))))))
   
