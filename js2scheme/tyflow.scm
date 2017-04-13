;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/tyflow.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 16 06:12:13 2016                          */
;*    Last change :  Thu Apr 13 09:18:43 2017 (serrano)                */
;*    Copyright   :  2016-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    js2scheme type inference                                         */
;*    -------------------------------------------------------------    */
;*    This pass does not assume any type decoration in the AST, not    */
;*    even for literals and constants.                                 */
;*    -------------------------------------------------------------    */
;*    This stage assigns type to variable declaration and variable     */
;*    references. At the a declaration site, the types are:            */
;*                                                                     */
;*      utype: the user given type                                     */
;*      vtype: the final type of the variable (used by scheme)         */
;*      itype: the initial type in the dataflow                        */
;*                                                                     */
;*    The vtype and itype are not supposed to be equal but             */
;*                                                                     */
;*      itype < vtype                                                  */
;*                                                                     */
;*    The following also holds                                         */
;*                                                                     */
;*      for in ref(v), T(ref(v)) < vtype                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_tyflow

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint
	   __js2scheme_use)

   (export j2s-tyflow-stage))

;*---------------------------------------------------------------------*/
;*    j2s-tyflow-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-tyflow-stage
   (instantiate::J2SStageProc
      (name "tyflow")
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
   
   (define j2s-verbose (config-get args :verbose 0))
   
   (with-access::J2SProgram this (headers decls nodes)
      (when (>=fx j2s-verbose 4) (display " " (current-error-port)))
      ;; main fix point
      (let ((fix (make-cell 0)))
	 (let loop ((i 1))
	    (when (>=fx j2s-verbose 4)
	       (fprintf (current-error-port) "~a." i)
	       (flush-output-port (current-error-port)))
	    (let ((ofix (cell-ref fix)))
	       ;; type all the nodes
	       (typing-seq (append headers decls nodes) '() #f fix)
	       (if (=fx (cell-ref fix) ofix)
		   (if (>=fx (config-get args :optim 0) 2)
		       ;; type check resolution
		       (begin
			  (j2s-resolve! this args fix)
			  (if (=fx (cell-ref fix) ofix)
			      (when (config-get args :optim-hint #f)
				 ;; hint typing optimization
				 (when (>=fx j2s-verbose 4)
				    (fprintf (current-error-port) "hint."))
				 (when (j2s-hint! this args)
				    (for-each reset-type! decls)
				    (for-each reset-type! nodes)
				    (loop (+fx i 1))))
			      (loop (+fx i 1)))))
		   (loop (+fx i 1))))))
      (unless (config-get args :optim-cast)
	 (unindex! this))
      ;; cleanup the ast use count and remove obviously useless definitions
      (program-cleanup! this))
   this)

;*---------------------------------------------------------------------*/
;*    program-cleanup! ...                                             */
;*---------------------------------------------------------------------*/
(define (program-cleanup! this::J2SProgram)
   (with-access::J2SProgram this (headers decls nodes direct-eval)
      (for-each reset-use-count headers)
      (for-each reset-use-count decls)
      (for-each reset-use-count nodes)
      (for-each (lambda (n) (use-count n +1)) headers)
      (for-each (lambda (n) (use-count n +1)) decls)
      (for-each (lambda (n) (use-count n +1)) nodes)
      (unless direct-eval
	 (set! decls (filter-dead-declarations decls)))
      this))
   
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
;*    unfix! ...                                                       */
;*---------------------------------------------------------------------*/
(define (unfix! fix reason)
   (tprint "--- UNFIX reason=" reason)
   (cell-set! fix (+fx 1 (cell-ref fix))))

(define-macro (unfix! fix reason)
   `(cell-set! ,fix (+fx 1 (cell-ref ,fix))))

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
	("charCodeAt" . integer)
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
;*    subtype? ...                                                     */
;*---------------------------------------------------------------------*/
(define (subtype? t1 t2)
   (or (eq? t1 t2)
       (and (eq? t1 'index) (or (eq? t2 'integer) (eq? t2 'number)))
       (and (eq? t1 'integer) (eq? t2 'number))))

;*---------------------------------------------------------------------*/
;*    decl-vtype-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (decl-vtype-set! decl::J2SDecl ty fix::cell)
   (with-access::J2SDecl decl (vtype id)
      (unless (or (not ty)
		  (eq? ty 'unknown)
		  (subtype? ty vtype)
		  (eq? vtype 'any))
	 (unfix! fix (format "J2SDecl(~a) vtype=~a/~a" id vtype ty))
	 (set! vtype (merge-types vtype ty)))))

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
      ((eq? right 'unknown) left)
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
;*    typing ::J2SMeta ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SMeta env::pair-nil fun fix::cell)
   (with-access::J2SMeta this (optim)
      (if (=fx optim 0)
	  (return 'void env '())
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SExpr ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SExpr env::pair-nil fun fix::cell)
   (call-next-method))

;* {*---------------------------------------------------------------------*} */
;* {*    typing ::J2SThis ...                                             *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (typing this::J2SThis env::pair-nil fun fix::cell) */
;*    (expr-type-set! this env fix 'any))                              */
;*                                                                     */
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
	    ((or (not (integer? val)) (flonum? val)) 'number)
	    ((and (> val 0) (< val (bit-lsh 1 28))) 'index)
	    ((= val 0) 'index)
	    ((flonum? val) 'number)
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
      (cond
	 ((memq id '(console))
	  (expr-type-set! this env fix 'object))
	 ((eq? id 'undefined)
	  (expr-type-set! this env fix 'undefined))
	 (else
	  (let ((cla (class-of this)))
	     (if (eq? cla 'unknown)
		 (return 'unknown env '())
		 (expr-type-set! this env fix 'object)))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SRef ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SRef env::pair-nil fun fix::cell)
   
   (define (escape-fun val::J2SFun)
      (with-access::J2SFun val (params)
	 (for-each (lambda (p::J2SDecl)
		      (with-access::J2SDecl p (itype vtype id)
			 (unless (eq? vtype 'any)
			    (set! vtype 'any)
			    (unfix! fix "escape"))
			 (unless (eq? itype 'any)
			    (set! itype 'any)
			    (unfix! fix "escape"))))
	    params)))
      
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (ronly id key)
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (val) (escape-fun val)))
	 (let ((etyp (env-lookup env decl)))
	    (when (eq? etyp 'unknown)
	       (when (and ronly (isa? decl J2SDeclInit))
		  (with-access::J2SDeclInit decl (vtype)
		     (set! etyp vtype))))
	    (expr-type-set! this env fix etyp)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDecl ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDecl env::pair-nil fun fix::cell)
   (return 'void env '()))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDeclInit env::pair-nil fun fix::cell)
   (with-access::J2SDeclInit this (val itype)
      (multiple-value-bind (typ env bk)
	 (typing val env fun fix)
	 (if (or (eq? typ 'unknown) (not typ))
	     (return 'void env bk)
	     (begin
		(decl-vtype-set! this typ fix)
		(set! itype (merge-types itype typ))
		(return 'void (extend-env env this typ) bk))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDeclFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDeclFun env::pair-nil fun fix::cell)

   (define (constructor-only? decl)
      (with-access::J2SDeclFun decl (usage)
	 (and (memq 'new usage)
	      (not (memq 'ref usage))
	      (not (memq 'call usage))
	      (not (memq 'eval usage)))))
   
   (with-access::J2SDeclFun this (val itype)
      (decl-vtype-set! this 'function fix)
      (when (constructor-only? this)
	 ;; a mere constructor
	 (with-access::J2SFun val (thisp)
	    (when (isa? thisp J2SDecl)
	       (with-access::J2SDecl thisp (vtype utype itype)
		  (set! vtype 'object)
		  (set! utype 'object)
		  (set! itype 'object)))))
      (multiple-value-bind (tyf env _)
	 (typing val env fun fix)
	 (return 'void (extend-env env this tyf) '()))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SAssig ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SAssig env::pair-nil fun fix::cell)
   (with-access::J2SAssig this (lhs rhs)
      (multiple-value-bind (tyv envl lbk)
	 (typing lhs env fun fix)
	 (cond
	    ;; this assignment
	    ((isa? lhs J2SRef)
	     ;; a variable assignment
	     (multiple-value-bind (tyr env rbk)
		(typing rhs envl fun fix)
		(if tyr
		    (with-access::J2SRef lhs (decl)
		       (decl-vtype-set! decl tyr fix)
		       (let ((nenv (extend-env env decl tyr)))
			  (expr-type-set! this nenv fix tyr (append lbk rbk))))
		    (return 'unknown env (append lbk rbk)))))
	    (else
	     ;; a non variable assinment
	     (multiple-value-bind (tyr nenv rbk)
		(typing rhs envl fun fix)
		(if tyr
		    (expr-type-set! this nenv fix tyr (append lbk rbk))
		    (return 'unknown env (append lbk rbk)))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SAssigOp ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SAssigOp env::pair-nil fun fix::cell)
   (with-access::J2SAssigOp this (lhs rhs op)
      (multiple-value-bind (tyr envr bkr)
	 (typing-binary op lhs rhs env fun fix)
	 (cond
	    ((isa? lhs J2SRef)
	     ;; a variable assignment
	     (with-access::J2SRef lhs (decl)
		(decl-vtype-set! decl tyr fix)
		(let ((nenv (extend-env envr decl tyr)))
		   (expr-type-set! this nenv fix tyr bkr))))
	    (else
	     ;; a non variable assinment
	     (expr-type-set! this envr fix tyr bkr))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SPostfix ...                                          */
;*    -------------------------------------------------------------    */
;*    As a fix point is involved, POSTFIX and PREFIX operations        */
;*    can be handled similarly.                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SPostfix env::pair-nil fun fix::cell)
   (with-access::J2SPostfix this (lhs rhs op)
      (multiple-value-bind (tyr envr bkr)
	 (typing rhs env fun fix)
	 (unless (type-number? tyr) (set! tyr 'number))
	 (multiple-value-bind (tyv __ lbk)
	    (typing lhs env fun fix)
	    (cond
	       ((isa? lhs J2SRef)
		;; a variable assignment
		(with-access::J2SRef lhs (decl)
		   (decl-vtype-set! decl tyr fix)
		   (let ((nenv (extend-env envr decl tyr)))
		      (expr-type-set! this nenv fix tyr (append lbk bkr)))))
	       (else
		;; a non variable assinment
		(expr-type-set! this envr fix tyr bkr)))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SPrefix ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SPrefix env::pair-nil fun fix::cell)
   (with-access::J2SPrefix this (lhs rhs op)
      (multiple-value-bind (tyr envr bkr)
	 (typing rhs env fun fix)
	 (unless (type-number? tyr) (set! tyr 'number))
	 (multiple-value-bind (tyv __ lbk)
	    (typing lhs env fun fix)
	    (cond
	       ((isa? lhs J2SRef)
		;; a variable assignment
		(with-access::J2SRef lhs (decl)
		   (decl-vtype-set! decl tyr fix)
		   (let ((nenv (extend-env envr decl tyr)))
		      (expr-type-set! this nenv fix tyr (append lbk bkr)))))
	       (else
		;; a non variable assinment
		(expr-type-set! this envr fix tyr bkr)))))))

;*---------------------------------------------------------------------*/
;*    typing-fun ...                                                   */
;*---------------------------------------------------------------------*/
(define (typing-fun this::J2SFun env::pair-nil fun fix::cell)
   (with-access::J2SFun this (body params rtype thisp)
      (let* ((envp (filter-map (lambda (p::J2SDecl)
				  (with-access::J2SDecl p (itype utype usage)
				     (cond
					((eq? usage 'rest)
					 (cons p 'array))
					((not (eq? itype 'unknown))
					 (cons p itype))
					((not (eq? utype 'unknown))
					 (cons p utype)))))
		      params))
	     (envt (if thisp
		       (with-access::J2SDecl thisp (utype)
			  (if (eq? utype 'object)
			      (extend-env envp thisp 'object)
			      envp))
		       envp)))
	 (typing body (append envt env) this fix)
	 (expr-type-set! this env fix 'function))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SFun env::pair-nil fun fix::cell)
   (with-access::J2SFun this (body params rtype)
      (let ((envc (filter-map (lambda (c)
				 (let ((d (car c))
				       (t (cdr c)))
				    (with-access::J2SDecl d (ronly vtype)
				       (if ronly c (cons d vtype)))))
		     env)))
	 (typing-fun this envc fun fix))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SCall ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCall env::pair-nil fun fix::cell)

   (define (type-call callee args env bk)
      (with-access::J2SFun callee (rtype params)
	 (let loop ((params params)
		    (args args))
	    (when (pair? params)
	       (with-access::J2SDecl (car params) (itype vtype usage)
		  (cond
		     ((and (null? (cdr params)) (memq 'rest usage))
		      (set! itype (merge-types itype 'array))
		      (set! vtype (merge-types vtype 'array)))
		     ((null? args)
		      (set! vtype (merge-types vtype 'undefined))
		      (set! itype (merge-types itype 'undefined))
		      (loop (cdr params) '()))
		     (else
		      (set! itype (merge-types itype (j2s-type (car args))))
		      (set! vtype (merge-types vtype (j2s-type (car args))))
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
	     (typing-fun callee env fun fix)
	     (type-call callee args env bk))
	    ((isa? callee J2SRef)
	     (with-access::J2SRef callee (decl)
		(cond
;* 		   ((isa? decl J2SDeclFunCnst)                         */
;* 		    (with-access::J2SDeclFunCnst decl (val)            */
;* 		       (type-call val args env bk)))                   */
		   ((isa? decl J2SDeclFun)
		    (with-access::J2SDeclFun decl (ronly val)
		       (if ronly
			   (type-call val args env bk)
			   (begin
			      (typing callee env fun fix)
			      (values 'any env bk)))))
		   ((isa? decl J2SDeclInit)
		    (with-access::J2SDeclInit decl (ronly val)
		       (if (and ronly (isa? val J2SFun))
			   (type-call val args env bk)
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
				      'any))
				  'any)))
		      (expr-type-set! this env fix ty bk)))))
	    (else
	     (type-default callee env bk))))))

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

   (define (non-zero-integer? ty expr)
      (when (and (type-integer? ty) (isa? expr J2SNumber))
	 (with-access::J2SNumber expr (val)
	    (not (= val 0)))))

   (with-access::J2SUnary this (op expr)
      (multiple-value-bind (ty env bk)
	 (typing expr env fun fix)
	 (let ((tye (case op
		       ((+) (if (non-zero-integer? ty expr) 'integer 'number))
		       ((~ -) (if (non-zero-integer? ty expr) 'integer 'number))
		       ((!) 'bool)
		       (else 'any))))
	    (expr-type-set! this env fix tye bk)))))

;*---------------------------------------------------------------------*/
;*    typing-binary ...                                                */
;*---------------------------------------------------------------------*/
(define (typing-binary op lhs::J2SExpr rhs::J2SExpr env::pair-nil fun fix::cell)
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
		       ((== === != !== < <= > >=)
			'bool)
		       ((in)
			(when (isa? lhs J2SRef)
			   (with-access::J2SRef lhs (decl)
			      (set! env (extend-env env decl 'object))))
			'bool)
		       ((instanceof)
			(when (isa? lhs J2SRef)
			   (with-access::J2SRef lhs (decl)
			      (set! env (extend-env env decl 'function))))
			'bool)
		       ((&& OR)
			(if (or (eq? typr 'unknown) (eq? typl 'unknown) )
			    'unknown
			    (merge-types typr typl)))
		       ((<< >> >>> ^ & BIT_OR)
			(cond
			   ((and (type-integer? typl) (type-integer? typr))
			    'integer)
			   ((or (eq? typl 'unknown) (eq? typr 'unknown))
			    'integer)
			   (else
			    'integer)))
		       (else
			'any))))
	    (return typ envr (append bkl bkr))))))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SBinary ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SBinary env::pair-nil fun fix::cell)
   (with-access::J2SBinary this (op lhs rhs)
      (multiple-value-bind  (typ env bk)
	 (typing-binary op lhs rhs env fun fix)
	 (expr-type-set! this env fix typ bk))))

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
		  ((isa? obj J2SThis)
		   (with-access::J2SThis obj (decl)
		      (let ((envt (extend-env env decl 'object)))
			 (expr-type-set! this envt fix 'any (append bko bkf)))))
		  ((and (memq tyo '(array string)) (j2s-field-length? field))
		   (expr-type-set! this envf fix 'index (append bko bkf)))
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
      (multiple-value-bind (tye enve bke)
	 (typing expr env fun fix)
	 (if tye
	     (expr-type-set! this enve fix tye bke)
	     (return 'unknown enve bke)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SComprehension ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SComprehension env::pair-nil fun fix::cell)
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
      (let ((ienv (map (lambda (d::J2SDecl)
			  (with-access::J2SDecl d (itype)
			     (cons d itype)))
		     decls)))
	 (multiple-value-bind (_ denv bk)
	    (typing* decls (append ienv env) fun fix)
	    (multiple-value-bind (_ benv bks)
	       (typing-seq nodes denv fun fix)
	       (let ((nenv (filter (lambda (d)
				      (not (memq (car d) decls)))
			      benv)))
		  (return 'void nenv (append bk bks))))))))
	       
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
   
   (define (isa-and? test)
      (when (isa? test J2SBinary)
	 (with-access::J2SBinary test (op)
	    (eq? op '&&))))
   
   (define (typing-one-test test envt enve)
      (multiple-value-bind (op decl typ)
	 (j2s-expr-type-test test)
	 (if (symbol? typ)
	     (case op
		((== ===) (values (extend-env envt decl typ) enve))
		((!= !==) (values envt (extend-env enve decl typ)))
		((instanceof) (values (extend-env envt decl typ) enve))
		((!instanceof) (values envt (extend-env enve decl typ)))
		(else (values envt enve)))
	     (values envt enve))))
   
   (define (typing-test test envt enve)
      (if (isa-and? test)
	  (with-access::J2SBinary test (lhs rhs)
	     (multiple-value-bind (nenvt nenve)
		(typing-test lhs envt enve)
		(typing-test rhs nenvt nenve)))
	  (typing-one-test test envt enve)))

   (with-access::J2SIf this (test then else)
      (multiple-value-bind (tyi env bki)
	 (typing test env fun fix)
	 (multiple-value-bind (envt enve)
	    (typing-test test env env)
	    (multiple-value-bind (tyt envt bkt)
	       (typing then envt fun fix)
	       (multiple-value-bind (tye enve bke)
		  (typing else enve fun fix)
		  (let ((bk (append bki bke bkt)))
		     (return 'void (env-merge envt enve) bk))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SSwitch ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SSwitch env::pair-nil fun fix::cell)
   (with-access::J2SSwitch this (key cases)
      (multiple-value-bind (typk envk bk)
	 (typing key env fun fix)
	 (let ((bks '()))
	    (let loop ((cases cases)
		       (env envk))
	       (when (pair? cases)
		  (multiple-value-bind (t e b)
		     (typing (car cases) env fun fix)
		     (set! bks (append b bks))
		     (with-access::J2SCase (car cases) (cascade)
			(if cascade
			    (loop (cdr cases) e)
			    (loop (cdr cases) envk))))))
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
;*    Resolve statically type checks using type informations           */
;*    computed by the TYPING method.                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SNode fix::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SIf ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SIf fix::cell)
   
   (define (eq-typeof? type typ)
      (or (eq? type typ)
	  (and (memq type '(date array)) (eq? typ 'object))
	  (and (eq? typ 'number) (memq type '(integer index)))
	  (and (eq? typ 'boolean) (eq? type 'bool))))
   
   (define (is-true? expr)
      (when (isa? expr J2SBinary)
	 (with-access::J2SBinary expr (lhs rhs op)
	    (and (eq? op '==)
		 (memq (j2s-type lhs) '(null undefined))
		 (memq (j2s-type rhs) '(null undefined))))))

   (define (is-false? expr)
      (when (isa? expr J2SBinary)
	 (with-access::J2SBinary expr (lhs rhs op)
	    (and (eq? op '!=)
		 (memq (j2s-type lhs) '(null undefined))
		 (memq (j2s-type rhs) '(null undefined))))))
   
   (with-access::J2SIf this (test then else)
      (with-access::J2SExpr test (type)
	 (cond
	    ((memq type '(any unknown))
	     (call-default-walker))
	    ((is-true? test)
	     (unfix! fix "resolve.J2SIf")
	     (resolve! then fix))
	    ((is-false? test)
	     (unfix! fix "resolve.J2SIf")
	     (resolve! else fix))
	    (else
	     (multiple-value-bind (op decl typ ref)
		(j2s-expr-type-test test)
		(case op
		   ((== ===)
		    (with-access::J2SExpr ref (type)
		       (cond
			  ((eq-typeof? type typ)
			   (unfix! fix "resolve.J2SIf")
			   (resolve! then fix))
			  ((memq type '(unknown any))
			   (call-default-walker))
			  ((and (eq? type 'number) (memq typ '(integer index)))
			   (call-default-walker))
			  (else
			   (unfix! fix "return.J2SIf")
			   (resolve! else fix)))))
		   ((!= !==)
		    (with-access::J2SExpr ref (type)
		       (cond
			  ((eq-typeof? type typ)
			   (unfix! fix "return.J2SIf")
			   (resolve! else fix))
			  ((memq type '(unknown any))
			   (call-default-walker))
			  ((and (eq? type 'number) (memq typ '(integer index)))
			   (call-default-walker))
			  (else
			   (unfix! fix "return.J2SIf")
			   (resolve! then fix)))))
		   ((instanceof)
		    (with-access::J2SExpr ref (type)
		       (cond
			  ((memq type '(unknown any object))
			   (call-default-walker))
			  ((eq? type typ)
			   (unfix! fix "resolve.J2SIf")
			   (resolve! then fix))
			  (else
			   (unfix! fix "return.J2SIf")
			   (resolve! else fix)))))
		   ((!instanceof)
		    (with-access::J2SExpr ref (type)
		       (cond
			  ((memq type '(unknown any object))
			   (call-default-walker))
			  ((eq? type typ)
			   (unfix! fix "resolve.J2SIf")
			   (resolve! else fix))
			  (else
			   (unfix! fix "return.J2SIf")
			   (resolve! then fix)))))
		   (else
		    (call-default-walker)))))))))

;*---------------------------------------------------------------------*/
;*    unindex! ...                                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (unindex! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unindex! ::J2SExpr ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unindex! this::J2SExpr)
   (with-access::J2SExpr this (type)
      (when (eq? type 'index) (set! type 'integer)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unindex! ::J2SFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (unindex! this::J2SFun)
   (with-access::J2SFun this (rtype params)
      (when (eq? rtype 'index) (set! rtype 'integer)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unindex! ::J2SDecl ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unindex! this::J2SDecl)
   (with-access::J2SDecl this (itype)
      (when (eq? itype 'index) (set! itype 'integer)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SRef ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SRef)
   (with-access::J2SRef this (type)
      (set! type 'unknown))
   this)

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SDecl ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SDecl)
   (call-default-walker)
   (with-access::J2SDecl this (itype vtype utype)
      (set! itype 'unknown)
      (set! vtype utype))
   this)

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SDeclFun ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SDeclFun)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SExpr)
   (with-access::J2SExpr this (type)
      (set! type 'unknown))
   (call-default-walker))

