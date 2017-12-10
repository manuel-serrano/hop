;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/tyflow.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 16 06:12:13 2016                          */
;*    Last change :  Sun Dec 10 13:46:27 2017 (serrano)                */
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

   (include "ast.sch")
   
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
      ;; mark local captured variables
      (program-capture! this)
      ;; main fix point
      (let ((fix (make-cell 0)))
	 (let loop ((i 1))
	    (when (>=fx j2s-verbose 4)
	       (fprintf (current-error-port) "~a." i)
	       (flush-output-port (current-error-port)))
	    (let ((ofix (cell-ref fix)))
	       ;; type all the nodes
	       (typing-seq (append headers decls nodes) '() fix)
	       (if (=fx (cell-ref fix) ofix)
		   (if (>=fx (config-get args :optim 0) 2)
		       ;; type check resolution
		       (begin
			  (j2s-resolve! this args fix)
			  (if (=fx (cell-ref fix) ofix)
			      (when (config-get args :optim-hint #f)
				 ;; hint typing optimization
				 (when (>=fx j2s-verbose 4)
				    (fprintf (current-error-port) "hint"))
				 (let ((dups (j2s-hint! this args)))
				    (when (pair? dups)
				       (when (>=fx j2s-verbose 3)
					  (fprintf (current-error-port)
					     (format " [~(,)]."
						(map (lambda (d)
							(with-access::J2SDecl d (id)
							   id))
						   dups))))
				       (for-each reset-type! decls)
				       (for-each reset-type! nodes)
				       (loop (+fx i 1)))))
			      (loop (+fx i 1)))))
		   (loop (+fx i 1))))))
      (unless (config-get args :optim-range)
	 (force-type! this 'integer 'number))
	 ;;(force-unary-type! this))
      ;; cleanup the ast use count and remove obviously useless definitions
      (force-type! this 'unknown 'any)
      (program-cleanup! this))
   this)

;*---------------------------------------------------------------------*/
;*    program-capture! ...                                             */
;*---------------------------------------------------------------------*/
(define (program-capture! this::J2SProgram)
   (with-access::J2SProgram this (decls nodes direct-eval)
      (unless direct-eval
	 (for-each (lambda (d) (mark-capture d '())) decls)
	 (for-each (lambda (n) (mark-capture n '())) nodes))))

;*---------------------------------------------------------------------*/
;*    mark-capture ::J2SNode ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SNode env::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-capture ::J2SRef ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SRef env::pair-nil)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (scope ronly %info)
	 (when (and (not ronly) (eq? scope 'local))
	    (unless (memq decl env)
	       (set! %info 'capture))))))

;*---------------------------------------------------------------------*/
;*    mark-capture ::J2SFun ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SFun env::pair-nil)
   (with-access::J2SFun this (params body)
      (for-each (lambda (p::J2SDecl)
		   (with-access::J2SDecl p (%info)
		      (set! %info 'nocapture)))
	 params)
      (mark-capture body params)))

;*---------------------------------------------------------------------*/
;*    mark-capture ::J2SLetBlock ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SLetBlock env::pair-nil)
   (with-access::J2SLetBlock this (decls nodes)
      (for-each (lambda (p::J2SDecl)
		   (with-access::J2SDecl p (%info)
		      (set! %info 'nocapture)))
	 decls)
      (let ((nenv (append decls env)))
	 (for-each (lambda (d) (mark-capture d nenv)) decls)
	 (for-each (lambda (n) (mark-capture n nenv)) nodes))))
   
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
	 (set! decls (filter-dead-declarations decls))
	 (for-each j2s-hint-meta-noopt! decls))
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
   (tprint "--- UNFIX (" (cell-ref fix) ") reason=" reason)
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
;*    string-static-method-type ...                                    */
;*---------------------------------------------------------------------*/
(define (string-static-method-type name)
   (builtin-method-type name
      '(("fromCharCode" . string))))
   
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
   (with-access::J2SExpr this (type)
;*       (unless (or (eq? ty 'unknown) (eq? type ty) (eq? type 'any))  */
      (unless (or (eq? ty 'unknown) (eq? type ty))
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
;*    env? ...                                                         */
;*---------------------------------------------------------------------*/
(define (env? o)
   ;; heuristic check (not very precise but should be enough)
   (or (null? o)
       (and (pair? o)
	    (isa? (caar o) J2SDecl)
	    (symbol? (cdar o)))))

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
;*    env-override ...                                                 */
;*---------------------------------------------------------------------*/
(define (env-override left::pair-nil right::pair-nil)
   (append right left))

;*---------------------------------------------------------------------*/
;*    typing-args ...                                                  */
;*    -------------------------------------------------------------    */
;*    Types all the arguments and returns a new environment. The       */
;*    arguments are typed a as left to right sequence.                 */
;*---------------------------------------------------------------------*/
(define (typing-args::pair-nil args env::pair-nil fix::cell)
   (let loop ((args args)
	      (env env)
	      (bks '()))
      (if (null? args)
	  (values env bks)
	  (multiple-value-bind (_ enva bk)
	     (typing (car args) env fix)
	     (loop (cdr args) enva (append bk bks))))))

;*---------------------------------------------------------------------*/
;*    typing* ...                                                      */
;*---------------------------------------------------------------------*/
(define (typing* nodes::pair-nil env::pair-nil fix::cell)
   (let loop ((nodes nodes)
	      (typ 'undefined)
	      (env env)
	      (bks '()))
      (if (null? nodes)
	  (return typ env bks)
	  (multiple-value-bind (typn envn bk)
	     (typing (car nodes) env fix)
	     (if (pair? bk)
		 (multiple-value-bind (typr envr bkr)
		    (typing-seq (cdr nodes) envn fix)
		    (return typr (env-merge envn envr) (append bk bk bks)))
		 (loop (cdr nodes) typn envn (append bk bks)))))))

;*---------------------------------------------------------------------*/
;*    typing-seq ...                                                   */
;*---------------------------------------------------------------------*/
(define (typing-seq nodes::pair-nil env::pair-nil fix::cell)
   (let loop ((nodes nodes)
	      (typ 'void)
	      (env env)
	      (bks '()))
      (if (null? nodes)
	  (return typ env bks)
	  (multiple-value-bind (typn envn bk)
	     (typing (car nodes) env fix)
	     (if (pair? bk)
		 (multiple-value-bind (typr envr bkr)
		    (typing-seq (cdr nodes) envn fix)
		    (return typr (env-merge envn envr) (append bk bk bks)))
		 (loop (cdr nodes) typn envn (append bk bks)))))))

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
(define-walk-method (typing this::J2SNode env::pair-nil fix::cell)
    ;; conservative guard
   (tprint "*** typing node: " (typeof this))
   (return 'any '() '()))

;*---------------------------------------------------------------------*/
;*    typing ::J2SMeta ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SMeta env::pair-nil fix::cell)
   (with-access::J2SMeta this (optim)
      (if (=fx optim 0)
	  (return 'void env '())
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SExpr ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SExpr env::pair-nil fix::cell)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    typing ::J2SNull ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SNull env::pair-nil fix::cell)
   (expr-type-set! this env fix 'null))

;*---------------------------------------------------------------------*/
;*    typing ::J2SUndefined ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SUndefined env::pair-nil fix::cell)
   (expr-type-set! this env fix 'undefined))

;*---------------------------------------------------------------------*/
;*    typing ::J2SArrayAbsent ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SArrayAbsent env::pair-nil fix::cell)
   (expr-type-set! this env fix 'undefined))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SNumber ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SNumber env::pair-nil fix::cell)
   (with-access::J2SNumber this (val type)
      (expr-type-set! this env fix
	 (cond
	    ((and (> val 0) (< val (bit-lsh 1 28))) 'index)
	    ((= val 0) 'index)
	    ((integer? val) 'integer)
	    (else 'number)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SBool ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SBool env::pair-nil fix::cell)
   (expr-type-set! this env fix 'bool))

;*---------------------------------------------------------------------*/
;*    typing ::J2SString ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SString env::pair-nil fix::cell)
   (expr-type-set! this env fix 'string))

;*---------------------------------------------------------------------*/
;*    typing ::J2SNativeString ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SNativeString env::pair-nil fix::cell)
   (expr-type-set! this env fix 'scmstring))

;*---------------------------------------------------------------------*/
;*    typing ::J2SRegExp ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SRegExp env::pair-nil fix::cell)
   (expr-type-set! this env fix 'regexp))

;*---------------------------------------------------------------------*/
;*    typing ::J2SCmap ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCmap env::pair-nil fix::cell)
   (expr-type-set! this env fix 'cmap))

;*---------------------------------------------------------------------*/
;*    typing ::J2STemplate ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2STemplate env::pair-nil fix::cell)
   (with-access::J2STemplate this (exprs)
      (multiple-value-bind (env bk)
	 (typing-args exprs env fix)
	 (expr-type-set! this env fix 'string bk))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SArray ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SArray env::pair-nil fix::cell)
   (with-access::J2SArray this (exprs len)
      (multiple-value-bind (env bk)
	 (typing-args exprs env fix)
	 (expr-type-set! this env fix 'array bk))))

;*---------------------------------------------------------------------*/
;*    typing ::J2STilde ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2STilde env::pair-nil fix::cell)
   (expr-type-set! this env fix 'tilde))

;*---------------------------------------------------------------------*/
;*    typing ::J2SSequence ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SSequence env::pair-nil fix::cell)
   (with-access::J2SSequence this (exprs)
      (typing* exprs env fix)))
      
;*---------------------------------------------------------------------*/
;*    typing ::J2SLiteralCnst ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SLiteralCnst env::pair-nil fix::cell)
   (with-access::J2SLiteralCnst this (val)
      (multiple-value-bind (tyv env bk)
	 (typing val env fix)
	 (expr-type-set! this env fix tyv bk))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SHopRef ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SHopRef env::pair-nil fix::cell)
   (with-access::J2SHopRef this (itype)
      (expr-type-set! this env fix itype)))

;*---------------------------------------------------------------------*/
;*    typing ::J2SUnresolvedRef ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SUnresolvedRef env::pair-nil fix::cell)
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
(define-walk-method (typing this::J2SRef env::pair-nil fix::cell)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (ronly id key)
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (val) (escape-fun val fix)))
	 (let ((etyp (env-lookup env decl)))
	    (when (eq? etyp 'unknown)
	       (with-access::J2SDecl decl (vtype)
		  (set! etyp vtype)))
;* 		  (cond                                                */
;* 		     ((and ronly (isa? decl J2SDeclInit))              */
;* 		      (set! etyp vtype))                               */
;* 		     ((isa? decl J2SDeclInit))                         */
;* 		      (set! etyp vtype))                               */
;* 		     ((eq? vtype 'any)                                 */
;* 		      (set! etyp vtype)))))                            */
	    (expr-type-set! this env fix etyp)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SBindExit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (typing this::J2SBindExit env::pair-nil fix::cell)
   (with-access::J2SBindExit this (stmt)
      (multiple-value-bind (typ env bk)
	 (typing stmt env fix)
	 (expr-type-set! this env fix typ))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDecl ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDecl env::pair-nil fix::cell)
   (with-access::J2SDecl this (itype)
      (decl-vtype-set! this 'any fix)
      (set! itype (merge-types itype 'any))
      (return 'void env '())))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDeclInit env::pair-nil fix::cell)
   (with-access::J2SDeclInit this (val itype)
      (multiple-value-bind (typv env bk)
	 (typing val env fix)
	 (if (or (eq? typv 'unknown) (not typv))
	     (return 'void env bk)
	     (begin
		(decl-vtype-set! this typv fix)
		(set! itype (merge-types itype typv))
		(return 'void (extend-env env this typv) bk))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDeclFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDeclFun env::pair-nil fix::cell)

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
	 (typing-fun val (typing-fun-decl val env) fix)
	 (return 'void (extend-env env this tyf) '()))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SAssig ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SAssig env::pair-nil fix::cell)
   
   (define (this-assig? lhs)
      (when (isa? lhs J2SAccess)
	 (with-access::J2SAccess lhs (obj)
	    (when (isa? obj J2SThis)
	       (with-access::J2SThis obj (decl)
		  decl)))))
   
   (with-access::J2SAssig this (lhs rhs)
      (multiple-value-bind (tyv envl lbk)
	 (typing lhs env fix)
	 (cond
	    ;; this assignment
	    ((isa? lhs J2SRef)
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (writable utype)
		   (multiple-value-bind (tyr env rbk)
		      (typing rhs envl fix)
		      (cond
			 ((and (not writable) (not (isa? this J2SInit)))
			  (let ((nenv (extend-env env decl utype)))
			     (expr-type-set! this nenv fix utype
				(append lbk rbk))))
			 (tyr
			  (with-access::J2SRef lhs (decl)
			     (decl-vtype-set! decl tyr fix)
			     (let ((nenv (extend-env env decl tyr)))
				(expr-type-set! this nenv fix tyr
				   (append lbk rbk)))))
			 (else
			  (return 'unknown env (append lbk rbk))))))))
	    ((this-assig? lhs)
	     =>
	     (lambda (decl)
		;; this property assignment
		(let ((envt (extend-env envl decl 'object)))
		   (multiple-value-bind (tyr nenv rbk)
		      (typing rhs envt fix)
		      (if tyr
			  (expr-type-set! this nenv fix tyr (append lbk rbk))
			  (return 'unknown
			     (extend-env env decl 'object)
			     (append lbk rbk)))))))
	    (else
	     ;; a non variable assinment
	     (multiple-value-bind (tyr nenv rbk)
		(typing rhs envl fix)
		(if tyr
		    (expr-type-set! this nenv fix tyr (append lbk rbk))
		    (return 'unknown env (append lbk rbk)))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SAssigOp ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SAssigOp env::pair-nil fix::cell)
   (with-access::J2SAssigOp this (lhs rhs op)
      (multiple-value-bind (tyr envr bkr)
	 (typing-binary op lhs rhs env fix)
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
(define-walk-method (typing this::J2SPostfix env::pair-nil fix::cell)
   (with-access::J2SPostfix this (lhs rhs op)
      (multiple-value-bind (tyr envr bkr)
	 (typing rhs env fix)
	 (unless (type-number? tyr) (set! tyr 'number))
	 (multiple-value-bind (tyv __ lbk)
	    (typing lhs env fix)
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
(define-walk-method (typing this::J2SPrefix env::pair-nil fix::cell)
   (with-access::J2SPrefix this (lhs rhs op)
      (multiple-value-bind (tyr envr bkr)
	 (typing rhs env fix)
	 (unless (type-number? tyr) (set! tyr 'number))
	 (multiple-value-bind (tyv __ lbk)
	    (typing lhs env fix)
	    (cond
	       ((isa? lhs J2SRef)
		;; a variable assignment
		(with-access::J2SRef lhs (decl)
		   (decl-vtype-set! decl tyr fix)
		   (let ((nenv (extend-env envr decl tyr)))
		      (expr-type-set! this nenv fix tyr (append lbk bkr)))))
	       (else
		;; a non variable assignment
		(expr-type-set! this envr fix tyr bkr)))))))

;*---------------------------------------------------------------------*/
;*    typing-fun ...                                                   */
;*---------------------------------------------------------------------*/
(define (typing-fun this::J2SFun env::pair-nil fix::cell)
   (with-access::J2SFun this (body params rtype thisp %info)
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
	 (multiple-value-bind (_ envf _)
	    (typing body (append envt env) fix)
	    (set! %info envf))
	 (expr-type-set! this env fix 'function))))

;*---------------------------------------------------------------------*/
;*    typing-fun-decl fun ...                                          */
;*---------------------------------------------------------------------*/
(define (typing-fun-decl this::J2SFun env::pair-nil)
   (with-access::J2SFun this (body rtype decl)
      (when (isa? decl J2SDecl)
	 (with-access::J2SDecl decl (vtype utype itype)
	    (set! vtype 'function)
	    (set! itype 'function)
	    (set! utype 'function)))
      (filter-map (lambda (c)
		     (let ((d (car c))
			   (t (cdr c)))
			(with-access::J2SDecl d (ronly vtype)
			   (if ronly c (cons d vtype)))))
	 env)))

;*---------------------------------------------------------------------*/
;*    escape-fun ...                                                   */
;*---------------------------------------------------------------------*/
(define (escape-fun val::J2SFun fix)
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

;*---------------------------------------------------------------------*/
;*    typing ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SFun env::pair-nil fix::cell)
   (escape-fun this fix)
   (typing-fun this (typing-fun-decl this env) fix))

;*---------------------------------------------------------------------*/
;*    typing ::J2SMethod ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SMethod env::pair-nil fix::cell)
   (call-default-walker)
   (expr-type-set! this env fix 'function))

;*---------------------------------------------------------------------*/
;*    typing ::J2SCall ...                                             */
;*    -------------------------------------------------------------    */
;*    Function calls may affect the typing environment, depending      */
;*    on the called function. Each case comes with a special rule,     */
;*    detailed in the code below.                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCall env::pair-nil fix::cell)
   
   (define (unknown-call-env env)
      ;; compute a new typing environment where all mutated globals
      ;; and all mutated captured locals are removed
      (filter (lambda (e)
		 (with-access::J2SDecl (car e) (ronly scope %info id)
		    (or ronly
			(and (eq? scope 'local) (eq? %info 'nocapture)))))
	 env))
   
   (define (type-known-call-args callee args env bk)
      (with-access::J2SFun callee (rtype params)
	 (let loop ((params params)
		    (args args))
	    (when (pair? params)
	       (with-access::J2SDecl (car params) (itype vtype usage id)
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
		      (loop (cdr params) (cdr args)))))))))
   
   (define (type-inline-call callee args env bk)
      ;; type a direct function call: ((function (...) { ... })( ... ))
      ;; side effects are automatically handled when
      ;; typing the function body
      (type-known-call-args callee args env bk)
      (multiple-value-bind (_ envf _)
	 (typing-fun callee env fix)
	 (with-access::J2SFun callee (rtype)
	    (expr-type-set! this envf fix rtype bk))))

   (define (type-known-call ref::J2SRef callee args env bk)
      ;; type a known constant function call: F( ... )
      ;; the new typing environment is a merge of env and the environment
      ;; produced by the function
      (expr-type-set! ref env fix 'function)
      (type-known-call-args callee args env bk)
      (with-access::J2SRef ref (decl)
	 (with-access::J2SDecl decl (scope)
	    (with-access::J2SFun callee (rtype %info)
	       (let ((nenv (if (env? %info) (env-override env %info) env)))
		  (expr-type-set! this nenv fix rtype bk))))))
   
   (define (type-ref-call callee args env bk)
      ;; call a JS variable, check is it a known function
      (with-access::J2SRef callee (decl)
	 (cond
	    ((isa? decl J2SDeclFun)
	     (with-access::J2SDeclFun decl (ronly val)
		(if ronly
		    (type-known-call callee val args env bk)
		    (type-unknown-call callee env bk))))
	    ((isa? decl J2SDeclInit)
	     (with-access::J2SDeclInit decl (ronly val)
		(if (and ronly (isa? val J2SFun))
		    (type-known-call callee val args env bk)
		    (type-unknown-call callee env bk))))
	    (else
	     (type-unknown-call callee env bk)))))

   (define (is-global? obj ident)
      (when (isa? obj J2SGlobalRef)
	 (with-access::J2SGlobalRef obj (id decl)
	    (when (eq? id ident)
	       (with-access::J2SDecl decl (ronly)
		  ronly)))))
   
   (define (String? obj)
      (is-global? obj 'String))

   (define (Array? obj)
      (is-global? obj 'Array))

   (define (type-method-call callee args env bk)
      ;; type a method call: O.m( ... )
      (multiple-value-bind (_ env bk)
	 (typing callee env fix)
	 (with-access::J2SAccess callee (obj field)
	    (let* ((fn (j2s-field-name field))
		   (ty (if (string? fn)
			   (case (j2s-type obj)
			      ((string) (string-method-type fn))
			      ((regexp) (regexp-method-type fn))
			      ((number integer index) (number-method-type fn))
			      ((array) (array-method-type fn))
			      (else (cond
				       ((String? obj)
					(string-static-method-type fn))
				       (else
					'any))))
			   'any)))
	       (if (eq? ty 'any)
		   ;; the method is unknown, filter out the typing env
		   (expr-type-set! this (unknown-call-env env) fix ty bk)
		   (expr-type-set! this env fix ty bk))))))
   
   (define (type-hop-call callee args env bk)
      ;; type a hop (foreign function) call: H( ... )
      ;; hop calls have no effect on the typing env
      (with-access::J2SHopRef callee (rtype)
	 (expr-type-set! this env fix rtype bk)))

   (define (type-global-call callee args env bk)
      (cond
	 ((Array? callee)
	  (expr-type-set! this (unknown-call-env env) fix 'array bk))
	 (else
	  (type-unknown-call callee env bk))))
   
   (define (type-unknown-call callee env bk)
      ;; type a unknown function call: expr( ... )
      ;; filter out the typing env
      (multiple-value-bind (_ env bk)
	 (typing callee env fix)
	 (expr-type-set! this (unknown-call-env env) fix 'any bk)))
   
   (with-access::J2SCall this ((callee fun) thisarg args)
      (multiple-value-bind (env bk)
	 (typing-args args env fix)
	 (cond
	    ((isa? callee J2SFun) (type-inline-call callee args env bk))
	    ((isa? callee J2SRef) (type-ref-call callee args env bk))
	    ((isa? callee J2SHopRef) (type-hop-call callee args env bk))
	    ((isa? callee J2SAccess) (type-method-call callee args env bk))
	    ((isa? callee J2SGlobalRef) (type-global-call callee args env bk))
	    (else (type-unknown-call callee env bk))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SCond ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCond env::pair-nil fix::cell)
   (with-access::J2SCond this (test then else)
      (multiple-value-bind (tyi env bki)
	 (typing test env fix)
	 (multiple-value-bind (tyt envt bkt)
	    (typing then env fix)
	    (multiple-value-bind (tye enve bke)
	       (typing else env fix)
	       (let ((envc (env-merge envt enve))
		     (typc (merge-types tyt tye))
		     (bk (append bki bkt bke)))
		  (expr-type-set! this envc fix typc bk)))))))

;*---------------------------------------------------------------------*/
;*    type ::J2SNew ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SNew env::pair-nil fix::cell)

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
	 (typing clazz env fix)
	 (multiple-value-bind (env bka)
	    (typing-args args env fix)
	    (expr-type-set! this env fix (class-type clazz) (append bk bka))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDataPropertyInit ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDataPropertyInit env::pair-nil fix::cell)
   (with-access::J2SDataPropertyInit this (val)
      (typing val env fix)))

;*---------------------------------------------------------------------*/
;*    typing ::J2SAccessorPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SAccessorPropertyInit env::pair-nil fix::cell)
   (with-access::J2SAccessorPropertyInit this (get set)
      (typing get env fix)
      (typing set env fix)
      (return 'void env '())))

;*---------------------------------------------------------------------*/
;*    typing ::J2SObjInit ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SObjInit env::pair-nil fix::cell)
   (with-access::J2SObjInit this (inits)
      (let ((args (filter-map (lambda (init)
				 (when (isa? init J2SDataPropertyInit)
				    (with-access::J2SDataPropertyInit init (val)
				       val)))
		     inits)))
	 (multiple-value-bind (env bk)
	    (typing-args args env fix)
	    (expr-type-set! this env fix 'object bk)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SUnary ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SUnary env::pair-nil fix::cell)

   (define (non-zero-integer? ty expr)
      (when (type-integer? ty)
	 (or (not (isa? expr J2SNumber))
	     (with-access::J2SNumber expr (val)
		(not (= val 0))))))

   (with-access::J2SUnary this (op expr)
      (multiple-value-bind (ty env bk)
	 (typing expr env fix)
	 (let ((tye (case op
		       ((+) (if (non-zero-integer? ty expr) 'integer 'number))
		       ((-) (if (non-zero-integer? ty expr) 'integer 'number))
		       ((~) 'integer)
		       ((!) 'bool)
		       ((typeof) 'string)
		       (else 'any))))
	    (expr-type-set! this env fix tye bk)))))

;*---------------------------------------------------------------------*/
;*    typing-binary ...                                                */
;*---------------------------------------------------------------------*/
(define (typing-binary op lhs::J2SExpr rhs::J2SExpr env::pair-nil fix::cell)
   (multiple-value-bind (typl envl bkl)
      (typing lhs env fix)
      (multiple-value-bind (typr envr bkr)
	 (typing rhs envl fix)
	 (let ((typ (case op
		       ((+)
			(cond
			   ((and (type-integer? typl) (type-integer? typr))
			    'integer)
			   ((and (typnum? typl) (typnum? typr))
			    'number)
			   ((or (eq? typl 'string) (eq? typr 'string))
			    'string)
			   ((or (eq? typl 'any) (eq? typr 'any))
			    'any)
			   ((or (eq? typl 'unknown) (eq? typr 'unknown))
			    'unknown)
			   (else
			    'unknown)))
		       ((- *)
			(cond
			   ((and (type-integer? typl) (type-integer? typr))
			    'integer)
			   ((or (eq? typl 'any) (eq? typr 'any))
			    'any)
			   ((or (eq? typl 'unknown) (eq? typr 'unknown))
			    'unknown)
			   (else
			    'number)))
		       ((% /)
			(cond
			   ((and (type-integer? typl) (type-integer? typr))
			    'number)
			   ((or (eq? typl 'any) (eq? typr 'any))
			    'any)
			   ((or (eq? typl 'unknown) (eq? typr 'unknown))
			    'unknown)
			   (else
			    'number)))
		       ((== === != !== < <= > >= eq?)
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
			'integer)
		       (else
			'any))))
	    (return typ (if (eq? op 'OR) (env-merge envl envr) envr)
	       (append bkl bkr))))))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SBinary ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SBinary env::pair-nil fix::cell)
   (with-access::J2SBinary this (op lhs rhs)
      (multiple-value-bind  (typ env bk)
	 (typing-binary op lhs rhs env fix)
	 (expr-type-set! this env fix typ bk))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SAccess ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SAccess env::pair-nil fix::cell)

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
	    (typing obj env fix)
	    (multiple-value-bind (tyf envf bkf)
	       (typing field envo fix)
	       (cond
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
;*    typing ::J2SCacheCheck ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCacheCheck env::pair-nil fix::cell)
   (multiple-value-bind (typf envf bkf)
      (call-default-walker)
      (with-access::J2SCacheCheck this (type)
	 (expr-type-set! this envf fix 'bool bkf))))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SCacheUpdate ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCacheUpdate env::pair-nil fix::cell)
   (multiple-value-bind (typf envf bkf)
      (call-default-walker)
      (with-access::J2SCacheCheck this (type)
	 (expr-type-set! this envf fix 'undefined bkf))))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SPragma ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SPragma env::pair-nil fix::cell)
   (with-access::J2SPragma this (lang expr type)
      (if (eq? lang 'scheme)
	  (return type env '())
	  (multiple-value-bind (_ _ bk)
	     (typing expr env fix)
	     (return type env '())))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SParen ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SParen env::pair-nil fix::cell)
   (with-access::J2SParen this (expr)
      (multiple-value-bind (tye enve bke)
	 (typing expr env fix)
	 (if tye
	     (expr-type-set! this enve fix tye bke)
	     (return 'unknown enve bke)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SComprehension ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SComprehension env::pair-nil fix::cell)
   (with-access::J2SComprehension this (decls iterables test expr)
      (multiple-value-bind (_ envi bki)
	 (typing-seq iterables env fix)
	 (let loop ((env envi)
		    (i 0))
	    (let ((ofix (cell-ref fix)))
	       (multiple-value-bind (_ envb bk)
		  (typing-seq (list test expr) env fix)
		  (if (=fx ofix (cell-ref fix))
		      (return 'array envb (filter-breaks (append bk bki) this))
		      (loop (env-merge env envb) (+fx i 1)))))))))
		  
;*---------------------------------------------------------------------*/
;*    typing ::J2SNop ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SNop env::pair-nil fix::cell)
   (return 'void env '()))

;*---------------------------------------------------------------------*/
;*    typing ::J2SStmtExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SStmtExpr env::pair-nil fix::cell)
   (with-access::J2SStmtExpr this (expr)
      (multiple-value-bind (typ env bk)
	 (typing expr env fix)
	 (return typ env bk))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SSeq ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SSeq env::pair-nil fix::cell)
   (with-access::J2SSeq this (nodes)
      (typing-seq nodes env fix)))

;*---------------------------------------------------------------------*/
;*    typing ::J2SLabel ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SLabel env::pair-nil fix::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    typing ::J2SLetBlock ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SLetBlock env::pair-nil fix::cell)
   (with-access::J2SLetBlock this (decls nodes)
      (let ((ienv (map (lambda (d::J2SDecl)
			  (with-access::J2SDecl d (itype)
			     (cons d itype)))
		     decls)))
	 (multiple-value-bind (_ denv bk)
	    (typing-seq decls (append ienv env) fix)
	    (multiple-value-bind (typ benv bks)
	       (typing-seq nodes denv fix)
	       (let ((nenv (filter (lambda (d)
				      (not (memq (car d) decls)))
			      benv)))
		  (return typ nenv (append bk bks))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SReturn ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SReturn env::pair-nil fix::cell)
   (with-access::J2SReturn this (expr from)
      (multiple-value-bind (tye enve bke)
	 (typing expr env fix)
	 (cond
	    ((isa? from J2SFun)
	     (with-access::J2SFun from (rtype)
		(let ((tyr (merge-types rtype tye)))
		   (unless (eq? tyr rtype)
		      (unfix! fix (format "J2SReturn(~a) ~a/~a" tye tyr rtype))
		      (set! rtype tyr)))
		(values 'void enve (list this))))
	    ((isa? from J2SExpr)
	     (with-access::J2SFun from (type)
		(let ((tyr (merge-types type tye)))
		   (unless (eq? tyr type)
		      (unfix! fix (format "J2SReturn(~a) ~a/~a" tye tyr type))
		      (set! type tyr)))
		(values 'void enve (list this))))
	    (else
	     (values 'void enve (list this)))))))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SReturnYield ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SReturnYield env::pair-nil fix::cell)
   (with-access::J2SReturnYield this (expr)
      (multiple-value-bind (tye enve bke)
	 (typing expr env fix)
	 (values 'void enve (list this)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SIf ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SIf env::pair-nil fix::cell)
   
   (define (isa-and? test)
      (when (isa? test J2SBinary)
	 (with-access::J2SBinary test (op)
	    (eq? op '&&))))
   
   (define (typing-one-test test envt enve)
      (multiple-value-bind (op decl typ)
	 (j2s-expr-type-test test)
	 (if (symbol? typ)
	     (case op
		((== === eq?) (values (extend-env envt decl typ) enve))
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
	 (typing test env fix)
	 (multiple-value-bind (envt enve)
	    (typing-test test env env)
	    (multiple-value-bind (tyt envt bkt)
	       (typing then envt fix)
	       (multiple-value-bind (tye enve bke)
		  (typing else enve fix)
		  (let ((bk (append bki bke bkt)))
		     (return (merge-types tyt tye)
			(env-merge envt enve) bk))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SSwitch ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SSwitch env::pair-nil fix::cell)
   (with-access::J2SSwitch this (key cases)
      (multiple-value-bind (typk envk bk)
	 (typing key env fix)
	 (let ((bks '())
	       (typ #f))
	    (let loop ((cases cases)
		       (env envk))
	       (when (pair? cases)
		  (multiple-value-bind (t e b)
		     (typing (car cases) env fix)
		     (set! bks (append b bks))
		     (set! typ (if (not typ) t (merge-types typ t)))
		     (with-access::J2SCase (car cases) (cascade)
			(if cascade
			    (loop (cdr cases) e)
			    (loop (cdr cases) envk))))))
	    (return typ '() bks)))))

;*---------------------------------------------------------------------*/
;*    type ::J2SCase ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCase env::pair-nil fix::cell)
   (with-access::J2SCase this (expr body cascade)
      (multiple-value-bind (typx envx bk)
	 (typing expr env fix)
	 (multiple-value-bind (typb envb bkb)
	    (typing body envx fix)
	    (return typb envb (append bkb bk))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SBreak ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SBreak env::pair-nil fix::cell)
   (return 'void env (list this)))

;*---------------------------------------------------------------------*/
;*    typing ::J2SWhile ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SWhile env::pair-nil fix::cell)
   (with-access::J2SWhile this (test body)
      (let loop ((env env)
		 (i 0))
	 (let ((ofix (cell-ref fix)))
	    (multiple-value-bind (typ envb bk)
	       (typing-seq (list test body) env fix)
	       (if (=fx ofix (cell-ref fix))
		   (return typ envb (filter-breaks bk this))
		   (loop (env-merge env envb) (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDo ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDo env::pair-nil fix::cell)
   (with-access::J2SDo this (test body)
      (let loop ((env env))
	 (let ((ofix (cell-ref fix)))
	    (multiple-value-bind (typ envb bk)
	       (typing-seq (list body test) env fix)
	       (if (=fx ofix (cell-ref fix))
		   (return typ envb (filter-breaks bk this))
		   (loop (env-merge env envb))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SFor ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SFor env::pair-nil fix::cell)
   (with-access::J2SFor this (init test incr body)
      (let loop ((env env))
	 (let ((ofix (cell-ref fix)))
	    (multiple-value-bind (typ envb bk)
	       (typing-seq (list init test body incr) env fix)
	       (if (=fx ofix (cell-ref fix))
		   (return typ envb (filter-breaks bk this))
		   (loop (env-merge env envb))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SForIn ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SForIn env::pair-nil fix::cell)
   (with-access::J2SForIn this (lhs obj body)
      (let loop ((env env))
	 (let ((ofix (cell-ref fix)))
	    (multiple-value-bind (typ envb bk)
	       (typing-seq (list obj body) env fix)
	       (if (=fx ofix (cell-ref fix))
		   (return typ envb (filter-breaks bk this))
		   (loop (env-merge env envb))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2STry ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2STry env::pair-nil fix::cell)
   (with-access::J2STry this (body catch finally)
      (multiple-value-bind (_ envb bkb)
	 (typing body env fix)
	 (when (isa? catch J2SCatch)
	    (with-access::J2SCatch catch (param)
	       (decl-vtype-set! param 'any fix)))
	 (multiple-value-bind (_ envc bkh)
	    (typing catch env fix)
	    (multiple-value-bind (_ envf bkf)
	       (typing finally (env-merge envb envc) fix)
	       (return 'void envf (append bkb bkh bkf)))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SCatch ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCatch env::pair-nil fix::cell)
   (with-access::J2SCatch this (body)
      (typing body env fix)))

;*---------------------------------------------------------------------*/
;*    type ::J2SThrow ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SThrow env::pair-nil fix::cell)
   (with-access::J2SThrow this (expr)
      (multiple-value-bind (_ env bk)
	 (typing expr env fix)
	 (return 'void env (cons this bk)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SClass ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SClass env::pair-nil fix::cell)
   (with-access::J2SClass this (expr)
      (call-default-walker)
      (expr-type-set! this env fix 'class)))

;*---------------------------------------------------------------------*/
;*    typing ::J2SClassElement ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SClassElement env::pair-nil fix::cell)
   (with-access::J2SClassElement this (prop)
      (typing prop env fix)
      (return 'void env '())))
      
;*---------------------------------------------------------------------*/
;*    j2s-resolve! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Resolve statically type checks using type informations           */
;*    computed by the TYPING method.                                   */
;*---------------------------------------------------------------------*/
(define (j2s-resolve! this::J2SProgram args fix)   
   (with-access::J2SProgram this (headers decls nodes)
      (set! headers (map! (lambda (o) (resolve! o fix)) headers))
      (set! decls (map! (lambda (o) (resolve! o fix)) decls))
      (set! nodes (map! (lambda (o) (resolve! o fix)) nodes))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SNode fix::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SBinary ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SBinary fix)

   (define (eq-typeof? type typ)
      (or (eq? type typ)
	  (and (memq type '(date array)) (eq? typ 'object))
	  (and (eq? typ 'number) (memq type '(integer index)))
	  (and (eq? typ 'boolean) (eq? type 'bool))))
   
   (with-access::J2SBinary this (loc op)
      (case op
	 ((&&)
	  (with-access::J2SBinary this (lhs rhs loc)
	     (set! lhs (resolve! lhs fix))
	     (set! rhs (resolve! rhs fix))
	     (cond
		((and (isa? lhs J2SBool) (isa? rhs J2SBool))
		 (with-access::J2SBool lhs ((lval val))
		    (with-access::J2SBool rhs ((rval val))
		       (J2SBool (and lval rval)))))
		((isa? lhs J2SBool)
		 (with-access::J2SBool lhs (val)
		    (if val rhs (J2SBool #f))))
		((isa? rhs J2SBool)
		 (with-access::J2SBool rhs (val)
		    (if val lhs (J2SBool #f))))
		(else
		 this))))
	 ((OR)
	  (with-access::J2SBinary this (lhs rhs loc)
	     (set! lhs (resolve! lhs fix))
	     (set! rhs (resolve! rhs fix))
	     (cond
		((and (isa? lhs J2SBool) (isa? rhs J2SBool))
		 (with-access::J2SBool lhs ((lval val))
		    (with-access::J2SBool rhs ((rval val))
		       (J2SBool (or lval rval)))))
		((isa? lhs J2SBool)
		 (with-access::J2SBool lhs (val)
		    (if val (J2SBool #t) rhs)))
		((isa? rhs J2SBool)
		 (with-access::J2SBool rhs (val)
		    (if val (J2SBool #t) lhs)))
		(else
		 this))))
	 (else
	  (multiple-value-bind (op decl typ ref)
	     (j2s-expr-type-test this)
	     (case op
		((== === eq?)
		 (with-access::J2SExpr ref (type)
		    (cond
		       ((eq-typeof? type typ)
			(unfix! fix "resolve.J2SBinary")
			(J2SBool #t))
		       ((memq type '(unknown any))
			(call-default-walker))
		       ((and (eq? type 'number) (memq typ '(integer index)))
			(call-default-walker))
		       (else
			(unfix! fix "resolve.J2SBinary")
			(J2SBool #f)))))
		((!= !==)
		 (with-access::J2SExpr ref (type)
		    (cond
		       ((eq-typeof? type typ)
			(unfix! fix "resolve.J2SBinary")
			(J2SBool #f))
		       ((memq type '(unknown any))
			(call-default-walker))
		       ((and (eq? type 'number) (memq typ '(integer index)))
			(call-default-walker))
		       (else
			(unfix! fix "resolve.J2SBinary")
			(J2SBool #t)))))
		((instanceof)
		 (with-access::J2SExpr ref (type)
		    (cond
		       ((or (memq type '(unknown any object)) (eq? typ 'object))
			(call-default-walker))
		       ((eq? type typ)
			(unfix! fix "resolve.J2SBinary")
			(J2SBool #t))
		       (else
			(unfix! fix "resolve.J2SBinary")
			(J2SBool #f)))))
		((!instanceof)
		 (with-access::J2SExpr ref (type)
		    (cond
		       ((or (memq type '(unknown any object)) (eq? typ 'object))
			(call-default-walker))
		       ((eq? type typ)
			(unfix! fix "resolve.J2SBinary")
			(J2SBool #f))
		       (else
			(unfix! fix "resolve.J2SBinary")
			(J2SBool #t)))))
		((&&)
		 (with-access::J2SBinary this (lhs rhs loc)
		    (tprint "AND lhs=" (j2s->list lhs) " rhs=" (j2s->list rhs))
		    (set! lhs (resolve! lhs fix))
		    (set! rhs (resolve! rhs fix))
		    (if (and (isa? lhs J2SBool) (isa? rhs J2SBool))
			(with-access::J2SBool lhs ((lval val))
			   (with-access::J2SBool rhs ((rval val))
			      (J2SBool (and lval rval))))
			this)))
		((or)
		 (with-access::J2SBinary this (lhs rhs loc)
		    (set! lhs (resolve! lhs fix))
		    (set! rhs (resolve! rhs fix))
		    (if (and (isa? lhs J2SBool) (isa? rhs J2SBool))
			(with-access::J2SBool lhs ((lval val))
			   (with-access::J2SBool rhs ((rval val))
			      (J2SBool (or lval rval))))
			this)))
		(else
		 (call-default-walker))))))))
   
;*---------------------------------------------------------------------*/
;*    resolve! ::J2SIf ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SIf fix::cell)
   
   (define (is-true? expr)
      (cond
	 ((isa? expr J2SBool)
	  (with-access::J2SBool expr (val) val))
	 ((isa? expr J2SBinary)
	  (with-access::J2SBinary expr (lhs rhs op)
	     (and (eq? op '==)
		  (memq (j2s-type lhs) '(null undefined))
		  (memq (j2s-type rhs) '(null undefined)))))))

   (define (is-false? expr)
      (cond
	 ((isa? expr J2SBool)
	  (with-access::J2SBool expr (val) (not val)))
	 ((isa? expr J2SBinary)
	  (with-access::J2SBinary expr (lhs rhs op)
	     (and (eq? op '!=)
		  (memq (j2s-type lhs) '(null undefined))
		  (memq (j2s-type rhs) '(null undefined)))))))
   
   (with-access::J2SIf this (test then else)
      (set! test (resolve! test fix))
      (with-access::J2SExpr test (type)
	 (cond
	    ((memq type '(any unknown))
	     (set! then (resolve! then fix))
	     (set! else (resolve! else fix))
	     this)
	    ((is-true? test)
	     (unfix! fix "resolve.J2SIf")
	     (resolve! then fix))
	    ((is-false? test)
	     (unfix! fix "resolve.J2SIf")
	     (resolve! else fix))
	    (else
	     (set! then (resolve! then fix))
	     (set! else (resolve! else fix))
	     this)))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SCall fix::cell)
   
   (define (same-type typ tyr)
      (cond
	 ((eq? typ tyr) #t)
	 ((eq? typ 'fixnum) #unspecified)
	 (else #f)))
   
   (with-access::J2SCall this (loc)
      (multiple-value-bind (op decl typ ref)
	 (j2s-expr-type-test this)
	 (if (or (not op) (not ref) (memq (j2s-type ref) '(any unknown)))
	     (call-default-walker)
	     (case op
		((==)
		 (let ((b (same-type typ (j2s-type ref))))
		    (if (boolean? b)
			(J2SBool b)
			(call-default-walker))))
		((!=)
		 (let ((b (same-type typ (j2s-type ref))))
		    (if (boolean? b)
			(J2SBool (not b))
			(call-default-walker))))
		(else
		 (call-default-walker)))))))

;*---------------------------------------------------------------------*/
;*    force-type! ...                                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SNode from to)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SExpr from to)
   (with-access::J2SExpr this (type)
      (when (eq? type from) (set! type to)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SFun from to)
   (with-access::J2SFun this (rtype thisp)
      (when (isa? thisp J2SNode) (force-type! thisp from to))
      (when (eq? rtype from) (set! rtype to)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SDecl ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SDecl from to)
   (with-access::J2SDecl this (itype vtype)
      (when (eq? itype from) (set! itype to))
      (when (eq? vtype from) (set! vtype to)))
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
   (with-access::J2SDecl this (itype vtype utype ronly id)
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

;*---------------------------------------------------------------------*/
;*    force-get-type! ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (force-unary-type! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    typing ::J2SUnary ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (force-unary-type! this::J2SUnary)

   (define (non-zero-integer? expr)
      (when (isa? expr J2SNumber)
	 (with-access::J2SNumber expr (val)
	    (not (= val 0)))))

   (with-access::J2SUnary this (op expr type)
      (when (eq? type 'integer)
	 (unless (non-zero-integer? expr)
	    (when (memq op '(+ -))
	       (set! type 'number)))))

   this)


