;*=====================================================================*/
;*    .../prgm/project/hop/3.2.x-new-types/js2scheme/tyflow.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 16 06:12:13 2016                          */
;*    Last change :  Tue Aug 14 05:40:06 2018 (serrano)                */
;*    Copyright   :  2016-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    js2scheme type inference                                         */
;*    -------------------------------------------------------------    */
;*    This pass does not assume any type decoration in the AST, not    */
;*    even for literals and constants.                                 */
;*    -------------------------------------------------------------    */
;*    This stage assigns type to variable declaration and variable     */
;*    references. At the a declaration site, the types are:            */
;*                                                                     */
;*      itype: the initial type of the variable                        */
;*      utype: the user given type, enforced on assignments            */
;*      vtype: the final type of the variable (used by scheme)         */
;*                                                                     */
;*    Types of declared variables have the following properties:       */
;*      1- itype < vtype                                               */
;*      2- for in ref(v), T(ref(v)) < vtype                            */
;*      3- if utype != unknown then itype = vtype = utype              */
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
;*    debug-tyflow ...                                                 */
;*---------------------------------------------------------------------*/
(define debug-tyflow #f)

;*---------------------------------------------------------------------*/
;*    j2s-typing! ::obj ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-typing! this args)
   (when (isa? this J2SProgram)
      (type-program! this args)
      this))

;*---------------------------------------------------------------------*/
;*    tyflow-type ...                                                  */
;*    -------------------------------------------------------------    */
;*    Maps to tyflow understood types.                                 */
;*---------------------------------------------------------------------*/
(define (tyflow-type ty)
   (j2s-hint-type ty))

;*---------------------------------------------------------------------*/
;*    type-program! ...                                                */
;*---------------------------------------------------------------------*/
(define (type-program! this::J2SProgram args)
   
   (define j2s-verbose (config-get args :verbose 0))
   
   (with-access::J2SProgram this (headers decls nodes)
      (when (>=fx j2s-verbose 3) (display " " (current-error-port)))
      ;; mark local captured variables
      (program-capture! this)
      ;; main fix point
      (if (config-get args :optim-tyflow #f)
	  (let ((fix (make-cell 0)))
	     (let loop ((i 1))
		(when (>=fx j2s-verbose 3)
		   (fprintf (current-error-port) "~a." i)
		   (flush-output-port (current-error-port)))
		(let ((ofix (cell-ref fix)))
		   ;; type all the nodes
		   (typing-seq (append headers decls nodes) '() fix)
		   (cond
		      ((not (=fx (cell-ref fix) ofix))
		       (loop (+fx i 1)))
		      ((config-get args :optim-tyflow-resolve #f)
		       ;; type check resolution
		       (j2s-resolve! this args fix)
		       (cond
			  ((not (=fx (cell-ref fix) ofix))
			   (loop (+fx i 1)))
			  ((config-get args :optim-hint #f)
			   ;; hint typing optimization
			   (when (>=fx j2s-verbose 3)
			      (fprintf (current-error-port) "hint"))
			   (let ((dups (j2s-hint! this args)))
			      (cond
				 ((pair? dups)
				  (when (>=fx j2s-verbose 3)
				     (fprintf (current-error-port)
					(format " [~(,)]."
					   (map (lambda (d)
						   (with-access::J2SDecl d (id)
						      id))
					      dups))))
				  (for-each reset-type! decls)
				  (for-each reset-type! nodes)
				  (loop (+fx i 1)))
				 ((force-type this 'unknown 'any)
				  (when (>=fx j2s-verbose 3)
				     (fprintf (current-error-port) "."))
				  (loop (+fx i 1))))))
			  ((force-type this 'unknown 'any)
			   (loop (+fx i 1)))))))))
	  (force-type this 'unknown 'any))
      (unless (config-get args :optim-integer)
	 (force-type this 'integer 'number))
	 ;;(force-unary-type! this))
      ;;(cleanup-hint! this)
      (program-cleanup! this))
   this)

;*---------------------------------------------------------------------*/
;*    program-cleanup! ...                                             */
;*---------------------------------------------------------------------*/
(define (program-cleanup! this::J2SProgram)
   (with-access::J2SProgram this (headers decls nodes direct-eval)
      (reinit-use-count! this)
      (unless direct-eval
	 (set! decls (filter-dead-declarations decls))
	 (for-each j2s-hint-meta-noopt! decls))
      this))
   
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
;*    -------------------------------------------------------------    */
;*    Mark variables declaration that are captured by a closure.       */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SNode env::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-capture ::J2SRef ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SRef env::pair-nil)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (scope ronly %info)
	 (when (not ronly)
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
;*    mark-capture ::J2SKont ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SKont env::pair-nil)
   (with-access::J2SKont this (param exn body)
      (with-access::J2SDecl param (%info)
	 (set! %info 'nocapture))
      (with-access::J2SDecl exn (%info)
	 (set! %info 'nocapture))
      (mark-capture body (list param exn))))
      
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
;*    decl-vtype-set! ...                                              */
;*    -------------------------------------------------------------    */
;*    Assign a unique type to a variable declaration.                  */
;*---------------------------------------------------------------------*/
(define (decl-vtype-set! decl::J2SDecl ty::symbol fix::cell)
   (with-access::J2SDecl decl (vtype id loc)
      [assert (ty) (or (eq? vtype 'unknown) (eq? vtype ty))]
      (when (or (eq? vtype 'unknown) (not (eq? vtype ty)))
	 (unfix! fix (format "J2SDecl.set(~a, ~a) vtype=~a/~a" id loc vtype ty))
	 (set! vtype (tyflow-type ty)))))

;*---------------------------------------------------------------------*/
;*    decl-vtype-add! ...                                              */
;*    -------------------------------------------------------------    */
;*    Add a new type to a variable declaration.                        */
;*---------------------------------------------------------------------*/
(define (decl-vtype-add! decl::J2SDecl ty::symbol fix::cell)
   
   (define (subtype? t1 t2)
      (or (eq? t1 t2)
	  (and (memq t1 '(index length indexof)) (memq t2 '(integer number)))
	  (and (eq? t1 'integer) (eq? t2 'number))))

   (with-access::J2SDecl decl (vtype id loc)
      (unless (or (eq? ty 'unknown) (subtype? ty vtype) (eq? vtype 'any))
	 (unfix! fix (format "J2SDecl.add(~a, ~a) vtype=~a/~a" id loc vtype ty))
	 (set! vtype (tyflow-type (merge-types vtype ty))))))

;*---------------------------------------------------------------------*/
;*    decl-itype-add! ...                                              */
;*    -------------------------------------------------------------    */
;*    Add a new initial type to a variable declaration.                */
;*---------------------------------------------------------------------*/
(define (decl-itype-add! decl::J2SDecl ty::symbol fix::cell)
   
   (define (subtype? t1 t2)
      (or (eq? t1 t2)
	  (and (memq t1 '(index length indexof)) (memq t2 '(integer number)))
	  (and (eq? t1 'integer) (eq? t2 'number))))

   (with-access::J2SDecl decl (itype id)
      (unless (or (eq? ty 'unknown) (subtype? ty itype) (eq? itype 'any))
	 (unfix! fix (format "J2SDecl(~a) vtype=~a/~a" id itype ty))
	 (set! itype (tyflow-type (merge-types itype ty))))))

;*---------------------------------------------------------------------*/
;*    expr-type-set! ...                                               */
;*    -------------------------------------------------------------    */
;*    Set the expression type and if needed update the fix stamp.      */
;*---------------------------------------------------------------------*/
(define (expr-type-set! this::J2SExpr env::pair-nil fix::cell ty::symbol
	   #!optional (bk '()))
   (with-access::J2SExpr this (type)
      (unless (or (eq? ty 'unknown) (eq? type ty))
	 (let ((ntype (merge-types type ty)))
	    (unless (eq? ntype type)
	       (unfix! fix
		  (format "expr-type-set! ~a ~a/~a -> ~a"
		     (j2s->list this) ty type ntype))
	       (set! type (tyflow-type ntype)))))
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
      ((and (eq? left 'number) (eq? right 'integer)) 'number)
      (else 'any)))

;*---------------------------------------------------------------------*/
;*    typnum? ...                                                      */
;*---------------------------------------------------------------------*/
(define (typnum? ty::symbol)
   (memq ty '(index length indexof integer real number)))

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
	  (with-access::J2SDecl decl (vtype) vtype))))

;*---------------------------------------------------------------------*/
;*    env-merge ...                                                    */
;*---------------------------------------------------------------------*/
(define (env-merge::pair-nil left::pair-nil right::pair-nil)
   
   (define (merge2 env1 env2)
      (filter-map (lambda (entry)
		     (let* ((decl (car entry))
			    (typl (cdr entry))
			    (typf (env-lookup env2 decl))
			    (typm (if (eq? typf 'unknown)
				      'unknown
				      (merge-types typl typf))))
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
;*    Type all the arguments and return a new environment. The         */
;*    arguments are typed as left to right sequence.                   */
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
	      (ty 'undefined)
	      (env env)
	      (bks '()))
      (if (null? nodes)
	  (return ty env bks)
	  (multiple-value-bind (tyn envn bk)
	     (typing (car nodes) env fix)
	     (if (pair? bk)
		 (multiple-value-bind (tyr envr bkr)
		    (typing-seq (cdr nodes) envn fix)
		    (return tyr (env-merge envn envr) (append bk bk bks)))
		 (loop (cdr nodes) tyn envn (append bk bks)))))))

;*---------------------------------------------------------------------*/
;*    typing-seq ...                                                   */
;*---------------------------------------------------------------------*/
(define (typing-seq nodes::pair-nil env::pair-nil fix::cell)
   (let loop ((nodes nodes)
	      (ty 'void)
	      (env env)
	      (bks '()))
      (if (null? nodes)
	  (return ty env bks)
	  (multiple-value-bind (tyn envn bk)
	     (typing (car nodes) env fix)
	     (if (pair? bk)
		 (multiple-value-bind (tyr envr bkr)
		    (typing-seq (cdr nodes) envn fix)
		    (return tyr (env-merge envn envr) (append bk bk bks)))
		 (loop (cdr nodes) tyn envn (append bk bks)))))))

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
   (return 'any '() '()))

;*---------------------------------------------------------------------*/
;*    typing ::J2SMeta ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SMeta env::pair-nil fix::cell)
   (with-access::J2SMeta this (optim)
      (if (=fx optim 0)
	  (begin
	     (when (force-type this 'unknown 'any) (unfix! fix "force meta"))
	     (return 'void env '()))
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
	 (if (flonum? val) 'real 'integer))))

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
      (multiple-value-bind (tye env bk)
	 (typing* exprs env fix)
	 (expr-type-set! this env fix tye bk))))
      
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
   (with-access::J2SHopRef this (type)
      (return type env '())))

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
		 (expr-type-set! this env fix 'any)
		 (expr-type-set! this env fix 'object)))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SGlobalRef ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SGlobalRef env::pair-nil fix::cell)
   (with-access::J2SGlobalRef this (decl id)
      (with-access::J2SDecl decl (utype usage)
	 (cond
	    ((not (eq? utype 'unknown))
	     (decl-vtype-add! decl utype fix)
	     (expr-type-set! this env fix utype))
	    ((usage? '(assig) usage)
	     (multiple-value-bind (tyv env bk)
		(call-next-method)
		(decl-vtype-add! decl tyv fix)
		(return tyv env bk)))
	    ((memq id '(Math String Error Regex Date Function Array Promise))
	     (decl-vtype-add! decl 'object fix)
	     (expr-type-set! this env fix 'object))
	    ((eq? id 'undefined)
	     (decl-vtype-add! decl 'undefined fix)
	     (expr-type-set! this env fix 'undefined))
	    (else
	     (decl-vtype-add! decl 'any fix)
	     (expr-type-set! this env fix 'any))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SRef ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SRef env::pair-nil fix::cell)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (id key utype usage)
	 (when (and (isa? decl J2SDeclFun) (not (constructor-only? decl)))
	    (with-access::J2SDeclFun decl (val)
	       (if (isa? val J2SMethod)
		   (escape-method val fix)
		   (escape-fun val fix))))
	 (if (eq? utype 'unknown)
	     (let ((ty (env-lookup env decl)))
		(expr-type-set! this env fix ty))
	     (expr-type-set! this env fix utype)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SWithRef ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SWithRef env::pair-nil fix::cell)
   (with-access::J2SWithRef this (expr)
      (typing expr '() fix)
      (expr-type-set! this env fix 'any)))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SBindExit ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SBindExit env::pair-nil fix::cell)
   (with-access::J2SBindExit this (stmt type)
      (multiple-value-bind (typ env bk)
	 (typing stmt env fix)
	 (return type env '()))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDecl ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDecl env::pair-nil fix::cell)
   (decl-vtype-add! this 'undefined fix)
   (return 'void env '()))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDeclArguments ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDeclArguments env::pair-nil fix::cell)
   (decl-itype-add! this 'arguments fix)
   (decl-vtype-add! this 'arguments fix)
   (return 'void (extend-env env this 'arguments) '()))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDeclInit env::pair-nil fix::cell)
   (with-access::J2SDeclInit this (val utype usage id loc)
      (multiple-value-bind (ty env bk)
	 (typing val env fix)
	 (cond
	    ((not (eq? utype 'unknown))
	     (decl-vtype-set! this utype fix)
	     (return 'void (extend-env env this utype) bk))
	    ((usage? '(eval) usage)
	     (decl-vtype-add! this 'any fix)
	     (return 'void (extend-env env this ty) bk))
	    ((or (eq? ty 'unknown) (not ty))
	     (return 'void env bk))
	    (else
	     (decl-vtype-add! this ty fix)
	     (return 'void (extend-env env this ty) bk))))))

;*---------------------------------------------------------------------*/
;*    constructor-only? ...                                            */
;*    -------------------------------------------------------------    */
;*    This predicates is #t iff the function is only used as a         */
;*    constructor.                                                     */
;*---------------------------------------------------------------------*/
(define (constructor-only?::bool decl::J2SDeclFun)
   (with-access::J2SDeclFun decl (usage)
      (and (usage? '(new) usage)
	   (not (usage? '(ref call eval) usage)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDeclFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDeclFun env::pair-nil fix::cell)

   (define (typing-ctor-only val)
      (with-access::J2SFun val (generator rtype thisp)
	 (when generator
	    (set! rtype 'any))
	 (when (isa? thisp J2SDecl)
	    (with-access::J2SDecl thisp (itype)
	       (decl-itype-add! thisp 'object fix)))))
   
   (with-access::J2SDeclFun this (val ronly)
      (if ronly
	  (decl-vtype-set! this 'function fix)
	  (decl-vtype-add! this 'function fix))
      (cond
	 ((isa? this J2SDeclSvc)
	  ;; services are as escaping function, the arguments are "any"
	  (escape-fun val fix))
	 ((constructor-only? this)
	  ;; a mere constructor
	  (if (isa? val J2SFun)
	      (typing-ctor-only val)
	      (with-access::J2SMethod val (function method)
		 (typing-ctor-only function)
		 (typing-ctor-only method)))))
      (multiple-value-bind (tyf env _)
	 (if (isa? val J2SMethod)
	     (typing val env fix)
	     (typing-fun val (typing-fun-decl val env fix) fix))
	 (return 'void (extend-env env this tyf) '()))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDeclClass ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDeclClass env::pair-nil fix::cell)
   (with-access::J2SDeclClass this (val ronly)
      (if ronly
	  (decl-vtype-set! this 'class fix)
	  (decl-vtype-add! this 'class fix))
      (multiple-value-bind (tyf env bk)
	 (typing val env fix)
	 (return 'class env bk))))

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
	    ;; variable assignment
	    ((isa? lhs J2SRef)
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (writable utype id)
		   (multiple-value-bind (tyr env rbk)
		      (typing rhs envl fix)
		      (cond
			 ((and (not writable) (not (isa? this J2SInit)))
			  (let ((nenv (extend-env env decl tyv)))
			     (expr-type-set! this nenv fix tyv
				(append lbk rbk))))
			 ((not (eq? utype 'unknown))
			  ;; force utype to be in vtype (for instance, for
			  ;; argumentsp)
			  (decl-vtype-add! decl utype fix)
			  (expr-type-set! this env fix utype
			     (append lbk rbk)))
			 (tyr
			  (with-access::J2SRef lhs (decl loc)
			     (decl-vtype-add! decl tyr fix)
			     (let ((nenv (extend-env env decl tyr)))
				(expr-type-set! this nenv fix tyr
				   (append lbk rbk)))))
			 (else
			  (return 'unknown env (append lbk rbk))))))))
	    ((this-assig? lhs)
	     =>
	     (lambda (decl)
		;; "this" property assignment
		(let ((envt (extend-env envl decl 'object)))
		   (multiple-value-bind (tyr nenv rbk)
		      (typing rhs envt fix)
		      (if tyr
			  (expr-type-set! this nenv fix tyr (append lbk rbk))
			  (return 'unknown envt (append lbk rbk)))))))
	    (else
	     ;; a non variable assignment
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
		(with-access::J2SDecl decl (writable utype)
		   (cond
		      ((not writable)
		       (multiple-value-bind (tyv envl lbk)
			  (typing lhs env fix)
			  (let ((nenv (extend-env env decl tyv)))
			     (expr-type-set! this nenv fix tyv
				(append lbk bkr)))))
		      ((not (eq? utype 'unknown))
		       (return utype env bkr))
		      (else
		       (decl-vtype-add! decl tyr fix)
		       (let ((nenv (extend-env envr decl tyr)))
			  (expr-type-set! this nenv fix tyr bkr)))))))
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
		   (with-access::J2SDecl decl (writable utype)
		      (cond
			 ((not writable)
			  (multiple-value-bind (tyv envl lbk)
			     (typing lhs env fix)
			     (let ((nenv (extend-env env decl tyv)))
				(expr-type-set! this nenv fix tyv
				   (append lbk bkr)))))
			 ((not (eq? utype 'unknown))
			  (return utype env bkr))
			 (else
			  (decl-vtype-add! decl tyr fix)
			  (let ((nenv (extend-env envr decl tyr)))
			     (expr-type-set! this nenv fix tyr
				(append lbk bkr))))))))
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
		   (with-access::J2SDecl decl (writable utype)
		      (cond
			 ((not writable)
			  (multiple-value-bind (tyv envl lbk)
			     (typing lhs env fix)
			     (let ((nenv (extend-env env decl tyv)))
				(expr-type-set! this nenv fix tyv
				   (append lbk bkr)))))
			 ((not (eq? utype 'unknown))
			  (return utype env bkr))
			 (else
			  (decl-vtype-add! decl tyr fix)
			  (let ((nenv (extend-env envr decl tyr)))
			     (expr-type-set! this nenv fix tyr
				(append lbk bkr))))))))
	       (else
		;; a non variable assignment
		(expr-type-set! this envr fix tyr bkr)))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDProducer ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDProducer env::pair-nil fix::cell)
   (with-access::J2SDProducer this (expr type)
      (multiple-value-bind (ty env bk)
	 (typing expr env fix)
	 (return type env bk))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SDConsumer ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SDConsumer env::pair-nil fix::cell)
   (with-access::J2SDConsumer this (expr)
      (multiple-value-bind (ty env bk)
	 (typing expr env fix)
	 (expr-type-set! this env fix ty bk))))

;*---------------------------------------------------------------------*/
;*    typing-fun ...                                                   */
;*---------------------------------------------------------------------*/
(define (typing-fun this::J2SFun env::pair-nil fix::cell)
   (with-access::J2SFun this (body thisp params rtype %info vararg argumentsp type)
      (let ((envp (map (lambda (p::J2SDecl)
			  (with-access::J2SDecl p (usage utype itype)
			     (cond
				((not (eq? utype 'unknown))
				 (set! itype utype)
				 (cond
				    ((not (eq? usage 'rest))
				     (decl-vtype-add! p utype fix)
				     (cons p utype))
				    ((eq? utype 'array)
				     (decl-vtype-add! p utype fix)
				     (cons p utype))
				    (else
				     (error "js2scheme"
					"Illegal parameter type"
					p))))
				((eq? usage 'rest)
				 (decl-vtype-add! p 'array fix)
				 (cons p 'array))
				(vararg
				 (decl-vtype-add! p 'any fix)
				 (cons p 'any))
				(else
				 (cons p itype)))))
		     params)))
	 ;; transfer all the itypes to vtypes
	 (for-each (lambda (decl)
		      (with-access::J2SDecl decl (itype)
			 (decl-vtype-add! decl itype fix)))
	    params)
	 (let ((fenv (append envp env)))
	    (when thisp
	       (with-access::J2SDecl thisp (itype loc)
		  (decl-vtype-add! thisp itype fix)
		  (set! fenv (extend-env fenv thisp itype))))
	    (when argumentsp
	       (decl-itype-add! argumentsp 'arguments fix)
	       (decl-vtype-add! argumentsp 'arguments fix)
	       (set! fenv (extend-env fenv argumentsp 'arguments)))
	    (multiple-value-bind (_ envf _)
	       (typing body fenv fix)
	       (set! %info fenv)))
	 (expr-type-set! this env fix 'function))))

;*---------------------------------------------------------------------*/
;*    typing-fun-decl ::J2SFun ...                                     */
;*---------------------------------------------------------------------*/
(define (typing-fun-decl this::J2SFun env::pair-nil fix)
   (with-access::J2SFun this (body rtype decl)
      (when (isa? decl J2SDecl)
	 (decl-vtype-set! decl 'function fix))
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
   
   (define (escape-type rtype)
      (case rtype
	 ((undefined any obj object null) rtype)
	 (else 'any)))

   (with-access::J2SFun val (params rtype thisp)
      (when thisp (decl-vtype-add! thisp 'any fix))
      (set! rtype (tyflow-type (escape-type rtype)))
      (for-each (lambda (p::J2SDecl)
		   (with-access::J2SDecl p (utype itype)
		      (when (eq? utype 'unknown)
			 (decl-itype-add! p 'any fix))))
	 params)))

;*---------------------------------------------------------------------*/
;*    escape-method ...                                                */
;*---------------------------------------------------------------------*/
(define (escape-method fun::J2SMethod fix)
   (with-access::J2SMethod fun (method function)
      (escape-fun function fix)
      (escape-fun method fix)))
				  
;*---------------------------------------------------------------------*/
;*    typing ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SFun env::pair-nil fix::cell)
   (escape-fun this fix)
   (typing-fun this (typing-fun-decl this env fix) fix))

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
   (with-access::J2SCall this ((callee fun) thisarg args)
      (multiple-value-bind (tty env bkt)
	 (if (pair? thisarg)
	     (typing (car thisarg) env fix)
	     (values 'unknown env '()))
	 (multiple-value-bind (rty env bkc)
	    (typing-call callee tty args env fix)
	    (expr-type-set! this env fix rty (append bkt bkc))))))

;*---------------------------------------------------------------------*/
;*    typing-call ...                                                  */
;*---------------------------------------------------------------------*/
(define (typing-call callee tty args env fix)
   
   (define (unknown-call-env env)
      ;; compute a new typing environment where all mutated globals
      ;; and all mutated captured locals are removed
      (filter (lambda (e)
		 (with-access::J2SDecl (car e) (ronly scope %info id)
		    (or ronly
			(and (eq? scope 'local) (eq? %info 'nocapture)))))
	 env))
   
   (define (type-known-call-args callee args env bk)
      (with-access::J2SFun callee (rtype params vararg mode thisp mode)
	 (when thisp
	    (decl-itype-add! thisp
	       (cond
		  ((not (eq? tty 'unknown)) tty)
		  ((eq? mode 'strict) 'object)
		  (else 'undefined))
	       fix))
	 (let loop ((params params)
		    (args args))
	    (when (pair? params)
	       (with-access::J2SDecl (car params) (usage id)
		  (cond
		     (vararg
		      (decl-itype-add! (car params) 'any fix))
		     ((and (null? (cdr params)) (memq 'rest usage))
		      (decl-itype-add! (car params) 'array fix))
		     ((null? args)
		      (decl-itype-add! (car params) 'undefined fix)
		      (loop (cdr params) '()))
		     (else
		      (decl-itype-add! (car params) (j2s-type (car args)) fix)
		      (loop (cdr params) (cdr args)))))))))
   
   (define (type-inline-call callee args env bk)
      ;; type a direct function call: ((function (...) { ... })( ... ))
      ;; side effects are automatically handled when
      ;; typing the function body
      (type-known-call-args callee args env bk)
      (multiple-value-bind (_ envf _)
	 (typing-fun callee env fix)
	 (with-access::J2SFun callee (rtype mode)
	    (return rtype env bk))))
   
   (define (type-known-call ref::J2SRef callee args env bk)
      ;; type a known constant function call: F( ... )
      ;; the new typing environment is a merge of env and the environment
      ;; produced by the function
      (with-access::J2SRef ref (decl)
	 (expr-type-set! ref env fix 'function)
	 (type-known-call-args callee args env bk)
	 (with-access::J2SDecl decl (scope usage)
	    (with-access::J2SFun callee (rtype %info)
;* 	       (let ((nenv (if (env? %info) (env-override env %info) env))) */
	       (let ((nenv env))
		  (return rtype nenv bk))))))
   
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
   
   (define (Array? obj)
      (is-global? obj 'Array))
   
   (define (type-method-call callee args env bk)
      ;; type a method call: O.m( ... )
      (multiple-value-bind (_ env bk)
	 (typing callee env fix)
	 (with-access::J2SAccess callee (obj field)
	    (let* ((fn (j2s-field-name field))
		   (ty (if (string? fn)
			   (tyflow-type (car (builtin-method-type obj fn)))
			   'any)))
	       (if (eq? ty 'any)
		   ;; the method is unknown, filter out the typing env
		   (return ty (unknown-call-env env) bk)
		   (return ty env bk))))))
   
   (define (type-hop-call callee args env bk)
      ;; type a hop (foreign function) call: H( ... )
      ;; hop calls have no effect on the typing env
      (with-access::J2SHopRef callee (rtype loc)
	 (return rtype env bk)))
   
   (define (type-global-call callee args env bk)
      (typing callee env fix)
      (cond
	 ((Array? callee)
	  (return 'array (unknown-call-env env) bk))
	 (else
	  (type-unknown-call callee env bk))))
   
   (define (type-unknown-call callee env bk)
      ;; type a unknown function call: expr( ... )
      ;; filter out the typing env
      (multiple-value-bind (_ env bk)
	 (typing callee env fix)
	 (return 'any (unknown-call-env env) bk)))

   (multiple-value-bind (env bk)
      (typing-args args env fix)
      (cond
	 ((isa? callee J2SFun) (type-inline-call callee args env bk))
	 ((isa? callee J2SRef) (type-ref-call callee args env bk))
	 ((isa? callee J2SHopRef) (type-hop-call callee args env bk))
	 ((isa? callee J2SAccess) (type-method-call callee args env bk))
	 ((isa? callee J2SGlobalRef) (type-global-call callee args env bk))
	 (else (type-unknown-call callee env bk)))))
   
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

   (with-access::J2SNew this (clazz args loc)
      (multiple-value-bind (_ env bk)
	 (typing-call clazz 'object args env fix)
	 (expr-type-set! this env fix (class-type clazz) bk))))

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
      (let ((args (append-map (lambda (init)
				 (cond
				    ((isa? init J2SDataPropertyInit)
				     (with-access::J2SDataPropertyInit init (name val)
					(list name val)))
				    ((isa? init J2SAccessorPropertyInit)
				     (with-access::J2SAccessorPropertyInit init (name get set)
					(list name get set)))))
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
	 (let* ((tnum (if (eq? ty 'real) 'real 'number))
		(tye (case op
			((+) (if (non-zero-integer? ty expr) 'integer tnum))
			((-) (if (non-zero-integer? ty expr) 'integer tnum))
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
			   ((and (eq? typl 'real) (eq? typr 'real))
			    'real)
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
			   ((or (eq? typl 'real) (eq? typr 'real))
			    'real)
			   ((and (type-integer? typl) (type-integer? typr))
			    'integer)
			   ((or (eq? typl 'unknown) (eq? typr 'unknown))
			    'unknown)
			   (else
			    'number)))
		       ((% /)
			(cond
			   ((or (eq? typl 'real) (eq? typr 'real))
			    'real)
			   ((or (eq? typl 'unknown) (eq? typr 'unknown))
			    'unknown)
			   ((or (eq? typl 'any) (eq? typr 'any))
			    'any)
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
			(cond
			   ((or (eq? typr 'any) (eq? typl 'any) )
			    'any)
			   ((or (eq? typr 'unknown) (eq? typl 'unknown) )
			    'unknown)
			   (else
			    (merge-types typr typl))))
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
   
   (with-access::J2SAccess this (obj field)
      (multiple-value-bind (tyo envo bko)
	 (typing obj env fix)
	 (multiple-value-bind (tyf envf bkf)
	    (typing field envo fix)
	    (cond
	       ((and (memq tyo '(array string arguments)) (j2s-field-length? field))
		(with-access::J2SString field (val)
		   (expr-type-set! this envf fix 'integer (append bko bkf))))
	       ((not (j2s-field-name field))
		(expr-type-set! this envf fix 'any (append bko bkf)))
	       ((eq? tyo 'string)
		(let* ((fn (j2s-field-name field))
		       (ty (if (eq? (car (string-method-type fn)) 'any)
			       'any 'function)))
		   (expr-type-set! this envf fix ty (append bko bkf))))
	       ((eq? tyo 'regexp)
		(let* ((fn (j2s-field-name field))
		       (ty (if (eq? (car (regexp-method-type fn)) 'any)
			       'any 'function)))
		   (expr-type-set! this envf fix ty (append bko bkf))))
	       ((eq? tyo 'number)
		(let* ((fn (j2s-field-name field))
		       (ty (if (eq? (car (number-method-type fn)) 'any)
			       'any 'function)))
		   (expr-type-set! this envf fix ty (append bko bkf))))
	       ((eq? tyo 'array)
		(let* ((fn (j2s-field-name field))
		       (ty (if (eq? (car (array-method-type fn)) 'any)
			       'any 'function)))
		   (expr-type-set! this envf fix ty (append bko bkf))))
	       ((is-number-ref? obj)
		(let ((name (j2s-field-name field)))
		   (if (member name
			  '("POSITIVE_INFINITY" "NEGATIVE_INFINITY"))
		       (expr-type-set! this envf fix 'number
			  (append bko bkf))
		       (expr-type-set! this envf fix 'any
			  (append bko bkf)))))
	       ((eq? tyo 'unknown)
		(expr-type-set! this envf fix 'unknown (append bko bkf)))
	       (else
		(expr-type-set! this envf fix 'any (append bko bkf))))))))

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
	  (if (eq? type 'unknown)
	      (expr-type-set! this env fix 'any)
	      (return type env '()))
	  (multiple-value-bind (_ _ bk)
	     (typing expr env fix)
	     (if (eq? type 'unknown)
		 (expr-type-set! this env fix 'any bk)
		 (return type env '()))))))

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
			  (with-access::J2SDecl d (utype vtype)
			     (if (eq? utype 'unknown)
				 (cons d vtype)
				 (cons d utype))))
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
   (with-access::J2SReturn this (expr from loc)
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
	     (with-access::J2SExpr from (type)
		(let ((tyr (merge-types type tye)))
		   (unless (eq? tyr type)
		      (unfix! fix (format "J2SReturn(~a) ~a/~a" tye tyr type))
		      (set! type tyr)))
		(values 'void enve (list this))))
	    (else
	     (values 'void enve (list this)))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SWith ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SWith env::pair-nil fix::cell)
   (with-access::J2SWith this (obj block)
      (multiple-value-bind (tye enve bke)
	 (typing obj env fix)
	 (multiple-value-bind (tyb envb bkb)
	    (typing block enve fix)
	    (return 'void envb (append bke bkb))))))
   
;*---------------------------------------------------------------------*/
;*    typing ::J2SReturnYield ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SReturnYield env::pair-nil fix::cell)
   (with-access::J2SReturnYield this (expr kont)
      (typing kont env fix)
      (multiple-value-bind (tye enve bke)
	 (typing expr env fix)
	 (values 'void enve (list this)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SKont ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SKont env::pair-nil fix::cell)
   (with-access::J2SKont this (body exn param)
      (with-access::J2SDecl param (%info itype utype)
	 (cond
	    ((not (eq? utype 'unknown))
	     (decl-vtype-add! param utype fix))
	    ((not (eq? itype 'unknown))
	     (decl-vtype-add! param itype fix))))
      (with-access::J2SDecl exn (%info itype utype)
	 (cond
	    ((not (eq? utype 'unknown))
	     (decl-vtype-add! exn utype fix))
	    ((not (eq? itype 'unknown))
	     (decl-vtype-add! exn itype fix))))
      (typing body env fix)
      (expr-type-set! this env fix 'procedure)))

;*---------------------------------------------------------------------*/
;*    typing ::J2SIf ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SIf env::pair-nil fix::cell)
   
   (define (isa-and? test)
      (when (isa? test J2SBinary)
	 (with-access::J2SBinary test (op)
	    (eq? op '&&))))
   
   (define (typing-one-test test envt enve)
      (multiple-value-bind (op decl typ ref)
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
		   (return typ (env-merge env envb) (filter-breaks bk this))
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
		   (return typ (env-merge env envb) (filter-breaks bk this))
		   (loop (env-merge env envb))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SForIn ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SForIn env::pair-nil fix::cell)
   (with-access::J2SForIn this (lhs obj body op)
      (let ((decl (if (isa? lhs J2SRef)
		      (with-access::J2SRef lhs (decl) decl)
		      (with-access::J2SGlobalRef lhs (decl) decl)))
	    (ty (if (eq? op 'in) 'string 'any)))
	 (decl-vtype-add! decl ty fix)
	 (let loop ((env (extend-env env decl ty)))
	    (let ((ofix (cell-ref fix)))
	       (multiple-value-bind (typ envb bk)
		  (typing-seq (list obj body) env fix)
		  (if (=fx ofix (cell-ref fix))
		      (return typ envb (filter-breaks bk this))
		      (loop (env-merge env envb)))))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2STry ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2STry env::pair-nil fix::cell)
   (with-access::J2STry this (body catch finally)
      (multiple-value-bind (_ envb bkb)
	 (typing body env fix)
	 (multiple-value-bind (_ envc bkh)
	    (typing catch env fix)
	    (multiple-value-bind (_ envf bkf)
	       (typing finally (env-merge envb envc) fix)
	       (return 'void envf (append bkb bkh bkf)))))))

;*---------------------------------------------------------------------*/
;*    typing ::J2SCatch ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (typing this::J2SCatch env::pair-nil fix::cell)
   (with-access::J2SCatch this (body param)
      (decl-vtype-add! param 'any fix)
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
   (with-access::J2SClass this (expr decl)
      (call-default-walker)
      (when decl (decl-vtype-add! decl 'function fix))
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
	  (and (eq? typ 'number) (memq type '(integer index real)))
	  (and (eq? type 'bool))))
   
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
		 ;; no reduction can be applied if val is false, see
		 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.7.3
		 (with-access::J2SBool rhs (val)
		    (if val lhs this)))
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
	 ((and (eq? typ 'integer) (memq tyr '(number integer))) #unspecified)
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
;*    force-type ...                                                   */
;*---------------------------------------------------------------------*/
(define (force-type::bool this::J2SNode from to)
   (let ((cell (make-cell #f)))
      (force-type! this from to cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    force-type! ...                                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SNode from to cell::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SExpr from to cell)
   (with-access::J2SExpr this (type loc)
      (when (and (eq? type from) (not (eq? type to)))
	 (when (and (eq? from 'unknown) debug-tyflow)
	    (tprint "*** COMPILER WARNING : unpexected `unknown' type " loc
	       " " (j2s->list this)))
	 (cell-set! cell #t)
	 (set! type to)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SFun from to cell)
   (with-access::J2SFun this (rtype thisp loc type)
      (when (isa? thisp J2SNode) (force-type! thisp from to cell))
      (when (and (eq? type from) (not (eq? type to)))
	 (when (and (eq? from 'unknown) debug-tyflow)
	    (tprint "*** COMPILER WARNING : unpexected `unknown' type " loc
	       " " (j2s->list this)))
	 (cell-set! cell #t)
	 (set! type to))
      (when (and (eq? rtype from) (not (eq? rtype to)))
	 (when (and (eq? from 'unknown) debug-tyflow)
	    (tprint "*** COMPILER WARNING : unpexected `unknown' type " loc
	       " " (j2s->list this)))
	 (cell-set! cell #t)
	 (set! rtype to)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SDecl ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SDecl from to cell)
   (with-access::J2SDecl this (vtype loc)
      (when (and (eq? vtype from) (not (eq? vtype to)))
	 (when (and (eq? from 'unknown) debug-tyflow)
	    (tprint "*** COMPILER WARNING : unpexected `unknown' type " loc
	       " " (j2s->list this)))
	 (cell-set! cell #t)
	 (set! vtype to)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SDeclInit ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SDeclInit from to cell)
   (with-access::J2SDeclInit this (vtype loc val)
      (when (and (eq? vtype from) (not (eq? vtype to)))
	 (when (and (eq? from 'unknown) debug-tyflow)
	    (tprint "*** COMPILER WARNING : unpexected `unknown' type " loc
	       " " (j2s->list this)))
	 (cell-set! cell #t)
	 (set! vtype to))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SCatch ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SCatch from to cell)
   (with-access::J2SCatch this (param)
      (force-type! param from to cell)
      (call-default-walker)))

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
   (with-access::J2SDecl this (vtype utype)
      (when (eq? utype 'unknown)
	 (set! vtype 'unknown)))
   this)

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SDeclFun ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SDeclFun)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SNumber ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SNumber)
   this)
   
;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SExpr)
   (with-access::J2SExpr this (type)
      (set! type 'unknown))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SHopExpr ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SExpr)
   this)

;*---------------------------------------------------------------------*/
;*    force-unary-type! ...                                            */
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

;*---------------------------------------------------------------------*/
;*    cleanup-hint! ...                                                */
;*---------------------------------------------------------------------*/
(define (cleanup-hint! this)
   (cond
      ((isa? this J2SNode)
       (cleanup-hint-node! this))
      ((pair? this)
       (for-each cleanup-hint! this))))

;*---------------------------------------------------------------------*/
;*    cleanup-hint-node! ...                                           */
;*---------------------------------------------------------------------*/
(define (cleanup-hint-node! this::J2SNode)
   (let ((fields (class-all-fields (object-class this))))
      (let loop ((i (-fx (vector-length fields) 1)))
	 (when (>=fx i 0)
	    (let* ((f (vector-ref fields i))
		   (info (class-field-info f)))
	       (cond
		  ((eq? (class-field-name f) 'hint)
		   (let* ((get (class-field-accessor f))
			  (set (class-field-mutator f))
			  (hint (get this)))
		      (when (and (pair? hint) (pair? (assq 'no-string hint)))
			 (let ((c (assq 'string hint)))
			    (set! hint (delete! c hint))
			    (set this hint))))
		   (loop (-fx i 1)))
		  ((and (pair? info) (member "notraverse" info))
		   (loop (-fx i 1)))
		  (else
		   (cleanup-hint! ((class-field-accessor f) this))
		   (loop (-fx i 1)))))))))
