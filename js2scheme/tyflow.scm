;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/tyflow.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 16 06:12:13 2016                          */
;*    Last change :  Thu Jul 15 18:49:35 2021 (serrano)                */
;*    Copyright   :  2016-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    js2scheme type inference                                         */
;*    -------------------------------------------------------------    */
;*    This pass does not assume any type decoration in the AST, not    */
;*    even for literals and constants.                                 */
;*    -------------------------------------------------------------    */
;*    This stage assigns types to variable declarations and variable   */
;*    references. At a declaration site, the types are:                */
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

   (include "ast.sch"
	    "usage.sch")
   
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
;*    utype-compatible? ...                                            */
;*---------------------------------------------------------------------*/
(define (utype-compatible? utype type)
   (or (eq? type 'magic) (type-subtype? type utype)))

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
(define (type-program! this::J2SProgram conf)
   
   (define j2s-verbose (config-get conf :verbose 0))
   
   (with-access::J2SProgram this (headers decls nodes mode)
      (when (>=fx j2s-verbose 3) (display " " (current-error-port)))
      ;; main fix point
      (if (config-get conf :optim-tyflow #f)
	  (let ((ctx (cons mode 0)))
	     (let loop ((i 1))
		(when (>=fx j2s-verbose 3)
		   (fprintf (current-error-port) "~a." i)
		   (flush-output-port (current-error-port)))
		(let ((ofix (cdr ctx)))
		   ;; type all the nodes
		   (node-type-seq (append headers decls nodes) '() ctx 'void)
		   (cond
		      ((not (=fx (cdr ctx) ofix))
		       (loop (+fx i 1)))
		      ((config-get conf :optim-tyflow-resolve #f)
		       ;; type check resolution
		       (j2s-resolve! this conf ctx)
		       (cond
			  ((not (=fx (cdr ctx) ofix))
			   (loop (+fx i 1)))
			  ((config-get conf :optim-hint #f)
			   ;; hint node-type optimization
			   (when (>=fx j2s-verbose 3)
			      (fprintf (current-error-port) "hint"))
			   (let ((dups (j2s-hint! this conf)))
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
				 ((force-type this 'unknown 'any #f)
				  (when (>=fx j2s-verbose 3)
				     (fprintf (current-error-port) "."))
				  (loop (+fx i 1))))))
			  ((force-type this 'unknown 'any #f)
			   (loop (+fx i 1)))))))))
	  (force-type this 'unknown 'any #t))
      (when (config-get conf :optim-hintblock)
	 (when (>=fx (config-get conf :verbose 0) 4)
	    (display " hint-block" (current-error-port)))
	 (j2s-hint-block! this conf))
      (unless (config-get conf :optim-integer)
	 (force-type this 'integer 'number #t))
	 ;;(force-unary-type! this))
      (cleanup-hint! this)
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
(define (unfix! ctx reason)
   (tprint "--- UNFIX (" (cdr ctx) ") reason=" reason)
   (set-cdr! ctx (+fx 1 (cdr ctx))))

(define-macro (unfix! ctx reason)
   `(set-cdr! ,ctx (+fx 1 (cdr ,ctx))))

;*---------------------------------------------------------------------*/
;*    return ...                                                       */
;*---------------------------------------------------------------------*/
(define (return ty env::pair-nil bk::pair-nil)
   (values ty env bk))

;*---------------------------------------------------------------------*/
;*    subtype? ...                                                     */
;*---------------------------------------------------------------------*/
(define (subtype? t1 t2)
   (or (eq? t1 t2)
       (and (memq t1 '(index length indexof)) (memq t2 '(integer number)))
       (and (eq? t1 'integer) (eq? t2 'number))
       (and (eq? t1 'function) (eq? t2 'arrow))))

;*---------------------------------------------------------------------*/
;*    decl-vtype-set! ...                                              */
;*    -------------------------------------------------------------    */
;*    Assign a unique type to a variable declaration.                  */
;*---------------------------------------------------------------------*/
(define (decl-vtype-set! decl::J2SDecl ty::obj ctx::pair)
   (with-access::J2SDecl decl (vtype id loc)
      [assert (ty) (or (eq? vtype 'unknown) (eq? vtype ty))]
      (when (or (eq? vtype 'unknown) (not (eq? vtype ty)))
	 (unfix! ctx (format "J2SDecl.vset(~a, ~a) vtype=~a/~a" id loc
			(type->sexp vtype) (type->sexp ty)))
	 (set! vtype (tyflow-type ty)))))

;*---------------------------------------------------------------------*/
;*    decl-vtype-add! ...                                              */
;*    -------------------------------------------------------------    */
;*    Add a new type to a variable declaration.                        */
;*---------------------------------------------------------------------*/
(define (decl-vtype-add! decl::J2SDecl ty ctx::pair)
   (with-access::J2SDecl decl (vtype id loc)
      (unless (or (eq? ty 'unknown) (subtype? ty vtype) (eq? vtype 'any))
	 (unfix! ctx (format "J2SDecl.vadd(~a, ~a) vtype=~a/~a" id loc
			(type->sexp vtype) (type->sexp ty)))
	 (set! vtype (tyflow-type (merge-types vtype ty))))))

;*---------------------------------------------------------------------*/
;*    decl-itype-add! ...                                              */
;*    -------------------------------------------------------------    */
;*    Add a new initial type to a variable declaration.                */
;*---------------------------------------------------------------------*/
(define (decl-itype-add! decl::J2SDecl ty ctx::pair)
   (with-access::J2SDecl decl (itype id)
      (unless (or (eq? ty 'unknown) (subtype? ty itype) (eq? itype 'any))
	 (unfix! ctx (format "J2SDecl.iadd(~a) itype=~a/~a" id
			(type->sexp itype) (type->sexp ty)))
	 (set! itype (tyflow-type (merge-types itype ty))))))

;*---------------------------------------------------------------------*/
;*    expr-type-add! ...                                               */
;*    -------------------------------------------------------------    */
;*    Set the expression type and if needed update the ctx stamp.      */
;*---------------------------------------------------------------------*/
(define (expr-type-add! this::J2SExpr env::pair-nil ctx::pair ty
	   #!optional (bk '()))
   (with-access::J2SExpr this (type loc)
      (unless (or (eq? ty 'unknown) (eq? type ty))
	 (let ((ntype (merge-types type ty)))
	    (unless (eq? ntype type)
	       (unfix! ctx
		  (format "J2SExpr.add(~a) ~a ~a/~a -> ~a"
		     loc (j2s->list this)
		     (type->sexp ty)
		     (type->sexp type)
		     (type->sexp ntype)))
	       (set! type (tyflow-type ntype)))))
      (return type env bk)))

;*---------------------------------------------------------------------*/
;*    merge-types ...                                                  */
;*---------------------------------------------------------------------*/
(define (merge-types left right)
   (cond
      ((eq? left right) left)
      ((and (eq? left 'arrow) (eq? right 'function)) 'arrow)
      ((and (eq? left 'function) (eq? right 'arrow)) 'arrow)
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
(define (typnum? ty)
   (memq ty '(index length indexof integer real bigint number)))

;*---------------------------------------------------------------------*/
;*    funtype ...                                                      */
;*---------------------------------------------------------------------*/
(define (funtype this::J2SFun)
   (cond
      ((isa? this J2SArrow) 'arrow)
      ((isa? this J2SSvc) 'service)
      (else 'function)))

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
(define (extend-env::pair-nil env::pair-nil decl::J2SDecl ty)
   (if (eq? ty 'unknown)
       env
       (cons (cons decl ty) env)))

;*---------------------------------------------------------------------*/
;*    env-lookup ...                                                   */
;*---------------------------------------------------------------------*/
(define (env-lookup env::pair-nil decl::J2SDecl)
   (let ((c (assq decl env)))
      (if (pair? c)
	  (cdr c)
	  (with-access::J2SDecl decl (vtype) vtype))))

;*---------------------------------------------------------------------*/
;*    env-nocapture ...                                                */
;*---------------------------------------------------------------------*/
(define (env-nocapture env::pair-nil)
   (map (lambda (c)
	   (with-access::J2SDecl (car c) (escape)
	      (if (and escape (decl-usage-has? (car c) '(assig)))
		  (cons (car c) 'any)
		  c)))
      env))
   
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
   (append right
      (filter (lambda (l)
		 (not (assq (car l) right)))
	 left)))

;*---------------------------------------------------------------------*/
;*    node-type-args ...                                               */
;*    -------------------------------------------------------------    */
;*    Type all the arguments and return a new environment. The         */
;*    arguments are typed as left to right sequence.                   */
;*---------------------------------------------------------------------*/
(define (node-type-args::pair-nil args env::pair-nil ctx::pair)
   (let loop ((args args)
	      (env env)
	      (bks '()))
      (if (null? args)
	  (values env bks)
	  (multiple-value-bind (_ enva bk)
	     (node-type (car args) env ctx)
	     (loop (cdr args) enva (append bk bks))))))

;*---------------------------------------------------------------------*/
;*    node-type-seq ...                                                */
;*---------------------------------------------------------------------*/
(define (node-type-seq nodes::pair-nil env::pair-nil ctx::pair initty)
   (let loop ((nodes nodes)
	      (ty initty)
	      (env env)
	      (bks '()))
      (if (null? nodes)
	  (return ty env bks)
	  (multiple-value-bind (tyn envn bk)
	     (node-type (car nodes) env ctx)
	     (if (pair? bk)
		 (multiple-value-bind (tyr envr bkr)
		    (node-type-seq (cdr nodes) envn ctx initty)
		    (return tyr (env-merge envn envr) (append bk bks)))
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
;*    node-type ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SNode env::pair-nil ctx::pair)
   (with-trace 'j2s-tyflow "node-type ::J2SNode"
      (return 'any '() '())))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SMeta ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SMeta env::pair-nil ctx::pair)
   (with-trace 'j2s-tyflow "node-type ::J2SMeta"
      (with-access::J2SMeta this (stmt)
	 (node-type stmt env ctx))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SExpr ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SExpr env::pair-nil ctx::pair)
   (with-trace 'j2s-tyflow "node-type ::J2SExpr"
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SNull ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SNull env::pair-nil ctx::pair)
   (with-trace 'j2s-tyflow "node-type ::J2SNull"
      (expr-type-add! this env ctx 'null)))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SUndefined ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SUndefined env::pair-nil ctx::pair)
   (expr-type-add! this env ctx 'undefined))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SArrayAbsent ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SArrayAbsent env::pair-nil ctx::pair)
   (expr-type-add! this env ctx 'undefined))
   
;*---------------------------------------------------------------------*/
;*    node-type ::J2SNumber ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SNumber env::pair-nil ctx::pair)
   (with-access::J2SNumber this (val type)
      (expr-type-add! this env ctx
	 (if (flonum? val) 'real (if (bignum? val) 'bigint 'integer)))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SBool ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SBool env::pair-nil ctx::pair)
   (expr-type-add! this env ctx 'bool))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SString ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SString env::pair-nil ctx::pair)
   (expr-type-add! this env ctx 'string))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SNativeString ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SNativeString env::pair-nil ctx::pair)
   (expr-type-add! this env ctx 'scmstring))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SRegExp ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SRegExp env::pair-nil ctx::pair)
   (expr-type-add! this env ctx 'regexp))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SLiteralCnst ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SLiteralCnst env::pair-nil ctx::pair)
   (with-access::J2SLiteralCnst this (val)
      (multiple-value-bind (tyv env bk)
	 (node-type val env ctx)
	 (expr-type-add! this env ctx tyv bk))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SCmap ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SCmap env::pair-nil ctx::pair)
   (expr-type-add! this env ctx 'cmap))

;*---------------------------------------------------------------------*/
;*    node-type ::J2STemplate ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2STemplate env::pair-nil ctx::pair)
   (with-access::J2STemplate this (exprs)
      (multiple-value-bind (env bk)
	 (node-type-args exprs env ctx)
	 (expr-type-add! this env ctx 'string bk))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2STilde ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2STilde env::pair-nil ctx::pair)
   (expr-type-add! this env ctx 'tilde))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SArray ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SArray env::pair-nil ctx::pair)
   (with-access::J2SArray this (exprs len isvector type)
      (multiple-value-bind (env bk)
	 (node-type-args exprs env ctx)
	 (expr-type-add! this env ctx type bk))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SSpread ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SSpread env::pair-nil ctx::pair)
   (with-access::J2SSpread this (expr len)
      (multiple-value-bind (tye enve bke)
	 (node-type expr env ctx)
	 (expr-type-add! this enve ctx tye bke))))
   
;*---------------------------------------------------------------------*/
;*    node-type ::J2SPragma ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SPragma env::pair-nil ctx::pair)
   (with-access::J2SPragma this (lang expr type)
      (cond
	 ((eq? lang 'scheme)
	  (if (eq? type 'unknown)
	      (expr-type-add! this env ctx 'any)
	      (return type env env)))
	 ((isa? expr J2SNode)
	  (multiple-value-bind (_ _ bk)
	     (node-type expr env ctx)
	     (if (eq? type 'unknown)
		 (expr-type-add! this env ctx 'any bk)
		 (return type env env))))
	 (else
	  (expr-type-add! this env ctx 'any)))))
	  
;*---------------------------------------------------------------------*/
;*    node-type ::J2SDataPropertyInit ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SDataPropertyInit env::pair-nil ctx::pair)
   (with-access::J2SDataPropertyInit this (val)
      (node-type val env ctx)))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SAccessorPropertyInit ...                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SAccessorPropertyInit env::pair-nil ctx::pair)
   (with-access::J2SAccessorPropertyInit this (get set)
      (call-default-walker)
      (return 'void env '())))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SObjInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SObjInit env::pair-nil ctx::pair)
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
	    (node-type-args args env ctx)
	    (expr-type-add! this env ctx 'object bk)))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SParen ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SParen env::pair-nil ctx::pair)
   (with-access::J2SParen this (expr)
      (multiple-value-bind (tye enve bke)
	 (node-type expr env ctx)
	 (expr-type-add! this enve ctx tye bke))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SSequence ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SSequence env::pair-nil ctx::pair)
   
   (define (node-type* nodes::pair-nil env::pair-nil ctx::pair)
      (let loop ((nodes nodes)
		 (ty 'undefined)
		 (env env)
		 (bks '()))
	 (if (null? nodes)
	     (return ty env bks)
	     (multiple-value-bind (tyn envn bk)
		(node-type (car nodes) env ctx)
		(if (pair? bk)
		    (multiple-value-bind (tyr envr bkr)
		       (node-type-seq (cdr nodes) envn ctx 'unknown)
		       (return tyr (env-merge envn envr) (append bk bk bks)))
		    (loop (cdr nodes) tyn envn (append bk bks)))))))

   (with-trace 'j2s-tyflow "node-type ::J2SSequence"
      (with-access::J2SSequence this (exprs)
	 (multiple-value-bind (tye env bk)
	    (node-type* exprs env ctx)
	    (expr-type-add! this env ctx tye bk)))))
      
;*---------------------------------------------------------------------*/
;*    node-type ::J2SBindExit ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SBindExit env::pair-nil ctx::pair)
   (with-access::J2SBindExit this (stmt type loc)
      (multiple-value-bind (typ env bk)
	 (node-type stmt env ctx)
	 (return type env bk))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SHopRef ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SHopRef env::pair-nil ctx::pair)
   (with-access::J2SHopRef this (type)
      (return type env '())))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SUnresolvedRef ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SUnresolvedRef env::pair-nil ctx::pair)
   (with-access::J2SUnresolvedRef this (id)
      (cond
	 ((memq id '(console))
	  (expr-type-add! this env ctx 'object))
	 ((eq? id 'undefined)
	  (expr-type-add! this env ctx 'undefined))
	 (else
	  (let ((cla (class-of this)))
	     (if (eq? cla 'unknown)
		 (expr-type-add! this env ctx 'any)
		 (expr-type-add! this env ctx 'object)))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SGlobalRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SGlobalRef env::pair-nil ctx::pair)
   (with-access::J2SGlobalRef this (decl id)
      (cond
	 ((eq? id 'undefined)
	  (decl-vtype-add! decl 'undefined ctx)
	  (expr-type-add! this env ctx 'undefined))
	 ((eq? id 'NaN)
	  (decl-vtype-add! decl 'real ctx)
	  (expr-type-add! this env ctx 'real))
	 ((memq id '(Math String Error Regex Date Function Array Promise))
	  (decl-vtype-add! decl 'object ctx)
	  (expr-type-add! this env ctx 'object))
	 ;; MS CARE UTYPE
;* 	 ((not (eq? utype 'unknown))                                   */
;* 	  (decl-vtype-add! decl utype ctx)                             */
;* 	  (expr-type-add! this env ctx utype))                         */
	 ((not (decl-ronly? decl))
	  (multiple-value-bind (tyv env bk)
	     (call-next-method)
	     (decl-vtype-add! decl tyv ctx)
	     (return tyv env bk)))
	 (else
	  (decl-vtype-add! decl 'any ctx)
	  (expr-type-add! this env ctx 'any)))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SRef env::pair-nil ctx::pair)
   (with-access::J2SRef this (decl loc)
      (with-access::J2SDecl decl (id key vtype scope)
	 (let ((nenv env))
	    (when (and (isa? decl J2SDeclFun) (not (constructor-only? decl)))
	       (set! nenv (env-nocapture env))
	       (with-access::J2SDeclFun decl (val id)
		  (if (isa? val J2SMethod)
		      (escape-method val ctx)
		      (escape-fun val ctx #f))))
	    (if (and (memq scope '(%scope global)) (not (eq? vtype 'unknown)))
		(expr-type-add! this nenv ctx vtype)
		(let ((ty (env-lookup env decl)))
		   (expr-type-add! this nenv ctx ty)))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SWithRef ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SWithRef env::pair-nil ctx::pair)
   (with-access::J2SWithRef this (expr)
      (node-type expr '() ctx)
      (expr-type-add! this env ctx 'any)))
   
;*---------------------------------------------------------------------*/
;*    node-type ::J2SDecl ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SDecl env::pair-nil ctx::pair)
   (decl-itype-add! this 'undefined ctx)
   (decl-vtype-add! this 'undefined ctx)
   (return 'void (extend-env env this 'undefined) '()))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SDeclArguments ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SDeclArguments env::pair-nil ctx::pair)
   (decl-itype-add! this 'arguments ctx)
   (decl-vtype-add! this 'arguments ctx)
   (return 'void (extend-env env this 'arguments) '()))
   
;*---------------------------------------------------------------------*/
;*    node-type ::J2SDeclInit ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SDeclInit env::pair-nil ctx::pair)
   (with-access::J2SDeclInit this (val id loc _usage writable utype)
      (multiple-value-bind (ty env bk)
	 (node-type val env ctx)
	 (cond
	    ((and (not (eq? utype 'unknown)) (eq? (car ctx) 'hopscript))
	     (decl-vtype-set! this utype ctx)
	     (let ((tyv (j2s-type val)))
		(unless (utype-compatible? utype tyv)
		   (if (memq tyv '(unknown any object))
		       (set! val (J2SCheck utype val))
		       (utype-error val utype tyv))))
	     (return 'void (extend-env env this utype) bk))
	    ((decl-usage-has? this '(eval))
	     (decl-vtype-add! this 'any ctx)
	     (return 'void (extend-env env this ty) bk))
	    ((or (eq? ty 'unknown) (not ty))
	     (return 'void env bk))
	    ((and (not writable)
		  (not (decl-usage-has? this '(uninit)))
		  (decl-usage-has? this '(assig)))
	     ;; wait for the ::J2SInit expression for assigning a type
	     ;; to this constant
	     (return 'void env bk))
	    (else
	     (decl-vtype-add! this ty ctx)
	     (return 'void (extend-env env this ty) bk))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SDeclFun ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SDeclFun env::pair-nil ctx::pair)

   (define (node-type-optional-args val env ctx)
      (with-access::J2SFun val (params)
	 (for-each (lambda (p)
		      (when (isa? p J2SDeclInit)
			 (node-type p env ctx)))
	    params)))
      
   (define (node-type-ctor-only val)
      (with-access::J2SFun val (generator rtype thisp)
	 (when generator
	    (set! rtype 'any))
	 (when (isa? thisp J2SDecl)
	    (decl-itype-add! thisp 'object ctx))))
   
   (with-access::J2SDeclFun this (val scope id loc)
      (if (isa? val J2SFun)
	  (node-type-optional-args val env ctx)
	  (with-access::J2SMethod val (function method)
	     (node-type-optional-args function env ctx)
	     (node-type-optional-args method env ctx)))
      (let ((ty (if (isa? this J2SDeclSvc) 'service 'function)))
	 (if (decl-ronly? this)
	     (decl-vtype-set! this ty ctx)
	     (decl-vtype-add! this ty ctx)))
      (cond
	 ((or (isa? this J2SDeclSvc) (eq? scope 'export))
	  ;; services and exported function are as escaping functions,
	  ;; the arguments and return types are "any"
	  (if (isa? val J2SFun)
	      (escape-fun val ctx #f)
	      (escape-method val ctx)))
	 ((constructor-only? this)
	  ;; a mere constructor
	  (if (isa? val J2SFun)
	      (node-type-ctor-only val)
	      (with-access::J2SMethod val (function method)
		 (node-type-ctor-only function)
		 (node-type-ctor-only method)))))
      (multiple-value-bind (tyf env _)
	 (if (isa? val J2SMethod)
	     (node-type val env ctx)
	     (node-type-fun val (node-type-fun-decl val env ctx) ctx))
	 (return 'void (extend-env env this tyf) '()))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SDeclClass ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SDeclClass env::pair-nil ctx::pair)

   (define (node-type-class this)
      (with-access::J2SDeclClass this (val)
	 (if (decl-ronly? this)
	     (decl-vtype-set! this 'function ctx)
	     (decl-vtype-add! this 'function ctx))
	 (multiple-value-bind (tyf env bk)
	    (node-type val env ctx)
	    (return 'function env bk))))

   (define (node-type-record this)
      (with-access::J2SDeclClass this (val id)
	 (with-access::J2SRecord val (itype type)
	    (unless (isa? itype J2STypeRecord)
	       (decl-vtype-set! this 'function ctx)
	       (set! itype (instantiate::J2STypeRecord
			     (id id)
			     (clazz val))))
	    (multiple-value-bind (tyf env bk)
	       (node-type val env ctx)
	       (return 'function env bk)))))
   
   (with-access::J2SDeclClass this (val)
      (call-default-walker)
      (if (isa? val J2SRecord)
	  (node-type-record this)
	  (node-type-class this))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SAssig ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SAssig env::pair-nil ctx::pair)
   
   (define (this-assig? lhs)
      (when (isa? lhs J2SAccess)
	 (with-access::J2SAccess lhs (obj)
	    (when (isa? obj J2SThis)
	       (with-access::J2SThis obj (decl)
		  decl)))))
   
   (with-access::J2SAssig this (lhs rhs loc)
      (let loop ((lhs lhs))
	 (multiple-value-bind (tyv envl lbk)
	    (node-type lhs env ctx)
	    (cond
	       ;; variable assignment
	       ((isa? lhs J2SRef)
		(with-access::J2SRef lhs (decl)
		   (with-access::J2SDecl decl (writable id utype)
		      (multiple-value-bind (tyr env rbk)
			 (node-type rhs envl ctx)
			 (cond
			    ((and (not writable) (not (isa? this J2SInit)))
			     (let ((nenv (extend-env env decl tyv)))
				(expr-type-add! this nenv ctx tyv
				   (append lbk rbk))))
			    ((and (not (eq? utype 'unknown))
				  (eq? (car ctx) 'hopscript))
			     (unless (utype-compatible? utype tyr)
				(if (memq tyr '(unknown any object))
				    (set! rhs (J2SCheck utype rhs))
				    (utype-error rhs utype tyv)))
			     (expr-type-add! this env ctx utype
				(append lbk rbk)))
			    (tyr
			     (with-access::J2SRef lhs (decl loc)
				(decl-vtype-add! decl tyr ctx)
				(let ((nenv (extend-env env decl tyr)))
				   (expr-type-add! this nenv ctx tyr
				      (append lbk rbk)))))
			    (else
			     (return 'unknown env (append lbk rbk))))))))
	       ((isa? lhs J2SWithRef)
		(with-access::J2SWithRef lhs (expr)
		   (loop expr)))
	       ((this-assig? lhs)
		=>
		(lambda (decl)
		   ;; "this" property assignment
		   (let ((envt (extend-env envl decl 'object)))
		      (multiple-value-bind (tyr nenv rbk)
			 (node-type rhs envt ctx)
			 (if tyr
			     (expr-type-add! this nenv ctx tyr (append lbk rbk))
			     (return 'unknown envt (append lbk rbk)))))))
	       (else
		;; a non variable assignment
		(multiple-value-bind (tyr nenv rbk)
		   (node-type rhs envl ctx)
		   (if tyr
		       (expr-type-add! this nenv ctx tyr (append lbk rbk))
		       (return 'unknown env (append lbk rbk))))))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SAssigOp ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SAssigOp env::pair-nil ctx::pair)
   (with-access::J2SAssigOp this (lhs rhs op)
      (multiple-value-bind (tyr envr bkr)
	 (node-type-binary op lhs rhs env ctx)
	 (cond
	    ((isa? lhs J2SRef)
	     ;; a variable assignment
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (writable)
		   (cond
		      ((not writable)
		       (multiple-value-bind (tyv envl lbk)
			  (node-type lhs env ctx)
			  (let ((nenv (extend-env env decl tyv)))
			     (expr-type-add! this nenv ctx tyv
				(append lbk bkr)))))
		      (else
		       (decl-vtype-add! decl tyr ctx)
		       (let ((nenv (extend-env envr decl tyr)))
			  (expr-type-add! this nenv ctx tyr bkr)))))))
	    (else
	     ;; a non variable assinment
	     (expr-type-add! this envr ctx tyr bkr))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SPostfix ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SPostfix env::pair-nil ctx::pair)

   (define (numty ty)
      ;; postctx expressions only evaluate as numbers
      (cond
	 ((eq? ty 'unknown) 'unknown)
	 ((type-number? ty) ty)
	 (else 'number)))
   
   (with-access::J2SPostfix this (lhs rhs op type loc)
      (when (eq? (caddr loc) '643)
	 (tprint "------------- postctx env=" (dump-env env)))
      (multiple-value-bind (tyr envr bkr)
	 (node-type rhs env ctx)
	 (multiple-value-bind (tyv __ lbk)
	    (node-type lhs env ctx)
	    (expr-type-add! rhs envr ctx (numty tyr) bkr)
	    (when (eq? (caddr loc) '643)
	       (tprint "------------- postctx lhs=" tyv))
	    (cond
	       ((isa? lhs J2SRef)
		;; a variable assignment
		(with-access::J2SRef lhs (decl)
		   (with-access::J2SDecl decl (writable)
		      (cond
			 ((not writable)
			  (multiple-value-bind (tyv envl lbk)
			     (node-type lhs env ctx)
			     (let ((nenv (extend-env env decl (numty tyv))))
				(expr-type-add! this nenv ctx (numty tyv)
				   (append lbk bkr)))))
			 (else
			  (let* ((ntyr (numty tyr))
				 (nty (if (eq? ntyr 'unknown) (numty tyv) ntyr)))
			     (decl-vtype-add! decl nty ctx)
			     (let ((nenv (extend-env envr decl nty)))
				(expr-type-add! this nenv ctx nty
				   (append lbk bkr)))))))))
	       (else
		;; a non variable assignment
		(expr-type-add! this envr ctx (numty tyr) bkr)))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SPrefix ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SPrefix env::pair-nil ctx::pair)

   (define (numty ty)
      ;; prectx expressions only evaluate as numbers
      (cond
	 ((eq? ty 'unknown) 'unknown)
	 ((type-number? ty) ty)
	 (else 'number)))
   
   (with-access::J2SPrefix this (lhs rhs op)
      (multiple-value-bind (tyr envr bkr)
	 (node-type rhs env ctx)
	 (expr-type-add! rhs envr ctx (numty tyr) bkr)
	 (multiple-value-bind (tyv __ lbk)
	    (node-type lhs env ctx)
	    (cond
	       ((isa? lhs J2SRef)
		;; a variable assignment
		(with-access::J2SRef lhs (decl)
		   (with-access::J2SDecl decl (writable)
		      (cond
			 ((not writable)
			  (multiple-value-bind (tyv envl lbk)
			     (node-type lhs env ctx)
			     (let ((nenv (extend-env env decl (numty tyv))))
				(expr-type-add! this nenv ctx (numty tyv)
				   (append lbk bkr)))))
			 ;; MS CARE UTYPE
;* 			 ((not (eq? utype 'unknown))                   */
;* 			  (return utype env bkr))                      */
			 (else
			  (decl-vtype-add! decl (numty tyr) ctx)
			  (let ((nenv (extend-env envr decl (numty tyr))))
			     (expr-type-add! this nenv ctx (numty tyr)
				(append lbk bkr))))))))
	       (else
		;; a non variable assignment
		(expr-type-add! this envr ctx (numty tyr) bkr)))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SDProducer ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SDProducer env::pair-nil ctx::pair)
   (with-access::J2SDProducer this (expr type)
      (multiple-value-bind (ty env bk)
	 (node-type expr env ctx)
	 (return type env bk))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SDConsumer ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SDConsumer env::pair-nil ctx::pair)
   (with-access::J2SDConsumer this (expr)
      (multiple-value-bind (ty env bk)
	 (node-type expr env ctx)
	 (expr-type-add! this env ctx ty bk))))

;*---------------------------------------------------------------------*/
;*    node-type-fun ...                                                */
;*---------------------------------------------------------------------*/
(define (node-type-fun this::J2SFun env::pair-nil ctx::pair)
   (with-access::J2SFun this (body thisp params %info vararg argumentsp type loc mode rtype rutype mode)
      (let ((envp (map (lambda (p::J2SDecl)
			  (with-access::J2SDecl p (itype utype vtype id)
			     (cond
				((and (not (eq? utype 'unknown)) (eq? mode 'hopscript))
				 (set! itype utype)
				 (decl-vtype-set! p utype ctx)
				 (cons p utype))
				((decl-usage-has? p '(rest))
				 (decl-vtype-add! p 'array ctx)
				 (cons p 'array))
				(vararg
				 (decl-vtype-add! p 'any ctx)
				 (cons p 'any))
				(else
				 (cons p itype)))))
		     params)))
	 ;; transfer all itypes to vtypes
	 (for-each (lambda (decl)
		      (with-access::J2SDecl decl (itype)
			 (decl-vtype-add! decl itype ctx)))
	    params)
	 ;; rutype
	 (unless (or (memq rutype '(unknown any))
		     (not (eq? (car ctx) 'hopscript))
		     (eq? rtype rutype))
	    (set! rtype rutype))
	 (let ((fenv (append envp env)))
	    (when thisp
	       (with-access::J2SDecl thisp (vtype itype loc)
		  ;; MS CARE UTYPE
		  ;; (if (eq? utype 'object)
		  (unless (eq? vtype 'object)
		     (decl-vtype-add! thisp itype ctx))
		  (set! fenv (extend-env fenv thisp itype))))
	    (when argumentsp
	       (decl-itype-add! argumentsp 'arguments ctx)
	       (decl-vtype-add! argumentsp 'arguments ctx)
	       (set! fenv (extend-env fenv argumentsp 'arguments)))
	    (multiple-value-bind (_ envf _)
	       (node-type body fenv (cons mode (cdr ctx)))
	       (set! %info envf)))
	 (expr-type-add! this env ctx (funtype this)))))

;*---------------------------------------------------------------------*/
;*    node-type-fun-decl ::J2SFun ...                                  */
;*---------------------------------------------------------------------*/
(define (node-type-fun-decl this::J2SFun env::pair-nil ctx)
   (with-access::J2SFun this (body decl)
      (when (isa? decl J2SDecl)
	 (decl-vtype-set! decl (funtype this) ctx))
      (filter-map (lambda (c)
		     (let ((d (car c))
			   (t (cdr c)))
			(with-access::J2SDecl d (vtype)
			   (if (decl-ronly? d)
			       c
			       (cons d vtype)))))
	 env)))

;*---------------------------------------------------------------------*/
;*    escape-fun ...                                                   */
;*---------------------------------------------------------------------*/
(define (escape-fun val::J2SFun ctx met::bool)
   
   (define (escape-type rtype)
      (case rtype
	 ((unknown undefined any obj object null) rtype)
	 (else 'any)))

   (with-access::J2SFun val (params rtype rutype thisp name mode)
      (when (and (not met) thisp)
	 (decl-vtype-add! thisp 'any ctx))
      (when (or (eq? rutype 'unknown) (not (eq? mode 'hopscript)))
	 (set! rtype (tyflow-type (escape-type rtype))))
      (for-each (lambda (p::J2SDecl)
		   (with-access::J2SDecl p (itype utype)
		      (when (or (eq? utype 'unknown)
				(not (eq? mode 'hopscript)))
			 (decl-itype-add! p 'any ctx))))
	 params)))

;*---------------------------------------------------------------------*/
;*    escape-method ...                                                */
;*---------------------------------------------------------------------*/
(define (escape-method fun::J2SMethod ctx)
   (with-access::J2SMethod fun (method function)
      (escape-fun function ctx #f)
      (escape-fun method ctx #t)))

;*---------------------------------------------------------------------*/
;*    node-type-fun-or-method ...                                      */
;*---------------------------------------------------------------------*/
(define (node-type-fun-or-method this::J2SFun env::pair-nil ctx::pair met::bool)
   (escape-fun this ctx met)
   (node-type-fun this (node-type-fun-decl this (env-nocapture env) ctx) ctx))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SFun env::pair-nil ctx::pair)
   (node-type-fun-or-method this env ctx #f))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SMethod ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SMethod env::pair-nil ctx::pair)
   (with-access::J2SMethod this (method function)
      (node-type-fun-or-method function env ctx #f)
      (node-type-fun-or-method method env ctx #t)
      (expr-type-add! this env ctx 'function)))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SCall ...                                          */
;*    -------------------------------------------------------------    */
;*    Function calls may affect the node-type environment, depending   */
;*    on the called function. Each case comes with a special rule,     */
;*    detailed in the code below.                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SCall env::pair-nil ctx::pair)
   (with-access::J2SCall this (fun thisarg args protocol type loc)
      (multiple-value-bind (tty env bkt)
	 (if (pair? thisarg)
	     (node-type (car thisarg) env ctx)
	     (values 'unknown env '()))
	 (multiple-value-bind (rty env bkc)
	    (node-type-call fun protocol tty args env ctx)
	    (expr-type-add! this env ctx rty (append bkt bkc))))))

;*---------------------------------------------------------------------*/
;*    node-type-call ...                                               */
;*---------------------------------------------------------------------*/
(define (node-type-call callee protocol tty args env ctx)

   (define (unknown-call-env env)
      ;; compute a new node-type environment where all mutated globals
      ;; and all mutated captured locals are removed
      (filter (lambda (e)
		 (with-access::J2SDecl (car e) (writable scope escape id)
		    (or (decl-ronly? (car e))
			(not writable)
			(and (memq scope '(local letblock inner))
			     (not escape)))))
	 env))
   
   (define (global-call-env env)
      ;; compute a new node-type environment where all mutated globals
      ;; and all mutated captured locals are removed
      (filter (lambda (e)
		 (with-access::J2SDecl (car e) (writable scope escape id)
		    (or (decl-ronly? (car e))
			(not writable)
			(or (memq scope '(local letblock inner))))))
	 env))

   (define (local-call-env env)
      (unknown-call-env env))
      
   (define (type-known-call-args fun::J2SFun args env bk)
      (with-access::J2SFun fun (rtype params vararg mode thisp mode)
	 (when thisp
	    (decl-itype-add! thisp
	       (cond
		  ((not (eq? tty 'unknown)) tty)
		  ((eq? mode 'strict) 'object)
		  (else 'undefined))
	       ctx))
	 (let loop ((params params)
		    (args args))
	    (when (pair? params)
	       (with-access::J2SDecl (car params) (id utype)
		  (cond
		     (vararg
		      (decl-itype-add! (car params) 'any ctx))
		     ((and (null? (cdr params)) (decl-usage-has? (car params) '(rest)))
		      (decl-itype-add! (car params) 'array ctx))
		     ((null? args)
		      (unless (or (eq? utype 'unknown) (not (eq? mode 'hopscript)))
			 (unless (utype-compatible? utype 'undefined)
			     (utype-error (car params) utype 'undefined)))
		      (decl-itype-add! (car params) 'undefined ctx)
		      (loop (cdr params) '()))
		     ((and (not (eq? utype 'unknown))
			   (eq? mode 'hopscript))
		      (let ((ty (j2s-type (car args))))
			 (cond
			    ((utype-compatible? utype ty)
			     (decl-itype-add! (car params) ty ctx)
			     (loop (cdr params) (cdr args)))
			    ((memq ty '(unknown any object))
			     (with-access::J2SExpr (car args) (loc)
				(set-car! args (J2SCheck utype (car args)))
				(decl-itype-add! (car params) utype ctx)
				(loop (cdr params) (cdr args))))
			    (else
			     (utype-error (car args) utype ty)))))
		     (else
		      (decl-itype-add! (car params) (j2s-type (car args)) ctx)
		      (loop (cdr params) (cdr args)))))))))
   
   (define (type-inline-call fun::J2SFun args env bk)
      ;; type a direct function call: ((function (...) { ... })( ... ))
      ;; side effects are automatically handled when
      ;; node-type the function body
      (type-known-call-args fun args env bk)
      (multiple-value-bind (_ envf _)
	 (node-type-fun callee env ctx)
	 (with-access::J2SFun fun (rtype mode %info)
	    (let* ((oenv (if (env? %info) (env-override env %info) env))
		   (nenv (local-call-env oenv)))
	       (return rtype nenv bk)))))
   
   (define (type-known-call ref::J2SRef fun::J2SFun args env bk)
      ;; type a known constant function call: F( ... )
      ;; the new node-type environment is a merge of env and the environment
      ;; produced by the function
      (with-access::J2SRef ref (decl)
	 (expr-type-add! ref env ctx (funtype fun))
	 (type-known-call-args fun args env bk)
	 (with-access::J2SDecl decl (scope id)
	    (with-access::J2SFun fun (rtype %info)
	       (let* ((oenv (if (env? %info) (env-override env %info) env))
		      (nenv (if (memq scope '(global %scope))
				(global-call-env oenv)
				(local-call-env oenv))))
		  (return rtype nenv bk))))))

   (define (type-ref-call callee args env bk)
      ;; call a JS variable, check is it a known function
      (with-access::J2SRef callee (decl)
	 (cond
	    ((isa? decl J2SDeclFun)
	     (with-access::J2SDeclFun decl (val)
		(if (decl-ronly? decl)
		    (if (isa? val J2SMethod)
			(with-access::J2SMethod val (function method)
			   (if (eq? tty 'object)
			       (type-known-call callee method args env bk)
			       (type-known-call callee function args env bk)))
			(type-known-call callee val args env bk))
		    (type-unknown-call callee env bk))))
	    ((isa? decl J2SDeclInit)
	     (with-access::J2SDeclInit decl (val)
		(cond
		   ((is-builtin-ref? callee 'Array)
		    (return 'array env bk))
		   ((and (decl-ronly? decl) (isa? val J2SFun))
		    (type-known-call callee val args env bk))
		   ((and (decl-ronly? decl) (isa? val J2SMethod))
		    (with-access::J2SMethod val (function method)
		       (if (eq? tty 'object)
			   (type-known-call callee method args env bk)
			   (type-known-call callee function args env bk))))
		   (else
		    (type-unknown-call callee env bk)))))
	    (else
	     (type-unknown-call callee env bk)))))
   
   (define (is-global? obj ident)
      (cond
	 ((isa? obj J2SGlobalRef)
	  (with-access::J2SGlobalRef obj (id decl)
	     (when (eq? id ident)
		(decl-ronly? decl))))
	 ((isa? obj J2SRef)
	  (with-access::J2SRef obj (decl)
	     (when (isa? decl J2SDeclExtern)
		(with-access::J2SDeclExtern decl (id)
		   (when (eq? id ident)
		      (decl-ronly? decl))))))))
   
   (define (type-method-call callee args env bk)
      ;; type a method call: O.m( ... )
      (multiple-value-bind (_ env bk)
	 (node-type callee env ctx)
	 (with-access::J2SAccess callee (obj field)
	    (let* ((fn (j2s-field-name field))
		   (ty (if (string? fn)
			   (car (find-builtin-method-type obj fn))
			   'any)))
	       (cond
		  ((eq? ty 'any)
		   ;; the method is unknown, filter out the node-type env
		   (return ty (unknown-call-env env) bk))
		  ((eq? ty 'anumber)
		   (if (pair? args)
		       (let ((aty (j2s-type (car args))))
			  (if (memq aty '(integer real))
			      (return aty env bk)
			      (return 'number env bk)))
		       (return 'number env bk)))
		  (else
		   (return (tyflow-type ty) env bk)))))))
   
   (define (type-hop-call callee args env bk)
      ;; type a hop (foreign function) call: H( ... )
      ;; hop calls have no effect on the node-type env
      (with-access::J2SHopRef callee (rtype loc)
	 (return rtype env bk)))
   
   (define (type-global-call callee args env bk)
      (node-type callee env ctx)
      (cond
	 ((is-global? callee 'Array)
	  (return 'array (unknown-call-env env) bk))
	 ((is-global? callee 'String)
	  (return 'string (unknown-call-env env) bk))
	 ((is-global? callee 'parseInt)
	  (return 'number (unknown-call-env env) bk))
	 ((is-global? callee 'isNaN)
	  (return 'bool (unknown-call-env env) bk))
	 ((is-global? callee 'unescape)
	  (return 'string (unknown-call-env env) bk))
	 ((is-global? callee 'encodeURI)
	  (return 'string (unknown-call-env env) bk))
	 ((is-global? callee 'encodeURIComponent)
	  (return 'string (unknown-call-env env) bk))
	 (else
	  (type-unknown-call callee env bk))))
   
   (define (type-unknown-call callee env bk)
      ;; type a unknown function call: expr( ... )
      ;; filter out the node-type env
      (multiple-value-bind (_ env bk)
	 (node-type callee env ctx)
	 (return 'any (unknown-call-env env) bk)))
   
   (multiple-value-bind (env bk)
      (node-type-args args env ctx)
      (cond
	 ((eq? protocol 'spread) (type-unknown-call callee env bk))
	 ((isa? callee J2SFun) (type-inline-call callee args env bk))
	 ((isa? callee J2SRef) (type-ref-call callee args env bk))
	 ((isa? callee J2SHopRef) (type-hop-call callee args env bk))
	 ((isa? callee J2SAccess) (type-method-call callee args env bk))
	 ((isa? callee J2SGlobalRef) (type-global-call callee args env bk))
	 (else (type-unknown-call callee env bk)))))
   
;*---------------------------------------------------------------------*/
;*    node-type ::J2SCond ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SCond env::pair-nil ctx::pair)
   (with-access::J2SCond this (test then else)
      (multiple-value-bind (tyi env bki)
	 (node-type test env ctx)
	 (multiple-value-bind (tyt envt bkt)
	    (node-type then env ctx)
	    (multiple-value-bind (tye enve bke)
	       (node-type else env ctx)
	       (let ((envc (env-merge envt enve))
		     (typc (merge-types tyt tye))
		     (bk (append bki bkt bke)))
		  (expr-type-add! this envc ctx typc bk)))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SNew ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SNew env::pair-nil ctx::pair)
   
   (define (class-type clazz)
      (cond
	 ((isa? clazz J2SUnresolvedRef)
	  (with-access::J2SUnresolvedRef clazz (id)
	     (case id
		((Array) 'array)
		((Vector) 'jsvector)
		((Date) 'date)
		((RegExp) 'regexp)
		((Int8Array) 'int8array)
		((Uint8Array) 'uint8array)
		(else 'object))))
	 ((isa? clazz J2SRef)
	  (with-access::J2SRef clazz (decl)
	     (cond
		((isa? decl J2SDeclExtern)
		 (with-access::J2SDeclExtern decl (id)
		    (when (decl-ronly? decl)
		       (case id
			  ((Array) 'array)
			  ((Vector) 'jsvector)
			  ((Int8Array) 'int8array)
			  ((Uint8Array) 'uint8array)
			  ((Date) 'date)
			  ((RegExp) 'regexp)
			  (else 'object)))))
		((isa? decl J2SDeclClass)
		 (with-access::J2SDeclClass decl (val)
		    (with-access::J2SClass val (itype)
		       itype)))
		(else
		 'object))))
	 (else
	  'object)))
   
   (with-access::J2SNew this (clazz args loc protocol)
      (multiple-value-bind (_ env bk)
	 (node-type clazz env ctx)
	 (multiple-value-bind (_ env bk)
	    (node-type-call clazz protocol 'object args env ctx)
	    (expr-type-add! this env ctx (class-type clazz) bk)))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SUnary ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SUnary env::pair-nil ctx::pair)

   (define (non-zero-integer? ty expr)
      (when (type-integer? ty)
	 (or (not (isa? expr J2SNumber))
	     (with-access::J2SNumber expr (val)
		(not (= val 0))))))

   (with-access::J2SUnary this (op expr)
      (multiple-value-bind (ty env bk)
	 (node-type expr env ctx)
	 (let* ((tnum (if (eq? ty 'real) 'real 'number))
		(tye (case op
			((+) (if (non-zero-integer? ty expr) 'integer tnum))
			((-) (if (non-zero-integer? ty expr) 'integer tnum))
			((~) 'integer)
			((!) 'bool)
			((typeof) 'string)
			(else 'any))))
	    (expr-type-add! this env ctx tye bk)))))

;*---------------------------------------------------------------------*/
;*    node-type-binary ...                                             */
;*---------------------------------------------------------------------*/
(define (node-type-binary op lhs::J2SExpr rhs::J2SExpr env::pair-nil ctx::pair)
   (multiple-value-bind (typl envl bkl)
      (node-type lhs env ctx)
      (multiple-value-bind (typr envr bkr)
	 (node-type rhs envl ctx)
	 (let ((typ (case op
		       ((+)
			(cond
			   ((and (eq? typl 'real) (eq? typr 'real))
			    'real)
			   ((and (eq? typl 'bigint) (eq? typr 'bigint))
			    'bigint)
			   ((and (type-integer? typl) (type-integer? typr))
			    'integer)
			   ((or (and (type-integer? typl) (eq? typr 'bool))
				(and (eq? typl 'bool) (type-integer? typr)))
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
		       ((++)
			(cond
			   ((and (eq? typl 'real) (eq? typr 'real))
			    'real)
			   ((and (eq? typl 'bigint) (eq? typr 'bigint))
			    'bigint)
			   ((and (type-integer? typl) (type-integer? typr))
			    'integer)
			   ((and (typnum? typl) (typnum? typr))
			    'number)
			   ((or (eq? typl 'string) (eq? typr 'string))
			    'number)
			   ((or (eq? typl 'any) (eq? typr 'any))
			    'number)
			   ((or (eq? typl 'unknown) (eq? typr 'unknown))
			    'unknown)
			   (else
			    'unknown)))
		       ((- -- * **)
			(cond
			   ((or (eq? typl 'real) (eq? typr 'real))
			    'real)
			   ((or (eq? typl 'bigint) (eq? typr 'bigint))
			    'bigint)
			   ((and (type-integer? typl) (type-integer? typr))
			    'integer)
			   ((or (eq? typl 'unknown) (eq? typr 'unknown))
			    'unknown)
			   (else
			    'number)))
		       ((/)
			(cond
			   ((or (eq? typl 'real) (eq? typr 'real))
			    'real)
			   ((or (eq? typl 'bigint) (eq? typr 'bigint))
			    'bigint)
			   ((eq? typr 'integer)
			    'number)
			   ((eq? typr 'unknown)
			    'unknown)
			   (else
			    'real)))
		       ((%)
			(cond
			   ((or (eq? typl 'real) (eq? typr 'real))
			    'real)
			   ((or (eq? typl 'bigint) (eq? typr 'bigint))
			    'bigint)
			   ((or (eq? typl 'unknown) (eq? typr 'unknown))
			    'unknown)
			   ((or (eq? typl 'any) (eq? typr 'any))
			    'number)
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
		       ((&& OR OR*)
			(cond
			   ((or (eq? typr 'any) (eq? typl 'any))
			    'any)
			   ((or (eq? typr 'unknown) (eq? typl 'unknown))
			    'unknown)
			   (else
			    (merge-types typr typl))))
		       ((<< >> >>> ^ & BIT_OR)
			'integer)
		       (else
			'any))))
	    (return typ (if (memq op '(OR OR*)) (env-merge envl envr) envr)
	       (append bkl bkr))))))
   
;*---------------------------------------------------------------------*/
;*    node-type ::J2SBinary ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SBinary env::pair-nil ctx::pair)
   (with-access::J2SBinary this (op lhs rhs)
      (multiple-value-bind  (typ env bk)
	 (node-type-binary op lhs rhs env ctx)
	 (expr-type-add! this env ctx typ bk))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SAccess ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SAccess env::pair-nil ctx::pair)

   (define (is-number-ref? expr::J2SNode)
      (when (isa? expr J2SUnresolvedRef)
	 (with-access::J2SUnresolvedRef expr (id)
	    (eq? id 'Number))))

   (define (is-math-ref? expr::J2SNode)
      (when (isa? expr J2SRef)
	 (with-access::J2SRef expr (decl)
	    (when (isa? decl J2SDeclExtern)
	       (with-access::J2SDecl decl (id)
		  (eq? id 'Math))))))
   
   (with-access::J2SAccess this (obj field loc)
      (multiple-value-bind (tyo envo bko)
	 (node-type obj env ctx)
	 (multiple-value-bind (tyf envf bkf)
	    (node-type field envo ctx)
	    (cond
	       ((and (memq tyo '(array string jsvector)) (j2s-field-length? field))
		(with-access::J2SString field (val)
		   (expr-type-add! this envf ctx 'integer (append bko bkf))))
	       ((and (eq? tyo 'arguments)
		     (isa? obj J2SRef)
		     (j2s-field-length? field)
		     (with-access::J2SRef obj (decl)
			;; MS 13may2021: because of the new arguments
			;; optimization, alias arguments are optimized too
			(and ;; (isa? decl J2SDeclArguments)
			     (not (decl-usage-has? decl '(set ref))))))
		;; The length field of a arguments is not necessarily
		;; an number (when assigned a random value, see
		;; S10.6_A5_T4.js test.
		;; arguments.length is known to return and integer only
		;; if arguments is not assigned anything and not passed
		;; to anyone.
		(with-access::J2SString field (val)
		   (expr-type-add! this envf ctx 'integer (append bko bkf))))
	       ((not (j2s-field-name field))
		(expr-type-add! this envf ctx 'any (append bko bkf)))
	       ((eq? tyo 'string)
		(let* ((fn (j2s-field-name field))
		       (ty (if (eq? (car (string-method-type fn)) 'any)
			       'any 'function)))
		   (expr-type-add! this envf ctx ty (append bko bkf))))
	       ((eq? tyo 'regexp)
		(let* ((fn (j2s-field-name field))
		       (ty (if (eq? (car (regexp-method-type fn)) 'any)
			       'any 'function)))
		   (expr-type-add! this envf ctx ty (append bko bkf))))
	       ((eq? tyo 'number)
		(let* ((fn (j2s-field-name field))
		       (ty (if (eq? (car (number-method-type fn)) 'any)
			       'any 'function)))
		   (expr-type-add! this envf ctx ty (append bko bkf))))
	       ((eq? tyo 'array)
		(let* ((fn (j2s-field-name field))
		       (ty (if (eq? (car (array-method-type fn)) 'any)
			       'any 'function)))
		   (expr-type-add! this envf ctx ty (append bko bkf))))
	       ((is-number-ref? obj)
		(let ((name (j2s-field-name field)))
		   (if (member name
			  '("POSITIVE_INFINITY" "NEGATIVE_INFINITY"))
		       (expr-type-add! this envf ctx 'number
			  (append bko bkf))
		       (expr-type-add! this envf ctx 'any
			  (append bko bkf)))))
	       ((is-math-ref? obj)
		(let ((name (j2s-field-name field)))
		   (if (member name
			  '("E" "LN10" "LN2" "LOG2E" "LOG10E" "PI"
			    "SQRT1_2" "SQRT2"))
		       (expr-type-add! this envf ctx 'real
			  (append bko bkf))
		       (expr-type-add! this envf ctx 'any
			  (append bko bkf)))))
	       ((isa? tyo J2STypeRecord)
		(if (isa? field J2SString)
		    (with-access::J2SString field (val)
		       (with-access::J2STypeRecord tyo (clazz)
			  (multiple-value-bind (index el)
			     (j2s-class-get-property clazz val)
			     (if (isa? el J2SClassElement)
				 (with-access::J2SClassElement el (type)
				    (expr-type-add! this envf ctx type (append bko bkf)))
				 (expr-type-add! this envf ctx 'unknown (append bko bkf))))))
		    (expr-type-add! this envf ctx 'any (append bko bkf))))
	       ((eq? tyo 'unknown)
		(expr-type-add! this envf ctx 'unknown (append bko bkf)))
	       (else
		(expr-type-add! this envf ctx 'any (append bko bkf))))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SCacheCheck ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SCacheCheck env::pair-nil ctx::pair)
   (multiple-value-bind (typf envf bkf)
      (call-default-walker)
      (with-access::J2SCacheCheck this (type)
	 (expr-type-add! this envf ctx 'bool bkf))))
   
;*---------------------------------------------------------------------*/
;*    node-type ::J2SCacheUpdate ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SCacheUpdate env::pair-nil ctx::pair)
   (multiple-value-bind (typf envf bkf)
      (call-default-walker)
      (with-access::J2SCacheCheck this (type)
	 (expr-type-add! this envf ctx 'undefined bkf))))
   
;*---------------------------------------------------------------------*/
;*    node-type ::J2SNop ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SNop env::pair-nil ctx::pair)
   (return 'void env '()))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SStmtExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SStmtExpr env::pair-nil ctx::pair)
   (with-trace 'j2s-tyflow "node-type ::J2SStmtExpr"
      (with-access::J2SStmtExpr this (expr loc)
	 (trace-item "loc=" loc)
	 (multiple-value-bind (typ env bk)
	    (node-type expr env ctx)
	    (return typ env bk)))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SSeq ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SSeq env::pair-nil ctx::pair)
   (with-trace 'j2s-tyflow "node-type ::J2SSeq"
      (with-access::J2SSeq this (nodes loc)
	 (trace-item "loc=" loc)
	 (node-type-seq nodes env ctx 'void))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SLabel ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SLabel env::pair-nil ctx::pair)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SLetBlock ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SLetBlock env::pair-nil ctx::pair)
   (with-trace 'j2s-tyflow "node-type ::J2SLetBlock"
      (with-access::J2SLetBlock this (decls nodes loc)
	 (trace-item "loc=" loc)
	 (let ((ienv (filter-map (lambda (d::J2SDecl)
				    (with-access::J2SDecl d (vtype)
				       ;; MS CARE UTYPE
;* 				       (if (eq? utype 'unknown)        */
;* 					   (if (eq? vtype 'unknown)    */
;* 					       #f                      */
;* 					       (cons d vtype))         */
;* 					   (cons d utype))             */
				       (unless (eq? vtype 'unknown)
					  (cons d vtype))))
			decls)))
	    (multiple-value-bind (_ denv bk)
	       (node-type-seq decls (append ienv env) ctx 'void)
	       (multiple-value-bind (typ benv bks)
		  (node-type-seq nodes denv ctx 'void)
		  (let ((nenv (filter (lambda (d)
					 (not (memq (car d) decls)))
				 benv)))
		     (return typ nenv (append bk bks)))))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SWith ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SWith env::pair-nil ctx::pair)
   (with-access::J2SWith this (obj block)
      (multiple-value-bind (tye enve bke)
	 (node-type obj env ctx)
	 (multiple-value-bind (tyb envb bkb)
	    (node-type block enve ctx)
	    (return 'void envb (append bke bkb))))))
   
;*---------------------------------------------------------------------*/
;*    node-type ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SReturn env::pair-nil ctx::pair)
   (with-trace 'j2sreturn "node-type ::J2SReturn"
      (with-access::J2SReturn this (expr from loc)
	 (multiple-value-bind (tye enve bke)
	    (node-type expr env ctx)
	    (cond
	       ((isa? from J2SFun)
		(with-access::J2SFun from (rtype rutype)
		   (when (and (not (eq? rutype 'unknown))
			      (eq? (car ctx) 'hopscript)
			      (not (utype-compatible? rutype tye)))
		      (if (memq tye '(unknown any object))
			  (begin
			     (set! tye rutype)
			     (set! expr (J2SCheck rutype expr)))
			  (utype-error expr rutype tye)))
		   (let ((tyr (merge-types rtype tye)))
		      (unless (eq? tyr rtype)
			 (unfix! ctx
			    (format "J2SReturn(~a) e=~a ~a/~a" loc tye tyr rtype))
			 (set! rtype tyr)))
		   (values 'void enve (list this))))
	       ((isa? from J2SBindExit)
		(with-access::J2SBindExit from (type loc utype)
		   (when (and (not (eq? utype 'unknown))
			      (eq? (car ctx) 'hopscript)
			      (not (utype-compatible? utype tye)))
		      (if (memq tye '(unknown any object))
			  (begin
			     (set! tye utype)
			     (set! expr (J2SCheck utype expr)))
			  (utype-error expr utype tye)))
		   (let ((tyr (merge-types type tye)))
		      (unless (eq? tyr type)
			 (unfix! ctx
			    (format "J2SReturn(~a) e=~a ~a/~a" loc tye tyr type))
			 (set! type tyr))))
		(values 'void enve (list this)))
	       ((isa? from J2SExpr)
		(expr-type-add! from env ctx tye)
		(values 'void enve (list this)))
	       (else
		(values 'void enve (list this))))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SReturnYield ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SReturnYield env::pair-nil ctx::pair)
   (with-access::J2SReturnYield this (expr kont)
      (node-type kont env ctx)
      (multiple-value-bind (tye enve bke)
	 (node-type expr env ctx)
	 (values 'void enve (list this)))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SKont ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SKont env::pair-nil ctx::pair)
   (with-access::J2SKont this (body exn param)
      (with-access::J2SDecl param (%info itype)
	 (cond
	    ;; MS CARE UTYPE
;* 	    ((not (eq? utype 'unknown))                                */
;* 	     (decl-vtype-add! param utype ctx))                        */
	    ((not (eq? itype 'unknown))
	     (decl-vtype-add! param itype ctx))))
      (with-access::J2SDecl exn (%info itype)
	 (cond
	    ;; MS CARE UTYPE
;* 	    ((not (eq? utype 'unknown))                                */
;* 	     (decl-vtype-add! exn utype ctx))                          */
	    ((not (eq? itype 'unknown))
	     (decl-vtype-add! exn itype ctx))))
      (node-type body env ctx)
      (expr-type-add! this env ctx 'procedure)))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SIf ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SIf env::pair-nil ctx::pair)
   
   (define (isa-and? test)
      (when (isa? test J2SBinary)
	 (with-access::J2SBinary test (op lhs rhs)
	    (when (eq? op '&&)
	       (node-type-one-positive-test? lhs)))))

   (define (node-type-one-positive-test? test)
      (cond
	 ((isa? test J2SBinary)
	  (with-access::J2SBinary test (op)
	     (memq op '(== === eq? instanceof))))
	 ((isa? test J2SCall)
	  ;; we assume that the only type predicate are positive tests
	  #t)
	 (else
	  #f)))
   
   (define (node-type-one-test test envt enve)
      (multiple-value-bind (op decl typ ref)
	 (j2s-expr-type-test test)
	 (if (and (symbol? typ) (j2s-known-type typ))
	     (case op
		((== === eq?) (values (extend-env envt decl typ) enve))
		((!= !==) (values envt (extend-env enve decl typ)))
		((instanceof) (values (extend-env envt decl typ) enve))
		((!instanceof) (values envt (extend-env enve decl typ)))
		(else (values envt enve)))
	     (values envt enve))))
   
   (define (node-type-test test envt enve)
      (if (isa-and? test)
	  (with-access::J2SBinary test (lhs rhs)
	     (multiple-value-bind (nenvt nenve)
		(node-type-test lhs envt enve)
		(node-type-test rhs nenvt nenve)))
	  (node-type-one-test test envt enve)))

   (with-trace 'j2s-tyflow "node-type ::J2SIf"
      (with-access::J2SIf this (test then else loc)
	 (trace-item "loc=" loc)
	 (multiple-value-bind (tyi env bki)
	    (node-type test env ctx)
	    (multiple-value-bind (envt enve)
	       (node-type-test test env env)
	       (multiple-value-bind (tyt envt bkt)
		  (node-type then envt ctx)
		  (multiple-value-bind (tye enve bke)
		     (node-type else enve ctx)
		     (let ((bk (append bki bke bkt)))
			(return (merge-types tyt tye)
			   (env-merge envt enve) bk)))))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SSwitch ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SSwitch env::pair-nil ctx::pair)
   (with-access::J2SSwitch this (key cases)
      (multiple-value-bind (typk envk bk)
	 (node-type key env ctx)
	 (let ((bks '())
	       (typ #f))
	    (let loop ((cases cases)
		       (env envk))
	       (when (pair? cases)
		  (multiple-value-bind (t e b)
		     (node-type (car cases) env ctx)
		     (set! bks (append b bks))
		     (set! typ (if (not typ) t (merge-types typ t)))
		     (with-access::J2SCase (car cases) (cascade)
			(if cascade
			    (loop (cdr cases) e)
			    (loop (cdr cases) envk))))))
	    (return typ '() bks)))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SCase ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SCase env::pair-nil ctx::pair)
   (with-access::J2SCase this (expr body cascade)
      (multiple-value-bind (typx envx bk)
	 (node-type expr env ctx)
	 (multiple-value-bind (typb envb bkb)
	    (node-type body envx ctx)
	    (return typb envb (append bkb bk))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SBreak ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SBreak env::pair-nil ctx::pair)
   (return 'void env (list this)))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SWhile ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SWhile env::pair-nil ctx::pair)
   (with-trace 'j2s-tyflow "node-type ::J2SWhile"
      (with-access::J2SWhile this (test body loc)
	 (trace-item "loc=" loc)
	 (let loop ((env env)
		    (i 0))
	    (let ((octx (cdr ctx)))
	       (trace-item "while seq loc=" loc)
	       (multiple-value-bind (typ envb bk)
		  (node-type-seq (list test body) env ctx 'void)
		  (let ((nenv (env-merge env envb)))
		     (if (=fx octx (cdr ctx))
			 (return typ nenv (filter-breaks bk this))
			 (loop nenv (+fx i 1))))))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SDo ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SDo env::pair-nil ctx::pair)
   (with-trace 'j2s-tyflow "node-type ::J2SDo"
      (with-access::J2SDo this (test body loc)
	 (trace-item "loc=" loc)
	 (let loop ((env env))
	    (let ((octx (cdr ctx)))
	       (trace-item "while seq loc=" loc)
	       (multiple-value-bind (typ envb bk)
		  (node-type-seq (list body test) env ctx 'void)
		  (let ((nenv (env-merge env envb)))
		     (if (=fx octx (cdr ctx))
			 (return typ nenv (filter-breaks bk this))
			 (loop nenv)))))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SFor ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SFor env::pair-nil ctx::pair)
   (with-trace 'j2s-tyflow "node-type ::J2SFor"
      (with-access::J2SFor this (init test incr body loc)
	 (trace-item "loc=" loc)
	 (let loop ((env env))
	    (let ((octx (cdr ctx)))
	       (trace-item "for seq loc=" loc)
	       (multiple-value-bind (typ envb bk)
		  (node-type-seq (list init test body incr) env ctx 'void)
		  (let ((nenv (env-merge env envb)))
		     (if (=fx octx (cdr ctx))
			 (return typ nenv (filter-breaks bk this))
			 (loop nenv)))))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SForIn ...                                         */
;*    -------------------------------------------------------------    */
;*    !!! WARNING: After the for..in loop the key variable is          */
;*    undefined if the object contains no property.                    */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SForIn env::pair-nil ctx::pair)
   (with-trace 'j2s-tyflow "node-type ::J2SForIn"
      (with-access::J2SForIn this (lhs obj body op loc)
	 (trace-item "loc=" loc)
	 (let ((decl (if (isa? lhs J2SRef)
			 (with-access::J2SRef lhs (decl) decl)
			 (with-access::J2SGlobalRef lhs (decl) decl)))
	       (ty (if (eq? op 'in) 'string 'any)))
	    (decl-vtype-add! decl ty ctx)
	    (expr-type-add! lhs env ctx ty '())
	    (let loop ((env (extend-env env decl ty)))
	       (let ((octx (cdr ctx)))
		  (trace-item "for seq loc=" loc)
		  (multiple-value-bind (typ envb bk)
		     (node-type-seq (list obj body) env ctx 'void)
		     (cond
			((not (=fx octx (cdr ctx)))
			 (loop (env-merge env envb)))
			((eq? op 'in)
			 (decl-vtype-add! decl 'undefined ctx)
			 (return typ (extend-env envb decl 'any)
			    (filter-breaks bk this)))
			(else
			 (return typ envb
			    (filter-breaks bk this)))))))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2STry ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2STry env::pair-nil ctx::pair)
   (with-access::J2STry this (body catch finally)
      (multiple-value-bind (_ envb bkb)
	 (node-type body env ctx)
	 (multiple-value-bind (_ envc bkh)
	    (node-type catch env ctx)
	    (multiple-value-bind (_ envf bkf)
	       (node-type finally (env-merge envb envc) ctx)
	       (return 'void envf (append bkb bkh bkf)))))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SCatch ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SCatch env::pair-nil ctx::pair)
   (with-access::J2SCatch this (body param)
      (decl-vtype-add! param 'any ctx)
      (node-type body env ctx)))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SThrow ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SThrow env::pair-nil ctx::pair)
   (with-access::J2SThrow this (expr)
      (multiple-value-bind (_ env bk)
	 (node-type expr env ctx)
	 (return 'void env (cons this bk)))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SClass ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SClass env::pair-nil ctx::pair)
   (with-access::J2SClass this (expr decl super elements)
      (for-each (lambda (e)
		   (node-type e env ctx))
	 elements)
      (when decl (decl-vtype-add! decl 'function ctx))
      (multiple-value-bind (tys env bki)
	 (node-type super env ctx)
	 (expr-type-add! this env ctx 'class))))

;*---------------------------------------------------------------------*/
;*    node-type ::J2SClassElement ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (node-type this::J2SClassElement env::pair-nil ctx::pair)
   
   (define (constructor? prop::J2SDataPropertyInit)
      (with-access::J2SDataPropertyInit prop (name)
	 (let loop ((name name))
	    (cond
	       ((isa? name J2SLiteralCnst)
		(with-access::J2SLiteralCnst name (val)
		   (loop val)))
	       ((isa? name J2SLiteralValue)
		(with-access::J2SLiteralValue name (val)
		   (equal? val "constructor")))))))
   
   (with-access::J2SClassElement this (prop type)
      (cond
	 ((constructor? prop)
	  (with-access::J2SDataPropertyInit prop (val)
	     (with-access::J2SFun val (thisp)
		(with-access::J2SDecl thisp (itype vtype eloc)
		   ;; MS CARE UTYPE
		   ;; (unless (eq? utype 'object)
		   (unless (eq? itype 'object)
		      (set! itype 'object)
		      (set! vtype 'any)
		      (unfix! ctx "constructor type"))))))
	 (else
	  (when (eq? type 'unknown)
	     (set! type 'any)
	     (unfix! ctx "class element"))))
      (node-type prop env ctx)
      (return 'void env '())))

;*---------------------------------------------------------------------*/
;*    j2s-resolve! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Resolve statically type checks using type informations           */
;*    computed by the NODE-TYPE method.                                */
;*---------------------------------------------------------------------*/
(define (j2s-resolve! this::J2SProgram args ctx)   
   (with-access::J2SProgram this (headers decls nodes)
      (set! headers (map! (lambda (o) (resolve! o ctx)) headers))
      (set! decls (map! (lambda (o) (resolve! o ctx)) decls))
      (set! nodes (map! (lambda (o) (resolve! o ctx)) nodes))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SNode ctx::pair)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SBinary ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SBinary ctx)

   (define (eq-typeof? type typ)
      (or (eq? type typ)
	  (and (memq type '(date array)) (eq? typ 'object))
	  (and (eq? typ 'number) (memq type '(integer index real bigint)))
	  (and (eq? typ 'function) (memq type '(function arrow)))
	  (and (eq? type 'bool))))
   
   (with-access::J2SBinary this (loc op)
      (case op
	 ((&&)
	  (with-access::J2SBinary this (lhs rhs loc)
	     (set! lhs (resolve! lhs ctx))
	     (set! rhs (resolve! rhs ctx))
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
	     (set! lhs (resolve! lhs ctx))
	     (set! rhs (resolve! rhs ctx))
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
			(unfix! ctx "resolve.J2SBinary")
			(J2SBool #t))
		       ((memq type '(unknown any))
			(call-default-walker))
		       ((and (eq? type 'number) (memq typ '(integer index)))
			(call-default-walker))
		       (else
			(unfix! ctx "resolve.J2SBinary")
			(J2SBool #f)))))
		((!= !==)
		 (with-access::J2SExpr ref (type)
		    (cond
		       ((eq-typeof? type typ)
			(unfix! ctx "resolve.J2SBinary")
			(J2SBool #f))
		       ((memq type '(unknown any))
			(call-default-walker))
		       ((and (eq? type 'number) (memq typ '(integer index)))
			(call-default-walker))
		       (else
			(unfix! ctx "resolve.J2SBinary")
			(J2SBool #t)))))
		((instanceof)
		 (with-access::J2SExpr ref (type)
		    (cond
		       ((or (memq type '(unknown any object)) (eq? typ 'object))
			(call-default-walker))
		       ((eq? type typ)
			(unfix! ctx "resolve.J2SBinary")
			(J2SBool #t))
		       (else
			(unfix! ctx "resolve.J2SBinary")
			(J2SBool #f)))))
		((!instanceof)
		 (with-access::J2SExpr ref (type)
		    (cond
		       ((or (memq type '(unknown any object)) (eq? typ 'object))
			(call-default-walker))
		       ((eq? type typ)
			(unfix! ctx "resolve.J2SBinary")
			(J2SBool #f))
		       (else
			(unfix! ctx "resolve.J2SBinary")
			(J2SBool #t)))))
		(else
		 (call-default-walker))))))))
   
;*---------------------------------------------------------------------*/
;*    resolve! ::J2SIf ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SIf ctx::pair)
   
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
      (set! test (resolve! test ctx))
      (with-access::J2SExpr test (type)
	 (cond
	    ((memq type '(any unknown))
	     (set! then (resolve! then ctx))
	     (set! else (resolve! else ctx))
	     this)
	    ((is-true? test)
	     (unfix! ctx "resolve.J2SIf")
	     (resolve! then ctx))
	    ((is-false? test)
	     (unfix! ctx "resolve.J2SIf")
	     (resolve! else ctx))
	    (else
	     (set! then (resolve! then ctx))
	     (set! else (resolve! else ctx))
	     this)))))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SCall ctx::pair)
   
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
(define (force-type::bool this::J2SNode from to final::bool)
   (let ((cell (make-cell #f)))
      (force-type! this from to cell final)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    force-type! ...                                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SNode from to cell::cell final::bool)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SExpr from to cell final)
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
(define-walk-method (force-type! this::J2SFun from to cell final)
   (with-access::J2SFun this (rtype thisp loc type params)
      (when (isa? thisp J2SNode) (force-type! thisp from to cell final))
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
;*    force-type! ::J2JRef ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SRef from to cell final)
   (with-access::J2SRef this (decl type loc)
      (when (isa? decl J2SDeclArguments)
	 (force-type! decl from to cell final))
      (when (eq? type from)
	 (set! type to)
	 (cell-set! cell #t))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2JHopRef ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SHopRef from to cell final)
   (with-access::J2SHopRef this (type)
      (when (eq? type from)
	 (set! type to)
	 (cell-set! cell #t)))
   this)

;*---------------------------------------------------------------------*/
;*    force-type! ::J2JGlobalRef ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SGlobalRef from to cell final)
   (with-access::J2SGlobalRef this (type loc decl)
      (with-access::J2SDecl decl (vtype)
	 (when (and (eq? vtype from) (not (eq? vtype to)))
	    (set! vtype to)
	    (cell-set! cell #t)))
      (when (and (eq? type from) (not (eq? type to)))
	 (set! type to)
	 (cell-set! cell #t)))
   this)

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SDecl ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SDecl from to cell final)
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
;*    -------------------------------------------------------------    */
;*    It might be situation where the declaration is not uninit        */
;*    but the declaration and the initialization are still split.      */
;*    For instance, it might be that a constant is initialized         */
;*    with an object but declared with the undefined value. This       */
;*    function handles these situation to preserve a well typed        */
;*    ast. It uses two situations:                                     */
;*      1- either it changes the value of the declaration to use       */
;*         a well-type constant                                        */
;*      2- it changes the variable type                                */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SDeclInit from to cell final)

   (define (type->init type val)
      (with-access::J2SExpr val (loc)
	 (case type
	    ((number integer) (J2SNumber/type 'number 0))
	    ((string) (J2SString ""))
	    ((bool) (J2SBool #f))
	    ((null) (J2SNull))
	    ((object) (J2SHopRef/type '%this 'object))
	    (else #f))))

   (define (type-eq? t1 t2)
      (or (eq? t1 t2)
	  (and (eq? t1 'number) (eq? t2 'integer))
	  (and (eq? t1 'integer) (eq? t2 'number))))
	      
   (with-access::J2SDeclInit this (vtype loc val)
      (when (and (eq? vtype from) (not (eq? vtype to)))
	 (when (and (eq? from 'unknown) debug-tyflow)
	    (tprint "*** COMPILER WARNING : unpexected `unknown' type " loc
	       " " (j2s->list this)))
	 (cell-set! cell #t)
	 (set! vtype to))
      (when (and final (not (eq? vtype 'any)) (not (type-eq? vtype (j2s-type val))))
	 (cond
	    ((decl-usage-has? this '(uninit))
	     (error "force-type!" "Declaration inconsistent with init"
		(j2s->list this)))
	    ((type->init vtype val)
	     =>
	     (lambda (v)
		(set! val v)))
	    (else
	     (set! vtype 'any))))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SCatch ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SCatch from to cell final)
   (with-access::J2SCatch this (param)
      (force-type! param from to cell final)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SClass ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SClass from to cell final)
   (with-access::J2SClass this (decl)
      (when decl
	 (force-type! decl from to cell final)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SPostfix ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SPostfix from to cell final)
   (with-access::J2SPostfix this (type)
      (when (eq? type 'unknown)
	 (set! type 'number)
	 (cell-set! cell #t)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-type! ::J2SPrefix ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type! this::J2SPrefix from to cell final)
   (with-access::J2SPrefix this (type)
      (when (eq? type 'unknown)
	 (set! type 'number)
	 (cell-set! cell #t)))
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
   (with-access::J2SDecl this (vtype)
      (when (eq? vtype '(any))
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
;*    force-unary-type! ::J2SNode ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (force-unary-type! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-unary-type! ::J2SUnary ...                                 */
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
(define-walk-method (cleanup-hint! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cleanup-hint! ::J2SDecl ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (cleanup-hint! this::J2SDecl)
   (with-access::J2SDecl this (hint vtype)
      (unless (memq vtype '(number any))
	 (set! hint (filter (lambda (h) (=fx (cdr h) (minvalfx))) hint))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cleanup-hint! ::J2SExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (cleanup-hint! this::J2SExpr)
   (with-access::J2SExpr this (hint type)
      (unless (memq type '(number any))
	 (set! hint (filter (lambda (h) (=fx (cdr h) (minvalfx))) hint))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    utype-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (utype-error this::J2SExpr utype tyv)
   (with-access::J2SExpr this (loc)
      (raise
	 (instantiate::&type-error
	    (proc "hopc")
	    (msg (format "illegal type, \"~a\" expected"
		    (type-name utype '())))
	    (obj (type-name tyv '()))
	    (type (type-name utype '()))
	    (fname (cadr loc))
	    (location (caddr loc))))))
