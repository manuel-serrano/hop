;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x-new-types/js2scheme/hint.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 19 10:13:17 2016                          */
;*    Last change :  Mon Aug 13 07:56:54 2018 (serrano)                */
;*    Copyright   :  2016-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hint typing.                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_type-hint

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_use
	   __js2scheme_alpha)

   (static (class FunHintInfo
	      hinted
	      unhinted
	      types::pair-nil))

   (export (j2s-hint!::pair-nil ::J2SProgram ::obj)
	   (generic j2s-call-hint!::J2SNode ::J2SNode ::bool)
	   (j2s-hint-meta-noopt! ::J2SDecl)
	   (j2s-hint-type::symbol ::symbol)))

;*---------------------------------------------------------------------*/
;*    j2s-hint! ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-hint! prgm conf)
   (with-access::J2SProgram prgm (decls nodes)
      ;; reset previously collected hints
      (for-each j2s-reset-hint decls)
      (for-each j2s-reset-hint nodes)
      ;; first collect all possible hints...
      (for-each (lambda (n) (j2s-hint n '())) decls)
      (for-each (lambda (n) (j2s-hint n '())) nodes)
      ;; then, for each function whose parameters are "hinted", generate
      ;; an ad-hoc typed version
      (if (config-get conf :optim-hint)
	  (let ((dups (append-map (lambda (d) (j2s-hint-function* d conf))
			 decls)))
	     (when (pair? dups)
		(set! decls
		   (append (filter (lambda (dup::J2SDeclFun)
				      (with-access::J2SDeclFun dup (usecnt)
					 (>fx usecnt 0)))
			      dups)
		      decls)))
	     (for-each (lambda (n) (j2s-call-hint! n #f)) decls)
	     (for-each (lambda (n) (j2s-call-hint! n #f)) nodes)
	     (when (config-get conf :optim-hint-loop #f)
		(for-each (lambda (n) (j2s-hint-loop! n #f 0)) decls)
		(for-each (lambda (n) (j2s-hint-loop! n #f 0)) nodes))
	     dups)
	  '())))

;*---------------------------------------------------------------------*/
;*    j2s-hint-type ...                                                */
;*    -------------------------------------------------------------    */
;*    Maps to hint/tyflow understood types.                            */
;*---------------------------------------------------------------------*/
(define (j2s-hint-type ty)
   (cond
      ((memq ty
	  '(integer real number bool string
	    regexp array date arguments function object
	    null undefined void
	    any unknown
	    cmap scmstring tilde pair))
       ty)
      ((memq ty '(index indexof length)) 'integer)
      (else (error "js2scheme" "Illegal hint type" ty))))

;*---------------------------------------------------------------------*/
;*    j2s-reset-hint ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-reset-hint this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-reset-hint ::J2SDecl ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-reset-hint this::J2SDecl)
   (with-access::J2SDecl this (hint)
      (set! hint '())
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    add-hints! ...                                                   */
;*---------------------------------------------------------------------*/
(define (add-hints! decl::J2SDecl hints::pair-nil)
   
   (define (add-hint! decl type::symbol inc)
      (with-access::J2SDecl decl (id hint vtype)
	 (let ((c (assq type hint)))
	    (if (pair? c)
		(set-cdr! c (+fx inc (cdr c)))
		(set! hint (cons (cons type inc) hint))))))
   
   (with-access::J2SDecl decl (id hint itype vtype)
      (when (and (pair? hints)
		 (memq itype '(unknown any number))
		 (memq vtype '(unknown any number)))
	 (for-each (lambda (hi)
		      (let ((ty (car hi))
			    (inc (cdr hi)))
			 (unless (memq ty '(unknown any))
			    (case ty
			       ((string)
				(unless (assq 'no-string hint)
				   (add-hint! decl ty inc)))
			       ((no-string)
				(let ((c (assq 'string hint)))
				   (set! hint (delete! c hint))
				   (add-hint! decl ty inc)))
			       (else
				(add-hint! decl ty inc))))))
	    hints))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SNode hints::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SRef ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SRef hints)
   (with-access::J2SRef this (decl loc type)
      (add-hints! decl hints)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SExpr ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SExpr hints)
   (multiple-value-bind (op decl type ref)
      (j2s-expr-type-test this)
      (if op
	  (add-hints! decl `((,type . 2)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SBinary ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SBinary hints)
   (with-access::J2SBinary this (op lhs rhs)
      (case op
	 ((<< >> >>> ^ & BIT_OR)
	  (j2s-hint rhs  '((integer . 5)))
	  (j2s-hint lhs '((integer . 5))))
	 ((< <= >= > - * /)
	  (case (j2s-type lhs)
	     ((real) (j2s-hint rhs '((real . 5))))
	     ((integer) (j2s-hint rhs '((integer . 3))))
	     (else (j2s-hint rhs '((integer . 2) (real . 2)))))
	  (case (j2s-type rhs)
	     ((real) (j2s-hint rhs '((real . 5))))
	     ((integer) (j2s-hint rhs '((integer . 3))))
	     (else (j2s-hint rhs '((integer . 2) (real . 2))))))
	 ((%)
	  (case (j2s-type lhs)
	     ((real) (j2s-hint rhs '((real . 5))))
	     ((integer) (j2s-hint rhs '((integer . 5))))
	     (else (j2s-hint rhs '((integer . 4)))))
	  (case (j2s-type rhs)
	     ((real) (j2s-hint lhs '((real . 5))))
	     ((integer) (j2s-hint lhs '((integer . 5))))
	     (else (j2s-hint lhs '((integer . 4))))))
	 ((+)
	  (cond
	     ((eq? (j2s-type lhs) 'real)
	      (j2s-hint rhs '((real . 5))))
	     ((eq? (j2s-type rhs) 'real)
	      (j2s-hint rhs '((real . 5))))
	     ((eq? (j2s-type lhs) 'integer)
	      (j2s-hint rhs '((integer . 5) (real . 4))))
	     ((eq? (j2s-type rhs) 'integer)
	      (j2s-hint rhs '((integer . 5) (real . 4))))
	     ((eq? (j2s-type lhs) 'number)
	      (j2s-hint rhs '((integer . 3) (real . 3))))
	     ((eq? (j2s-type rhs) 'number)
	      (j2s-hint rhs '((integer . 3) (real . 3))))
	     ((eq? (j2s-type lhs) 'string)
	      (j2s-hint rhs '((string . 5))))
	     ((eq? (j2s-type rhs) 'string)
	      (j2s-hint lhs '((string . 5))))
	     (else
	      (j2s-hint lhs '((string . 2) (integer . 2) (real . 1)))
	      (j2s-hint rhs '((string . 2) (integer . 2) (real . 1))))))
	 ((== === != !== eq?)
	  (cond
	     ((isa? lhs J2SNull)
	      (j2s-hint rhs '((null . 1))))
	     ((isa? rhs J2SNull)
	      (j2s-hint lhs '((null . 1))))
	     ((isa? lhs J2SUndefined)
	      (j2s-hint rhs '((undefined . 1))))
	     ((isa? rhs J2SUndefined)
	      (j2s-hint lhs '((undefined . 1))))
	     ((eq? (j2s-type lhs) 'integer)
	      (j2s-hint rhs '((integer . 3))))
	     ((eq? (j2s-type rhs) 'integer)
	      (j2s-hint lhs '((integer . 3))))
	     ((eq? (j2s-type lhs) 'real)
	      (j2s-hint rhs '((real . 5))))
	     ((eq? (j2s-type lhs) 'real)
	      (j2s-hint rhs '((real . 5))))
	     ((not (memq (j2s-type lhs) '(any unknown)))
	      (j2s-hint rhs `((j2s-type lhs) . 5)))
	     ((not (memq (j2s-type rhs) '(any unknown)))
	      (j2s-hint lhs `((j2s-type lhs) . 5)))
	     (else
	      (call-default-walker))))
	 ((instanceof)
	  (j2s-hint rhs '((function . 5)))
	  (j2s-hint lhs '((object . 2) (function . 1) (array . 1))))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SSwitch ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SSwitch hints)
   
   (define (cases-hints cases)
      (let ((ty 'unknown))
	 (for-each (lambda (c)
		      (when ty
			 (unless (isa? c J2SDefault)
			    (with-access::J2SCase c (expr)
			       (case (j2s-type expr)
				  ((integer)
				   (case ty
				      ((unknown) (set! ty 'integer))
				      ((integer) #unspecified)
				      (else (set! ty #f))))
				  ((string)
				   (case ty
				      ((unknown) (set! ty 'string))
				      ((integer) #unspecified)
				      (else (set! ty #f)))))))))
	    cases)
	 (if (and ty (not (memq ty '(unknown any))))
	     `((,ty . 5))
	     `((integer . 4) (string . 3)))))
      
   (with-access::J2SSwitch this (key cases)
      (j2s-hint key (cases-hints cases))
      (for-each (lambda (cases)
		   (j2s-hint cases hints))
	 cases)))
	 
;*---------------------------------------------------------------------*/
;*    j2s-string-methods ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-string-methods
   '("charAt" "charCodeAt" "localeCompare" "naturalCompare"
     "match" "replace" "search" "split" "substring" "toLowerCase"
     "toLocaleLowerCase" "toUpperCase" "toLocaleUpperCase"
     "trim" "substr"))

;*---------------------------------------------------------------------*/
;*    j2s-string-array-methods ...                                     */
;*---------------------------------------------------------------------*/
(define j2s-string-array-methods
   '("indexOf" "lastIndexOf" "concat"))

;*---------------------------------------------------------------------*/
;*    j2s-array-methods ...                                            */
;*---------------------------------------------------------------------*/
(define j2s-array-methods
   '("push" "pop" "join" "reverse" "shift" "slice" "sort" "splice" "unshift"
     "every" "some" "forEach" "map" "filter" "find" "reduce" "reduceRight"
     "iterator"))

;*---------------------------------------------------------------------*/
;*    j2s-hint-access ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-hint-access this maybe-string)
   (with-access::J2SAccess this (obj field)
      (let loop ((val field))
	 (cond
	    ((isa? val J2SString)
	     (with-access::J2SString field (val)
		(if (string=? val "length")
		    (if maybe-string
			(j2s-hint obj '((array . 5) (string . 5) (object . 2)))
			(j2s-hint obj '((array . 5) (no-string . 20) (object . 2))))
		    (j2s-hint obj '((object . 5))))))
	    ((isa? field J2SLiteralCnst)
	     (with-access::J2SLiteralCnst field (val)
		(loop val)))
	    (else
	     (j2s-hint field '())
	     (if maybe-string
		 (j2s-hint obj '((object . 5)))
		 (j2s-hint obj '((object . 5) (no-string . 20)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SAccess ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SAccess hints)
   (j2s-hint-access this #t))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SAssig ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SAssig hints)
   (with-access::J2SAssig this (lhs rhs)
      (when (isa? lhs J2SAccess) (j2s-hint-access lhs #f))
      (j2s-hint rhs '())))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SPostfix ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SPostfix hints)
   (with-access::J2SPostfix this (lhs)
      (if (isa? lhs J2SAccess)
	  (j2s-hint-access lhs #f)
	  (j2s-hint lhs '((integer . 5))))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SPrefix ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SPrefix hints)
   (with-access::J2SPrefix this (lhs)
      (if (isa? lhs J2SAccess)
	  (j2s-hint-access lhs #f)
	  (j2s-hint lhs '((integer . 5))))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SNew ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SNew hints)
   (with-access::J2SNew this (clazz args)
      (j2s-hint clazz '((function . 5)))
      (for-each (lambda (a) (j2s-hint a '())) args)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SFun hints)
   (with-access::J2SFun this (body)
      (j2s-hint body hints)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SCall hints)
   
   (define (hint-known-call callee args)
      (with-access::J2SFun callee (params)
	 (let loop ((args args)
		    (params params))
	    (when (and (pair? args) (pair? params))
	       (let ((ty (j2s-type (car args))))
		  (unless (memq ty '(any unknown))
		     (j2s-hint (car args) `((,ty . 2)))))
	       (loop (cdr args) (cdr params))))))
   
   (define (hint-unknown-call callee args)
      (for-each (lambda (a) (j2s-hint a '())) args)
      (j2s-hint callee '((function . 5))))
   
   (define (hint-ref-call callee args)
      (with-access::J2SRef callee (decl)
	 (cond
	    ((isa? decl J2SDeclFun)
	     (with-access::J2SDeclFun decl (ronly val)
		(if ronly
		    (hint-known-call val args)
		    (hint-unknown-call callee args))))
	    ((isa? decl J2SDeclInit)
	     (with-access::J2SDeclInit decl (ronly val)
		(if (and ronly (isa? val J2SFun))
		    (hint-known-call val args)
		    (hint-unknown-call callee args))))
	    (else
	     (hint-unknown-call callee args)))))
   
   (define (hint-access-call callee args)
      (with-access::J2SAccess callee (obj field)
	 (let* ((fn (j2s-field-name field))
		(tys (if (string? fn)
			 (map j2s-hint-type (builtin-method-type obj fn))
			 '(any))))
	    (let loop ((args args)
		       (tys (cdr tys)))
	       (when (and (pair? args) (pair? tys))
		  (j2s-hint (car args) `((,(car tys) . 2)))
		  (loop (cdr args) (cdr tys)))))))
   
   (with-access::J2SCall this (fun args)
      (cond
	 ((isa? fun J2SFun) (hint-known-call fun args))
	 ((isa? fun J2SRef) (hint-ref-call fun args))
	 ((isa? fun J2SAccess) (hint-access-call fun args))
	 ((isa? fun J2SGlobalRef) (hint-unknown-call fun args))
	 (else (hint-unknown-call fun args)))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SFor ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SFor hints)
   (with-access::J2SFor this (init test incr body)
      (j2s-hint init '())
      (j2s-hint test '((bool . 2)))
      (j2s-hint incr '())
      (j2s-hint body '())))
   
;*---------------------------------------------------------------------*/
;*    j2s-hint-function ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-function* this::J2SNode conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint-function ::J2SDeclFun ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-function* this::J2SDeclFun conf)
   
   (define (call-hinted orig idthis params types)
      (with-access::J2SDeclFun this (loc)
	 (instantiate::J2SCall
	    (loc loc)
	    (protocol 'bounce)
	    (fun (instantiate::J2SRef
		    (decl orig)
		    (loc loc)))
	    (thisarg (list (instantiate::J2SHopRef
			      (loc loc)
			      (type 'any)
			      (id idthis))))
	    (args (map (lambda (p::J2SDecl type::symbol)
			  (with-access::J2SDecl p (hint)
			     (instantiate::J2SRef
				(loc loc)
				(type type)
				(decl p))))
		     params types)))))
   
   (define (dispatch-body body pred callt callu fun::J2SFun)
      (with-access::J2SBlock body (loc endloc)
	 (instantiate::J2SBlock
	    (loc loc)
	    (endloc endloc)
	    (nodes (list (instantiate::J2SIf
			    (loc loc)
			    (test pred)
			    (then (instantiate::J2SReturn
				     (from fun)
				     (loc loc)
				     (expr callt)))
			    (else (instantiate::J2SReturn
				     (from fun)
				     (loc loc)
				     (expr callu)))))))))
   
   (define (unparen expr::J2SExpr)
      (if (isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (unparen expr))
	  expr))
   
   (define (typeof? expr::J2SExpr)
      (when (isa? (unparen expr) J2SUnary)
	 (with-access::J2SUnary expr (op)
	    (eq? op 'typeof))))
   
   (define (type-checker? fun::J2SFun)
      
      (define (check-node? node)
	 (when (isa? node J2SReturn)
	    (with-access::J2SReturn node (expr)
	       (when (isa? (unparen expr) J2SBinary)
		  (with-access::J2SBinary (unparen expr) (op lhs rhs)
		     (or (eq? op 'instanceof)
			 (when (memq op '(== != === !== eq?))
			    (or (typeof? lhs) (typeof? rhs)))))))))
      
      (define (block-check-node? node)
	 (when (isa? node J2SBlock)
	    (with-access::J2SBlock node (nodes)
	       (match-case nodes
		  (((? check-node?)) #t)
		  (else #f)))))
      
      (with-access::J2SFun fun (body)
	 (with-access::J2SBlock body (nodes)
	    (match-case nodes
	       (((? profile-node?) (? block-check-node?)) #t)
	       (((? check-node?)) #t)
	       (else #f)))))
   
   (define (param-best-hint-type p::J2SDecl)
      (with-access::J2SDecl p (hint usecnt useinloop vtype id)
	 (if (and (>=fx (length hint) 4)
		  (let* ((w (map cdr hint))
			 (max (apply max w))
			 (min (apply min w)))
		     (<fx (-fx max min) 6)))
	     ;; a megamorphic parameter, don't specialize it
	     (cons 'any 0)
	     (multiple-value-bind (bt bc)
		(best-hint-type p #t)
		(if (or (eq? vtype 'unknown)
			(eq? vtype 'any)
			(and (eq? vtype 'number) (or (assq 'integer hint))))
		    (let ((c (if useinloop (*fx 2 (* bc usecnt)) (* bc usecnt))))
		       (cons bt c))
		    (cons 'any 0))))))
   
   (define (fun-duplicable? decl::J2SDeclFun)
      ;; returns #t iff the function is duplicable, returns #f otherwise
      (with-access::J2SDeclFun this (val id %info hintinfo)
	 (when (isa? val J2SFun)
	    (with-access::J2SFun val (params vararg body)
	       (and (not (isa? %info J2SDecl))
		    (not (isa? hintinfo FunHintInfo))
		    (not (isa? hintinfo J2SDeclFun))
		    (not vararg)
		    (not (isa? val J2SSvc))
		    (any (lambda (p)
			    (with-access::J2SDecl p (vtype itype)
			       ;; at leat one parameter is not precisely typed
			       (and (memq vtype '(unknown number))
				    (memq itype '(unknown number)))))
		       params)
		    (not (type-checker? val)))))))
   
   (define (typed? decl::J2SDeclFun)
      ;; return #t iff the function's arguments are all typed
      (with-access::J2SDeclFun this (val id %info hintinfo)
	 (when (isa? val J2SFun)
	    (with-access::J2SFun val (params vararg)
	       (and (not (isa? %info J2SDecl))
		    (not (isa? hintinfo FunHintInfo))
		    (not vararg)
		    (not (isa? val J2SSvc))
		    (pair? params)
		    (any (lambda (p::J2SDecl)
			    (with-access::J2SDecl p (hint usecnt vtype)
			       (when (>fx usecnt 0)
				  (not (memq vtype '(unknown any))))))
		       params))))))
   
   (define (fun-dispatch! fun::J2SDecl htypes::pair-nil ft vtypes::pair-nil fu)
      (with-access::J2SDeclFun this (val id)
	 (with-access::J2SFun val (params body idthis loc)
	    (let* ((newparams (map j2sdecl-duplicate params vtypes))
		   (pred (test-hint-decls newparams htypes loc))
		   (callt (call-hinted ft idthis newparams htypes))
		   (callu (call-hinted fu idthis newparams vtypes))
		   (disp (dispatch-body body pred callt callu val)))
	       (set! params newparams)
	       (set! body disp)
	       (when (config-get conf :profile-hint)
		  (profile-hint! this id 'dispatch))))))
   
   (define (duplicated? fun::J2SDecl)
      (with-access::J2SDeclFun fun (val %info)
	 (when (isa? %info J2SDeclFun)
	    (with-access::J2SDeclFun %info (hintinfo)
	       (when (isa? hintinfo FunHintInfo)
		  (with-access::FunHintInfo hintinfo (unhinted hinted)
		     (or (eq? hinted fun) (eq? unhinted fun))))))))
   
   (with-access::J2SDeclFun this (val id rtype)
      (let loop ((dup (fun-duplicable? this)))
	 (cond
	    (dup
	     (with-access::J2SFun val (params body)
		(let ((besthints (map (lambda (p)
					 (param-best-hint-type p))
				    params)))
		   (if (<fx (apply max (map cdr besthints)) 10)
		       ;; no benefit in duplicating this function
		       (loop #f)
		       (let ((htypes (map (lambda (bh)
					     (if (>=fx (cdr bh) 3)
						 (car bh)
						 'any))
					besthints)))
			  (if (or (not (memq 'object htypes))
				  (not (self-recursive? this)))
			      ;; only hints non-recursive or non-object functions
			      (let* ((vtypes (map (lambda (p::J2SDecl)
						     (with-access::J2SDecl p (vtype)
							vtype))
						params))
				     (fu (fun-duplicate-untyped this conf))
				     (ft (fun-duplicate-typed this htypes fu conf)))
				 (fun-dispatch! this htypes ft vtypes fu)
				 (list ft fu))
			      (loop #f)))))))
	    ((typed? this)
	     (when (config-get conf :profile-hint #f)
		(unless (profile-hint? this)
		   (profile-hint! this id 'type)))
	     '())
	    ((not (duplicated? this))
	     (when (config-get conf :profile-hint #f)
		(unless (profile-hint? this)
		   (profile-hint! this id 'notype)))
	     '())
	    (else
	     '())))))

;*---------------------------------------------------------------------*/
;*    best-hint-type ...                                               */
;*---------------------------------------------------------------------*/
(define (best-hint-type::symbol decl::J2SDecl normalize)
   
   (define (normalize-hint hint)
      (let loop ((l hint)
		 (r '()))
	 (cond
	    ((null? l)
	     r)
	    ((memq (caar l) '(number integer))
	     (let ((c (assq 'num r)))
		(if (pair? c)
		    (begin
		       (set-cdr! c (+fx (cdr c) (cdar l)))
		       (loop (cdr l) r))
		    (loop (cons (cons 'num (cdar l)) (cdr l)) r))))
	    (else
	     (loop (cdr l) (cons (car l) r))))))
   
   (define (return decl t c)
      (with-access::J2SDecl decl (ronly hint)
	 (cond
	    ((eq? t 'object)
	     (cond
		(ronly (values 'object c))
		((or (assq 'undefined hint) (assq 'null hint)) (values 'any 0))
		(else (values 'object c))))
	    ((not (eq? t 'num)) (values t c))
	    ((assq 'integer hint) (values 'integer c))
	    (else (values 'number c)))))
   
   (with-access::J2SDecl decl (hint)
      (let loop ((l (if normalize (normalize-hint hint) hint))
		 (t 'any)
		 (c 0))
	 (cond
	    ((null? l)
	     (return decl t c))
	    ((>fx (cdar l) c)
	     (loop (cdr l) (caar l) (cdar l)))
	    ((and (=fx (cdar l) c) (eq? t 'string))
	     ;; in doubt, prefer arrays over strings
	     (loop (cdr l) (caar l) (cdar l)))
	    ((and (=fx (cdar l) c) (eq? t 'any))
	     ;; ... and prefer everything over "any"
	     (loop (cdr l) (caar l) (cdar l)))
	    (else
	     (loop (cdr l) t c))))))

;*---------------------------------------------------------------------*/
;*    hint-type-predicate ...                                          */
;*---------------------------------------------------------------------*/
(define (hint-type-predicate::symbol type::symbol)
   (case type
      ((number) 'number?)
      ((integer) 'fixnum?)
      ((string) 'js-jsstring?)
      ((array) 'js-array?)
      ((object) 'js-object?)
      ((function) 'js-function?)
      ((bool) 'boolean?)
      ((undefined) 'js-undefined?)
      ((null) 'js-null?)
      ((regexp) 'js-regexp?)
      (else (error "hint-type-predicate" "Unknown hint type predicate" type))))

;*---------------------------------------------------------------------*/
;*    test-hint-decls ...                                              */
;*---------------------------------------------------------------------*/
(define (test-hint-decls decls::pair htypes::pair loc)
   
   (define (test-hint-decl param htype)
      (with-access::J2SDecl param (loc)
	 (instantiate::J2SCall
	    (loc loc)
	    (fun (instantiate::J2SHopRef
		    (loc loc)
		    (type 'function)
		    (rtype 'bool)
		    (id (hint-type-predicate htype))))
	    (thisarg '())
	    (args (list (instantiate::J2SRef
			   (loc loc)
			   (decl param)))))))

   (let loop ((decls decls)
	      (htypes htypes))
      (let ((decl (car decls))
	    (htype (car htypes)))
	 (with-access::J2SDecl decl (vtype)
	    (cond
	       ((null? (cdr decls))
		(if (memq htype '(unknown any))
		    (instantiate::J2SBool
		       (loc loc)
		       (val #t))
		    (test-hint-decl (car decls) htype)))
	       ((memq htype '(unknown any))
		(loop (cdr decls) (cdr htypes)))
	       (else
		(instantiate::J2SBinary
		   (loc loc)
		   (op '&&)
		   (lhs (test-hint-decl (car decls) htype))
		   (rhs (loop (cdr decls) (cdr htypes))))))))))

;*---------------------------------------------------------------------*/
;*    profile-hint! ...                                                */
;*---------------------------------------------------------------------*/
(define (profile-hint! fun::J2SDeclFun id::symbol attr::symbol)
   
   (define (prof val::J2SFun)
      (with-access::J2SFun val (body loc)
	 (let ((prof (J2SStmtExpr
			(J2SPragma
			   `(profile-hint ,(format "~a" id) ',attr)))))
	    (set! body
	       (duplicate::J2SBlock body
		  (nodes (list prof body)))))))
   
   (with-access::J2SDeclFun fun (val loc)
      (if (isa? val J2SFun)
	  (prof val)
	  (with-access::J2SMethod val (function method)
	     (prof function)
	     (prof method)))))

;*---------------------------------------------------------------------*/
;*    profile-hint? ...                                                */
;*---------------------------------------------------------------------*/
(define (profile-hint? fun::J2SDeclFun)
   (with-access::J2SDeclFun fun (val id %info loc)
      (when (isa? val J2SFun)
	 (with-access::J2SFun val (body)
	    (with-access::J2SBlock body (nodes)
	       (when (pair? nodes)
		  (profile-node? (car nodes))))))))

;*---------------------------------------------------------------------*/
;*    profile-node? ...                                                */
;*---------------------------------------------------------------------*/
(define (profile-node? node)
   (when (isa? node J2SStmtExpr)
      (with-access::J2SStmtExpr node (expr)
	 (when (isa? expr J2SPragma)
	    (with-access::J2SPragma expr (expr)
	       (match-case expr
		  ((profile-hint . ?-) #t)
		  (else #f)))))))

;*---------------------------------------------------------------------*/
;*    fun-duplicate-untyped ...                                        */
;*---------------------------------------------------------------------*/
(define (fun-duplicate-untyped::J2SDeclFun fun::J2SDeclFun conf)
   (with-access::J2SDeclFun fun (val id)
      (with-access::J2SFun val (params body name generator idthis loc)
	 (let ((nfun (duplicate::J2SDeclFun fun
			(parent fun)
			(key (ast-decl-key))
			(id (symbol-append id '%%))
			(ronly #t)
			(writable #f)
			(binder 'let)
			(scope 'none)
			(usecnt 1)
			(utype 'function)
			(%info fun)
			(hintinfo fun)
			(val (duplicate::J2SFun val
				(generator #f)
				(optimize #f)
				(idgen generator)
				(idthis (if (this? body) idthis #f))
				(name (when (symbol? name)
					 (symbol-append name '%%)))
				(params params)
				(body (duplicate::J2SBlock body
					 (nodes (list body)))))))))
	    (with-access::J2SDeclFun nfun ((nval val))
	       (with-access::J2SFun nval (body)
		  ;; force a copy of the three to avoid sharing with the
		  ;; typed version
		  (set! body
		     (return-patch! (j2s-alpha body '() '()) val nval))))
	    (when (config-get conf :profile-hint)
	       (profile-hint! nfun id 'nohint))
	    nfun))))

;*---------------------------------------------------------------------*/
;*    fun-duplicate-typed ...                                          */
;*---------------------------------------------------------------------*/
(define (fun-duplicate-typed::J2SDeclFun fun::J2SDeclFun types unhinted conf)
   
   (define (type-initial t)
      (case t
	 ((integer) #\I)
	 ((number) #\N)
	 ((array) #\V)
	 ((string) #\S)
	 ((unknown) #\X)
	 (else (string-ref (symbol->string t) 0))))
   
   (with-access::J2SDeclFun fun (val id hintinfo)
      (with-access::J2SFun val (params body idthis generator thisp rtype)
	 (let* ((newparams (map j2sdecl-duplicate params types))
		(newthisp (when thisp
			     (with-access::J2SDecl thisp (itype)
				(j2sdecl-duplicate thisp itype))))
		(typeid (string->symbol
			   (string-upcase!
			      (apply string
				 (map type-initial types)))))
		(nbody (if thisp
			   (j2s-alpha body
			      (cons thisp params)
			      (cons newthisp newparams))
			   (j2s-alpha body params newparams)))
		(unbody (reset-type! nbody newparams))
		(newfun (duplicate::J2SFun val
			   (generator #f)
			   (idgen generator)
			   (%info #unspecified)
			   (idthis (if (this? body) idthis #f))
			   (thisp newthisp)
			   (params newparams)
			   (body unbody)))
		(newdecl (duplicate::J2SDeclFun fun
			    (parent fun)
			    (key (ast-decl-key))
			    (id (symbol-append id '%% typeid))
			    (ronly #t)
			    (writable #f)
			    (binder 'let)
			    (scope 'none)
			    (usecnt 1)
			    (%info fun)
			    (utype 'function)
			    (hintinfo fun)
			    (val newfun))))
	    (with-access::J2SFun newfun (body)
	       (set! body (return-patch! body val newfun)))
	    (use-count nbody +1 #f)
	    (with-access::J2SFun newfun (decl)
	       (set! decl newdecl))
	    (when (config-get conf :profile-hint)
	       (profile-hint! newdecl id 'hint))
	    (set! hintinfo
	       (instantiate::FunHintInfo
		  (hinted newdecl)
		  (unhinted unhinted)
		  (types types)))
	    newdecl))))

;*---------------------------------------------------------------------*/
;*    j2sdecl-duplicate ...                                            */
;*---------------------------------------------------------------------*/
(define (j2sdecl-duplicate p::J2SDecl type::symbol)
   (if (isa? p J2SDeclInit)
       (duplicate::J2SDeclInit p
	  (key (ast-decl-key))
	  (hint '())
	  (itype type))
       (duplicate::J2SDecl p
	  (key (ast-decl-key))
	  (hint '())
	  (itype type))))

;*---------------------------------------------------------------------*/
;*    j2s-call-hint! ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-call-hint! this::J2SNode concrete-type::bool)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-call-hint! ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-call-hint! this::J2SCall concrete-type)

   (define (type-number? t)
      (memq t '(integer real number)))
   
   (define (fun-hint-info fun)
      (when (isa? fun J2SRef)
	 (with-access::J2SRef fun (decl)
	    (when (isa? decl J2SDeclFun)
	       (with-access::J2SDeclFun decl (val hintinfo)
		  (cond
		     ((isa? val J2SFun)
		      (with-access::J2SFun val (generator)
			 (unless generator
			    (when (isa? hintinfo FunHintInfo)
			       hintinfo))))
		     ((isa? val J2SMethod)
		      (with-access::J2SMethod val (function)
			 (with-access::J2SFun function (generator)
			    (unless generator
			       (when (isa? hintinfo FunHintInfo)
				  hintinfo)))))))))))
   
   (with-access::J2SCall this (fun args thisarg)
      (set! args (map! (lambda (n) (j2s-call-hint! n concrete-type)) args))
      (set! fun (j2s-call-hint! fun concrete-type))
      (let ((hinfo (fun-hint-info fun)))
	 (if hinfo
	     (with-access::J2SRef fun (decl)
		(with-access::FunHintInfo hinfo (hinted unhinted types)
		   (with-access::J2SDeclFun hinted (val)
		      (with-access::J2SFun val (generator)
			 (cond
			    ((not (=fx (length args) (length types)))
			     (with-access::J2SFun val (idthis)
				(duplicate::J2SCall this
				   (thisarg thisarg)
				   (fun (duplicate::J2SRef fun
					   (type 'function)
					   (decl unhinted))))))
			    ((every (lambda (a t)
				       (or (eq? t 'unknown)
					   (let ((tya (j2s-type a)))
					      (or (and (eq? t 'number)
						       (type-number? tya))
						  (eq? tya t)))))
				args types)
			     (with-access::J2SFun val (idthis)
				;; adjust the usecnt count
				(with-access::J2SDecl hinted (usecnt)
				   (set! usecnt (+fx usecnt 1)))
				(with-access::J2SDecl decl (usecnt)
				   (set! usecnt (-fx usecnt 1)))
				(duplicate::J2SCall this
				   (thisarg thisarg)
				   (fun (duplicate::J2SRef fun
					   (type 'function)
					   (decl hinted))))))
			    ((every (lambda (a t)
				       (or (eq? t 'unknown)
					   (let ((tya (j2s-type a)))
					      (or (memq tya '(any unknown))
						  (eq? tya t)
						  (and (type-number? tya)
						       (type-number? t))))))
				args types)
			     this)
			    (else
			     (with-access::J2SFun val (idthis)
				(duplicate::J2SCall this
				   (thisarg thisarg)
				   (fun (duplicate::J2SRef fun
					   (type 'function)
					   (decl unhinted)))))))))))
	     this))))

;*---------------------------------------------------------------------*/
;*    j2s-hint-loop! ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-loop! this::J2SNode inloop inc)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint-loop! ::J2SLoop ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-loop! this::J2SLoop inloop inc)
   (with-access::J2SLoop this (body)
      (set! body (j2s-hint-loop! body #t (+ 3 inc)))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-hint-loop! ::J2SLetBlock ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-loop! this::J2SLetBlock inloop inc)
   
   (define hint-loop-threshold 0.5)
   
   (define (loop-duplicable? this decls)
      ;; returns #t iff it is worth duplicating this loop
      (any (lambda (p::J2SDecl)
	      (with-access::J2SDecl p (hint usecnt vtype id)
		 (when (pair? hint)
		    (when (or (eq? vtype 'unknown)
			      (eq? vtype 'any)
			      (and (eq? vtype 'number)
				   (assq 'integer hint)))
		       (let ((hintcnt (apply + (map cdr hint))))
			  (> (/ hintcnt usecnt) hint-loop-threshold))))))
	 decls))
   
   (define (dispatch-body this pred then otherwise)
      (with-access::J2SBlock this (loc)
	 (instantiate::J2SIf
	    (loc loc)
	    (test pred)
	    (then then)
	    (else otherwise))))
   
   (define (loop-dispatch this::J2SLetBlock decls::pair-nil htypes::pair-nil)
      (with-access::J2SLetBlock this (nodes loc)
	 (let* ((pred (test-hint-decls decls htypes loc))
		(then (duplicate::J2SBlock this
			 (nodes (map (lambda (n)
					(let ((an (j2s-alpha n '() '())))
					   (reset-type! an decls)))
				   nodes))))
		(otherwise (J2SMeta 0 0
			      (duplicate::J2SBlock this))))
	    (dispatch-body this pred then otherwise))))
   
   (with-access::J2SLetBlock this (decls nodes loc)
      (let ((decls (filter (lambda (d::J2SDecl)
			      (with-access::J2SDecl d (usecnt)
				 (>=fx usecnt 2)))
		      decls)))
	 (if (and inloop (loop-duplicable? this decls))
	     (let ((htypes (map (lambda (p)
				   (multiple-value-bind (bt _)
				      (best-hint-type p #t)
				      bt))
			      decls)))
		(set! nodes (list (loop-dispatch this decls htypes)))
		this)
	     (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SNode decls)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SRef ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SRef decls)
   (with-access::J2SRef this (decl type)
      (when (or (eq? type 'any) (memq decl decls))
	 (set! type 'unknown)))
   this)

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SExpr decls)
   (with-access::J2SExpr this (type)
      (when (eq? type 'any)
	 (set! type 'unknown)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint-meta-noopt! ...                                         */
;*    -------------------------------------------------------------    */
;*    When a function is duplicated and when both hinted and unhinted  */
;*    versions are used, mark the unhinted function as noopt.          */
;*---------------------------------------------------------------------*/
(define (j2s-hint-meta-noopt! d::J2SDecl)
   (when (and (isa? d J2SDeclFun) #f)
      (with-access::J2SDeclFun d (%info id)
	 (when (isa? %info J2SDeclFun)
	    (with-access::J2SDeclFun %info (hintinfo)
	       (when (isa? hintinfo FunHintInfo)
		  (with-access::FunHintInfo hintinfo (unhinted hinted)
		     (when (and unhinted (eq? hinted d))
			;; the two hinted and unhinted function are used
			(with-access::J2SDeclFun unhinted (val)
			   (with-access::J2SFun val (body)
			      (with-access::J2SNode body (loc)
				 (let ((mbody (instantiate::J2SMeta
						 (loc loc)
						 (optim 0)
						 (stmt body))))
				    (set! body
				       (duplicate::J2SBlock body
					  (nodes (list mbody))))))))))))))))

;*---------------------------------------------------------------------*/
;*    return-patch! ::J2SNode ...                                      */
;*    -------------------------------------------------------------    */
;*    After duplicating functions' body, the return nodes must         */
;*    be adjusted so that the FROM fields point to the new function.   */
;*---------------------------------------------------------------------*/
(define-walk-method (return-patch! this::J2SNode old new)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    return-patch! ::J2SReturn ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (return-patch! this::J2SReturn old new)
   (with-access::J2SReturn this (from)
      (when (eq? from old)
	 (set! from new)))
   this)

;*---------------------------------------------------------------------*/
;*    self-recursive? ...                                              */
;*---------------------------------------------------------------------*/
(define (self-recursive? this::J2SDeclFun)
   (with-access::J2SDeclFun this (val)
      (let ((cell (make-cell #f)))
	 (self-recursive val this cell)
	 (cell-ref cell))))

;*---------------------------------------------------------------------*/
;*    self-recursive ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (self-recursive this::J2SNode self::J2SDecl cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    self-recursive ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (self-recursive this::J2SCall self::J2SDecl cell)
   (call-default-walker)
   (with-access::J2SCall this (fun)
      (when (isa? fun J2SRef)
	 (with-access::J2SRef fun (decl)
	    (when (eq? decl self)
	       (cell-set! cell #t))))))

;*---------------------------------------------------------------------*/
;*    decl-duplicated? ...                                             */
;*---------------------------------------------------------------------*/
(define (decl-duplicated? fun::J2SDecl)
   (with-access::J2SDeclFun fun (val %info)
      (when (isa? %info J2SDeclFun)
	 (with-access::J2SDeclFun %info (hintinfo)
	    (when (isa? hintinfo FunHintInfo)
	       (with-access::FunHintInfo hintinfo (unhinted hinted)
		  (or (eq? hinted fun) (eq? unhinted fun))))))))
