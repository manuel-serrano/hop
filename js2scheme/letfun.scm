;*=====================================================================*/
;*    serrano/prgm/project/hop/3.4.x/js2scheme/letfun.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 06:35:14 2015                          */
;*    Last change :  Wed May 19 07:00:30 2021 (serrano)                */
;*    Copyright   :  2015-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Let function optimization. This optimizations implements         */
;*    two transformations: letfun! and letfun-sa! that replaces        */
;*    closure allocations with plain functions.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_letfun

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer
	   __js2scheme_alpha
	   __js2scheme_usage)

   (export j2s-letfun-stage))

;*---------------------------------------------------------------------*/
;*    j2s-letun-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-letfun-stage
   (instantiate::J2SStageProc
      (name "letfun")
      (comment "Implicit function declarations")
      (proc j2s-letfun)
      (optional :optim-letfun)))

;*---------------------------------------------------------------------*/
;*    j2s-letfun ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-letfun this args)
   (when (isa? this J2SProgram)
      (letfun! this args)
      (letfun-sa! this args))
   this)

;*---------------------------------------------------------------------*/
;*    letfun! ...                                                      */
;*    -------------------------------------------------------------    */
;*    letfun! rewrites the following pattern:                          */
;*       var f = function() { ... }                                    */
;*    info:                                                            */
;*       function f() { ... }                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (letfun! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    letfun! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (letfun! this::J2SFun args)
   (call-default-walker)
   (with-access::J2SFun this (body)
      (if (isa? body J2SBlock)
	  (multiple-value-bind (vars nodes)
	     (block-collect-vars body)
	     (multiple-value-bind (inits rest)
		(nodes-collect-inits nodes)
		(let ((finits (filter (lambda (i) (init-fun? i vars)) inits))
		      (assigs (filter (lambda (i) (assig-nofun? i vars)) inits)))
		   (when (pair? finits)
		      (let* ((odecls (map initvdecl finits))
			     (ndecls (map initfdecl finits))
			     (vdecls (filter-map initvdecl assigs))
			     (noinits (filter (lambda (v)
						 (and (not (memq v odecls))
						      (not (memq v vdecls))))
					 vars))
			     (nblock (duplicate::J2SBlock body
					(nodes rest))))
			 (set! body 
			    (duplicate::J2SBlock body
			       (nodes (append
					 (map (lambda (n)
						 (decl-alpha n odecls ndecls))
					    noinits)
					 (map (lambda (n)
						 (decl-alpha n odecls ndecls))
					    vdecls)
					 (map (lambda (n)
						 (decl-alpha n odecls ndecls))
					    ndecls)
					 (map (lambda (n)
						 (assig-alpha n odecls ndecls))
					    assigs)
					 (list (j2s-alpha nblock
						  odecls ndecls)))))))))))))
   this)

;*---------------------------------------------------------------------*/
;*    init-fun? ...                                                    */
;*---------------------------------------------------------------------*/
(define (init-fun? this::J2SAssig vars)
   (when (isa? this J2SInit)
      (with-access::J2SAssig this (lhs rhs)
	 (when (isa? rhs J2SFun)
	    (with-access::J2SRef lhs (decl)
	       (unless (decl-usage-has? decl '(assig))
		  (memq decl vars)))))))

;*---------------------------------------------------------------------*/
;*    assig-nofun? ...                                                 */
;*---------------------------------------------------------------------*/
(define (assig-nofun? this::J2SAssig vars)
   (not (isa? this J2SInit)))

;*---------------------------------------------------------------------*/
;*    initvdecl ...                                                    */
;*---------------------------------------------------------------------*/
(define (initvdecl this::J2SAssig)
   (when (isa? this J2SInit)
      (with-access::J2SAssig this (lhs rhs)
	 (with-access::J2SRef lhs (decl)
	    decl))))

;*---------------------------------------------------------------------*/
;*    initfdecl ...                                                    */
;*---------------------------------------------------------------------*/
(define (initfdecl this::J2SInit)
   (with-access::J2SInit this (lhs rhs)
      (with-access::J2SRef lhs (decl)
	 (with-access::J2SDecl decl (id writable scope usage binder utype vtype loc)
	    (instantiate::J2SDeclFun
	       (loc loc)
	       (id id)
	       (writable writable)
	       (scope scope)
	       (usage usage)
	       (binder binder)
	       (utype utype)
	       (vtype vtype)
	       (val rhs))))))

;*---------------------------------------------------------------------*/
;*    decl-alpha ...                                                   */
;*---------------------------------------------------------------------*/
(define (decl-alpha d::J2SDecl olds news)
   (when (isa? d J2SDeclInit)
      (with-access::J2SDeclInit d (val)
	 (set! val (j2s-alpha val olds news))))
   d)

;*---------------------------------------------------------------------*/
;*    assig-alpha ...                                                  */
;*---------------------------------------------------------------------*/
(define (assig-alpha n::J2SAssig olds news)
   (with-access::J2SAssig n (loc)
      (instantiate::J2SStmtExpr
	 (loc loc)
	 (expr (j2s-alpha n olds news)))))

;*---------------------------------------------------------------------*/
;*    block-collect-vars ...                                           */
;*---------------------------------------------------------------------*/
(define (block-collect-vars node::J2SBlock)
   (with-access::J2SBlock node (nodes)
      (let loop ((nodes nodes)
		 (vars '()))
	 (cond
	    ((null? nodes)
	     (values (reverse! vars) '()))
	    ((isa? (car nodes) J2SDecl)
	     (loop (cdr nodes) (cons (car nodes) vars)))
	    (else
	     (values (reverse! vars) nodes))))))

;*---------------------------------------------------------------------*/
;*    nodes-collect-inits ...                                          */
;*---------------------------------------------------------------------*/
(define (nodes-collect-inits nodes::pair-nil)

   (define (safe-access? rhs blacklist)
      (when (isa? rhs J2SAccess)
	 (with-access::J2SAccess rhs (obj field)
	    (and (safe-expr? obj blacklist) (safe-expr? field blacklist)))))

   (define (safe-array? rhs blacklist)
      (when (isa? rhs J2SArray)
	 (with-access::J2SArray rhs (exprs)
	    (every (lambda (e) (safe-expr? e blacklist)) exprs))))

   (define (safe-objinit? rhs blacklist)
      
      (define (safe-init? init)
	 (when (isa? init J2SDataPropertyInit)
	    (with-access::J2SDataPropertyInit init (name val)
	       (and (safe-expr? name blacklist) (safe-expr? val blacklist)))))
   
      (when (isa? rhs J2SObjInit)
	 (with-access::J2SObjInit rhs (inits)
	    (every safe-init? inits))))
		    
   (define (safe-ref? rhs)
      (when (isa? rhs J2SRef)
	 (with-access::J2SRef rhs (decl)
	    (with-access::J2SDecl decl (scope binder id)
	       (or (memq scope '(%scope global))
		   (eq? binder 'param))))))

   (define (safe-expr? expr blacklist)
      (or (isa? expr J2SFun)
	  (isa? expr J2SLiteral)
	  (safe-ref? expr)
	  (when (isa? expr J2SRef)
	     (with-access::J2SRef expr (decl)
		 (unless (memq decl blacklist)
		    (set-cdr! blacklist (cons decl (cdr blacklist)))
		    #t)))
	  (safe-access? expr blacklist)
	  (safe-array? expr blacklist)
	  (safe-objinit? expr blacklist)))

   (define (init node blacklist)
      (when (isa? node J2SStmtExpr)
	 (with-access::J2SStmtExpr node (expr)
	    (when (isa? expr J2SAssig)
	       (with-access::J2SAssig expr (rhs)
		  (when (safe-expr? rhs blacklist)
		     expr))))))
   
   (let loop ((nodes nodes)
	      (inits '())
	      (blacklist (cons 'mark '())))
      (cond
	 ((null? nodes)
	  (values (reverse! inits) '()))
	 ((isa? (car nodes) J2SLetBlock)
	  (values (reverse! inits) nodes))
	 ((isa? (car nodes) J2SSeq)
	  (with-access::J2SSeq (car nodes) ((bnodes nodes))
	     (loop (append bnodes (cdr nodes)) inits blacklist)))
	 ((init (car nodes) blacklist)
	  =>
	  (lambda (init)
	     (loop (cdr nodes) (cons init inits) blacklist)))
	 (else
	  (values (reverse! inits) nodes)))))
   
;*---------------------------------------------------------------------*/
;*    letfun-sa! ...                                                   */
;*    -------------------------------------------------------------    */
;*    letfun-sa! transforms patterns:                                  */
;*        block {                                                      */
;*           v = fun() { ... };                                        */
;*           ...                                                       */
;*           v();                                                      */
;*        }                                                            */
;*    into:                                                            */
;*        block {                                                      */
;*           letblock fun v'() { ... }                                 */
;*             ...                                                     */
;*             v'();                                                   */
;*           }                                                         */
;*        }                                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (letfun-sa! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    letfun-sa! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (letfun-sa! this::J2SFun args)
   (call-default-walker)
   (with-access::J2SFun this (body name)
      (when (isa? body J2SBlock)
	 (let ((env (make-cell '())))
	    (node-collect-sa body (list body) env)
	    (let ((assig (filter (lambda (d)
				    (with-access::J2SDecl d (%info)
				       (pair? %info)))
			    (cell-ref env))))
	       (when (pair? assig)
		  (set! body (letfun-sa-transform! body assig)))))))
   this)

;*---------------------------------------------------------------------*/
;*    node-collect-sa ::J2SNode ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (node-collect-sa this::J2SNode stack::pair-nil env::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    node-collect-sa ::J2SDecl ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (node-collect-sa this::J2SDecl stack::pair-nil env)
   (with-access::J2SDecl this (%info)
      (set! %info #unspecified)))

;*---------------------------------------------------------------------*/
;*    node-collect-sa ::J2SDeclInit ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (node-collect-sa this::J2SDeclInit stack::pair-nil env)
   (with-access::J2SDecl this (%info)
      (set! %info #f)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    node-collect-sa ::J2SAssig ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (node-collect-sa this::J2SAssig stack env)
   (with-access::J2SAssig this (lhs rhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (scope %info id)
		(when (and (eq? %info #unspecified)
			   (memq scope '(local inner)))
		   (if (and (isa? rhs J2SFun)
			    (with-access::J2SFun rhs (decl) (not decl)))
		       ;; optimize funtion expression only
		       (begin
			  (cell-set! env (cons decl (cell-ref env)))
			  (set! %info (cons rhs (car stack))))
		       ;; don't optimize function declaration
		       (set! %info #f)))
		(node-collect-sa rhs stack env)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    node-collect-sa ::J2SBlock ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (node-collect-sa this::J2SBlock stack env)
   (set! stack (cons this stack))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    node-collect-sa ::J2SLetBlock ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (node-collect-sa this::J2SLetBlock stack env)
   (with-access::J2SLetBlock this (decls)
      ;; invalidate all letblock decls
      (for-each (lambda (d)
		   (with-access::J2SDecl d (%info) (set! %info #f)))
	 decls)
      (set! stack (cons this stack))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    node-collect-sa ::J2SRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-collect-sa this::J2SRef stack env)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info)
	 ;; don't optimize closures, i.e., functions that escape
	 (set! %info #f))))

;*---------------------------------------------------------------------*/
;*    node-collect-sa ::J2SCall ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (node-collect-sa this::J2SCall stack env)
   (with-access::J2SCall this (fun args)
      (if (isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     (with-access::J2SDecl decl (scope %info)
		(when (pair? %info)
		   ;; if the call is outside the assignment block
		   ;; disable the optimization
		   (let ((block (cdr %info)))
		      (unless (memq block stack)
			 (set! %info #f))))
		(for-each (lambda (a) (node-collect-sa a stack env)) args)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    letfun-sa-transform! ::J2SNode ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (letfun-sa-transform! this::J2SNode env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    letfun-sa-transform! ::J2SBlock ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (letfun-sa-transform! this::J2SBlock env)
   (let ((odecls (filter (lambda (d)
			    (with-access::J2SDecl d (%info)
			       (eq? (cdr %info) this)))
		    env)))
      (if (pair? odecls)
	  (let ((ndecls (map (lambda (d)
				(with-access::J2SDecl d (%info id usage)
				   (with-access::J2SFun (car %info) (loc)
				      (instantiate::J2SDeclFun
					 (loc loc)
					 (id id)
					 (writable #f)
					 (scope 'local)
					 (usage (usage-rem usage 'assig))
					 (binder 'let-opt)
					 (utype 'function)
					 (vtype 'function)
					 (val (car %info))))))
			   odecls)))
	     (for-each (lambda (d)
			  (with-access::J2SDecl d (%info id)
			     (with-access::J2SFun (car %info) (body)
				(set! body
				   (letfun-sa-transform!
				      (j2s-alpha body odecls ndecls)
				      env)))))
		odecls)
	     (with-access::J2SBlock this (loc nodes)
		(J2SLetBlock* ndecls
		   (map! (lambda (n)
			    (j2s-alpha (letfun-sa-transform! n env)
			       odecls ndecls))
		      nodes))))
	  ;; rewrite
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    letfun-sa-transform! ::J2SAssig ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (letfun-sa-transform! this::J2SAssig env)
   (with-access::J2SAssig this (lhs rhs loc)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (%info id)
		(if (and (pair? %info) (isa? (car %info) J2SFun))
		    (J2SUndefined)
		    (call-default-walker))))
	  (call-default-walker))))
		   
