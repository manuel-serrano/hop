;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/inline.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Fri Dec  1 09:48:26 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Method inlining optimization                                     */
;*    -------------------------------------------------------------    */
;*    The method inlining proceeds as follows:                         */
;*      1- the AST is traversed to find all toplevel methods assigned  */
;*         to prototype objects.                                       */
;*      2- the AST is traversed to detect all the method calls, that   */
;*         might concern one the prototype methods, and to inline      */
;*         the call, protecting it with a hidden class check.          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_inline

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint
	   __js2scheme_alpha
	   __js2scheme_node-size)

   (export j2s-inline-stage))

;*---------------------------------------------------------------------*/
;*    j2s-inline-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-inline-stage
   (instantiate::J2SStageProc
      (name "inline")
      (comment "Method inlining optimization")
      (optional :optim-inline)
      (proc j2s-inline!)))

;*---------------------------------------------------------------------*/
;*    inline-default-factor ...                                        */
;*---------------------------------------------------------------------*/
(define inline-default-global-expansion 2)
(define inline-default-call-expansion 16)
(define inline-max-targets 3)
(define inline-recursive #f)

;*---------------------------------------------------------------------*/
;*    dev                                                              */
;*---------------------------------------------------------------------*/
(define blacklist
      (if (string? (getenv "BLACKLIST"))
	  (map string->symbol
	     (call-with-input-string (getenv "BLACKLIST") port->string-list))
	  '()))
(define whitelist
   (if (string? (getenv "WHITELIST"))
       (map string->symbol
	  (call-with-input-string (getenv "WHITELIST") port->string-list))
       '()))

(define checked '())

(define (check-id id)
   (cond
      ((not (or (null? blacklist) (not (memq id blacklist))))
       (unless (memq id checked)
	  (set! checked (cons id checked))
	  (tprint "black " id))
       #f)
      ((not (or (null? whitelist) (memq id whitelist)))
       (unless (memq id checked)
	  (set! checked (cons id checked))
	  (tprint "not white " id))
       #f)
      (else
       #t)))

;*---------------------------------------------------------------------*/
;*    j2s-inline! ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-inline! this args)
   (if (isa? this J2SProgram)
       (with-access::J2SProgram this (decls nodes)
	  (let ((pms (ptable (append-map collect-proto-methods* nodes))))
	     (inline!* decls pms this args)
	     (inline!* nodes pms this args)
	     this))
       this))

;*---------------------------------------------------------------------*/
;*    iinfo                                                            */
;*---------------------------------------------------------------------*/
(define-struct inlinfo size obody)
(define-struct callinfo call callees ifctx loopctx)
(define-struct protoinfo assig method svar)

;*---------------------------------------------------------------------*/
;*    get-method-size! ...                                             */
;*---------------------------------------------------------------------*/
(define (get-method-size! this::J2SFun)
   (with-access::J2SFun this (%info body)
      (unless (inlinfo? %info)
	 (set! %info (inlinfo (node-size this) body)))
      (inlinfo-size %info)))
			     
;*---------------------------------------------------------------------*/
;*    collect-proto-methods* ...                                       */
;*    -------------------------------------------------------------    */
;*    Collect the inline candidates, which are the top level methods   */
;*    assigned to constructor prototypes.                              */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-proto-methods* this::J2SNode)
   '())

;*---------------------------------------------------------------------*/
;*    collect-proto-methods* ::J2SSeq ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-proto-methods* this::J2SSeq)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-proto-methods* ::J2SStmtExpr ...                         */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-proto-methods* this::J2SStmtExpr)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-proto-methods* ::J2SAssig ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-proto-methods* this::J2SAssig)

   (define (method-of rhs)
      (if (isa? rhs J2SMethod)
	  (with-access::J2SMethod rhs (method) method)
	  rhs))

   (with-access::J2SAssig this (lhs rhs)
      (if (and (isa? lhs J2SAccess) (or (isa? rhs J2SFun) (isa? rhs J2SMethod)))
	  (with-access::J2SAccess lhs (obj (metname field))
	     (if (and (isa? obj J2SAccess) (isa? metname J2SString))
		 (with-access::J2SAccess obj (obj field)
		    (if (and (isa? obj J2SRef) (isa? field J2SString))
			(with-access::J2SString field (val)
			   (if (string=? val "prototype")
			       (with-access::J2SString metname (val)
				  (if (check-id (string->symbol val))
				      (list (cons val
					       (protoinfo this (method-of rhs) #f)))
				      '()))
			       '()))
			'()))
		 '()))
	  '())))

;*---------------------------------------------------------------------*/
;*    ptable ...                                                       */
;*    -------------------------------------------------------------    */
;*    Store all inline candidate methods into a global hashtable.      */
;*---------------------------------------------------------------------*/
(define (ptable::struct alist)
   (let ((table (create-hashtable)))
      (for-each (lambda (e)
		   (hashtable-add! table (car e) cons (cdr e) '()))
	 alist)
      table))

;*---------------------------------------------------------------------*/
;*    inline!* ...                                                     */
;*---------------------------------------------------------------------*/
(define (inline!* lst pmethods prgm args)
   (map (lambda (o) (inline! o '() pmethods prgm args)) lst))
		       
;*---------------------------------------------------------------------*/
;*    inline! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SNode stack::pair-nil
		       pmethods prgm args)
   (call-default-walker)
   this)

;*---------------------------------------------------------------------*/
;*    inline! ::J2SDeclFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SDeclFun stack::pair-nil
		       pmethods prgm args)
   (with-access::J2SDeclFun this (val id)
      (inline! val (cons this stack) pmethods prgm args)
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SFun stack::pair-nil
		       pmethods prgm args)
   
   (define (loc->string loc)
      (match-case loc
	 ((at ?file ?pos) (format "[~a:~a]" file pos))
	 (else (format "[~s]" loc))))   
   
   (define (verb-call call deltasz callees)
      (with-output-to-port (current-error-port)
	 (lambda ()
	    (with-access::J2SCall call (fun loc)
	       (cond
		  ((isa? fun J2SRef)
		   (with-access::J2SRef fun (decl)
		      (with-access::J2SDecl decl (id)
			 (display "\n      ")
			 (display id))))
		  ((isa? fun J2SAccess)
		   (with-access::J2SAccess fun (obj field)
		      (with-access::J2SString field (val)
			 (display "\n      ")
			 (when (isa? obj J2SRef)
			    (with-access::J2SRef obj (decl)
			       (with-access::J2SDecl decl (id)
				  (display id))))
			 (display ".")
			 (display val)))))
	       (display "() ")
	       (display (loc->string loc))
	       (display " (")
	       (when (>fx deltasz 0) (display "+"))
	       (display deltasz)
	       (display " x")
	       (display (length callees))
	       (display ") ")))))
   
   (define (highscore c1 c2)
      (let ((l1 (length (callinfo-callees c1)))
	    (l2 (length (callinfo-callees c2))))
	 (cond
	    ((<fx l1 l2) #t)
	    ((>=fx (callinfo-loopctx c1) (callinfo-loopctx c2)) #t)
	    ((<=fx (callinfo-ifctx c1) (callinfo-ifctx c2)) #t)
	    (else #f))))
   
   (define (try-inline inlinables isize)
      (let loop ((inlinables (sort highscore inlinables))
		 (ninlinables '()))
	 (if (null? inlinables)
	     (values #f #f 0 ninlinables #f)
	     (let* ((inl (car inlinables))
		    (call (callinfo-call inl))
		    (callees (callinfo-callees inl))
		    (expr (inline-call call callees prgm))
		    (esize (node-size expr)))
		(if (and (<=fx esize isize)
			 (<=fx esize
			    (* inline-default-call-expansion
			       (node-size call))))
		    (values call expr esize
		       (append ninlinables (cdr inlinables)) inl)
		    (loop (cdr inlinables)
		       (cons (car inlinables) ninlinables)))))))

   (with-access::J2SFun this (optimize body %info loc)
      (when optimize
	 (let loop ((isize (* (get-method-size! this)
			      (config-get args :inline-factor
				 inline-default-global-expansion)))
		    (inlinables (collect-inlinables* body
				   pmethods (list this) 0 0))
		    (stack (list this))
		    (cnt 1))
	    (multiple-value-bind (call inl inlsz inlinables ci)
	       (try-inline inlinables isize)
	       (when inl
		  (when (>=fx (config-get args :verbose 0) 2)
		     (verb-call call (-fx inlsz (node-size call))
			(callinfo-callees ci)))
		  (set! body (inline-node! body call inl))
		  (let* ((nisize (-fx isize (-fx inlsz (node-size call))))
			 (nstack (append (map protoinfo-method
					    (callinfo-callees ci))
				    stack))
			 (ninlinables (filter (lambda (ci)
						 (not (eq? (callinfo-call ci) call)))
					 (collect-inlinables* inl pmethods
					    nstack
					    (callinfo-ifctx ci)
					    (callinfo-loopctx ci)))))
		     (loop nisize
			(append ninlinables inlinables)
			nstack
			(+fx cnt 1))))))
	 ;; inline all the subfunctions
	 (call-default-walker))
      this))

;*---------------------------------------------------------------------*/
;*    inline-call ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-call::J2SExpr this::J2SCall callees::pair prgm::J2SProgram)
   
   (define (ronly-variable? obj)
      (cond
	 ((isa? obj J2SRef)
	  (with-access::J2SRef obj (decl)
	     (with-access::J2SDecl decl (ronly)
		ronly)))
	 ((isa? obj J2SDecl)
	  (with-access::J2SDecl obj (ronly)
	     ronly))
	 (else
	  #f)))
   
   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))
   
   (define (cache-check c loc obj field kont inline::J2SStmt)
      (with-access::J2SCall this (cache)
	 (J2SIf (J2SCacheCheck 'proto-method c obj field)
	    inline
	    (kont))))
   
   (define (LetBlock loc t body)
      (if (pair? t)
	  (J2SLetRecBlock #f t body)
	  body))
   
   (define (get-svar callee)
      (if (protoinfo-svar callee)
	  (protoinfo-svar callee)
	  (let ((fun (gensym '%fun)))
	     (protoinfo-svar-set! callee fun)
	     (with-access::J2SProgram prgm (globals)
		(set! globals (cons `(define ,fun #unspecified) globals))
		(with-access::J2SAssig (protoinfo-assig callee) (rhs loc)
		   (set! rhs
		      (J2SSequence
			 (J2SAssig (J2SHopRef fun) rhs)
			 (J2SHopRef fun)))))
	     fun)))
   
   (define (inline-args params args loc)
      (map (lambda (p a)
	      (if (and (ronly-variable? p) (isa? a J2SLiteral))
		  a
		  (with-access::J2SDecl p (usage id)
		     (J2SLetOpt usage id a))))
	 params args))
   
   (define (inline-method obj::J2SRef field callee args cache loc kont)
      (with-access::J2SFun (protoinfo-method callee) (body thisp params)
	 (let ((vals (inline-args params args loc)))
	    (with-access::J2SRef obj (decl)
	       (cache-check cache loc obj field kont
		  (LetBlock loc (filter (lambda (b) (isa? b J2SDecl)) vals)
		     (j2s-alpha body
			(cons thisp params) (cons decl vals))))))))
   
   (define (inline-object-method-call this obj args)
      (with-access::J2SCall this (fun loc)
	 (with-access::J2SAccess fun (field)
	    (let loop ((callees callees)
		       (caches '()))
	       (if (null? callees)
		   (let ((f (J2SLetOpt '(call) (gensym 'f) fun)))
		      (J2SLetRecBlock #f (list f)
			 (let loop ((caches caches))
			    (if (null? caches)
				(J2SNop)
				(let ((v (get-svar (cdar caches))))
				   (J2SIf
				      (J2SBinary 'eq? (J2SRef f)
					 (J2SHopRef v))
				      (J2SStmtExpr
					 (J2SCacheUpdate 'proto-method
					    (caar caches) obj))
				      (loop (cdr caches))))))
			 (J2SReturn #t
			    (J2SMethodCall* (J2SRef f) (list obj) args))))
		   (let ((cache (get-cache prgm)))
		      (inline-method obj field (car callees) args cache loc
			 (lambda ()
			    (loop (cdr callees)
			       (cons (cons cache (car callees)) caches))))))))))
   
   (define (inline-method-call this)
      (with-access::J2SCall this (fun args)
	 (with-access::J2SAccess fun (obj field loc)
	    ;; see J2S-EXPR-TYPE-TEST@__JS2SCHEME_AST for the
	    ;; shape of the test that suits the tyflow analysis
	    (let* ((vals (map (lambda (a)
				 (if (isa? a J2SLiteral)
				     a
				     (let ((id (gensym 'a)))
					(J2SLetOpt '(ref) id a))))
			    args))
		   (t (filter (lambda (b) (isa? b J2SDecl)) vals))
		   (args (map (lambda (v)
				 (if (isa? v J2SDecl)
				     (with-access::J2SDecl v (loc)
					(J2SRef v))
				     v))
			    vals)))
	       (cond
		  ((and (not (eq? (j2s-type obj) 'object))
			(not (isa? obj J2SRef)))
		   (let* ((id (gensym 'this))
			  (d (J2SLetOpt '(get) id obj)))
		      (LetBlock loc (cons d t)
			 (J2SIf (J2SHopCall (J2SHopRef/rtype 'js-object? 'bool)
				   (J2SRef d))
			    (inline-object-method-call this (J2SRef d) args)
			    (J2SMeta 0 0
			       (J2SStmtExpr
				  (J2SCall* (J2SAccess (J2SRef d) field)
				     args)))))))
		  ((not (eq? (j2s-type obj) 'object))
		   (LetBlock loc t 
		      (J2SIf (J2SHopCall (J2SHopRef/rtype 'js-object? 'bool)
				obj)
			 (inline-object-method-call this obj args)
			 (J2SMeta 0 0
			    (J2SStmtExpr
			       (J2SCall* (J2SAccess obj field) args))))))
		  ((not (isa? obj J2SRef))
		   (let* ((id (gensym 'this))
			  (d (J2SLetOpt '(get) id obj)))
		      (LetBlock loc (cons d t)
			 (inline-object-method-call this (J2SRef d) args))))
		  (else
		   (inline-object-method-call this obj args)))))))
   
   (define (inline-function-call this)
      (with-access::J2SCall this (fun loc args)
	 (with-access::J2SFun (protoinfo-method (car callees)) (body thisp params loc)
	    (let ((vals (inline-args params args loc)))
	       (LetBlock loc (filter (lambda (b) (isa? b J2SDecl)) vals)
		  (j2s-alpha body
		     (cons thisp params) (cons (J2SUndefined) vals)))))))

   (define (stmt->expr node)
      (cond
	 ((isa? node J2SStmtExpr)
	  (with-access::J2SStmtExpr node (expr)
	     expr))
	 ((isa? node J2SLetBlock)
	  (with-access::J2SLetBlock node (loc decls nodes)
	     (if (and (null? decls) (pair? nodes) (null? (cdr nodes)))
		 (stmt->expr (cadr nodes))
		 (J2SExprStmt node))))
	 ((isa? node J2SBlock)
	  (with-access::J2SBlock node (nodes loc)
	     (cond
		((null? nodes)
		 (J2SUndefined))
		((null? (cdr nodes))
		 (stmt->expr (car nodes)))
		(else
		 (J2SExprStmt node)))))
	 (else
	  (with-access::J2SStmt node (loc)
	     (J2SExprStmt node)))))
   
   (with-access::J2SCall this (fun loc args)
      (let ((body (if (isa? fun J2SAccess)
		      (inline-method-call this)
		      (inline-function-call this))))
	 (let* ((lbl (gensym '%return))
		(cell (make-cell #f))
		(bd (bind-exit! body lbl cell)))
	    (if (cell-ref cell)
		(J2SBindExit lbl bd)
		(stmt->expr bd))))))

;*---------------------------------------------------------------------*/
;*    collect-inlinables ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SNode pmethods stk ifctx loopctx)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SFun ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SFun pmethods stk ifctx loopctx)
   '())

;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SIf ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SIf pmethods stk ifctx loopctx)
   (with-access::J2SIf this (test then else)
      (append (collect-inlinables* test pmethods stk ifctx loopctx)
	 (collect-inlinables* then pmethods stk (+fx 1 ifctx) loopctx)
	 (collect-inlinables* else pmethods stk (+fx 1 ifctx) loopctx))))

;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SFor ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SFor pmethods stk ifctx loopctx)
   (with-access::J2SFor this (init test incr body)
      (append (collect-inlinables* init pmethods stk ifctx loopctx)
	 (collect-inlinables* test pmethods stk ifctx (+fx 1 loopctx))
	 (collect-inlinables* incr pmethods stk ifctx (+fx 1 loopctx))
	 (collect-inlinables* body pmethods stk ifctx (+fx 1 loopctx)))))
	 
;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SWhile ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SWhile pmethods stk ifctx loopctx)
   (with-access::J2SWhile this (test body)
      (append (collect-inlinables* test pmethods stk ifctx loopctx)
	 (collect-inlinables* body pmethods stk ifctx (+fx 1 loopctx)))))

;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SMeta ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SMeta pmethods stk ifctx loopctx)
   (with-access::J2SMeta this (optim debug)
      (if (or (=fx optim 0) (>fx debug 0))
	  '()
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SCall ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SCall pmethods stk ifctx loopctx)

   (define (same-arity? fun::J2SFun arity)
      (with-access::J2SFun fun (params)
	 (=fx (length params) arity)))

   (define (find-inline-methods fun arity)
      (with-access::J2SAccess fun (obj field)
	 (when (isa? obj J2SRef)
	    (when (isa? field J2SString)
	       (with-access::J2SString field (val)
		  (filter (lambda (m::struct)
			     (and (same-arity? (protoinfo-method m) arity)
				  (or inline-recursive
				      (not (memq (protoinfo-method m) stk)))))
		     (or (hashtable-get pmethods val) '())))))))

   (define (find-inline-function fun arity)
      (with-access::J2SRef fun (decl)
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (val id)
	       (when (and (same-arity? val arity)
			  (or inline-recursive
			      (not (memq val stk))))
		  (when (check-id id)
		     (list (protoinfo #f val #f))))))))
   
   (define (find-inlines fun arity)
      (cond
	 ((isa? fun J2SAccess) (find-inline-methods fun arity))
	 ((isa? fun J2SRef) (find-inline-function fun arity))))

   (with-access::J2SCall this (fun args type)
      (let ((inls (find-inlines fun (length args))))
	 (if (and (pair? inls) (<=fx (length inls) inline-max-targets))
	     (cons (callinfo this inls ifctx loopctx) (call-default-walker))
	     (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    inline-expr! ...                                                 */
;*---------------------------------------------------------------------*/
(define (inline-expr!::J2SExpr expr::J2SExpr call stmt)
   (let ((node (inline-node! expr call stmt)))
      (if (isa? node J2SExpr)
	  node
	  (with-access::J2SExpr expr (loc)
	     (let ((t (J2SLetOpt '(ref set) (gensym 'a) (J2SUndefined))))
		(J2SExprStmt
		   (J2SLetRecBlock #f (list t)
		      (unreturn! node
			 (lambda (n::J2SReturn)
			    (with-access::J2SReturn n (expr loc)
			       (J2SStmtExpr
				  (J2SAssig (J2SRef t) expr)))))
		      (J2SRef t))))))))

;*---------------------------------------------------------------------*/
;*    inline-expr*! ...                                                */
;*---------------------------------------------------------------------*/
(define (inline-expr*! exprs::pair-nil call stmt)
   (map! (lambda (e) (inline-expr! e call stmt)) exprs))
   
;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SNode ...                                       */
;*    -------------------------------------------------------------    */
;*    Replace expression CALL with statement STMT.                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SNode call::J2SCall inl::J2SNode)
   (call-default-walker)
   this)

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SCall ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SCall call inl)
   (if (eq? this call)
       inl
       (with-access::J2SCall this (fun args loc)
	  (set! fun (inline-expr! fun call inl))
	  (set! args (inline-expr*! args call inl))
	  this)))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SNew ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SNew call inl)
   (with-access::J2SNew this (clazz args loc)
      (set! clazz (inline-expr! clazz call inl))
      (set! args (inline-expr*! args call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-mode! ::J2SParen ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SParen call inl)
   (with-access::J2SParen this (expr)
      (set! expr (inline-node! expr call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SUnary ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SUnary call inl)
   (with-access::J2SUnary this (expr loc)
      (set! expr (inline-expr! expr call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SBinary ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SBinary call inl)
   (with-access::J2SBinary this (rhs lhs loc)
      (set! lhs (inline-expr! lhs call inl))
      (set! rhs (inline-expr! rhs call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SAccess ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SAccess call inl)
   (with-access::J2SAccess this (obj field)
      (set! obj (inline-expr! obj call inl))
      (set! field (inline-expr! field call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SStmtExpr ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SStmtExpr call inl)
   (with-access::J2SStmtExpr this (expr loc)
      (let ((node (inline-node! expr call inl)))
	 (if (isa? node J2SExpr)
	     (begin
		(set! expr node)
		this)
	     (unreturn! node
		(lambda (n::J2SReturn)
		   (with-access::J2SReturn n (expr)
		      (J2SStmtExpr expr))))))))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SIf ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SIf call inl)
   (with-access::J2SIf this (test then else loc)
      (set! then (inline-node! then call inl))
      (set! else (inline-node! else call inl))
      (set! test (inline-expr! test call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SReturn ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SReturn call inl)
   (with-access::J2SReturn this (expr tail)
      (set! expr (inline-expr! expr call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SAssig ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SAssig call inl)
   ;; for now, only the rhs part is considered for inlining
   (with-access::J2SAssig this (lhs rhs loc)
      (set! rhs (inline-expr! rhs call inl))
      (if (isa? lhs J2SRef)
	  (let ((node (inline-node! rhs call inl)))
	     (if (isa? node J2SExpr)
		 (begin
		    (set! rhs node)
		    this)
		 (unreturn! node
		    (lambda (n::J2SReturn)
		       (with-access::J2SReturn n (expr loc)
			  (set! expr (J2SAssig lhs expr))
			  n)))))
	  this)))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SLoop ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SLoop call inl)
   (with-access::J2SLoop this (body)
      (set! body (inline-node! body call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SFor ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SFor call inl)
   (with-access::J2SFor this (init test incr body)
      (set! test (inline-expr! test call inl))
      (set! incr (inline-expr! incr call inl))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SWhile ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SWhile call inl)
   (with-access::J2SWhile this (test body)
      (set! test (inline-expr! test call inl))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SDeclInit ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SDeclInit call inl)
   (with-access::J2SDeclInit this (val)
      (set! val (inline-expr! val call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SLetBlock ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SLetBlock call inl)
   (with-access::J2SLetBlock this (decls nodes)
      (set! decls (map! (lambda (d) (inline-node! d call inl)) decls))
      (set! nodes (map! (lambda (n) (inline-node! n call inl)) nodes))
      this))
   
;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SNode conv)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SReturn conv)
   (with-access::J2SReturn this (tail exit expr loc)
      (if (or tail exit)
	  (conv this)
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SFun conv)
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SMethod ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SMethod conv)
   this)

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SNode ...                                         */
;*    -------------------------------------------------------------    */
;*    Replace untail return (those of the inlined function) with       */
;*    an exit.                                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SNode l cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SReturn l cell)
   (with-access::J2SReturn this (tail exit lbl expr loc)
      (cond
	 (exit
	  (set! expr (bind-exit! expr l cell))
	  this)
	 (tail
	  (J2SStmtExpr (bind-exit! expr l cell)))
	 (else
	  (set! lbl l)
	  (cell-set! cell #t)
	  this))))

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SFun l cell)
   this)

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SMethod ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SMethod l cell)
   this)

;*---------------------------------------------------------------------*/
;*    node-return-count ...                                            */
;*---------------------------------------------------------------------*/
(define (node-return-count this::J2SNode max)
   (let ((cell (make-cell 0)))
      (return-count this cell max)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    return-count ::J2SNode ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (return-count this::J2SNode cnt max)
   (if (< (cell-ref cnt) max)
       (call-default-walker)
       (cell-ref cnt)))

;*---------------------------------------------------------------------*/
;*    return-count ::J2SReturn ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (return-count this::J2SReturn cnt max)
   (cell-set! cnt (+fx 1 (cell-ref cnt))))

