;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/inline.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Tue Jan 23 14:09:37 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
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
	   __js2scheme_use
	   __js2scheme_node-size)

   (static (class J2SMetaInl::J2SMeta
	      (inlstack::pair-nil (info '("notraverse")))))

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
;*    J2SMetaInl ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (J2SMetaInl stack optim stmt)
   `(instantiate::J2SMetaInl
       (loc loc)
       (inlstack ,stack)
       (debug 0)
       (optim ,optim)
       (stmt ,stmt)))

;*---------------------------------------------------------------------*/
;*    inline-default-factor ...                                        */
;*---------------------------------------------------------------------*/
(define inline-default-global-expansion 2)
(define inline-default-call-expansion 16)
(define inline-max-targets 3)
(define inline-recursive #f)
(define inline-max-size 40)

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
	  ;; mark all the function sizes
	  (let ((size (node-size this))
		(pms (ptable (append-map collect-proto-methods* nodes))))
	     (let loop ((limit 10))
		(inline! this limit '() pms this args)
		(let ((nsize (max (node-size this) limit)))
		   (when (<fx limit 60)
		      (loop (+fx limit 5)))))
	     ;; cleanup use count
	     (for-each reset-use-count decls)
	     (for-each reset-use-count nodes)
	     (for-each (lambda (n) (use-count n +1 #f)) decls)
	     (for-each (lambda (n) (use-count n +1 #f)) nodes)
	     (set! decls (filter used-decl? decls))
	     ;; cleanup metainl
	     (unmetainl! this)
	     this))
       this))

;*---------------------------------------------------------------------*/
;*    used-decl? ...                                                   */
;*---------------------------------------------------------------------*/
(define (used-decl? decl)
   (with-access::J2SDecl decl (usecnt)
      (or (>fx usecnt 0) (not (isa? decl J2SDeclFun)))))

;*---------------------------------------------------------------------*/
;*    iinfo                                                            */
;*---------------------------------------------------------------------*/
(define-struct inlinfo size)
(define-struct protoinfo assig method svar)

;*---------------------------------------------------------------------*/
;*    function-arity ...                                               */
;*---------------------------------------------------------------------*/
(define (function-arity fun::J2SFun)
   (with-access::J2SFun fun (params)
      (length params)))

;*---------------------------------------------------------------------*/
;*    function-size ...                                                */
;*---------------------------------------------------------------------*/
(define (function-size this::J2SFun)
   (with-access::J2SFun this (%info body)
      (unless (inlinfo? %info)
	 (set! %info (inlinfo (node-size this))))
      (inlinfo-size %info)))

;*---------------------------------------------------------------------*/
;*    invalidate-function-size! ...                                    */
;*---------------------------------------------------------------------*/
(define (invalidate-function-size! this::J2SFun)
   (with-access::J2SFun this (%info body)
      (set! %info #f)))
			     
;*---------------------------------------------------------------------*/
;*    ronly-variable? ...                                              */
;*---------------------------------------------------------------------*/
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

;*---------------------------------------------------------------------*/
;*    LetBlock ...                                                     */
;*---------------------------------------------------------------------*/
(define (LetBlock loc t body)
   (if (pair? t)
       (J2SLetRecBlock #f t body)
       body))

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
(define (inline!* lst limit::long stack::pair-nil pmethods prgm conf)
   (map (lambda (o) (inline! o limit stack pmethods prgm conf)) lst))
		       
;*---------------------------------------------------------------------*/
;*    inline! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SNode
		       limit::long stack::pair-nil pmethods prgm conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SMeta ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SMeta
		       limit::long stack::pair-nil pmethods prgm conf)
   (with-access::J2SMeta this (optim debug)
      (if (or (=fx optim 0) (>fx debug 0))
	  this
	  (call-default-walker))))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SMetaInl ...                                         */
;*---------------------------------------------------------------------*/
(define-method (inline! this::J2SMetaInl
		       limit::long stack::pair-nil pmethods prgm conf)
   (with-access::J2SMetaInl this (inlstack stmt loc)
      (set! stmt
	 (inline! stmt limit (append inlstack stack) pmethods prgm conf))
      this))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SDeclFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SDeclFun
		       limit::long stack::pair-nil pmethods prgm conf)
   (with-access::J2SDeclFun this (val id)
      (inline! val limit stack pmethods prgm conf)
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SFun
		       limit::long stack::pair-nil pmethods prgm conf)
   (with-access::J2SFun this (optimize body)
      (when optimize
	 (set! body (inline! body limit (cons this stack) pmethods prgm conf)))
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SCall ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SCall
		       limit::long stack::pair-nil pmethods prgm conf)
   
   (define (find-inline-methods this fun arity)
      (with-access::J2SAccess fun (obj field)
	 (when (isa? obj J2SRef)
	    (when (isa? field J2SString)
	       (with-access::J2SString field (val)
		  (let ((mets (filter (lambda (m::struct)
					 (let ((f (protoinfo-method m)))
					    (and (=fx (function-arity f) arity)
						 (not (memq f stack)))))
				 (or (hashtable-get pmethods val) '()))))
		     (when (<fx (apply +
				   (map (lambda (m)
					   (function-size
					      (protoinfo-method m)))
				      mets))
			      limit)
			mets)))))))
   
   (define (find-inline-function this::J2SCall fun arity)
      (with-access::J2SRef fun (decl)
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (val id)
	       (when (and (=fx (function-arity val) arity)
			  (not (memq val stack))
			  (<=fx (function-size val) limit))
		  val)))))
   
   (define (inline-access-call this::J2SCall fun::J2SAccess args loc)
      (let ((mets (find-inline-methods this fun (length args))))
	 (when (and mets (pair? mets))
	    (let* ((vals (map protoinfo-method mets))
		   (sz (apply + (map function-size vals))))
	       (inline-verb loc fun (length mets) sz limit conf)
	       (when (pair? stack) 
		  (invalidate-function-size! (car stack)))
	       (let ((e (inline-method-call fun mets args loc
			   limit stack pmethods prgm conf)))
		  (inline-stmt->expr loc
		     (inline! e limit (append vals stack) pmethods prgm conf)))))))
   
   (define (inline-ref-call this::J2SCall fun::J2SRef args loc)
      (let ((val (find-inline-function this fun (length args))))
	 (when val
	    (inline-verb loc fun 0 (function-size val) limit conf)
	    (when (pair? stack)
	       (invalidate-function-size! (car stack)))
	    (inline-stmt->expr loc
	       (inline-function-call val args loc
		  limit stack pmethods prgm conf)))))
   
   (with-access::J2SCall this (fun args type loc)
      (cond
	 ((isa? fun J2SAccess) (or (inline-access-call this fun args loc) this))
	 ((isa? fun J2SRef) (or (inline-ref-call this fun args loc) this))
	 (else this))))

;*---------------------------------------------------------------------*/
;*    inline-function-call ...                                         */
;*---------------------------------------------------------------------*/
(define (inline-function-call val::J2SFun args::pair-nil loc
	   limit::long stack::pair-nil pmethods prgm conf)
   (with-access::J2SFun val (body thisp params (floc loc))
      (let ((vals (inline-args params args limit stack pmethods prgm conf)))
	 (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
	    (J2SMetaInl (cons val stack)
	       (config-get conf :optim 0)
	       (inline!
		  (j2s-alpha body
		     (cons thisp params) (cons (J2SUndefined) vals))
		  limit (cons val stack) pmethods prgm conf))))))

;*---------------------------------------------------------------------*/
;*    inline-method-call ...                                           */
;*---------------------------------------------------------------------*/
(define (inline-method-call fun::J2SAccess callees::pair args::pair-nil loc
	   limit::long stack::pair-nil pmethods prgm conf)
   
   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))

   (define (cache-check c loc obj field kont inline::J2SStmt)
      (J2SIf (J2SCacheCheck 'proto-method c obj field)
	 inline
	 (kont)))

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

   (define (inline-method-args args)
      (map (lambda (a)
	      (if (isa? a J2SLiteral)
		  a
		  (let ((id (gensym 'a)))
		     (with-access::J2SNode a (loc)
			(J2SLetOpt '(ref) id
			   (inline! a limit stack pmethods prgm conf))))))
	 args))

   (define (inline-method obj::J2SRef field callee args cache loc kont)
      (let ((val (protoinfo-method callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (let ((vals (inline-args params args limit stack pmethods prgm conf)))
	       (with-access::J2SRef obj (decl)
		  (cache-check cache loc obj field kont
		     (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
			(J2SMetaInl (cons val stack)
			   (config-get conf :option 0)
			   (inline!
			      (j2s-alpha body
				 (cons thisp params) (cons decl vals))
			      limit (cons val stack) pmethods prgm conf)))))))))
   
   (define (inline-object-method-call fun obj args)
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
			    (cons (cons cache (car callees)) caches)))))))))
   
   (with-access::J2SAccess fun (obj field loc)
      ;; see J2S-EXPR-TYPE-TEST@__JS2SCHEME_AST for the
      ;; shape of the test that suits the tyflow analysis
      (let* ((vals (inline-method-args args))
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
		      (inline-object-method-call fun (J2SRef d) args)
		      (J2SMeta 0 0
			 (J2SStmtExpr
			    (J2SCall* (J2SAccess (J2SRef d) field)
			       args)))))))
	    ((not (eq? (j2s-type obj) 'object))
	     (LetBlock loc t 
		(J2SIf (J2SHopCall (J2SHopRef/rtype 'js-object? 'bool)
			  obj)
		   (inline-object-method-call fun obj args)
		   (J2SMeta 0 0
		      (J2SStmtExpr
			 (J2SCall* (J2SAccess obj field) args))))))
	    ((not (isa? obj J2SRef))
	     (let* ((id (gensym 'this))
		    (d (J2SLetOpt '(get) id obj)))
		(LetBlock loc (cons d t)
		   (inline-object-method-call fun (J2SRef d) args))))
	    (else
	     (inline-object-method-call fun obj args))))))

;*---------------------------------------------------------------------*/
;*    inline-args ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-args params args limit stack pmethods prgm conf)
   (map (lambda (p a)
	   (if (and (ronly-variable? p) (isa? a J2SLiteral))
	       a
	       (with-access::J2SDecl p (usage id writable)
		  (with-access::J2SNode a (loc)
		     (let ((d (J2SLetOpt usage (gensym id)
				 (inline! a limit stack pmethods prgm conf))))
			(with-access::J2SDecl d ((w writable))
			   (set! w writable))
			d)))))
      params args))

;*---------------------------------------------------------------------*/
;*    inline-stmt->expr::J2SExpr ...                                   */
;*    -------------------------------------------------------------    */
;*    Transforms an inline function body statement into an expression. */
;*---------------------------------------------------------------------*/
(define (inline-stmt->expr::J2SExpr loc body::J2SStmt)
   
   (define (stmt->expr node)
      (cond
	 ((isa? node J2SStmtExpr)
	  (with-access::J2SStmtExpr node (expr)
	     expr))
	 ((isa? node J2SLetBlock)
	  (with-access::J2SLetBlock node (loc decls nodes)
	     (when (and (null? decls) (pair? nodes) (null? (cdr nodes)))
		(stmt->expr (cadr nodes)))))
	 ((isa? node J2SBlock)
	  (with-access::J2SBlock node (nodes loc)
	     (cond
		((null? nodes)
		 (J2SUndefined))
		((null? (cdr nodes))
		 (stmt->expr (car nodes)))
		(else
		 #f))))
	 (else
	  #f)))
   
   (let* ((lbl (gensym '%return))
	  (cell (make-cell '()))
	  (bd (bind-exit! body lbl cell '())))
      (cond
	 ((pair? (cell-ref cell))
	  (let ((be (J2SBindExit lbl bd)))
	     (for-each (lambda (ret::J2SReturn)
			  (with-access::J2SReturn ret (from)
			     (set! from be)))
		(cell-ref cell))
	     be))
	 ((stmt->expr bd)
	  =>
	  (lambda (expr) expr))
	 (else
	  (J2SBindExit #f bd)))))
   
;*---------------------------------------------------------------------*/
;*    inline-expr! ...                                                 */
;*---------------------------------------------------------------------*/
(define (inline-expr!::J2SExpr expr::J2SExpr call stmt)
   (inline-node! expr call stmt))

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
(define-walk-method (bind-exit! this::J2SNode l cell env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SReturn l cell env)
   (with-access::J2SReturn this (tail exit from expr loc from)
      (set! expr (bind-exit! expr l cell env))
      (when (and (not exit) (not (memq from env)))
	 (unless exit
	    (set! tail #f)
	    (cell-set! cell (cons this (cell-ref cell)))))
      this))

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SBindExit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SBindExit l cell env)
   (with-access::J2SBindExit this (stmt)
      (set! stmt (bind-exit! stmt l cell (cons this env)))
      this))

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SFun l cell env)
   this)

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SMethod ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SMethod l cell env)
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

;*---------------------------------------------------------------------*/
;*    unmetainl! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unmetainl! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unmetainl! ::J2SMetaInl ...                                      */
;*---------------------------------------------------------------------*/
(define-method (unmetainl! this::J2SMetaInl)
   (with-access::J2SMetaInl this (stmt)
      stmt))

;*---------------------------------------------------------------------*/
;*    inline-verb ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-verb loc fun targets fsize limit conf)
   
   (define (loc->string loc)
      (match-case loc
	 ((at ?file ?pos) (format "[~a:~a]" file pos))
	 (else (format "[~s]" loc))))
   
   (when (>=fx (config-get conf :verbose 0) 3)
      (with-output-to-port (current-error-port)
	 (lambda ()
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
	    (display fsize)
	    (display "/")
	    (display limit)
	    (display ")")
	    (when (>=fx targets 1)
	       (display " [")
	       (display targets)
	       (display "]"))))))

