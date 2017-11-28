;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/inline.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Tue Nov 28 12:42:23 2017 (serrano)                */
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
(define inline-default-call-expansion 10)
(define inline-max-targets 3)

;*---------------------------------------------------------------------*/
;*    dev                                                              */
;*---------------------------------------------------------------------*/
(define blacklist
      (if (string? (getenv "BLACKLIST"))
	  (call-with-input-string (getenv "BLACKLIST") port->string-list)
	  '()))
(define whitelist
   (if (string? (getenv "WHITELIST"))
       (call-with-input-string (getenv "WHITELIST") port->string-list)
       '()))

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
				  (cond
				     ((member val blacklist)
				      (tprint "black " val)
				      '())
				     ((or (null? whitelist) (member val whitelist))
				      (list (cons val (method-of rhs))))
				     (else
				      (tprint "not white " val)
				      '())))
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
   
   (define (verb-call call deltasz)
      (with-access::J2SCall call (fun loc)
	 (cond
	    ((isa? fun J2SRef)
	     (with-access::J2SRef fun (decl)
		(with-access::J2SDecl decl (id)
		   (display "\n      " (current-error-port))
		   (display id (current-error-port)))))
	    ((isa? fun J2SAccess)
	     (with-access::J2SAccess fun (obj field)
		(with-access::J2SString field (val)
		   (display "\n      " (current-error-port))
		   (when (isa? obj J2SRef)
		      (with-access::J2SRef obj (decl)
			 (with-access::J2SDecl decl (id)
			    (display id (current-error-port)))))
		   (display "." (current-error-port))
		   (display val (current-error-port))))))
	 (display "() ")
	 (display (loc->string loc) (current-error-port))
	 (display " (" (current-error-port))
	 (when (>fx deltasz 0) (display "+" (current-error-port)))
	 (display deltasz (current-error-port))
	 (display ") ")))
   
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
		    (stmt (inline-call call callees prgm))
		    (ssize (node-size stmt)))
		(if (and (<=fx ssize isize)
			 (<=fx ssize
			    (* inline-default-call-expansion
			       (node-size call))))
		    (values call stmt ssize
		       (append ninlinables (cdr inlinables)) inl)
		    (loop (cdr inlinables)
		       (cons (car inlinables) ninlinables)))))))
   
   (with-access::J2SFun this (optimize body %info)
      (when optimize
	 (let loop ((isize (* (get-method-size! this)
			      (config-get args :inline-factor
				 inline-default-global-expansion)))
		    (inlinables (collect-inlinables* body pmethods 0 0)))
	    (multiple-value-bind (call stmt stmtsz inlinables ci)
	       (try-inline inlinables isize)
	       (when stmt
		  (when (>=fx (config-get args :verbose 0) 2)
		     (verb-call call (-fx stmtsz (node-size call))))
		  (set! body (inline-node! body call stmt))
		  (let ((nisize (-fx isize (-fx stmtsz (node-size call))))
			(ninlinables (filter (lambda (ci)
						(not (eq? (callinfo-call ci) call)))
					(collect-inlinables* stmt pmethods
					   (callinfo-ifctx ci)
					   (callinfo-loopctx ci)))))
		     (loop nisize
			(append ninlinables inlinables)))))))
      this))

;*---------------------------------------------------------------------*/
;*    inline-call ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-call::J2SStmt this::J2SCall callees::pair prgm::J2SProgram)
   
   (define (ronly-variable? obj)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (with-access::J2SDecl decl (ronly)
	       ronly))))
   
   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))
   
   (define (LetBlock loc t body)
      (cond
	 ((pair? t)
	  (J2SLetRecBlock #f t body))
	 ((isa? body J2SBlock)
	  body)
	 (else
	  (let ((endloc loc))
	     (J2SBlock body)))))
   
   (define (cache-check loc obj field kont inline::J2SStmt)
      (let ((c (get-cache prgm)))
	 (with-access::J2SCall this (cache)
	    (set! cache c)
	    (J2SIf (J2SCacheCheck 'proto-method c obj field)
	       inline
	       (kont)))))
   
   (define (inline-method obj field callee args kont)
      (with-access::J2SFun callee (body thisp params)
	 (with-access::J2SDecl thisp (loc usage id)
	    (cond
	       ((not (isa? obj J2SRef))
		(let ((d (J2SLetOptUtype 'object usage id obj))
		      (t (map (lambda (p a)
				 (with-access::J2SDecl p (usage id)
				    (J2SLetOpt usage id a)))
			    params args)))
		   (J2SLetRecBlock #f (list d)
		      (cache-check loc (J2SRef d) field kont
			 (J2SLetRecBlock #f t
			    (j2s-alpha body
			       (cons thisp params) (cons d t)))))))
	       ((ronly-variable? obj)
		;; no need to rebind this
		(let ((t (map (lambda (p a)
				 (with-access::J2SDecl p (usage id)
				    (J2SLetOpt usage id a)))
			    params args)))
		   (with-access::J2SRef obj (decl)
		      (cache-check loc obj field kont
			 (LetBlock loc t
			    (j2s-alpha body
			       (cons thisp params) (cons decl t)))))))
	       (else
		;; create a temporary for this
		(let ((d (J2SLetOptUtype 'object usage id obj))
		      (t (map (lambda (p a)
				 (with-access::J2SDecl p (usage id)
				    (J2SLetOpt usage id a)))
			    params args)))
		   (J2SLetRecBlock #f (list d)
		      (cache-check loc (J2SRef d) field kont
			 (LetBlock loc t
			    (j2s-alpha body
			       (cons thisp params) (cons d t)))))))))))

   (define (inline-method-call this)
      (with-access::J2SCall this (fun loc args)
	 (with-access::J2SAccess fun (obj field)
	    (let loop ((callees callees))
	       (if (null? callees)
		   (J2SReturn #f this)
		   (inline-method obj field (car callees) args
		      (lambda ()
			 (loop (cdr callees)))))))))

   (define (inline-function-call this)
      (with-access::J2SCall this (fun loc args)
	 (with-access::J2SFun (car callees) (body thisp params loc)
	    (with-access::J2SDecl thisp (loc usage id)
	       (let ((t (map (lambda (p a)
				(with-access::J2SDecl p (usage id)
				   (J2SLetOpt usage id a)))
			   params args)))
		  (LetBlock loc t
		     (j2s-alpha body
			(cons thisp params) (cons (J2SUndefined) t))))))))
   
   (with-access::J2SCall this (fun loc args)
      (if (isa? fun J2SAccess)
	  (inline-method-call this)
	  (inline-function-call this))))

;*---------------------------------------------------------------------*/
;*    collect-inlinables ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SNode pmethods ifctx loopctx)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SIf ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SIf pmethods ifctx loopctx)
   (with-access::J2SIf this (test then else)
      (append (collect-inlinables* test pmethods ifctx loopctx)
	 (collect-inlinables* then pmethods (+fx 1 ifctx) loopctx)
	 (collect-inlinables* else pmethods (+fx 1 ifctx) loopctx))))

;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SFor ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SFor pmethods ifctx loopctx)
   (with-access::J2SFor this (init test incr body)
      (append (collect-inlinables* init pmethods ifctx loopctx)
	 (collect-inlinables* test pmethods ifctx (+fx 1 loopctx))
	 (collect-inlinables* incr pmethods ifctx (+fx 1 loopctx))
	 (collect-inlinables* body pmethods ifctx (+fx 1 loopctx)))))
	 
;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SWhile ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SWhile pmethods ifctx loopctx)
   (with-access::J2SWhile this (test body)
      (append (collect-inlinables* test pmethods ifctx loopctx)
	 (collect-inlinables* body pmethods ifctx (+fx 1 loopctx)))))

;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SCall ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SCall pmethods ifctx loopctx)
   
   (define (same-arity? fun::J2SFun arity)
      (with-access::J2SFun fun (params)
	 (=fx (length params) arity)))
   
   (define (find-inline-methods fun arity)
      (with-access::J2SAccess fun (obj field)
	 (when (isa? obj J2SRef)
	    (when (isa? field J2SString)
	       (with-access::J2SString field (val)
		  (filter (lambda (m::J2SFun)
			     (same-arity? m arity))
		     (or (hashtable-get pmethods val) '())))))))

   (define (find-inline-function fun arity)
      (with-access::J2SRef fun (decl)
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (val id)
	       (when (and (same-arity? val arity)
			  (or (null? blacklist)
			      (not (member (symbol->string! id) blacklist)))
			  (or (null? whitelist)
			      (member (symbol->string! id) blacklist)))
		  (list val))))))
   
   (define (find-inlines fun arity)
      (cond
	 ((isa? fun J2SAccess) (or (find-inline-methods fun arity)))
	 ((isa? fun J2SRef) (or (find-inline-function fun arity)))))

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
		   (J2SLetBlock (list t)
		      (unreturn! node
			 (lambda (n::J2SReturn)
			    (with-access::J2SReturn n (expr loc)
			       (J2SStmtExpr
				  (J2SAssig (J2SRef t) expr))))))))))))
   
;*---------------------------------------------------------------------*/
;*    inline-expr* ...                                                 */
;*---------------------------------------------------------------------*/
(define (inline-expr* exprs call stmt loc kont)
   (let ((nodes (map (lambda (e) (inline-node! e call stmt)) exprs)))
      (if (every (lambda (n) (isa? n J2SExpr)) nodes)
	  (kont #f nodes)
	  (let loop ((nodes nodes)
		     (nexprs '()))
	     (cond
		((null? nodes)
		 (kont #t (reverse! nexprs)))
		((isa? (car nodes) J2SExpr)
		 (let ((t (J2SLetOpt '(ref set) (gensym 'a) (car nodes))))
		    (J2SLetBlock (list t)
		       (loop (cdr nodes) (cons (J2SRef t) nexprs)))))
		((>fx (node-return-count (car nodes) 2) 1)
		 (let ((t (J2SLetOpt '(ref set) (gensym 'a) (J2SUndefined))))
		    (J2SLetBlock (list t)
		       (unreturn! (car nodes)
			  (lambda (n::J2SReturn)
			     (with-access::J2SReturn n (expr loc)
				(J2SStmtExpr
				   (J2SAssig (J2SRef t) expr)))))
		       (loop (cdr nodes) (cons (J2SRef t) nexprs)))))
		(else
		 (unreturn! (car nodes)
		    (lambda (n::J2SReturn)
		       (with-access::J2SReturn n (expr loc)
			  (loop (cdr nodes) (cons expr nexprs)))))))))))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SNode ...                                       */
;*    -------------------------------------------------------------    */
;*    Replace expression CALL with statement STMT.                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SNode call::J2SCall stmt::J2SStmt)
   (call-default-walker)
   this)

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SCall ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SCall call stmt)
   (if (eq? this call)
       stmt
       (with-access::J2SCall this (fun args loc)
	  (let ((node (inline-node! fun call stmt)))
	     (if (isa? node J2SExpr)
		 (inline-expr* args call stmt loc
		    (lambda (stmtp exprs)
		       (if stmtp
			   (J2SCall* node exprs)
			   (begin
			      (set! fun node)
			      (set! args exprs)
			      this))))
		 (unreturn! node
		    (lambda (n::J2SReturn)
		       (with-access::J2SReturn n (expr)
			  (inline-expr* args call stmt  loc
			     (lambda (stmtp exprs)
				(J2SCall* expr exprs)))))))))))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SNew ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SNew call stmt)
   (with-access::J2SNew this (clazz args loc)
      (let ((node (inline-node! clazz call stmt)))
	 (if (isa? node J2SExpr)
	     (inline-expr* args call stmt loc
		(lambda (stmtp exprs)
		   (if stmtp
		       (J2SNew* node exprs)
		       (begin
			  (set! clazz node)
			  (set! args exprs)
			  this))))
	     (unreturn! node
		(lambda (n::J2SReturn)
		   (with-access::J2SReturn n (expr)
		      (inline-expr* args call stmt  loc
			 (lambda (stmtp exprs)
			    (J2SNew* expr exprs))))))))))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SUnary ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SUnary call stmt)
   (with-access::J2SUnary this (op type expr loc)
      (inline-expr* (list expr) call stmt loc
	 (lambda (stmtp exprs)
	    (if stmtp
		(J2SUnary/type op type (car exprs))
		(begin
		   (set! expr (car exprs))
		   this))))))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SBinary ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SBinary call stmt)
   (with-access::J2SBinary this (op type rhs lhs loc)
      (inline-expr* (list lhs rhs) call stmt loc
	 (lambda (stmtp exprs)
	    (if stmtp
		(J2SBinary/type op type (car exprs) (cadr exprs))
		(begin
		   (set! lhs (car exprs))
		   (set! rhs (cadr exprs))
		   this))))))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SAccess ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SAccess call stmt)
   (with-access::J2SAccess this (obj field)
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SStmtExpr ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SStmtExpr call stmt)
   (with-access::J2SStmtExpr this (expr loc)
      (let ((node (inline-node! expr call stmt)))
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
(define-walk-method (inline-node! this::J2SIf call stmt)
   (with-access::J2SIf this (test then else loc)
      (set! then (inline-node! then call stmt))
      (set! else (inline-node! else call stmt))
      (let ((node (inline-node! test call stmt)))
	 (cond
	    ((isa? node J2SExpr)
	     (set! test node)
	     this)
	    ((>fx (node-return-count node 2) 1)
	     (let ((t (J2SLetOpt '(ref set) (gensym 'test) (J2SBool #f))))
		(J2SLetRecBlock #f (list t)
		   (unreturn! node
		      (lambda (n::J2SReturn)
			 (with-access::J2SReturn n (expr loc)
			    (J2SStmtExpr
			       (J2SAssig (J2SRef t) expr)))))
		   (J2SIf (J2SRef t) then else))))
	    (else
	     (unreturn! node
		(lambda (n::J2SReturn)
		   (with-access::J2SReturn n (expr)
		      (J2SIf expr then else)))))))))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SReturn ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SReturn call stmt)
   (with-access::J2SReturn this (expr tail)
      (let ((node (inline-node! expr call stmt)))
	 (if (isa? node J2SExpr)
	     (begin
		(set! expr node)
		this)
	     (unreturn! node
		(lambda (n::J2SReturn)
		   (with-access::J2SReturn n (expr loc)
		      (J2SReturn tail expr))))))))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SAssig ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SAssig call stmt)
   ;; for now, only the rhs part is considered for inlining
   (with-access::J2SAssig this (lhs rhs loc)
      (if (isa? lhs J2SRef)
	  (let ((node (inline-node! rhs call stmt)))
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
(define-walk-method (inline-node! this::J2SLoop call stmt)
   (with-access::J2SLoop this (body)
      (set! body (inline-node! body call stmt))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SFor ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SFor call stmt)
   (with-access::J2SFor this (init test incr body)
      (set! test (inline-expr! test call stmt))
      (set! incr (inline-expr! incr call stmt))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SWhile ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SWhile call stmt)
   (with-access::J2SWhile this (test body)
      (set! test (inline-expr! test call stmt))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SDeclInit ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SDeclInit call stmt)
   (with-access::J2SDeclInit this (val)
      (set! val (inline-expr! val call stmt))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SLetBlock ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SLetBlock call stmt)
   (with-access::J2SLetBlock this (decls nodes)
      (set! decls (map! (lambda (d) (inline-node! d call stmt)) decls))
      (set! nodes (map! (lambda (n) (inline-node! n call stmt)) nodes))
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
   (conv this))

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
