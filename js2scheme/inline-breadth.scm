;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/inline-breadth.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Sat Feb 12 12:08:29 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Function/Method inlining optimization                            */
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
(module __js2scheme_inline-breadth

   (library web)
   
   (include "ast.sch"
	    "usage.sch"
	    "inline.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint
	   __js2scheme_alpha
	   __js2scheme_use
	   __js2scheme_node-size
	   __js2scheme_freevars
	   __js2scheme_classutils
	   __js2scheme_inline-common)

   (export (j2s-inline-breadth ::J2SProgram conf)))

;*---------------------------------------------------------------------*/
;*    j2s-inline-breadth ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-inline-breadth this::J2SProgram conf)
   (with-access::J2SProgram this (decls nodes)
      ;; count and mark all the calls
      (j2s-count-calls! this conf)
      (let ((maxsize (*fx (node-size this) inline-global-expansion))
	    (pms (ptable
		    (append
		       (append-map collect-proto-methods* nodes)
		       (if (config-get conf :optim-inline-class-method)
			   (append-map collect-proto-methods* decls)
			   '())))))
	 (let loop ((limit 10))
	    (inline! this #f limit '() pms #f this conf)
	    (let ((nsize (node-size this)))
	       (when (and (<fx limit inline-max-function-size)
			  (<fx nsize maxsize))
		  (loop (*fx limit 2)))))
	 (ptable-verb pms))
      (j2s-inline-cleanup! this conf)))

;*---------------------------------------------------------------------*/
;*    invalidate-function-size! ...                                    */
;*---------------------------------------------------------------------*/
(define (invalidate-function-size! this::J2SFun)
   (with-access::J2SFun this (%info body)
      (set! %info #f)))
			     
;*---------------------------------------------------------------------*/
;*    inline! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SNode
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SMeta ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SMeta
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SMeta this (optim debug meta)
      (if (and (eq? meta 'inline) (or (=fx optim 0) (>fx debug 0)))
	  this
	  (call-default-walker))))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SMetaInl ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SMetaInl
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SMetaInl this (inlstack stmt loc optim)
      (when (>fx optim 0)
	 (set! stmt
	    (inline! stmt
	       targets limit (append inlstack stack) pmethods ingen prgm conf)))
      this))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SDeclFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SDeclFun
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SDeclFun this (val id)
      (inline! val targets limit stack pmethods ingen prgm conf)
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SFun
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SFun this (optimize body generator)
      (when optimize
	 (set! body
	    (inline! body
	       targets limit (cons this stack) pmethods
	       generator prgm conf)))
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SCall ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SCall
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   
   (define (find-inline-decl-function this::J2SCall fun arity limit stack)
      (with-access::J2SRef fun (decl)
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (id)
	       (let ((val (j2sdeclinit-val-fun decl)))
		  (when (and (>=fx (function-arity val) arity)
			     (function-fxarg? val)
			     (not (function-generator? val))
			     (not (function-newtarget? val))
			     (not (function-delete-argument? val))
			     (not (memq val stack))
			     (<=fx (function-size val) limit)
			     (inline-check-id id)
			     (not (function-self-recursive? val))
			     (eq? (function-mode val) (function-mode (car stack)))
			     (not (isa? val J2SSvc)))
		     val))))))

   (define (find-inline-methods this fun arity)
      (with-access::J2SAccess fun (obj field loc)
	 (when (and (isa? field J2SString)
		    (config-get conf :optim-inline-method #f)
		    (not (isa? obj J2SSuper)))
	    (with-access::J2SString field (val)
	       (let* ((mets (filter (lambda (m::struct)
				       (let ((f (protoinfo-method m)))
					  (and (=fx (function-arity f) arity)
					       (<fx (function-size f)
						  inline-max-method-size)
					       (function-fxarg? f)
					       (not (memq f stack))
					       (not (function-self-recursive? f)))))
			       (or (hashtable-get pmethods val) '())))
		      (sz (apply +
			     (map (lambda (m)
				     (+fx inline-method-check-size
					(function-size (protoinfo-method m))))
				mets))))
		  (when (and (<fx sz (*fx limit (length mets)))
			     (and (<fx sz inline-max-method-size)
				  (every (lambda (m)
					    (let ((f (protoinfo-method m)))
					       (function-leaf? f)))
				     mets)))
		     (sort (lambda (f1 f2)
			      (>=fx (function-size (protoinfo-method f1))
				 (function-size (protoinfo-method f2))))
			mets)))))))

   (define (inline-access-call this::J2SCall fun::J2SAccess args loc)
      (with-access::J2SAccess fun (obj)
	 (unless (eq? (j2s-type obj) 'proxy)
	    (let ((mets (find-inline-methods this fun (length args))))
	       (when (pair? mets)
		  (let* ((mets (filter-rutype
				  (if (pair? targets)
				      (filter (lambda (t) (memq t mets)) targets)
				      mets)
				  prgm))
			 (vals (map protoinfo-method mets))
			 (sz (apply + (map function-size vals))))
		     (when (pair? mets)
			(inline-verb loc fun
			   (map (lambda (x) '-) mets) sz limit 0 conf)
			(when (pair? stack) 
			   (invalidate-function-size! (car stack)))
			(let ((e (inline-method-call fun mets args
				    loc (node-endloc this)
				    '() limit stack pmethods ingen prgm conf)))
			   (inline-stmt->expr loc e
			      (function-rutype
				 (protoinfo-method (car mets))))))))))))
   
   (define (inline-ref-call this::J2SCall fun::J2SRef thisargs args loc)
      (cond
	 ((find-inline-decl-function this fun (length args) limit stack)
	  =>
	  (lambda (target)
	     (inline-verb loc fun '(-) (function-size target) limit 0 conf)
	     (when (pair? stack)
		(invalidate-function-size! (car stack)))
	     (inline-stmt->expr loc
		(inline-function-call target thisargs args loc
		   targets (if targets 0 limit) stack pmethods ingen prgm conf)
		(function-rutype target))))
	 ((pair? targets)
	  (let ((targets (filter-rutype targets prgm)))
	     (when (pair? targets)
		(when (pair? stack)
		   (invalidate-function-size! (car stack)))
		(inline-stmt->expr loc
		   (inline-unknown-call fun thisargs  args loc
		      targets limit stack pmethods ingen prgm conf)
		   (function-rutype (car targets))))))
	 (else
	  #f)))

   (define (inline-expr-call this fun thisargs args loc)
      (let ((decl (J2SDeclInit '(ref) (gensym '%fun) fun))
	    (endloc (node-endloc this)))
	 (inline-stmt->expr loc
	    (J2SLetBlock (list decl)
	       (inline-ref-call this (J2SRef decl) thisargs args loc))
	    'unknown)))
   
   (with-access::J2SCall this (fun thisargs args type loc cache protocol)
      (cond
	 ((memq (caddr loc) inline-blacklistloc)
	  (call-default-walker))
	 ((and (pair? inline-whitelistloc) (not (memq (caddr loc) inline-whitelistloc)))
	  (call-default-walker))
	 ((null? stack)
	  ;; don't inline at toplevel
	  (call-default-walker))
	 (cache
	  (call-default-walker))
	 ((eq? protocol 'spread)
	  (call-default-walker))
	 ((and ingen (any yield? args))
	  (call-default-walker))
	 ((isa? fun J2SAccess)
	  (or (inline-access-call this fun args loc)
	      (call-default-walker)))
	 ((isa? fun J2SRef)
	  (or (inline-ref-call this fun thisargs args loc)
	      (call-default-walker)))
	 ((pair? targets)
	  (or (inline-expr-call this fun thisargs args loc)
	      (call-default-walker)))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    inline-function-call ...                                         */
;*---------------------------------------------------------------------*/
(define (inline-function-call val::J2SFun thisargs args::pair-nil loc
	   targets limit::long stack::pair-nil pmethods ingen prgm conf)
   (with-access::J2SFun val (body thisp params (floc loc))
      (let* ((vals (inline-args (cons thisp params)
		      (if (pair? thisargs)
			  (append thisargs args)
			  (cons (J2SUndefined) args))
		      #f limit stack pmethods ingen prgm conf loc))
	     (nbody (j2s-alpha body (cons thisp params) vals)))
	 (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
	    (J2SMetaInl (cons val stack)
	       (config-get conf :optim 0)
	       (if (> limit 0)
		   (inline! nbody
		      #f limit (cons val stack) pmethods ingen prgm conf)
		   nbody))))))

;*---------------------------------------------------------------------*/
;*    inline-unknown-call ...                                          */
;*---------------------------------------------------------------------*/
(define (inline-unknown-call ref::J2SRef thisargs args::pair-nil loc
	   targets limit::long stack::pair-nil pmethods ingen prgm conf)
   (let loop ((targets targets))
      (if (null? targets)
	  (J2SStmtExpr (J2SCall* ref args))
	  (let* ((target (car targets))
		 (fun (targetinfo-fun target)))
	     (if (< (function-size fun) limit)
		 (begin
		    (inline-verb loc fun '(-) (function-size fun) limit 0 conf)
		    (J2SIf (J2SHopCall (J2SHopRef/rtype 'eq? 'bool)
			      (J2SRef (with-access::J2SRef ref (decl) decl))
			      (J2SRef (targetinfo-decl target)))
		       (inline-function-call fun thisargs args loc
			  #f 0 stack pmethods ingen prgm conf)
		       (loop (cdr targets))))
		 (loop '()))))))
   
;*---------------------------------------------------------------------*/
;*    inline-method-call ...                                           */
;*---------------------------------------------------------------------*/
(define (inline-method-call fun::J2SAccess callees::pair args::pair-nil loc endloc
	   targets limit::long stack::pair-nil pmethods ingen prgm conf)
   
   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))

   (define (proto-method obj)
      (if (isa? (j2s-type obj) J2SRecord)
	  (if (pair? (cdr callees)) 'record-method-mono 'record-method-poly)
	  (if (pair? (cdr callees)) 'proto-method-mono 'proto-method-poly)))
   
   (define (cache-check c loc owner obj field kont inline::J2SStmt)
      (if (private-field? field)
	  inline
	  (J2SIf (J2SCacheCheck (proto-method obj) c owner obj field)
	     inline
	     (kont))))

   (define (get-svar callee)
      (if (protoinfo-svar callee)
	  (protoinfo-svar callee)
	  (let ((fun (gensym '%inlmet)))
	     (protoinfo-svar-set! callee fun)
	     (with-access::J2SProgram prgm (globals)
		(set! globals (cons `(define ,fun #unspecified) globals))
		(let ((as (protoinfo-assig callee)))
		   (cond
		      ((isa? as J2SAssig)
		       (with-access::J2SAssig as (rhs loc)
			  (set! rhs
			     (J2SSequence
				(J2SAssig (J2SHopRef fun) rhs)
				(J2SHopRef fun)))))
		      ((isa? as J2SMethodPropertyInit)
		       (with-access::J2SMethodPropertyInit as (inlinecachevar)
			  (set! inlinecachevar fun)))
		      (else
		       (error "inline-method-call" "bad protoinfo" callee)))))
	     fun)))

   (define (inline-method-args args)
      (map (lambda (a)
	      (if (simple-argument? a)
		  a
		  (let ((id (gensym 'iarg)))
		     (with-access::J2SNode a (loc)
			(J2SLetOpt '(ref assig) id
			   (inline! a
			      #f limit stack pmethods ingen prgm conf))))))
	 args))

   (define (inline-record-method obj::J2SRef field callee args cache loc kont)
      (let ((val (protoinfo-method callee))
	    (clazz (protoinfo-owner callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (with-access::J2SRef obj (decl)
	       (let ((vals (inline-args params args
			      #f limit stack pmethods ingen prgm conf loc))
		     (ndecl (J2SLetOpt/vtype clazz '(get) (gensym 'this)
			       (J2SCast/static #t clazz obj))))
		  (cache-check cache loc (protoinfo-owner callee) obj field kont
		     (LetBlock floc (cons ndecl
				       (filter (lambda (b) (isa? b J2SDecl)) vals))
			(J2SMetaInl (cons val stack)
			   (config-get conf :optim 0)
			   (inline!
			      (j2s-alpha body
				 (cons thisp params) (cons ndecl vals))
			      #f limit
			      (cons val stack) pmethods ingen prgm conf)))))))))

   (define (inline-object-method obj::J2SRef field callee args cache loc kont)
      (let ((val (protoinfo-method callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (with-access::J2SRef obj (decl)
	       (let ((vals (inline-args params args
			      #f limit stack pmethods ingen prgm conf loc)))
		  (cache-check cache loc (protoinfo-owner callee) obj field kont
		     (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
			(J2SMetaInl (cons val stack)
			   (config-get conf :optim 0)
			   (inline!
			      (j2s-alpha body
				 (cons thisp params) (cons decl vals))
			      #f limit
			      (cons val stack) pmethods ingen prgm conf)))))))))

   (define (inline-expr-method obj::J2SExpr field callee args cache loc kont)
      (let ((val (protoinfo-method callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (let ((vals (inline-args params args
			   #f limit stack pmethods ingen prgm conf loc))
		  (decl (J2SLetOpt '(ref) (gensym 'this) obj)))
	       (cache-check cache loc (protoinfo-owner callee) obj field kont
		  (LetBlock floc (cons decl (filter (lambda (b) (isa? b J2SDecl)) vals))
		     (J2SMetaInl (cons val stack)
			(config-get conf :optim 0)
			(inline!
			   (j2s-alpha body
			      (cons thisp params) (cons decl vals))
			   #f limit
			   (cons val stack) pmethods ingen prgm conf))))))))

   (define (inline-method obj field callee args cache loc kont)
      (cond
	 ((not (isa? obj J2SRef))
	  (inline-expr-method obj field callee args cache loc kont))
	 ((isa? (protoinfo-owner callee) J2SRecord)
	  (inline-record-method obj field callee args cache loc kont))
	 (else
	  (inline-object-method obj field callee args cache loc kont))))
   
   (define (inline-object-method-call fun self args)
      (with-access::J2SAccess fun (obj field cspecs)
	 (let loop ((callees callees)
		    (caches '()))
	    (if (null? callees)
		(let ((f (duplicate::J2SAccess fun
			    (obj self))))
		   (if (isa? (j2s-type self) J2SRecord)
		       (J2SMetaInl stack 0
			  (J2SReturn #t
			     (J2SMethodCall* f (list self) args)))
		       (let* ((c (get-cache prgm))
			      (r (J2SLetOpt '(call) (gensym 'r)
				    (J2SMethodCall/cache* f (list self) args
				       '(vtable-inline pmap-inline poly) c))))
			  (J2SLetRecBlock #f (list r)
			     (let loop ((cs caches))
				(if (null? cs)
				    (J2SNop)
				    (let ((v (get-svar (cdar cs))))
				       (J2SIf
					  (J2SCacheCheck 'method-and-owner
					     c (j2s-alpha self '() '()) (J2SHopRef v))
					  (J2SSeq*
					     (map (lambda (c)
						     (J2SStmtExpr
							(if (eq? c (car cs))
							    (J2SCacheUpdate (proto-method obj)
							       (car c) (j2s-alpha self '() '()))
							    (J2SUndefined))))
						caches))
					  (loop (cdr cs))))))
			     (J2SReturn #t (J2SRef r))))))
		(let ((cache (get-cache prgm)))
		   (inline-method self field (car callees) args cache loc
		      (lambda ()
			 (loop (cdr callees)
			    (cons (cons cache (car callees)) caches)))))))))

   (define (gen-check-object obj field args)
      (J2SIf (J2SHopCall (J2SHopRef/rtype 'js-object? 'bool) obj)
	 (inline-object-method-call fun obj args)
	 (J2SMeta 'inline 0 0
	    (J2SReturn #t
	       (J2SCall* (J2SAccess (j2s-alpha obj '() '()) field) args)))))
   
   (with-access::J2SAccess fun (obj field loc)
      ;; see J2S-EXPR-TYPE-TEST@__JS2SCHEME_AST for the
      ;; shape of the test that suits the tyflow analysis
      (let* ((vals (inline-method-args args))
	     (tmps (filter (lambda (b) (isa? b J2SDecl)) vals))
	     (args (map (lambda (v)
			   (if (isa? v J2SDecl)
			       (with-access::J2SDecl v (loc)
				  (J2SRef v))
			       v))
		      vals)))
	 (cond
	    ((not (type-object? (j2s-type obj)))
	     (if (simple-argument? obj)
		 (if (pair? tmps)
		     (LetBlock loc tmps
			(gen-check-object obj field args))
		     (gen-check-object obj field args))
		 (let* ((id (gensym 'this))
			(d (J2SLetOpt '(get) id obj)))
		    (LetBlock loc (cons d tmps)
		       (gen-check-object (J2SRef d) field args)))))
	    ((not (isa? obj J2SRef))
	     (let* ((id (gensym 'this))
		    (d (J2SLetOpt/vtype (j2s-type obj) '(get) id obj)))
		(LetBlock loc (cons d tmps)
		   (inline-object-method-call fun (J2SRef d) args))))
	    ((pair? tmps)
	     (LetBlock loc tmps
		(inline-object-method-call fun obj args)))
	    (else
	     (inline-object-method-call fun obj args))))))

;*---------------------------------------------------------------------*/
;*    inline-args ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-args params args targets limit stack pmethods ingen prgm conf loc)
   (let ((lena (length args))
	 (lenp (length params)))
      (map (lambda (p a)
	      (cond
		 ((and (ronly-variable? p) (simple-argument? a))
		  a)
		 (else
		  (with-access::J2SDecl p ((_usage usage) id writable)
		     (with-access::J2SNode a (loc)
			(let ((d (J2SLetOpt _usage (gensym id)
				    (inline! a
				       targets limit stack pmethods ingen prgm conf))))
			   (with-access::J2SDecl d ((w writable))
			      (set! w writable))
			   d))))))
	 params
	 (if (<fx lena lenp)
	     (append args
		;; complement with missing args
		(map! (lambda (i) (J2SUndefined)) (iota (-fx lenp lena))))
	     args))))

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
;*    update-parent! ...                                               */
;*---------------------------------------------------------------------*/
(define (update-parent! this::J2SNode old new)
   
   (define (update-pair! f old new)
      (let loop ((f f))
	 (cond
	    ((not (pair? f))
	     #f)
	    ((eq? (car f) old)
	     (set-car! f new)
	     f)
	    (else
	     (loop (cdr f))))))
      
   (let ((fields (class-all-fields (object-class this))))
      (let loop ((i (-fx (vector-length fields) 1)))
	 (if (=fx i -1)
	     (error "update-parent!" "Cannot find node" (j2s->sexp old))
	     (let* ((f (vector-ref fields i))
		    (info (class-field-info f)))
		(if (and (pair? info) (member "ast" info))
		    (let ((val ((class-field-accessor f) this)))
		       (cond
			  ((eq? val old)
			   ((class-field-mutator f) this new))
			  ((update-pair! val old new)
			   =>
			   (lambda (newp)
			      ((class-field-mutator f) this newp)))
			  (else
			   (loop (-fx i 1)))))
		    (loop (-fx i 1))))))))
