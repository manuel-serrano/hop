;*=====================================================================*/
;*    .../prgm/project/hop/3.2.x-new-types/js2scheme/inline.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Tue Aug 14 16:24:37 2018 (serrano)                */
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

   (library web)
   
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
(define inline-global-expansion 3)
(define inline-max-function-size 60)
(define inline-min-call-occurrence 100000)

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
   (cond
      ((not (isa? this J2SProgram))
       this)
      ((config-get args :profile-call #f)
       (j2s-count-calls! this args)
       this)
      ((config-get args :profile-log #f)
       =>
       (lambda (logfile)
	  (j2s-inline-profile this args logfile)))
      (else
       (j2s-inline-noprofile this args))))

;*---------------------------------------------------------------------*/
;*    j2s-inline-profile ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-inline-profile prgm::J2SProgram conf logfile)
   
   (define (find-protoinfo loc pms::struct)
      (bind-exit (return)
	 (hashtable-for-each pms
	    (lambda (k b)
	       (for-each (lambda (pi)
			    (with-access::J2SFun (protoinfo-method pi) ((mloc loc))
			       (match-case mloc
				  ((at ?- ?l)
				   (when (eq? l loc) (return pi))))))
		  b)))))
   
   (define (find-targets candidates funs pms::struct)
      (let ((tgts (vector-map (lambda (v)
				 (when (> (car v) inline-min-call-occurrence)
				    (let ((loc (cdr v)))
				       (cond
					  ((assq loc funs)
					   =>
					   (lambda (tgt)
					      (cons (car v) (cdr tgt))))
					  ((find-protoinfo loc pms)
					   =>
					   (lambda (pi)
					      (cons (car v) pi)))))))
		     candidates)))
	 (sort (lambda (x y) (> (car x) (car y)))
	    (filter (lambda (x) x) (vector->list tgts)))))
   
   (define (call-targets this::J2SCall clog funs pms::struct)
      (if (vector? (cddr clog))
	  (map cdr (find-targets (cddr clog) funs pms))
	  (>= (cadr clog) inline-min-call-occurrence)))
   
   (define (inline-profile!::long this::J2SCall clog leaf fuel owner pms funs)
      (with-access::J2SCall this (%info loc)
	 (let ((targets (call-targets this clog funs pms)))
	    (if targets
		(let ((parent %info)
		      (sz (node-size this))
		      (new (inline! this targets leaf inline-max-function-size
			      (list owner) pms prgm conf)))
		   (if (eq? new this)
		       (values 0 '())
		       (let ((nc (collect-calls-and-link* new parent owner)))
			  (update-parent! parent this new)
			  (values (-fx (node-size new) sz) nc))))
		(values 0 '())))))
   
   (define (inline-calls! nl::struct clog leaf::bool fuel pms::struct funs)
      (let loop ((calls (nodelink-nodes nl))
		 (sz 0)
		 (nc '()))
	 (if (null? calls)
	     (values sz nc)
	     (let ((call (car calls)))
		(multiple-value-bind (z c)
		   (inline-profile! call
		      clog leaf fuel (nodelink-owner nl) pms funs)
		   (loop (cdr calls) (+fx sz z) (append c nc)))))))
   
   (let ((log (call-profile-log prgm logfile conf)))
      (if log
	  (with-access::J2SProgram prgm (nodes)
	     (let* ((calls (collect-calls-and-link* prgm prgm #f))
		    (funs (collect-funs* prgm))
		    (size (node-size prgm))
		    (fuel (- (* size inline-global-expansion) size))
		    (pms (ptable (append-map collect-proto-methods* nodes))))
		(let loop ((i 0)
			   (fuel fuel)
			   (leaf #t))
		   (cond
		      ((<= fuel 0)
		       (j2s-inline-profile-cleanup! prgm conf))
		      ((=fx i (vector-length log))
		       (if leaf
			   (loop 0 fuel #f)
			   (j2s-inline-profile-cleanup! prgm conf)))
		      (else
		       (let ((clog (vector-ref log i)))
			  (if (and (>=fx (car clog) 0)
				   (>= (cadr clog) inline-min-call-occurrence))
			      (let ((nl (assq (car clog) calls)))
				 (if (pair? nl)
				     (multiple-value-bind (f c l)
					(inline-calls! (cdr nl) clog
					   leaf fuel pms funs)
					(nodelink-nodes-set! (cdr nl)
					   (append
					      (append-map
						 (lambda (c)
						    (nodelink-nodes (cdr c)))
						 c)
					      (nodelink-nodes (cdr nl))))
					(loop (+fx i 1) (- fuel f) leaf))
				     (loop (+fx i 1) fuel leaf)))
			      (loop (+fx i 1) fuel leaf))))))))
	  (j2s-inline-noprofile prgm conf))))

;*---------------------------------------------------------------------*/
;*    j2s-inline-noprofile ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-inline-noprofile this::J2SProgram conf)
   (with-access::J2SProgram this (decls nodes)
      ;; count and mark all the calls
      (j2s-count-calls! this conf)
      ;; mark all the function sizes
      (let ((size (node-size this))
	    (pms (ptable (append-map collect-proto-methods* nodes))))
	 (when (string-contains (or (getenv "HOPTRACE") "") "j2s:inline")
	    (with-output-to-port (current-error-port)
	       (lambda ()
		  (print "\n      proto methods:")
		  (hashtable-for-each pms
		     (lambda (k b)
			(print "       " k ": ")
			(for-each (lambda (p)
				     (with-access::J2SAssig (protoinfo-assig p) (loc)
					(print "         " (cadr loc)
					   ":" (caddr loc) " size="
					   (node-size (protoinfo-method p)))))
			   b))))))
	 (let liip ((leaf #f))
	    (let loop ((limit 10))
	       (inline! this #f leaf limit '() pms this conf)
	       (let ((nsize (node-size this)))
		  (when (and (<fx limit inline-max-function-size)
			     (< nsize (* size inline-global-expansion)))
		     (loop (+fx limit 5)))))
	    (unless leaf
	       (liip #t))))
      (j2s-inline-profile-cleanup! this conf)))

;*---------------------------------------------------------------------*/
;*    j2s-inline-profile-cleanup! ...                                  */
;*---------------------------------------------------------------------*/
(define (j2s-inline-profile-cleanup! this::J2SProgram conf)
   (with-access::J2SProgram this (decls nodes)
      ;; cleanup use count
      (reinit-use-count! this)
      (set! decls (filter used-decl? decls))
      ;; cleanup metainl
      (unmetainl! this)
      this))

;*---------------------------------------------------------------------*/
;*    used-decl? ...                                                   */
;*---------------------------------------------------------------------*/
(define (used-decl? decl)
   (with-access::J2SDecl decl (usecnt usage)
      (when (or (>fx usecnt 0)
		(not (isa? decl J2SDeclFun))
		(usage? '(eval) usage))
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (val)
	       (set! val (dead-inner-decl! val))))
	 #t)))

;*---------------------------------------------------------------------*/
;*    iinfo                                                            */
;*---------------------------------------------------------------------*/
(define-struct inlinfo size)
(define-struct protoinfo assig method svar)
(define-struct targetinfo decl fun)
(define-struct nodelink nodes owner)

;*---------------------------------------------------------------------*/
;*    function-arity ...                                               */
;*---------------------------------------------------------------------*/
(define (function-arity fun::J2SFun)
   (with-access::J2SFun fun (params)
      (length params)))

;*---------------------------------------------------------------------*/
;*    function-fxarg? ...                                              */
;*---------------------------------------------------------------------*/
(define (function-fxarg? fun::J2SFun)
   (with-access::J2SFun fun (vararg)
      (not vararg)))

;*---------------------------------------------------------------------*/
;*    function-leaf? ...                                               */
;*---------------------------------------------------------------------*/
(define (function-leaf? fun::J2SFun)
   (with-access::J2SFun fun (body)
      (let ((cell (make-cell #t)))
	 (node-leaf body cell)
	 (cell-ref cell))))
			       
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
	  (with-access::J2SDecl decl (ronly usage)
	     (or ronly (not (usage? '(assig) usage))))))
      ((isa? obj J2SDecl)
       (with-access::J2SDecl obj (ronly usage)
	  (or ronly (not (usage? '(assig) usage)))))
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
;*    Collect the inline methods candidates, which are the top level   */
;*    methods assigned to constructor prototypes.                      */
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
(define (inline!* lst targets leaf::bool limit::long stack::pair-nil pmethods prgm conf)
   (map (lambda (o)
	   (inline! o targets leaf limit stack pmethods prgm conf))
      lst))
		       
;*---------------------------------------------------------------------*/
;*    inline! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SNode
		       targets leaf limit::long stack::pair-nil pmethods prgm conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SMeta ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SMeta
		       targets leaf limit::long stack::pair-nil pmethods prgm conf)
   (with-access::J2SMeta this (optim debug)
      (if (or (=fx optim 0) (>fx debug 0))
	  this
	  (call-default-walker))))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SMetaInl ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SMetaInl
		       targets leaf limit::long stack::pair-nil pmethods prgm conf)
   (with-access::J2SMetaInl this (inlstack stmt loc)
      (set! stmt
	 (inline! stmt
	    targets leaf limit (append inlstack stack) pmethods prgm conf))
      this))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SDeclFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SDeclFun
		       targets leaf limit::long stack::pair-nil pmethods prgm conf)
   (with-access::J2SDeclFun this (val id)
      (inline! val targets leaf limit stack pmethods prgm conf)
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SFun
		       targets leaf limit::long stack::pair-nil pmethods prgm conf)
   (with-access::J2SFun this (optimize body)
      (when optimize
	 (set! body
	    (inline! body
	       targets leaf limit (cons this stack) pmethods prgm conf)))
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SCall ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SCall
		       targets leaf limit::long stack::pair-nil pmethods prgm conf)
   
   (define (find-inline-decl-function this::J2SCall fun arity limit stack)
      (with-access::J2SRef fun (decl)
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (id)
	       (let ((val (j2sdeclinit-val-fun decl)))
		  (when (and (=fx (function-arity val) arity)
			     (function-fxarg? val)
			     (or (not leaf) (function-leaf? val))
			     (not (memq val stack))
			     (<=fx (function-size val) limit)
			     (check-id id))
		     val))))))

   (define (find-inline-methods this fun arity)
      (with-access::J2SAccess fun (obj field)
	 (when (isa? field J2SString)
	    (with-access::J2SString field (val)
	       (let ((mets (filter (lambda (m::struct)
				      (let ((f (protoinfo-method m)))
					 (and (=fx (function-arity f) arity)
					      (function-fxarg? f)
					      (or (not leaf) (function-leaf? f))
					      (not (memq f stack)))))
			      (or (hashtable-get pmethods val) '()))))
		  (when (<fx (apply +
				(map (lambda (m)
					(function-size
					   (protoinfo-method m)))
				   mets))
			   limit)
		     mets))))))

   (define (inline-access-call this::J2SCall fun::J2SAccess args loc)
      (let ((mets (find-inline-methods this fun (length args))))
	 (when (pair? mets)
	    (let* ((mets (if (pair? targets)
			     (filter (lambda (t) (memq t mets)) targets)
			     mets))
		   (vals (map protoinfo-method mets))
		   (sz (apply + (map function-size vals))))
	       (inline-verb loc fun (length mets) sz limit conf)
	       (when (pair? stack) 
		  (invalidate-function-size! (car stack)))
	       (let ((e (inline-method-call fun mets args loc
			   '() leaf limit stack pmethods prgm conf)))
		  (inline-stmt->expr loc
		     (inline! e
			'() leaf 0 (append vals stack) pmethods prgm conf)))))))
   
   (define (inline-ref-call this::J2SCall fun::J2SRef thisarg args loc)
      (cond
	 ((find-inline-decl-function this fun (length args) limit stack)
	  =>
	  (lambda (target)
	     (inline-verb loc fun 0 (function-size target) limit conf)
	     (when (pair? stack)
		(invalidate-function-size! (car stack)))
	     (inline-stmt->expr loc
		(inline-function-call target thisarg args loc
		   targets leaf (if targets 0 limit) stack pmethods prgm conf))))
	 ((pair? targets)
	  (when (pair? stack)
	     (invalidate-function-size! (car stack)))
	  (inline-stmt->expr loc
	     (inline-unknown-call fun thisarg  args loc
		targets leaf limit stack pmethods prgm conf)))
	 (else
	  #f)))

   (define (inline-expr-call this fun thisarg args loc)
      (let ((decl (J2SDeclInit '(ref) (gensym '%fun) fun)))
	 (inline-stmt->expr loc
	    (J2SLetBlock (list decl)
	       (inline-ref-call this (J2SRef decl) thisarg args loc)))))
   
   (with-access::J2SCall this (fun thisarg args type loc cache)
      (cond
	 (cache (call-default-walker))
	 ((isa? fun J2SAccess) (or (inline-access-call this fun args loc) this))
	 ((isa? fun J2SRef) (or (inline-ref-call this fun thisarg args loc) this))
	 ((pair? targets) (or (inline-expr-call this fun thisarg args loc) this))
	 (else this))))

;*---------------------------------------------------------------------*/
;*    inline-function-call ...                                         */
;*---------------------------------------------------------------------*/
(define (inline-function-call val::J2SFun thisarg args::pair-nil loc
	   targets leaf limit::long stack::pair-nil pmethods prgm conf)
   (with-access::J2SFun val (body thisp params (floc loc))
      (let* ((vals (inline-args (cons thisp params)
		      (if (pair? thisarg)
			  (append thisarg args)
			  (cons (J2SUndefined) args))
		      #f leaf limit stack pmethods prgm conf))
	     (nbody (j2s-alpha body (cons thisp params) vals)))
	 (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
	    (J2SMetaInl (cons val stack)
	       (config-get conf :optim 0)
	       (if (> limit 0)
		   (inline! nbody
		      #f leaf limit (cons val stack) pmethods prgm conf)
		   nbody))))))

;*---------------------------------------------------------------------*/
;*    inline-unknown-call ...                                          */
;*---------------------------------------------------------------------*/
(define (inline-unknown-call ref::J2SRef thisarg args::pair-nil loc
	   targets leaf limit::long stack::pair-nil pmethods prgm conf)
   (let loop ((targets targets))
      (if (null? targets)
	  (J2SStmtExpr (J2SCall* ref args))
	  (let* ((target (car targets))
		 (fun (targetinfo-fun target)))
	     (if (< (function-size fun) limit)
		 (begin
		    (inline-verb loc fun 0 (function-size fun) limit conf)
		    (J2SIf (J2SHopCall (J2SHopRef/rtype 'eq? 'bool)
			      (J2SRef (with-access::J2SRef ref (decl) decl))
			      (J2SRef (targetinfo-decl target)))
		       (inline-function-call fun thisarg args loc
			  #f leaf 0 stack pmethods prgm conf)
		       (loop (cdr targets))))
		 (loop '()))))))
   
;*---------------------------------------------------------------------*/
;*    inline-method-call ...                                           */
;*---------------------------------------------------------------------*/
(define (inline-method-call fun::J2SAccess callees::pair args::pair-nil loc
	   targets leaf limit::long stack::pair-nil pmethods prgm conf)
   
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
	  (let ((fun (gensym '%met)))
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
			   (inline! a
			      #f leaf limit stack pmethods prgm conf))))))
	 args))

   (define (inline-method obj::J2SRef field callee args cache loc kont)
      (let ((val (protoinfo-method callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (let ((vals (inline-args params args
			   #f leaf limit stack pmethods prgm conf)))
	       (with-access::J2SRef obj (decl)
		  (cache-check cache loc obj field kont
		     (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
			(J2SMetaInl (cons val stack)
			   (config-get conf :optim 0)
			   (inline!
			      (j2s-alpha body
				 (cons thisp params) (cons decl vals))
			      #f leaf limit
			      (cons val stack) pmethods prgm conf)))))))))
   
   (define (inline-object-method-call-PAS-BON fun obj args)
      (with-access::J2SAccess fun (field cspecs)
	 (tprint "ICI PAS bON LE CALL...qui fait un cache miss")
	 (set! cspecs '(pmap vtable-method))
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

   (define (inline-object-method-call-cache-proto fun obj args)
      (with-access::J2SAccess fun (field cspecs)
	 (let loop ((callees callees)
		    (caches '()))
	    (if (null? callees)
		(let* ((c (get-cache prgm))
		       (r (J2SLetOpt '(call) (gensym 'r)
			     (J2SMethodCall/cache* fun (list obj) args '(pmap-inline vtable-inline) c))))
		   (J2SLetRecBlock #f (list r)
		      (let loop ((cs caches))
			 (if (null? cs)
			     (J2SSeq*
				(map (lambda (c)
					(J2SStmtExpr
					   (J2SCacheUpdate 'proto-reset
					      (car c) obj)))
				   caches))
			     (let ((v (get-svar (cdar cs))))
				(J2SIf
				   (J2SCacheCheck 'method
				      c (J2SHopRef v))
				   (J2SSeq*
				      (map (lambda (c)
					      (J2SStmtExpr
						 (if (eq? c (car cs))
						     (J2SCacheUpdate 'proto-method
							(car c) obj)
						     (J2SCacheUpdate 'proto-reset
							(car c) obj))))
					 caches))
				   (loop (cdr cs))))))
		      (J2SReturn #t (J2SRef r))))
		(let ((cache (get-cache prgm)))
		   (inline-method obj field (car callees) args cache loc
		      (lambda ()
			 (loop (cdr callees)
			    (cons (cons cache (car callees)) caches)))))))))

   (define (inline-object-method-call-UNUSED fun obj args)
      (with-access::J2SAccess fun (field cspecs)
	 (let* ((c (get-cache prgm))
		(f (J2SLetOpt '(call) (gensym 'f)
		      (J2SAccess/cache obj field c '(pmap vtable)))))
	    (J2SLetRecBlock #f (cons f (filter (lambda (b) (isa? b J2SDecl)) args))
	       (let loop ((callees callees))
		  (if (null? callees)
		      (J2SReturn #t
			 (J2SMethodCall* (J2SRef f) (list obj)
			    (map (lambda (a) (if (isa? a J2SDecl) (J2SRef a) a)) args)))
		      (let ((v (get-svar (car callees))))
			 (J2SIf (J2SBinary '=== (J2SRef f) (J2SHopRef v))
			    (let ((val (protoinfo-method (car callees))))
			       (with-access::J2SFun val (body thisp params (floc loc))
				  (with-access::J2SRef obj (decl)
				     (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) args)
					(J2SMetaInl (cons val stack)
					   (config-get conf :optim 0)
					   (inline!
					      (j2s-alpha body
						 (cons thisp params) (cons decl args))
					      #f leaf limit
					      (cons val stack) pmethods prgm conf))))))
			    (loop (cdr callees))))))))))

   (define (inline-object-method-call fun obj args)
      (with-access::J2SAccess fun (field cspecs)
	 (let loop ((callees callees)
		    (caches '()))
	    (if (null? callees)
		(let* ((c (get-cache prgm))
		       (r (J2SLetOpt '(call) (gensym 'r)
			     (J2SMethodCall/cache* fun (list obj) args '(pmap-inline vtable-inline) c))))
		   (J2SLetRecBlock #f (list r)
		      (let loop ((cs caches))
			 (if (null? cs)
			     (J2SSeq*
				(map (lambda (c)
					(J2SStmtExpr
					   (J2SCacheUpdate 'proto-reset
					      (car c) obj)))
				   caches))
			     (let ((v (get-svar (cdar cs))))
				(J2SIf
				   (J2SCacheCheck 'method
				      c (J2SHopRef v))
				   (J2SSeq*
				      (map (lambda (c)
					      (J2SStmtExpr
						 (if (eq? c (car cs))
						     (J2SCacheUpdate 'proto-method
							(car c) obj)
						     (J2SCacheUpdate 'proto-reset
							(car c) obj))))
					 caches))
				   (loop (cdr cs))))))
		      (J2SReturn #t (J2SRef r))))
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
	    ((not (eq? (j2s-type obj) 'object))
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
(define (inline-args params args targets leaf limit stack pmethods prgm conf)
   (map (lambda (p a)
	   (cond
	      ((and (ronly-variable? p) (isa? a J2SLiteral))
	       a)
	      ((and (ronly-variable? p) (isa? a J2SRef) (ronly-variable? a))
	       a)
	      (else
	       (with-access::J2SDecl p (usage id writable)
		  (with-access::J2SNode a (loc)
		     (let ((d (J2SLetOpt usage (gensym id)
				 (inline! a
				    targets leaf limit stack pmethods prgm conf))))
			(with-access::J2SDecl d ((w writable))
			   (set! w writable))
			d))))))
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
;*    node-leaf ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-leaf node::J2SNode cell)
   (when (cell-ref cell)
      (call-default-walker))
   (cell-ref cell))

;*---------------------------------------------------------------------*/
;*    node-leaf ::J2SCall ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-leaf node::J2SCall cell)
   (cell-set! cell #f)
   #f)

;*---------------------------------------------------------------------*/
;*    node-leaf ::J2SNew ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-leaf node::J2SNew cell)
   (cell-set! cell #f)
   #f)

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
      (unmetainl! stmt)))

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
		      (let loop ((obj obj))
			 (cond
			    ((isa? obj J2SRef)
			     (with-access::J2SRef obj (decl)
				(with-access::J2SDecl decl (id)
				   (display id))))
			    ((isa? obj J2SAccess)
			     (with-access::J2SAccess obj (obj field)
				(loop obj)
				(display ".")
				(if (isa? field J2SString)
				    (with-access::J2SString field (val)
				       (display val))
				    (display "..."))))))
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


;*---------------------------------------------------------------------*/
;*    dead-inner-decl! ::J2SNode ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-inner-decl! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    dead-inner-decl! ::J2SDeclInit ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-inner-decl! this::J2SDeclInit)
   
   (define (simple-expr? val::J2SExpr)
      (or (isa? val J2SLiteralCnst)
	  (isa? val J2SFun)
	  (isa? val J2SRef)))
   
   (with-access::J2SDeclInit this (usecnt loc id key val)
      (if (and (=fx usecnt 0) (simple-expr? val))
	  (J2SNop)
	  (begin
	     (call-default-walker)
	     this))))

;*---------------------------------------------------------------------*/
;*    dead-inner-decl! ::J2SLetBlock ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-inner-decl! this::J2SLetBlock)
   (call-default-walker)
   (with-access::J2SLetBlock this (decls)
      (set! decls (filter (lambda (d) (isa? d J2SDecl)) decls))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-count-calls! ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-count-calls! this::J2SProgram conf)
   (with-access::J2SProgram this (call-size)
      (let ((count (make-cell 0)))
	 (count-call this count)
	 (set! call-size (cell-ref count)))))

;*---------------------------------------------------------------------*/
;*    count-call ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (count-call this::J2SNode count)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    count-call ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (count-call this::J2SCall count)
   (with-access::J2SCall this (profid)
      (let ((id (cell-ref count)))
	 (cell-set! count (+fx id 1))
	 (call-default-walker)
	 (set! profid id))))

;*---------------------------------------------------------------------*/
;*    collect-funs* ::J2SNode ...                                      */
;*    -------------------------------------------------------------    */
;*    Collect all the function definitions.                            */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-funs* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-funs* ::J2SDeclFun ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-funs* this::J2SDeclFun)
   (with-access::J2SDeclFun this (val)
      (with-access::J2SFun val (loc)
	 (if (pair? loc)
	     (list (cons (caddr loc) (targetinfo this val)))
	     '()))))
   
;*---------------------------------------------------------------------*/
;*    collect-calls-and-link* ::J2SNode ...                            */
;*    -------------------------------------------------------------    */
;*    Link the call nodes to their parents and return the list of      */
;*    all calls.                                                       */
;*---------------------------------------------------------------------*/
(define-generic (collect-calls-and-link*::pair-nil this parent::J2SNode owner)
   (if (pair? this)
       (append-map (lambda (n) (collect-calls-and-link* n parent owner)) this)
       '()))

;*---------------------------------------------------------------------*/
;*    collect-calls-and-link* ::J2SNode ...                            */
;*---------------------------------------------------------------------*/
(define-method (collect-calls-and-link* this::J2SNode parent::J2SNode owner)
   (let ((fields (class-all-fields (object-class this))))
      (let loop ((i (-fx (vector-length fields) 1))
		 (c '()))
	 (if (=fx i -1)
	     c
	     (let* ((f (vector-ref fields i))
		    (info (class-field-info f)))
		(if (and (pair? info) (member "ast" info))
		    (loop (-fx i 1)
		       (append
			  (collect-calls-and-link*
			     ((class-field-accessor f) this) this owner)
			  c))
		    (loop (-fx i 1)
		       c)))))))

;*---------------------------------------------------------------------*/
;*    collect-calls-and-link* ::J2SFun ...                             */
;*---------------------------------------------------------------------*/
(define-method (collect-calls-and-link* this::J2SFun parent owner)
   (set! owner this)
   (set! parent this)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    collect-calls-and-link* ::J2SCall ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-calls-and-link* this::J2SCall parent::J2SNode owner)
   (with-access::J2SCall this (fun thisarg args %info loc)
      (set! %info parent)
      (let ((next (append
		     (collect-calls-and-link* fun this owner)
		     (collect-calls-and-link* thisarg this owner)
		     (collect-calls-and-link* args this owner))))
	 (if loc
	     (cons (cons (caddr loc) (nodelink (list this) owner)) next)
	     next))))

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
	     (error "update-parent!" "Cannot find node" (j2s->list old))
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

;*---------------------------------------------------------------------*/
;*    call-profile-log ...                                             */
;*---------------------------------------------------------------------*/
(define (call-profile-log this::J2SProgram logfile conf)
   
   (define (get key lst)
      (let ((c (assq key lst)))
	 (when (pair? c) (cdr c))))

   (define (sum-method-count c)
      (let loop ((i (-fx (vector-length c) 1))
		 (s #l0))
	 (if (<fx i 0)
	     s
	     (let ((e (vector-ref c i)))
		(let ((cnt (get 'cnt e)))
		   (loop (-fx i 1) (+ s cnt)))))))

   (let* ((log (load-profile-log logfile))
	  (srcs (get 'calls log))
	  (file (config-get conf :filename)))
      (when (vector? srcs)
	 (when (>=fx (config-get conf :verbose 0) 3)
	    (with-output-to-port (current-error-port)
	       (lambda ()
		  (display "\n      ")
		  (display* "loading log file "
		     (string-append "\"" logfile "\"...")))))
	 (let loop ((i (-fx (vector-length srcs) 1)))
	    (when (>=fx i 0)
	       (let ((filename (get 'filename (vector-ref srcs i))))
		  (if (string=? file filename)
		      (let ((verb (make-cell 0))
			    (cvec (get 'calls (vector-ref srcs i))))
			 (vector-map! (lambda (c)
					 (let ((cnt (get 'cnt c)))
					    (list (get 'point c)
					       (if (vector? cnt)
						   (sum-method-count cnt)
						   cnt)
					       (if (vector? cnt)
						   (vector-map! (lambda (c)
								   (cons (get 'cnt c)
								      (get 'point c)))
						      cnt)
						   #f))))
			    cvec)
			 (sort (lambda (x y) (>= (cadr x) (cadr y))) cvec))
		      (loop (-fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    load-profile-log ...                                             */
;*---------------------------------------------------------------------*/
(define (load-profile-log logfile)

   (define (counts-val val)
      (vector-map! (lambda (v)
		      (cond
			 ((vector? v)
			  (vector-map! (lambda (c)
					  (match-case c
					     (((loc . ?loc) (cnt . ?cnt))
					      (cons cnt loc))
					     (((cnt . ?cnt) (loc . ?loc))
					      (cons cnt loc))
					     (else
					      (error "load-profile-log"
						 "bad count value" c))))
			     v))
			 ((number? v)
			  v)
			 (else
			  (error "load-profile-log" "bad count value" v))))
	 val))

   (call-with-input-file logfile
      (lambda (ip)
	 (let ((fprofile #f))
	    (json-parse ip
	       :array-alloc (lambda () (make-cell '()))
	       :array-set (lambda (a i val)
			     (cell-set! a (cons val (cell-ref a))))
	       :array-return (lambda (a i)
				(list->vector (reverse! (cell-ref a))))
	       :object-alloc (lambda ()
				(make-cell '()))
	       :object-set (lambda (o p val)
			      (cond
				 ((string=? p "calls")
				  (unless fprofile
				     (error "fprofile" "Wrong log format" logfile))
				  (cell-set! o
				     (cons (cons 'calls val)
					(cell-ref o))))
				 ((string=? p "format")
				  (set! fprofile (equal? val "fprofile")))
				 ((string=? p "counts")
				  (cell-set! o
				     (cons (cons 'counts (counts-val val))
					(cell-ref o))))
				 (else
				  (cell-set! o
				     (cons (cons (string->symbol p) val)
					(cell-ref o))))))
	       :object-return (lambda (o)
				 (reverse! (cell-ref o)))
	       :parse-error (lambda (msg fname loc)
			       (error/location "fprofile" "Wrong JSON file" msg
				  fname loc)))))))
