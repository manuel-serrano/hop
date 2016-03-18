;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/letopt.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 06:35:14 2015                          */
;*    Last change :  Fri Mar 18 08:28:18 2016 (serrano)                */
;*    Copyright   :  2015-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Let optimisation                                                 */
;*    -------------------------------------------------------------    */
;*    This implements the Let optimisation. When possible, it replaces */
;*    LetInit with LetOpt nodes, which are much more efficient because */
;*    they are potentially implemented as registers while LetInit are  */
;*    always implemented as boxed variables.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_letopt

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer
	   __js2scheme_use)

   (static (class J2SLetOptInit
	      (declinit::J2SDeclInit read-only)))
   
   (export j2s-letopt-stage
	   (generic j2s-letopt ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-letopt-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-letopt-stage
   (instantiate::J2SStageProc
      (name "letopt")
      (comment "Allocate let/const variables to registers")
      (proc j2s-letopt)
      (optional #t)))

;*---------------------------------------------------------------------*/
;*    j2s-letopt ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (j2s-letopt this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-letopt ::J2SProgram ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-letopt this::J2SProgram args)
   (with-access::J2SProgram this (nodes headers decls)
      ;; statement optimization
      (for-each (lambda (o) (j2s-update-ref! (j2s-letopt! o))) headers)
      (for-each (lambda (o) (j2s-update-ref! (j2s-letopt! o))) decls)
      (for-each (lambda (o) (j2s-update-ref! (j2s-letopt! o))) nodes)
      ;; toplevel lets optimization
      (let ((lets '())
	    (vars '()))
	 (for-each (lambda (x)
		      (cond
			 ((not (isa? x J2SDecl))
			  (error "j2s-letopt" "internal error" (j2s->list x)))
			 ((j2s-let? x)
			  (set! lets (cons x lets)))
			 (else
			  (set! vars (cons x vars)))))
	    decls)
	 (when (pair? lets)
	    ;; this modify nodes in place
	    (set! nodes
	       (map! j2s-update-ref!
		  (j2s-toplevel-letopt! nodes (reverse! lets) vars))))))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-update-ref! ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-update-ref! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-update-ref! ::J2SRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-update-ref! this::J2SRef)
   ;; patch the optimized ref nodes
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info)
	 (when (isa? %info J2SLetOptInit)
	    (with-access::J2SLetOptInit %info (declinit)
	       (set! decl declinit)))))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-letopt! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-letopt! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-use! ...                                                   */
;*---------------------------------------------------------------------*/
(define (reset-use! decls::pair-nil)
   (for-each (lambda (decl)
		(with-access::J2SDecl decl (usecnt)
		   (set! usecnt 0)))
      decls))

;*---------------------------------------------------------------------*/
;*    all-unused? ...                                                  */
;*---------------------------------------------------------------------*/
(define (all-unused?::bool decls::pair-nil)
   (every (lambda (d::J2SDecl)
	     (with-access::J2SDecl d (usecnt)
		(=fx usecnt 0)))
      decls))

;*---------------------------------------------------------------------*/
;*    get-let-inits ...                                                */
;*    -------------------------------------------------------------    */
;*    Extract the list of let-declarations of a statement.             */
;*---------------------------------------------------------------------*/
(define (get-let-inits::pair-nil node::J2SStmt)
   (if (not (isa? node J2SSeq))
       '()
       (with-access::J2SSeq node (nodes)
	  (filter-map (lambda (n::J2SStmt)
			 (when (isa? n J2SStmtExpr)
			    (with-access::J2SStmtExpr n (expr)
			       (when (isa? expr J2SInit)
				  (when (j2s-let? (init-decl expr))
				     expr)))))
	     nodes))))

;*---------------------------------------------------------------------*/
;*    get-inits ...                                                    */
;*    -------------------------------------------------------------    */
;*    As GET-LET-INITS but returns #f if no init found (easier with    */
;*    COND forms).						       */
;*---------------------------------------------------------------------*/
(define (get-inits::obj node::J2SNode)
   (let ((inits (get-let-inits node)))
      (when (pair? inits)
	 inits)))

;*---------------------------------------------------------------------*/
;*    init-decl ...                                                    */
;*---------------------------------------------------------------------*/
(define (init-decl::J2SDecl this::J2SInit)
   (with-access::J2SInit this (lhs)
      (with-access::J2SRef lhs (decl)
	 decl)))

;*---------------------------------------------------------------------*/
;*    j2s-letopt! ::J2SBlock ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-letopt! this::J2SBlock)
   (call-default-walker)
   (with-access::J2SBlock this (nodes)
      ;; check the pattern (j2sblock decl decl ... decl j2sletblock)
      (let loop ((lnodes nodes)
		 (vdecls '()))
	 (cond
	    ((null? lnodes)
	     this)
	    ((isa? (car lnodes) J2SDecl)
	     (loop (cdr lnodes) (cons (car lnodes) vdecls)))
	    ((isa? (car lnodes) J2SLetBlock)
	     (when (pair? vdecls)
		;; merge the decls into the j2sletblock
		(with-access::J2SLetBlock (car lnodes) (decls)
		   (for-each (lambda (decl)
				(when (isa? decl J2SDeclFun)
				   (with-access::J2SDeclFun decl (scope)
				      (set! scope 'letblock))))
		      vdecls)
		   (set! decls (append vdecls decls))))
	     (with-access::J2SLetBlock (car lnodes) (nodes)
		(set! nodes (append nodes (cdr lnodes))))
	     (car lnodes))
	    (else
	     this)))))

;*---------------------------------------------------------------------*/
;*    j2s-letopt! ::J2SLetBlock ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-letopt! this::J2SLetBlock)
   (with-access::J2SLetBlock this (decls nodes)
      ;; reseting the occurrence counters which are used and set below
      (reset-use! decls)
      ;; start iterating over all the LetBlock statements to find
      ;; the first decl
      (with-trace 'j2s-letopt "j2s-letopt!"
	 (trace-item "" (j2s->list this))
	 (let loop ((n nodes))
	    (cond
	       ((null? n)
		;; should never be reached
		(trace-item "<<<.1 " (j2s->list this))
		this)
	       ((null? (get-let-inits (car n)))
		(trace-item "---.2 " (j2s->list (car n)))
		;; not an init statement, count the number
		;; of used variables
		(use-count (car n))
		;; optimize recursively
		(set-car! n (j2s-letopt! (car n)))
		;; keep optimizing the current let block
		(loop (cdr n)))
	       ((all-unused? decls)
		;; got an initialization block, which can be
		;; optimized
		(if (eq? n nodes)
		    ;; the let init started the let-block
		    (let ((res (tail-let! this this)))
		       (trace-item "<<<.3a " (j2s->list res))
		       res)
		    ;; re-organize the let-block to place this inits
		    ;; at the head of a fresh let
		    (let ((res (tail-let! this (head-let! this n))))
		       (trace-item "<<<.3b " (j2s->list res))
		       res)))
	       (else
		(trace-item "---.4 " (j2s->list this))
		;; give up optimizing
		(for-each j2s-letopt! n)
		(trace-item "<<<.4 " (j2s->list this))
		this))))))

;*---------------------------------------------------------------------*/
;*    head-let! ...                                                    */
;*    -------------------------------------------------------------    */
;*    Modify the letblock so its first statement is a declaration.     */
;*---------------------------------------------------------------------*/
(define (head-let! this::J2SLetBlock head::pair-nil)
   (with-trace 'j2s-letopt "head-let!"
      (trace-item "" (j2s->list this))
      (with-access::J2SLetBlock this (loc endloc decls nodes)
	 (let loop ((n nodes)
		    (prev '()))
	    (if (eq? n head)
		(begin
		   (set! nodes n)
		   (instantiate::J2SBlock
		      (loc loc)
		      (endloc endloc)
		      (nodes (reverse! (cons this prev)))))
		(loop (cdr n) (cons (car n) prev)))))))

;*---------------------------------------------------------------------*/
;*    tail-let! ...                                                    */
;*---------------------------------------------------------------------*/
(define (tail-let! this::J2SLetBlock resnode::J2SStmt)
   
   (define (statement->expression node)
      ;; transform a statement into a expression
      (with-access::J2SNode node (loc)
	 (instantiate::J2SExprStmt
	    (loc loc)
	    (stmt node))))
   
   (define (new-let-opt node::J2SStmt)
      ;; create a new declaration for the statement
      (with-access::J2SNode node (loc)
	 (instantiate::J2SDeclInit
	    (loc loc)
	    (id (gensym))
	    (ronly #t)
	    (writable #f)
	    (binder 'let-opt)
	    (val (statement->expression node)))))
   
   (define (dependencies usedecls deps)
      ;; transitive closure of the depends-on property
      (let loop ((udecls usedecls)
		 (res '()))
	 (cond
	    ((null? udecls)
	     res)
	    ((memq (car udecls) res)
	     (loop (cdr udecls) res))
	    (else
	     (let ((udeps (assq (car udecls) deps)))
		(if (pair? udeps)
		    (loop (append (cdr udeps) (cdr udecls))
		       (cons (car udecls) res))
		    (loop (cdr udecls) (cons (car udecls) res))))))))

   ;; the main optimization loop
   (with-trace 'j2s-letopt "tail-let!"
      (trace-item "this=" (j2s->list this))
      (unless (eq? this resnode)
	 (trace-item "resnode=" (j2s->list resnode)))
      (with-access::J2SLetBlock this (nodes decls loc)
	 (let loop ((n nodes)
		    (ndecls decls)
		    (ninits '())
		    (nbody '())
		    (disabled '())
		    (deps '()))
	    (cond
	       ((or (null? n) (null? ndecls))
		;; all nodes or declarations checked
		(set! decls
		   (map! j2s-letopt! (reverse! ninits)))
		(set! nodes
		   (append! (map! j2s-letopt! (reverse! nbody))
		      (map! j2s-letopt! n)))
		resnode)
	       ((get-inits (car n))
		=>
		(lambda (inits)
		   ;; a letinit node
		   (let liip ((inits inits)
			      (ndecls ndecls)
			      (ninits ninits)
			      (nbody nbody)
			      (disabled disabled)
			      (deps deps))
		      (cond
			 ((null? inits)
			  (loop (cdr n) ndecls ninits nbody disabled deps))
			 ((memq (init-decl (car inits)) disabled)
			  ;; cannot optimize...
			  (with-access::J2SInit (car inits) (rhs)
			     (let* ((init (car inits))
				    (decl (init-decl init))
				    (used (get-used-decls rhs ndecls))
				    (ninit (duplicate::J2SDecl decl)))
				(liip (cdr inits)
				   (remq decl ndecls)
				   (append ninits (list ninit))
				   (cons init nbody)
				   disabled
				   (cons (cons decl used) deps)))))
			 (else
			  (with-access::J2SInit (car inits) (rhs)
			     (let ((used (get-used-decls rhs ndecls))
				   (init (car inits)))
				(cond
				   ((null? used)
				    ;; optimize this binding
				    (let ((decl (init-decl init))
					  (idecl (init->decl-init init)))
				       (liip (cdr inits)
					  (remq decl ndecls)
					  (cons idecl ninits)
					  nbody
					  disabled deps)))
				   ((isa? rhs J2SFun)
				    ;; optimize this binding but keep tracks
				    ;; of its dependencies
				    (let ((decl (init-decl init))
					  (idecl (init->decl-init init)))
				       (liip (cdr inits)
					  (remq decl ndecls)
					  (cons idecl ninits)
					  nbody
					  disabled
					  (cons (cons decl used) deps))))
				   (else
				    ;; keep a regular binding and mark
				    ;; its dependencies
				    (let* ((decl (init-decl init))
					   (ninit (duplicate::J2SDecl decl))
					   (used (get-used-decls rhs ndecls))
					   (body (instantiate::J2SStmtExpr
						    (loc loc)
						    (expr init))))
				       (liip (cdr inits)
					  (remq decl ndecls)
					  (append ninits (list ninit))
					  (cons body nbody)
					  (append used disabled)
					  deps)))))))))))
	       ((null? (cdr n))
		;; this is the last stmt which happens not to be a binding
		(loop (cdr n) ndecls ninits
		   (cons (car n) nbody) disabled deps))
	       (else
		;; a regular statement
		(with-access::J2SNode (car n) (loc)
		   (let* ((used (get-used-decls (car n) decls))
			  (ndecl (new-let-opt (j2s-letopt! (car n))))
			  (ninit ndecl)
			  (ref (instantiate::J2SRef
				  (loc loc)
				  (decl ndecl))))
		      (if (null? used)
			  ;; harmless statement, create a new temp for it
			  (loop (cdr n) ndecls (cons ninit ninits)
			     (cons ref nbody) disabled deps)
			  ;; disable optimization for recursively used decls
			  (loop (cdr n) ndecls (cons ninit ninits)
			     (cons ref nbody)
			     (append (get-used-deps used deps) disabled)
			     deps))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-toplevel-letopt! ...                                         */
;*    -------------------------------------------------------------    */
;*    Optimize toplevel let declarations.                              */
;*---------------------------------------------------------------------*/
(define (j2s-toplevel-letopt! nodes::pair-nil decls::pair-nil vars::pair-nil)
   
   (define (remove-disabled!::pair-nil decls::pair-nil disabled::pair-nil)
      (for-each (lambda (d)
		   (set! decls (remq! d decls)))
	 disabled)
      decls)
   
   (let loop ((n nodes)
	      (decls decls)
	      (deps '())
	      (res '()))
      (cond
	 ((null? n)
	  (reverse! res))
	 ((null? decls)
	  (append (reverse! res) n))
	 ((get-inits (car n))
	  =>
	  (lambda (inits)
	     ;; a letinit node
	     (let liip ((inits inits)
			(decls decls)
			(deps deps)
			(res res))
		(if (null? inits)
		    (loop (cdr n) decls deps res)
		    (with-access::J2SInit (car inits) (rhs)
		       (let ((used (get-used-decls rhs (append decls vars)))
			     (init (car inits)))
			  (cond
			     ((null? used)
			      ;; optimize this binding
			      (let ((decl (init-decl init)))
				 (with-access::J2SInit init (rsh)
				    (with-access::J2SDeclInit decl (binder val)
				       (set! val rhs)
				       (set! binder 'let-opt)))
				 (let ((ndecls (remq decl decls)))
				    (liip (cdr inits) ndecls deps res))))
			     ((isa? rhs J2SFun)
			      ;; optimize this binding but keep tracks
			      ;; of its dependencies
			      (let ((decl (init-decl init)))
				 (with-access::J2SInit init (rhs)
				    (with-access::J2SDeclInit decl (binder val)
				       (set! val rhs)
				       (set! binder 'let-opt)))
				 (let ((ndecls (remq decl decls)))
				    (liip (cdr inits) ndecls
				       (cons (cons init used) deps)
				       res))))
			     (else
			      ;; optimize this binding but mark that
			      ;; the variables it uses are disabled
			      (let ((decl (init-decl init))
				    (used (get-used-decls rhs (append decls vars)))
				    (disabled (get-used-deps used deps)))
				 (liip (cdr inits)
				    (remove-disabled! decls disabled)
				    deps
				    (cons (car n) res)))))))))))
	 ((null? (cdr n))
	  ;; this is the last stmt which happens not to be a binding
	  (reverse! (cons (car n) res)))
	 (else
	  ;; a regular statement
	  (with-access::J2SNode (car n) (loc)
	     (let ((used (get-used-decls (car n) (append decls vars))))
		(if (null? used)
		    ;; harmless statement, ignore it
		    (loop (cdr n) decls deps (cons (car n) res))
		    ;; disable optimization for recursively used decls
		    (let ((disabled (get-used-deps used deps)))
		       (loop (cdr n)
			  (remove-disabled! decls disabled)
			  deps
			  (cons (car n) res))))))))))

;*---------------------------------------------------------------------*/
;*    get-used-decls ...                                               */
;*    -------------------------------------------------------------    */
;*    Amongst DECLS, returns those that appear in NODE.                */
;*---------------------------------------------------------------------*/
(define (get-used-decls node::J2SNode decls)
   ;; reset all decls
   (for-each (lambda (d::J2SDecl)
		(with-access::J2SDecl d (usecnt)
		   (set! usecnt 0)))
      decls)
   ;; count all variables
   (use-count node)
   ;; amongst the decls, returns those that are used
   (filter (lambda (d)
	      (with-access::J2SDecl d (usecnt)
		 (>fx usecnt 0)))
      decls))

;*---------------------------------------------------------------------*/
;*    get-used-deps ...                                                */
;*    -------------------------------------------------------------    */
;*    transitive closure of the depends-on property                    */
;*---------------------------------------------------------------------*/
(define (get-used-deps usedecls deps)
   (let loop ((udecls usedecls)
	      (res '()))
      (cond
	 ((null? udecls)
	  res)
	 ((memq (car udecls) res)
	  (loop (cdr udecls) res))
	 (else
	  (let ((udeps (assq (car udecls) deps)))
	     (if (pair? udeps)
		 (loop (append (cdr udeps) (cdr udecls))
		    (cons (car udecls) res))
		 (loop (cdr udecls) (cons (car udecls) res))))))))

;*---------------------------------------------------------------------*/
;*    init->decl-init ...                                              */
;*---------------------------------------------------------------------*/
(define (init->decl-init::J2SDeclInit this::J2SInit)
   
   (define (decl->declinit::J2SDecl decl::J2SDecl val)
      (with-access::J2SDecl decl (loc id %info)
	 (let ((new (instantiate::J2SDeclInit
		       (id id)
		       (loc loc)
		       (key -1)
		       (val val)))
	       (fields (class-all-fields J2SDecl)))
	    (let loop ((i (-fx (vector-length fields) 1)))
	       (when (>=fx i 0)
		  (let* ((f (vector-ref fields i))
			 (get (class-field-accessor f))
			 (set (class-field-mutator f)))
		     (when set
			(set new (get decl))
			(loop (-fx i 1))))))
	    (with-access::J2SDecl new (binder scope key)
	       (set! key (ast-decl-key))
	       (set! scope 'local)
	       (set! binder 'let-opt))
	    (set! %info (instantiate::J2SLetOptInit (declinit new)))
	    new)))
   
   (with-access::J2SInit this (rhs loc lhs)
      (with-access::J2SRef lhs (decl)
	 (with-access::J2SDecl decl (id)
	    (decl->declinit decl (j2s-letopt! rhs))))))

