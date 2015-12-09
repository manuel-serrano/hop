;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/letopt.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 06:35:14 2015                          */
;*    Last change :  Wed Dec  9 08:31:45 2015 (serrano)                */
;*    Copyright   :  2015 Manuel Serrano                               */
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
      (for-each (lambda (o) (j2s-letopt! o)) headers)
      (for-each (lambda (o) (j2s-letopt! o)) decls)
      (for-each (lambda (o) (j2s-letopt! o)) nodes)
      ;; toplevel lets optimization
      (let ((lets '())
	    (vars '()))
	 (for-each (lambda (x)
		      (cond
			 ((isa? x J2SLet)
			  (set! lets (cons x lets)))
			 ((isa? x J2SDecl)
			  (set! vars (cons x vars)))
			 (else
			  (error "j2s-letopt" "internal error" (j2s->list x)))))
	    decls)
	 (when (pair? lets)
	    ;; this modify nodes in place
	    (set! nodes (j2s-toplevel-letopt! nodes (reverse! lets) vars)))))
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
			       (when (isa? expr J2SInitLet)
				  expr))))
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
(define (init-decl::J2SDecl this::J2SInitLet)
   (with-access::J2SInitLet this (lhs)
      (with-access::J2SRef lhs (decl)
	 decl)))

;*---------------------------------------------------------------------*/
;*    j2s-letopt! ::J2SLetBlock ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-letopt! this::J2SLetBlock)
   (with-access::J2SLetBlock this (decls nodes)
      ;; reseting the occurrence counters which are used an set below
      (reset-use! decls)
      ;; start iterating over all the LetBlock statements to find
      ;; the first decl 
      (let loop ((n nodes))
	 (cond
	    ((null? n)
	     ;; should never be reached
	     this)
	    ((null? (get-let-inits (car n)))
	     ;; not an init statement, count the number
	     ;; of used variables
	     (use-count (car n))
	     ;; optimize recursively
	     (j2s-letopt! (car n))
	     ;; keep optimizing the current let block
	     (loop (cdr n)))
	    ((all-unused? decls)
	     ;; got an initialization block, which can be
	     ;; optimized
	     (if (eq? n nodes)
		 ;; the let init started the let-block
		 (tail-let! this this)
		 ;; re-organize the let-block to place this inits
		 ;; at the head of a fresh let
		 (tail-let! this (head-let! this n))))
	    (else
	     ;; give up optimizing
	     (for-each j2s-letopt! n)
	     this)))))

;*---------------------------------------------------------------------*/
;*    head-let! ...                                                    */
;*    -------------------------------------------------------------    */
;*    Modify the letblock so its first statement is a declaration.     */
;*---------------------------------------------------------------------*/
(define (head-let! this::J2SLetBlock head::pair-nil)
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
	     (loop (cdr n) (cons (car n) prev))))))

;*---------------------------------------------------------------------*/
;*    tail-let! ...                                                    */
;*---------------------------------------------------------------------*/
(define (tail-let! this::J2SLetBlock resnode::J2SStmt)
   
   (define (init-let->let::J2SLet this::J2SInitLet)
      (with-access::J2SInitLet this (rhs loc lhs)
	 (with-access::J2SRef lhs (decl)
	    (duplicate::J2SLet decl))))
   
   (define (statement->expression node)
      ;; transform a statement into a expression
      (with-access::J2SNode node (loc)
	 (instantiate::J2SExprStmt
	    (loc loc)
	    (stmt node))))
   
   (define (new-let-opt node::J2SStmt)
      ;; create a new declaration for the statement
      (with-access::J2SNode node (loc)
	 (instantiate::J2SLetOpt 
	    (loc loc)
	    (id (gensym))
	    (ronly #t)
	    (writable #f)
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
		(reverse! ninits))
	     (set! nodes
		(append! (map! j2s-letopt! (reverse! nbody))
		   (map! j2s-letopt! n)))
	     resnode)
	    ((get-inits (car n))
	     =>
	     (lambda (inits)
		;; a letinit node
		(let liip ((inits inits)
			   (decls decls)
			   (ninits ninits)
			   (nbody nbody)
			   (disabled disabled)
			   (deps deps))
		   (cond
		      ((null? inits)
		       (loop (cdr n) decls ninits nbody disabled deps))
		      ((memq (init-decl (car inits)) disabled)
		       ;; cannot optimize...
		       (with-access::J2SInitLet (car inits) (rhs)
			  (let* ((init (car inits))
				 (decl (init-decl init))
				 (used (get-used-decls rhs decls))
				 (ninit (init-let->let init)))
			     (liip (cdr inits)
				(remq decl ndecls)
				(append ninits (list ninit))
				(cons init nbody)
				disabled
				(cons (cons decl used) deps)))))
		      (else
		       (with-access::J2SInitLet (car inits) (rhs)
			  (let ((used (get-used-decls rhs decls))
				(init (car inits)))
			     (cond
				((null? used)
				 ;; optimize this binding
				 (let ((decl (init-decl init))
				       (optinit (init-let->let-opt init)))
				    (liip (cdr inits)
				       (remq decl ndecls)
				       (cons optinit ninits)
				       nbody
				       disabled deps)))
				((isa? rhs J2SFun)
				 ;; optimize this binding but keep tracks
				 ;; of its dependencies
				 (let ((decl (init-decl init))
				       (optinit (init-let->let-opt init)))
				    (liip (cdr inits)
				       (remq decl ndecls)
				       (cons optinit ninits)
				       nbody
				       disabled
				       (cons (cons decl used) deps))))
				(else
				 ;; keep a regular binding an mark
				 ;; its dependencies
				 (let* ((decl (init-decl init))
					(used (get-used-decls rhs decls))
					(ninit (init-let->let init))
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
		       (ndecl (new-let-opt (car n)))
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
			  deps)))))))))

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
		    (with-access::J2SInitLet (car inits) (rhs)
		       (let ((used (get-used-decls rhs (append decls vars)))
			     (init (car inits)))
			  (cond
			     ((null? used)
			      ;; optimize this binding
			      (init-let->let-opt init)
			      (let ((ndecls (remq (init-decl init) decls)))
				 (liip (cdr inits) ndecls deps res)))
			     ((isa? rhs J2SFun)
			      ;; optimize this binding but keep tracks
			      ;; of its dependencies
			      (init-let->let-opt init)
			      (let ((ndecls (remq (init-decl init) decls)))
				 (liip (cdr inits) ndecls
				    (cons (cons init used) deps)
				    res)))
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
;*    init-let->let-opt ...                                            */
;*---------------------------------------------------------------------*/
(define (init-let->let-opt::J2SLetOpt this::J2SInitLet)
   (with-access::J2SInitLet this (rhs loc lhs)
      (with-access::J2SRef lhs (decl)
	 (with-access::J2SLet decl (id)
	    (widen!::J2SLetOpt decl
	       (val (j2s-letopt! rhs)))))))
