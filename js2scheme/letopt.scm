;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/letopt.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 06:35:14 2015                          */
;*    Last change :  Fri Mar 31 07:44:33 2017 (serrano)                */
;*    Copyright   :  2015-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Let optimisation                                                 */
;*    -------------------------------------------------------------    */
;*    This implements the Let optimisation. When possible, it replaces */
;*    LetInit with LetOpt nodes, which are more efficient as they are  */
;*    potentially implemented as registers while LetInit are always    */
;*    implemented as boxed variables.                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_letopt

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer
	   __js2scheme_use)

   (static (class DeclInfo
	      (optdecl (default #unspecified))
	      (used (default #unspecified))
	      (init (default #unspecified)))

	   (class FunInfo
	      (used::pair-nil (default '()))))
   
   (export j2s-letopt-stage))

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
(define (j2s-letopt this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls)
	 ;; statement optimization
	 (for-each (lambda (o) (j2s-update-ref! (j2s-letopt! o))) headers)
	 (for-each (lambda (o) (j2s-update-ref! (j2s-letopt! o))) decls)
	 (for-each (lambda (o) (j2s-update-ref! (j2s-letopt! o))) nodes)
	 ;; toplevel let optimization
	 (let ((lets '())
	       (vars '()))
	    (for-each (lambda (x)
			 (cond
			    ((not (isa? x J2SDecl))
			     (error "j2s-letopt" "internal error"
				(j2s->list x)))
			    ((j2s-let? x)
			     (set! lets (cons x lets)))
			    (else
			     (set! vars (cons x vars)))))
	       decls)
	    (when (pair? lets)
	       ;; this modify nodes in place
	       (set! nodes
		  (map! j2s-update-ref!
		     (j2s-toplevel-letopt! nodes (reverse! lets) vars)))))
	 ;; optimze global variables literals
	 (when (config-get args :optim-literals #f)
	    (when (>= (config-get args :verbose 0) 4)
	       (fprintf (current-error-port) " [optim-literal]"))
	    (j2s-letopt-global-literals! decls nodes))))
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
	 (when (isa? %info DeclInfo)
	    (with-access::DeclInfo %info (optdecl)
	       (when (isa? optdecl J2SDecl)
		  (set! decl optdecl))))))
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
;*    get-inits ...                                                    */
;*    -------------------------------------------------------------    */
;*    As GET-LET-INITS but returns #f if no init found (easier with    */
;*    COND forms).						       */
;*---------------------------------------------------------------------*/
(define (get-inits::obj node::J2SNode decls::pair-nil)
   (let ((inits (get-let-inits node decls)))
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
;*    decl-init ...                                                    */
;*---------------------------------------------------------------------*/
(define (decl-init this::J2SDecl)
   (with-access::J2SDecl this (%info)
      (with-access::DeclInfo %info (init)
	 init)))

;*---------------------------------------------------------------------*/
;*    j2s-letopt! ::J2SLetBlock ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-letopt! this::J2SLetBlock)
   
   (define (init-decl! d)
      (with-access::J2SDecl d (%info scope binder)
	 (set! scope 'letblock)
	 ;; non optimized local variables might have already
	 ;; be scanned before being pushed down by the
	 ;; TAIL-LET! stage
	 (unless (isa? %info DeclInfo)
	    (let ((optdecl (if (eq? binder 'let-opt)
			       ;; already optimized
			       d
			       ;; don't know yet
			       #unspecified)))
	       (set! %info (instantiate::DeclInfo
			      (optdecl optdecl)))))))
   
   (define (split-letblock! this::J2SLetBlock)
      (with-trace 'j2s-letopt "split-letblock!"
	 (with-access::J2SLetBlock this (decls nodes loc)
	    (let loop ((ns nodes)
		       (stmts '()))
	       (cond
		  ((null? ns)
		   (optimize-letblock! this))
		  ((not (get-let-inits (car ns) decls))
		   (if (null? (get-used-decls (car ns) decls))
		       (loop (cdr ns) (cons (car ns) stmts))
		       (begin
			  (set! nodes ns)
			  (J2SBlock*
			     (reverse! (cons (optimize-letblock! this) stmts))))))
		  (else
		   (set! nodes ns)
		   (J2SBlock*
		      (reverse! (cons (optimize-letblock! this) stmts)))))))))
   
   (define (optimize-letblock! this::J2SLetBlock)
      (with-trace 'j2s-letopt "optimize-letblock!"
	 (with-access::J2SLetBlock this (decls nodes)
	    ;; flatten the letblock
	    (flatten-letblock! this)
	    (let loop ((n nodes)
		       (decls decls))
	       (cond
		  ((null? n)
		   ;; should never be reached
		   (trace-item "<<<.1 " (j2s->list this))
		   this)
		  ((not (get-let-inits (car n) decls))
		   (trace-item "---.2 " (j2s->list (car n)))
		   ;; optimize recursively
		   (set-car! n (j2s-letopt! (car n)))
		   ;; keep optimizing the current let block
		   (mark-used-noopt*! (car n) decls)
		   ;; keep the non-marked decls
		   (loop (cdr n) (filter decl-maybe-opt? decls)))
		  ((null? decls)
		   (trace-item "---.3 " (j2s->list (car n)))
		   ;; nothing more to be potentially optimzed
		   (map! j2s-letopt! n)
		   (trace-item "<<<.3 " (j2s->list (car n)))
		   this)
		  ((eq? n nodes)
		   (trace-item "---.4 " (j2s->list (car n)))
		   ;; got an initialization block, which can be optimized
		   (let ((res (tail-let! this this)))
		      (trace-item "<<<.4a " (j2s->list res))
		      res))
		  (else
		   (trace-item "---.5 " (j2s->list (car n)))
		   ;; re-organize the let-block to place this inits
		   ;; at the head of a fresh let
		   (let ((res (tail-let! this (head-let! this n))))
		      (trace-item "<<<.5 " (j2s->list res))
		      res)))))))

   (define (flatten-letblock! this::J2SLetBlock)
      (with-trace 'j2s-letopt "flatten-letblock!"
	 (with-access::J2SLetBlock this (nodes)
	    (let loop ()
	       (when (and (pair? nodes) (null? (cdr nodes))
			  (isa? (car nodes) J2SBlock)
			  (not (isa? (car nodes) J2SLetBlock)))
		  (with-access::J2SBlock (car nodes) ((ns nodes))
		     (set! nodes ns))
		  (loop))))
	 (trace-item "this=" (j2s->list this))))
   
   (with-access::J2SLetBlock this (decls nodes)
      ;; start iterating over all the LetBlock statements to find
      ;; the first decl
      (with-trace 'j2s-letopt "j2s-letopt!"
	 (trace-item "" (j2s->list this))
	 ;; mark all declarations
	 (for-each init-decl! decls)
	 (if (every (lambda (d)
		       (with-access::J2SDecl d (binder)
			  (not (eq? binder 'let-opt))) )
		decls)
	     ;; try to move before the letblock statements not using any
	     ;; of the introduced variable
	     (split-letblock! this)
	     ;; at least one binding is already optimized, splitting is
	     ;; not possible as it would break the evaluation order
	     (optimize-letblock! this)))))

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
		;; collect the non-optimized decls
		(let ((noopts '())
		      (opts '()))
		   (for-each (lambda (d)
				(if (decl-maybe-opt? d)
				    (set! opts (cons d opts))
				    (set! noopts (cons d noopts))))
		      decls)
		   (set! opts (reverse! opts))
		   (set! noopts (reverse! noopts))
		   (trace-item "noopts=" (j2s-dump-decls noopts))
		   (trace-item "opts=" (j2s-dump-decls opts))
		   (set! nodes n)
		   (set! decls opts)
		   (if (null? noopts)
		       (instantiate::J2SBlock
			  (loc loc)
			  (endloc endloc)
			  (nodes (reverse! (cons this prev))))
		       (instantiate::J2SLetBlock
			  (loc loc)
			  (endloc endloc)
			  (decls noopts)
			  (nodes (reverse! (cons this prev))))))
		(loop (cdr n) (cons (car n) prev)))))))

;*---------------------------------------------------------------------*/
;*    tail-let! ...                                                    */
;*---------------------------------------------------------------------*/
(define (tail-let! this::J2SLetBlock resnode::J2SStmt)
   
   (define (init->stmt::J2SStmtExpr init::J2SInit)
      (with-access::J2SInit init (loc)
	 (instantiate::J2SStmtExpr
	    (loc loc)
	    (expr init))))
   
   (define (decl-fun? d)
      (or (isa? d J2SDeclFun) (isa? d J2SDeclFunCnst)))
   
   (define (decl-fun-val d)
      (if (isa? d J2SDeclFun)
	  (with-access::J2SDeclFun d (val) val)
	  (with-access::J2SDeclFunCnst d (val) val)))
   
   (define (letblock-nodes-split nodes::pair-nil decls)
      ;; Split the NODES of a LET-BLOCK in two parts: INITS x RESTS
      ;;    INITS = the consecutive inits of NODES
      ;;    RESTS = the following NODES
      (with-trace 'j2s-letopt "letblock-nodes-split"
	 (let loop ((nodes nodes)
		    (inits '()))
	    (cond
	       ((null? nodes)
		(trace-item "inits.1=" (map j2s->list inits))
		(values '() inits))
	       ((get-let-inits (car nodes) decls)
		=>
		(lambda (is) (loop (cdr nodes) (append! inits is))))
	       ((isa? (car nodes) J2SNop)
		(loop (cdr nodes) inits))
	       (else
		(trace-item "inits.2=" (map j2s->list inits))
		(values nodes inits))))))
   
   (define (sort-inodes this::J2SLetBlock)
      (with-trace 'j2s-letopt "sort-inodes"
	 ;; sort the declarations list (move uninitialized and functions
	 ;; upfront) and returns the list of sortted init statements
	 (with-access::J2SLetBlock this (nodes decls loc)
	    ;; extracts the initialization nodes
	    (multiple-value-bind (rests inits)
	       (letblock-nodes-split nodes decls)
	       ;; sort the declarations according to the inits order
	       (let ((odecls (map init-decl inits))
		     (revdecls (reverse decls)))
		  (trace-item "decls=" (j2s-dump-decls decls))
		  (trace-item "odecls=" (j2s-dump-decls odecls))
		  ;; add the function definitions
		  (for-each (lambda (d)
			       (when (decl-fun? d)
				  (set! inits (cons d inits))
				  (set! odecls (cons d odecls))))
		     revdecls)
		  ;; add the non-initialized variable definitions
		  (for-each (lambda (d)
			       (when (and (not (decl-fun? d))
					  (not (memq d odecls)))
				  (set! inits (cons d inits))
				  (set! odecls (cons d odecls))))
		     revdecls)
		  (set! decls odecls))
	       (trace-item "inits=" (j2s-dump-decls inits))
	       (trace-item "decls=" (j2s-dump-decls decls))
	       (trace-item "rest=" (map j2s->list rests))
	       (values rests inits)))))

   (define (split-decls decls::pair)
      ;; Separate the decls list in 3 groups: noopt, opt, unprocessed.
      (with-trace 'j2s-letopt "split-decls"
	 (let ((noopts '())
	       (opts '())
	       (unprocs '()))
	    (for-each (lambda (d::J2SDecl)
			 (with-access::J2SDecl d (%info)
			    (with-access::DeclInfo %info (optdecl)
			       (cond
				  ((not optdecl)
				   (set! noopts (cons d noopts)))
				  ((eq? optdecl #unspecified)
				   (set! unprocs (cons d unprocs)))
				  (else
				   (set! opts (cons optdecl opts)))))))
	       decls)
	    (set! noopts (delete-duplicates! (reverse! noopts)))
	    (set! opts (reverse! opts))
	    (set! unprocs (reverse! unprocs))
	    (trace-item "noopts=" (j2s-dump-decls noopts))
	    (trace-item "opts=" (j2s-dump-decls opts))
	    (trace-item "unprocs=" (j2s-dump-decls unprocs))
	    (values noopts opts unprocs))))
   
   (define (optimize-letblock! this::J2SLetBlock resnode inodes rests)
      (with-trace 'j2s-letopt "optimize-letblock"
	 (with-access::J2SLetBlock this (nodes decls loc)
	    (multiple-value-bind (noopts opts unprocs)
	       (split-decls decls)
	       (let ((newinits (filter-map (lambda (n)
					      (when (isa? n J2SInit)
						 (let ((d (init-decl n)))
						    (unless (decl-maybe-opt? d)
						       (j2s-letopt!
							  (init->stmt n))))))
				  inodes))
		     (restnodes (if (pair? unprocs)
				    (list (j2s-letopt!
					     (duplicate::J2SLetBlock this
						(decls unprocs)
						(nodes rests))))
				    (map! j2s-letopt! rests))))
		  (set! decls (append noopts opts))
		  (set! nodes (append! newinits restnodes)))))
	 resnode))

   (define (used-before-init? decl::J2SNode inits::pair-nil rests::pair-nil)

      (define (used-in-inits? decl inits)
	 (let ((decls (list decl)))
	    (let loop ((inits inits))
	       (cond
		  ((null? inits)
		   #f)
		  ((eq? decl (car inits))
		   (loop (cdr inits)))
		  ((memq decl (get-used-decls (car inits) decls))
		   #t)
		  (else
		   (loop (cdr inits)))))))

      (define (used-in-rests? decl rests)
	 (let ((decls (list decl)))
	    (let loop ((rests rests))
	       (cond
		  ((null? rests)
		   #f)
		  ((memq decl (get-used-decls (car rests) decls))
		   #t)
		  (else
		   (loop (cdr rests)))))))

      (or (used-in-inits? decl inits)
	  (used-in-rests? decl rests)))
   
   ;; the main optimization loop
   (with-trace 'j2s-letopt "tail-let!"
      (trace-item "this=" (j2s->list this))
      (with-access::J2SLetBlock this (decls)
	 (multiple-value-bind (rests inits)
	    (sort-inodes this)
	    ;; iterate over all the inits
	    (let loop ((inodes inits))
	       (cond
		  ((null? inodes)
		   (optimize-letblock! this resnode inits rests))
		  ((decl-fun? (car inodes))
		   (trace-item "decl-fun=" (j2s-dump-decls (car inodes)))
		   ;; a function
		   (let ((decl (car inodes)))
		      (j2s-letopt! decl)
		      (with-access::J2SDecl decl (binder %info)
			 (if (decl-maybe-opt? decl)
			     (mark-decl-opt! decl)
			     (mark-used-noopt*! (decl-fun-val decl) decls)))
		      (loop (cdr inodes))))
		  ((isa? (car inodes) J2SDecl)
		   ;; a variable declaration without init
		   (trace-item "no-init=" (j2s-dump-decls (car inodes)))
		   (let ((decl (car inodes)))
		      (if (used-before-init? decl inits rests)
			  ;; potentially used before initialized
			  (mark-decl-noopt! decl)
			  ;; never used before initialized
			  (with-access::J2SDecl decl (loc)
			     (decl-update-info! decl
				(new-let-opt decl
				   (J2SInit (J2SRef decl) (J2SUndefined)))))))
		   (loop (cdr inodes)))
		  ((not (decl-maybe-opt? (init-decl (car inodes))))
		   (trace-item "no-opt="
		      (j2s-dump-decls (init-decl (car inodes))))
		   ;; already invalidated variables
		   (loop (cdr inodes)))
		  ((duplicate-init? (car inodes))
		   (trace-item "dup=" (j2s-dump-decls (init-decl (car inodes))))
		   (mark-decl-noopt! (init-decl (car inodes)))
		   (loop '()))
		  (else
		   (let ((decl (init-decl (car inodes))))
		      (trace-item "regular=" (j2s-dump-decls decl))
		      ;; optimize that binding
		      (decl-update-info! decl
			 (new-let-opt (car inodes)
			    (j2s-letopt! (car inodes))))
		      (unless (fun-init? (car inodes))
			 (mark-used-noopt*! (car inodes) decls))
		      (loop (cdr inodes))))))))))

;*---------------------------------------------------------------------*/
;*    duplicate-init? ...                                              */
;*    -------------------------------------------------------------    */
;*    Predicate is true for overriden variables (i.e., variables       */
;*    with two or more inits).					       */
;*---------------------------------------------------------------------*/
(define (duplicate-init? expr::J2SInit)
   (let ((decl (init-decl expr)))
      (with-access::J2SDecl decl (%info)
	 (with-access::DeclInfo %info (optdecl)
	    (not (eq? optdecl #unspecified))))))

;*---------------------------------------------------------------------*/
;*    decl-update-init! ...                                            */
;*---------------------------------------------------------------------*/
(define (decl-update-init! decl::J2SDecl i)
   (with-access::J2SDecl decl (%info)
      (if (isa? %info DeclInfo)
	  (with-access::DeclInfo %info (init)
	     (set! init i))
	  (set! %info
	     (instantiate::DeclInfo
		(init i))))))

;*---------------------------------------------------------------------*/
;*    new-let-opt ...                                                  */
;*---------------------------------------------------------------------*/
(define (new-let-opt node::J2SNode expr::J2SInit)
   ;; create a new declaration for the statement
   (with-access::J2SInit expr (rhs)
      (let ((decl (init-decl expr)))
	 (if (isa? decl J2SDeclInit)
	     (with-access::J2SDeclInit decl (binder val scope)
		(set! binder 'let-opt)
		(set! val rhs)
		decl)
	     (with-access::J2SDecl decl (loc id %info)
		(let ((new (instantiate::J2SDeclInit
			      (id id)
			      (loc loc)
			      (key -1)
			      (val rhs)))
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
		   new))))))

;*---------------------------------------------------------------------*/
;*    fun-init? ...                                                    */
;*---------------------------------------------------------------------*/
(define (fun-init? init)
   (when (isa? init J2SInit)
      (with-access::J2SInit init (rhs)
	 (isa? rhs J2SFun))))

;*---------------------------------------------------------------------*/
;*    get-let-inits ...                                                */
;*    -------------------------------------------------------------    */
;*    Extract the list of let-declarations of a statement.             */
;*---------------------------------------------------------------------*/
(define (get-let-inits node::J2SStmt decls)
   (when (isa? node J2SSeq)
      (with-access::J2SSeq node (nodes)
	 (let loop ((nodes nodes)
		    (inits '()))
	    (if (null? nodes)
		(when (pair? inits)
		   (reverse! inits))
		(let ((n::J2SStmt (car nodes)))
		   (when (isa? n J2SStmtExpr)
		      (with-access::J2SStmtExpr n (expr)
			 (when (isa? expr J2SInit)
			    (let ((decl (init-decl expr)))
			       (when (memq decl decls)
				  (decl-update-init! decl expr)
				  (loop (cdr nodes) (cons expr inits)))))))))))))

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
	 ((get-inits (car n) decls)
	  =>
	  (lambda (inits)
	     ;; a letinit node
	     (let liip ((inits inits)
			(decls decls)
			(deps deps)
			(res res))
		(if (null? inits)
		    (loop (cdr n) decls deps res)
		    (with-access::J2SInit (car inits) (rhs loc)
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
				 (let ((ndecls (remq decl decls))
				       (stmtinit (instantiate::J2SStmtExpr
						    (loc loc)
						    (expr init))))
				    (liip (cdr inits) ndecls
				       deps
				       res))))
;* 				       (cons stmtinit res)))))         */
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
(define (get-used-decls node::J2SNode decls::pair-nil)
   (delete-duplicates! (node-used* node decls #t)))

;*---------------------------------------------------------------------*/
;*    node-used* ...                                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SNode decls store)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    node-used* ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SDecl decls store)
   (if (member node decls) (list node) '()))

;*---------------------------------------------------------------------*/
;*    node-used* ::J2Ref ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SRef decls store)
   (with-access::J2SRef node (decl)
      (if (member decl decls) (list decl) '())))

;*---------------------------------------------------------------------*/
;*    node-used* ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-used* node::J2SFun decls store)
   (with-access::J2SFun node (%info body decl)
      (if (isa? %info FunInfo)
	  (with-access::FunInfo %info (used)
	     used)
	  (let ((info (instantiate::FunInfo)))
	     (set! %info info)
	     (let ((bodyused (node-used* body decls #f)))
		(if store
		    (begin
		       (with-access::FunInfo info (used)
			  (set! used bodyused))
		       (when (isa? decl J2SDecl)
			  (with-access::J2SDecl decl (%info)
			     (if (isa? %info DeclInfo)
				 (with-access::DeclInfo %info (used)
				    (set! used bodyused))
				 (set! %info
				    (instantiate::DeclInfo
				       (used bodyused)))))))
		    (set! %info #f))
		bodyused)))))

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
;*    decl-update-info! ...                                            */
;*---------------------------------------------------------------------*/
(define (decl-update-info! decl::J2SDecl od::J2SDecl)
   (with-access::J2SDecl decl (%info binder)
      (with-access::DeclInfo %info (optdecl)
	 (when (eq? optdecl #unspecified)
	    (set! binder 'let-opt)
	    (set! optdecl od)))))

;*---------------------------------------------------------------------*/
;*    decl-maybe-opt? ...                                              */
;*---------------------------------------------------------------------*/
(define (decl-maybe-opt? decl::J2SDecl)
   (with-access::J2SDecl decl (%info)
      (with-access::DeclInfo %info (optdecl)
	 optdecl)))

;*---------------------------------------------------------------------*/
;*    mark-decl-opt! ...                                               */
;*---------------------------------------------------------------------*/
(define (mark-decl-opt! decl::J2SDecl)
   (with-access::J2SDecl decl (%info binder)
      (set! binder 'let-opt)
      (with-access::DeclInfo %info (optdecl)
	 (when (eq? optdecl #unspecified)
	    (when optdecl
	       (with-trace 'j2s-letopt "mark-decl-opt!"
		  (trace-item "decl=" (j2s-dump-decls decl)))))
	 (set! optdecl decl)))
   decl)
   
;*---------------------------------------------------------------------*/
;*    mark-decl-noopt! ...                                             */
;*---------------------------------------------------------------------*/
(define (mark-decl-noopt! decl::J2SDecl)
   (with-access::J2SDecl decl (%info binder)
      (when (eq? binder 'let-opt) (set! binder 'let))
      (with-access::DeclInfo %info (optdecl)
	 (when optdecl
	    (with-trace 'j2s-letopt "mark-decl-noopt!"
	       (trace-item "decl=" (j2s-dump-decls decl))))
	 (set! optdecl #f)))
   decl)

;*---------------------------------------------------------------------*/
;*    mark-used-noopt*! ...                                            */
;*---------------------------------------------------------------------*/
(define (mark-used-noopt*! node decls)
   (with-trace 'j2s-letopt "mark-used-noopt*"
      (trace-item "node=" (j2s->list node))
      (trace-item "no-used*=" (map j2s->list (node-used* node decls #t)))
      (for-each (lambda (d)
		   (when (eq? (decl-maybe-opt? d) #unspecified)
		      (mark-decl-noopt! d)))
	 (node-used* node decls #t))))
   
;*---------------------------------------------------------------------*/
;*    j2s-letopt-global-literals! ...                                  */
;*    -------------------------------------------------------------    */
;*    Scan te nodes, skip the nop statements. Mark global constant     */
;*    initializations as let-opt vars. Stop when the first non-let var */
;*    is found.                                                        */
;*---------------------------------------------------------------------*/
(define (j2s-letopt-global-literals! decls nodes)
   
   (define (nop? node)
      (or (isa? node J2SNop)
	  (when (isa? node J2SStmtExpr)
	     (with-access::J2SStmtExpr node (expr)
		(isa? expr J2SLiteral)))
	  (when (isa? node J2SSeq)
	     (with-access::J2SSeq node (nodes)
		(every nop? nodes)))))
   
   (define (init node)
      (cond
	 ((isa? node J2SSeq)
	  (with-access::J2SSeq node (nodes)
	     (when (and (pair? nodes) (null? (cdr nodes)))
		(init (car nodes)))))
	 ((isa? node J2SStmtExpr)
	  (with-access::J2SStmtExpr node (expr)
	     (when (isa? expr J2SInit)
		node)))))
   
   (define (literal? node)
      (cond
	 ((isa? node J2SLiteral)
	  (or (not (isa? node J2SArray))
	      (with-access::J2SArray node (exprs)
		 (every literal? exprs))))
	 ((isa? node J2SUnary)
	  (with-access::J2SUnary node (op expr)
	     (literal? expr)))))
   
   (define (letopt-literals literals)
      (when (pair? literals)
	 (for-each (lambda (literal)
		      (with-access::J2SDecl (car literal) (%info loc)
			 (let ((odecl (new-let-opt (car literal) (cdr literal))))
			    (with-access::J2SDeclInit odecl (scope)
			       (set! scope '%scope))
			    (set! %info
			       (instantiate::DeclInfo
				  (optdecl odecl))))))
	    literals)
	 ;; modify the declaration lists
	 (map! (lambda (decl)
		  (with-access::J2SDecl decl (binder scope %info)
		     (if (and (eq? binder 'var) (eq? scope '%scope)
			      (isa? %info DeclInfo))
			 (with-access::DeclInfo %info (optdecl)
			    optdecl)
			 decl)))
	    decls)
	 ;; change the references in the program nodes
	 (for-each j2s-update-ref! decls)
	 (for-each j2s-update-ref! nodes)))
   
   (let loop ((n nodes)
	      (literals '()))
      (cond
	 ((null? n)
	  (letopt-literals literals))
	 ((nop? (car n))
	  (loop (cdr n) literals))
	 ((init (car n))
	  =>
	  (lambda (stmt)
	     (with-access::J2SStmtExpr stmt (expr)
		(with-access::J2SInit expr (lhs rhs loc)
		   (if (isa? lhs J2SRef)
		       (with-access::J2SRef lhs (decl)
			  (with-access::J2SDecl decl (binder scope %info)
			     (if (and (eq? binder 'var) (eq? scope '%scope))
				 (if (literal? rhs)
				     (let ((init expr))
					(set! expr (J2SUndefined))
					(loop (cdr n)
					   (cons (cons decl init) literals)))
				     (letopt-literals literals))
				 (letopt-literals literals))))
		       (letopt-literals literals))))))
	 (else
	  (letopt-literals literals)))))


		   
