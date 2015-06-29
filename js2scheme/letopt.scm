;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/letopt.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 06:35:14 2015                          */
;*    Last change :  Mon Jun 29 17:22:23 2015 (serrano)                */
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
   (with-access::J2SProgram this (nodes)
      (for-each (lambda (o) (j2s-letopt! o)) nodes))
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
		(with-access::J2SDecl decl (use)
		   (set! use 0)))
      decls))

;*---------------------------------------------------------------------*/
;*    all-unused? ...                                                  */
;*---------------------------------------------------------------------*/
(define (all-unused?::bool decls::pair-nil)
   (every (lambda (d::J2SDecl)
	     (with-access::J2SDecl d (use)
		(=fx use 0)))
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
   
   (define (let-init->let-opt::J2SLetOpt this::J2SInitLet)
      (with-access::J2SInitLet this (rhs loc lhs)
	 (with-access::J2SRef lhs (decl)
	    (with-access::J2SLet decl (id)
	       (widen!::J2SLetOpt decl
		  (val (j2s-letopt! rhs)))))))
   
   (define (stop n inits forward-decls backward-decls)
      (tprint "stop inits=" (map j2s->list inits))
      (tprint "forward=" (map j2s->list forward-decls))
      (tprint "backward=" (map j2s->list backward-decls))
      ;; optimizes everything subnodes
      (for-each j2s-letopt! n)
      ;; produce the optimization result
      (with-access::J2SLetBlock this (decls nodes loc endloc)
	 (cond
	    ((null? inits)
	     ;; give up optimizing
	     resnode)
	    ((null? forward-decls)
	     ;; optimizes everything, just change the letblock body
	     (with-access::J2SLetBlock this (nodes decls)
		(set! decls inits)
		(set! nodes n))
	     resnode)
	    (else
	     ;; optimize partially, create a new letblock
	     (let ((new (instantiate::J2SLetBlock
			   (loc loc)
			   (endloc endloc)
			   (decls backward-decls)
			   (nodes n))))
		(set! decls backward-decls)
		(set! nodes (reverse! (cons n inits)))
		resnode)))))
   
   (define (statement->expression node)
      ;; transform a statement into a expression
      (with-access::J2SNode node (loc)
	 (let* ((block (instantiate::J2SBlock
			  (loc loc)
			  (endloc loc)
			  (nodes (list node))))
		(fun (instantiate::J2SFun
			(name '||)
			(params '())
			(loc loc)
			(body block)))
		(call (instantiate::J2SCall
			 (loc loc)
			 (fun fun))))
	    call)))
   
   (define (new-decl node::J2SStmt)
      ;; create a new declaration for the statement
      (with-access::J2SNode node (loc)
	 (instantiate::J2SLetOpt 
	    (loc loc)
	    (id (gensym))
	    (ronly #t)
	    (writable #f)
	    (val (statement->expression node)))))

   (tprint "optim=" (j2s->list this))
   (with-access::J2SLetBlock this (decls nodes)
      (let loop ((n nodes)
		 (ninits '())
		 (forward-decls decls)
		 (backward-decls '()))
	 (cond
	    ((null? n)
	     (stop n ninits forward-decls backward-decls))
	    ((null? forward-decls)
	     ;; all declarations checked
	     (stop n ninits '() backward-decls))
	    (else
	     (let* ((node (car n))
		    (inits (get-let-inits node)))
		(if (null? inits)
		    ;; a regular statement
		    (begin
		       (use-count node)
		       (if (all-unused? forward-decls)
			   ;; optimizations are still possible,
			   ;; put this statement into a variable
			   (let* ((ndecl (new-decl node))
				  (ninit ndecl))
			      (loop (cdr n)
				 (cons ninit ninits)
				 forward-decls
				 (cons ndecl backward-decls)))
			   ;; stop optimizing
			   (stop n ninits forward-decls backward-decls)))
		    ;; a list of inits
		    (let liip ((inits inits)
			       (ninits ninits)
			       (forward-decls forward-decls)
			       (backward-decls backward-decls))
		       (if (null? inits)
			   (loop (cdr n) ninits forward-decls backward-decls)
			   (with-access::J2SInitLet (car inits) (rhs)
			      (unless (isa? rhs J2SFun)
				 (use-count rhs))
			      (if (all-unused? forward-decls)
				  ;; keep optimizing everything
				  (let ((decl (init-decl (car inits)))
					(init (let-init->let-opt (car inits))))
				     (liip (cdr inits)
					(cons init ninits)
					(remq decl forward-decls)
					backward-decls))
				  ;; stop optimizing
				  (stop n ninits
				     forward-decls backward-decls))))))))))))




