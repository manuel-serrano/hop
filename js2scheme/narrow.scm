;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/narrow.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 25 07:41:22 2015                          */
;*    Last change :  Mon Apr 30 09:14:13 2018 (serrano)                */
;*    Copyright   :  2015-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Narrow local variable scopes                                     */
;*    -------------------------------------------------------------    */
;*    This optimization consists in transforming global VAR decl into  */
;*    LET-OPT bindings.                                                */ 
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_narrow

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer)

   (static (class J2SNarrowInfo
	      (deffun::obj (default #f))
	      (defblock::obj (default #f))
	      (useblocks::pair-nil (default '()))
	      (narrowable::bool (default #t))
	      (ldecl (default #f))))

   (export j2s-narrow-stage))

;*---------------------------------------------------------------------*/
;*    j2s-narrow-stage                                                 */
;*---------------------------------------------------------------------*/
(define j2s-narrow-stage
   (instantiate::J2SStageProc
      (name "narrow")
      (comment "Narrow resolution")
      (proc j2s-narrow)))

;*---------------------------------------------------------------------*/
;*    j2s-info->list ::J2SNarrowInfo ...                               */
;*---------------------------------------------------------------------*/
(define-method (j2s-info->list this::J2SNarrowInfo)
   (with-access::J2SNarrowInfo this (defblock narrowable ldecl useblocks)
      (format "[[J2SNarrowInfo defblock=~s narrowable=~s useblocks=~s]]"
	 (when (isa? defblock J2SNode)
	     (with-access::J2SNode defblock (loc)
		loc))
	 narrowable
	 (format "~(, )"
	    (map (lambda (b) (with-access::J2SBlock b (loc) loc)) useblocks)))))

;*---------------------------------------------------------------------*/
;*    j2s-narrow ...                                                   */
;*    -------------------------------------------------------------    */
;*    Warning, headers are not scanned for variable resolution!        */
;*---------------------------------------------------------------------*/
(define (j2s-narrow this conf)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (decls nodes headers)
	 ;; mark that header declarations are not narrowable
	 (for-each j2s-mark-unnarrowable headers)
	 ;; add extra block before each init not already at the head of a block
	 (for-each (lambda (o)
		      (when (isa? o J2SDeclFun)
			 (with-access::J2SDeclFun o (val)
			    (j2s-blockify! val))))
	    decls)
	 ;; statement optimization
	 (for-each (lambda (o)
		      (cond
			 ((isa? o J2SDeclFun)
			  (with-access::J2SDeclFun o (val)
			     (j2s-narrow-fun! val)))
			 ((isa? o J2SDecl)
			  (j2s-mark-unnarrowable o))))
	    decls)
	 ;; nodes
	 (set! nodes
	    (map! (lambda (o)
		     (j2s-find-init-blocks o #f #f)
		     (j2s-mark-narrowable o '() #f
			(and (isa? o J2SFun) o) (make-cell #f))
		     (j2s-lift-inits! (j2s-narrow-body! o)))
	       nodes))))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-mark-unnarrowable ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-unnarrowable this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-mark-unnarrowable ::J2SDecl ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-unnarrowable this::J2SDecl)
   (with-access::J2SDecl this (%info %%dump)
      (set! %%dump "unnarrowable")
      (set! %info
	 (instantiate::J2SNarrowInfo
	    (narrowable #f)))))

;*---------------------------------------------------------------------*/
;*    j2s-blockify! ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-blockify! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-blockify! ::J2SBlock ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-blockify! this::J2SBlock)
   
   (define (is-init? n::J2SNode)
      (when (isa? n J2SSeq)
	 (with-access::J2SSeq n (nodes)
	    (every (lambda (s)
		      (when (isa? s J2SStmtExpr)
			 (with-access::J2SStmtExpr s (expr)
			    (isa? expr J2SInit))))
	       nodes))))

   (with-access::J2SBlock this (nodes)
      (let loop ((nodes nodes)
		 (prev #f))
	 (cond
	    ((null? nodes)
	     this)
	    (prev
	     (if (is-init? (car nodes))
		 (with-access::J2SNode (car nodes) (loc)
		    (let ((nseq (j2s-blockify! (J2SBlock*/w-endloc nodes))))
		       (set-cdr! prev (list nseq))
		       this))
		 (begin
		    (set-car! nodes (j2s-blockify! (car nodes)))
		    (loop (cdr nodes) nodes))))
	    (else
	     (set-car! nodes (j2s-blockify! (car nodes)))
	     (if (is-init? (car nodes))
		 (loop (cdr nodes) #f)
		 (loop (cdr nodes) nodes)))))))

;*---------------------------------------------------------------------*/
;*    j2s-narrow-fun! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-narrow-fun! o::J2SFun)
   (with-access::J2SFun o (body)
      ;; find the declaring block of all declarations
      (j2s-find-init-blocks body #f o)
      ;; find the variables used but never initialized
      (when #t
	 (let ((uvars (j2s-find-non-init-vars* body)))
	    (when (pair? uvars)
	       (let ((btree (build-body-btree body))
		     (assigs (collect-assig* body uvars body)))
		  (for-each (lambda (uvar)
			       (let ((block (find-drop-block uvar btree)))
				  (when block
				     (let ((assig (find-drop-assig
						     uvar assigs block)))
					(when assig
					   (rewrite-assig! body assig)
					   (with-access::J2SDecl uvar (%info)
					      (with-access::J2SNarrowInfo %info
						    (defblock narrowable ldecl)
						 (set! defblock block)
						 (set! ldecl uvar)
						 (set! narrowable #t))))))))
		     uvars)))))
      ;; get the set of narrowable declarations
      (j2s-mark-narrowable body '() #f o (make-cell #f))
      ;; narrow the function body
      (set! body (j2s-lift-inits! (j2s-narrow-body! body)))))

;*---------------------------------------------------------------------*/
;*    j2s-find-init-blocks! ...                                        */
;*    -------------------------------------------------------------    */
;*    For each declaration, finds the block that initializes it and    */
;*    store for each declaration the list of blocks that use it.       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-find-init-blocks this::J2SNode block fun)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-find-init-blocks ::J2SFun ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-find-init-blocks this::J2SFun blocks fun)
   (with-access::J2SFun this (body)
      (j2s-find-init-blocks body #f this)))

;*---------------------------------------------------------------------*/
;*    j2s-find-init-blocks ::J2SBlock ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-find-init-blocks this::J2SBlock block fun)
   (with-access::J2SBlock this (nodes)
      (for-each (lambda (b) (j2s-find-init-blocks b this fun)) nodes)))

;*---------------------------------------------------------------------*/
;*    j2s-find-init-blocks ::J2SInit ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-find-init-blocks this::J2SInit block fun)
   (when block
      (with-access::J2SInit this (lhs rhs)
	 (j2s-find-init-blocks rhs block fun)
	 (when (isa? lhs J2SRef)
	    (with-access::J2SRef lhs (decl)
	       (unless (or (j2s-let? decl) (j2s-param? decl))
		  ;; skip let/const declarations
		  (with-access::J2SDecl decl (%info %%dump binder)
		     (if (isa? %info J2SNarrowInfo)
			 (with-access::J2SNarrowInfo %info (useblocks defblock deffun)
			    (set! useblocks
			       (if (memq block useblocks)
				   useblocks
				   (cons block useblocks)))
			    (set! defblock block)
			    (set! deffun fun))
			 (set! %info
			    (instantiate::J2SNarrowInfo
			       (deffun fun)
			       (useblocks (list block))
			       (defblock block)))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-find-init-blocks ::J2SRef ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-find-init-blocks this::J2SRef block fun)
   (with-access::J2SRef this (decl)
      (unless (or (j2s-let? decl) (j2s-param? decl))
	 ;; skip let/const declarations
	 (with-access::J2SDecl decl (%info)
	    (if (isa? %info J2SNarrowInfo)
		(with-access::J2SNarrowInfo %info (useblocks)
		   (set! useblocks
		      (if (memq block useblocks)
			  useblocks
			  (cons block useblocks))))
		(set! %info
		   (instantiate::J2SNarrowInfo
		      (useblocks (list block))
		      (narrowable #f))))))))

;*---------------------------------------------------------------------*/
;*    j2s-mark-narrowable ...                                          */
;*    -------------------------------------------------------------    */
;*    A var declaration is narrowable if all the following conditions  */
;*    are meet:                                                        */
;*      1- it is never used outside its bounding block                 */
;*      2- it is never captured inside a loop                          */
;*      3- it is never used after a yield inside a loop                */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-narrowable this::J2SNode blocks inloop fun yield)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-mark-narrowable ::J2SBlock ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-narrowable this::J2SBlock blocks inloop fun yield)
   (let ((nblocks (cons this blocks)))
      (with-access::J2SBlock this (nodes)
	 (for-each (lambda (b) (j2s-mark-narrowable b nblocks inloop fun yield))
	    nodes))))

;*---------------------------------------------------------------------*/
;*    j2s-mark-narrowable ::J2SRef ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-narrowable this::J2SRef blocks inloop fun yield)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info id)
	 (when (isa? %info J2SNarrowInfo)
	    (with-access::J2SNarrowInfo %info (deffun defblock narrowable)
	       (unless (and (memq defblock blocks)
			    (or (not inloop)
				(and (eq? fun deffun) (not (cell-ref yield)))))
		  (set! narrowable #f)))))))

;*---------------------------------------------------------------------*/
;*    j2s-mark-narrowable ::J2SFun ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-narrowable this::J2SFun blocks inloop fun yield)
   (with-access::J2SFun this (body)
      (j2s-mark-narrowable body blocks inloop this yield)))

;*---------------------------------------------------------------------*/
;*    j2s-mark-narrowable ::J2SLoop ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-narrowable this::J2SLoop blocks inloop fun yield)
   (with-access::J2SLoop this (body)
      (let ((res (call-default-walker)))
	 (unless inloop (cell-set! yield #f))
	 res)))

;*---------------------------------------------------------------------*/
;*    j2s-mark-narrowable ::J2SYield ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-narrowable this::J2SYield blocks inloop fun yield)
   (with-access::J2SYield this (expr)
      (cell-set! yield #t)
      (j2s-mark-narrowable expr blocks inloop fun yield)))

;*---------------------------------------------------------------------*/
;*    j2s-narrow-body! ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow-body! this::J2SNode)
   (call-default-walker))


;*---------------------------------------------------------------------*/
;*    j2s-narrow-body! ::J2SFun ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow-body! this::J2SFun)
   (j2s-narrow-fun! this)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-narrow-body! ::J2SDecl ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow-body! this::J2SDecl)
   (call-default-walker)
   (with-access::J2SDecl this (loc %info)
      (or (when (isa? %info J2SNarrowInfo)
	     (with-access::J2SNarrowInfo %info (defblock narrowable)
		(when narrowable
		   (instantiate::J2SNop (loc loc)))))
	  this)))

;*---------------------------------------------------------------------*/
;*    j2s-narrow-body! ::J2SDeclFun ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow-body! this::J2SDeclFun)
   (with-access::J2SDeclFun this (val)
      (j2s-narrow-fun! val))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-narrow-body! ::J2SRef ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow-body! this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info)
	 (if (isa? %info J2SNarrowInfo)
	     (with-access::J2SNarrowInfo %info (ldecl narrowable)
		(if narrowable
		    (begin
		       (set! decl ldecl)
		       this)
		    (call-default-walker)))
	     (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    j2s-narrow-body! ::J2SStmtExpr ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow-body! this::J2SStmtExpr)
   
   (define (decl->let!::J2SDecl decl::J2SDecl)
      (with-access::J2SDecl decl (binder scope)
	 (set! scope 'local)
	 (set! binder 'let)
	 decl))

   (define (patch-defblock! block::J2SBlock decl::J2SDecl)
      (let loop ((block block))
	 (if (isa? block J2SLetBlock)
	     (with-access::J2SLetBlock block (decls nodes)
		(unless (memq decl decls)
		   ;; explicit test for variable re-definition
		   (set! decls (append! decls (list decl)))))
	     (with-access::J2SBlock block (endloc loc nodes %info)
		(if (isa? %info J2SLetBlock)
		    (loop %info)
		    (let ((lblock (instantiate::J2SLetBlock
				     (endloc endloc)
				     (loc loc)
				     (decls (list decl))
				     (nodes nodes))))
		       (set! nodes (list lblock))
		       (set! %info lblock)))))))
   
   (with-access::J2SStmtExpr this (expr)
      (or (when (isa? expr J2SInit)
	     (with-access::J2SInit expr (lhs rhs loc)
		(when (isa? lhs J2SRef)
		   (with-access::J2SRef lhs (decl)
		      (when (isa? decl J2SDecl)
			 (with-access::J2SDecl decl (id %info)
			    (when (isa? %info J2SNarrowInfo)
			       (with-access::J2SNarrowInfo %info (narrowable defblock ldecl)
				  (when narrowable
				     ;; (tprint "  +-- NARROWING: " id)
				     (set! ldecl (decl->let! decl))
				     (patch-defblock! defblock ldecl)
				     this)))))))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    j2s-narrow-body! ::J2SBlock ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow-body! this::J2SBlock)
   (call-default-walker)
   (with-access::J2SBlock this (nodes)
      (set! nodes (filter! (lambda (n) (not (isa? n J2SNop))) nodes))
      this))
   
;*---------------------------------------------------------------------*/
;*    j2s-lift-inits! ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-lift-inits! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-lift-inits! ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-lift-inits! this::J2SLetBlock)
   
   (define (lift? node::J2SNode decls)
      ;; lift everything that is not an initilazation of one decls
      (or (not (isa? node J2SStmtExpr))
	  (with-access::J2SStmtExpr node (expr)
	     (or (not (isa? expr J2SInit))
		 (with-access::J2SInit expr (lhs)
		    (or (not (isa? lhs J2SRef))
			(with-access::J2SRef lhs (decl)
			   (not (memq decl decls)))))))))
   
   (call-default-walker)
   (with-access::J2SLetBlock this (nodes decls)
      (if (and (pair? nodes) (isa? (car nodes) J2SSeq))
	  (with-access::J2SSeq (car nodes) ((snodes nodes))
	     (let loop ((inodes snodes)
			(lifts '()))
		(cond
		   ((null? inodes)
		    this)
		   ((lift? (car inodes) decls)
		    (loop (cdr inodes) (cons (car inodes) lifts)))
		   ((null? lifts)
		    this)
		   (else
		    (set! snodes inodes)
		    (duplicate::J2SBlock this
		       (nodes (reverse! (cons this lifts))))))))
	  this)))

;*---------------------------------------------------------------------*/
;*    j2s-find-non-init-vars* ...                                      */
;*    -------------------------------------------------------------    */
;*    Find all the variable (declarations) that are used but           */
;*    never initialized (for instance when the variable is assigned).  */
;*    		                                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-find-non-init-vars* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-find-non-init-vars* ::J2SDecl ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-find-non-init-vars* this::J2SDecl)
   (with-access::J2SDecl this (%info)
      (if (and (isa? %info J2SNarrowInfo) (not (isa? this J2SDeclFun)))
	  (with-access::J2SNarrowInfo %info (useblocks defblock)
	     (if (and (not defblock) (pair? useblocks))
		 (list this)
		 '()))
	  '())))

;*---------------------------------------------------------------------*/
;*    build-body-btree ::J2SNode ...                                   */
;*    -------------------------------------------------------------    */
;*    Build the J2SBlock tree in order to find a block parent          */
;*    and to implement the predicate included-in?                      */
;*---------------------------------------------------------------------*/
(define-walk-method (build-body-btree this::J2SNode)
   (let ((t (call-default-walker)))
      (when (pair? t) t)))

;*---------------------------------------------------------------------*/
;*    build-body-btree ::J2SBlock ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (build-body-btree this::J2SBlock)
   (with-access::J2SBlock this (nodes)
      (cons this (filter-map build-body-btree nodes))))
   
;*---------------------------------------------------------------------*/
;*    find-drop-block ...                                              */
;*    -------------------------------------------------------------    */
;*    Drop down a variable declaration is the top-most ancestor of     */
;*    all its uses.                                                    */
;*---------------------------------------------------------------------*/
(define (find-drop-block decl::J2SDecl btree::pair)
   ;; (tprint "j2s-assign decl=" (j2s->list decl) " " (btree->list btree))
   (with-access::J2SDecl decl (%info)
      (when (isa? %info J2SNarrowInfo)
	 (with-access::J2SNarrowInfo %info (useblocks)
	    (let loop ((blocks (cdr useblocks))
		       (block (car useblocks)))
	       (if (null? blocks)
		   (unless (eq? block (car btree))
		      block)
		   (loop (cdr blocks)
		      (find-common-ancestor block (car blocks) btree))))))))

(define (btree->list btree)
   (cond
      ((isa? btree J2SNode)
       (with-access::J2SNode btree (loc) loc))
      ((pair? btree)
       (cons (btree->list (car btree)) (btree->list (cdr btree))))
      (else btree)))

;*---------------------------------------------------------------------*/
;*    find-common-ancestor ...                                         */
;*---------------------------------------------------------------------*/
(define (find-common-ancestor block1 block2 btree)

   (define (find-path block btree)
      (cond
	 ((null? btree)
	  #f)
	 ((eq? block (car btree))
	  (list (car btree)))
	 (else
	  (let ((subpath (find (lambda (bt) (find-path block bt)) (cdr btree))))
	     (when subpath
		(cons (car btree) subpath))))))

   (if (eq? block1 block2)
       block1
       (begin
	  ;; (tprint "P1=" (btree->list (find-path block1 btree)))
	  ;; (tprint "BT=" (btree->list btree))
	  (let loop ((path1 (find-path block1 btree))
		     (path2 (find-path block2 btree))
		     (parent (car btree)))
	     (cond
		((null? path1)
		 parent)
		((null? path2)
		 parent)
		((eq? (car path1) (car path2))
		 (loop (cdr path1) (cdr path2) (car path1)))
		(else
		 parent))))))

;*---------------------------------------------------------------------*/
;*    find-drop-assig ...                                              */
;*    -------------------------------------------------------------    */
;*    Find a declaration assignment in drop block.                     */
;*---------------------------------------------------------------------*/
(define (find-drop-assig decl assigs block)
   (let ((a (find (lambda (a)
		     (and (eq? (vector-ref a 0) decl)
			  (eq? (vector-ref a 1) block)))
	       assigs)))
      (when a (vector-ref a 2))))

;*---------------------------------------------------------------------*/
;*    collect-assig ::J2SNode ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-assig* this::J2SNode decls block)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-assig* ::J2SBlock ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-assig* this::J2SBlock decls block)
   (with-access::J2SBlock this (nodes)
      (append-map (lambda (n) (collect-assig* n decls this)) nodes)))

;*---------------------------------------------------------------------*/
;*    collect-assig* ::J2SAssig ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-assig* this::J2SAssig decls block)
   (with-access::J2SAssig this (lhs rhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (if (memq decl decls)
		 ;; no need to descent the rhs
		 (list (vector decl block this))
		 (append (collect-assig* lhs decls block)
		    (collect-assig* rhs decls block))))
	  (append (collect-assig* lhs decls block)
	     (collect-assig* rhs decls block)))))
	  
;*---------------------------------------------------------------------*/
;*    rewrite-assig! ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (rewrite-assig! this::J2SNode assig)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    rewrite-assig! ::J2SAssig ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (rewrite-assig! this::J2SAssig assig)
   (if (eq? this assig)
       (duplicate::J2SInit this)
       (call-default-walker)))
   
