;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/narrow.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 25 07:41:22 2015                          */
;*    Last change :  Tue May  1 16:07:47 2018 (serrano)                */
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
;*    j2s-narrow-stage ...                                             */
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
      (format "[[J2SNarrowInfo defblock=~s narrowable=~s useblocks=~(, )]]"
	 (when (isa? defblock J2SNode)
	     (with-access::J2SNode defblock (loc)
		loc))
	 narrowable
	 (if (pair? useblocks)
	     (map (lambda (b)
		     (with-access::J2SBlock b (loc)
			(format "~s" loc)))
		useblocks)
	     '()))))

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
;*    j2s-narrow-fun/w-init! ...                                       */
;*    -------------------------------------------------------------    */
;*    For variables that are not initialize (but setted) try to        */
;*    find one of the assignments that could act as an initializer.    */
;*---------------------------------------------------------------------*/
(define (j2s-narrow-fun/w-init! o::J2SFun)
   (with-access::J2SFun o (body)
      (let ((uvars (j2s-find-non-init-vars* body)))
	 (when (pair? uvars)
	    (mark-block-parent body body)
	    (let ((assigs (collect-assig* body uvars body)))
	       (for-each (lambda (uvar)
			    (with-access::J2SDecl uvar (id)
			       (let ((block (find-drop-block uvar body)))
				  (when block
				     (let ((assig (find-drop-assig
						     uvar assigs block)))
					(when (always-executed? block assig)
					   ;;(tprint "ASSIG=" (j2s->list assig))
					   (rewrite-assig! body assig)
					   (with-access::J2SDecl uvar (%info)
					      (with-access::J2SNarrowInfo %info
						    (defblock narrowable ldecl)
						 (set! defblock block)
						 (set! ldecl uvar)
						 (set! narrowable #t)))))))))
		  uvars))))))

;*---------------------------------------------------------------------*/
;*    j2s-narrow-fun! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-narrow-fun! o::J2SFun)
   (with-access::J2SFun o (body)
      ;; find the declaring block of all declarations
      (j2s-find-init-blocks body body o)
      ;; find the variables used but not initialized
      (j2s-narrow-fun/w-init! o)
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
(define-walk-method (j2s-find-init-blocks this::J2SFun block fun)
   (with-access::J2SFun this (body)
      (j2s-find-init-blocks body body this)))

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
			    (when block
			       (set! useblocks
				  (if (memq block useblocks)
				      useblocks
				      (cons block useblocks))))
			    (set! defblock block)
			    (set! deffun fun))
			 (set! %info
			    (instantiate::J2SNarrowInfo
			       (deffun fun)
			       (useblocks (if block (list block) '()))
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
		   (when block
		      (set! useblocks
			 (if (memq block useblocks)
			     useblocks
			     (cons block useblocks)))))
		(set! %info
		   (instantiate::J2SNarrowInfo
		      (useblocks (if block (list block) '()))
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
;*    -------------------------------------------------------------    */
;*    Move down the narrowable variables.                              */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow-body! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-narrow-body! ::J2SFun ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow-body! this::J2SFun)
   ;;(call-default-walker)
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
      (with-access::J2SFun val (body)
	 (set! body (j2s-narrow-body! body)))
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
;*    j2s-narrow-init! ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-narrow-init! this::J2SInit)
   
   (define (decl->let!::J2SDecl decl::J2SDecl)
      (with-access::J2SDecl decl (binder scope %info)
	 (set! scope 'local)
	 (set! binder 'let)
	 (set! %info #f)
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
   
   (with-access::J2SInit this (lhs rhs loc)
      (set! rhs (j2s-narrow-body! rhs))
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	    (when (isa? decl J2SDecl)
	       (with-access::J2SDecl decl (id %info)
		  (when (isa? %info J2SNarrowInfo)
		     (with-access::J2SNarrowInfo %info (narrowable defblock ldecl)
			(when narrowable
			   (set! ldecl (decl->let! decl))
			   (patch-defblock! defblock ldecl)
			   this)))))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-narrow-body! ::J2SStmtExpr ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow-body! this::J2SStmtExpr)
   (with-access::J2SStmtExpr this (expr)
      (if (and (isa? expr J2SInit) (j2s-narrow-init! expr))
	  this
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    j2s-narrow-body! ::J2SInit ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow-body! this::J2SInit)
   (or (j2s-narrow-init! this) (call-default-walker)))


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
      ;; lift everything that is not an initialization of one decls
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
;*    mark-block-parent ::J2SNode ...                                  */
;*    -------------------------------------------------------------    */
;*    Add a parent link (in the %info field) to all blocks.            */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-block-parent this::J2SNode parent)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-block-parent ::J2SBlock ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-block-parent this::J2SBlock parent)
   (with-access::J2SBlock this (nodes %info)
      (set! %info parent)
      (for-each (lambda (n) (mark-block-parent n this)) nodes)))
      
;*---------------------------------------------------------------------*/
;*    find-drop-block ...                                              */
;*    -------------------------------------------------------------    */
;*    Drop down a variable declaration is the top-most ancestor of     */
;*    all its uses.                                                    */
;*---------------------------------------------------------------------*/
(define (find-drop-block decl::J2SDecl parent::J2SNode)
   ;;(tprint "j2s-assign decl=" (j2s->list decl))
   (with-access::J2SDecl decl (%info)
      (when (isa? %info J2SNarrowInfo)
	 (with-access::J2SNarrowInfo %info (useblocks)
	    ;;(tprint "blocks=" (block*->list useblocks))
	    (let loop ((blocks (cdr useblocks))
		       (block (car useblocks)))
	       (if (null? blocks)
		   (unless (eq? block parent)
		      ;;(tprint "block=" (car (block*->list (list block))))
		      block)
		   (loop (cdr blocks)
		      (find-common-ancestor block (car blocks) parent))))))))

(define (block*->list bs)
   (map (lambda (b) (with-access::J2SBlock b (loc) loc)) bs))

;*---------------------------------------------------------------------*/
;*    find-common-ancestor ...                                         */
;*---------------------------------------------------------------------*/
(define (find-common-ancestor block1 block2 parent)

   (define (build-path block)
      (let loop ((block block)
		 (res '()))
	 (with-access::J2SBlock block (%info)
	    (if (eq? block %info)
		res
		(loop %info (cons block res))))))

   (if (eq? block1 block2)
       block1
       (let loop ((path1 (build-path block1))
		  (path2 (build-path block2))
		  (parent parent))
	  (cond
	     ((null? path1)
	      parent)
	     ((null? path2)
	      parent)
	     ((eq? (car path1) (car path2))
	      (loop (cdr path1) (cdr path2) (car path1)))
	     (else
	      parent)))))

;*---------------------------------------------------------------------*/
;*    find-drop-assig ...                                              */
;*    -------------------------------------------------------------    */
;*    Find a declaration assignment in the drop block.                 */
;*---------------------------------------------------------------------*/
(define (find-drop-assig decl assigs block)
   (let ((a (find (lambda (a)
		     (and (eq? (vector-ref a 0) decl)
			  (eq? (vector-ref a 1) block)))
	       assigs)))
      (when a (vector-ref a 2))))

;*---------------------------------------------------------------------*/
;*    always-executed? ...                                             */
;*    -------------------------------------------------------------    */
;*    Check that in the block, the assigned variable is never used     */
;*    before its assignment is executed.                               */
;*---------------------------------------------------------------------*/
(define (always-executed? block assig)
   (when assig
      (let ((cell (make-cell #unspecified)))
	 (always-executed block assig cell)
	 (eq? (cell-ref cell) #t))))

;*---------------------------------------------------------------------*/
;*    always-executed ::J2SNode ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (always-executed this::J2SNode assig::J2SAssig cell::cell)
   (when (eq? (cell-ref cell) #unspecified)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    always-executed ::J2SAssig ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (always-executed this::J2SAssig assig cell)
   (with-access::J2SAssig this (lhs rhs)
      (always-executed rhs assig cell)
      (cond
	 ((not (eq? (cell-ref cell) #unspecified))
	  #unspecified)
	 ((not (isa? lhs J2SRef))
	  (always-executed lhs assig cell))
	 ((eq? this assig)
	  (cell-set! cell #t))
	 (else
	  (always-executed lhs assig cell)))))

;*---------------------------------------------------------------------*/
;*    always-executed ::J2SRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (always-executed this::J2SRef assig cell)
   (with-access::J2SRef this (decl)
      (with-access::J2SAssig assig (lhs)
	 (with-access::J2SRef lhs ((adecl decl))
	    (when (eq? adecl decl)
	       (cell-set! cell #f))))))

;*---------------------------------------------------------------------*/
;*    always-executed-condition ...                                    */
;*---------------------------------------------------------------------*/
(define (always-executed-condition test then else assig cell)
   (when (eq? (cell-ref cell) #unspecified)
      (always-executed test assig cell)
      (when (eq? (cell-ref cell) #unspecified)
	 (always-executed test assig cell)
	 ;; check that the initialization does not occur in one
	 ;; of the two branches of the test
	 (if (eq? (cell-ref cell) #t)
	     (cell-set! cell #f)
	     (begin
		(always-executed else assig cell)
		(when (eq? (cell-ref cell) #t)
		   (cell-set! cell #f)))))))

;*---------------------------------------------------------------------*/
;*    always-executed ::J2SIf ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (always-executed this::J2SIf assig cell)
   (with-access::J2SIf this (test then else)
      (always-executed-condition test then else assig cell)))

;*---------------------------------------------------------------------*/
;*    always-executed ::J2SCond ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (always-executed this::J2SCond assig cell)
   (with-access::J2SCond this (test then else)
      (always-executed-condition test then else assig cell)))

;*---------------------------------------------------------------------*/
;*    always-executed ::J2SLoop ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (always-executed this::J2SLoop assig cell)
   (when (eq? (cell-ref cell) #unspecified)
      (call-default-walker)
      (when (eq? (cell-ref cell) #t)
	 (cell-set! cell #f))))
   
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
;*    collect-assig* ::J2SLoop ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-assig* this::J2SLoop decls block)
   (with-access::J2SLoop this (test incr body)
      ;; ignore consider the body part of the loops
      (collect-assig* body decls block)))

;*---------------------------------------------------------------------*/
;*    collect-assig* ::J2SSwitch ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-assig* this::J2SSwitch decls block)
   (with-access::J2SSwitch this (cases)
      ;; only consider case bodies
      (append-map (lambda (c)
		     (with-access::J2SCase c (body)
			(collect-assig* body decls block)))
	 cases)))

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
   
