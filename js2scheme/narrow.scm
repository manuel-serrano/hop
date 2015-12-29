;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/narrow.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 25 07:41:22 2015                          */
;*    Last change :  Mon Dec 28 19:23:49 2015 (serrano)                */
;*    Copyright   :  2015 Manuel Serrano                               */
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

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer)

   (static (class J2SNarrowInfo
	      (defblock::obj (default #f))
	      (narrowable::bool (default #t))
	      (ldecl (default #f))))

   (export j2s-narrow-stage
	   (generic j2s-narrow ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-narrow-stage                                                 */
;*---------------------------------------------------------------------*/
(define j2s-narrow-stage
   (instantiate::J2SStageProc
      (name "narrow")
      (comment "Narrow resolution")
      (proc j2s-narrow)))

;*---------------------------------------------------------------------*/
;*    j2s-narrow ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (j2s-narrow this conf)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-narrow ::J2SProgram ...                                      */
;*    -------------------------------------------------------------    */
;*    Warning, headers are not scanned for variable resolution!        */
;*---------------------------------------------------------------------*/
(define-method (j2s-narrow this::J2SProgram conf)
   (with-access::J2SProgram this (decls)
      ;; statement optimization
      (for-each (lambda (o)
		   (when (isa? o J2SDeclFun)
		      (j2s-narrow-fun! o)))
	 decls))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-narrow-fun! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-narrow-fun! o::J2SDeclFun)
   (with-access::J2SDeclFun o (id)
      (tprint "Narrow " id))
   ;; find the declaring block of all declaration
   (j2s-find-init-blocks o #f)
   ;; get the set of narrowable declrations
   (j2s-mark-narrowable o '() #f)
   ;; narrow the function body
   (j2s-narrow! o))

;*---------------------------------------------------------------------*/
;*    j2s-init-blocks! ...                                             */
;*    -------------------------------------------------------------    */
;*    For each declaration, finds the block that initializes it.       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-find-init-blocks this::J2SNode block)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-find-init-blocks ::J2SBlock ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-find-init-blocks this::J2SBlock block)
   (with-access::J2SBlock this (nodes)
      (for-each (lambda (b) (j2s-find-init-blocks b this)) nodes)))

;*---------------------------------------------------------------------*/
;*    j2s-find-init-blocks ::J2SInit ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-find-init-blocks this::J2SInit block)
   (when block
      (with-access::J2SInit this (lhs)
	 (with-access::J2SRef lhs (decl)
	    (unless (or (isa? decl J2SLet) (isa? decl J2SParam))
	       ;; skip let/const declarations
	       (with-access::J2SDecl decl (%info)
		  (unless (isa? %info J2SNarrowInfo)
		     (set! %info
			(instantiate::J2SNarrowInfo
			   (defblock block))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-find-init-blocks ::J2SRef ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-find-init-blocks this::J2SRef block)
   (with-access::J2SRef this (decl)
      (unless (or (isa? decl J2SLet) (isa? decl J2SParam))
	 ;; skip let/const declarations
	 (with-access::J2SDecl decl (%info)
	    (unless (isa? %info J2SNarrowInfo)
	       (set! %info
		  (instantiate::J2SNarrowInfo
		     (narrowable #f))))))))

;*---------------------------------------------------------------------*/
;*    j2s-mark-narrowable ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-narrowable this::J2SNode blocks::pair-nil inloop)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-mark-narrowable ::J2SBlock ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-narrowable this::J2SBlock blocks inloop)
   (let ((nblocks (cons this blocks)))
      (with-access::J2SBlock this (nodes)
	 (for-each (lambda (b) (j2s-mark-narrowable b nblocks inloop)) nodes))))

;*---------------------------------------------------------------------*/
;*    j2s-mark-narrowable ::J2SRef ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-narrowable this::J2SRef blocks inloop)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info)
	 (when (isa? %info J2SNarrowInfo)
	    (with-access::J2SNarrowInfo %info (defblock narrowable)
	       (unless (memq defblock blocks)
		  (set! narrowable #f)))))))

;*---------------------------------------------------------------------*/
;*    j2s-narrow! ...                                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-narrow! ::J2SDecl ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow! this::J2SDecl)
   (call-default-walker)
   (with-access::J2SDecl this (loc %info)
      (or (when (isa? %info J2SNarrowInfo)
	     (with-access::J2SNarrowInfo %info (defblock narrowable)
		(when narrowable
		   (instantiate::J2SNop (loc loc)))))
	  this)))

;*---------------------------------------------------------------------*/
;*    j2s-narrow! ::J2SRef ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-narrow! this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info)
	 (if (isa? %info J2SNarrowInfo)
	     (with-access::J2SNarrowInfo %info (ldecl)
		(tprint "ldecl=" (typeof ldecl) " ref=" (j2s->list this))
		(set! decl ldecl)
		this)
	     (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    j2s-narrow! ::J2SFor ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-narrow! this::J2SFor)
   
   (define (init-let node::J2SNode)
      (when (isa? node J2SStmtExpr)
	 (with-access::J2SStmtExpr node (expr)
	    (when (isa? expr J2SInit)
	       (with-access::J2SInit expr (lhs)
		  (when (isa? lhs J2SRef)
		     (with-access::J2SRef lhs (decl)
			(with-access::J2SDecl decl (%info)
			   (when (isa? %info J2SNarrowInfo)
			      expr)))))))))
   
   (define (get-let-inits! init)
      (when (isa? init J2SSeq)
	 (with-access::J2SSeq init (nodes)
	    (let loop ((n nodes)
		       (inits '())
		       (decls '()))
	       (cond
		  ((null? n)
		   (when (pair? decls)
		      (set! nodes (reverse! inits))
		      decls))
		  ((init-let (car n))
		   =>
		   (lambda (decl)
		      (loop (cdr n) inits (cons decl decls))))
		  (else
		   (loop (cdr n) (cons (car n) inits) decls)))))))
   

   (tprint "j2sfor " (j2s->list this))
   (with-access::J2SFor this (init test incr loc body)
      (let ((decls (get-let-inits! init)))
	 (if (pair? decls)
	     (with-access::J2SBlock body (endloc)
		(j2s-narrow!
		   (instantiate::J2SLetBlock
		      (loc loc)
		      (endloc endloc)
		      (decls decls)
		      (nodes (list this)))))
	     (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    j2s-narrow! ::J2SStmtExpr ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow! this::J2SStmtExpr)
   
   (define (decl->let::J2SLet decl::J2SDecl)
      (with-access::J2SDecl decl (id loc)
	 (let ((new (if (isa? decl J2SDeclInit)
			(with-access::J2SDeclInit decl (val)
			   (instantiate::J2SLetInit
			      (id id)
			      (loc loc)
			      (val val)))
			(instantiate::J2SLet
			   (id id)
			   (loc loc)))))
	    (let ((fields (class-all-fields (object-class decl))))
	       (let loop ((i (-fx (vector-length fields) 1)))
		  (when (>=fx i 0)
		     (let* ((f (vector-ref fields i))
			    (get (class-field-accessor f))
			    (set (class-field-mutator f)))
			(when set
			   (set new (get decl))
			   (loop (-fx i 1)))))))
	    new)))
   
   (define (init-decl->let::J2SInitLet init::J2SInit)
      (with-access::J2SInit init (loc lhs rhs)
	 (instantiate::J2SInitLet
	    (lhs lhs)
	    (rhs rhs)
	    (loc loc))))
   
   (define (patch-defblock! block::J2SBlock decl::J2SLet)
      (let loop ((block block))
	 (if (isa? block J2SLetBlock)
	     (with-access::J2SLetBlock block (decls)
		(set! decls (append! decls (list decl))))
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
		(with-access::J2SRef lhs (decl)
		   (when (isa? decl J2SDecl)
		      (with-access::J2SDecl decl (%info)
			 (when (isa? %info J2SNarrowInfo)
			    (with-access::J2SNarrowInfo %info (narrowable defblock ldecl)
			       (when narrowable
				  (tprint "  +-- NARROWING: " (j2s->list this))
				  (set! ldecl (decl->let decl))
				  (set! decl ldecl)
				  (patch-defblock! defblock ldecl)
				  (set! expr (init-decl->let expr))
				  this))))))))
	  (call-next-method))))
