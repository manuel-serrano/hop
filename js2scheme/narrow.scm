;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/narrow.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 25 07:41:22 2015                          */
;*    Last commit :                                                    */
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
	      (narrowable::bool (default #t))))

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
      (tprint "*** NARROW " id))
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
		     (tprint "INIT=" (j2s->list this) " block=" (typeof block))
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
	       (tprint "INVALIDATE " (j2s->list this))
	       (set! %info
		  (instantiate::J2SNarrowInfo
		     (narrowable #f))))))))

;*---------------------------------------------------------------------*/
;*    j2s-mark-narrowable ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-mark-narrowable this::J2SNode blocks::pair-nil inloop)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-mark-narrowable ::J2SBlock ...                              */
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
		  (tprint "INVALIDATE.2 " (j2s->list this))
		  (set! narrowable #f)))))))

;*---------------------------------------------------------------------*/
;*    show-narrowable ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (show-narrowable this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    show-narrowable ::J2SDecl ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (show-narrowable this::J2SDecl)
   (call-default-walker)
   (with-access::J2SDecl this (%info)
      (when (isa? %info J2SNarrowInfo)
	 (with-access::J2SNarrowInfo %info (defblock narrowable)
	    (tprint "NARROWABLE: " (j2s->list this)
	       " defblock=" (typeof defblock) " narrowable=" narrowable)))))

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
;*    j2s-narrow! ::J2SStmtExpr ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-narrow! this::J2SStmtExpr)
   
   (define (copy-decl decl::J2SDecl loc rhs)
      (with-access::J2SDecl decl (id)
	 (let ((res (instantiate::J2SDeclInit
		       (id id)
		       (loc loc)
		       (val rhs)))
	       (fields (class-all-fields J2SDecl)))
	    (let loop ((i (-fx (vector-length fields) 1)))
	       (when (>=fx i 0)
		  (let ((f (vector-ref fields i)))
		     (when (class-field-mutable? f)
			(let ((set (class-field-mutator f))
			      (get (class-field-accessor f)))
			   (set res (get decl))))
		     (loop (-fx i 1)))))
	    res)))
   
   (with-access::J2SStmtExpr this (expr)
      (or (when (isa? expr J2SInit)
	     (with-access::J2SInit expr (lhs rhs loc)
		(with-access::J2SRef lhs (decl)
		   (when (isa? decl J2SDecl)
		      (tprint "INIT expr: " (j2s->list expr))
		      (with-access::J2SDecl decl (%info)
			 (when (isa? %info J2SNarrowInfo)
			    (with-access::J2SNarrowInfo %info (narrowable)
			       (when narrowable
				  (copy-decl decl loc rhs)))))))))
	  (call-next-method))))
