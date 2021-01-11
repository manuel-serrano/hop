;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/letfun.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 06:35:14 2015                          */
;*    Last change :  Fri Jan 31 08:04:38 2020 (serrano)                */
;*    Copyright   :  2015-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Let function optimization                                        */
;*    -------------------------------------------------------------    */
;*    Rewrite the following pattern:                                   */
;*       var f = function() { ... }                                    */
;*    info                                                             */
;*       function f() { ... }                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_letfun

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer
	   __js2scheme_alpha)

   (export j2s-letfun-stage))

;*---------------------------------------------------------------------*/
;*    j2s-letun-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-letfun-stage
   (instantiate::J2SStageProc
      (name "letfun")
      (comment "Implicit function declarations")
      (proc j2s-letfun)
      (optional :optim-letfun)))

;*---------------------------------------------------------------------*/
;*    j2s-letfun ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-letfun this args)
   (when (isa? this J2SProgram)
      (letfun! this args)))

;*---------------------------------------------------------------------*/
;*    letfun! ...                                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (letfun! this::J2SNode args)
   (call-default-walker))

(define (dump n)
   (with-access::J2SInit n (lhs)
      (with-access::J2SRef lhs (decl)
	 (j2s->list decl))))
   
;*---------------------------------------------------------------------*/
;*    letfun! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (letfun! this::J2SFun args)
   (call-default-walker)
   (with-access::J2SFun this (body)
      (if (isa? body J2SBlock)
	  (multiple-value-bind (vars nodes)
	     (block-collect-vars body)
	     (multiple-value-bind (inits rest)
		(nodes-collect-inits nodes)
		(let ((finits (filter (lambda (i) (init-fun? i vars)) inits))
		      (assigs (filter (lambda (i) (assig-nofun? i vars)) inits)))
		   (when (pair? finits)
		      (let* ((odecls (map initvdecl finits))
			     (ndecls (map initfdecl finits))
			     (vdecls (filter-map initvdecl assigs))
			     (noinits (filter (lambda (v)
						 (and (not (memq v odecls))
						      (not (memq v vdecls))))
					 vars))
			     (nblock (duplicate::J2SBlock body
					(nodes rest))))
			 (set! body 
			    (duplicate::J2SBlock body
			       (nodes (append
					 (map (lambda (n)
						 (decl-alpha n odecls ndecls))
					    noinits)
					 (map (lambda (n)
						 (decl-alpha n odecls ndecls))
					    vdecls)
					 (map (lambda (n)
						 (decl-alpha n odecls ndecls))
					    ndecls)
					 (map (lambda (n)
						 (assig-alpha n odecls ndecls))
					    assigs)
					 (list (j2s-alpha nblock
						  odecls ndecls)))))))))))))
   this)

;*---------------------------------------------------------------------*/
;*    init-fun? ...                                                    */
;*---------------------------------------------------------------------*/
(define (init-fun? this::J2SAssig vars)
   (when (isa? this J2SInit)
      (with-access::J2SAssig this (lhs rhs)
	 (when (isa? rhs J2SFun)
	    (with-access::J2SRef lhs (decl)
	       (memq decl vars))))))

;*---------------------------------------------------------------------*/
;*    assig-nofun? ...                                                 */
;*---------------------------------------------------------------------*/
(define (assig-nofun? this::J2SAssig vars)
   (or (not (isa? this J2SInit))
       (not (init-fun? this vars))))

;*---------------------------------------------------------------------*/
;*    initvdecl ...                                                    */
;*---------------------------------------------------------------------*/
(define (initvdecl this::J2SAssig)
   (when (isa? this J2SInit)
      (with-access::J2SAssig this (lhs rhs)
	 (with-access::J2SRef lhs (decl)
	    decl))))

;*---------------------------------------------------------------------*/
;*    initfdecl ...                                                    */
;*---------------------------------------------------------------------*/
(define (initfdecl this::J2SInit)
   (with-access::J2SInit this (lhs rhs)
      (with-access::J2SRef lhs (decl)
	 (with-access::J2SDecl decl (id writable scope usage binder utype vtype loc)
	    (instantiate::J2SDeclFun
	       (loc loc)
	       (id id)
	       (writable writable)
	       (scope scope)
	       (usage usage)
	       (binder binder)
	       (utype utype)
	       (vtype vtype)
	       (val rhs))))))

;*---------------------------------------------------------------------*/
;*    decl-alpha ...                                                   */
;*---------------------------------------------------------------------*/
(define (decl-alpha d::J2SDecl olds news)
   (when (isa? d J2SDeclInit)
      (with-access::J2SDeclInit d (val)
	 (set! val (j2s-alpha val olds news))))
   d)

;*---------------------------------------------------------------------*/
;*    assig-alpha ...                                                  */
;*---------------------------------------------------------------------*/
(define (assig-alpha n::J2SAssig olds news)
   (with-access::J2SAssig n (loc)
      (instantiate::J2SStmtExpr
	 (loc loc)
	 (expr (j2s-alpha n olds news)))))

;*---------------------------------------------------------------------*/
;*    block-collect-vars ...                                           */
;*---------------------------------------------------------------------*/
(define (block-collect-vars node::J2SBlock)
   (with-access::J2SBlock node (nodes)
      (let loop ((nodes nodes)
		 (vars '()))
	 (cond
	    ((null? nodes)
	     (values (reverse! vars) '()))
	    ((isa? (car nodes) J2SDecl)
	     (loop (cdr nodes) (cons (car nodes) vars)))
	    (else
	     (values (reverse! vars) nodes))))))

;*---------------------------------------------------------------------*/
;*    nodes-collect-inits ...                                          */
;*---------------------------------------------------------------------*/
(define (nodes-collect-inits nodes::pair-nil)

   (define (safe-access? rhs blacklist)
      (when (isa? rhs J2SAccess)
	 (with-access::J2SAccess rhs (obj field)
	    (and (safe-expr? obj blacklist) (safe-expr? field blacklist)))))

   (define (safe-array? rhs blacklist)
      (when (isa? rhs J2SArray)
	 (with-access::J2SArray rhs (exprs)
	    (every (lambda (e) (safe-expr? e blacklist)) exprs))))

   (define (safe-objinit? rhs blacklist)
      
      (define (safe-init? init)
	 (when (isa? init J2SDataPropertyInit)
	    (with-access::J2SDataPropertyInit init (name val)
	       (and (safe-expr? name blacklist) (safe-expr? val blacklist)))))
   
      (when (isa? rhs J2SObjInit)
	 (with-access::J2SObjInit rhs (inits)
	    (every safe-init? inits))))
		    
   (define (safe-ref? rhs)
      (when (isa? rhs J2SRef)
	 (with-access::J2SRef rhs (decl)
	    (with-access::J2SDecl decl (scope binder id)
	       (or (memq scope '(%scope global))
		   (eq? binder 'param))))))

   (define (safe-expr? expr blacklist)
      (or (isa? expr J2SFun)
	  (isa? expr J2SLiteral)
	  (safe-ref? expr)
	  (when (isa? expr J2SRef)
	     (with-access::J2SRef expr (decl)
		 (unless (memq decl blacklist)
		    (set-cdr! blacklist (cons decl (cdr blacklist)))
		    #t)))
	  (safe-access? expr blacklist)
	  (safe-array? expr blacklist)
	  (safe-objinit? expr blacklist)))

   (define (init node blacklist)
      (when (isa? node J2SStmtExpr)
	 (with-access::J2SStmtExpr node (expr)
	    (when (isa? expr J2SAssig)
	       (with-access::J2SAssig expr (rhs)
		  (when (safe-expr? rhs blacklist)
		     expr))))))
   
   (let loop ((nodes nodes)
	      (inits '())
	      (blacklist (cons 'mark '())))
      (cond
	 ((null? nodes)
	  (values (reverse! inits) '()))
	 ((isa? (car nodes) J2SLetBlock)
	  (values (reverse! inits) nodes))
	 ((isa? (car nodes) J2SSeq)
	  (with-access::J2SSeq (car nodes) ((bnodes nodes))
	     (loop (append bnodes (cdr nodes)) inits blacklist)))
	 ((init (car nodes) blacklist)
	  =>
	  (lambda (init)
	     (loop (cdr nodes) (cons init inits) blacklist)))
	 (else
	  (values (reverse! inits) nodes)))))

	 
   
