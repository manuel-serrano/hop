;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/imethod.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Mon Nov 27 16:40:18 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
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
(module __js2scheme_imethod

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint
	   __js2scheme_alpha
	   __js2scheme_node-size)

   (export j2s-imethod-stage))

;*---------------------------------------------------------------------*/
;*    j2s-imethod-stage ...                                            */
;*---------------------------------------------------------------------*/
(define j2s-imethod-stage
   (instantiate::J2SStageProc
      (name "imethod")
      (comment "Method inlining optimization")
      (optional :optim-imethod)
      (proc j2s-imethod!)))

;*---------------------------------------------------------------------*/
;*    j2s-imethod! ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-imethod! this args)
   (if (isa? this J2SProgram)
       (with-access::J2SProgram this (decls nodes)
	  (let ((pms (ptable (append-map collect-proto-methods* nodes))))
	     (tprint "PMS:")
	     (hashtable-for-each pms
		(lambda (k v)
		   (tprint "k=" k)))
	     (inline-method!* decls pms this args)
	     (inline-method!* nodes pms this args)
	     this))
       this))

;*---------------------------------------------------------------------*/
;*    iinfo                                                            */
;*---------------------------------------------------------------------*/
(define-struct inlinfo size obody)
(define-struct callinfo call callees ifctx loopctx)

;*---------------------------------------------------------------------*/
;*    get-method-size! ...                                             */
;*---------------------------------------------------------------------*/
(define (get-method-size! this::J2SFun)
   (with-access::J2SFun this (%info body)
      (unless (inlinfo? %info)
	 (set! %info (inlinfo (node-size this) body)))
      (inlinfo-size %info)))
			     
;*---------------------------------------------------------------------*/
;*    collect-proto-methods* ...                                       */
;*    -------------------------------------------------------------    */
;*    Collect the inline candidates, which are the top level methods   */
;*    assigned to constructor prototypes.                              */
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

   (define blacklist
      (if (string? (getenv "BLACKLIST"))
	  (call-with-input-string (getenv "BLACKLIST") port->string-list)
	  '()))
   (define whitelist
      (if (string? (getenv "WHITELIST"))
	  (call-with-input-string (getenv "WHITELIST") port->string-list)
	  '()))

   (with-access::J2SAssig this (lhs rhs)
      (if (and (isa? lhs J2SAccess) (or (isa? rhs J2SFun) (isa? rhs J2SMethod)))
	  (with-access::J2SAccess lhs (obj (metname field))
	     (if (and (isa? obj J2SAccess) (isa? metname J2SString))
		 (with-access::J2SAccess obj (obj field)
		    (if (and (isa? obj J2SRef) (isa? field J2SString))
			(with-access::J2SString field (val)
			   (if (string=? val "prototype")
			       (with-access::J2SString metname (val)
				  (cond
				     ((member val blacklist)
				      (tprint "black " val)
				      '())
				     ((or (null? whitelist) (member val whitelist))
				      (list (cons val (method-of rhs))))
				     (else
				      (tprint "not white " val)
				      '())))
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
;*    inline-method!* ...                                              */
;*---------------------------------------------------------------------*/
(define (inline-method!* lst pmethods prgm args)
   (map (lambda (o) (inline-method! o '() pmethods prgm args)) lst))
		       
;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SNode stack::pair-nil
		       pmethods prgm args)
   (call-default-walker)
   this)

;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SDeclFun ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SDeclFun stack::pair-nil
		       pmethods prgm args)
   (with-access::J2SDeclFun this (val id)
      (with-access::J2SFun val (body)
	 (tprint "id=" id " sz=" (node-size val)))
      (inline-method! val (cons this stack) pmethods prgm args)
      this))

;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SFun ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SFun stack::pair-nil
		       pmethods prgm args)
   
   (define (highscore c1 c2)
      (let ((l1 (length (callinfo-callees c1)))
	    (l2 (length (callinfo-callees c2))))
	 (cond
	    ((<fx l1 l2) #t)
	    ((>=fx (callinfo-loopctx c1) (callinfo-loopctx c2)) #t)
	    ((<=fx (callinfo-ifctx c1) (callinfo-ifctx c2)) #t)
	    (else #f))))
   
   (define (try-inline inlinables isize)
      (let loop ((inlinables (sort highscore inlinables))
		 (ninlinables '()))
	 (if (null? inlinables)
	     (values #f #f 0 ninlinables #f)
	     (let* ((inl (car inlinables))
		    (call (callinfo-call inl))
		    (callees (callinfo-callees inl))
		    (stmt (inline-call call callees prgm args))
		    (ssize (node-size stmt)))
		(tprint "isize=" isize " ssize=" ssize)
		(if (<=fx ssize isize)
		    (values call stmt ssize
		       (append ninlinables (cdr inlinables)) inl)
		    (loop (cdr inlinables)
		       (cons (car inlinables) ninlinables)))))))
   
   (with-access::J2SFun this (optimize body %info)
      (when optimize
	 (let loop ((isize (* (get-method-size! this)
			      (config-get args :inline-factor 1)))
		    (inlinables (collect-inlinables* body pmethods 0 0)))
	    (multiple-value-bind (call stmt stmtsz inlinables ci)
	       (try-inline inlinables isize)
	       (when stmt
		  (tprint "try call=" (j2s->list call))
		  (tprint "    stmt=" (j2s->list stmt))
		  (set! body (inline-node! body call stmt))
		  (let ((nisize (-fx isize (-fx stmtsz (node-size call))))
			(ninlinables (filter (lambda (ci)
						(not (eq? (callinfo-call ci) call)))
					(collect-inlinables* stmt pmethods
					   (callinfo-ifctx ci)
					   (callinfo-loopctx ci)))))
		     (loop nisize
			(append ninlinables inlinables)))))))
      this))

;*---------------------------------------------------------------------*/
;*    inline-call ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-call::J2SStmt this::J2SCall callees::pair prgm::J2SProgram args)
   
   (define (ronly-variable? obj)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (with-access::J2SDecl decl (ronly)
	       ronly))))
   
   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))
   
   (define (cache-check loc obj field inline::J2SStmt)
      (let ((c (get-cache prgm)))
	 (with-access::J2SCall this (cache)
	    (set! cache c)
	    (J2SIf (J2SCacheCheck 'proto-method c obj field)
	       inline
	       (J2SReturn #f this)))))
   
   (define (loc->string loc)
      (match-case loc
	 ((at ?file ?pos) (format "[~a:~a]" file pos))
	 (else (format "[~s]" loc))))
   
   (define (inline-callee obj field callee)
      (with-access::J2SFun callee (body thisp params)
	 (with-access::J2SDecl thisp (loc usage id)
	    (cond
	       ((not (isa? obj J2SRef))
		(let ((d (J2SLetOptUtype 'object usage id obj))
		      (t (map (lambda (p a)
				 (with-access::J2SDecl p (usage id)
				    (J2SLetOpt usage id a)))
			    params args)))
		   (J2SLetRecBlock #f (list d)
		      (cache-check loc (J2SRef d) field 
			 (J2SLetRecBlock #f t
			    (j2s-alpha body
			       (cons thisp params) (cons d t)))))))
	       ((ronly-variable? obj)
		;; not need to rebind this
		(let ((t (map (lambda (p a)
				 (with-access::J2SDecl p (usage id)
				    (J2SLetOpt usage id a)))
			    params args)))
		   (with-access::J2SRef obj (decl)
		      (cache-check loc obj field 
			 (J2SLetRecBlock #f t
			    (j2s-alpha body
			       (cons thisp params) (cons decl t)))))))
	       (else
		;; create a temporary for this
		(let ((d (J2SLetOptUtype 'object usage id obj))
		      (t (map (lambda (p a)
				 (with-access::J2SDecl p (usage id)
				    (J2SLetOpt usage id a)))
			    params args)))
		   (J2SLetRecBlock #f (list d)
		      (cache-check loc (J2SRef d) field
			 (J2SLetRecBlock #f t
			    (j2s-alpha body
			       (cons thisp params) (cons d t)))))))))))
   
   (with-access::J2SCall this (fun loc args)
      (with-access::J2SAccess fun (obj field)
	 (when (>=fx (config-get args :verb 0) 2)
	    (with-access::J2SString field (val)
	       (display "\n      ." (current-error-port))
	       (display val (current-error-port))
	       (display " " (current-error-port))
	       (display (loc->string loc) (current-error-port))))
	 (inline-callee obj field (car callees)))))

;*---------------------------------------------------------------------*/
;*    collect-inlinables ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SNode pmethods ifctx loopctx)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SIf ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SIf pmethods ifctx loopctx)
   (with-access::J2SIf this (test then else)
      (append (collect-inlinables* test pmethods ifctx loopctx)
	 (collect-inlinables* then pmethods (+fx 1 ifctx) loopctx)
	 (collect-inlinables* else pmethods (+fx 1 ifctx) loopctx))))

;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SFor ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SFor pmethods ifctx loopctx)
   (with-access::J2SFor this (init test incr body)
      (append (collect-inlinables* init pmethods ifctx loopctx)
	 (collect-inlinables* test pmethods ifctx (+fx 1 (loopctx)))
	 (collect-inlinables* incr pmethods ifctx (+fx 1 (loopctx)))
	 (collect-inlinables* body pmethods ifctx (+fx 1 (loopctx))))))
	 
;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SWhile ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SWhile pmethods ifctx loopctx)
   (with-access::J2SWhile this (test body)
      (append (collect-inlinables* test pmethods ifctx loopctx)
	 (collect-inlinables* body pmethods ifctx (+fx 1 (loopctx))))))

;*---------------------------------------------------------------------*/
;*    collect-inlinables* ::J2SCall ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-inlinables* this::J2SCall pmethods ifctx loopctx)
   
   (define (same-arity? fun::J2SFun arity)
      (with-access::J2SFun fun (params)
	 (=fx (length params) arity)))
   
   (define (inline-methods fun arity)
      (when (isa? fun J2SAccess)
	 (with-access::J2SAccess fun (obj field)
	    (when (isa? field J2SString)
	       (with-access::J2SString field (val)
		  (filter (lambda (m::J2SFun)
			     (same-arity? m arity))
		     (or (hashtable-get pmethods val) '())))))))
   
   (with-access::J2SCall this (fun args)
      (let ((inls (inline-methods fun (length args))))
	 (if (pair? inls)
	     (cons (callinfo this inls ifctx loopctx) (call-default-walker))
	     (call-default-walker)))))
   
;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SNode ...                                       */
;*    -------------------------------------------------------------    */
;*    Replace expression CALL with statement STMT.                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SNode call::J2SCall stmt::J2SStmt)
   (call-default-walker)
   this)

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SCall ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SCall call stmt)
   (if (eq? this call)
       stmt
       (call-default-walker)))
      
;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SAccess ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SAccess call stmt)
   (with-access::J2SAccess this (obj field)
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SStmtExpr ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SStmtExpr call stmt)
   (with-access::J2SStmtExpr this (expr loc)
      (let ((node (inline-node! expr call stmt)))
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
(define-walk-method (inline-node! this::J2SIf call stmt)
   (with-access::J2SIf this (test then else loc)
      (set! then (inline-node! then call stmt))
      (set! else (inline-node! else call stmt))
      (let ((node (inline-node! test call stmt)))
	 (cond
	    ((isa? node J2SExpr)
	     (set! test node)
	     this)
	    ((>fx (node-return-count node 2) 1)
	     (let ((t (J2SLetOpt '(ref set) (gensym 'test) (J2SBool #f))))
		(J2SLetRecBlock #f (list t)
		   (unreturn! node
		      (lambda (n::J2SReturn)
			 (with-access::J2SReturn n (expr loc)
			    (J2SAssig (J2SRef t) expr))))
		   (J2SIf (J2SRef t) then else))))
	    (else
	     (unreturn! node
		(lambda (n::J2SReturn)
		   (with-access::J2SReturn n (expr)
		      (J2SIf expr then else)))))))))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SReturn ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SReturn call stmt)
   (with-access::J2SReturn this (expr tail)
      (let ((node (inline-node! expr call stmt)))
	 (if (isa? node J2SExpr)
	     (begin
		(set! expr node)
		this)
	     (unreturn! node
		(lambda (n::J2SReturn)
		   (with-access::J2SReturn n (expr loc)
		      (J2SReturn tail expr))))))))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SAssig ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SAssig call stmt)
   ;; for now, only the rhs part is considered for inlining
   (with-access::J2SAssig this (lhs rhs loc)
      (if (isa? lhs J2SRef)
	  (let ((node (inline-node! rhs call stmt)))
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
;*    unreturn! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SNode conv)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SReturn conv)
   (conv this))

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
