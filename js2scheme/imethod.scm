;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/imethod.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Thu Sep 28 09:10:31 2017 (serrano)                */
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
      (optional #t)
      (proc j2s-imethod!)))

;*---------------------------------------------------------------------*/
;*    j2s-imethod! ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-imethod! this args)
   (if (and (isa? this J2SProgram) (config-get args :optim-imethod #f))
       (with-access::J2SProgram this (decls nodes)
	  (let ((pms (ptable (append-map collect-proto-methods* nodes))))
	     (inline-method!* decls pms '() 1.0 this)
	     (inline-method!* nodes pms '() 1.0 this)
	     this))
       this))

;*---------------------------------------------------------------------*/
;*    size ...                                                         */
;*---------------------------------------------------------------------*/
(define-struct size value)

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
   
   (with-access::J2SAssig this (lhs rhs)
      (if (and (isa? lhs J2SAccess) (or (isa? rhs J2SFun) (isa? rhs J2SMethod)))
	  (with-access::J2SAccess lhs (obj (metname field))
	     (if (and (isa? obj J2SAccess) (isa? metname J2SString))
		 (with-access::J2SAccess obj (obj field)
		    (if (and (isa? obj J2SRef) (isa? field J2SString))
			(with-access::J2SString field (val)
			   (if (string=? val "prototype")
			       (with-access::J2SString metname (val)
				  (list (cons val (method-of rhs))))
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
(define (inline-method!* lst pmethods stack kfactor prgm)
   (map (lambda (o) (inline-method! o pmethods stack kfactor prgm)) lst))
		       
;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SNode pmethods stack kfactor prgm)
   (call-default-walker)
   this)

;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SFun ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SFun pmethods stack kfactor prgm)
   (with-access::J2SFun this (optimize body)
      (when optimize
	 (set! body (inline-method! body pmethods stack kfactor prgm)))
      this))

;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SDeclFun ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SDeclFun pmethods stack kfactor prgm)
   (with-access::J2SDeclFun this (val id)
      (set! val (inline-method! val pmethods stack kfactor prgm))
      this))

;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SStmtExpr ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SStmtExpr pmethods stack kfactor prgm)
   (with-access::J2SStmtExpr this (expr)
      (let ((node (inline-method! expr pmethods stack kfactor prgm)))
	 (if (isa? node J2SExpr)
	     (begin
		(set! expr node)
		this)
	     (unreturn! node
		(lambda (n::J2SReturn)
		   (with-access::J2SReturn n (expr loc)
		      (J2SStmtExpr expr))))))))

;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SReturn ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SReturn pmethods stack kfactor prgm)
   (with-access::J2SReturn this (expr tail)
      (let ((node (inline-method! expr pmethods stack kfactor prgm)))
	 (if (isa? node J2SExpr)
	     (begin
		(set! expr node)
		this)
	     (unreturn! node
		(lambda (n::J2SReturn)
		   (with-access::J2SReturn n (expr loc)
		      (J2SReturn tail expr))))))))

;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SAssig ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SAssig pmethods stack kfactor prgm)
   ;; for now, only the rhs part is considered for inlining
   (with-access::J2SAssig this (lhs rhs loc)
      (if (isa? lhs J2SRef)
	  (let ((node (inline-method! rhs pmethods stack kfactor prgm)))
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
;*    inline-method! ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SCall pmethods stack kfactor prgm)
   ;; walk thru the call arguments
   (call-default-walker)
   ;; consider whether this call should be inlined
   (let ((inls (find-inline-candidates this pmethods stack kfactor)))
      (if (or (not (pair? inls)) (pair? (cdr inls)))
	  ;; only inline if there is one candidate, might be improved 
	  this
	  (let ((inl (car inls)))
	     ;; let's inline that call with that method
	     (let ((stmt (inline-call this inl prgm)))
		;; this return a stmt, not an expr
		(inline-method! stmt pmethods (cons inl stack) kfactor prgm))))))
      
;*---------------------------------------------------------------------*/
;*    find-inline-candidates ...                                       */
;*---------------------------------------------------------------------*/
(define (find-inline-candidates::pair-nil this::J2SCall pmethods stack kfactor)
   
   (define (same-arity? fun::J2SFun arity)
      (with-access::J2SFun fun (params)
	 (=fx (length params) arity)))
   
   (define (inline-methods fun)
      (when (isa? fun J2SAccess)
	 (with-access::J2SAccess fun (obj field)
	    (when (isa? field J2SString)
	       (with-access::J2SString field (val)
		  (hashtable-get pmethods val))))))
   
   (define (small-enough? met::J2SFun stack kfactor)
      (with-access::J2SFun met (%info params)
	 (let ((sz (if (size? %info)
		       (size-value %info)
		       (let ((sz (method-size met)))
			  (set! %info (size sz))
			  sz))))
	    (< sz (* kfactor (+fx 8 (+fx 1 (length params))))))))
   
   (define (method-size obj)
      (if (isa? obj J2SFun)
	  (with-access::J2SFun obj (body)
	     (node-size body))
	  (with-access::J2SMethod obj (method)
	     (with-access::J2SFun method (body)
		(node-size body)))))
   
   (with-access::J2SCall this (fun args loc)
      (filter (lambda (m)
		 (unless (memq m stack)
		    ;; for now we consider methods inlining only where this
		    ;; is a single method candidate
		    (when (same-arity? m (length args))
		       ;; arities match
		       (small-enough? m stack kfactor))))
	 (or (inline-methods fun) '()))))

;*---------------------------------------------------------------------*/
;*    inline-call ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-call::J2SStmt this::J2SCall callee::J2SFun prgm::J2SProgram)
   
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
   
   (with-access::J2SCall this (fun loc)
      (with-access::J2SAccess fun (obj field)
	 (with-access::J2SFun callee (body thisp)
	    (with-access::J2SDecl thisp (loc usage id)
	       (cond
		  ((not (isa? obj J2SRef))
		   (let ((d (J2SLetOptUtype 'object usage id obj)))
		      (cache-check loc (J2SRef d) field 
			 (J2SLetBlock (list d)
			    (j2s-alpha body (list thisp) (list d))))))
		  ((ronly-variable? obj)
		   ;; not need to rebind this
		   (with-access::J2SRef obj (decl)
		      (cache-check loc obj field 
			 (j2s-alpha body (list thisp) (list decl)))))
		  (else
		   ;; create a temporary for this
		   (let ((d (J2SLetOptUtype 'object usage id obj)))
		      (cache-check loc (J2SRef d) field
			 (J2SLetBlock (list d)
			    (j2s-alpha body (list thisp) (list d))))))))))))

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
