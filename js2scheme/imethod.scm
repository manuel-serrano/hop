;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/imethod.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Mon Sep 18 15:28:05 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Method inlining optimization                                     */
;*    -------------------------------------------------------------    */
;*    The method inlining proceeds as follows:                         */
;*      1- the AST is traversed to find all the method assigned to     */
;*         prototype objects.                                          */
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
	   __js2scheme_alpha)

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
	  (let ((pmethods (ptable (append-map prototype-methods* nodes))))
	     (inline-method!* decls pmethods '() 1.0)
	     (inline-method!* nodes pmethods '() 1.0)
	     this))
       this))

;*---------------------------------------------------------------------*/
;*    size ...                                                         */
;*---------------------------------------------------------------------*/
(define-struct size value)

;*---------------------------------------------------------------------*/
;*    prototype-methods* ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (prototype-methods* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    prototype-methods* ::J2SAssig ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (prototype-methods* this::J2SAssig)

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
;*    Store all methods into a global hashtable.                       */
;*---------------------------------------------------------------------*/
(define (ptable::struct alist)
   (let ((table (create-hashtable)))
      (for-each (lambda (e)
		   (hashtable-add! table (car e) cons (cdr e) '()))
	 alist)
      (tprint "itable=" (hashtable-map table (lambda (k e) (cons k (length e)))))
      table))

;*---------------------------------------------------------------------*/
;*    inline-method!* ...                                              */
;*---------------------------------------------------------------------*/
(define (inline-method!* lst pmethods stack kfactor)
   (for-each (lambda (o) (inline-method! o pmethods stack kfactor)) lst))
		       
;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SNode pmethods stack kfactor)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SFun ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SFun pmethods stack kfactor)
   (with-access::J2SFun this (optimize body)
      (when optimize
	 (set! body (inline-method! body pmethods stack kfactor)))
      this))

;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SDeclFun ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SDeclFun pmethods stack kfactor)
   (with-access::J2SDeclFun this (val id)
      (set! val (inline-method! val pmethods stack kfactor))
      this))

;*---------------------------------------------------------------------*/
;*    inline-method! ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-method! this::J2SCall pmethods stack kfactor)
   
   (define (same-arity? fun::J2SFun arity)
      (with-access::J2SFun fun (params vararg)
	 (=fx (length params) arity)))
   
   (define (method-size obj)
      (if (isa? obj J2SFun)
	  (with-access::J2SFun obj (body)
	     (node-size body))
	  (with-access::J2SMethod obj (method)
	     (with-access::J2SFun method (body)
		(node-size body)))))

   (define (is-small-enough? met::J2SFun stack kfactor)
      (with-access::J2SFun met (%info params)
	 (let ((sz (if (size? %info)
		       (size-value %info)
		       (let ((sz (method-size met)))
			  (set! %info (size sz))
			  sz))))
	    (< sz (* kfactor (+fx 8 (+fx 1 (length params))))))))
   
   (define (inline-methods fun)
      (when (isa? fun J2SAccess)
	 (with-access::J2SAccess fun (obj field)
	    (when (isa? field J2SString)
	       (with-access::J2SString field (val)
		  (hashtable-get pmethods val))))))

   (define (inline-call met fun obj args loc)
      (let ((nfun (j2s-alpha met '() '())))
	 (with-access::J2SFun nfun (thisp body)
	    (with-access::J2SDecl thisp (vtype itype utype ronly)
	       (set! vtype 'object)
	       (set! utype 'object)
	       (set! itype 'object)
	       (when ronly (this-type nfun)))
	    (set! body
	       (inline-method! body pmethods (cons met stack)
		  (* 0.8 kfactor)))
	    (J2SMethodCall* nfun obj args))))

   (with-access::J2SCall this (fun args loc)
      (let ((imets (inline-methods fun)))
	 (if (pair? imets)
	     (let* ((arity (length args))
		    (mets (filter (lambda (p)
				     (when (same-arity? p arity)
					(unless (memq p stack)
					   (is-small-enough? p stack kfactor))))
			     imets)))
		(if (and (pair? mets) (null? (cdr imets)))
		    (with-access::J2SAccess fun (obj)
		       (tprint "+++ YES INLINE: " (j2s->list this)
			  " loc=" loc
			  " arity=" arity " size=" (method-size (car mets))
			  " kfactor=" kfactor)
		       (tprint (j2s->list (car mets)))
		       (inline-call (car mets) fun obj args loc))
		    (begin
		       (tprint "--- NO INLINE.1: "
			  (length mets) " " (j2s->list this)
			  " arity=" arity
			  " sizes=" (map node-size imets))
		       this)))
	     (begin
		(when (isa? fun J2SAccess)
		   (tprint "--- NO INLINE.2: " (j2s->list this))) 
		this)))))

;*---------------------------------------------------------------------*/
;*    this-type ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (this-type this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    this-type ::J2SThis ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (this-type this::J2SThis)
   (with-access::J2SThis this (type)
      (set! type 'object)))
   
;*---------------------------------------------------------------------*/
;*    node-size ::obj ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (node-size::long this::obj)
   (cond
      ((null? this) 0)
      ((pair? this) (apply + (map node-size this)))
      (else 1)))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SNode)
   (let ((fields (class-all-fields (object-class this))))
      (let loop ((i (-fx (vector-length fields) 1))
		 (s 0))
	 (if (=fx i -1)
	     s
	     (let* ((f (vector-ref fields i))
		    (info (class-field-info f)))
		(if (and (pair? info) (member "ast" info))
		    (loop (-fx i 1)
		       (+fx (node-size ((class-field-accessor f) this)) s))
		    (loop (-fx i 1)
		       s)))))))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SSeq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SSeq)
   (with-access::J2SSeq this (nodes)
      (apply + (map node-size nodes))))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SStmt ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SStmt)
   (+fx 1 (call-next-method)))

;*---------------------------------------------------------------------*/
;*    node-size ::J2Seq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SLetBlock)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SIf ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SIf)
   (+fx 1 (call-next-method)))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SStmtExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SStmtExpr)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SNop ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SNop)
   0)

;*---------------------------------------------------------------------*/
;*    node-size ::J2SExpr ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SExpr)
   (+fx 1 (call-next-method)))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SExprStmt ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SExprStmt)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SFun)
   (with-access::J2SFun this (params)
      (+ (call-next-method) 1 (length params))))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SMethod ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SMethod)
   (with-access::J2SMethod this (method)
      (node-size method)))


