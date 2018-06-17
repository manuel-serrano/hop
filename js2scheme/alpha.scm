;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/alpha.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 20 14:34:39 2016                          */
;*    Last change :  Sun Jun 17 12:37:41 2018 (serrano)                */
;*    Copyright   :  2016-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    AST Alpha conversion                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_alpha

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils)

   (static (class AlphaInfo
	      %oinfo
	      new)
	   (class TargetInfo
	      new))

   (export (j2s-alpha::J2SNode node::J2SNode ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    j2s-alpha ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-alpha node olds news)
   (for-each (lambda (old new)
		(with-access::J2SDecl old (%info)
		   (set! %info
		      (instantiate::AlphaInfo
			 (new new)
			 (%oinfo %info)))))
      olds news)
   (let ((newbody (alpha node)))
      (for-each (lambda (old)
		   (with-access::J2SDecl old (%info)
		      (with-access::AlphaInfo %info (%oinfo)
			 (set! %info %oinfo))))
	 olds)
      newbody))

;*---------------------------------------------------------------------*/
;*    j2s->list ::AlphaInfo ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s->list o::AlphaInfo)
   (with-access::AlphaInfo o (new)
      (if (isa? new J2SDecl)
	  (with-access::J2SDecl new (id)
	     (format "<AlphaInfo ~a>" id))
	  (format "<AlphaInfo ~a>" (typeof new)))))

;*---------------------------------------------------------------------*/
;*    alpha ::obj ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (alpha this::obj)
   (if (pair? this)
       (map alpha this)
       this))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SNode ...                                              */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SNode)
   (let* ((clazz (object-class this))
	  (ctor (class-constructor clazz))
	  (inst ((class-allocator clazz)))
	  (fields (class-all-fields clazz)))
      ;; instance fields
      (let loop ((i (-fx (vector-length fields) 1)))
	 (when (>=fx i 0)
	    (let* ((f (vector-ref-ur fields i))
		   (v ((class-field-accessor f) this))
		   (fi (class-field-info f))
		   (nv (cond
			  ((and (pair? fi) (member "notraverse" fi)) v)
			  ((pair? v) (map alpha v))
			  (else (alpha v)))))
	       ((class-field-mutator f) inst nv)
	       (loop (-fx i 1)))))
      ;; constructor
      (when (procedure? ctor) ctor inst)
      inst))

;*---------------------------------------------------------------------*/
;*    alpha/targetinfo ...                                             */
;*---------------------------------------------------------------------*/
(define (alpha/targetinfo this::J2SNode)
   (with-access::J2SNode this (%info)
      (let* ((clazz (object-class this))
	     (ctor (class-constructor clazz))
	     (inst ((class-allocator clazz)))
	     (fields (class-all-fields clazz))
	     (oinfo %info))
	 (set! %info (instantiate::TargetInfo (new inst)))
	 ;; instance fields
	 (let loop ((i (-fx (vector-length fields) 1)))
	    (when (>=fx i 0)
	       (let* ((f (vector-ref-ur fields i))
		      (v ((class-field-accessor f) this))
		      (fi (class-field-info f))
		      (nv (if (and (pair? fi) (member "notraverse" fi))
			      v
			      (alpha v))))
		  ((class-field-mutator f) inst nv)
		  (loop (-fx i 1)))))
	 ;; constructor
	 (when (procedure? ctor) ctor inst)
	 (set! %info oinfo)
	 inst)))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SDecl ...                                              */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SDecl)
   (let* ((clazz (object-class this))
	  (ctor (class-constructor clazz))
	  (inst ((class-allocator clazz)))
	  (fields (class-all-fields clazz)))
      (with-access::J2SDecl this (%info)
	 (set! %info
	    (instantiate::AlphaInfo
	       (%oinfo %info)
	       (new inst))))
      ;; instance fields
      (let loop ((i (-fx (vector-length fields) 1)))
	 (when (>=fx i 0)
	    (let* ((f (vector-ref-ur fields i))
		   (v ((class-field-accessor f) this))
		   (fi (class-field-info f))
		   (nv (cond
			  ((and (pair? fi) (member "notraverse" fi)) v)
			  ((pair? v) (map alpha v))
			  (else (alpha v)))))
	       ((class-field-mutator f) inst nv)
	       (loop (-fx i 1)))))
      ;; constructor
      (when (procedure? ctor) ctor inst)
      inst))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SLoop ...                                              */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SLoop)
   (alpha/targetinfo this))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SSwitch ...                                            */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SSwitch)
   (alpha/targetinfo this))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SBindExit ...                                          */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SBindExit)
   (let ((new (duplicate::J2SBindExit this)))
      (with-access::J2SBindExit this (%info)
	 (set! %info
	    (instantiate::AlphaInfo
	       (new new)
	       (%oinfo %info)))
	 (with-access::J2SBindExit new (stmt)
	    (set! stmt (alpha stmt))
	    (with-access::AlphaInfo %info (%oinfo)
	       (set! %info %oinfo))
	    new))))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SReturn ...                                            */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SReturn)
   (with-access::J2SReturn this (expr from)
      (if (isa? from J2SExpr)
	  (with-access::J2SExpr from (%info)
	     (if (isa? %info AlphaInfo)
		 (with-access::AlphaInfo %info (new)
		    (duplicate::J2SReturn this
		       (expr (alpha expr))
		       (from new)))
		 (duplicate::J2SReturn this
		    (expr (alpha expr)))))
	  (duplicate::J2SReturn this
	     (expr (alpha expr))))))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SBreak ...                                             */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SBreak)
   (with-access::J2SBreak this (target)
      (with-access::J2SStmt target (%info)
	 (duplicate::J2SBreak this
	    (target (if (isa? %info TargetInfo)
			(with-access::TargetInfo %info (new)
			   new)
			target))))))
      
;*---------------------------------------------------------------------*/
;*    alpha ::J2SContinue ...                                          */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SContinue)
   (with-access::J2SContinue this (target)
      (with-access::J2SStmt target (%info)
	 (duplicate::J2SContinue this
	    (target (if (isa? %info TargetInfo)
			(with-access::TargetInfo %info (new)
			   new)
			target))))))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SRef ...                                               */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info)
	 (if (isa? %info AlphaInfo)
	     (with-access::AlphaInfo %info (new)
		(cond
		   ((isa? new J2SDecl)
		    (duplicate::J2SRef this
		       (decl new)))
		   ((isa? new J2SExpr)
		    (alpha new))
		   (else
		    (error "alpha" "new must be a decl or an expr" new))))
	     (duplicate::J2SRef this)))))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SThis ...                                              */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SThis)
   (with-access::J2SThis this (decl)
      (with-access::J2SDecl decl (%info)
	 (if (isa? %info AlphaInfo)
	     (with-access::AlphaInfo %info (new)
		(duplicate::J2SThis this
		   (decl new)))
	     (duplicate::J2SThis this)))))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SFun ...                                               */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SFun)
   (with-access::J2SFun this (params body)
      (let ((nparams (map j2sdecl-duplicate params)))
	 (duplicate::J2SFun this
	    (params nparams)
	    (body (j2s-alpha body params nparams))))))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SLetBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SLetBlock)
   (with-access::J2SLetBlock this (decls nodes)
      (let ((ndecls (map j2sdecl-duplicate decls)))
	 (for-each (lambda (d)
		      (when (isa? d J2SDeclInit)
			 (with-access::J2SDeclInit d (val)
			    (set! val (j2s-alpha val decls ndecls)))))
	    ndecls)
	 (duplicate::J2SLetBlock this
	    (decls ndecls)
	    (nodes (map (lambda (n) (j2s-alpha n decls ndecls)) nodes))))))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SSvc ...                                               */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SSvc)
   (with-access::J2SSvc this (params body init)
      (let ((nparams (map j2sdecl-duplicate params)))
	 (set! init (alpha init))
	 (duplicate::J2SSvc this
	    (params nparams)
	    (body (j2s-alpha body params nparams))))))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SArrow ...                                             */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SArrow)
   (with-access::J2SArrow this (params body)
      (let ((nparams (map j2sdecl-duplicate params)))
	 (duplicate::J2SArrow this
	    (params nparams)
	    (body (j2s-alpha body params nparams))))))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SKont ...                                              */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SKont)
   (with-access::J2SKont this (param exn body)
      (let ((nparam (j2sdecl-duplicate param))
	    (nexn (j2sdecl-duplicate exn)))
	 (duplicate::J2SKont this
	    (param nparam)
	    (exn nexn)
	    (body (j2s-alpha body (list param exn) (list nparam nexn)))))))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SDConsumer ...                                         */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SDConsumer)
   (with-access::J2SDConsumer this (decl expr)
      (duplicate::J2SDConsumer this
	 (decl (j2sdecl-duplicate decl))
	 (expr (alpha expr)))))

;*---------------------------------------------------------------------*/
;*    alpha ::J2SDProducer ...                                         */
;*---------------------------------------------------------------------*/
(define-method (alpha this::J2SDProducer)
   (with-access::J2SDProducer this (decl expr)
      (duplicate::J2SDProducer this
	 (decl (j2sdecl-duplicate decl))
	 (expr (alpha expr)))))

;*---------------------------------------------------------------------*/
;*    j2sdecl-duplicate ...                                            */
;*---------------------------------------------------------------------*/
(define (j2sdecl-duplicate p::J2SDecl)
   (if (isa? p J2SDeclInit)
       (with-access::J2SDeclInit p (val)
	  (duplicate::J2SDeclInit p
	     (key (ast-decl-key))))
       (duplicate::J2SDecl p
	  (key (ast-decl-key)))))

