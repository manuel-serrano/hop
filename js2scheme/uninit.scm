;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/uninit.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 24 13:11:25 2019                          */
;*    Last change :  Fri Dec 20 19:10:37 2019 (serrano)                */
;*    Copyright   :  2019-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Mark global variables potentially used before being initialized. */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_uninit

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-uninit-stage
	   j2s-uninit-globprop-stage
	   j2s-uninit-force-stage))

;*---------------------------------------------------------------------*/
;*    j2s-uninit-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-uninit-stage
   (instantiate::J2SStageProc
      (name "uninit")
      (comment "Global variable initialization optimization")
      (proc (lambda (n args) (j2s-uninit! n args)))))

;*---------------------------------------------------------------------*/
;*    j2s-uninit-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-uninit-globprop-stage
   (instantiate::J2SStageProc
      (name "uninit.globprop")
      (comment "Global variable initialization optimization")
      (proc (lambda (n args) (j2s-uninit! n args)))
      (optional :optim-globprop)))

;*---------------------------------------------------------------------*/
;*    j2s-uninit-force-stage ...                                       */
;*---------------------------------------------------------------------*/
(define j2s-uninit-force-stage
   (instantiate::J2SStageProc
      (name "uninit.force")
      (comment "Global variable initialization optimization")
      (proc (lambda (n args) (j2s-uninit-force! n args)))))

;*---------------------------------------------------------------------*/
;*    funinfo ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct funinfo globals protos invalidated)

;*---------------------------------------------------------------------*/
;*    j2s-uninit! ::J2SProgram ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-uninit! this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (cond
	    ((not (config-get args :optim-uninit #f))
	     (j2s-uninit-force! this args))
	    (direct-eval
	     (for-each (lambda (decl)
			  (unless (isa? decl J2SDeclFun)
			     (decl-usage-add! decl 'uninit)))
		decls))
	    (else
	     ;; collect all the globals used by all global functions
	     (for-each function-collect-globals decls)
	     ;; scan in sequence all the declarations and all the nodes
	     (for-each scan-decl decls)
	     (for-each scan-node nodes)))))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-uninit-force! ::J2SProgram ...                               */
;*---------------------------------------------------------------------*/
(define (j2s-uninit-force! this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (for-each uninit-force! decls)
	 (for-each uninit-force! nodes)))
   this)

;*---------------------------------------------------------------------*/
;*    decl-global? ...                                                 */
;*---------------------------------------------------------------------*/
(define (decl-global? decl::J2SDecl)
   (unless (isa? decl J2SDeclExtern)
      (with-access::J2SDecl decl (scope)
	 (memq scope '(global %scope)))))

;*---------------------------------------------------------------------*/
;*    function-collect-globals ...                                     */
;*---------------------------------------------------------------------*/
(define (function-collect-globals decl::J2SDecl)
   (when (isa? decl J2SDeclFun)
      (with-access::J2SDeclFun decl (%info val)
	 (set! %info
	    (funinfo (collect-globals* val) '() #f)))))

;*---------------------------------------------------------------------*/
;*    collect-globals* ::J2SNode ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globals* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-globals* ::J2SRef ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globals* this::J2SRef)
   (with-access::J2SRef this (decl)
      (if (decl-global? decl)
	  (list decl)
	  '())))

;*---------------------------------------------------------------------*/
;*    scan-decl ...                                                    */
;*---------------------------------------------------------------------*/
(define (scan-decl decl::J2SDecl)
   (when (and (isa? decl J2SDeclInit) (not (isa? decl J2SDeclFun)))
      (with-access::J2SDeclInit decl (val %info binder)
	 (when (eq? binder 'let-opt)
	    (unless (decl-usage-has? decl '(uninit))
	       (with-access::J2SDecl decl (%info)
		  (set! %info 'init))))
	 (invalidate! val))))

;*---------------------------------------------------------------------*/
;*    scan-node ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (scan-node this::J2SNode)
   (let* ((clazz (object-class this))
	  (ctor (class-constructor clazz))
	  (inst ((class-allocator clazz)))
	  (fields (class-all-fields clazz)))
      (let loop ((i (-fx (vector-length fields) 1)))
	 (when (>=fx i 0)
	    (let* ((f (vector-ref-ur fields i))
		   (fi (class-field-info f))
		   (v ((class-field-accessor f) this)))
	       (cond
		  ((and (pair? fi) (member "notraverse" fi)) v)
		  ((pair? v) (map scan-node v))
		  ((isa? v J2SExpr) (invalidate! v))
		  ((isa? v J2SNode) (scan-node v)))
	       (loop (-fx i 1)))))
      this))

;*---------------------------------------------------------------------*/
;*    scan-node ::J2SStmtExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-method (scan-node this::J2SStmtExpr)

   (define (is-prototype? field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (and (string? val) (string=? val "prototype")))))
   
   (define (global-prototype-assign assig::J2SAssig)
      (with-access::J2SAssig assig (lhs rhs)
	 (if (isa? rhs J2SFun)
	     (if (isa? lhs J2SAccess)
		 (with-access::J2SAccess lhs (obj field)
		    (if (isa? obj J2SAccess)
			(with-access::J2SAccess obj (obj field)
			   (if (isa? obj J2SRef)
			       (with-access::J2SRef obj (decl)
				  (if (and (decl-global? decl)
					   (is-prototype? field))
				      (values decl rhs)
				      (values #f #f)))
			       (values #f #f)))
			(values #f #f)))
		 (values #f #f))
	     (values #f #f))))
   
   (with-access::J2SStmtExpr this (expr loc)
      (cond
	 ((isa? expr J2SInit)
	  (with-access::J2SInit expr (lhs rhs)
	     (invalidate! rhs)
	     (if (isa? lhs J2SRef)
		 (with-access::J2SRef lhs (decl)
		    (if (decl-global? decl)
			(unless (decl-usage-has? decl '(uninit))
			   (with-access::J2SDecl decl (%info)
			      (set! %info 'init)))
			(invalidate! lhs)))
		 (invalidate! lhs))))
	 ((isa? expr J2SAssig)
	  (multiple-value-bind (decl fun)
	     (global-prototype-assign expr)
	     (if (and decl fun)
		 (with-access::J2SDecl decl (%info id)
		    (if (or (not (funinfo? %info)) (funinfo-invalidated %info))
			(invalidate! fun)
			(funinfo-protos-set! %info
			   (cons fun (funinfo-protos %info)))))
		 (invalidate! expr))))
	 (else
	  (invalidate! expr)))))

;*---------------------------------------------------------------------*/
;*    invalidate! ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (invalidate! this)
   (when (pair? this)
      (for-each invalidate! this)))

;*---------------------------------------------------------------------*/
;*    invalidate! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-method (invalidate! this::J2SNode)
   (with-access::J2SNode this (%info)
      (unless (eq? %info 'invalidated)
	 (set! %info 'invalidated)
	 (let* ((clazz (object-class this))
		(ctor (class-constructor clazz))
		(inst ((class-allocator clazz)))
		(fields (class-all-fields clazz)))
	    (let loop ((i (-fx (vector-length fields) 1)))
	       (when (>=fx i 0)
		  (let* ((f (vector-ref-ur fields i))
			 (v ((class-field-accessor f) this))
			 (fi (class-field-info f)))
		     (cond
			((and (pair? fi) (member "notraverse" fi)) v)
			((pair? v) (for-each invalidate! v))
			((isa? v J2SExpr) (invalidate! v))
			((isa? v J2SNode) (invalidate! v)))
		     (loop (-fx i 1)))))))
      this))

;*---------------------------------------------------------------------*/
;*    invalidate! ::J2SRef ...                                         */
;*---------------------------------------------------------------------*/
(define-method (invalidate! this::J2SRef)
   (with-access::J2SRef this (decl loc)
      (when (decl-global? decl)
	 (with-access::J2SDecl decl (%info id scope)
	    (cond
	       ((isa? decl J2SDeclFun)
		(unless (funinfo-invalidated %info)
		   (funinfo-invalidated-set! %info #t)
		   (with-access::J2SDeclFun decl (val)
		      (invalidate! val))
		   (for-each invalidate! (funinfo-protos %info))))
	       ((not (eq? %info 'init))
		(decl-usage-add! decl 'uninit)))))))

;*---------------------------------------------------------------------*/
;*    invalidate-overridden-decl ::J2SNode ...                         */
;*    -------------------------------------------------------------    */
;*    Scan the whole program and invalidate all global variables       */
;*    that are initialized several times.                              */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-overridden-decl this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    invalidate-overridden-decl ::J2SInit ...                         */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-overridden-decl this::J2SInit)
   (with-access::J2SInit this (lhs rhs)
      (invalidate-overridden-decl rhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (%info)
		(case %info
		   ((unknown)
		    (set! %info 'init1))
		   ((init1)
		    (set! %info 'overridden)))))
	  (invalidate-overridden-decl lhs))))

;*---------------------------------------------------------------------*/
;*    invalidate-overridden-decl ::J2SDeclInit ...                     */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-overridden-decl this::J2SDeclInit)
   (with-access::J2SDeclInit this (%info val)
      (invalidate-overridden-decl val)
      (case %info
	 ((unknown)
	  (set! %info 'init0))
	 ((init0)
	  (set! %info 'overridden)))))
      
;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SNode ...                              */
;*    -------------------------------------------------------------    */
;*    Scan the whole program and invalidate all global variables       */
;*    that can possibily be accessed before initialized.               */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SNode initp::bool)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SRef ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SRef initp)
   (with-access::J2SRef this (decl loc)
      (with-access::J2SDecl decl (%info id key)
	 (when (memq %info '(unknown init0 overridden))
	    (if (isa? decl J2SDeclFun)
		(begin
		   (set! %info 'init)
		   (with-access::J2SDeclFun decl (val)
		      (invalidate-early-decl val #f)))
		(begin
		   (set! %info 'uninit)
		   (decl-usage-add! decl 'uninit)))))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SFun ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SFun initp)
   (with-access::J2SFun this (body)
      (invalidate-early-decl body #f)))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SInit ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SInit initp)
   (with-access::J2SInit this (lhs rhs %%dump)
      (invalidate-early-decl rhs initp)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (%info id)
		(when (and initp (eq? %info 'init0))
		   ;; for sure this is initialized
		   (set! %info 'init))))
	  (invalidate-early-decl lhs initp))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SDeclInit ...                          */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SDeclInit initp)
   (with-access::J2SDeclInit this (%info val)
      (invalidate-early-decl val initp)
      (when (eq? %info 'init0)
	 ;; for sure this is initialized
	 (set! %info 'init))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SDeclFun ...                           */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SDeclFun initp)
   (with-access::J2SDeclInit this (%info val)
      (when (eq? %info 'initi0)
	 ;; for sure this is initialized
	 (set! %info 'init))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SLabel ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SLabel initp)
   (with-access::J2SLabel this (body)
      (invalidate-early-decl body #f)))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2STry ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2STry initp)
   (with-access::J2STry this (body catch finally)
      (invalidate-early-decl body #f)
      (invalidate-early-decl catch #f)
      (invalidate-early-decl finally #f)))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SLoop ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SLoop initp)
   (with-access::J2SLoop this (body)
      (invalidate-early-decl body #f)))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SFor ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SFor initp)
   (with-access::J2SFor this (init test incr)
      (invalidate-early-decl init initp)
      (invalidate-early-decl test initp)
      (invalidate-early-decl incr initp)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SForIn ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SForIn initp)
   (with-access::J2SForIn this (lhs obj)
      (invalidate-early-decl lhs initp)
      (invalidate-early-decl obj initp)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    uninit-force! ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (uninit-force! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    uninit-force! ::J2SDeclInit ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (uninit-force! this::J2SDeclInit)
   (unless (isa? this J2SDeclFun)
      (decl-usage-add! this 'uninit))
   (call-default-walker))
