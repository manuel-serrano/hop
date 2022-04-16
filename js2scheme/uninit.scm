;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/uninit.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 24 13:11:25 2019                          */
;*    Last change :  Thu Apr 14 08:20:13 2022 (serrano)                */
;*    Copyright   :  2019-22 Manuel Serrano                            */
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
	   __js2scheme_alpha
	   __js2scheme_classutils)

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
			  (unless (or (isa? decl J2SDeclFun)
				      (isa? decl J2SDeclExtern)
				      (j2s-decl-class? decl))
			     (decl-usage-add! decl 'uninit)))
		decls))
	    (else
	     (uninit-nodes* decls '())
	     (uninit-nodes* nodes '())))))
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
;*    intersection ...                                                 */
;*---------------------------------------------------------------------*/
(define (intersection el er)
   (for-each (lambda (d) (with-access::J2SNode d (%info) (set! %info #f))) el)
   (for-each (lambda (d) (with-access::J2SNode d (%info) (set! %info #t))) er)
   (filter (lambda (d) (with-access::J2SNode d (%info) %info)) el))

;*---------------------------------------------------------------------*/
;*    uninit-nodes* ...                                                */
;*---------------------------------------------------------------------*/
(define (uninit-nodes* nodes env)
   (let loop ((nodes nodes)
	      (env env))
      (if (null? nodes)
	  env
	  (loop (cdr nodes) (uninit* (car nodes) env)))))

;*---------------------------------------------------------------------*/
;*    uninit-par* ...                                                  */
;*---------------------------------------------------------------------*/
(define (uninit-par* nodes env0)
   (let loop ((nodes nodes)
	      (env env0))
      (if (null? nodes)
	  env
	  (let ((nenv (uninit* (car nodes) env0)))
	     (loop (cdr nodes) (intersection nenv env))))))

;*---------------------------------------------------------------------*/
;*    uninit* ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (uninit* this env)
   (if (pair? this)
       (uninit-par* this env)
       env))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SNode env)
   (let* ((clazz (object-class this))
	  (ctor (class-constructor clazz))
	  (inst ((class-allocator clazz)))
	  (fields (class-all-fields clazz)))
      (let loop ((i (-fx (vector-length fields) 1))
		 (env env))
	 (if (>=fx i 0)
	     (let* ((f (vector-ref-ur fields i))
		    (fi (class-field-info f))
		    (v ((class-field-accessor f) this))
		    (env (if (and (pair? fi) (member "ast" fi))
			     (uninit* v env)
			     env)))
		(loop (-fx i 1) env))
	     env))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SDecl ...                                            */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SDecl env)
   (cons this env))
   
;*---------------------------------------------------------------------*/
;*    uninit* ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SDeclInit env)
   (with-access::J2SDeclInit this (val)
      (uninit* val env)
      (decl-usage-add! this 'init)
      (cons this env)))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SRef ...                                             */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SRef env)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (scope)
	 (when (memq scope '(global %scope tls))
	    (unless (or (isa? decl J2SDeclInit)
			(isa? decl J2SDeclExtern)
			(j2s-decl-class? decl))
	       (unless (or (decl-usage-has? decl '(uninit)) (memq decl env))
		  (decl-usage-add! decl 'uninit))))))
   env)

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SAccess ...                                          */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SAccess env)
   (with-access::J2SAccess this (obj field)
      (let ((env (uninit* obj env)))
	 (let ((el (class-private-element-access this)))
	    (if (isa? el J2SClassElement)
		(if (memq el env)
		    env
		    (with-access::J2SClassElement el (usage)
		       (set! usage (usage-add usage 'uninit))
		       env))
		(uninit* field env))))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SInit ...                                            */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SInit env)
   (with-access::J2SInit this (lhs rhs)
      (let ((env (uninit* rhs env)))
	 (if (isa? lhs J2SRef)
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (scope)
		   (decl-usage-add! decl 'init)
		   (if (memq scope '(global %scope tls))
		       (if (decl-usage-has? decl '(uninit))
			   env
			   (cons decl env))
		       env)))
	     (uninit* lhs env)))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SAssig ...                                           */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SAssig env)
   (with-access::J2SAssig this (lhs rhs)
      (let ((env (uninit* rhs env)))
	 (cond
	    ((isa? lhs J2SAccess)
	     (let ((el (class-private-element-access lhs)))
		(if (isa? el J2SClassElement)
		    (if (memq el env)
			env
			(cons el env))
		    (uninit* lhs env))))
	    ((not (isa? lhs J2SRef))
	     (uninit* lhs env))
	    (else
	     env)))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SSeq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SSeq env)
   (with-access::J2SSeq this (nodes)
      (uninit-nodes* nodes env)))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SIf ...                                              */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SIf env)
   (with-access::J2SIf this (test then else)
      (let* ((testenv (uninit* test env))
	     (thenenv (uninit* then testenv))
	     (elseenv (uninit* else testenv)))
	 (intersection thenenv elseenv))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SLetBlock ...                                        */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SLetBlock env)
   (with-access::J2SLetBlock this (decls nodes)
      (let loop ((decls decls)
		 (env env))
	 (if (null? decls)
	     (uninit-nodes* nodes env)
	     (loop (cdr decls) (uninit* (car decls) env))))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SSwitch ...                                          */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SSwitch env)
   (with-access::J2SSwitch this (key cases)
      (let ((kenv (uninit* key env)))
	 (uninit-par* cases kenv))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SCase ...                                            */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SCase env)
   (with-access::J2SCase this (expr body)
      (let ((eenv (uninit* expr env)))
	 (uninit* body eenv))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SFor ...                                             */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SFor env)
   (with-access::J2SFor this (init test incr body)
      (let* ((ienv (uninit* init env))
	     (tenv (uninit* test ienv))
	     (benv (uninit* body ienv)))
	 (uninit* incr benv)
	 (filter (lambda (d)
		    (or (not (isa? d J2SDecl))
			(not (decl-usage-has? d '(uninit)))))
	    ienv))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SForIn ...                                           */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SForIn env)
   (with-access::J2SForIn this (lhs obj body)
      (let* ((lenv (uninit* lhs env))
	     (oenv (uninit* obj lenv)))
	 (uninit* body oenv)
	 (filter (lambda (d)
		    (or (not (isa? d J2SDecl))
			(not (decl-usage-has? d '(uninit)))))
	    oenv))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SWhile ...                                           */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SWhile env)
   (with-access::J2SWhile this (test body)
      (let* ((tenv (uninit* test env)))
	 (uninit* body tenv)
	 (filter (lambda (d)
		    (or (not (isa? d J2SDecl))
			(not (decl-usage-has? d '(uninit)))))
	    tenv))))
      
;*---------------------------------------------------------------------*/
;*    uninit* ::J2SDo ...                                              */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SDo env)
   (with-access::J2SDo this (test body need-bind-exit-break need-bind-exit-continue)
      (let ((benv (uninit* body env)))
	 (uninit* test 
	    (filter (lambda (d)
		       (or (not (isa? d J2SDecl))
			   (not (decl-usage-has? d '(uninit)))))
	       (if (or need-bind-exit-break need-bind-exit-continue)
		   env
		   benv))))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SClass ...                                           */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SClass env)
   (with-access::J2SClass this (elements)
      (let* ((ctor (j2s-class-get-constructor this))
	     (env (filter (lambda (el)
			     (with-access::J2SClassElement el (prop)
				(isa? prop J2SDataPropertyInit)))
		     elements))
	     (env0 (if ctor (uninit* ctor env) env)))
	 (let loop ((els elements)
		    (env env0))
	    (cond
	       ((null? els)
		;; mark all the initialized class elements
		(for-each (lambda (e)
			     (when (isa? e J2SClassElement)
				(with-access::J2SClassElement e (usage)
				   (set! usage (usage-add usage 'init)))))
		   env)
		;; mark all the non-initialized class elements
		(for-each (lambda (el)
			     (with-access::J2SClassElement el (usage)
				(unless (usage-has? usage '(init))
				   (set! usage (usage-add usage 'uninit)))))
		   elements)
		env)
	       ((eq? (car els) ctor)
		(loop (cdr els) env))
	       (else
		(let ((nenv (uninit* (car els) env)))
		   (loop (cdr els) (intersection nenv env)))))))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SClassElement ...                                    */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SClassElement env)
   (with-access::J2SClassElement this (prop usage)
      (with-access::J2SPropertyInit prop (name)
	 (unless (and (isa? name J2SString)
		      (with-access::J2SString name (private) private))
	    (set! usage (usage-add* usage '(uninit get set delete))))
	 (uninit* prop env))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SDataPropertyInit ...                                */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SDataPropertyInit env)
   (with-access::J2SDataPropertyInit this (val)
      (uninit* val env)))
   
;*---------------------------------------------------------------------*/
;*    uninit* ::J2SAccessorPropertyInit ...                            */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SAccessorPropertyInit env)
   (with-access::J2SAccessorPropertyInit this (get set)
      (let ((env (if get (uninit* get env) env)))
	 (if set (uninit* set env) env))))

;*---------------------------------------------------------------------*/
;*    decl-global? ...                                                 */
;*---------------------------------------------------------------------*/
(define (decl-global? decl::J2SDecl)
   (unless (isa? decl J2SDeclExtern)
      (with-access::J2SDecl decl (scope)
	 (memq scope '(global %scope tls)))))

;*---------------------------------------------------------------------*/
;*    uninit-force! ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (uninit-force! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    uninit-force! ::J2SDeclInit ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (uninit-force! this::J2SDeclInit)
   (unless (or (isa? this J2SDeclFun) (j2s-decl-class? this))
      (decl-usage-add! this 'uninit))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    uninit-force! ::J2SClass ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (uninit-force! this::J2SClass)
   (with-access::J2SClass this (elements)
      (for-each (lambda (el)
		   (with-access::J2SClassElement el (usage)
		      (set! usage (usage-add usage 'uninit))))
	 elements))
   (call-default-walker))
   
