;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/varpreinit.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 21 09:27:29 2017                          */
;*    Last change :  Mon Apr 23 12:27:43 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This optimization consists in "pre-initializating" variables     */
;*    declarations in order to improve the variable typing.            */
;*                                                                     */
;*    When a variable is declared with a plain J2SDecl/var             */
;*    statement and if it can be proved that the variable is never     */
;*    accessed before initialized and if the initialization type is    */
;*    known (typically an number), then the declaration that should    */
;*    normally binds the variable to undefined is replaced to a        */
;*    more type friendly declaration.                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_varpreinit

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_alpha)

   (export j2s-varpreinit-stage))

;*---------------------------------------------------------------------*/
;*    j2s-varpreinit-stage                                             */
;*---------------------------------------------------------------------*/
(define j2s-varpreinit-stage
   (instantiate::J2SStageProc
      (name "varpreinit")
      (comment "Variable pre-initialization")
      (proc j2s-varpreinit)
      (optional 2)))

;*---------------------------------------------------------------------*/
;*    j2s-varpreinit ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-varpreinit this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls loc pcache-size cnsts)
	 (for-each (lambda (n) (preinit* n '())) decls)
	 (for-each (lambda (n) (preinit* n '())) nodes)
	 (for-each (lambda (n) (patchinit! n args)) decls)
	 (for-each (lambda (n) (patchinit! n args)) nodes)))
   this)

;*---------------------------------------------------------------------*/
;*    preinit ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct preinit type)

;*---------------------------------------------------------------------*/
;*    invalidate-decl! ...                                             */
;*---------------------------------------------------------------------*/
(define (invalidate-decl! decl::J2SDecl)
   (with-access::J2SDecl decl (%info)
      (set! %info 'no-preinit)))

;*---------------------------------------------------------------------*/
;*    invalidated-decl? ...                                            */
;*---------------------------------------------------------------------*/
(define (invalidated-decl? decl::J2SDecl)
   (with-access::J2SDecl decl (%info)
      (eq? %info 'no-preinit)))

;*---------------------------------------------------------------------*/
;*    merge-env ...                                                    */
;*---------------------------------------------------------------------*/
(define (merge-env left right)
   
   (define (decl=? d1 d2)
      (with-access::J2SDecl d1 ((lkey key))
	 (with-access::J2SDecl d2 ((rkey key))
	    (=fx lkey rkey))))

   (define (decl<=? d1 d2)
      (with-access::J2SDecl d1 ((lkey key))
	 (with-access::J2SDecl d2 ((rkey key))
	    (<=fx lkey rkey))))
      
   (let loop ((l (sort decl<=? left))
	      (r (sort decl<=? right))
	      (a '()))
      (cond
	 ((null? l)
	  (when (pair? a)
	     (for-each invalidate-decl! r))
	  (reverse! a))
	 ((null? r)
	  (for-each invalidate-decl! l)
	  (reverse! a))
	 ((decl=? (car l) (car r))
	  (loop (cdr l) (cdr r) (cons (car l) a)))
	 ((decl<=? (car l) (car r))
	  (invalidate-decl! (car l))
	  (loop (cdr l) r a))
	 (else
	  (invalidate-decl! (car r))
	  (loop l (cdr r) a)))))
      
;*---------------------------------------------------------------------*/
;*    preinit* ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (preinit* this::obj env::pair-nil)
   (if (pair? this)
       (let loop ((this this)
		  (env env))
	  (if (null? this)
	      env
	      (loop (cdr this) (preinit* (car this) env))))
       env))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SNode env::pair-nil)
   (let* ((clazz (object-class this))
	  (fields (class-all-fields clazz)))
      (let loop ((i (-fx (vector-length fields) 1))
		 (env env))
	 (if (=fx i -1)
	     env
	     (let* ((f (vector-ref-ur fields i))
		    (info (class-field-info f)))
		(if (and (pair? info) (member "ast" info))
		    (let ((v ((class-field-accessor f) this)))
		       (loop (-fx i 1) (preinit* v env)))
		    (loop (-fx i 1) env)))))))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SLetBlock ...                                       */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SLetBlock env::pair-nil)
   (with-access::J2SLetBlock this (decls nodes)
      (preinit* nodes (preinit* decls env))))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SRef ...                                            */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SRef env::pair-nil)
   (with-access::J2SRef this (decl loc)
      (if (isa? decl J2SDeclInit)
	  env
	  (if (memq decl env)
	      (with-access::J2SDecl decl (%info useinfun)
		 (if (and (preinit? %info) (not useinfun))
		     env
		     (begin
			(invalidate-decl! decl)
			(remq! decl env))))
	      (begin
		 (invalidate-decl! decl)
		 (remq! decl env))))))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SInit ...                                           */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SInit env::pair-nil)
   
   (define (init->type init::J2SExpr)
      (cond
	 ((isa? init J2SNull) 'null)
	 ((isa? init J2SUndefined) 'undefined)
	 ((isa? init J2SString) 'string)
	 ((isa? init J2SBool) 'bool)
	 ((isa? init J2SNumber) 'number)
	 (else #f)))
   
   (with-access::J2SInit this (lhs rhs)
      (let ((env (preinit* rhs env)))
	 (if (isa? lhs J2SRef)
	     (with-access::J2SRef lhs (decl)
		(if (invalidated-decl? decl)
		    env
		    (with-access::J2SDecl decl (binder %info id)
		       (if (eq? binder 'var)
			   (let ((typ (init->type rhs)))
			      (cond
				 ((not typ)
				  (invalidate-decl! decl)
				  (remq! decl env))
				 ((preinit? %info)
				  (if (eq? (preinit-type %info) typ)
				      env
				      (begin
					 (invalidate-decl! decl)
					 (remq! decl env))))
				 (else
				  (set! %info (preinit typ))
				  (cons decl env))))
			   env))))
	     (preinit* lhs env)))))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SIf ...                                             */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SIf env::pair-nil)
   (with-access::J2SIf this (test then else)
      (let* ((nenv (preinit* test env))
	     (tenv (preinit* then nenv))
	     (eenv (preinit* then nenv)))
	 (merge-env tenv eenv))))
	 
;*---------------------------------------------------------------------*/
;*    preinit* ::J2SCond ...                                           */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SCond env::pair-nil)
   (with-access::J2SCond this (test then else)
      (let* ((nenv (preinit* test env))
	     (tenv (preinit* then nenv))
	     (eenv (preinit* then nenv)))
	 (merge-env tenv eenv))))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SSwitch ...                                         */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SSwitch env::pair-nil)
   (with-access::J2SSwitch this (key cases)
      (let ((env0 (preinit* key env)))
	 (if (null? cases)
	     env0
	     (let loop ((cases cases)
			(env (preinit* (car cases) env0)))
		(if (null? cases)
		    env
		    (let ((cenv (preinit* (car cases) env0)))
		       (loop (cdr cases) (merge-env env cenv)))))))))
	    
;*---------------------------------------------------------------------*/
;*    preinit* ::J2SFor ...                                            */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SFor env::pair-nil)
   (with-access::J2SFor this (init test incr body loc)
      (let* ((ienv (preinit* init env))
	     (tenv (preinit* test ienv))
	     (benv (preinit* body tenv)))
	 (preinit* incr benv))))
	 
;*---------------------------------------------------------------------*/
;*    preinit* ::J2SForIn ...                                          */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SForIn env::pair-nil)
   (with-access::J2SForIn this (lhs obj body)
      (preinit* body (preinit* obj env))))
	 
;*---------------------------------------------------------------------*/
;*    preinit* ::J2SWhile ...                                          */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SWhile env::pair-nil)
   (with-access::J2SWhile this (test body)
      (preinit* body (preinit* test env))))
	 
;*---------------------------------------------------------------------*/
;*    preinit* ::J2SDo ...                                             */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SDo env::pair-nil)
   (with-access::J2SDo this (test body)
      (preinit* test (preinit* body env))))
	 
;*---------------------------------------------------------------------*/
;*    patchinit! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (patchinit! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    patchinit! ::J2SLetBlock ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (patchinit! this::J2SLetBlock args)
   
   (define (type->val loc ty)
      (case ty
	 ((null) (J2SNull))
	 ((undefined) (J2SUndefined))
	 ((string) (J2SString "_"))
	 ((bool) (J2SBool #t))
	 ((number) (J2SNumber 2))
	 (else (error "type->val" "wrong type" ty))))

   (with-access::J2SLetBlock this (decls nodes)
      (let ((olds '())
	    (news '()))
	 (set! decls
	    (map! (lambda (o)
		     (with-access::J2SDecl o (%info loc id)
			(cond
			   ((isa? o J2SDeclInit)
			    (with-access::J2SDeclInit o (val)
			       (set! val (patchinit! val args))
			       o))
			   ((preinit? %info)
			    (when (>=fx (config-get args :verbose 0) 3)
			       (fprintf (current-error-port)
				  " [~a/~a:~a]" id (preinit-type %info)
				  (match-case loc
				     ((at ?file ?pos) pos)
				     (else ""))))
			    (let ((n (duplicate::J2SDeclInit o
					(binder 'let-opt)
					(val (type->val loc
						(preinit-type %info))))))
			       (set! olds (cons o olds))
			       (set! news (cons n news))
			       n))
			   (else
			    o))))
	       decls))
	 (if (pair? olds)
	     (set! nodes
		(map! (lambda (n) (j2s-alpha (patchinit! n args) olds news))
		   nodes))
	     (set! nodes
		(map! (lambda (n) (patchinit! n args))
		   nodes)))
	 this)))
