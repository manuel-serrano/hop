;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/varpreinit.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 21 09:27:29 2017                          */
;*    Last change :  Mon Jun 26 10:20:25 2023 (serrano)                */
;*    Copyright   :  2017-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This optimization consists in "pre-initializating" variables     */
;*    declarations in order to improve the variable typing.            */
;*                                                                     */
;*    When a variable is declared with a plain J2SDecl/var             */
;*    statement and if it can be proved that the variable is never     */
;*    accessed before initialized and if the initialization type is    */
;*    known (typically a number), then the declaration that should     */
;*    normally bind the variable to undefined is replaced with a       */
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
	   __js2scheme_alpha
	   __js2scheme_usage)

   (export j2s-varpreinit-stage))

;*---------------------------------------------------------------------*/
;*    j2s-varpreinit-stage                                             */
;*---------------------------------------------------------------------*/
(define j2s-varpreinit-stage
   (instantiate::J2SStageProc
      (name "varpreinit")
      (comment "Variable pre-initialization")
      (proc j2s-varpreinit)
      (optional :optim-varpreinit)))

;*---------------------------------------------------------------------*/
;*    j2s-varpreinit ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-varpreinit this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls loc pcache-size cnsts)
	 (preinit* decls '())
	 (preinit* nodes '())
	 (set! decls (map! (lambda (n) (patchinit-decl! n args)) decls))
	 (set! decls (map! (lambda (n) (patchinit! n args)) decls))
	 (set! nodes (map! (lambda (n) (patchinit! n args)) nodes))))
   this)

;*---------------------------------------------------------------------*/
;*    preinit ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct preinit type)
(define-struct alphainit decl)

;*---------------------------------------------------------------------*/
;*    invalidate-decl! ...                                             */
;*---------------------------------------------------------------------*/
(define (invalidate-decl! decl::J2SDecl)
   (with-access::J2SDecl decl (%info id)
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
      (cond
	 ((isa? decl J2SDeclFun)
	  (with-access::J2SDeclFun decl (%info val)
	     (unless (eq? %info 'preinit*)
		(set! %info 'preinit*)
		(preinit* val env))
	     env))
	 ((isa? decl J2SDeclInit)
	  env)
	 (else
	  (if (memq decl env)
	      (with-access::J2SDecl decl (%info escape id)
		 ;; MS care 15oct2020: since procedure are followed
		 ;; when they escape, I don't think it is needed to
		 ;; check escape here
		 ;; (if (and (preinit? %info) (not escape))
		 (if (preinit? %info)
		     env
		     (begin
			(invalidate-decl! decl)
			(remq! decl env))))
	      (begin
		 (invalidate-decl! decl)
		 (remq! decl env)))))))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SDeclFun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SDeclFun env::pair-nil)
   ;; only tranverse J2SDeclFun when they are referenced for the first time
   (with-access::J2SDeclFun this (%info val)
      (when (decl-usage-has? this '(ref))
	 (unless (eq? %info 'preinit*)
	    (set! %info 'preinit*)
	    (preinit* val env))))
   env)

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
   
   (with-access::J2SInit this (lhs rhs loc)
      (let ((env (preinit* rhs env)))
	 (if (isa? lhs J2SRef)
	     (with-access::J2SRef lhs (decl)
		(if (invalidated-decl? decl)
		    env
		    (with-access::J2SDecl decl (binder %info id escape)
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
;*    break-or-continue? ...                                           */
;*---------------------------------------------------------------------*/
(define (break-or-continue? this::J2SLoop)
   (with-access::J2SLoop this (need-bind-exit-continue need-bind-exit-break)
      (or need-bind-exit-continue need-bind-exit-break)))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SFor ...                                            */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SFor env::pair-nil)
   (with-access::J2SFor this (init test incr body)
      (let* ((ienv (preinit* init env))
	     (tenv (preinit* test ienv))
	     (benv (preinit* body tenv))
	     (renv (preinit* incr benv)))
	 (if (break-or-continue? this)
	     tenv
	     (merge-env renv tenv)))))
	 
;*---------------------------------------------------------------------*/
;*    preinit* ::J2SForIn ...                                          */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SForIn env::pair-nil)
   (with-access::J2SForIn this (lhs obj body)
      (let* ((oenv (preinit* obj env))
	     (benv (preinit* body oenv)))
	 (if (break-or-continue? this)
	     oenv
	     benv))))
	 
;*---------------------------------------------------------------------*/
;*    preinit* ::J2SWhile ...                                          */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SWhile env::pair-nil)
   (with-access::J2SWhile this (test body)
      (let* ((tenv (preinit* test env))
	     (benv (preinit* body tenv)))
	 (if (break-or-continue? this)
	     (merge-env benv tenv)
	     tenv))))
	 
;*---------------------------------------------------------------------*/
;*    preinit* ::J2SDo ...                                             */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2SDo env::pair-nil)
   (with-access::J2SDo this (test body loc)
      (let ((benv (preinit* body env)))
	 (if (break-or-continue? this)
	     env
	     (preinit* test benv)))))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2STry ...                                            */
;*---------------------------------------------------------------------*/
(define-method (preinit* this::J2STry env::pair-nil)
   (with-access::J2STry this (body catch finally loc)
      (preinit* catch env)
      (preinit* body (preinit* finally env))))

;*---------------------------------------------------------------------*/
;*    patchinit-decl! ...                                              */
;*---------------------------------------------------------------------*/
(define (patchinit-decl! this::J2SDecl args)
   (with-access::J2SDecl this (scope %info)
      (if (and (memq scope '(%scope global tls)) (preinit? %info))
	  (let ((ndecl (decl->declinit this)))
	     (set! %info (alphainit ndecl))
	     ndecl)
	  this)))
   
;*---------------------------------------------------------------------*/
;*    patchinit! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (patchinit! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    decl->declinit ...                                               */
;*---------------------------------------------------------------------*/
(define (decl->declinit o)
   
   (define (type->val loc ty)
      (case ty
	 ((null) (J2SNull))
	 ((undefined) (J2SUndefined))
	 ((string) (J2SString "_"))
	 ((bool) (J2SBool #t))
	 ((number) (J2SNumber 2))
	 (else (error "type->val" "wrong type" ty))))

   (with-access::J2SDecl o (%info loc)
      (duplicate::J2SDeclInit o
	 (key (ast-decl-key))
	 (binder 'let-opt)
	 (val (type->val loc (preinit-type %info))))))

;*---------------------------------------------------------------------*/
;*    patchinit! ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (patchinit! this::J2SDecl args)
   (with-access::J2SDecl this (scope %info)
      (if (and (memq scope '(%scope global tls)) (alphainit? %info))
	  (alphainit-decl %info)
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    patchinit! ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (patchinit! this::J2SRef args)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info id)
	 (when (alphainit? %info)
	    (set! decl (alphainit-decl %info)))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    patchinit! ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (patchinit! this::J2SDeclInit args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    patchinit! ::J2SLetBlock ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (patchinit! this::J2SLetBlock args)
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
			    (let ((n (decl->declinit o)))
			       (set! olds (cons o olds))
			       (set! news (cons n news))
			       n))
			   (else
			    o))))
	       decls))
	 (if (pair? olds)
	     (begin
		(set! nodes
		   (map! (lambda (n) (j2s-alpha (patchinit! n args) olds news))
		      nodes))
		(for-each (lambda (d)
			    (when (isa? d J2SDeclInit)
			       (with-access::J2SDeclInit d (val)
				  (set! val (j2s-alpha val olds news)))))
		   decls))
	     (set! nodes
		(map! (lambda (n) (patchinit! n args))
		   nodes)))
	 this)))

;*---------------------------------------------------------------------*/
;*    patchinit! ::J2SBlock ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (patchinit! this::J2SBlock args)
   (with-access::J2SBlock this (loc endloc nodes)
      (let ((preinits (filter-map (lambda (n)
				     (when (isa? n J2SDecl)
					(with-access::J2SDecl n (binder %info)
					   (when (and (eq? binder 'var)
						      (preinit? %info))
					      n))))
			 nodes)))
	 (if (pair? preinits)
	     (let ((news (map decl->declinit preinits)))
		(set! nodes
		   (filter (lambda (n)
			      (or (not (isa? n J2SDecl))
				  (with-access::J2SDecl n (binder %info)
				     (or (not (eq? binder 'var))
					 (not (preinit? %info))))))
		      nodes))
		(J2SLetRecBlock #f news
		   (j2s-alpha (patchinit! this args) preinits news)))
	     (call-default-walker)))))
