;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/varpreinit.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 21 09:27:29 2017                          */
;*    Last change :  Thu Dec 21 17:07:06 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    This optimization consists in "pre-initializating" variables     */
;*    declarations in order to improve the variable typing.            */
;*                                                                     */
;*    When a variable is declared with a plain J2SDec/var              */
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
	     (for-each (lambda (d) (invalidate-decl! (car d))) r))
	  (reverse! a))
	 ((null? r)
	  (for-each (lambda (d) (invalidate-decl! (car d))) l)
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
(define-walk-method (preinit* this::J2SNode env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SRef ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (preinit* this::J2SRef env)
   (with-access::J2SRef this (decl)
      (unless (memq decl env)
	 (invalidate-decl! decl)))
   env)

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SInit ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (preinit* this::J2SInit env)
   
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
			      (if typ
				  (match-case %info
				     ((preinit ?t)
				      (unless (eq? t typ)
					 (invalidate-decl! decl)
					 (remq! decl env)))
				     (else
				      (set! %info (cons 'preinit typ))
				      (cons decl env)))
				  (begin
				     (invalidate-decl! decl)
				     (remq! decl env))))
			   env))))
	     (preinit* lhs env)))))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SIf ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (preinit* this::J2SIf env)
   (with-access::J2SIf this (test then else)
      (let* ((nenv (preinit* test env))
	     (tenv (preinit* then nenv))
	     (eenv (preinit* then nenv)))
	 (merge-env tenv eenv))))
	 
;*---------------------------------------------------------------------*/
;*    preinit* ::J2SCond ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (preinit* this::J2SCond env)
   (with-access::J2SCond this (test then else)
      (let* ((nenv (preinit* test env))
	     (tenv (preinit* then nenv))
	     (eenv (preinit* then nenv)))
	 (merge-env tenv eenv))))

;*---------------------------------------------------------------------*/
;*    preinit* ::J2SSeq ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (preinit* this::J2SSeq env)
   (with-access::J2SSeq this (nodes)
      (let loop ((nodes nodes)
		 (env env))
	 (if (null? nodes)
	     env
	     (loop (cdr nodes) (preinit* (car nodes) env))))))
	 
      
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
			   ((and (pair? %info) (eq? (car %info) 'preinit))
			    (when (>=fx (config-get args :verbose 0) 3)
			       (fprintf (current-error-port)
				  " [~a/~a:~a]" id (cdr %info)
				  (match-case loc
				     ((at ?file ?pos) pos)
				     (else ""))))
			    (let ((n (duplicate::J2SDeclInit o
					(binder 'let-opt)
					(val (type->val loc (cdr %info))))))
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
