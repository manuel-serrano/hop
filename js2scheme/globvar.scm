;*=====================================================================*/
;*    .../prgm/project/hop/3.2.x-new-types/js2scheme/globvar.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Sun Sep  2 16:15:45 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Global variables optimization (initialization and constant       */
;*    propagation).                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_globvar

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-globvar-stage))

;*---------------------------------------------------------------------*/
;*    j2s-globvar-stage ...                                            */
;*---------------------------------------------------------------------*/
(define j2s-globvar-stage
   (instantiate::J2SStageProc
      (name "globvar")
      (comment "Global variable initialization optimization")
      (proc (lambda (n args) (j2s-globvar! n args)))
      (optional 2)))

;*---------------------------------------------------------------------*/
;*    j2s-globvar! ::J2SProgram ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-globvar! this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (unless direct-eval
	    (let ((gcnsts (collect-gloconst* this)))
	       (when (pair? gcnsts)
		  ;; mark all the global before traversing for references
		  (invalidate-early-decl this #f '())
		  (when (find (lambda (g)
				 (with-access::J2SDecl g (%info)
				    (pair? %info)))
			   gcnsts)
		     ;; propagate the constants
		     (propagate-constant! this)))
	       (when (>=fx (config-get args :verbose 0) 3)
		  (display " " (current-error-port))
		  (fprintf (current-error-port) "(~(, ))"
		     (filter-map (lambda (g)
				    (with-access::J2SDecl g (%info id)
				       (when (and (pair? %info)
						  (eq? (car %info) 'init))
					  id)))
			gcnsts)))))))
   this)

;*---------------------------------------------------------------------*/
;*    constant? ...                                                    */
;*---------------------------------------------------------------------*/
(define (constant? expr::J2SExpr)
   (cond
      ((or (isa? expr J2SLiteralCnst)
	   (isa? expr J2SNativeString)
	   (isa? expr J2SString)
	   (isa? expr J2SBool)
	   (isa? expr J2SUndefined)
	   (isa? expr J2SNull)
	   (isa? expr J2SNumber))
       #t)
      ((isa? expr J2SRef)
       (with-access::J2SRef expr (decl)
	  (with-access::J2SDecl decl (ronly writable usage)
	     (or ronly (not writable) (not (usage? '(assig) usage))))))
      ((isa? expr J2SUnary)
       (with-access::J2SUnary expr (expr)
	  (constant? expr)))
      ((isa? expr J2SBinary)
       (with-access::J2SBinary expr (lhs rhs)
	  (and (constant? lhs) (constant? rhs))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SNode ...                                  */
;*    -------------------------------------------------------------    */
;*    Collect all the global variables that are initialized but        */
;*    never assigned.                                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SDecl ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SDecl)
   (with-access::J2SDecl this (%info %%dump)
      (set! %%dump #unspecified)
      (set! %info #unspecified))
   '())

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SInit ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SInit)
   (with-access::J2SInit this (lhs rhs)
      ;; no need to scan rhs as we are only looking for variable decls/inits
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (usage ronly val %info %%dump id)
		(if (and (not (usage? '(assig) usage)) (constant? rhs))
		    (cond
		       ((and (pair? %info) (eq? (car %info) 'uninit))
			;; multiple init, invalidate
			   (set! %%dump "globvar:multiple")
			   (set! %info 'multi)
			'())
		       ((not %info)
			(set! %info (cons 'uninit this))
			(list decl))
		       (else
			'()))
		    (begin
		       (set! %%dump "globvar:not constant")
		       (set! %info 'noconstant)
		       '()))))
	  '())))

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SDeclInit ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SDeclInit)
   (with-access::J2SDeclInit this (usage ronly val %info %%dump id)
      (if (and ronly (not (usage? '(assig) usage)) (constant? val))
	  (begin
	     (set! %%dump this)
	     (set! %info (cons 'undecl this))
	     (list this))
	  (begin
	     (set! %%dump "globvar:read-write")
	     (set! %info #unspecified)
	     '()))))

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SFun ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SFun)
   '())

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SNode ...                              */
;*    -------------------------------------------------------------    */
;*    Scan the whole program and invalidate all global variables       */
;*    that can possibily be accessed before initialized.               */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SNode inexpr::bool stack)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SRef ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SRef inexpr stack)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info %%dump)
	 (unless (and (pair? %info) (eq? (car %info) 'init))
	    (set! %%dump "globvar:early")
	    (set! %info #f))
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (val)
	       (unless (memq decl stack)
		  (invalidate-early-decl val #t (cons decl stack))))))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SInit ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SInit inexpr stack)
   (with-access::J2SInit this (lhs rhs %%dump)
      (invalidate-early-decl rhs #t stack)
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	    (with-access::J2SDecl decl (%info)
	       (when (and (pair? %info) (eq? (car %info) 'uninit))
		  ;; for sure this can be optimized
		  (set-car! %info 'init)))))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SDeclInit ...                          */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SDeclInit inexpr stack)
   (with-access::J2SDeclInit this (val %info %%dump)
      (invalidate-early-decl val #t stack)
      (when (and (pair? %info) (eq? (car %info) 'uninit))
	 ;; for sure this can be optimized
	 (set-car! %info 'init))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SDeclFun ...                           */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SDeclFun inexpr stack)
   (with-access::J2SDeclInit this (val %info %%dump)
      (when (and (pair? %info) (eq? (car %info) 'uninit))
	 ;; for sure this can be optimized
	 (set-car! %info 'init))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SAssig ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SAssig inexpr stack)
   (if inexpr
       (call-default-walker)
       (with-access::J2SAssig this (lhs rhs)
	  (let loop ((lhs lhs))
	     (cond
		((isa? lhs J2SAccess)
		 (with-access::J2SAccess lhs (obj field)
		    (loop obj)))
		((not (isa? lhs J2SRef))
		 (invalidate-early-decl lhs #t stack)
		 (invalidate-early-decl rhs #t stack))
		(else
		 (with-access::J2SRef lhs (decl)
		    (invalidate-early-decl lhs inexpr stack)
		    (with-access::J2SDecl decl (%info %%dump)
		       (invalidate-early-decl rhs
			  (not (and (pair? %info) (eq? (car %info) 'init)))
			  stack)))))))))

;*---------------------------------------------------------------------*/
;*    propagate-constant! ::J2SNode ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-constant! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    propagage-constant! ::J2SRef ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-constant! this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info id)
	 (cond
	    ((and (pair? %info) (isa? (cdr %info) J2SInit))
	     (with-access::J2SInit (cdr %info) (rhs)
		;; copy the value
		(j2s-alpha (propagate-constant! rhs) '() '())))
	    ((and (pair? %info) (eq? (car %info) 'init) (isa? decl J2SDeclInit))
	     (with-access::J2SDeclInit decl (val usecnt)
		(set! usecnt (-fx usecnt 1))
		;; copy the value
		(j2s-alpha (propagate-constant! val) '() '())))
	    
	    (else
	     (call-default-walker))))))

;*---------------------------------------------------------------------*/
;*    propagate-constant! ::J2SInit ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-constant! this::J2SInit)
   (with-access::J2SInit this (lhs rhs loc)
      (if (and (isa? lhs J2SRef)
	       (with-access::J2SRef lhs (decl)
		  (with-access::J2SDecl decl (%info)
		     (and (pair? %info) (isa? (cdr %info) J2SInit)))))
	  (J2SUndefined)
	  (call-default-walker))))
