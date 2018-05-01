;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/globvar.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Tue May  1 06:41:04 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Global variables optimization (initialization and constant       */
;*    propagation).                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_globvar

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
;*    globconst-mark ...                                               */
;*---------------------------------------------------------------------*/
(define globconst-mark (cons 1 2))

;*---------------------------------------------------------------------*/
;*    globcnst ...                                                     */
;*---------------------------------------------------------------------*/
(define-struct globcnst expr)

;*---------------------------------------------------------------------*/
;*    j2s-globvar! ::J2SProgram ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-globvar! this::J2SProgram args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (unless direct-eval
	    (let ((gcnsts (collect-gloconst* this)))
	       (when (pair? gcnsts)
		  ;; mark all the global before traversing for references
		  (for-each (lambda (g)
			       (with-access::J2SDecl g (%info)
				  (set! %info #unspecified)))
		     gcnsts)
		  (invalidate-early-accesses this gcnsts)
		  (when (find (lambda (g)
				 (with-access::J2SDecl g (%info)
				    (eq? %info globconst-mark)))
			   gcnsts)
		     ;; propagate the constants
		     (propagate-constant! this)))))))
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
	  (with-access::J2SDecl decl (ronly writable)
	     (or ronly (not writable)))))
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
   '())

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SDeclInit ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SDeclInit)
   (with-access::J2SDeclInit this (usage ronly val)
      (if (and (usage? '(init) usage)
	       (not (usage? '(assig) usage))
	       (constant? val))
	  (list this)
	  (collect-gloconst* val))))

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SFun ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SFun)
   '())

;*---------------------------------------------------------------------*/
;*    invalidate-early-accesses ::J2SNode ...                          */
;*    -------------------------------------------------------------    */
;*    Scan the whole program and invalidate all global variables       */
;*    that can possibily be accessed before initialized.               */
;*---------------------------------------------------------------------*/
(define (invalidate-early-accesses this::J2SNode gcnsts::pair-nil)
   (invalidate-early-decl this gcnsts globconst-mark))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SNode ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SNode gcnsts flag)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SRef ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SRef gcnsts flag)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info %%dump)
	 (unless (eq? %info globconst-mark)
	    (set! %%dump "globvar:early")
	    (set! %info #f)))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SDeclInit ...                          */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SDeclInit gcnsts flag)
   (with-access::J2SDeclInit this (val %info %%dump)
      (invalidate-early-decl val gcnsts flag)
      (when (eq? %info #unspecified)
	 (set! %%dump "globvar:conditional")
	 (set! %info flag))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl-condition ...                              */
;*---------------------------------------------------------------------*/
(define (invalidate-early-decl-condition test then else gcnsts flag)
   (invalidate-early-decl test gcnsts flag)
   (invalidate-early-decl then gcnsts #f)
   (invalidate-early-decl else gcnsts #f))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SIf ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SIf gcnsts flag)
   (with-access::J2SIf this (test then else)
      (invalidate-early-decl-condition test then else gcnsts flag)))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SCond ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SCond gcnsts flag)
   (with-access::J2SCond this (test then else)
      (invalidate-early-decl-condition test then else gcnsts flag)))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SLoop ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SLoop gcnsts flag)
   (with-access::J2SLoop this (body)
      ;; invalidate the body first
      (invalidate-early-decl body gcnsts #f)
      ;; and invoke the default walker for loop test and increment
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SCall ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SCall gcnsts flag)

   (define (invalidate-all! gcnsts)
      (for-each (lambda (g)
		   (with-access::J2SDecl g (%info %%dump)
		      (when (eq? %info #unspecified)
			 (set! %%dump "globvar:closure")
			 (set! %info #f))))
	 gcnsts))

   (define (invalidate-function! this::J2SFun gcnsts flag)
      (with-access::J2SFun this (%info body)
	 (unless (eq? %info 'globvar-invalidation-done)
	    (set! %info 'globvar-invalidation-done)
	    (invalidate-early-decl body gcnsts flag))))
   
   (with-access::J2SCall this (fun args)
      ;; follow the function
      (unless (isa? fun J2SFun)
	 (invalidate-early-decl fun gcnsts flag))
      ;; follow the arg
      (for-each (lambda (a)
		   (invalidate-early-decl a gcnsts flag))
	 args)
      ;; follow static functions
      (cond
	 ((isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     (if (isa? decl J2SDeclFun)
		 (with-access::J2SDeclFun decl (val)
		    (invalidate-function! val gcnsts flag))
		 (invalidate-all! gcnsts))))
	 ((isa? fun J2SFun)
	  (invalidate-function! fun gcnsts flag))
	 (else
	  (invalidate-all! gcnsts)))))

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
      (with-access::J2SDecl decl (%info)
	 (if (eq? %info globconst-mark)
	     (with-access::J2SDeclInit decl (val usecnt)
		(set! usecnt (-fx usecnt 1))
		;; copy the value
		(j2s-alpha val '() '()))
	     (call-default-walker)))))
   
   
(define (j2s-globvar-old! this::J2SProgram args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (unless direct-eval
	    (let ((closures (make-cell '())))
	       (for-each (lambda (d)
			    (cond
			       ((isa? d J2SDeclSvc) (globvar! d closures #t))
			       ((isa? d J2SDeclFun) #unspecified)
			       ((isa? d J2SDeclInit) (globvar! d closures #t))))
		  decls)
	       (map! (lambda (n) (globvar! n closures #t)) nodes)))))
   this)

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SNode closures::cell exec)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SFun closures::cell exec)
   (cell-set! closures (cons this (cell-ref closures)))
   this)

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SRef ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SRef closures::cell exec)
   
   (define (constant? expr::J2SExpr)
      (cond
	 ((or (isa? expr J2SLiteralCnst)
	      (isa? expr J2SNativeString)
	      (isa? expr J2SString)
	      (isa? expr J2SNumber))
	  #t)
	 ((isa? expr J2SRef)
	  (with-access::J2SRef expr (decl)
	     (with-access::J2SDecl decl (ronly writable)
		(or ronly (not writable)))))
	 ((isa? expr J2SUnary)
	  (with-access::J2SUnary expr (expr)
	     (constant? expr)))
	 ((isa? expr J2SBinary)
	  (with-access::J2SBinary expr (lhs rhs)
	     (and (constant? lhs) (constant? rhs))))
	 (else
	  #f)))
   
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (scope %info id)
	 (when (isa? decl J2SDeclFun)
	    (cell-set! closures (cons decl (cell-ref closures)))
	    (with-access::J2SDeclFun decl (val)
	       (globvar! val closures exec)))
	 (cond
	    ((not (or (eq? scope 'global) (eq? scope '%scope)))
	     (call-default-walker))
	    ((not (globcnst? %info))
	     (set! %info 'noglobcnst)
	     (call-default-walker))
	    (else
	     (let ((expr (globcnst-expr %info)))
		(if (constant? expr)
		    (j2s-alpha expr '() '())
		    (call-default-walker))))))))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SCall closures::cell exec)
   
   (define (flush-function! this exec)
      (with-access::J2SFun this (%info body)
	 (if (eq? %info 'globvar-fun)
	     this
	     (begin
		(set! %info 'globvar-fun)
		(set! body (globvar! body closures exec))))))
   
   (define (flush-closures! closures)
      (let loop ()
	 (let ((cs (cell-ref closures)))
	    (cell-set! closures '())
	    (when (pair? cs)
	       (for-each (lambda (c)
			    (cond
			       ((isa? c J2SFun)
				(flush-function! c #f))
			       ((isa? c J2SDeclFun)
				(with-access::J2SDeclFun c (val)
				   (flush-function! val #f)))))
		  cs)
	       (loop)))))

   (with-access::J2SCall this (fun args)
      ;; follow static functions
      (if (isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     (if (isa? decl J2SDeclFun)
		 (with-access::J2SDeclFun decl (val id)
		    (flush-function! val #t))
		 (with-access::J2SDecl decl (id)
		    (flush-closures! closures))))
	  (flush-closures! closures))
      (set! args (map! (lambda (a) (globvar! a closures #f)) args))
      this))
      
;*---------------------------------------------------------------------*/
;*    globvar! ::J2SInit ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SInit closures::cell exec)
   (with-access::J2SInit this (lhs rhs)
      (set! rhs (globvar! rhs closures exec))
      (when (and exec (isa? lhs J2SRef))
	 (with-access::J2SRef lhs (decl)
	    (with-access::J2SDecl decl (scope usage %info id)
	       (when (or (eq? scope 'global) (eq? scope '%scope))
		  (unless (or (eq? %info 'noglobcnst)
			      (memq 'assig usage)
			      (memq 'delete usage))
		     (set! %info (globcnst rhs)))))))
      this))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SDeclInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SDeclInit closures::cell exec)
   (let ((init (call-default-walker)))
      (when exec
	 (with-access::J2SDeclInit this (val scope usage %info)
	    (when (or (eq? scope 'global) (eq? scope '%scope))
	       (unless (or (eq? %info 'noglobcnst)
			   (memq 'assig usage)
			   (memq 'delete usage))
		  (set! %info (globcnst val))))))
      init))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SCond ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SCond closures::cell exec)
   (with-access::J2SCond this (test then else)
      (set! test (globvar! test closures exec))
      (set! then (globvar! then closures #f))
      (set! else (globvar! else closures #f))
      this))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SIf ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SIf closures::cell exec)
   (with-access::J2SIf this (test then else)
      (set! test (globvar! test closures exec))
      (set! then (globvar! then closures #f))
      (set! else (globvar! else closures #f))
      this))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SSwitch ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SSwitch closures::cell exec)
   (with-access::J2SSwitch this (key cases)
      (set! key (globvar! key closures exec))
      (set! cases (map! (lambda (c) (globvar! c closures #f)) cases))
      this))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SFor ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SFor closures::cell exec)
   (with-access::J2SFor this (init test incr body)
      (set! init (globvar! init closures exec))
      (set! test (globvar! test closures exec))
      (set! incr (globvar! incr closures #f))
      (set! body (globvar! body closures #f))
      this))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SForIn ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SForIn closures::cell exec)
   (with-access::J2SForIn this (lhs obj body)
      (set! lhs (globvar! lhs closures exec))
      (set! obj (globvar! obj closures exec))
      (set! body (globvar! body closures #f))
      this))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SWhile ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SWhile closures::cell exec)
   (with-access::J2SWhile this (test body)
      (set! test (globvar! test closures exec))
      (set! body (globvar! body closures #f))
      this))

