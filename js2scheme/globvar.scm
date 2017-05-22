;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/globvar.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Mon May 22 14:39:05 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Global variables optimization (initialization)                   */
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
      (optional #t)))

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
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (scope %info id)
	 (when (isa? decl J2SDeclFun)
	    (cell-set! closures (cons decl (cell-ref closures))))
	 (cond
	    ((not (or (eq? scope 'global) (eq? scope '%scope)))
	     (call-default-walker))
	    ((not (globcnst? %info))
	     (set! %info 'noglobcnst)
	     (call-default-walker))
	    (else
	     (let ((expr (globcnst-expr %info)))
		(cond
		   ((or (isa? expr J2SLiteralCnst)
			(isa? expr J2SNativeString)
			(isa? expr J2SString)
			(isa? expr J2SNumber))
		    (j2s-alpha expr '() '()))
		   ((when (isa? expr J2SRef)
		       (with-access::J2SRef expr (decl)
			  (with-access::J2SDecl decl (ronly writable)
			     (or ronly (not writable)))))
		    (j2s-alpha expr '() '()))
		   (else
		    (call-default-walker)))))))))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SCall closures::cell exec)
   
   (define (flush-function! this)
      (with-access::J2SFun this (%info body)
	 (if (eq? %info 'globvar-fun)
	     this
	     (begin
		(set! %info 'globvar-fun)
		(globvar! body closures #f)))))
   
   (define (flush-closures! closures)
      (let loop ()
	 (let ((cs (cell-ref closures)))
	    (cell-set! closures '())
	    (when (pair? cs)
	       (for-each (lambda (c)
			    (cond
			       ((isa? c J2SFun)
				(flush-function! c))
			       ((isa? c J2SDeclFun)
				(with-access::J2SDeclFun c (val)
				   (flush-function! val)))))
		  cs)
	       (loop)))))
   
   (with-access::J2SCall this (fun args)
      ;; follow static functions
      (if (isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     (if (isa? decl J2SDeclFun)
		 (with-access::J2SDeclFun decl (val)
		    (globvar! val closures exec))
		 (flush-closures! closures)))
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
      (globvar! test closures exec)
      (globvar! then closures #f)
      (globvar! else closures #f)
      this))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SIf ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SIf closures::cell exec)
   (with-access::J2SIf this (test then else)
      (globvar! test closures exec)
      (globvar! then closures #f)
      (globvar! else closures #f)
      this))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SSwitch ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SSwitch closures::cell exec)
   (with-access::J2SSwitch this (key cases)
      (globvar! key closures exec)
      (for-each (lambda (c) (globvar! c closures #f)) cases)
      this))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SFor ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SFor closures::cell exec)
   (with-access::J2SFor this (init test incr body)
      (globvar! init closures exec)
      (globvar! test closures exec)
      (globvar! incr closures #f)
      (globvar! body closures #f)
      this))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SForIn ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SForIn closures::cell exec)
   (with-access::J2SForIn this (lhs obj body)
      (globvar! lhs closures exec)
      (globvar! obj closures exec)
      (globvar! body closures #f)
      this))

;*---------------------------------------------------------------------*/
;*    globvar! ::J2SWhile ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (globvar! this::J2SWhile closures::cell exec)
   (with-access::J2SWhile this (test body)
      (globvar! test closures exec)
      (globvar! body closures #f)
      this))

