;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/uninit.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 24 13:11:25 2019                          */
;*    Last change :  Fri Jan 25 09:05:55 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Mark global variables potentially used before being initialized. */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_uninit

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-uninit-stage
	   j2s-uninit-globprop-stage))

;*---------------------------------------------------------------------*/
;*    j2s-uninit-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-uninit-stage
   (instantiate::J2SStageProc
      (name "uninit")
      (comment "Global variable initialization optimization")
      (proc (lambda (n args) (j2s-uninit! n args)))
      (optional 2)))

;*---------------------------------------------------------------------*/
;*    j2s-uninit-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-uninit-globprop-stage
   (instantiate::J2SStageProc
      (name "uninit")
      (comment "Global variable initialization optimization")
      (proc (lambda (n args) (j2s-uninit! n args)))
      (optional :optim-globprop)))

;*---------------------------------------------------------------------*/
;*    j2s-uninit! ::J2SProgram ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-uninit! this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (if direct-eval
	     (for-each (lambda (decl)
			  (unless (isa? decl J2SDeclFun)
			     (with-access::J2SDecl decl (usage)
				(set! usage (cons 'uninit usage)))))
		decls)
	     (begin
		;; initialize all global variables
		(for-each (lambda (decl)
			     (with-access::J2SDecl decl (%info)
				(if (isa? decl J2SDeclInit)
				    (set! %info 'init0)
				    (set! %info 'unknown))))
		   decls)
		;; mark single decl init initialization
		(for-each invalidate-double-decl nodes)
		;; mark global declinit single init
		(for-each (lambda (decl)
			     (with-access::J2SDecl decl (%info)
				(when (and (isa? decl J2SDeclInit) (eq? %info 'init0))
				   (set! %info 'init))))
		   decls)
		;; mark variables used before initialized
		(for-each (lambda (n) (invalidate-early-decl n #t)) nodes)
		;; mark all variables not initialized for sure
		(for-each (lambda (decl)
			     (with-access::J2SDecl decl (%info usage id)
				(when (memq %info '(init0 unknown double))
				   (set! usage (cons 'uninit usage)))))
		   decls)))))
   this)

;*---------------------------------------------------------------------*/
;*    invalidate-double-decl ::J2SNode ...                             */
;*    -------------------------------------------------------------    */
;*    Scan the whole program and invalidate all global variables       */
;*    that are initialized several times.                              */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-double-decl this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    invalidate-double-decl ::J2SInit ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-double-decl this::J2SInit)
   (with-access::J2SInit this (lhs rhs)
      (invalidate-double-decl rhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (%info usage)
		(case %info
		   ((unknown)
		    (set! %info 'init0))
		   ((init0)
		    (set! %info 'double)))))
	  (invalidate-double-decl lhs))))

;*---------------------------------------------------------------------*/
;*    invalidate-double-decl ::J2SDeclInit ...                         */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-double-decl this::J2SDeclInit)
   (with-access::J2SDeclInit this (%info val)
      (invalidate-double-decl val)
      (case %info
	 ((unknown)
	  (set! %info 'init0))
	 ((init0)
	  (set! %info 'double)))))
      
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
      (with-access::J2SDecl decl (%info usage id key)
	 (when (memq %info '(unknown init0 double))
	    (if (isa? decl J2SDeclFun)
		(begin
		   (set! %info 'init)
		   (with-access::J2SDeclFun decl (val)
		      (invalidate-early-decl val #f)))
		(begin
		   (set! %info 'uninit)
		   (set! usage (cons 'uninit usage))))))))

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

      
