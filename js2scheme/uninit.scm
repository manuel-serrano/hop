;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/uninit.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 24 13:11:25 2019                          */
;*    Last change :  Thu Jan 24 17:06:29 2019 (serrano)                */
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
				(set! %info 'unknown)))
		   decls)
		;; mark double initialization
		(for-each invalidate-double-decl nodes)
		;; mark variables used before initialized
		(for-each invalidate-early-decl nodes)))))
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
;*    invalidate-early-decl ::J2SNode ...                              */
;*    -------------------------------------------------------------    */
;*    Scan the whole program and invalidate all global variables       */
;*    that can possibily be accessed before initialized.               */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SRef ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SRef)
   (with-access::J2SRef this (decl loc)
      (with-access::J2SDecl decl (%info usage id key)
	 (when (memq %info '(unknown init0 double))
	    (if (isa? decl J2SDeclFun)
		(begin
		   (set! %info 'init)
		   (with-access::J2SDeclFun decl (val)
		      (invalidate-early-decl val)))
		(begin
		   (set! %info 'uninit)
		   (set! usage (cons 'uninit usage))))))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SInit ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SInit)
   (with-access::J2SInit this (lhs rhs %%dump)
      (invalidate-early-decl rhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (%info id)
		(when (eq? %info 'init0)
		   ;; for sure this is initialized
		   (set! %info 'init))))
	  (invalidate-early-decl lhs))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SDeclInit ...                          */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SDeclInit)
   (with-access::J2SDeclInit this (%info val)
      (invalidate-early-decl val)
      (when (eq? %info 'init0)
	 ;; for sure this is initialized
	 (set! %info 'init))))

;*---------------------------------------------------------------------*/
;*    invalidate-early-decl ::J2SDeclFun ...                           */
;*---------------------------------------------------------------------*/
(define-walk-method (invalidate-early-decl this::J2SDeclFun)
   (with-access::J2SDeclInit this (%info val)
      (when (eq? %info 'initi0)
	 ;; for sure this is initialized
	 (set! %info 'init))))

