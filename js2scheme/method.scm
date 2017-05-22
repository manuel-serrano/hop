;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/method.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Mon May 22 18:21:33 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Function->method transformation                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_method

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-method-stage))

;*---------------------------------------------------------------------*/
;*    j2s-method-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-method-stage
   (instantiate::J2SStageProc
      (name "method")
      (comment "Function->method transformation")
      (proc (lambda (n args) (j2s-method! n args)))
      (optional #t)))

;*---------------------------------------------------------------------*/
;*    j2s-method! ::J2SProgram ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-method! this::J2SProgram args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes)
	 (for-each method! nodes)))
   this)

;*---------------------------------------------------------------------*/
;*    method! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (method! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    method! ::J2SAssig ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (method! this::J2SAssig)
   (with-access::J2SAssig this (lhs rhs)
      (when (isa? rhs J2SFun)
	 (with-access::J2SFun rhs (thisp loc)
	    (when thisp
	       (with-access::J2SDecl thisp (usecnt)
		  (when (>=fx usecnt 2)
		     (set! rhs
			(instantiate::J2SMethod
			   (loc loc)
			   (function rhs)
			   (method (function->method rhs)))))))))
      this))

;*---------------------------------------------------------------------*/
;*    function->method ...                                             */
;*---------------------------------------------------------------------*/
(define (function->method this::J2SFun)
   
   (define (j2sdecl-duplicate p::J2SDecl)
      (duplicate::J2SDecl p
	 (key (ast-decl-key))))
   
   (with-access::J2SFun this (params thisp name body method)
      (let* ((nparams (map j2sdecl-duplicate params))
	     (nthisp (j2sdecl-duplicate thisp))
	     (nbody (j2s-alpha body (cons thisp params) (cons nthisp params))))
	 (with-access::J2SDecl nthisp (itype vtype utype)
	    (set! itype 'object)
	    (set! vtype 'object)
	    (set! utype 'object)
	    (let ((m (duplicate::J2SFun this
			(name (when (symbol? name) (symbol-append name '%%%)))
			(params nparams)
			(thisp nthisp)
			(body nbody))))
	       (set! method m)
	       m)))))


