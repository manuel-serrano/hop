;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/method.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Thu Nov 30 07:31:52 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Function->method transformation                                  */
;*    -------------------------------------------------------------    */
;*    This optimization duplication function as method, where THIS     */
;*    is statically known to be an object. This transformation applies */
;*    only when the occurrence number of THIS inside a function is     */
;*    sufficient.                                                      */
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
      (optional 2)))

;*---------------------------------------------------------------------*/
;*    this-occurrence-threshold ...                                    */
;*---------------------------------------------------------------------*/
(define this-occurrence-threshold 1)

;*---------------------------------------------------------------------*/
;*    j2s-method! ::J2SProgram ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-method! this::J2SProgram args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls)
	 (for-each method! decls)
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
		  (when (>=fx usecnt this-occurrence-threshold)
		     (set! rhs
			(instantiate::J2SMethod
			   (loc loc)
			   (function rhs)
			   (method (function->method rhs)))))))))
      this))

;*---------------------------------------------------------------------*/
;*    method! ::J2SDeclFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (method! this::J2SDeclFun)
   (with-access::J2SDeclFun this (usage val)
      (when (only-usage? '(new init) usage)
	 (with-access::J2SFun val (thisp)
	    (with-access::J2SDecl thisp (itype utype vtype)
	       (set! itype 'object)
	       (set! utype 'object)
	       (set! vtype 'object)))))
      (call-default-walker))

;*---------------------------------------------------------------------*/
;*    function->method ...                                             */
;*---------------------------------------------------------------------*/
(define (function->method this::J2SFun)
   
   (define (j2sdecl-duplicate p::J2SDecl)
      (duplicate::J2SDecl p
	 (key (ast-decl-key))))
   
   (with-access::J2SFun this (params thisp name body method optimize)
      (let* ((nparams (map j2sdecl-duplicate params))
	     (nthisp (j2sdecl-duplicate thisp))
	     (nbody (j2s-alpha body (cons thisp params) (cons nthisp nparams))))
	 (set! optimize #f)
	 (with-access::J2SDecl thisp (itype vtype utype)
	    (set! itype 'any)
	    (set! vtype 'any)
	    (set! utype 'any))
	 (with-access::J2SDecl nthisp (itype vtype utype)
	    (set! itype 'object)
	    (set! vtype 'object)
	    (set! utype 'object)
	    (let ((m (duplicate::J2SFun this
			(optimize #t)
			(name (when (symbol? name) (symbol-append name '%%%)))
			(params nparams)
			(thisp nthisp)
			(body nbody))))
	       (set! method m)
	       m)))))


