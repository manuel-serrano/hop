;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/method.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Sun Aug 12 07:13:31 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Function->method transformation                                  */
;*    -------------------------------------------------------------    */
;*    This optimization duplicates functions as methods, where THIS    */
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
	   __js2scheme_alpha
	   __js2scheme_use
	   __js2scheme_node-size)

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
(define body-size-threshold 150)

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
	 (with-access::J2SFun rhs (thisp loc body)
	    (when thisp
	       (with-access::J2SDecl thisp (usecnt)
		  (when (and (>=fx usecnt this-occurrence-threshold)
			     (<fx (node-size body) body-size-threshold))
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
   (with-access::J2SDeclFun this (usage val id)
      (set! val (method! val))
      (with-access::J2SFun val (thisp loc body)
	 (with-access::J2SDecl thisp (usecnt)
	    (cond
	       ((only-usage? '(new init) usage)
		(with-access::J2SDecl thisp (utype)
		   (set! utype 'object)))
	       ((and (usage? '(ref get) usage)
		     (not (usage? '(new) usage))
		     (>=fx usecnt this-occurrence-threshold)
		     (<fx (node-size body) body-size-threshold))
		(set! val
		   (instantiate::J2SMethod
		      (loc loc)
		      (function val)
		      (method (function->method val))))))))
      this))

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
	 (use-count nbody +1 #f)
;* 	 (with-access::J2SDecl thisp (utype)                         */
;* 	    (set! utype 'any))                                       */
	 (with-access::J2SDecl nthisp (utype)
	    (set! utype 'object)
	    (let ((m (duplicate::J2SFun this
			(optimize #t)
			(name (when (symbol? name) (symbol-append name '%%%)))
			(params nparams)
			(thisp nthisp)
			(body nbody))))
	       (set! method m)
	       m)))))


