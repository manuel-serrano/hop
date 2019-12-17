;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/method.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Tue Dec 17 09:18:04 2019 (serrano)                */
;*    Copyright   :  2017-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Function->method transformation                                  */
;*    -------------------------------------------------------------    */
;*    This optimization duplicates functions as methods, where THIS    */
;*    is statically known to be an object. This transformation applies */
;*    only when the occurrence number of THIS inside a function is     */
;*    above THIS-OCCURRENCE-THRESHOLD                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_method

   (include "ast.sch"
	    "usage.sch")
   
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
      (optional :optim-method)))

;*---------------------------------------------------------------------*/
;*    this-occurrence-threshold ...                                    */
;*---------------------------------------------------------------------*/
(define this-occurrence-threshold 1)
(define body-size-threshold 150)

;*---------------------------------------------------------------------*/
;*    j2s-method! ::J2SProgram ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-method! this::J2SProgram conf)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls)
	 (let ((log (make-cell '())))
	    (for-each (lambda (d) (method! d conf log)) decls)
	    (for-each (lambda (n) (method! n conf log)) nodes)
	    (when (>=fx (config-get conf :verbose 0) 3)
	       (display " " (current-error-port))
	       (fprintf (current-error-port) "(~(, ))"
		  (map (lambda (l)
			  (with-access::J2SNode (cdr l) (loc)
			     (format "~a(~a)" (car l) (caddr loc))))
		     (cell-ref log)))))))
   this)

;*---------------------------------------------------------------------*/
;*    method! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (method! this::J2SNode conf log)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    method! ::J2SAssig ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (method! this::J2SAssig conf log)
   (with-access::J2SAssig this (lhs rhs)
      (when (isa? rhs J2SFun)
	 (with-access::J2SFun rhs (thisp loc body generator name)
	    (when (and thisp (not generator))
	       (with-access::J2SDecl thisp (usecnt)
		  (when (and (>=fx usecnt this-occurrence-threshold)
			     (<fx (node-size body) body-size-threshold))
		     (cell-set! log (cons (cons name this) (cell-ref log)))
		     (let ((met (function->method rhs conf)))
			(set! rhs
			   (instantiate::J2SMethod 
			      (loc loc)
			      (function (prof-fun rhs conf))
			      (method met)))))))))
      this))

;*---------------------------------------------------------------------*/
;*    method! ::J2SDeclFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (method! this::J2SDeclFun conf log)
   (with-access::J2SDeclFun this (val id)
      (set! val (method! val conf log))
      (with-access::J2SFun val (thisp loc body generator)
	 (with-access::J2SDecl thisp (usecnt)
	    (cond
	       ((and (not (decl-usage-has? this
			     '(assig ref assig get set call delete instanceof uninit rest eval)))
		     (not (isa? this J2SDeclSvc)))
		(with-access::J2SDecl thisp (utype)
		   (set! utype 'object)))
	       ((and (decl-usage-has? this '(ref get))
		     (not (decl-usage-has? this '(new)))
		     (>=fx usecnt this-occurrence-threshold)
		     (<fx (node-size body) body-size-threshold)
		     (not generator))
		(cell-set! log (cons (cons id this) (cell-ref log)))
		(let ((met (function->method val conf)))
		   (set! val
		      (instantiate::J2SMethod
			 (loc loc)
			 (function (prof-fun val conf))
			 (method met))))))))
      this))

;*---------------------------------------------------------------------*/
;*    prof-fun ...                                                     */
;*---------------------------------------------------------------------*/
(define (prof-fun val::J2SFun conf)
   (with-access::J2SFun val (body name)
      (when (config-get conf :profile-method #f)
	 (with-access::J2SBlock body (loc endloc)
	    (set! body
	       (J2SBlock
		  (J2SStmtExpr
		     (J2SPragma
			`(js-profile-log-method-function
			    (& ,(symbol->string name)) ',loc)))
		  body))))
      val))

;*---------------------------------------------------------------------*/
;*    function->method ...                                             */
;*---------------------------------------------------------------------*/
(define (function->method this::J2SFun conf)
   
   (define (j2sdecl-duplicate p::J2SDecl)
      (duplicate::J2SDecl p
	 (key (ast-decl-key))))

   (define (prof name body::J2SBlock)
      (if (config-get conf :profile-method #f)
	  (with-access::J2SBlock body (loc endloc)
	     (J2SBlock
		(J2SStmtExpr
		   (J2SPragma
		      `(js-profile-log-method-method
			  (& ,(symbol->string name)) ',loc)))
		body))
	  body))
   
   (with-access::J2SFun this (params thisp name body method optimize)
      (let* ((nparams (map j2sdecl-duplicate params))
	     (nthisp (j2sdecl-duplicate thisp))
	     (nbody (j2s-alpha body (cons thisp params) (cons nthisp nparams))))
	 (set! optimize #f)
	 (use-count nbody +1 0)
	 (with-access::J2SDecl nthisp (utype)
	    (set! utype 'object)
	    (let ((m (duplicate::J2SFun this
			(optimize #t)
			(name (when (symbol? name) (symbol-append name '&)))
			(params nparams)
			(thisp nthisp)
			(body (prof name nbody)))))
	       (set! method m)
	       m)))))


