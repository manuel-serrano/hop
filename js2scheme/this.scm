;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/this.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Mon Aug  6 14:29:48 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Init the this variable of all functions in non-strict mode.      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_this

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)

   (export j2s-this-stage))

;*---------------------------------------------------------------------*/
;*    j2s-this-stage ...                                               */
;*---------------------------------------------------------------------*/
(define j2s-this-stage
   (instantiate::J2SStageProc
      (name "this")
      (comment "Init the this variable of non-strict functions")
      (proc j2s-this)
      (optional #f)))

;*---------------------------------------------------------------------*/
;*    j2s-this ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-this this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls)
	 (for-each (lambda (o) (this! o)) headers)
	 (for-each (lambda (o) (this! o)) decls)
	 (for-each (lambda (o) (this! o)) nodes)))
   this)

;*---------------------------------------------------------------------*/
;*    this! ::J2SNode ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (this! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    this! ::J2SFun ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (this! this::J2SFun)

   (define (init-this loc)
      (let ((prag (instantiate::J2SPragma
		     (loc loc)
		     (type 'any)
		     (expr `(cond
			       ((or (eq? this (js-undefined))
				    (eq? this (js-null)))
				;; use to be
				;; (set! this %scope)
				;; but it breaks nodejs/test/simple/test-fs-fstat.js
				(set! this %this))
			       ((not (js-object? this))
				(set! this (js-toobject %this this))))))))
      (instantiate::J2SStmtExpr
	 (loc loc)
	 (expr prag))))
   
   (with-access::J2SFun this (mode body params id loc thisp)
      (when (eq? mode 'normal)
	 (let ((nbody (this! body)))
	    (when (this? nbody)
	       (with-access::J2SDecl thisp (vtype)
		  (set! vtype 'any))
	       (set! body
		  (with-access::J2SBlock body (endloc)
		     (instantiate::J2SBlock
			(loc loc)
			(endloc endloc)
			(nodes (list (init-this loc) nbody)))))))))
   this)
