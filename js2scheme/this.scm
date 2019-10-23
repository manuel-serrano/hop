;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/this.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Wed Oct 23 09:41:35 2019 (serrano)                */
;*    Copyright   :  2013-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Init the this variable of all non-strict mode functions.         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_this

   (include "ast.sch")
   
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

   (define (init-this thisp loc)
      (with-access::J2SDecl thisp (id)
	 (J2SIf (J2SPragma/bindings 'bool
		   '(^this) (list (J2SThis thisp))
		   '(or (eq? ^this (js-undefined)) (eq? ^this (js-null))))
	    (J2SStmtExpr
	       (J2SAssig (J2SThis thisp) (J2SPragma/type 'object '%this)))
	    (J2SIf (J2SPragma/bindings 'bool
		      '(^this) (list (J2SThis thisp))
		      '(not (js-object? ^this)))
	       (J2SStmtExpr
		  (J2SAssig (J2SThis thisp)
		     (J2SPragma/bindings 'object
			'(^this) (list (J2SThis thisp))
			'(js-toobject %this ^this))))
	       (J2SNop)))))
   
   (with-access::J2SFun this (mode body params id loc thisp)
      (when (eq? mode 'normal)
	 (let ((nbody (this! body)))
	    (when (this? nbody)
	       (with-access::J2SDecl thisp (utype)
		  (set! utype 'any))
	       (set! body
		  (with-access::J2SBlock body (endloc)
		     (instantiate::J2SBlock
			(loc loc)
			(endloc endloc)
			(nodes (list (init-this thisp loc) nbody)))))))))
   this)
