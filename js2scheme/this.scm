;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/this.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Fri Jul  3 16:14:04 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Init the this variable of all function in non-strict mode        */
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

   (export j2s-this-stage
	   (generic j2s-this ::obj ::obj)))

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
(define-generic (j2s-this this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-this ::J2SProgram ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-this this::J2SProgram args)
   (with-access::J2SProgram this (nodes headers decls)
      (for-each (lambda (o) (this! o)) headers)
      (for-each (lambda (o) (this! o)) decls)
      (for-each (lambda (o) (this! o)) nodes))
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
      (instantiate::J2SPragma
	 (loc loc)
	 (expr `(cond
		   ((or (eq? this (js-undefined))
			(eq? this (js-null)))
		    ;; use to be
		    ;; (set! this %scope)
		    ;; but it breaks nodejs/test/simple/test-fs-fstat.js
		    (set! this %this))
		   ((not (isa? this JsObject))
		    (set! this (js-toobject %this this)))))))
   
   (with-access::J2SFun this (mode body params id loc)
      (when (eq? mode 'normal)
	 (let ((nbody (this! body)))
	    (when (this? nbody)
	       (set! body
		  (with-access::J2SBlock body (endloc)
		     (instantiate::J2SBlock
			(loc loc)
			(endloc endloc)
			(nodes (list (init-this loc) nbody)))))))))
   this)

;*---------------------------------------------------------------------*/
;*    this? ...                                                        */
;*---------------------------------------------------------------------*/
(define (this? body)
   (let ((res (make-cell #f)))
      (use-this? body res)
      (cell-ref res)))

;*---------------------------------------------------------------------*/
;*    use-this? ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (use-this? this::J2SNode res)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    this? ::J2SThis ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (use-this? this::J2SThis res)
   (cell-set! res #t))
   
;*---------------------------------------------------------------------*/
;*    this? ::J2SThis ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (use-this? this::J2SFun res)
   #f)
   
