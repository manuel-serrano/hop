;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/topasync.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul  5 12:24:05 2024                          */
;*    Last change :  Fri Jul  5 12:58:52 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    This stage pack the top level forms into an async function when  */
;*    one of these form is an async.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_topasync

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils)

   (export j2s-topasync-stage
	   (generic j2s-topasync ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-topasync-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-topasync-stage
   (instantiate::J2SStageProc
      (name "topasync")
      (comment "Add a dummy global async function is one top level is an async")
      (proc j2s-topasync)))

;*---------------------------------------------------------------------*/
;*    j2s-topasync ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (j2s-topasync this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-topasync ::J2SProgram ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-topasync this::J2SProgram args)
   (with-access::J2SProgram this (nodes loc)
      (when (any (lambda (n)
		  (let ((cell (make-cell #f)))
		     (async? n cell)
		     (cell-ref cell)))
	     nodes)
	 (let* ((id (gensym 'async))
		(endloc loc)
		(fun (async-fun (J2SArrow id '() (J2SBlock* nodes)))))
	    (set! nodes (list (J2SCall fun))))))
   this)

;*---------------------------------------------------------------------*/
;*    async? ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (async? this::J2SNode cell)
   (or (cell-ref cell) (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    async? ::J2SYield ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (async? this::J2SYield cell)
   (with-access::J2SYield this (await)
      (when await
	 (cell-set! cell #t))))

;*---------------------------------------------------------------------*/
;*    async? ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (async? this::J2SFun cell)
   #f)

;*---------------------------------------------------------------------*/
;*    async-fun ...                                                    */
;*---------------------------------------------------------------------*/
(define (async-fun fun::J2SFun)
   (with-access::J2SFun fun (body mode thisp name)
      (with-access::J2SBlock body (loc endloc)
	 (let ((gen (instantiate::J2SFun
		       (thisp thisp)
		       (loc loc)
		       (src #f)
		       (generator #t)
		       (name (symbol-append name '*))
		       (mode 'strict)
		       (body body))))
	    (set! body
	       (J2SBlock
		  (J2SReturn #t
		     (J2SHopCall (J2SHopRef 'js-spawn)
			gen
			(J2SHopRef '%this)
			(J2SHopRef '%this)))))
	    fun))))
