;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/bestpractice.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Sun May 25 18:45:13 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Check strict mode best practice rules                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_bestpractice

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)

   (export j2s-bestpractice-stage
	   (generic j2s-bestpractice ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-bestpractice-stage ...                                       */
;*---------------------------------------------------------------------*/
(define j2s-bestpractice-stage
   (instantiate::J2SStageProc
      (name "bestpractice")
      (comment "Enforce strict mode best practice rules")
      (proc j2s-bestpractice)
      (optional #f)))

;*---------------------------------------------------------------------*/
;*    j2s-bestpractice ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (j2s-bestpractice this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-bestpractice ::J2SProgram ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-bestpractice this::J2SProgram args)
   (with-access::J2SProgram this (nodes mode)
      (for-each (lambda (n) (bestpractice n mode #f)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    bestpractice ::J2SNode ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (bestpractice this::J2SNode mode parent)
   (default-walk this mode this))

;*---------------------------------------------------------------------*/
;*    bestpractice ::J2SFun ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (bestpractice this::J2SFun mode parent)
   (with-access::J2SFun this (body id (fmode mode))
      (let ((mode (if (eq? mode 'strict) mode fmode)))
	 (if (isa? body J2SBlock)
	     (with-access::J2SBlock body (nodes)
		(for-each (lambda (n) (bestpractice n mode this)) nodes))
	     (bestpractice body mode this)))))

;*---------------------------------------------------------------------*/
;*    bestpractice ::J2SDeclFun ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (bestpractice this::J2SDeclFun mode parent)
   (when (and (eq? mode 'strict) parent (not (isa? parent J2SFun)))
      (with-access::J2SDeclFun this (id loc)
	 (raise
	    (instantiate::&io-parse-error
	       (proc "js-symbol")
	       (msg "In strict mode code, functions can only be declared at top level or immediately within another function")
	       (obj id)
	       (fname (cadr loc))
	       (location (caddr loc)))))))
