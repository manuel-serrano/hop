;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-bexit.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 19 08:59:11 2019                          */
;*    Last change :  Thu Nov 25 15:02:57 2021 (serrano)                */
;*    Copyright   :  2019-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of bind-exit forms.                       */
;*    -------------------------------------------------------------    */
;*    The detection of tails bind-exit does not improve speed but      */
;*    it reduces generated code size.                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-bexit

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-string
	   __js2scheme_scheme-regexp
	   __js2scheme_scheme-math
	   __js2scheme_scheme-json
	   __js2scheme_scheme-date
	   __js2scheme_scheme-array
	   __js2scheme_scheme-class
	   __js2scheme_scheme-ops
	   __js2scheme_scheme-arguments
	   __js2scheme_scheme-spread)

   (export (bindexit-cast ::J2SBindExit cast::J2SCast)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBindExit ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBindExit mode return conf)
   (with-access::J2SBindExit this (lbl stmt loc)
      (if (and lbl (not (all-return-tail? stmt this)))
	  (epairify loc
	     `(bind-exit (,lbl)
		 ,(j2s-scheme stmt mode return conf)))
	  (j2s-scheme stmt mode return conf))))

;*---------------------------------------------------------------------*/
;*    all-return-tail? ...                                             */
;*---------------------------------------------------------------------*/
(define (all-return-tail? this::J2SNode bexit::J2SBindExit)
   (let ((cell (make-cell #t)))
      (all-return-tail this bexit cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    all-return-tail ::J2SNode ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (all-return-tail this::J2SNode bexit::J2SBindExit cell)
   (when (cell-ref cell)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    all-return-tail ::J2SReturn ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (all-return-tail this::J2SReturn bexit::J2SBindExit cell)
   (with-access::J2SReturn this (tail from)
      (when (eq? from bexit)
	 (unless tail (cell-set! cell #f)))
      (when (cell-ref cell)
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    bindexit-cast ...                                                */
;*    -------------------------------------------------------------    */
;*    Propagate the cast to all return expressions.                    */
;*---------------------------------------------------------------------*/
(define (bindexit-cast this::J2SBindExit cast::J2SCast)
   (with-access::J2SBindExit this (stmt)
      (push-cast stmt this cast))
   this)

;*---------------------------------------------------------------------*/
;*    push-cast ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (push-cast this::J2SNode bexit::J2SBindExit cast::J2SCast)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    push-cast ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (push-cast this::J2SReturn bexit cast)
   (with-access::J2SReturn this (from expr)
      (call-default-walker)
      (when (eq? from bexit)
	 (set! expr 
	    (duplicate::J2SCast cast
	       (expr expr))))))
   
