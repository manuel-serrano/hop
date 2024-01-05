;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/topfun.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 06:35:14 2015                          */
;*    Last change :  Tue Dec 19 05:19:42 2023 (serrano)                */
;*    Copyright   :  2015-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The topfun optimization lifts up the top-level function          */
;*    definitions. It transforms:                                      */
;*      var x = 3;                                                     */
;*      function f1() { ... }                                          */
;*      const z = 4;                                                   */
;*      function f2() { ...}                                           */
;*    into                                                             */
;*      function f1() { ... }                                          */
;*      function f2() { ...}                                           */
;*      var x = 3;                                                     */
;*      const z = 4;                                                   */
;*    This helps the Scheme compiler to avoid top-level closure        */
;*    allocations.                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_topfun

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer
	   __js2scheme_alpha
	   __js2scheme_usage
	   __js2scheme_freevars)

   (export j2s-topfun-stage))

;*---------------------------------------------------------------------*/
;*    j2s-topfun-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-topfun-stage
   (instantiate::J2SStageProc
      (name "topfun")
      (comment "Toplevel function declarations")
      (proc j2s-topfun)
      (optional :optim-topfun)))

;*---------------------------------------------------------------------*/
;*    j2s-topfun ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-topfun this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (decls)
	 (let loop ((all decls)
		    (funs '())
		    (nofuns '()))
	    (cond
	       ((null? all)
		(set! decls (append (reverse! funs) (reverse! nofuns))))
	       ((isa? (car all) J2SDeclFun)
		(loop (cdr all) (cons (car all) funs) nofuns))
	       (else
		(loop (cdr all) funs (cons (car all) nofuns)))))))
   this)

