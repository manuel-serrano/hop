;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/letfusion.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 27 13:16:54 2016                          */
;*    Last change :  Thu Mar  9 08:57:20 2017 (serrano)                */
;*    Copyright   :  2016-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Let fusion merges J2SBlock with variable decarations and         */
;*    J2SLetBlock.                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_letfusion

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer
	   __js2scheme_use)

   (export j2s-letfusion-stage))

;*---------------------------------------------------------------------*/
;*    j2s-letfusion-stage ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-letfusion-stage
   (instantiate::J2SStageProc
      (name "letfusion")
      (comment "Allocate let/const variables to registers")
      (proc j2s-letfusion!)
      (optional #f)))

;*---------------------------------------------------------------------*/
;*    j2s-letfusion! ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-letfusion! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-letfusion! ::J2SBlock ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-letfusion! this::J2SBlock args)
   (call-default-walker)
   (with-access::J2SBlock this (nodes)
      ;; check the pattern (j2sblock decl decl ... decl j2sletblock)
      (let loop ((lnodes nodes)
		 (vdecls '()))
	 (cond
	    ((null? lnodes)
	     this)
	    ((isa? (car lnodes) J2SDecl)
	     (loop (cdr lnodes) (cons (car lnodes) vdecls)))
	    ((isa? (car lnodes) J2SLetBlock)
	     (when (pair? vdecls)
		;; merge the decls into the j2sletblock
		(with-access::J2SLetBlock (car lnodes) (decls)
		   (set! decls (append (reverse! vdecls) decls))
		   (for-each (lambda (decl)
				(with-access::J2SDecl decl (scope)
				   (set! scope 'letblock)))
		      decls)))
	     (with-access::J2SLetBlock (car lnodes) (nodes)
		(set! nodes (append nodes (cdr lnodes))))
	     (car lnodes))
	    (else
	     this)))))

;*---------------------------------------------------------------------*/
;*    j2s-letfusion! ::J2SLetBlock ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-letfusion! this::J2SLetBlock args)
   (with-access::J2SLetBlock this (nodes decls)
      (map! (lambda (d) (walk! d args)) decls)
      (set! nodes (map! (lambda (n) (walk! n args)) nodes))
      this))
