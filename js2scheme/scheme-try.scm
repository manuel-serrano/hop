;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-try.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 25 07:00:50 2018                          */
;*    Last change :  Mon Jun 14 12:35:39 2021 (serrano)                */
;*    Copyright   :  2018-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript function calls              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-try

   (include "ast.sch"
	    "usage.sch"
	    "context.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2STry ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2STry mode return ctx)
   (with-access::J2STry this (loc body catch finally)
      (epairify-deep loc
	 (let* ((trybody (j2s-scheme body mode return ctx))
		(w-handler (if (need-unwind? body)
			       'with-handler
			       'js-with-handler-no-unwind))
		(trie (if (isa? catch J2SNop)
			  (j2s-scheme body mode return ctx)
			  (with-access::J2SCatch catch (loc param body)
			     (epairify-deep loc
				`(,w-handler
				    (lambda (,(j2s-scheme param mode return ctx))
				       ,(j2s-scheme body mode return ctx))
				    ,trybody))))))
	    (if (isa? finally J2SNop)
		trie
		`(unwind-protect
		    ,trie
		    ,(j2s-scheme finally mode return ctx)))))))

;*---------------------------------------------------------------------*/
;*    need-unwind? ...                                                 */
;*---------------------------------------------------------------------*/
(define (need-unwind? this::J2SNode)
   (let ((cell (make-cell #f)))
      (need-unwind this '() '() cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    need-unwind ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (need-unwind this::J2SNode bexits loops cell)
   (unless (cell-ref cell)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    need-unwind ::J2SReturn ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (need-unwind this::J2SReturn bexits loops cell)
   (unless (cell-ref cell)
      (with-access::J2SReturn this (tail from)
	 (unless (memq from bexits)
	    (cell-set! cell #t))
	 (unless (cell-ref cell)
	    (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    need-unwind ::J2SBreak ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (need-unwind this::J2SBreak bexists loops cell)
   (with-access::J2SBreak this (target)
      (unless (cell-ref cell)
	 (unless (memq target loops)
	    (cell-set! cell #t)))))
   
;*---------------------------------------------------------------------*/
;*    need-unwind ::J2SBindExit ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (need-unwind this::J2SBindExit bexits loops cell)
   (with-access::J2SBindExit this (stmt)
      (unless (cell-ref cell)
	 (need-unwind stmt (cons this bexits) loops cell))))

;*---------------------------------------------------------------------*/
;*    need-unwind ::J2SLoop ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (need-unwind this::J2SLoop bexits loops cell)
   (unless (cell-ref cell)
      (set! loops (cons this loops))
      (call-default-walker)))
