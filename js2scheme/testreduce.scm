;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/testreduce.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 21 06:50:42 2021                          */
;*    Last change :  Sun Nov 21 07:22:50 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Static tests reduction                                           */
;*    -------------------------------------------------------------    */
;*    This (very simple) optimization reduces test of IF and SWITCH    */
;*    construct when the test is statically known, which happens after */
;*    the previous optimizations such as globprop and globvar have     */
;*    been applied.                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_testreduce

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha
	   __js2scheme_use)

   (export j2s-testreduce-stage))

;*---------------------------------------------------------------------*/
;*    j2s-testreduce-stage ...                                         */
;*---------------------------------------------------------------------*/
(define j2s-testreduce-stage
   (instantiate::J2SStageProc
      (name "testreduce")
      (comment "Static test reduction")
      (proc testreduce!)
      (optional :optim-testreduce)))

;*---------------------------------------------------------------------*/
;*    testreduce! ::J2SProgram ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (testreduce! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    testreduce! ::J2SIf ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (testreduce! this::J2SIf args)
   (call-default-walker)
   (with-access::J2SIf this (test then else)
      (let ((cnst (toboolean (constant-value test 'dynamic))))
	 (cond
	    ((eq? cnst #t) then)
	    ((eq? cnst #f) else)
	    (else this)))))

;*---------------------------------------------------------------------*/
;*    testreduce! ::J2SSwitch ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (testreduce! this::J2SSwitch args)

   (define (cases->statement body::J2SStmt cases::pair-nil loc)
      ;; Transform a list of case test into a sequence of statements.
      ;; This function is called when reducing a switch, that is when
      ;; a case test has already been resolved.
      (J2SSeq*
	 (cons body
	    (let loop ((cases cases))
	       (cond
		  ((null? cases)
		   '())
		  ((isa? (car cases) J2SDefault)
		   (with-access::J2SCase (car cases) (body)
		      (list body)))
		  (else
		   (with-access::J2SCase (car cases) (body expr cascade)
		      (if cascade
			  (cons* (J2SStmtExpr expr) body (loop (cdr cases)))
			  (list body)))))))))

   (define (find-case cnst cases)
      (let loop ((cases cases))
	 (cond
	    ((null? cases)
	     #f)
	    ((isa? (car cases) J2SDefault)
	     #f)
	    (else
	     (with-access::J2SCase (car cases) (expr)
		(if (eq? (constant-value expr 'test) cnst)
		    cases
		    (loop (cdr cases))))))))
   
   (call-default-walker)
   (with-access::J2SSwitch this (key cases)
      (let ((cnst (constant-value key 'dynamic)))
	 (if (eq? cnst 'dynamic)
	     this
	     (let ((cases (find-case cnst cases)))
		(if (pair? cases)
		    (with-access::J2SCase (car cases) (body cascade loc)
		       (if cascade
			   (cases->statement body (cdr cases) loc)
			   body))
		    this))))))
		       
;*---------------------------------------------------------------------*/
;*    toboolean ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function is called to transform a JS static constant        */
;*    into a Scheme boolean.                                           */
;*---------------------------------------------------------------------*/
(define (toboolean val)
   (cond
      ((symbol? val) val)
      ((boolean? val) val)
      ((eq? val #unspecified) #f)
      ((null? val) #f)
      ((number? val) (not (= val 0)))
      ((equal? val "") #f)
      (else #t)))

;*---------------------------------------------------------------------*/
;*    constant-value ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (constant-value this::J2SExpr default)
   default)

;*---------------------------------------------------------------------*/
;*    constant-value ::J2SNull ...                                     */
;*---------------------------------------------------------------------*/
(define-method (constant-value this::J2SNull default)
   '())
      
;*---------------------------------------------------------------------*/
;*    constant-value ::J2SUndefined ...                                */
;*---------------------------------------------------------------------*/
(define-method (constant-value this::J2SUndefined default)
   #unspecified)
      
;*---------------------------------------------------------------------*/
;*    constant-value ::J2SLiteralValue ...                             */
;*---------------------------------------------------------------------*/
(define-method (constant-value this::J2SLiteralValue default)
   (with-access::J2SLiteralValue this (val)
      val))

