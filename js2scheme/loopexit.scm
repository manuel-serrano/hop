;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/loopexit.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 14:30:38 2013                          */
;*    Last change :  Wed Aug 10 12:05:04 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript Loopexit -> bind-exit                                 */
;*    -------------------------------------------------------------    */
;*    This module implements a walker that traverses the tree to       */
;*    find FOR and WHILE loops and SWITCH that don't need a bind-exit  */
;*    form.                                                            */
;*    -------------------------------------------------------------    */
;*    This is an optional stage                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_loopexit

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax)

   (export j2s-loopexit-stage
	   (generic j2s-loopexit ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-loopexit-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-loopexit-stage
   (instantiate::J2SStageProc
      (optional #f)
      (name "loopexit")
      (comment "Bind loop labels and mark loops that don't need bind-exit")
      (proc j2s-loopexit)))

;*---------------------------------------------------------------------*/
;*    j2s-loopexit ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (j2s-loopexit this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-loopexit ::J2SProgram ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-loopexit this::J2SProgram args)
   (with-access::J2SProgram this (nodes headers decls)
      (for-each (lambda (o) (mark-exit! o '() #f)) headers)
      (for-each (lambda (o) (mark-exit! o '() #f)) nodes)
      (for-each (lambda (o) (mark-exit! o '() #f)) decls))
   this)

;*---------------------------------------------------------------------*/
;*    mark-exit! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-exit! this::J2SNode targets::pair-nil label)
   (default-walk! this targets #f))

;*---------------------------------------------------------------------*/
;*    mark-exit! ::J2SLoop ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-exit! this::J2SLoop targets label)
   (with-access::J2SLoop this (body need-bind-exit-break need-bind-exit-continue id)
      (set! need-bind-exit-break #f)
      (set! need-bind-exit-continue #f)
      (set! id label)
      (set! body (walk! body (cons this targets) #f)))
   this)

;*---------------------------------------------------------------------*/
;*    mark-exit! ::J2SFor ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-exit! this::J2SFor targets label)
   (with-access::J2SFor this (init test incr)
      (set! init (walk! init targets #f))
      (set! test (walk! test targets #f))
      (set! incr (walk! incr targets #f))
      (call-next-method))
   this)

;*---------------------------------------------------------------------*/
;*    mark-exit! ::J2SForIn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-exit! this::J2SForIn targets label)
   (with-access::J2SForIn this (lhs obj)
      (set! lhs (walk! lhs targets #f))
      (set! obj (walk! obj targets #f))
      (call-next-method))
   this)

;*---------------------------------------------------------------------*/
;*    mark-exit! ::J2SWhile ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-exit! this::J2SWhile targets label)
   (with-access::J2SWhile this (test)
      (set! test (walk! test targets #f))
      (call-next-method))
   this)

;*---------------------------------------------------------------------*/
;*    mark-exit! ::J2SSwitch ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-exit! this::J2SSwitch targets label)
   (with-access::J2SSwitch this (key cases id need-bind-exit-break)
      (set! need-bind-exit-break #f)
      (set! id label)
      (set! key (walk! key targets #f))
      (let ((targets (cons this targets)))
	 (for-each (lambda (case::J2SCase)
		      (with-access::J2SCase case (expr body)
			 (set! body (walk! body targets #f))))
	    cases)))
   this)

;*---------------------------------------------------------------------*/
;*    mark-exit! ::J2SContinue ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-exit! this::J2SContinue targets label)
   (with-access::J2SContinue this (id loc target)
      (cond
	 (id
	  (let ((t (find-target id targets)))
	     (if t
		 (with-access::J2SLoop t (need-bind-exit-continue)
		    (set! target t)
		    (set! need-bind-exit-continue #t))
		 (raise
		    (instantiate::&io-parse-error
		       (proc "js-loopexit")
		       (msg (format "Unbound continue \"~a\"" id))
		       (obj (j2s->list this))
		       (fname (cadr loc))
		       (location (caddr loc)))))))
	 ((find (lambda (x) (isa? x J2SLoop)) targets)
	  ;; skip the "switch" stmt while handling "continue"
	  =>
	  (lambda (loop)
	     (with-access::J2SLoop loop (need-bind-exit-continue)
		(set! target loop)
		(set! need-bind-exit-continue #t))))
	 (else
	  (raise
	     (instantiate::&io-parse-error
		(proc "js-loopexit")
		(msg (format "Out of loop continue \"~a\"" id))
		(obj (j2s->list this))
		(fname (cadr loc))
		(location (caddr loc)))))))
   this)

;*---------------------------------------------------------------------*/
;*    mark-exit! ::J2SBreak ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-exit! this::J2SBreak targets label)
   (with-access::J2SBreak this (id loc target)
      (cond
	 ((null? targets)
	  (raise
	     (instantiate::&io-parse-error
		(proc "js-loopexit")
		(msg "Out of loop break")
		(obj (j2s->list this))
		(fname (cadr loc))
		(location (caddr loc)))))
	 (id
	  (let ((t (find-target id targets)))
	     (if t
		 (with-access::J2SIdStmt t (need-bind-exit-break)
		    (set! target t)
		    (set! need-bind-exit-break #t))
		 (raise
		    (instantiate::&io-parse-error
		       (proc "js-loopexit")
		       (msg (format "Unbound break \"~a\"" id))
		       (obj (j2s->list this))
		       (fname (cadr loc))
		       (location (caddr loc)))))))
	 (else
	  (set! target (car targets))
	  (with-access::J2SIdStmt (car targets) (need-bind-exit-break)
	     (set! need-bind-exit-break #t)))))
   this)

;*---------------------------------------------------------------------*/
;*    mark-exit! ::J2SLabel ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-exit! this::J2SLabel targets label)
   (with-access::J2SLabel this (body id)
      (walk! body (cons this targets) id))
   this)

;*---------------------------------------------------------------------*/
;*    mark-exit! ::J2SSeq ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-exit! this::J2SSeq targets label)
   (with-access::J2SSeq this (nodes)
      (let loop ((nodes nodes)
		 (label label))
	 (if (null? nodes)
	     this
	     (begin
		(walk! (car nodes) targets label)
		(loop (cdr nodes) #f))))))

;*---------------------------------------------------------------------*/
;*    mark-exit! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-exit! this::J2SFun targets label)
   (with-access::J2SFun this (body)
      (walk! body '() #f))
   this)

;*---------------------------------------------------------------------*/
;*    find-target ...                                                  */
;*---------------------------------------------------------------------*/
(define (find-target id env)
   (when (pair? env)
      (with-access::J2SIdStmt (car env) ((tid id))
	 (if (eq? tid id)
	     (car env)
	     (find-target id (cdr env))))))
