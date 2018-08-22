;*=====================================================================*/
;*    .../prgm/project/hop/3.2.x-new-types/js2scheme/multivar.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 22 10:23:45 2018                          */
;*    Last change :  Wed Aug 22 12:05:17 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Multiple variable declaration split optimization.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_multivar
   
   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-multivar-stage))

;*---------------------------------------------------------------------*/
;*    j2s-multivar-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-multivar-stage
   (instantiate::J2SStageProc
      (name "multivar")
      (comment "Multiple variable declarations optimization")
      (proc (lambda (n args) (multivar! n args)))
      (optional :optim-multivar)))

;*---------------------------------------------------------------------*/
;*    multivar! ...                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (multivar! this::J2SNode conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    multivar! ::J2SBlock ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (multivar! this::J2SBlock conf)
   
   (define (collect-decl* nodes)
      (let loop ((nodes nodes)
		 (decls '()))
	 (cond
	    ((null? nodes)
	     decls)
	    ((isa? (car nodes) J2SDecl)
	     (loop (cdr nodes) (cons (car nodes) decls)))
	    (else
	     decls))))
   
   (with-access::J2SBlock this (nodes)
      (let* ((ds (collect-decl* nodes))
	     (vs (collect-multi* this #t ds)))
	 (if (pair? vs)
	     (begin
		(mark-captured this #f)
		(let ((vars (filter (lambda (d)
				       (with-access::J2SDecl d (%info) %info))
			       vs)))
		   (when (pair? vars)
		      (when (>=fx (config-get conf :verbose 0) 3)
			 (display " " (current-error-port))
			 (fprintf (current-error-port) "(~(, ))"
			    (map (lambda (g)
				    (with-access::J2SDecl g (id) id))
			       vars)))
		      (let ((nvars '()))
			 (for-each (lambda (decl)
				      (with-access::J2SDecl decl (%info)
					 (set! %info (cons* #f decl
							(map (lambda (d)
								(let ((nd (duplicate::J2SDecl d
									     (key (ast-decl-key)))))
								   (set! nvars (cons nd nvars))
								   nd))
							   (cdr %info))))))
			    vars)
			 (alpha! this)
			 (set! nodes (append nvars nodes))))
		   this))
	     (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    collect-multi* ::J2SNode ...                                     */
;*    -------------------------------------------------------------    */
;*    Collect the variables that are sequentially declared multiple    */
;*    times.                                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-multi* this::J2SNode inseq::bool decls::pair-nil)
   (set! inseq #f)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-multi* ::J2SSeq ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-multi* this::J2SSeq inseq decls)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-multi* ::J2SStmtExpr ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-multi* this::J2SStmtExpr inseq decls)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-multi* ::J2SDecl ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-multi* this::J2SDecl inseq decls::pair-nil)
   (with-access::J2SDecl this (%info)
      (set! %info '()))
   '())

;*---------------------------------------------------------------------*/
;*    collect-multi* ::J2SInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-multi* this::J2SInit inseq decls::pair-nil)
   (with-access::J2SInit this (lhs rhs)
      ;; no need to scan rhs as we are only looking for variable decls/inits
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (if (memq decl decls) 
		 (with-access::J2SDecl decl (%info)
		    (if (not inseq)
			(begin
			   (set! %info '())
			   '())
			(let ((o %info))
			   (set! %info (cons decl o))
			   (if (and (pair? o) (null? (cdr o)))
			       (list decl)
			       '()))))
		 '()))
	  '())))

;*---------------------------------------------------------------------*/
;*    mark-captured ::J2SNode ...                                      */
;*    -------------------------------------------------------------    */
;*    Mark all variables captured in a closure.                        */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-captured this::J2SNode inclo::bool)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-captured ::J2SRef ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-captured this::J2SRef inclo)
   (when inclo
      (with-access::J2SRef this (decl)
	 (with-access::J2SDecl decl (%info)
	    (set! %info #f))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-captured ::J2SFun ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-captured this::J2SFun inclo)
   (with-access::J2SFun this (body)
      (mark-captured body #t)))

;*---------------------------------------------------------------------*/
;*    alpha! ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (alpha! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    alpha! ::J2SInit ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (alpha! this::J2SInit)
   (with-access::J2SInit this (lhs)
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	 (when (isa? decl J2SDecl)
	    (with-access::J2SDecl decl (%info)
	       (when (pair? %info)
		  (set! %info (cdr %info)))))))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    alpha! ::J2SRef ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (alpha! this::J2SRef)
   (with-access::J2SRef this (decl)
      (when (isa? decl J2SDecl)
	  (with-access::J2SDecl decl (%info)
	     (when (pair? %info)
		(set! decl (car %info))))))
   (call-default-walker))


   
