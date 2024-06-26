;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/globvar.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Mon May 20 06:58:59 2024 (serrano)                */
;*    Copyright   :  2017-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Global variables optimization (constant propagation).            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_globvar

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha
	   __js2scheme_use)

   (export j2s-globvar-stage))

;*---------------------------------------------------------------------*/
;*    j2s-globvar-stage ...                                            */
;*---------------------------------------------------------------------*/
(define j2s-globvar-stage
   (instantiate::J2SStageProc
      (name "globvar")
      (comment "Global variable initialization optimization")
      (proc j2s-globvar!)
      (optional :optim-globvar)))

;*---------------------------------------------------------------------*/
;*    j2s-globvar! ::J2SProgram ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-globvar! this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (unless direct-eval
	    (let ((gcnsts (collect-gloconst* this)))
	       (when (pair? gcnsts)
		  ;; propagate the constants
		  (propagate-constant! this)
		  (reinit-use-count! this))
	       (when (>=fx (config-get args :verbose 0) 3)
		  (display " " (current-error-port))
		  (fprintf (current-error-port) "(~(, ))"
		     (filter-map (lambda (g)
				    (with-access::J2SDecl g (%info id)
				       (when (and (pair? %info)
						  (eq? (car %info) 'init))
					  id)))
			gcnsts)))))))
   this)

;*---------------------------------------------------------------------*/
;*    constant? ...                                                    */
;*    -------------------------------------------------------------    */
;*    Used for initialization values (J2SInit and J2SDeclInit).        */
;*---------------------------------------------------------------------*/
(define (constant? expr::J2SExpr)
   (cond
      ((or (isa? expr J2SLiteralCnst)
	   (isa? expr J2SNativeString)
	   (isa? expr J2SString)
	   (isa? expr J2SBool)
	   (isa? expr J2SUndefined)
	   (isa? expr J2SNull)
	   (isa? expr J2SNumber))
       #t)
      ((isa? expr J2SRef)
       (with-access::J2SRef expr (decl)
	  (with-access::J2SDecl decl (writable)
	     (or (not writable) (not (decl-usage-has? decl '(assig uninit)))))))
      ((isa? expr J2SUnary)
       (with-access::J2SUnary expr (expr)
	  (constant? expr)))
      ((isa? expr J2SBinary)
       (with-access::J2SBinary expr (lhs rhs)
	  (and (constant? lhs) (constant? rhs))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SNode ...                                  */
;*    -------------------------------------------------------------    */
;*    Collect all the global variables that are initialized but        */
;*    never assigned.                                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SDecl ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SDecl)
   '())

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SInit ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SInit)
   (with-access::J2SInit this (lhs rhs)
      ;; no need to scan rhs as we are only looking for variable decls/inits
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (%info)
		(if (not (decl-usage-has? decl '(assig uninit)))
		    (begin
		       (if (constant? rhs)
			   (set! %info (cons 'init rhs))
			   (set! %info (cons 'init 'no-constant)))
		       (list decl))
		    '())))
	  '())))

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SDeclInit ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SDeclInit)
   (with-access::J2SDeclInit this (val %info %%dump id)
      (if (and (not (decl-usage-has? this '(assig uninit)))
	       (constant? val)
	       (or (not (pair? %info)) (not (eq? (car %info) 'init))))
	  (begin
	     (set! %info (cons 'init this))
	     (list this))
	  '())))

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SFun ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SFun)
   '())

;*---------------------------------------------------------------------*/
;*    propagate-constant! ::J2SNode ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-constant! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    propagage-constant! ::J2SRef ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-constant! this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info id export)
	 (cond
	    ((or (not (pair? %info)) (not (eq? (car %info) 'init)))
	     (call-default-walker))
	    ((eq? (cdr %info) 'no-constant)
	     (call-default-walker))
	    ((isa? (cdr %info) J2SExpr)
	     (j2s-alpha (propagate-constant! (cdr %info)) '() '()))
	    ((isa? decl J2SDeclInit)
	     (with-access::J2SDeclInit decl (val)
		;; copy the value
		(j2s-alpha (propagate-constant! val) '() '())))
	    (else
	     (call-default-walker))))))

;*---------------------------------------------------------------------*/
;*    propagate-constant! ::J2SInit ...                                */
;*    -------------------------------------------------------------    */
;*    Remove the variable initialization if it is bound to a           */
;*    constant that is inlined on all its use sites (which implies     */
;*    that the variable is not exported).                              */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-constant! this::J2SInit)
   (with-access::J2SInit this (lhs rhs loc)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (%info export)
		(if (and (pair? %info) (isa? (cdr %info) J2SExpr))
		    (if export
			(begin
			   (set! rhs (propagate-constant! rhs))
			   this)
			(J2SUndefined))
		    (call-default-walker))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    propagate-constant! ::J2Unary ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-constant! this::J2SUnary)
   (with-access::J2SUnary this (op expr)
      (if (and (eq? op 'delete) (isa? expr J2SRef))
	  this
	  (call-default-walker))))
