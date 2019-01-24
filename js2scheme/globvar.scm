;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/globvar.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Thu Jan 24 13:46:36 2019 (serrano)                */
;*    Copyright   :  2017-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Global variables optimization (constant propagation).            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_globvar

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-globvar-stage))

;*---------------------------------------------------------------------*/
;*    j2s-globvar-stage ...                                            */
;*---------------------------------------------------------------------*/
(define j2s-globvar-stage
   (instantiate::J2SStageProc
      (name "globvar")
      (comment "Global variable initialization optimization")
      (proc j2s-globvar!)
      (optional 2)))

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
		  (propagate-constant! this))
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
	  (with-access::J2SDecl decl (ronly writable usage)
	     (or ronly (not writable) (not (usage? '(assig uninit) usage))))))
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
	     (with-access::J2SDecl decl (usage ronly val %info id)
		(if (and (not (usage? '(assig uninit) usage)) (constant? rhs))
		    (begin
		       (set! %info (cons 'init rhs))
		       (list decl))
		    '())))
	  '())))

;*---------------------------------------------------------------------*/
;*    collect-gloconst* ::J2SDeclInit ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-gloconst* this::J2SDeclInit)
   (with-access::J2SDeclInit this (usage ronly val %info %%dump id)
      (if (and ronly (not (usage? '(assig) usage)) (constant? val))
	  (begin
	     (set! %%dump this)
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
      (with-access::J2SDecl decl (%info id)
	 (cond
	    ((or (not (pair? %info)) (not (eq? (car %info) 'init)))
	     (call-default-walker))
	    ((isa? (cdr %info) J2SExpr)
	     (j2s-alpha (propagate-constant! (cdr %info)) '() '()))
	    ((isa? decl J2SDeclInit)
	     (with-access::J2SDeclInit decl (val usecnt)
		(set! usecnt (-fx usecnt 1))
		;; copy the value
		(j2s-alpha (propagate-constant! val) '() '())))
	    
	    (else
	     (call-default-walker))))))

;*---------------------------------------------------------------------*/
;*    propagate-constant! ::J2SInit ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-constant! this::J2SInit)
   (with-access::J2SInit this (lhs rhs loc)
      (if (and (isa? lhs J2SRef)
	       (with-access::J2SRef lhs (decl)
		  (with-access::J2SDecl decl (%info)
		     (and (pair? %info) (isa? (cdr %info) J2SExpr)))))
	  (J2SUndefined)
	  (call-default-walker))))
