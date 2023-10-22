;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-test.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:41:17 2017                          */
;*    Last change :  Sat Oct 21 12:52:59 2023 (serrano)                */
;*    Copyright   :  2017-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme test code generation                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-test

   (include "ast.sch" "context.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-cast)

   (export (j2s-test ::J2SExpr mode return conf)
	   (j2s-test-not ::J2SExpr mode return conf)
	   (j2s-totest ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-test ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-test test::J2SExpr mode return conf)
   (let ((ty (j2s-type test)))
      (cond
	 ((eq? ty 'bool)
	  (j2s-bool-test test mode return conf))
	 ((memq ty '(object array))
	  #t)
	 ((eq? ty 'int32)
	  `(not (=s32 ,(j2s-scheme test mode return conf) #s32:0)))
	 ((eq? ty 'uint32)
	  `(not (=u32 ,(j2s-scheme test mode return conf) #u32:0)))
	 ((is-fixnum? test conf)
	  `(not (=fx ,(j2s-scheme test mode return conf) 0)))
	 ((type-number? ty)
	  `(not (= ,(j2s-scheme test mode return conf) 0)))
	 ((eq? ty 'string)
	  `(js-jsstring-toboolean ,(j2s-scheme test mode return conf)))
	 ((notbool-expr? test)
	  (j2s-toboolean (j2s-scheme test mode return conf)))
	 (else
	  (with-access::J2SExpr test (hint)
	     (if (pair? (assq 'object hint))
		 `(js-totest-likely-object ,(j2s-scheme test mode return conf))
		 (j2s-totest (j2s-scheme test mode return conf) conf)))))))

;*---------------------------------------------------------------------*/
;*    j2s-test-not ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-test-not this::J2SExpr mode return conf)
   (js-not (j2s-bool-test this mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-totest ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function tries to push the totest conversion as deeply      */
;*    as possible to help the forthcoming register allocation.         */
;*---------------------------------------------------------------------*/
(define (j2s-totest expr ctx)
   (match-case expr
      ((js-regexp-prototype-exec ?rx ?arg ?%this)
       `(js-regexp-prototype-exec-as-bool ,rx ,arg ,%this))
      ((js-regexp-prototype-maybe-exec ?rx ?arg ?%this ?cache)
       `(js-regexp-prototype-maybe-exec-as-bool ,rx ,arg ,%this ,cache))
      ((js-jsstring-match-regexp-from-string ?obj ?arg ?rx ?%this)
       `(js-jsstring-match-regexp-from-string-as-bool ,obj ,arg ,rx ,%this))
      ((js-object-isfrozen ?a ?b)
       expr)
      ((js-has-own-property . ?-)
       expr)
      (((or let let*) ?- (js-has-own-property . ?-))
       expr)
      ((let ((?var ?-)) ((kwote or) (js-array? ?var) (js-proxy-array? ?var)))
       expr)
      ((let ?bindings ?body)
       `(let ,bindings
	   ,(match-case body
	       ((cond . ?clauses)
		`(cond
		    ,@(map (lambda (c) `(,(car c) ,(j2s-totest (cadr c) ctx)))
		       clauses)))
	       (else
		(j2s-totest body ctx)))))
      (else
       (if (config-get (context-conf ctx) :optim-inline 0)
	   `(js-totest-inline ,expr)
	   `(js-totest ,expr)))))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SNode ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SNode mode return conf)
   (j2s-scheme this mode return conf))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SExpr mode return conf)
   
   (define (bool-test type)
      (let ((sexp (j2s-scheme this mode return conf)))
	 (j2s-cast sexp this type 'bool conf)))
   
   (with-access::J2SExpr this (type)
      (cond
	 ((eq? type 'bool)
	  (j2s-scheme this mode return conf))
	 ((isa? this J2SBindExit)
	  (with-access::J2SBindExit this (stmt type)
	     (if (isa? stmt J2SIf)
		 (with-access::J2SIf stmt (then else)
		    (return-to-bool! then this)
		    (return-to-bool! else this)
		    (set! type 'bool)
		    (j2s-scheme this mode return conf))
		 (bool-test type))))
	 (else
	  (bool-test type)))))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SParen ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SParen mode return conf)
   (with-access::J2SParen this (expr)
      (j2s-bool-test expr mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SBinary ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SBinary mode return conf)
   (with-access::J2SBinary this (op lhs rhs loc)
      (case op
	 ((&&)
	  (let ((t1 (j2s-test lhs mode return conf)))
	     (cond
		((eq? t1 #t)
		 (j2s-test rhs mode return conf))
		((eq? t1 #f)
		 #f)
		(else
		 (epairify loc
		    `(and ,t1 ,(j2s-test rhs mode return conf)))))))
	 ((OR)
	  (let ((t1 (j2s-test lhs mode return conf)))
	     (cond
		((eq? t1 #t)
		 #t)
		((eq? t1 #f)
		 (j2s-test rhs mode return conf))
		(else
		 (epairify loc
		    `(or ,(j2s-test lhs mode return conf)
			 ,(j2s-test rhs mode return conf)))))))
	 ((OR*)
	  ;; special form used only for destructuing values
	  (let ((t1 (j2s-scheme lhs mode return conf)))
	     (cond
		((eq? t1 #t)
		 #t)
		((eq? t1 #f)
		 (j2s-test rhs mode return conf))
		(else
		 (let ((tmp (gensym '%or*)))
		    (epairify loc
		       `(let ((,tmp ,t1))
			   (if (eq? ,tmp (js-undefined))
			       ,(j2s-test rhs mode return conf)
			       ,tmp))))))))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SUnary ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SUnary mode return conf)
   (with-access::J2SUnary this (op expr loc)
      (if (eq? op '!)
	  (j2s-test-not expr mode return conf)
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    notbool-expr? ...                                                */
;*---------------------------------------------------------------------*/
(define (notbool-expr? this::J2SNode)
   (let ((ty (j2s-type this)))
      (and (symbol? ty) (not (eq? ty 'bool))
	   (not (eq? ty 'obj))
	   (not (eq? ty 'any)))))

;*---------------------------------------------------------------------*/
;*    j2s-toboolean ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-toboolean expr)
   (match-case expr
      (((or let let*) ?- (js-has-own-property . ?-))
       expr)
      (else
       `(js-toboolean ,expr))))

;*---------------------------------------------------------------------*/
;*    return-to-bool! ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (return-to-bool! this::J2SNode target)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    return-to-bool! ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (return-to-bool! this::J2SReturn target)
   (with-access::J2SReturn this (expr from)
      (if (eq? from target)
	  (let ((ty (j2s-type expr)))
	     (with-access::J2SExpr expr (loc)
		(unless (eq? ty 'bool)
		   (set! expr (J2SCast 'bool expr))))
	     this)
	  (call-default-walker))))
