;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-ops.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:21:19 2017                          */
;*    Last change :  Tue Oct 17 15:36:00 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Unary and binary Scheme code generation                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-ops

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-test)

   (export (j2s-in? loc id obj)
	   (j2s-num-op op left right lhs::J2SNode rhs::J2SNode conf)
	   (js-binop2 loc op::symbol type lhs::J2SNode rhs::J2SNode
	      mode return conf hint::pair-nil totype)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUnary ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUnary mode return conf hint totype)
   
   (define (err id)
      (with-access::J2SUnary this (loc)
	 (match-case loc
	    ((at ?fname ?loc)
	     `(with-access::JsGlobalObject %this (js-syntax-error)
		 (js-raise
		    (js-new %this js-syntax-error
		       ,(j2s-jsstring
			   (format "Delete of an unqualified identifier in strict mode: \"~a\"" id)
			   loc)
		       ,fname ,loc))))
	    (else
	     `(with-access::JsGlobalObject %this (js-syntax-error)
		 (js-raise
		    (js-new %this js-syntax-error
		       ,(j2s-jsstring
			   (format "Delete of an unqualified identifier in strict mode: \"~a\"" id)
			   loc))))))))
   
   (define (delete->scheme expr)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.7
      (cond
	 ((isa? expr J2SWithRef)
	  (with-access::J2SWithRef expr (id withs expr loc)
	     (let loop ((withs withs))
		(if (null? withs)
		    `(begin ,(j2s-scheme expr mode return conf hint totype) #f)
		    `(if ,(j2s-in? loc `',id (car withs))
			 (js-delete! ,(j2s-scheme (car withs) mode return conf hint totype)
			    ',(j2s-scheme id mode return conf hint totype)
			    #f
			    %this)
			 ,(loop (cdr withs)))))))
	 ((isa? expr J2SAccess)
	  (with-access::J2SAccess expr (obj field)
	     `(js-delete! ,(j2s-scheme obj mode return conf hint totype)
		 ,(j2s-scheme field mode return conf hint totype)
		 ,(strict-mode? mode)
		 %this)))
	 ((isa? expr J2SUnresolvedRef)
	  (if (strict-mode? mode)
	      (with-access::J2SUnresolvedRef expr (id)
		 (err id))
	      (with-access::J2SUnresolvedRef expr (id)
		 `(js-delete! ,j2s-unresolved-del-workspace ',id #f %this))))
	 ((and (isa? expr J2SRef) (not (isa? expr J2SThis)))
	  (if (strict-mode? mode)
	      (with-access::J2SRef expr (decl)
		 (with-access::J2SDecl decl (id)
		    (err id)))
	      '(begin #f)))
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (delete->scheme expr)))
	 (else
	  `(begin ,(j2s-scheme expr mode return conf hint totype) #t))))

   (define (typeof->scheme expr)
      (cond
	 ((isa? expr J2SUnresolvedRef)
	  ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.3
	  (with-access::J2SUnresolvedRef expr (id loc cache)
	     `(js-typeof ,(j2s-unresolved id cache #f))))
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (typeof->scheme expr)))
	 (else
	  `(js-typeof ,(j2s-scheme expr mode return conf hint totype)))))
   
   (with-access::J2SUnary this (loc expr op type)
      (case op
	 ((!)
	  (epairify loc
	     `(if ,(j2s-test expr mode return conf) #f #t)))
	 ((typeof)
	  (epairify loc
	     (typeof->scheme expr)))
	 ((void)
	  (epairify loc
	     `(begin
		 ,(j2s-scheme expr mode return conf hint totype)
		 (js-undefined))))
	 ((delete)
	  (epairify loc
	     (delete->scheme expr)))
	 ((+)
	  ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.6
	  (let ((expr (j2s-scheme expr mode return conf hint totype)))
	     (if (eqv? expr 0)
		 +0.0
		 (epairify loc
		    `(js-tonumber ,expr %this)))))
	 ((-)
	  ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.7
	  (let ((expr (j2s-scheme expr mode return conf hint totype))
		(typ (j2s-type expr)))
	     (cond
		((eqv? expr 0)
		 -0.0)
		((number? expr)
		 (let ((n (j2s-number (- expr) conf)))
		    (if (pair? n)
			(epairify loc n)
			n)))
		((and (type-uint32? typ) (type-integer? type))
		 (epairify loc
		    `(negu32 ,expr)))
		((and (type-int30? typ) (type-int30? type))
		 (epairify loc
		    `(negfx ,expr)))
		((and (type-int53? typ) (type-int53? type) (m64? conf))
		 (epairify loc
		    `(negfx ,expr)))
		((and (type-integer? typ) (type-integer? type) (m64? conf))
		 (epairify loc
		    `(negfx ,expr)))
		(else
		 (epairify loc
		    `(js-neg ,expr %this))))))
	 ((~)
	  ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.8
	  (epairify loc
	     `(js-bitnot ,(j2s-scheme expr mode return conf hint totype) %this)))
	 (else
	  (epairify loc
	     `(,op ,(j2s-scheme expr mode return conf hint totype)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBinary mode return conf hint totype)
   (with-access::J2SBinary this (loc op lhs rhs type hint)
      (epairify-deep loc
	 (js-binop2 loc op type lhs rhs mode return conf hint type))))

;*---------------------------------------------------------------------*/
;*    scm-fixnum? ...                                                  */
;*---------------------------------------------------------------------*/
(define (scm-fixnum? sexp node::J2SNode)
   (cond
      ((fixnum? sexp) #t)
      ((type-int30? (j2s-type node)) #t)
      ((symbol? sexp) `(fixnum? ,sexp))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    scm-and ...                                                      */
;*---------------------------------------------------------------------*/
(define (scm-and left right)

   (define (fixnum-test e)
      (match-case e
	 ((fixnum? ?e) e)
	 (else #f)))

   (cond
      ((eq? left #t)
       (if (eq? right #t) #t right))
      ((eq? right #t)
       left)
      ((and (fixnum-test left) (fixnum-test right))
       `(fixnums? ,(fixnum-test left) ,(fixnum-test right)))
      ((and left right)
       `(and ,left ,right))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    scm-if ...                                                       */
;*---------------------------------------------------------------------*/
(define (scm-if test then otherwise)
   (cond
      ((eq? test #t) then)
      ((> (bigloo-debug) 0) otherwise)
      (else `(if ,test ,then ,otherwise))))

;*---------------------------------------------------------------------*/
;*    js-binop2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-binop2 loc op::symbol type lhs::J2SNode rhs::J2SNode
	   mode return conf hint::pair-nil totype)
   (case op
      ((+ - *)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left right)))
	   (js-arithmetic loc op type lhs rhs mode return conf hint type)))
      ((== === != !==)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left right)))
	   (js-equality loc op type lhs rhs mode return conf hint totype)))
      ((< <= > >=)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left right)))
	   (js-cmp loc op lhs rhs mode return conf hint totype)))
      ((& ^ BIT_OR >> >>> <<)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left right)))
	   (js-bitop loc op type lhs rhs mode return conf hint type)))
      ((%)
       (cond
	  ((=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left right))))
	  ((and (is-uint32? lhs) (is-uint32? rhs) (type-uint32? type) (u32? conf))
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (if (and (number? right) (= right 0))
		     +nan.0
		     `(js-%u32 ,left ,right)))))
	  ((and (eq? (j2s-type lhs) 'integer) (eq? (j2s-type rhs) 'integer))
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (if (and (number? right) (= right 0))
		     +nan.0
		     `(js-%$$NN ,left ,right)))))
	  ((and (is-number? lhs) (is-number? rhs))
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (if (and (number? right) (not (= right 0)))
		     `(js-%$$NZ ,left ,right)
		     `(js-%$$NN ,left ,right)))))
	  (else
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left right))))))
      ((remainderfx remainder)
       (binop lhs rhs mode return conf hint 'any
	  (lambda (left right)
	     `(,op ,left ,right))))
      
      ((/)
       (binop lhs rhs mode return conf hint 'any
	  (lambda (left right)
	     (js-binop loc op left right))))
      ((OR)
       (let ((lhsv (gensym 'lhs)))
	  `(let ((,lhsv ,(j2s-scheme lhs mode return conf hint totype)))
	      (if ,(if (eq? (j2s-type lhs) 'bool) lhsv `(js-totest ,lhsv))
		  ,lhsv
		  ,(j2s-scheme rhs mode return conf hint totype)))))
      ((&&)
       (let ((lhsv (gensym 'lhs)))
	  `(let ((,lhsv ,(j2s-scheme lhs mode return conf hint totype)))
	      (if ,(if (eq? (j2s-type lhs) 'bool) lhsv `(js-totest ,lhsv))
		  ,(j2s-scheme rhs mode return conf hint totype)
		  ,lhsv))))
      (else
       (binop lhs rhs mode return conf hint 'any
	  (lambda (left right)
	     (js-binop loc op left right))))))

;*---------------------------------------------------------------------*/
;*    j2s-in? ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-in? loc id obj)
   (if (> (bigloo-debug) 0)
       `(js-in?/debug %this ',loc ,id ,obj)
       `(js-in? %this ,id ,obj)))

;*---------------------------------------------------------------------*/
;*    j2s-num-op ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function is called with left and right being either         */
;*    atoms or variable references. Hence it does not generate         */
;*    bindings.                                                        */
;*    -------------------------------------------------------------    */
;*    WARNING ! The LHS and RHS arguments can only be used for         */
;*    there types.                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-num-op op left right lhs::J2SNode rhs::J2SNode conf)
   
   (define (fx op)
      (case op
	 ((+ -)
	  (case (config-get conf :long-size 0)
	     ((32)
	      (symbol-append 'js op 'fx32))
	     ((64)
	      (symbol-append 'js op 'fx64))
	     (else
	      (symbol-append 'js op 'fx))))
	 ((/)
	  (symbol-append 'js op 'fx))
	 (else
	  (symbol-append op 'fx))))
   
   (cond
      ((fixnum? left)
       (cond
	  ((fixnum? right)
	   `(,(fx op) ,left ,right))
	  ((number? right)
	   `(,op ,left ,right))
	  (else
	   `(if (fixnum? ,right)
		(,(fx op) ,left ,right)
		(,op ,left ,right)))))
      ((number? left)
       `(,op ,left ,right))
      ((fixnum? right)
       `(if (fixnum? ,left)
	    (,(fx op) ,left ,right)
	    (,op ,left ,right)))
      ((number? right)
       `(,op ,left ,right))
      (else
       (scm-if (scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
	  `(,(fx op) ,left ,right)
	  `(,op ,left ,right)))))

;*---------------------------------------------------------------------*/
;*    js-binop ...                                                     */
;*    -------------------------------------------------------------    */
;*    This function is called with left and right being either         */
;*    atoms or variable references. Hence it does not generate         */
;*    bindings.                                                        */
;*---------------------------------------------------------------------*/
(define (js-binop loc op lhs rhs)
   (case op
      ((+)
       `(js+ ,lhs ,rhs %this))
      ((-)
       `(js- ,lhs ,rhs %this))
      ((*)
       `(js* ,lhs ,rhs %this))
      ((/)
       `(js/ ,lhs ,rhs %this))
      ((%)
       `(js% ,lhs ,rhs %this))
      ((<)
       `(js< ,lhs ,rhs %this))
      ((<=)
       `(js<= ,lhs ,rhs %this))
      ((>)
       `(js> ,lhs ,rhs %this))
      ((>=)
       `(js>= ,lhs ,rhs %this))
      ((==)
       `(js-equal? ,lhs ,rhs %this))
      ((!=)
       `(not (js-equal? ,lhs ,rhs %this)))
      ((===)
       `(js-strict-equal? ,lhs ,rhs))
      ((!==)
       `(not (js-strict-equal? ,lhs ,rhs)))
      ((eq?)
       `(eq? ,lhs ,rhs))
      ((!eq?)
       `(not (eq? ,lhs ,rhs)))
      ((eqil?)
       `(js-eqil? ,lhs ,rhs))
      ((eqir?)
       `(js-eqir? ,lhs ,rhs))
      ((!eqil?)
       `(not (js-eqil? ,lhs ,rhs)))
      ((!eqir?)
       `(not (js-eqir? ,lhs ,rhs)))
      ((<-)
       `(js<- ,lhs ,rhs %this))
      ((instanceof)
       (if (> (bigloo-debug) 0)
	   `(js-instanceof?/debug %this ',loc ,lhs ,rhs)
	   `(js-instanceof? %this ,lhs ,rhs)))
      ((in)
       (j2s-in? loc lhs rhs))
      ((&)
       `(js-bitand ,lhs ,rhs %this))
      ((BIT_OR)
       `(js-bitor ,lhs ,rhs %this))
      ((^)
       `(js-bitxor ,lhs ,rhs %this))
      ((>>)
       `(js-bitrsh ,lhs ,rhs %this))
      ((>>>)
       `(js-bitursh ,lhs ,rhs %this))
      ((<<)
       `(js-bitlsh ,lhs ,rhs %this))
      ((OR &&)
       (error "binop" "should not be here" op))
      (else
       `(,op ,lhs ,rhs %this))))

;*---------------------------------------------------------------------*/
;*    binop ...                                                        */
;*---------------------------------------------------------------------*/
(define (binop lhs rhs mode return conf hint::pair-nil optype gen::procedure)

   (define (simple? expr)
      (cond
	 ((isa? expr J2SRef)
	  #t)
	 ((isa? expr J2SLiteral)
	  #t)
	 ((isa? expr J2SBinary)
	  (with-access::J2SBinary expr (lhs rhs)
	     (and (simple? lhs) (simple? rhs))))
	 ((isa? expr J2SUnary)
	  (with-access::J2SUnary expr (expr)
	     (simple? expr)))
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (simple? expr)))
	 ((isa? expr J2SCast)
	  (with-access::J2SCast expr (expr)
	     (simple? expr)))
	 (else
	  #f)))
   
   (define (atom? expr)
      (or (number? expr)
	  (string? expr)
	  (boolean? expr)
	  (equal? expr '(js-undefined))
	  (equal? expr '(js-null))
	  (match-case expr
	     ((js-ascii->jsstring (? string?)) #t)
	     ((js-utf8->jsstring (? string?)) #t)
	     (else #f))))
   
   (let* ((scmlhs (j2s-scheme lhs mode return conf hint optype))
	  (scmrhs (j2s-scheme rhs mode return conf hint optype))
	  (testl (or (atom? scmlhs) (and (symbol? scmlhs) (simple? rhs))))
	  (testr (or (atom? scmrhs) (and (symbol? scmrhs) (simple? lhs)))))
      (cond
	 ((and testl testr)
	  (gen scmlhs scmrhs))
	 (testl
	  (let ((right (gensym 'right)))
	     `(let ((,(utype-ident right (j2s-type rhs) conf #t) ,scmrhs))
		 ,(gen scmlhs right))))
	 (testr
	  (let ((left (gensym 'left)))
	     `(let ((,(utype-ident left (j2s-type lhs) conf #t) ,scmlhs))
		 ,(gen left scmrhs))))
	 (else
	  (let ((left (gensym 'left))
		(right (gensym 'right)))
	     `(let* ((,(utype-ident left (j2s-type lhs) conf #t) ,scmlhs)
		     (,(utype-ident right (j2s-type rhs) conf #t) ,scmrhs))
		 ,(gen left right)))))))

;*---------------------------------------------------------------------*/
;*    js-cmp ...                                                       */
;*    -------------------------------------------------------------    */
;*    The compilation of the comparison functions.                     */
;*---------------------------------------------------------------------*/
(define (js-cmp loc op::symbol lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)

   (define (u32op op) (symbol-append op 'u32))
   (define (fxop op) (symbol-append op 'fx))
   (define (strop op) (symbol-append 'js-jsstring op '?))
   
   (cond
      ((and (is-uint32? lhs) (is-uint32? rhs) (u32? conf))
       (binop lhs rhs mode return conf hint 'uint32
	  (lambda (left right)
	     `(,(u32op op) ,left ,right))))
      ((and (is-int30? lhs) (is-int30? rhs))
       (binop lhs rhs mode return conf hint 'int30
	  (lambda (left right)
	     `(,(fxop op) ,left ,right))))
      ((and (is-int53? lhs) (is-int53? rhs) (m64? conf))
       (binop lhs rhs mode return conf hint 'int53
	  (lambda (left right)
	     `(,(fxop op) ,left ,right))))
      ((and (is-integer? lhs) (is-integer? rhs))
       (cond
	  ((m64? conf)
	   (binop lhs rhs mode return conf hint 'int53
	      (lambda (left right)
		 `(,(fxop op) ,left ,right))))
	  ((and (maybe-number? lhs) (maybe-number? rhs))
	   (binop lhs rhs mode return conf hint 'integer
	      (lambda (left right)
		 (scm-if (scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		    `(,(fxop op) ,left ,right)
		    `(,op ,left ,right)))))
	  (else
	   (binop lhs rhs mode return conf '(fixnum) 'integer
	      (lambda (left right)
		 `(,op ,left ,right))))))
      ((and (is-string? lhs) (is-string? rhs))
       (binop lhs rhs mode return conf '(string) 'string
	  (lambda (left right)
	     `(,(strop op) ,left ,right))))
      ((memq 'integer hint)
       (binop lhs rhs mode return conf hint 'integer
	  (lambda (left right)
	     (scm-if (scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		`(,(fxop op) ,left ,right)
		(js-binop loc op left right)))))
      ((or (eq? (j2s-type lhs) 'number) (eq? (j2s-type rhs) 'number))
       (binop lhs rhs mode return conf hint 'integer
	  (lambda (left right)
	     (scm-if (scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		`(,(fxop op) ,left ,right)
		(js-binop loc op left right)))))
      ((or (eq? (j2s-type lhs) 'any) (eq? (j2s-type rhs) 'any))
       (if (and (maybe-number? lhs) (maybe-number? rhs))
	   (binop lhs rhs mode return conf hint 'integer
	      (lambda (left right)
		 (scm-if (scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		    `(,(fxop op) ,left ,right)
		    (js-binop loc op left right))))
	   (binop lhs rhs mode return conf '(number) 'any
	      (lambda (left right)
		 (js-binop loc op left right)))))
      (else
       (binop lhs rhs mode return conf '(number) 'any
	  (lambda (left right)
	     (js-binop loc op left right))))))

;*---------------------------------------------------------------------*/
;*    js-equality ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-equality loc op::symbol type lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)

   (define (is-fixnum/conf? expr)
      (is-fixnum? expr conf))

   (define (j2s-aref-length? expr::J2SExpr)
      (when (isa? expr J2SAccess)
	 (with-access::J2SAccess expr (obj field)
	    (when (j2s-field-length? field)
	       (isa? obj J2SAref)))))
   
   (define (j2s-cast-aref-length? expr::J2SExpr)
      (when (isa? expr J2SCast)
	 (with-access::J2SCast expr (expr)
	    (j2s-aref-length? expr))))

   (define (cast-aref::J2SAref expr::J2SCast)
      (when (isa? expr J2SCast)
	 (with-access::J2SCast expr (expr)
	    (aref expr))))
   
   (define (aref::J2SAref expr::J2SAccess)
      (with-access::J2SAccess expr (obj field)
	 obj))

   (define (j2s-typeof-predicate this::J2SExpr expr)
      (when (isa? this J2SUnary)
	 (with-access::J2SUnary this (op)
	    (when (eq? op 'typeof)
	       (when (or (isa? expr J2SString) (isa? expr J2SNativeString))
		  (with-access::J2SLiteralValue expr (val)
		     (cond
			((string=? val "number") 'js-number?)
			((string=? val "function") 'js-function?)
			((string=? val "string") 'js-jsstring?)
			((string=? val "undefined") 'js-undefined?)
			((string=? val "boolean") 'boolean?)
			((string=? val "object") #f)
			(else (tprint "TYPEOF PAS OPT " val) #f))))))))

   (define (typeof-expr expr mode return conf hint totype)
      (cond
	 ((isa? expr J2SUnresolvedRef)
	  ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.3
	  (with-access::J2SUnresolvedRef expr (id loc cache)
	     (j2s-unresolved id cache #f)))
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (typeof-expr expr mode return conf hint totype)))
	 (else
	  (j2s-scheme expr mode return conf hint totype))))

   (cond
      ((j2s-typeof-predicate lhs rhs)
       =>
       (lambda (pred)
	  (with-access::J2SUnary lhs (expr)
	     (let ((t `(,pred ,(typeof-expr expr mode return conf hint totype))))
		(if (memq op '(!= !==))
		    `(not ,t)
		    t)))))
      ((j2s-typeof-predicate rhs lhs)
       =>
       (lambda (pred)
	  (with-access::J2SUnary rhs (expr)
	     (let ((t `(,pred ,(typeof-expr expr mode return conf hint totype))))
		(if (memq op '(!= !==))
		    `(not ,t)
		    t)))))
      ((and (is-uint32? lhs) (is-uint32? rhs))
       (cond
	  ((j2s-aref-length? rhs)
	   (with-access::J2SAref (aref rhs) (field alen)
	      (let ((test `(or (=u32 %lhs ,(j2s-decl-scheme-id alen))
			       (=u32 %lhs ,(j2s-scheme rhs mode return conf '(fixnum) 'any)))))
		 `(let ((%lhs ,(j2s-scheme lhs mode return conf '(fixnum) 'any)))
		     ,(if (memq op '(!= !==))
			  (js-not test)
			  test)))))
	  ((j2s-aref-length? lhs)
	   (with-access::J2SAref (aref lhs) (field alen)
	      (let ((test `(or (=u32 ,(j2s-decl-scheme-id alen) %rhs)
			       (=u32 ,(j2s-scheme rhs mode return conf '(fixnum) 'any)
				  %rhs))))
		 `(let ((%rhs ,(j2s-scheme rhs mode return conf '(fixnum) 'any)))
		     ,(if (memq op '(!= !==))
			  (js-not test)
			  test)))))
	  (else
	   (let ((res `(=u32 ,(j2s-scheme lhs mode return conf '(fixnum) 'any)
			  ,(j2s-scheme rhs mode return conf '(fixnum) 'any))))
	      (if (memq op '(!= !==))
		  (js-not res)
		  res)))))
      ((and (is-fixnum/conf? lhs) (is-fixnum/conf? rhs))
       (cond
	  ((j2s-cast-aref-length? rhs)
	   (with-access::J2SAref (cast-aref rhs) (field alen)
	      (let ((test `(or (=fx %lhs ,(j2s-decl-scheme-id alen))
			       (=fx %lhs ,(j2s-scheme rhs mode return conf '(fixnum) 'any)))))
		 `(let ((%lhs ,(j2s-scheme lhs mode return conf '(fixnum) 'any)))
		     ,(if (memq op '(!= !==))
			  (js-not test)
			  test)))))
	  ((j2s-cast-aref-length? lhs)
	   (with-access::J2SAref (cast-aref lhs) (field alen)
	      (let ((test `(or (=fx ,(j2s-decl-scheme-id alen) %rhs)
			       (=fx ,(j2s-scheme rhs mode return conf '(fixnum) 'any)
				  %rhs))))
		 `(let ((%rhs ,(j2s-scheme rhs mode return conf '(fixnum) 'any)))
		     ,(if (memq op '(!= !==))
			  (js-not test)
			  test)))))
	  (else
	   (let ((res (binop lhs rhs mode return conf '(fixnum) 'any
			 (lambda (left right)
			    `(=fx ,left ,right)))))
	      (if (memq op '(!= !==))
		  (js-not res)
		  res)))))
      ((and (is-number? lhs) (is-number? rhs))
       (cond
	  ((j2s-cast-aref-length? rhs)
	   (with-access::J2SAref (cast-aref rhs) (field alen)
	      (let ((test `(or (= %lhs ,(j2s-decl-scheme-id alen))
			       (= %lhs ,(j2s-scheme rhs mode return conf '(number) 'any)))))
		 `(let ((%lhs ,(j2s-scheme lhs mode return conf '(number) 'any)))
		     ,(if (memq op '(!= !==))
			  (js-not test)
			  test)))))
	  ((j2s-cast-aref-length? lhs)
	   (with-access::J2SAref (cast-aref lhs) (field alen)
	      (let ((test `(or (= ,(j2s-decl-scheme-id alen) %rhs)
			       (= ,(j2s-scheme rhs mode return conf '(number) 'any)
				  %rhs))))
		 `(let ((%rhs ,(j2s-scheme rhs mode return conf '(number) 'any)))
		     ,(if (memq op '(!= !==))
			  (js-not test)
			  test)))))
	  (else
	   (let ((res (binop lhs rhs mode return conf hint 'any
			 (lambda (left right)
			    (j2s-num-op '= left right lhs rhs conf)))))
	      (if (memq op '(!= !==))
		  (js-not res)
		  res)))))
      ((and (eq? (j2s-type lhs) 'string) (eq? (j2s-type rhs) 'string))
       (binop lhs rhs mode return conf '(string) 'any
	  (lambda (left right)
	     (js-binop loc op left right))))
      ((and (eq? (j2s-type lhs) 'bool) (eq? (j2s-type rhs) 'bool))
       (binop lhs rhs mode return conf '(bool) 'any
	  (lambda (left right)
	     (if (memq op '(!= !==))
		 `(not (eq? ,left ,right))
		 `(eq? ,left ,right)))))
      ((and (memq op '(== !=))
	    (or (memq (j2s-type lhs) '(bool string object array))
		(memq (j2s-type rhs) '(bool string object array))))
       (binop lhs rhs mode return conf '(bool) 'any
	  (lambda (left right)
	     (if (eq? op '!=)
		 `(not (js-equal-sans-flonum? ,left ,right %this))
		 `(js-equal-sans-flonum? ,left ,right %this)))))
      ((or (memq (j2s-type lhs) '(undefined null))
	   (memq (j2s-type rhs) '(undefined null)))
       (binop lhs rhs mode return conf '(bool) 'any
	  (lambda (left right)
	     (case op
		((!==)
		 `(not (eq? ,left ,right)))
		((===)
		 `(eq? ,left ,right))
		((==)
		 (if (memq (j2s-type lhs) '(undefined null))
		     `(or (eq? (js-undefined) ,right) (eq? (js-null) ,right))
		     `(or (eq? ,left (js-undefined)) (eq? ,left (js-null)))))
		((!=)
		 (if (memq (j2s-type lhs) '(undefined null))
		     `(not (or (eq? (js-undefined) ,right) (eq? (js-null) ,right)))
		     `(not (or (eq? ,left (js-undefined)) (eq? ,left (js-null))))))
		(else
		 (js-binop loc op left right))))))
      ((and (or (eq? op '===) (eq? op '!==))
	    (or (eq? (j2s-type lhs) 'bool) (eq? (j2s-type rhs) 'bool)))
       (binop lhs rhs mode return conf '(bool) 'any
	  (lambda (left right)
	     (if (eq? op '!==)
		 `(not (eq? ,left ,right))
		 `(eq? ,left ,right)))))
      (else
       (binop lhs rhs mode return conf hint 'any
	  (lambda (left right)
	     (let ((op (cond
			  ((not (memq 'integer hint))
			   op)
			  ((is-fixnum/conf? lhs)
			   (if (memq op '(== ===)) 'eqil? '!eqil?))
			  ((is-fixnum/conf? rhs)
			   (if (memq op '(== ===)) 'eqir? '!eqir?))
			  (else
			   op))))
		(js-binop loc op left right)))))))

;*---------------------------------------------------------------------*/
;*    js-bitop ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-bitop loc op::symbol type lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)
   
   (define (fxop op)
      (case op
	 ((&) 'bit-ands32)
	 ((BIT_OR) 'bit-ors32)
	 ((^) 'bit-xors32)
	 ((>>) 'bit-rshs32)
	 ((>>>) 'bit-urshs32)
	 ((<<) 'bit-lshs32)
	 (else (error "js-bitop" "unknown operator" op))))
   
   (define (fx->int32 val)
      (if (fixnum? val)
	  (fixnum->int32 val)
	  `(fixnum->int32 ,val)))
   
   (define (bit-andfx val num)
      (if (fixnum? val)
	  (bit-and val 31)
	  `(bit-and ,val 31)))
   
   (define (retnum expr)
      (if (type-fixnum? type)
	  `(int32->fixnum ,expr)
	  `(int32->integer ,expr)))

   (let ((tl (j2s-type lhs))
	 (tr (j2s-type rhs)))
      (cond
	 ((and (or (type-fixnum? tl) (eq? tl 'int32))
	       (or (type-fixnum? tr) (eq? tr 'int32)))
	  (case op
	     ((<< >> >>>)
	      (binop lhs rhs mode return conf hint type
		 (lambda (left right)
		    (retnum
		       `(,(fxop op) ,(fx->int32 left) ,(bit-andfx right 31))))))
	     (else
	      (binop lhs rhs mode return conf hint type
		 (lambda (left right)
		    (retnum
		       `(,(fxop op) ,(fx->int32 left) ,(fx->int32 right))))))))
	 ((or (type-fixnum? tr) (eq? tr 'int32))
	  (case op
	     ((<< >> >>>)
	      (binop lhs rhs mode return conf hint type
		 (lambda (left right)
		    `(if (fixnum? ,left)
			 ,(retnum
			     `(,(fxop op) ,(fx->int32 left) ,(bit-andfx right 31)))
			 ,(js-binop loc op left right)))))
	     (else
	      (binop lhs rhs mode return conf hint type
		 (lambda (left right)
		    `(if (fixnum? ,left)
			 ,(retnum
			     `(,(fxop op) ,(fx->int32 left) ,(fx->int32 right)))
			 ,(js-binop loc op left right)))))))
	 ((or (type-fixnum? tl) (eq? tl 'int32))
	  (binop lhs rhs mode return conf hint type
	     (lambda (left right)
		`(if (fixnum? ,right)
		     ,(retnum
			 `(,(fxop op) ,(fx->int32 left) ,(fx->int32 right)))
		     ,(js-binop loc op left right)))))
	 ((memq op '(BIT_OR & ^))
	  (binop lhs rhs mode return conf hint 'any
	     (lambda (left right)
		(scm-if (scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		   (retnum
		       `(,(fxop op) ,(fx->int32 left) ,(fx->int32 right)))
		   (js-binop loc op left right)))))
	 (else
	  (binop lhs rhs mode return conf hint 'any
	     (lambda (left right)
		(js-binop loc op left right)))))))
   
;*---------------------------------------------------------------------*/
;*    js-arithmetic ...                                                */
;*---------------------------------------------------------------------*/
(define (js-arithmetic loc op::symbol type lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)
   
   (define (u32op op) (symbol-append op 'u32))
   (define (fxop op) (symbol-append op 'fx))
   (define (strop op) (symbol-append 'js-jsstring op '?))
   (define (jsopfx op) (symbol-append 'js op 'fx))
   (define (jsopfx32 op) (symbol-append 'js op 'fx32))
   (define (jsopfx64 op) (symbol-append 'js op 'fx64))
   (define (jsop op) (symbol-append 'js op))
   
   (define (js-binopfx op x y conf)
      (case (config-get conf :long-size 32)
	 ((32)
	  `(,(jsopfx32 op) ,x ,y))
	 ((64)
	  `(,(jsopfx64 op) ,x ,y))
	 (else
	  `(,(jsopfx op) ,x ,y))))

   (cond
      ((and (is-uint32? lhs) (is-uint32? rhs) (type-uint32? type) (u32? conf))
       (binop lhs rhs mode return conf hint 'uint32
	  (lambda (left right)
	     `(,(u32op op) ,left ,right))))
      ((and (is-int53? lhs) (is-int53? rhs) (m64? conf))
       (if (type-int53? type)
	   (binop lhs rhs mode return conf hint type
	      (lambda (left right)
		 `(,(fxop op) ,left ,right)))
	   (binop lhs rhs mode return conf hint type
	      (lambda (left right)
		 (js-binopfx op left right conf)))))
      ((and (is-fx? lhs) (is-fx? rhs))
       (if (type-fixnum? type)
	   (binop lhs rhs mode return conf hint type
	      (lambda (left right)
		 `(,(fxop op) ,left ,right)))
	   (binop lhs rhs mode return conf hint type
	      (lambda (left right)
		 (js-binopfx op left right conf)))))
      ((and (eq? op '+) 
	    (or (eq? (j2s-type lhs) 'string) (eq? (j2s-type rhs) 'string)))
       (binop lhs rhs mode return conf hint type
	  (lambda (left right)
	     (cond
		((and (eq? (j2s-type lhs) 'string) (eq? (j2s-type rhs) 'string))
		 `(js-jsstring-append ,left ,right))
		((eq? (j2s-type lhs) 'string)
		 `(js-jsstring-append
		     ,left
		     (js-tojsstring (js-toprimitive ,right 'any %this) %this)))
		(else
		 `(js-jsstring-append
		     (js-tojsstring (js-toprimitive ,left 'any %this) %this)
		     ,right))))))
      ((or (memq 'integer hint) (type-integer? type))
       (binop lhs rhs mode return conf hint 'integer
	  (lambda (left right)
	     (scm-if (scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		(js-binopfx op left right conf)
		(js-binop loc op left right)))))
      ((and (is-number? lhs) (is-number? rhs))
       (binop lhs rhs mode return conf hint 'integer
	  (lambda (left right)
	     (scm-if (scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		(js-binopfx op left right conf)
		`(,(jsop op) ,left ,right %this)))))
      (else
       (binop lhs rhs mode return conf hint 'integer
	  (lambda (left right)
	     (if (and (maybe-number? lhs) (maybe-number? rhs))
		 `(if ,(scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		      ,(js-binopfx op left right conf)
		      (,(jsop op) ,left ,right %this))
		 `(,(jsop op) ,left ,right %this)))))))

