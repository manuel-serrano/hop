;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-ops.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:21:19 2017                          */
;*    Last change :  Fri Dec  8 08:22:23 2017 (serrano)                */
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
	   __js2scheme_scheme-cast
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
	  (let ((expr (j2s-scheme expr mode return conf hint totype))
		(typ (j2s-type-ref expr)))
	     (cond
		((eqv? expr 0) +0.0)
		((memq typ '(int32 uint32 int53 integer number)) expr)
		(else (epairify loc `(js-tonumber ,expr %this))))))
	 ((-)
	  ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.7
	  (let ((expr (j2s-scheme expr mode return conf hint totype))
		(typ (j2s-type-ref expr)))
	     (cond
		((eqv? expr 0)
		 -0.0)
		((eq? typ 'int32)
		 (epairify loc `(negs32 ,expr)))
		((eq? typ 'uint32)
		 (epairify loc `(negu32 ,expr)))
		((eq? typ 'int53)
		 (epairify loc `(negfx ,expr)))
		((number? expr)
		 (let ((n (j2s-number (- expr) conf)))
		    (if (pair? n)
			(epairify loc n)
			n)))
		(else
		 (epairify loc
		    `(js-neg ,expr %this))))))
	 ((~)
	  ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.8
	  (unless (eq? type 'int32)
	     (error "j2scheme" "expected int32 type for ~" type))
	  (epairify loc
	     `(bit-nots32
		 ,(j2s-scheme expr mode return conf hint totype))))
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
      ((type-int30? (j2s-type-ref node)) #t)
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
      ((+)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left right)))
	   (js-binop2-add loc type lhs rhs mode return conf hint totype)))
      ((-)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left right)))
	   (js-arithmetic-addsub loc op type lhs rhs mode return conf hint totype)))
      ((*)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left right)))
	   (js-arithmetic-mul loc type lhs rhs mode return conf hint totype)))
      ((/)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left right)))
	   (js-arithmetic-div loc type lhs rhs mode return conf hint totype)))
      ((remainder)
       (js-arithmetic-remainder loc type lhs rhs mode return conf hint totype))
      ((eq?)
       (binop lhs rhs mode return conf hint 'any
	  (lambda (left right)
	     `(eq? ,left ,right))))
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
	  ((and (eq? (j2s-type-ref lhs) 'integer) (eq? (j2s-type-ref rhs) 'integer))
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
      
      ((OR)
       (let ((lhsv (gensym 'lhs)))
	  `(let ((,lhsv ,(j2s-scheme lhs mode return conf hint totype)))
	      (if ,(if (eq? (j2s-type-ref lhs) 'bool) lhsv `(js-totest ,lhsv))
		  ,lhsv
		  ,(j2s-scheme rhs mode return conf hint totype)))))
      ((&&)
       (let ((lhsv (gensym 'lhs)))
	  `(let ((,lhsv ,(j2s-scheme lhs mode return conf hint totype)))
	      (if ,(if (eq? (j2s-type-ref lhs) 'bool) lhsv `(js-totest ,lhsv))
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
      ((/num)
       `(js/num ,lhs ,rhs))
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
      ((eq?)
       `(eq? ,lhs ,rhs))
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
	 ((isa? expr J2SHopRef)
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
	  (let ((right (gensym 'rhs)))
	     `(let ((,(utype-ident right (j2s-type-ref rhs) conf #t) ,scmrhs))
		 ,(gen scmlhs right))))
	 (testr
	  (let ((left (gensym 'lhs)))
	     `(let ((,(utype-ident left (j2s-type-ref lhs) conf #t) ,scmlhs))
		 ,(gen left scmrhs))))
	 (else
	  (let ((left (gensym 'lhs))
		(right (gensym 'rhs)))
	     `(let* ((,(utype-ident left (j2s-type-ref lhs) conf #t) ,scmlhs)
		     (,(utype-ident right (j2s-type-ref rhs) conf #t) ,scmrhs))
		 ,(gen left right)))))))

;*---------------------------------------------------------------------*/
;*    js-cmp ...                                                       */
;*    -------------------------------------------------------------    */
;*    The compilation of the comparison functions.                     */
;*---------------------------------------------------------------------*/
(define (js-cmp-old loc op::symbol lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)

   (define (u32op op) (symbol-append op 'u32))
   (define (fxop op) (symbol-append op 'fx))
   (define (jsop op) (symbol-append 'js op))
   (define (strop op) (symbol-append 'js-jsstring op '?))

   (let ((lt (j2s-type-ref lhs))
	 (rt (j2s-type-ref rhs)))
      (cond
	 ;; uint32
	 ((and (memq lt '(uint29 uint32 index length))
	       (memq rt '(uint29 uint32 index length)))
	  (binop lhs rhs mode return conf hint 'uint32
	     (lambda (left right)
		`(,(u32op op) ,left ,right))))
	 ((and (memq op '(< <=))
	       (memq lt '(integer number))
	       (memq rt '(uint29 index length uint32)))
	  (binop lhs rhs mode return conf hint 'uint32
	     (lambda (left right)
		(if (m64? conf)
		    `(if (fixnum? ,left)
			 (,(fxop op) ,left (uint32->fixnum ,right))
			 (,(jsop op) ,left ,right %this))
		    `(if (fixnum? ,left)
			 (or (<fx ,left 0)
			     (,(u32op op) (fixnum->uint32 ,left) ,right))
			 (,(jsop op) ,left ,right %this))))))
	 ((and (memq op '(< <=))
	       (memq lt '(any))
	       (memq rt '(uint29 index length uint32)))
	  (binop lhs rhs mode return conf hint 'uint32
	     (lambda (left right)
		`(if (fixnum? ,left)
		     ,(if (m64? conf)
			  `(,(fxop op) ,left (uint32->fixnum ,right))
			  `(or (<fx ,left 0)
			       (,(u32op op) (fixnum->uint32 ,left) ,right)))
		     (,(jsop op) (js-tonumber ,left %this) ,right %this)))))
	 ((and (memq op '(> >=))
	       (memq lt '(any))
	       (memq rt '(uint29 index length uint32)))
	  (binop lhs rhs mode return conf hint 'uint32
	     (lambda (left right)
		`(if (fixnum? ,left)
		     ,(if (m64? conf)
			  `(,(fxop op) ,left (uint32->fixnum ,right))
			  `(or (>=fx ,left 0)
			       (,(u32op op) (fixnum->uint32 ,left) ,right)))
		     (,(jsop op) (js-tonumber ,left %this) ,right %this)))))
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
	 ((or (eq? (j2s-type-ref lhs) 'number) (eq? (j2s-type-ref rhs) 'number))
	  (binop lhs rhs mode return conf hint 'integer
	     (lambda (left right)
		(scm-if (scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		   `(,(fxop op) ,left ,right)
		   (js-binop loc op left right)))))
	 ((or (eq? (j2s-type-ref lhs) 'any) (eq? (j2s-type-ref rhs) 'any))
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
		(js-binop loc op left right)))))))

;*---------------------------------------------------------------------*/
;*    js-cmp ...                                                       */
;*    -------------------------------------------------------------    */
;*    The compilation of the comparison functions.                     */
;*---------------------------------------------------------------------*/
(define (js-cmp loc o::symbol lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)

   (define (op o base)
      (case o
	 ((== === != !==) (if (eq? base 'js) '= (symbol-append '= base)))
	 (else (symbol-append o base))))
      
   (define (opfx o) (op o 'fx))
   (define (opfl o) (op o 'fl))
   (define (ops32 o) (op o 's32))
   (define (opu32 o) (op o 'u32))
   (define (opjs o) (op o 'js))

   (define (notify o expr)
      (if (memq o '(!= !==))
	  (match-case expr
	     (((kwote not) ?val) val)
	     (else `(not ,expr)))
	  expr))
  
   (define (cmp/32 o lhs tl left rhs tr right)
      (cond
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(,(ops32 o) ,left ,right))
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(,(opu32 o) ,left ,right))
	 ((and (memq tl '(int32 uint32)) (memq tr '(int32 uint32)))
	  `(,(opfx o) ,(tolong/64 left tl) ,(tolong/64 right tr)))
	 ((eq? tl 'int32)
	  `(if (fixnum? ,right)
	       (,(ops32 o) ,left (fixnum->int32 ,right))
	       (,(opjs o) ,left ,right %this)))
	 ((eq? tr 'int32)
	  `(if (fixnum? ,left)
	       (,(ops32 o) (fixnum->int32 ,left) ,right)
	       (,(opjs o) ,left ,right %this)))
	 ((eq? tl 'uint32)
	  `(if (fixnum? ,right)
	       (if (>=fx ,right 0)
		   (,(opu32 o) ,left (fixnum->uint32 ,right))
		   #f)
	       (,(opjs o) ,left ,right %this)))
	 ((eq? tr 'uint32)
	  `(if (fixnum? ,left)
	       (if (>=fx ,left 0)
		   (,(opu32 o) (fixnum->uint32 ,left) ,right)
		   #t)
	       (,(opjs o) ,left ,right %this)))
	 (else
	  `(if (fixnums? ,left ,right)
	       (,(opfx o) ,left ,right)
	       (,(opjs o) ,left ,right %this)))))

   (define (cmp/64 o lhs tl left rhs tr right)
      (cond
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(,(ops32 o) ,left ,right))
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(,(opu32 o) ,left ,right))
	 ((and (memq tl '(int32 uint32 int53)) (memq tr '(int32 uint32 int53)))
	  `(,(opfx o) ,(tolong/64 left tl) ,(tolong/64 right tr)))
	 ((memq tl '(int32 uint32 int53))
	  `(if (fixnum? ,right)
	       (,(opfx o) ,(tolong/64 left tl) ,right)
	       (,(opjs o) ,left ,right %this)))
	 ((memq tr '(int32 uint32 int53))
	  `(if (fixnum? ,left)
	       (,(opfx o) ,left ,(tolong/64 right tr))
	       (,(opjs o) ,left ,right %this)))
	 (else
	  `(if (fixnums? ,left ,right)
	       (,(opfx o) ,left ,right)
	       (,(opjs o) ,left ,right %this)))))

   (binop lhs rhs mode return conf hint '*
      (lambda (left right)
	 (let ((tl (j2s-type-ref lhs))
	       (tr (j2s-type-ref rhs)))
	    (epairify loc
	       (notify o
		  (if (m64? conf)
		      (cmp/64 o lhs tl left rhs tr right)
		      (cmp/32 o lhs tl left rhs tr right))))))))

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
			((string=? val "pair") 'pair?)
			((string=? val "object") #f)
			((string=? val "symbol") 'js-symbol?)
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
	   (js-cmp loc op lhs rhs mode return conf hint totype))))
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
	   (js-cmp loc op lhs rhs mode return conf hint totype))))
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
	   (js-cmp loc op lhs rhs mode return conf hint totype))))
      ((and (eq? (j2s-type-ref lhs) 'string) (eq? (j2s-type-ref rhs) 'string))
       (binop lhs rhs mode return conf '(string) 'any
	  (lambda (left right)
	     (js-binop loc op left right))))
      ((and (eq? (j2s-type-ref lhs) 'bool) (eq? (j2s-type-ref rhs) 'bool))
       (binop lhs rhs mode return conf '(bool) 'any
	  (lambda (left right)
	     (if (memq op '(!= !==))
		 `(not (eq? ,left ,right))
		 `(eq? ,left ,right)))))
      ((and (memq op '(== !=))
	    (or (memq (j2s-type-ref lhs) '(bool string object array))
		(memq (j2s-type-ref rhs) '(bool string object array))))
       (binop lhs rhs mode return conf '(bool) 'any
	  (lambda (left right)
	     (if (eq? op '!=)
		 `(not (js-equal-sans-flonum? ,left ,right %this))
		 `(js-equal-sans-flonum? ,left ,right %this)))))
      ((or (memq (j2s-type-ref lhs) '(undefined null))
	   (memq (j2s-type-ref rhs) '(undefined null)))
       (binop lhs rhs mode return conf '(bool) 'any
	  (lambda (left right)
	     (case op
		((!==)
		 `(not (eq? ,left ,right)))
		((===)
		 `(eq? ,left ,right))
		((==)
		 (if (memq (j2s-type-ref lhs) '(undefined null))
		     `(or (eq? (js-undefined) ,right) (eq? (js-null) ,right))
		     `(or (eq? ,left (js-undefined)) (eq? ,left (js-null)))))
		((!=)
		 (if (memq (j2s-type-ref lhs) '(undefined null))
		     `(not (or (eq? (js-undefined) ,right) (eq? (js-null) ,right)))
		     `(not (or (eq? ,left (js-undefined)) (eq? ,left (js-null))))))
		(else
		 (js-binop loc op left right))))))
      ((and (or (eq? op '===) (eq? op '!==))
	    (or (eq? (j2s-type-ref lhs) 'bool) (eq? (j2s-type-ref rhs) 'bool)))
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
   
   (define (bitop op)
      (case op
	 ((&) 'bit-ands32)
	 ((BIT_OR) 'bit-ors32)
	 ((^) 'bit-xors32)
	 ((>>) 'bit-rshs32)
	 ((>>>) 'bit-urshu32)
	 ((<<) 'bit-lshs32)
	 (else (error "bitop" "unknown operator" op))))

   (define (mask32 sexpr expr::J2SExpr)
      (cond
	 ((fixnum? sexpr)
	  (bit-and sexpr 31))
	 ((uint32? sexpr)
	  (uint32->fixnum (bit-andu32 sexpr #u32:31)))
	 ((int32? sexpr)
	  (int32->fixnum (bit-ands32 sexpr #s32:31)))
	 ((inrange-32? expr)
	  (case (j2s-type-ref expr)
	     ((int32) `(int32->fixnum ,sexpr))
	     ((uint32) `(uint32->fixnum ,sexpr))
	     (else sexpr)))
	 (else
	  (case (j2s-type-ref expr)
	     ((int32) `(int32->fixnum ,sexpr))
	     ((uint32) `(uint32->fixnum ,sexpr))
	     ((integer) sexpr)
	     (else `(bit-and (js-tointeger ,sexpr %this) 31))))))
   
   (binop lhs rhs mode return conf hint '*
      (lambda (left right)
	 (let ((tl (j2s-type-ref lhs))
	       (tr (j2s-type-ref rhs)))
	    (epairify loc
	       (case op
		  ((>> <<)
		   (j2s-cast
		      `(,(bitop op) ,(toint32 left tl) ,(mask32 right rhs))
		      #f 'int32 type conf))
		  ((>>>)
		   (j2s-cast
		      `(,(bitop op) ,(touint32 left tl) ,(mask32 right rhs))
		      #f 'uint32 type conf))
		  (else
		   (j2s-cast
		      `(,(bitop op) ,(toint32 left tl) ,(toint32 right tr))
		      #f 'int32 type conf))))))))
;*                                                                     */
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
	    (or (eq? (j2s-type-ref lhs) 'string) (eq? (j2s-type-ref rhs) 'string)))
       (binop lhs rhs mode return conf hint type
	  (lambda (left right)
	     (cond
		((and (eq? (j2s-type-ref lhs) 'string) (eq? (j2s-type-ref rhs) 'string))
		 `(js-jsstring-append ,left ,right))
		((eq? (j2s-type-ref lhs) 'string)
		 (case (j2s-type-ref rhs)
		    ((uint29)
		     `(js-jsstring-append ,left
			 (js-ascii->jsstring (fixnum->string ,right))))
		    ((int30 int53)
		     `(js-jsstring-append
			 ,left
			 ,(if (m64? conf)
			      `(js-ascii->jsstring (fixnum->string ,right))
			      `(js-tojsstring
				  (js-toprimitive ,right 'any %this) %this))))
		    (else
		     `(js-jsstring-append
			 ,left
			 (js-tojsstring (js-toprimitive ,right 'any %this) %this)))))
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

;*---------------------------------------------------------------------*/
;*    js-binop2-add ...                                                */
;*---------------------------------------------------------------------*/
(define (js-binop2-add loc type lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)

   (define (add-string loc type lhs rhs mode return conf hint totype)
      (binop lhs rhs mode return conf hint type
	 (lambda (left right)
	    (cond
	       ((and (eq? (j2s-type-ref lhs) 'string) (eq? (j2s-type-ref rhs) 'string))
		`(js-jsstring-append ,left ,right))
	       ((eq? (j2s-type-ref lhs) 'string)
		(case (j2s-type-ref rhs)
		   ((uint29 index length uint32)
		    `(js-jsstring-append ,left
			(js-ascii->jsstring
			   (fixnum->string (uint32->fixnum ,right)))))
		   ((uint30 int32)
		    `(js-jsstring-append ,left
			(js-ascii->jsstring
			   (fixnum->string (int32->fixnum ,right)))))
		   ((int53)
		    `(js-jsstring-append
			,left
			,(if (m64? conf)
			     `(js-ascii->jsstring (fixnum->string ,right))
			     `(js-tojsstring
				 (js-toprimitive ,right 'any %this) %this))))
		   (else
		    `(js-jsstring-append
			,left
			(js-tojsstring (js-toprimitive ,right 'any %this) %this)))))
	       (else
		`(js-jsstring-append
		    (js-tojsstring (js-toprimitive ,left 'any %this) %this)
		    ,right))))))

   (define (add-generic loc type lhs rhs mode return conf hint totype)
      (js-arithmetic loc '+ type lhs rhs mode return conf hint totype))
   
   (cond
      ((type-number? type)
       (js-arithmetic-addsub loc '+ type lhs rhs mode return conf hint totype))
      ((or (eq? type 'string)
	   (eq? (j2s-type-ref lhs) 'string) (eq? (j2s-type-ref rhs) 'string))
       (add-string loc type lhs rhs mode return conf hint totype))
      (else
       (add-generic loc type lhs rhs mode return conf hint totype))))

;*---------------------------------------------------------------------*/
;*    js-arithmetic-addsub ...                                         */
;*---------------------------------------------------------------------*/
(define (js-arithmetic-addsub loc op::symbol type lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)

   (define (opu32 op) (symbol-append op 'u32))
   (define (ops32 op) (symbol-append op 's32))
   (define (opfx op) (symbol-append op 'fx))
   (define (opfx/overflow op) (symbol-append op 'fx/overflow))
   (define (op/overflow op) (symbol-append op '/overflow))
   (define (opjs op) (symbol-append op 'js))

   (define (addsub-generic/32 op type lhs tl left rhs tr right)
      (cond
	 ((inrange-int32? lhs)
	  (if (inrange-int32? rhs)
	      `(overflow29
		  (int32->fixnum
		     (,(ops32 op) ,(toint32/32 left tl) ,(toint32/32 right tr))))
	      `(if (fixnum? ,right)
		   (,(opfx/overflow op) ,(tofx/32 left tl) ,right)
		   (,(opjs op) ,left ,right %this))))
	 ((inrange-int32? rhs)
	  `(if (fixnum? ,left)
	       (,(opfx/overflow op) ,left ,(tofx/32 right tr))
	       (,(opjs op) ,left ,right %this)))
	 (else
	  `(if (fixnums? ,left ,right)
	       (,(opfx/overflow op) ,left ,right)
	       (,(opjs op) ,left ,right %this)))))

   (define (addsub-int32/32 op lhs tl left rhs tr right)
      (cond
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(,(ops32 op) ,left ,right))
	 ((and (eq? tl 'int32) (eq? tr 'uint32) (inrange-int32? rhs))
	  `(,(ops32 op) ,left (uint32->int32 ,right)))
	 ((and (eq? tl 'uint32) (eq? tr 'int32) (inrange-int32? lhs))
	  `(,(ops32 op) (uint32->int32 ,left) ,right))
	 (else
	  (let ((n (gensym)))
	     `(let ((,n ,(addsub-generic/32 op type lhs tl left rhs tr right)))
		 (if (fixnum? ,n) (fixnum->uint32 ,n) (flonum->uint32 ,n)))))))
   
   (define (addsub-uint32/32 op lhs tl left rhs tr right)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(,(opu32 op) ,left ,right))
	 ((and (eq? tl 'uint32) (eq? tr 'int32) (inrange-uint32? rhs))
	  `(,(opu32 op) ,left (int32->uint32 ,right)))
	 ((and (eq? tl 'int32) (eq? tr 'uint32) (inrange-uint32? lhs))
	  `(,(opu32 op) (int32->uint32 ,left) ,right))
	 (else
	  (let ((n (gensym)))
	     `(let ((,n ,(addsub-generic/32 op type lhs tl left rhs tr right)))
		 (if (fixnum? ,n) (fixnum->uint32 ,n) (flonum->uint32 ,n)))))))
   
   (define (addsub/32 op type lhs tl left rhs tr right)
      (case type
	 ((int32)
	  (addsub-int32/32 op lhs tl left rhs tr right))
	 ((uint32)
	  (addsub-uint32/32 op lhs tl left rhs tr right))
	 ((integer number obj any object)
	  (addsub-generic/32 op type lhs tl left rhs tr right))
	 (else
	  (error "addsub/32" "illegal integer type" type))))
   
   (define (addsub-generic/64 op type lhs tl left rhs tr right)
      `(if (fixnums? ,left ,right)
	   (,(opfx/overflow op) ,left ,right)
	   (,(opjs op) ,left ,right %this)))

   (define (addsub-number/64 op type lhs tl left rhs tr right)
      (cond
	 ((and (memq tl '(int32 uint32)) (memq tr '(int32 uint32)))
	  `(,(opfx/overflow op) ,(tolong/64 left tl) ,(tolong/64 right tr)))
	 ((and (eq? tl 'uint32) (eq? tr 'int32))
	  `(,(opfx/overflow op) (uint32->fixnum ,left) (int32->fixnum ,right)))
	 (else
	  `(if (fixnums? ,left ,right)
	       (,(opfx/overflow op) ,left ,right)
	       (,(op/overflow op) ,left ,right)))))

   (define (addsub-long/64 op lhs tl left rhs tr right)
      `(,(opfx op) ,(tolong/64 left tl) ,(tolong/64 right tr)))

   (define (addsub-int32/64 op lhs tl left rh<s tr right)
      (cond
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(,(ops32 op) ,left ,right))
	 ((and (memq tl '(int32 uint32 int53)) (memq tr '(int32 uint32 int53)))
	  `(fixnum->int32
	      ,(addsub-long/64 op lhs tl left rhs tr right)))
	 (else
	  `(fixnum->int32
	      ,(addsub-generic/64 op type lhs tl left rhs tr right)))))
   
   (define (addsub-uint32/64 op lhs tl left rhs tr right)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(,(opu32 op) ,left ,right))
	 ((and (memq tl '(int32 uint32 int53)) (memq tr '(int32 uint32 int53)))
	  `(fixnum->uint32
	      ,(addsub-long/64 op lhs tl left rhs tr right)))
	 (else
	  `(fixnum->uint32
	      (addsub-generic/64 op type lhs tl left rhs tr right)))))
      
   (define (addsub-int53/64 op lhs tl left rhs tr right)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(uint32->fixnum (+u32 ,left ,right)))
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(,(opfx op) (int32->fixnum ,left) (int32->fixnum ,right)))
	 ((and (memq tl '(int32 uint32 int53)) (memq tr '(int32 uint32 int53)))
	  `(,(opfx op) ,(tolong/64 left tl) ,(tolong/64 right tr)))
	 ((and (eq? tl 'bint) (eq? tr 'bint))
	  `(,(opfx/overflow op) ,left ,right))
	 (else
	  (addsub-generic/64 op type lhs tl left rhs tr right))))

   (define (addsub/64 op type lhs tl left rhs tr right)
      (case type
	 ((int32)
	  (addsub-int32/64 op lhs tl left rhs tr right))
	 ((uint32)
	  (addsub-uint32/64 op lhs tl left rhs tr right))
	 ((int53)
	  (addsub-int53/64 op lhs tl left rhs tr right))
	 ((integer number)
	  (addsub-number/64 op type lhs tl left rhs tr right))
	 ((obj any object)
	  (addsub-generic/64 op type lhs tl left rhs tr right))
	 (else
	  (error "addsub/64" "illegal integer type" type))))

   (binop lhs rhs mode return conf hint '*
      (lambda (left right)
	 (let ((tl (j2s-type-ref lhs))
	       (tr (j2s-type-ref rhs)))
	    (epairify loc
	       (if (m64? conf)
		   (addsub/64 op type lhs tl left rhs tr right)
		   (addsub/32 op type lhs tl left rhs tr right)))))))

;*---------------------------------------------------------------------*/
;*    js-arithmetic-mul ...                                            */
;*---------------------------------------------------------------------*/
(define (js-arithmetic-mul loc type lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)

   (define (mul/32 type lhs tl left rhs tr right)
      (if (and (memq tl '(int32 uint32)) (memq tr '(int32 uint32)))
	  `(*/overflow ,left ,right)
	  `(*js ,(tonumber/32 left tl) ,(tonumber/32 right tr) %this)))
   
   (define (mul/64 type lhs tl left rhs tr right)
      (if (and (memq tl '(int32 uint32 int53)) (memq tr '(int32 uint32 int53)))
	  `(*/overflow ,left ,right)
	  `(*js ,(tonumber/64 left tl) ,(tonumber/64 right tr) %this)))
   
   (binop lhs rhs mode return conf hint '*
      (lambda (left right)
	 (let ((tl (j2s-type-ref lhs))
	       (tr (j2s-type-ref rhs)))
	    (epairify loc
	       (if (m64? conf)
		   (mul/64 type lhs tl left rhs tr right)
		   (mul/32 type lhs tl left rhs tr right)))))))

;*---------------------------------------------------------------------*/
;*    js-arithmetic-div ...                                            */
;*---------------------------------------------------------------------*/
(define (js-arithmetic-div loc type lhs rhs
	   mode return conf hint::pair-nil totype)

   (define (power2 rsh)
      
      (define (find-power2 bitlsh one::obj n)
	 (let loop ((k 1))
	    (let ((m (bitlsh one k)))
	       (cond
		  ((= m n) (when (<fx k 31) k))
		  ((> m n) #f)
		  (else (loop (+fx k 1)))))))

      (define (find-power2fl n)
	 (when (and (integer? n) (=fl (/fl n 2.0) (roundfl (/fl n 2.0))))
	    (let loop ((k 1))
	       (let ((m (exptfl 2. (fixnum->flonum k))))
		  (cond
		     ((=fl m n) (when (<fx k 31) k))
		     ((>fl m n) #f)
		     (else (loop (+fx k 1))))))))
      
      (when (isa? rsh J2SNumber)
	 (with-access::J2SNumber rhs (val)
	    (cond
	       ((fixnum? val) (find-power2 bit-lsh 1 val))
	       ((uint32? val) (find-power2 bit-lshu32 #u32:1 val))
	       ((int32? val) (find-power2 bit-lshs32 #s32:1 val))
	       ((flonum? val) (find-power2fl val))))))
   
   (define (div-power2 k)
      
      (define (positive? n)
	 (with-access::J2SExpr n (range)
	    (and (interval? range) (>= (interval-min range) #l0))))
      
      (let ((n (gensym 'n)))
	 (case (j2s-type-ref lhs)
	    ((uint32)
	     `(let ((,n ,(j2s-scheme lhs mode return conf hint totype)))
		 (if (=u32 (bit-andu32
			      ,n ,(fixnum->uint32 (-fx (bit-lsh 1 k) 1)))
			#u32:0)
		     (bit-rsh ,n ,k)
		     (/js ,n ,(bit-lsh 1 k) %this))))
	    ((int32)
	     `(let ((,n ,(j2s-scheme lhs mode return conf hint totype)))
		 (if (=s32 (bit-ands32
			      ,n ,(fixnum->int32 (-fx (bit-lsh 1 k) 1)))
			#s32:0)
		     ,(if (positive? lhs)
			  `(bit-rsh ,n ,k)
			  `(/pow2s32 ,n ,k))
		     (/js ,n ,(bit-lsh 1 k) %this))))
	    (else
	     `(let ((,n ,(j2s-scheme lhs mode return conf hint totype)))
		 (if (and (fixnum? ,n) (=fx (bit-and ,n ,(-fx (bit-lsh 1 k) 1)) 0))
		     ,(if (positive? lhs)
			  `(bit-rsh ,n ,k)
			  `(/pow2fx ,n ,k))
		     (/js ,n ,(bit-lsh 1 k) %this)))))))
   
   (let ((k (power2 rhs)))
      (if k
	  (div-power2 k)
	  (binop lhs rhs mode return conf hint '*
	     (lambda (left right)
		(let ((tl (j2s-type-ref lhs))
		      (tr (j2s-type-ref rhs)))
		   (epairify loc
		      (cond
			 ((and (eq? tl 'uint32) (eq? tr 'uint32))
			  `(if (=u32 (remainderu32 ,left ,right) #u32:0)
			       (/u32 ,left ,right)
			       (/fl (uint32->flonum ,left)
				  (uint32->flonum ,right))))
			 ((and (eq? tl 'int32) (eq? tr 'int32))
			  `(if (=s32 (remainders32 ,left ,right) #s32:0)
			       (/s32 ,left ,right)
			       (/fl (int32->flonum ,left)
				  (int32->flonum ,right))))
			 (else
			  `(if (fixnums? ,left ,right)
			       (if (=fx (remainderfx ,left ,right) 0)
				   (/fx ,left ,right)
				   (/fl (fixnum->flonum ,left)
				      (fixnum->flonum ,right)))
			       (/js ,left ,right %this)))))))))))

;*---------------------------------------------------------------------*/
;*    js-arithmetic-remainder ...                                      */
;*---------------------------------------------------------------------*/
(define (js-arithmetic-remainder loc type lhs rhs
	   mode return conf hint::pair-nil totype)
   
   (define (remainderjs left right tl tr)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(remainderu32 ,left ,right))
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(remainders32 ,left ,right))
	 ((and (eq? tl 'uint32) (eq? tr 'int32) (inrange-int32? lhs))
	  `(remainders32 (uint32->int32 ,left) ,right))
	 ((and (eq? tl 'int32) (eq? tr 'uint32) (inrange-int32? rhs))
	  `(remainders32 ,left (uint32->int32 ,right)))
	 ((and (eq? tr 'uint32) (inrange-int32? rhs))
	  `(if (fixnum? ,left)
	       (remainderfx ,left (uint32->fixnum ,right))
	       (remainder ,left ,(todouble right tr))))
	 ((eq? tr 'int32)
	  `(if (fixnum? ,left)
	       (remainderfx ,left (int32->fixnum ,right))
	       (remainder ,left ,(todouble right tr))))
	 (else
	  `(remainder ,(todouble left tl) ,(todouble right tr)))))
   
   (define (remainders32 left right tl tr)
	  (tprint "##### REMAINDERs32 tl=" tl " tr=" tr)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(uint32->int32 (remainderu32 ,left ,right)))
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(remainders32 ,left ,right))
	 ((and (eq? tl 'uint32) (eq? tr 'int32) (inrange-int32? lhs))
	  `(remainders32 (uint32->int32 ,left) ,right))
	 ((and (eq? tl 'int32) (eq? tr 'uint32) (inrange-int32? rhs))
	  `(remainders32 ,left (uint32->int32 ,right)))
	 ((and (eq? tl 'int53) (eq? tr 'uint32))
	  `(remainderfx ,left ,(uint32->fixnum right)))
	 ((and (eq? tr 'uint32) (inrange-int32? rhs))
	  `(if (fixnum? ,left)
	       (fixnum->int32 (remainderfx ,left (uint32->fixnum ,right)))
	       (let ((n (remainder ,left ,(todouble right tr))))
		  (if (fixnum? n) (fixnum->int32 n) (flonum->int32 n)))))
	 ((eq? tr 'int32)
	  `(if (fixnum? ,left)
	       (fixnum->int32 (remainderfx ,left (int32->fixnum ,right)))
	       (let ((n (remainder ,left ,(todouble right tr))))
		  (if (fixnum? n) (fixnum->int32 n) (flonum->int32 n)))))
	 (else
	  `(let ((n (remainder ,(todouble left tl) ,(todouble right tr))))
	      (if (fixnum? n) (fixnum->int32 n) (flonum->int32 n))))))
   
   (define (remainderu32 left right tl tr)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(remainderu32 ,left ,right))
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(int32->uint32 (remainders32 ,left ,right)))
	 ((and (eq? tl 'uint32) (eq? tr 'int32) (inrange-int32? lhs))
	  `(int32->uint32 (remainders32 (uint32->int32 ,left) ,right)))
	 ((and (eq? tl 'int32) (eq? tr 'uint32) (inrange-int32? rhs))
	  `(int32->uint32 (remainders32 ,left (uint32->int32 ,right))))
	 ((and (eq? tr 'uint32) (inrange-int32? rhs))
	  `(if (fixnum? ,left)
	       (fixnum->uint32 (remainderfx ,left (uint32->fixnum ,right)))
	       (let ((n (remainder ,left ,(todouble right tr))))
		  (if (fixnum? n) (fixnum->uint32 n) (flonum->int32 n)))))
	 ((eq? tr 'int32)
	  `(if (fixnum? ,left)
	       (fixnum->uint32 (remainderfx ,left (int32->fixnum ,right)))
	       (let ((n (remainder ,left ,(todouble right tr))))
		  (if (fixnum? n) (fixnum->uint32 n) (flonum->int32 n)))))
	 (else
	  `(let ((n (remainder ,(todouble left tl) ,(todouble right tr))))
	      (if (fixnum? n) (fixnum->uint32 n) (flonum->int32 n))))))
   
   (binop lhs rhs mode return conf hint '*
      (lambda (left right)
	 (let ((tl (j2s-type-ref lhs))
	       (tr (j2s-type-ref rhs)))
	    (epairify loc
	       (case type
		  ((int32) (remainders32 left right tl tr))
		  ((uint32) (remainderu32 left right tl tr))
		  (else (remainderjs left right tl tr))))))))

;*---------------------------------------------------------------------*/
;*    tonumber/32 ...                                                  */
;*---------------------------------------------------------------------*/
(define (tonumber/32 val type::symbol)
   (case type
      ((int32)
       `(if (overflow29 (int32->fixnum ,val))
	    (int32->flonum ,val)
	    (int32->fixnum ,val)))
      ((uint32)
       `(if (<u32 ,val (-u32 (bit-lshu32 #u32:1 29) #u32:1))
	    (uint32->fixnum ,val)
	    (uint32->flonum ,val)))
      (else val)))

;*---------------------------------------------------------------------*/
;*    toint32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (toint32 val type)
   (case type
      ((int32)
       val)
      ((uint32)
       `(uint32->int32 ,val))
      (else
       `(if (fixnum? ,val) (fixnum->int32 ,val) (js-toint32 ,val %this)))))

;*---------------------------------------------------------------------*/
;*    touint32 ...                                                     */
;*---------------------------------------------------------------------*/
(define (touint32 val type)
   (case type
      ((uint32)
       `(int32->uint32 ,val))
      ((uint32)
       val)
      (else
       `(if (fixnum? ,val) (fixnum->uint32 ,val) (js-touint32 ,val %this)))))

;*---------------------------------------------------------------------*/
;*    toint32/32 ...                                                   */
;*---------------------------------------------------------------------*/
(define (toint32/32 val type::symbol)
   (case type
      ((int32) val)
      ((uint32) `(uint32->int32 ,val))
      (else `(fixnum->int32 ,val))))

;*---------------------------------------------------------------------*/
;*    tofx/32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (tofx/32 val type::symbol)
   (case type
      ((int32) `(int32->fixnum ,val))
      ((uint32) `(uint32->fixnum ,val))
      (else val)))

;*---------------------------------------------------------------------*/
;*    tolong/64 ...                                                    */
;*---------------------------------------------------------------------*/
(define (tolong/64 val type::symbol)
   (case type
      ((int32) `(int32->fixnum ,val))
      ((uint32) `(uint32->fixnum ,val))
      (else val)))

;*---------------------------------------------------------------------*/
;*    tonumber/64 ...                                                  */
;*---------------------------------------------------------------------*/
(define (tonumber/64 val type::symbol)
   (case type
      ((int32) `(int32->fixnum ,val))
      ((uint32) `(uint32->fixnum ,val))
      (else val)))

;*---------------------------------------------------------------------*/
;*    todouble ...                                                     */
;*---------------------------------------------------------------------*/
(define (todouble val type::symbol)
   (case type
      ((int32) `(int32->flonum ,val))
      ((uint32) `(uint32->flonum ,val))
      (else `(if (fixnum? ,val) (fixnum->flonum ,val) ,val))))
   
