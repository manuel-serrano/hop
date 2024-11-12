;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-ops.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:21:19 2017                          */
;*    Last change :  Tue Nov 12 09:56:30 2024 (serrano)                */
;*    Copyright   :  2017-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Unary and binary Scheme code generation                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-ops

   (include "ast.sch" "usage.sch" "context.sch")

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_classutils
	   __js2scheme_scheme
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-test
	   __js2scheme_scheme-constant)

   (export (j2s-in? loc id obj ::struct)
	   (j2s-scheme-binary-as ::J2SBinary mode return ::struct type)
	   (j2s-scheme-unary-as ::J2SUnary mode return ::struct type)
	   (js-binop2 loc op::symbol type lhs::J2SNode rhs::J2SNode
	      mode return ::struct)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUnary ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUnary mode return ctx)
   (with-access::J2SUnary this (loc op type expr)
      (js-unop loc op type expr mode return ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-unary-as ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-unary-as this::J2SUnary mode return ctx type)
   (with-access::J2SUnary this (loc expr op)
      (cond
	 ((and (eq? op '-) (memq type '(int32 uint32 int53 integer number)))
	  (js-unop loc op type expr mode return ctx))
	 ((and (eq? op '!) (eq? type 'bool))
	  (js-unop loc op type expr mode return ctx))
	 ((and (eq? op '~) (eq? type 'int32))
	  (js-unop loc op type expr mode return ctx))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBinary mode return ctx)
   (with-access::J2SBinary this (loc op lhs rhs type)
      (epairify-deep loc
	 (js-binop2 loc op type lhs rhs mode return ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-binary-as ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-binary-as this::J2SBinary mode return ctx type)
   (with-access::J2SBinary this (loc op lhs rhs (etype type))
      (cond
	 ((and (memq op '(+ ++)) (memq type '(int32 uint32 int53 integer number)))
	  (epairify-deep loc
	     (js-binop2 loc op type lhs rhs mode return ctx)))
	 ((and (eq? op '+) (memq type '(string buffer)) (memq etype '(string buffer)))
	  (epairify-deep loc
	     (js-binop2 loc op type lhs rhs mode return ctx)))
	 ((and (memq op '(* - -- / >> << >>> & BIT_OR ^))
	       (memq type '(int32 uint32)))
	  (if (memq etype '(int32 uint32))
	      (epairify-deep loc
		 (j2s-cast (js-binop2 loc op type lhs rhs mode return ctx)
		    this etype type ctx))
	      (epairify-deep loc
		 (js-binop2 loc op type lhs rhs mode return ctx))))
	 ((and (eq? type 'bool) (memq op '(OR OR* &&)))
	  (epairify-deep loc
	     (js-binop2 loc op 'bool
		(if (eq? (j2s-type lhs) 'bool) lhs (J2SCast 'bool lhs))
		(if (eq? (j2s-type rhs) 'bool) rhs (J2SCast 'bool rhs))
		mode return ctx)))
	 (else
	  #f))))

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
       `(and ,(j2s-fixnum? (fixnum-test left)) ,(j2s-fixnum? (fixnum-test right))))
      ((and left right)
       `(and ,left ,right))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    js-unop ...                                                      */
;*---------------------------------------------------------------------*/
(define (js-unop loc op type expr mode return ctx)
   
   (define (err id)
      (match-case loc
	 ((at ?fname ?loc)
	  `(with-access::JsGlobalObject %this (js-syntax-error)
	      (js-raise
		 (js-new %this js-syntax-error
		    ,(j2s-jsstring
			(format "Delete of an unqualified identifier in strict mode: \"~a\"" id)
			loc ctx)
		    ,fname ,loc))))
	 (else
	  `(with-access::JsGlobalObject %this (js-syntax-error)
	      (js-raise
		 (js-new %this js-syntax-error
		    ,(j2s-jsstring
			(format "Delete of an unqualified identifier in strict mode: \"~a\"" id)
			loc ctx)))))))
   
   (define (delete->scheme expr)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.7
      (cond
	 ((isa? expr J2SWithRef)
	  (with-access::J2SWithRef expr (id withs expr loc)
	     (let loop ((withs withs))
		(if (null? withs)
		    `(begin ,(j2s-scheme expr mode return ctx) #f)
		    `(if ,(j2s-in? loc (& id (context-program ctx)) (car withs) ctx)
			 (js-delete! ,(j2s-scheme (car withs) mode return ctx)
			    ,(& (j2s-scheme id mode return ctx) (context-program ctx))
			    #f
			    %this)
			 ,(loop (cdr withs)))))))
	 ((isa? expr J2SAccess)
	  (with-access::J2SAccess expr (obj field)
	     `(js-delete! ,(j2s-scheme obj mode return ctx)
		 ,(j2s-scheme field mode return ctx)
		 ,(strict-mode? mode)
		 %this)))
	 ((isa? expr J2SUnresolvedRef)
	  (if (strict-mode? mode)
	      (with-access::J2SUnresolvedRef expr (id)
		 (err id))
	      (with-access::J2SUnresolvedRef expr (id)
		 `(js-delete! ,j2s-unresolved-del-workspace
		     ,(& id (context-program ctx)) #f %this))))
	 ((and (isa? expr J2SRef) (not (isa? expr J2SThis)))
	  (cond
	     ((strict-mode? mode)
	      (with-access::J2SRef expr (decl)
		 (with-access::J2SDecl decl (id)
		    (err id))))
	     ((with-access::J2SRef expr (decl)
		 (isa? decl J2SDeclExtern))
	      (with-access::J2SRef expr (decl)
		 (with-access::J2SDeclExtern decl (configurable)
		    `(begin ,configurable))))
	     (else
	      '(begin #f))))
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (delete->scheme expr)))
	 ((isa? expr J2SUndefined)
	  `(begin #f))
	 (else
	  `(begin ,(j2s-scheme expr mode return ctx) #t))))

   (define (typeof->scheme expr)
      (let ((ty (j2s-type expr)))
	 (cond
	    ((memq ty '(int30 int32 uint32 fixnum integer real number))
	     `(& "number"))
	    ((memq ty '(string buffer))
	     `(& "string"))
	    ((isa? expr J2SUnresolvedRef)
	     ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.3
	     (with-access::J2SUnresolvedRef expr (id loc cache)
		`(js-typeof ,(j2s-unresolved id #f cache loc ctx) %this)))
	    ((isa? expr J2SParen)
	     (with-access::J2SParen expr (expr)
		(typeof->scheme expr)))
	    (else
	     `(js-typeof ,(j2s-scheme expr mode return ctx) %this)))))

   (define (bitnot loc expr)
      ;; optimize the pattern ~~expr that is sometime used to cast
      ;; an expression into a number
      (match-case expr
	 ((bit-nots32 ?expr) expr)
	 (else (epairify loc `(bit-nots32 ,expr)))))
   
   (case op
      ((!)
       (if (eq? type 'bool)
	   (let ((sexp (j2s-test-not expr mode return ctx)))
	      (if (pair? sexp)
		  (epairify loc sexp)
		  sexp))
	   (epairify loc
	      `(if ,(j2s-test expr mode return ctx) #f #t))))
      ((typeof)
       (epairify loc
	  (typeof->scheme expr)))
      ((void)
       (epairify loc
	  `(begin
	      ,(j2s-scheme expr mode return ctx)
	      (js-undefined))))
      ((delete)
       (epairify loc
	  (delete->scheme expr)))
      ((+)
       ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.6
       ;; this is the explicit JS type cast
       (let ((sexp (j2s-scheme expr mode return ctx)))
	  (cond
	     ((j2s-number? sexp)
	      (if (and (eq? type 'real) (fixnum? sexp))
		  (fixnum->flonum sexp)
		  sexp))
	     ((type-number? (j2s-type expr))
	      (epairify loc sexp))
	     (else
	      (epairify loc `(js-tonumeric ,sexp %this))))))
      ((-)
       ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.7
       (let ((sexp (j2s-scheme expr mode return ctx))
	     (ty (j2s-type expr)))
	  (cond
	     ((and (eq? ty 'uint32) (eq? type 'uint32))
	      (error "j2s-unop" "uint32 cannot be negated" (j2s->sexp expr)))
	     ((j2s-number? sexp)
	      (if (and (eq? type 'real) (fixnum? sexp))
		  (fixnum->flonum (negfx sexp))
		  (- 0 sexp)))
	     ((eq? ty 'int32)
	      (epairify loc (j2s-as `(negs32 ,sexp) expr 'int32 type ctx)))
	     ((eq? ty 'uint32)
	      (cond
		 ((eq? type 'int32)
		  (epairify loc `(negs32 (uint32->int32 ,sexp))))
		 ((eq? type 'int53)
		  (epairify loc `(negfx (uint32->fixnum ,sexp))))
		 (else
		  (if (m64? (context-conf ctx))
		      (epairify loc `(negfx (uint32->fixnum ,sexp)))
		      (let ((val (gensym)))
			 (epairify loc
			    `(let ((,val ,sexp))
				(if (<u32 ,val (bit-lshu32 #u32:1 30))
				    (negfx (uint32->fixnum ,val))
				    (negfl (uint32->flonum ,sexp))))))))))
	     ((eq? ty 'int53)
	      (epairify loc (j2s-as `(negfx ,sexp) expr 'int53 type ctx)))
	     ((eq? ty 'real)
	      (epairify loc (j2s-as `(negfl ,sexp) expr 'real type ctx)))
	     ((eq? ty 'number)
	      (epairify loc (j2s-as `(- ,sexp) expr 'number type ctx)))
	     ((memq type '(int32 uint32 int53))
	      (epairify loc
		 (j2s-as `(- (js-tonumeric ,sexp %this)) expr 'any type ctx)))
	     (else
	      (epairify loc
		 (j2s-as `(negjs (js-tonumeric ,sexp %this)) exp 'any type ctx))))))
      ((~)
       ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.8
       (if (eq? type 'int32)
	   (bitnot loc (j2s-scheme expr mode return ctx))
	   `(bit-notjs ,(j2s-scheme expr mode return ctx) %this)))
      ((?.)
       (error "j2s-unop" "should not be here" (j2s->sexp expr)))
      (else
       (epairify loc
	  `(,op ,(j2s-scheme expr mode return ctx))))))

;*---------------------------------------------------------------------*/
;*    js-binop2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-binop2 loc op::symbol type lhs::J2SNode rhs::J2SNode
	   mode return ctx)
   (with-trace 'scheme "j2s-binop2"
      (trace-item "op=" op)
      (trace-item "lhs=" (j2s->sexp lhs))
      (trace-item "rhs=" (j2s->sexp rhs))
      (case op
	 ((+ ++)
	  (if (=fx (context-get ctx :optim 0) 0)
	      (with-tmp lhs rhs mode return ctx
		 (lambda (left right)
		    (j2s-cast
		       (js-binop loc op left lhs right rhs ctx)
		       #f 'any type ctx)))
	      (js-binop-add loc op type lhs rhs mode return ctx)))
	 ((- --)
	  (if (=fx (context-get ctx :optim 0) 0)
	      (with-tmp lhs rhs mode return ctx
		 (lambda (left right)
		    (j2s-cast
		       (js-binop-arithmetic loc op left lhs right rhs ctx)
		       #f 'any type ctx)))
	      (js-arithmetic-addsub loc op type lhs rhs mode return ctx)))
	 ((*)
	  (if (=fx (context-get ctx :optim 0) 0)
	      (with-tmp lhs rhs mode return ctx
		 (lambda (left right)
		    (j2s-cast
		       (js-binop-arithmetic loc op left lhs right rhs ctx)
		       #f 'any type ctx)))
	      (js-arithmetic-mul loc type lhs rhs mode return ctx)))
	 ((**)
	  (if (=fx (context-get ctx :optim 0) 0)
	      (with-tmp lhs rhs mode return ctx
		 (lambda (left right)
		    (j2s-cast
		       (js-binop-arithmetic loc '** left lhs right rhs ctx)
		       #f 'any type ctx)))
	      (js-arithmetic-expt loc type lhs rhs mode return ctx)))
	 ((/)
	  (js-arithmetic-div loc type lhs rhs mode return ctx))
	 ((remainderfx remainder)
	  (with-tmp lhs rhs mode return ctx
	     (lambda (left right)
		`(,op ,left ,right))))
	 
	 ((%)
	  (js-arithmetic-% loc type lhs rhs mode return ctx))
	 ((eq?)
	  (with-tmp lhs rhs mode return ctx
	     (lambda (left right)
		`(eq? ,left ,right))))
	 ((== === != !==)
	  (if (=fx (context-get ctx :optim 0) 0)
	      (with-tmp lhs rhs mode return ctx
		 (lambda (left right)
		    (js-binop loc op left lhs right rhs ctx)))
	      (js-equality loc op type lhs rhs mode return ctx)))
	 ((< <= > >=)
	  (if (=fx (context-get ctx :optim 0) 0)
	      (with-tmp lhs rhs mode return ctx
		 (lambda (left right)
		    (js-binop loc op left lhs right rhs ctx)))
	      (js-cmp loc op lhs rhs mode return ctx)))
	 ((& ^ BIT_OR >> >>> <<)
	  (js-bitop loc op type lhs rhs mode return ctx))
	 ((OR)
	  (if (eq? type 'bool)
	      `(or ,(j2s-scheme lhs mode return ctx)
		   ,(j2s-scheme rhs mode return ctx))
	      (let* ((lhsv (gensym 'lhs))
		     (test (if (eq? (j2s-type lhs) 'bool)
			       lhsv
			       (j2s-cast lhsv lhs (j2s-type lhs) 'bool ctx))))
		 `(let ((,(type-ident lhsv (j2s-type lhs) (context-conf ctx))
			 ,(j2s-scheme lhs mode return ctx)))
		     ,(cond
			 ((eq? test #t)
			  (j2s-cast lhsv lhs (j2s-type lhs) type ctx))
			 ((eq? test #f)
			  (j2s-cast (j2s-scheme rhs mode return ctx) rhs
			     (j2s-type rhs) type ctx))
			 (else
			  `(if ,test
			       ,(j2s-cast lhsv lhs (j2s-type lhs) type ctx)
			       ,(j2s-cast (j2s-scheme rhs mode return ctx) rhs
				   (j2s-type rhs) type ctx))))))))
	 ((OR*)
	  (let ((tmp (gensym '%or*)))
	     `(let ((,tmp ,(j2s-scheme lhs mode return ctx)))
		 (if (eq? ,tmp (js-undefined))
		     ,(j2s-cast (j2s-scheme rhs mode return ctx) rhs
			 (j2s-type rhs) type ctx)
		     ,(j2s-cast tmp lhs (j2s-type lhs) type ctx)))))
	 ((&&)
	  (if (eq? type 'bool)
	      `(and ,(j2s-scheme lhs mode return ctx)
		    ,(j2s-scheme rhs mode return ctx))
	      (let* ((lhsv (gensym 'lhs))
		     (test (if (eq? (j2s-type lhs) 'bool)
			       lhsv
			       (j2s-cast lhsv lhs (j2s-type lhs) 'bool ctx))))
		 `(let ((,(type-ident lhsv (j2s-type lhs) (context-conf ctx))
			 ,(j2s-scheme lhs mode return ctx)))
		     ,(cond
			 ((eq? test #t)
			  (j2s-cast (j2s-scheme rhs mode return ctx) rhs
			     (j2s-type rhs) type ctx))
			 ((eq? test #f)
			  (j2s-cast lhsv lhs (j2s-type lhs) type ctx))
			 (else
			  `(if ,test
			       ,(j2s-cast (j2s-scheme rhs mode return ctx) rhs
				   (j2s-type rhs) type ctx)
			       ,(j2s-cast lhsv lhs (j2s-type lhs) type ctx))))))))
	 ((??)
	  `(js-nullish ,(box (j2s-scheme lhs mode return ctx) (j2s-type lhs) ctx)
	      ,(box (j2s-scheme rhs mode return ctx) (j2s-type rhs) ctx)))
	 ((MAX)
	  (js-min-max loc '>>= lhs rhs mode return ctx))
	 ((MIN)
	  (js-min-max loc '<<= lhs rhs mode return ctx))
	 ((as)
	  (box (j2s-scheme lhs mode return ctx) (j2s-type lhs) ctx))
	 (else
	  (with-tmp lhs rhs mode return ctx
	     (lambda (left right)
		(js-binop loc op left lhs right rhs ctx)))))))

;*---------------------------------------------------------------------*/
;*    j2s-in? ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-in? loc id obj ctx)
   
   (define (toname id)
      (match-case id
	 ((& . ?-) id)
	 (else `(js-toname ,id %this))))
   
   (if (> (context-get ctx :debug 0) 0)
       `(js-in?/debug ,obj ,(toname id) %this ',loc)
       `(js-in? ,obj ,(toname id) %this)))

;*---------------------------------------------------------------------*/
;*    js-binop ...                                                     */
;*    -------------------------------------------------------------    */
;*    This function is called with left and right being either         */
;*    atoms or variable references. Hence it does not generate         */
;*    bindings.                                                        */
;*---------------------------------------------------------------------*/
(define (js-binop loc op lhs l rhs r ctx)

   (define (strict-equal-uint32 lhs rhs tr)
      (cond
	 ((eq? tr 'uint32)
	  `(=u32 ,lhs ,rhs))
	 ((eq? tr 'int32)
	  `(and (>=s32 ,rhs 0) (=u32 ,lhs (int32->uint32 ,rhs))))
	 ((memq tr '(integer int53))
	  `(and (>=fx ,rhs 0) (=u32 ,lhs (fixnum->uint32 ,rhs))))
	 ((eq? tr 'real)
	  `(=fl ,(asreal lhs 'uint32) ,rhs))
	 (else
	  `(js-strict-equal-no-string? ,(asreal lhs 'uint32) ,rhs))))

   (define (strict-equal-int32 lhs rhs tr)
      (cond
	 ((eq? tr 'uint32)
	  `(and (>=s32 ,lhs 0) (=u32 (int32->uint32 ,lhs) ,rhs)))
	 ((eq? tr 'int32)
	  `(=s32 ,lhs ,rhs))
	 ((memq tr '(integer int53))
	  `(=s32 (int32->fixnum ,lhs) ,rhs))
	 ((eq? tr 'real)
	  `(=fl (int32->flonum ,lhs) ,rhs))
	 (else
	  `(js-strict-equal-no-string? (int32->flonum ,lhs) ,rhs))))
      
   (define (strict-equal lhs rhs)
      (let ((tl (j2s-type l))
	    (tr (j2s-type r)))
	 (cond
	    ((eq? tl 'uint32)
	     (strict-equal-uint32 lhs rhs tr))
	    ((eq? tr 'uint32)
	     (strict-equal-uint32 rhs lhs tl))
	    ((eq? tr 'int32)
	     (strict-equal-int32 rhs lhs tl))
	    ((or (eq? tl 'jsvector) (eq? tr 'jsvector))
	     `(eq? ,lhs ,rhs))
	    ((or (type-cannot? tr '(string buffer))
		 (type-cannot? tl '(string buffer)))
	     (if (and (type-cannot? tr '(real bigint))
		      (type-cannot? tl '(real bigint)))
		 `(eq? ,lhs ,rhs)
		 `(js-strict-equal-no-string? ,lhs ,rhs)))
	    (else
	     `(js-strict-equal? ,lhs ,rhs)))))

   (case op
      ((==)
       `(js-equal? ,lhs ,rhs %this))
      ((!=)
       `(not (js-equal? ,lhs ,rhs %this)))
      ((===)
       (strict-equal lhs rhs))
      ((!==)
       `(not ,(strict-equal lhs rhs)))
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
      ((!eqil?)
       `(not (js-eqil? ,lhs ,rhs)))
      ((!eqir?)
       `(not (js-eqir? ,lhs ,rhs)))
      ((<-)
       `(js<- ,lhs ,rhs %this))
      ((instanceof)
       (if (> (context-get ctx :debug 0) 0)
	   `(js-instanceof?/debug %this ',loc
	       ,(box lhs (j2s-type l) ctx) ,(box rhs (j2s-type r) ctx))
	   (j2s-instanceof? lhs l rhs r)))
      ((in)
       (j2s-in? loc lhs rhs ctx))
      ((+)
       `(js+ ,(box lhs (j2s-type l) ctx) ,(box rhs (j2s-type r) ctx) %this))
      ((++)
       (cond
	  ((isone? rhs) `(js++ ,(box lhs (j2s-type l) ctx) %this))
	  ((isminusone? rhs) `(js-- ,(box lhs (j2s-type l) ctx) %this))
	  (else (error "js-binop:++" "wrong rhs" (j2s->sexp r)))))
      ((--)
       (cond
	  ((isone? rhs) `(js-- ,(box lhs (j2s-type l) ctx) %this))
	  ((isminusone? rhs) `(js++ ,(box lhs (j2s-type l) ctx) %this))
	  (else (error "jsbinop:--" "wrong rhs" (j2s->sexp r)))))
      ((<)
       `(<js ,(box lhs (j2s-type l) ctx) ,(box rhs (j2s-type r) ctx) %this))
      ((<=)
       `(<=js ,(box lhs (j2s-type l) ctx) ,(box rhs (j2s-type r) ctx) %this))
      ((>)
       `(>js ,(box lhs (j2s-type l) ctx) ,(box rhs (j2s-type r) ctx) %this))
      ((>=)
       `(>=js ,(box lhs (j2s-type l) ctx) ,(box rhs (j2s-type r) ctx) %this))
      ((- -- * / % & ^ >> >>> << OR &&)
       (error "js-binop" "should not be here" op))
      (else
       `(,op ,(box lhs (j2s-type l) ctx) ,(box rhs (j2s-type r) ctx) %this))))

;*---------------------------------------------------------------------*/
;*    j2s-instanceof? ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-instanceof? lhs l rhs r)
   (case rhs
      ((!Object)
       `(js-object? ,lhs))
      ((!Array)
       (if (symbol? lhs)
	   `(or (js-array? ,lhs) (js-instanceof? %this ,lhs ,rhs))
	   `(let ((%o ,lhs))
	       (or (js-array? %o) (js-instanceof? %this %o ,rhs)))))
      ((!Vector)
       (if (symbol? lhs)
	   `(js-vector? ,lhs)
	   `(let ((%o ,lhs))
	       (js-vector? %o))))
      ((!Function)
       (if (symbol? lhs)
	   `(or (js-function? ,lhs) (js-instanceof? %this ,lhs ,rhs))
	   `(let ((%o ,lhs))
	       (or (js-function? %o) (js-instanceof? %this %o ,rhs)))))
      ((!Proxy)
       (if (symbol? lhs)
	   `(or (js-proxy? ,lhs) (js-instanceof? %this ,lhs ,rhs))
	   `(let ((%o ,lhs))
	       (or (js-proxy? %o) (js-instanceof? %this %o ,rhs)))))
      (else
       (cond
	  ((and (eq? (j2s-type r) 'function) (eq? (j2s-type l) 'object))
	   `(js-object-function-instanceof? %this ,lhs ,rhs))
	  ((eq? (j2s-type r) 'function)
	   (if (isa? r J2SRef)
	       (with-access::J2SRef r (decl)
		  (if (isa? decl J2SDeclClass)
		      (with-access::J2SDeclClass decl (val)
			 (if (isa? val J2SRecord)
			     `(,(class-predicate-id val) ,lhs)
			     `(js-function-instanceof? %this ,lhs ,rhs)))
		      `(js-function-instanceof? %this ,lhs ,rhs)))
	       `(js-function-instanceof? %this ,lhs ,rhs)))
	  (else
	   `(js-instanceof? %this ,lhs ,rhs))))))

;*---------------------------------------------------------------------*/
;*    js-binop-arithmetic ...                                          */
;*    -------------------------------------------------------------    */
;*    This function is called with left and right being either         */
;*    atoms or variable references. Hence it does not generate         */
;*    bindings.                                                        */
;*---------------------------------------------------------------------*/
(define (js-binop-arithmetic loc op left l right r ctx)
   (let ((tl (j2s-type l))
	 (tr (j2s-type r)))
      (case op
	 ((-)
	  `(-js ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((--)
	  (if (isone? right)
	      `(--js ,(box left tl ctx) %this)
	      (error "jsbinop:--" "wrong rhs" (j2s->sexp r))))
	 ((*)
	  `(*js ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((**)
	  `(**js ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((/)
	  `(/js ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((<)
	  `(<js ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((<=)
	  `(<=js ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((>)
	  `(>js ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((>=)
	  `(>=js ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((&)
	  `(bit-andjs ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((BIT_OR)
	  `(bit-orjs ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((^)
	  `(bit-xorjs ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((>>)
	  `(bit-rshjs ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((>>>)
	  `(bit-urshjs ,(box left tl ctx) ,(box right tr ctx) %this))
	 ((<<)
	  `(bit-lshjs ,(box left tl ctx) ,(box right tr ctx) %this))
	 (else
	  (error "js-binop-arihmetic" "should not be here" op)))))

;*---------------------------------------------------------------------*/
;*    js-cmp ...                                                       */
;*    -------------------------------------------------------------    */
;*    The compilation of the comparison functions.                     */
;*---------------------------------------------------------------------*/
(define (js-cmp loc o::symbol lhs::J2SExpr rhs::J2SExpr
	   mode return ctx)
   
   (define (notop o expr)
      (if (memq o '(!= !==))
	  (match-case expr
	     (((kwote not) ?val) val)
	     (else `(not ,expr)))
	  expr))
   
   (define (prefix-ref? expr)
      (when (isa? expr J2SPrefix)
	 (with-access::J2SPrefix expr (lhs)
	    (let loop ((lhs lhs))
	       (cond
		  ((isa? lhs J2SRef)
		   #t)
		  ((isa? lhs J2SCast)
		   (with-access::J2SCast lhs (expr)
		      (loop expr)))
		  (else
		   #f))))))

   (define (postfix-ref? expr)
      (when (isa? expr J2SPostfix)
	 (with-access::J2SPostfix expr (lhs)
	    (let loop ((lhs lhs))
	       (cond
		  ((isa? lhs J2SRef)
		   #t)
		  ((isa? lhs J2SCast)
		   (with-access::J2SCast lhs (expr)
		      (loop expr)))
		  (else
		   #f))))))

   (define (as-int53 expr::J2SExpr)
      (cond
	 ((isa? expr J2SPrefix)
	  (with-access::J2SPrefix expr (lhs rhs)
	     (duplicate::J2SPrefix expr
		(type 'int53)
		(lhs (as-int53 lhs))
		(rhs (as-int53 rhs)))))
	 ((isa? expr J2SPostfix)
	  (with-access::J2SPostfix expr (lhs rhs)
	     (duplicate::J2SPostfix expr
		(type 'int53)
		(lhs (as-int53 lhs))
		(rhs (as-int53 rhs)))))
	 ((isa? expr J2SCast)
	  (with-access::J2SCast expr (expr)
	     (as-int53 expr)))
	 ((isa? expr J2SRef)
	  (duplicate::J2SRef expr
	     (type 'int53)))
	 ((isa? expr J2SBinary)
	  (with-access::J2SBinary expr (lhs rhs)
	     (duplicate::J2SBinary expr
		(type 'int53)
		(lhs (as-int53 lhs))
		(rhs (as-int53 rhs)))))
	 (else
	  expr)))
   
   (let ((tl (j2s-type lhs))
	 (tr (j2s-type rhs))
	 (op (case o
		((!=) '==)
		((!==) '===)
		(else o))))
      (epairify loc
	 (notop o
	    (cond
	       ((eq? tl 'int32)
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (binop-int32-xxx op 'bool
			 lhs tl left rhs tr right ctx #f))))
	       ((eq? tr 'int32)
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (binop-int32-xxx op 'bool
			 rhs tr right lhs tl left ctx #t))))
	       ((eq? tl 'uint32)
		(if (and (eq? tr 'number) (or (prefix-ref? rhs) (prefix-ref? rhs)))
		    ;; fast path: uint32 op --NUM
		    (let ((rhsint53 (as-int53 rhs)))
		       (with-access::J2SAssig rhsint53 ((ref lhs))
			  `(if ,(j2s-fixnum? (j2s-scheme ref mode return ctx))
			       ,(js-cmp loc o lhs rhsint53 mode return ctx)
			       (with-tmp lhs rhs mode return ctx
				  (lambda (left right)
				     (binop-uint32-xxx op 'bool
					lhs tl left rhs tr right ctx #f))))))
		    (with-tmp lhs rhs mode return ctx
		       (lambda (left right)
			  (binop-uint32-xxx op 'bool
			     lhs tl left rhs tr right ctx #f)))))
	       ((eq? tr 'uint32)
		(if (and (eq? tl 'number) (or (prefix-ref? lhs) (prefix-ref? lhs)))
		    ;; fast path: --NUM op uint32
		    (let ((lhsint53 (as-int53 lhs)))
		       (with-access::J2SAssig lhsint53 ((ref lhs))
			  `(if ,(j2s-fixnum? (j2s-scheme ref mode return ctx))
			       ,(js-cmp loc o lhsint53 rhs mode return ctx)
			       ,(with-tmp lhs rhs mode return ctx
				   (lambda (left right)
				      (binop-uint32-xxx op 'bool
					 rhs tr right lhs tl left ctx #t))))))
		    (with-tmp lhs rhs mode return ctx
		       (lambda (left right)
			  (binop-uint32-xxx op 'bool
			     rhs tr right lhs tl left ctx #t)))))
	       ((eq? tl 'int53)
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (binop-int53-xxx op 'bool
			 lhs tl left rhs tr right ctx #f))))
	       ((eq? tr 'int53)
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (binop-int53-xxx op 'bool
			 rhs tr right lhs tl left ctx #t))))
	       ((eq? tl 'integer)
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (binop-integer-xxx op 'bool
			 lhs tl left rhs tr right ctx #f))))
	       ((eq? tr 'integer)
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (binop-integer-xxx op 'bool
			 rhs tr right lhs tl left ctx #t))))
	       ((eq? tl 'bint)
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (binop-bint-xxx op 'bool
			 lhs tl left rhs tr right ctx #f))))
	       ((eq? tr 'bint)
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (binop-bint-xxx op 'bool
			 rhs tr right lhs tl left ctx #t))))
	       ((eq? tl 'real)
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (binop-real-xxx op 'bool
			 lhs tl left rhs tr right ctx #f))))
	       ((eq? tr 'real)
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (binop-real-xxx op 'bool
			 rhs tr right lhs tl left ctx #t))))
	       ((or (is-hint? lhs 'real) (is-hint? rhs 'real))
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (if-flonums? left tl right tr
			 (binop-flonum-flonum op 'bool
			    (asreal left 'real)
			    (asreal right 'real)
			    #f)
			 (binop-any-any op 'bool
			    (box left tl ctx)
			    (box right tr ctx)
			    #f)))))
	       ((and (eq? tl 'number) (eq? tr 'number))
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (if-fixnums? left tl right tr
			 (binop-fixnum-fixnum op 'bool
			    (asfixnum left 'int53)
			    (asfixnum right 'int53)
			    #f)
			 (if-flonums? left tl right tr
			    (binop-flonum-flonum op 'bool
			       (asreal left 'real)
			       (asreal right 'real)
			       #f)
			    (binop-number-number op 'bool
			       (box left tl ctx)
			       (box right tr ctx)
			       #f))))))
	       (else
		(with-tmp lhs rhs mode return ctx
		   (lambda (left right)
		      (if-fixnums? left tl right tr
			 (binop-fixnum-fixnum op 'bool
			    (asfixnum left 'int53)
			    (asfixnum right 'int53)
			    #f)
			 (if-flonums? left tl right tr
			    (binop-flonum-flonum op 'bool
			       (asreal left 'real)
			       (asreal right 'real)
			       #f)
			    (let ((op (if (and (eq? op '===)
					       (or (type-cannot? tl '(string buffer))
						   (type-cannot? tr '(string buffer))))
					  'js-strict-equal-no-string?
					  op)))
			       (binop-any-any op 'bool
				  (box left tl ctx)
				  (box right tr ctx)
				  #f))))))))))))

;*---------------------------------------------------------------------*/
;*    js-min-max ...                                                   */
;*---------------------------------------------------------------------*/
(define (js-min-max loc op lhs rhs mode return ctx)
   (let loop ((lhst lhs)
	      (rhst rhs))
      (cond
	 ((isa? lhst J2SCast)
	  (with-access::J2SCast lhst (expr)
	     (loop expr rhst)))
	 ((isa? rhst J2SCast)
	  (with-access::J2SCast rhst (expr)
	     (loop lhst expr)))
	 (else
	  (with-tmp lhst rhst mode return ctx
	     (lambda (left right)
		(let ((lhsc (if (symbol? left)
				(with-access::J2SExpr lhs (loc)
				   (instantiate::J2SHopRef
				      (type (j2s-type lhst))
				      (loc loc)
				      (id left)))
				lhst))
		      (rhsc (if (symbol? right)
				(with-access::J2SExpr rhs (loc)
				   (instantiate::J2SHopRef
				      (type (j2s-type rhst))
				      (loc loc)
				      (id right)))
				rhst)))
		   `(if ,(js-cmp loc op lhsc rhsc mode return ctx)
			,(if (isa? lhs J2SCast)
			     (j2s-scheme (duplicate::J2SCast lhs
					    (expr lhsc))
				mode return ctx)
			     left)
			,(if (isa? rhs J2SCast)
			     (j2s-scheme (duplicate::J2SCast rhs
					    (expr rhsc))
				mode return ctx)
			     right)))))))))

;*---------------------------------------------------------------------*/
;*    js-equality ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-equality loc op::symbol type lhs::J2SExpr rhs::J2SExpr
	   mode return ctx)

   (define (type-fixnum? ty)
      (memq ty '(int32 uint32 integer bint)))

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
      (with-access::J2SCast expr (expr)
	 (aref expr)))
   
   (define (aref::J2SAref expr::J2SAccess)
      (with-access::J2SAccess expr (obj field)
	 obj))

   (define (j2s-case-type expr test #!key
	      (null 'false)
	      (number 'false)
	      (string 'false)
	      (object 'false)
	      (boolean 'false)
	      (regexp 'false)
	      (function 'false)
	      (arrow 'false)
	      (undefined 'false)
	      (pair 'false)
	      (array 'false)
	      (jsvector 'false)
	      (symbol 'false))
      (case (j2s-type expr)
	 ((int30 int32 uint32 fixnum integer real number) number)
	 ((string buffer) string)
	 ((boolean) boolean)
	 ((regexp) regexp)
	 ((function) function)
	 ((arrow) arrow)
	 ((undefined) undefined)
	 ((pair) pair)
	 ((symbol) pair)
	 ((array) array)
	 ((jsvector) jsvector)
	 (else test)))

   (define (j2s-unary-expr this::J2SExpr)
      (cond
	 ((isa? this J2SUnary)
	  (with-access::J2SUnary this (expr)
	     expr))
	 ((isa? this J2SParen)
	  (with-access::J2SParen this (expr)
	     (j2s-unary-expr expr)))
	 (else
	  #f)))
      
   (define (j2s-typeof-predicate this::J2SExpr expr)
      (cond
	 ((isa? this J2SUnary)
	  (with-access::J2SUnary this (op (unary expr))
	     (when (eq? op 'typeof)
		(when (or (isa? expr J2SString) (isa? expr J2SNativeString))
		   (with-access::J2SLiteralValue expr (val)
		      (cond
			 ((string=? val "number")
			  (j2s-case-type unary 'js-number? :number 'true))
			 ((string=? val "object")
			  (j2s-case-type unary 'js-object-or-null?
			     :object 'true :regexp 'true :null 'true :array 'true))
			 ((string=? val "function")
			  (if (eq? (j2s-type unary) 'function)
			      #t
			      (j2s-case-type unary 'js-procedure-proxy? :function 'true :arrow 'true)))
			 ((string=? val "string")
			  (j2s-case-type unary 'js-jsstring? :string 'true))
			 ((string=? val "undefined")
			  (j2s-case-type unary 'js-undefined? :undefined 'true))
			 ((string=? val "boolean")
			  (j2s-case-type unary 'boolean? :boolean 'true))
			 ((string=? val "pair")
			  (j2s-case-type unary 'pair? :pair 'true))
			 ((string=? val "symbol")
			  (j2s-case-type unary 'js-symbol? :symbol 'true))
			 (else
			  #f)))))))
	 ((isa? this J2SParen)
	  (with-access::J2SParen this ((child expr))
	     (j2s-typeof-predicate child expr)))
	 (else
	  #f)))

   (define (equality-int32 op lhs tl rhs tr mode return ctx flip::bool)
      ;; tl == int32, tr = ???
      (with-tmp-flip flip lhs rhs mode return ctx
	 (lambda (left right)
	    (let loop ((op op))
	       (cond
		  ((eq? op '!=)
		   `(not ,(loop '==)))
		  ((eq? op '!==)
		   `(not ,(loop '===)))
		  ((eq? tr 'int32)
		   `(=s32 ,left ,right))
		  ((eq? tr 'uint32)
		   (cond
		      ((inrange-int32? rhs)
		       `(=s32 ,left ,(asint32 right tr)))
		      ((m64? (context-conf ctx))
		       `(=fx ,(asfixnum left tl) ,(asfixnum right tr)))
		      (else
		       `(and (=s32 ,left ,(asint32 right tr))
			     (>=s32 ,left #s32:0)))))
		  ((eq? tr 'integer)
		   `(=fx ,(asfixnum left tl) ,right))
		  ((eq? tr 'real)
		   `(=fl ,(asreal left tl) ,right))
		  ((eq? op '===)
		   (cond
		      ((memq (j2s-type rhs) '(int32 uint32))
		       `(=fx ,(asfixnum left tl) ,right))
		      ((context-get ctx :=fx-as-eq #f)
		       `(js-eqil? ,(box left tl ctx) ,(box right tr ctx)))
		      (else
		       `(if ,(j2s-fixnum? right)
			    (=fx ,(asfixnum left tl) ,right)
			    (js-eqil?
			       ,(box left tl ctx)
			       ,(box right tr ctx))))))
		  ((not (eq? tr 'any))
		   `(js-equal-sans-flonum? ,(asfixnum left tl) ,right %this))
		  (else
		   `(js-equal-fixnum? ,(asfixnum left tl) ,right %this)))))))

   (define (equality-uint32 op lhs tl rhs tr mode return ctx flip::bool)
      ;; tl == uint32, tr = ???
      (with-tmp-flip flip lhs rhs mode return ctx
	 (lambda (left right)
	    (let loop ((op op))
	       (cond
		  ((eq? op '!=)
		   `(not ,(loop '==)))
		  ((eq? op '!==)
		   `(not ,(loop '===)))
		  ((eq? tr 'uint32)
		   `(us32 ,left right))
		  ((eq? tr 'int32)
		   (cond
		      ((inrange-int32? lhs)
		       `(=s32 ,(asint32 left tl) ,right))
		      ((m64? (context-conf ctx))
		       `(=fx ,(asfixnum left tl) ,(asfixnum right tr)))
		      (else
		       `(and (=s32 ,(asint32 left tl) ,right)
			     (>=s32 ,right #s32:0)))))
		  ((or (eq? tr 'integer) (eq? tr 'int53))
		   `(=fx ,(asfixnum left tl) ,right))
		  ((eq? tr 'real)
		   `(=fl ,(asreal left tl) ,right))
		  ((eq? op '===)
		   (cond
		      ((memq (j2s-type rhs) '(int32 uint32))
		       (if (inrange-int32? lhs)
			   `(=fx ,(asfixnum left tl) ,right)
			   `(and (=fx ,(asfixnum left tl) ,right)
				 (>=fx ,right 0))))
		      ((and (context-get ctx :=fx-as-eq #f)
			    (memq tr '(integer int53)))
		       (or (if (inrange-int32? lhs)
			       `(eq? ,(asfixnum left tl) ,right)
			       `(and (eq? ,(asfixnum left tl) ,right)
				     (>=fx ,right 0)))
			   `(js-eqil?
			       ,(box left tl ctx)
			       ,(box right tr ctx))))
		      ((and (inrange-int32? lhs) (context-get ctx :=fx-as-eq #f))
		       `(js-eqil?
			   ,(box left tl ctx)
			   ,(box right tr ctx)))
		      (else
		       `(if ,(j2s-fixnum? right)
			    ,(if (inrange-int32? lhs)
				 `(=fx ,(asfixnum left tl) ,right)
				 `(and (=fx ,(asfixnum left tl) ,right)
				       (>=fx ,right 0)))
			    (js-eqil?
			       ,(box left tl ctx)
			       ,(box right tr ctx))))))
		  (else
		   (if (inrange-int32? lhs)
		       (if (context-get ctx :=fx-as-eq #f)
			   `(js-equal-fixnum? ,(asfixnum left tl) ,right %this)
			   `(if ,(j2s-fixnum? right)
				(=fx ,(asfixnum left tl) ,right)
				(js-equal? ,(asfixnum left tl) ,right %this)))
		       (if (memq (j2s-type rhs) '(int32 uint32))
			   `(and (=fx ,(asfixnum left tl) ,right)
				  (>=fx ,right 0))
			   `(if ,(j2s-fixnum? right)
				(and (=fx ,(asfixnum left tl) ,right)
				     (>=fx ,right 0))
				(js-equal? 
				   ,(asreal left tl)
				   ,(box right tr ctx)
				   %this))))))))))

   (define (char-string? exp)
      (when (isa? exp J2SString)
	 (with-access::J2SString exp (val)
	    (=fx (string-length val) 1))))

   (define (empty-string? exp)
      (when (isa? exp J2SString)
	 (with-access::J2SString exp (val)
	    (=fx (string-length val) 0))))
   
   (define (charat? exp)
      (when (isa? exp J2SCall)
	 (with-access::J2SCall exp (fun args)
	    (when (and (isa? fun J2SAccess) (pair? args) (null? (cdr args)))
	       (with-access::J2SAccess fun (field)
		  (when (isa? field J2SString)
		     (with-access::J2SString field (val)
			(string=? val "charAt"))))))))

   (define (equality-char op char charat)
      (with-access::J2SCall charat (fun args)
	 (with-access::J2SAccess fun (obj)
	    (with-access::J2SString char (val)
	       (let ((expr `(eq? ,(char->integer (string-ref val 0))
			       (js-jsstring-charcodeat
				  ,(j2s-scheme obj mode return ctx)
				  ,(j2s-scheme (car args) mode return ctx)
				  %this))))
		  (if (eq? op '!==)
		      `(not ,expr)
		      expr))))))
   
   (define (equality-string op lhs tl rhs tr mode return ctx flip)
      (cond
	 ((and (memq tl '(string buffer))
	       (memq tr '(string buffer))
	       (or (char-string? lhs) (char-string? rhs))
	       (or (charat? lhs) (charat? rhs))
	       (let ((str (context-string ctx)))
		  (when str
		     (with-access::J2SDecl str (escape)
			(and (not escape)
			     (not (decl-usage-has? str '(assig))))))))
	  (if (char-string? lhs) 
	      (equality-char op lhs rhs)
	      (equality-char op rhs lhs)))
	 ((and (is-buffer-cast? lhs) (is-buffer-cast? rhs))
	  (with-access::J2SCast lhs ((lhs expr))
	     (with-access::J2SCast rhs ((rhs expr))
		(equality-string op lhs (j2s-type lhs)
		   rhs (j2s-type rhs) mode return ctx flip))))
	 ((is-buffer-cast? lhs)
	  (with-access::J2SCast lhs ((lhs expr))
	     (equality-string op lhs (j2s-type lhs)
		rhs tr mode return ctx flip)))
	 ((is-buffer-cast? rhs)
	     (with-access::J2SCast rhs ((rhs expr))
		(equality-string op lhs tl
		   rhs (j2s-type rhs) mode return ctx flip)))
	 ((and (memq tl '(string buffer)) (empty-string? rhs))
	  `(,(if (eq? op '!==) '>u32 '=u32)
	    (js-jsstring-length ,(j2s-scheme lhs mode return ctx)) #u32:0))
	 ((and (memq tr '(string buffer)) (empty-string? lhs))
	  `(,(if (eq? op '!==) '>u32 '=u32)
	    (js-jsstring-length ,(j2s-scheme lhs mode return ctx)) #u32:0))
	 (else
	  (with-tmp-flip flip lhs rhs mode return ctx
	     (lambda (left right)
		(if (or (type-cannot? tl '(string buffer)) (type-cannot? tr '(string buffer)))
		    (eq? op '!==)
		    (let ((cmp (cond
				  ((and (memq tl '(string buffer))
					(memq tr '(string buffer))
					(or (eq? tl 'buffer) (eq? tr 'buffer)))
				   'js-jsbuffer=?)
				  ((and (memq tl '(string buffer))
					(memq tr '(string buffer)))
				   'js-jsstring=?)
				  (else
				   'js-eqstring?))))
		       (if (eq? op '!==)
			   `(not (,cmp ,left ,right))
			   `(,cmp ,left ,right)))))))))
		
   (define (typeof-expr expr mode return ctx)
      (cond
	 ((isa? expr J2SUnresolvedRef)
	  ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.3
	  (with-access::J2SUnresolvedRef expr (id loc cache)
	     (j2s-unresolved id #f cache loc ctx)))
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (typeof-expr expr mode return ctx)))
	 (else
	  (j2s-scheme expr mode return ctx))))

   (define (typeof/pred expr pred)
      (cond
	 ((eq? pred 'true)
	  `(begin
	      ,(typeof-expr expr mode return ctx)
	      ,(if (memq op '(!= !==)) #f #t)))
	 ((eq? pred 'false)
	  `(begin
	      ,(typeof-expr expr mode return ctx)
	      ,(if (memq op '(!= !==)) #t #f)))
	 (else
	  (let ((t `(,pred ,(box (typeof-expr expr mode return ctx)
			       (j2s-type expr)
			       ctx))))
	     (if (memq op '(!= !==)) `(not ,t) t)))))

   (define (js-null? obj)
      (let ((v (j2s-scheme obj mode return ctx)))
	 (case op
	    ((!==) `(not (null? ,v)))
	    ((===) `(null? ,v))
	    ((!=) `(not (js-null-or-undefined? ,v)))
	    ((==) `(js-null-or-undefined? ,v))
	    (else "js-null?" "illegal operator" op))))

   (with-trace 'scheme "js-equality"
      (trace-item "op=" op " type=" type)
      (trace-item "lhs=" (j2s->sexp lhs))
      (trace-item "rhs=" (j2s->sexp rhs))
      (let ((tl (j2s-type lhs))
	    (tr (j2s-type rhs)))
	 (cond
	    ((j2s-typeof-predicate lhs rhs)
	     =>
	     (lambda (pred)
		(typeof/pred (j2s-unary-expr lhs) pred)))
	    ((j2s-typeof-predicate rhs lhs)
	     =>
	     (lambda (pred)
		(typeof/pred (j2s-unary-expr rhs) pred)))
	    ((and (is-uint32? lhs) (is-uint32? rhs))
	     (cond
		((j2s-aref-length? rhs)
		 (with-access::J2SAref (aref rhs) (field alen)
		    (let ((test `(or (=u32 %lhs ,(j2s-decl-scm-id alen ctx))
				     (=u32 %lhs ,(j2s-scheme rhs mode return ctx)))))
		       `(let ((%lhs ,(j2s-scheme lhs mode return ctx)))
			   ,(if (memq op '(!= !==))
				(js-not test)
				test)))))
		((j2s-aref-length? lhs)
		 (with-access::J2SAref (aref lhs) (field alen)
		    (let ((test `(or (=u32 ,(j2s-decl-scm-id alen ctx) %rhs)
				     (=u32 ,(j2s-scheme rhs mode return ctx)
					%rhs))))
		       `(let ((%rhs ,(j2s-scheme rhs mode return ctx)))
			   ,(if (memq op '(!= !==))
				(js-not test)
				test)))))
		(else
		 (js-cmp loc op lhs rhs mode return ctx))))
	    ((and (type-fixnum? tl) (type-fixnum? tr))
	     (cond
		((j2s-cast-aref-length? rhs)
		 (with-access::J2SAref (cast-aref rhs) (field alen)
		    (let ((test `(or (=fx %lhs ,(j2s-decl-scm-id alen ctx))
				     (=fx %lhs ,(j2s-scheme rhs mode return ctx)))))
		       `(let ((%lhs ,(j2s-scheme lhs mode return ctx)))
			   ,(if (memq op '(!= !==))
				(js-not test)
				test)))))
		((j2s-cast-aref-length? lhs)
		 (with-access::J2SAref (cast-aref lhs) (field alen)
		    (let ((test `(or (=fx ,(j2s-decl-scm-id alen ctx) %rhs)
				     (=fx ,(j2s-scheme rhs mode return ctx)
					%rhs))))
		       `(let ((%rhs ,(j2s-scheme rhs mode return ctx)))
			   ,(if (memq op '(!= !==))
				(js-not test)
				test)))))
		(else
		 (js-cmp loc op lhs rhs mode return ctx))))
	    ((eq? tl 'null)
	     (js-null? rhs))
	    ((eq? tr 'null)
	     (js-null? lhs))
	    ((eq? tl 'int32)
	     (equality-int32 op lhs tl rhs tr mode return ctx #f))
	    ((eq? tr 'int32)
	     (equality-int32 op rhs tr lhs tl mode return ctx #t))
	    ((eq? tl 'uint32)
	     (equality-uint32 op lhs tl rhs tr mode return ctx #f))
	    ((eq? tr 'uint32)
	     (equality-uint32 op rhs tr lhs tl mode return ctx #t))
	    ((and (memq op '(=== !==)) (memq tl '(string buffer)))
	     (equality-string op lhs tl rhs tr mode return ctx #f))
	    ((and (memq op '(=== !==)) (memq tr '(string buffer)))
	     (equality-string op rhs tr lhs tl mode return ctx #t))
	    ((and (memq op '(== !==)) (memq tl '(string buffer)) (charat? rhs))
	     (equality-string op lhs tl rhs tr mode return ctx #f))
	    ((and (memq op '(== !==)) (memq tr '(string buffer)) (charat? lhs))
	     (equality-string op rhs tr lhs tl mode return ctx #t))
	    ((and (eq? tl 'real) (eq? tr 'real))
	     (with-tmp lhs rhs mode return ctx 
		(lambda (left right)
		   (if (memq op '(== ===))
		       `(=fl ,left ,right)
		       `(not (=fl ,left ,right))))))
	    ((and (memq tl '(string buffer)) (memq tr '(string buffer)))
	     (with-tmp lhs rhs mode return ctx
		(lambda (left right)
		   (if (memq op '(== ===))
		       `(js-jsstring=? ,left ,right)
		       `(not (js-jsstring=? ,left ,right))))))
	    ((and (eq? tl 'bool) (eq? tr 'bool))
	     (with-tmp lhs rhs mode return ctx
		(lambda (left right)
		   (if (memq op '(== ===))
		       `(eq? ,left ,right)
		       `(not (eq? ,left ,right))))))
	    ((and (or (eq? tl 'bool) (eq? tr 'bool)) (memq op '(=== !==)))
	     (with-tmp lhs rhs mode return ctx
		(lambda (left right)
		   (if (eq? op '===)
		       `(eq? ,left ,right)
		       `(not (eq? ,left ,right))))))
	    ((and (is-number? lhs) (is-number? rhs))
	     (cond
		((j2s-cast-aref-length? rhs)
		 (with-access::J2SAref (cast-aref rhs) (field alen)
		    (let ((test `(or (= %lhs ,(j2s-decl-scm-id alen ctx))
				     (= %lhs ,(j2s-scheme rhs mode return ctx)))))
		       `(let ((%lhs ,(j2s-scheme lhs mode return ctx)))
			   ,(if (memq op '(!= !==))
				(js-not test)
				test)))))
		((j2s-cast-aref-length? lhs)
		 (with-access::J2SAref (cast-aref lhs) (field alen)
		    (let ((test `(or (= ,(j2s-decl-scm-id alen ctx) %rhs)
				     (= ,(j2s-scheme rhs mode return ctx) %rhs))))
		       `(let ((%rhs ,(j2s-scheme rhs mode return ctx)))
			   ,(if (memq op '(!= !==))
				(js-not test)
				test)))))
		(else
		 (js-cmp loc op lhs rhs mode return ctx))))
	    ((and (memq op '(== !=))
		  (or (memq tl '(bool string object array jsvector))
		      (memq tr '(bool string object array jsvector))))
	     (with-tmp lhs rhs mode return ctx
		(lambda (left right)
		   (cond
		      ((and (memq tl '(null undefined bool integer int53))
			    (memq tr '(null undefined bool integer int53)))
		       (if (eq? op '!=)
			   `(not (eq? ,left ,right))
			   `(eq? ,left ,right)))
		      ((memq tl '(string buffer))
		       (if (eq? op '!=)
			   `(not (js-equal-string? ,left ,right %this))
			   `(js-equal-string? ,left ,right %this)))
		      ((memq tr '(string buffer))
		       (if (eq? op '!=)
			   `(not (js-equal-string? ,right ,left %this))
			   `(js-equal-string? ,right ,left %this)))
		      (else
		       (if (eq? op '!=)
			   `(not (js-equal-sans-flonum? ,left ,right %this))
			   `(js-equal-sans-flonum? ,left ,right %this)))))))
	    ((or (memq tl '(undefined null)) (memq tr '(undefined null)))
	     (with-tmp lhs rhs mode return ctx
		(lambda (left right)
		   (case op
		      ((!==)
		       `(not (eq? ,left ,right)))
		      ((===)
		       `(eq? ,left ,right))
		      ((==)
		       (if (memq (j2s-type lhs) '(undefined null))
			   `(js-null-or-undefined? ,right)
			   `(js-null-or-undefined? ,left)))
		      ((!=)
		       (if (memq (j2s-type lhs) '(undefined null))
			   `(not (js-null-or-undefined? ,right))
			   `(not (js-null-or-undefined? ,left))))
		      (else
		       (js-binop loc op left lhs right rhs ctx))))))
	    (else
	     (with-tmp lhs rhs mode return ctx
		(lambda (left right)
		   (let ((op (cond
				((type-fixnum? tl)
				 (if (memq op '(== ===)) 'eqil? '!eqil?))
				((type-fixnum? tr)
				 (if (memq op '(== ===)) 'eqir? '!eqir?))
				(else
				 op))))
		      (js-binop loc op left lhs right rhs ctx)))))))))

;*---------------------------------------------------------------------*/
;*    js-bitop ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-bitop loc op::symbol type lhs::J2SExpr rhs::J2SExpr
	   mode return ctx)
   
   (define (bitop op)
      (case op
	 ((&) 'bit-ands32)
	 ((BIT_OR) 'bit-ors32)
	 ((^) 'bit-xors32)
	 ((>>) 'bit-rshs32)
	 ((>>>) 'bit-urshu32)
	 ((<<) 'bit-lshs32)
	 (else (error "bitop" "unknown operator" op))))

   (define (bitopjs op)
      (case op
	 ((&) 'bit-andjs)
	 ((BIT_OR) 'bit-orjs)
	 ((^) 'bit-xorjs)
	 ((>>) 'bit-rshjs)
	 ((>>>) 'bit-urshjs)
	 ((<<) 'bit-lshjs)
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
	  (case (j2s-type expr)
	     ((int32) `(bit-and ,(asfixnum sexpr 'int32) 31))
	     ((uint32) `(bit-and ,(asfixnum sexpr 'uint32) 31))
	     (else `(bit-and ,sexpr 31))))
	 (else
	  (case (j2s-type expr)
	     ((int32) `(bit-and ,(asfixnum sexpr 'int32) 31))
	     ((uint32) `(bit-and ,(asfixnum sexpr 'uint32) 31))
	     ((integer int53) `(bit-and ,sexpr 31))
	     ((real) `(bit-and (flonum->fixnum ,sexpr) 31))
	     (else `(bit-and (js-tointeger ,sexpr %this) 31))))))
   
   (with-tmp lhs rhs mode return ctx
      (lambda (left right)
	 (let ((tl (j2s-type lhs))
	       (tr (j2s-type rhs)))
	    (epairify loc
	       (cond
		  ((memq type '(int32 uint32))
		   (case op
		      ((>> <<)
		       (if (and (j2s-number? right) (= right 0) (eq? type tl))
			   (toint32 left tl ctx)
			   `(,(bitop op) ,(toint32 left tl ctx) ,(mask32 right rhs))))
		      ((>>>)
		       `(,(bitop op) ,(touint32 left tl ctx) ,(mask32 right rhs)))
		      (else
		       `(,(bitop op) ,(toint32 left tl ctx) ,(toint32 right tr ctx)))))
		  ((memq tr '(int32 uint32 integer))
		   (case op
		      ((>> <<)
		       (j2s-cast
			  (if (and (j2s-number? right) (= right 0) (eq? type tl))
			      (toint32 left tl ctx)
			      `(,(bitop op)
				,(toint32 left tl ctx) ,(mask32 right rhs)))
			  #f 'int32 type ctx))
		      ((>>>)
		       (j2s-cast
			  `(,(bitop op)
			    ,(touint32 left tl ctx) ,(mask32 right rhs))
			  #f 'uint32 type ctx))
		      (else
		       (j2s-cast
			  `(,(bitop op)
			    ,(toint32 left tl ctx) ,(toint32 right tr ctx))
			  #f 'int32 type ctx))))
		  ((memq tl '(int32 uint32 integer))
		   (case op
		      ((>> <<)
		       (j2s-cast
			  `(,(bitop op)
			    ,(asint32 left tl) ,(mask32 right rhs))
			  #f 'int32 type ctx))
		      ((>>>)
		       (j2s-cast
			  `(,(bitop op)
			    ,(asuint32 left tl) ,(mask32 right rhs))
			  #f 'uint32 type ctx))
		      (else
		       (j2s-cast
			  `(,(bitop op)
			    ,(asint32 left tl) ,(toint32 right tr ctx))
			  #f 'int32 type ctx))))
		  ((memq op '(& ^ BIT_OR))
		   `(if (and ,(j2s-fixnum? left) ,(j2s-fixnum? right))
			(js-int32-tointeger
			   (,(bitop op)
			    (fixnum->int32 ,left) (fixnum->int32 ,right)))
			(,(bitopjs op) ,left ,right %this)))
		  (else
		   `(,(bitopjs op)
		     ,(box left tl ctx) ,(box right tr ctx) %this))))))))

;*---------------------------------------------------------------------*/
;*    js-binop-add ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-binop-add loc op type lhs::J2SExpr rhs::J2SExpr mode return ctx)

   (define (j2sexpr-ascii? expr)
      (cond
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (j2sexpr-ascii? expr)))
	 ((isa? expr J2SString)
	  (with-access::J2SString expr (val)
	     (eq? (string-minimal-charset val) 'ascii)))
	 ((isa? expr J2SNumber)
	  #t)
	 ((isa? expr J2SCond)
	  (with-access::J2SCond expr (then else)
	     (and (j2sexpr-ascii? then) (j2sexpr-ascii? else))))
	 ((isa? expr J2SBinary)
	  (with-access::J2SBinary expr (lhs rhs)
	     (and (j2sexpr-ascii? lhs) (j2sexpr-ascii? rhs))))
	 (else
	  #f)))
      
   (define (ascii? expr x)
      (match-case x
	 ((& (and (? string?) (? (lambda (s) (eq? (string-minimal-charset s) 'ascii)))) . ?-)
	  #t)
	 ((js-integer->jsstring ?-)
	  #t)
	 ((js-real->jsstring ?-)
	  #t)
	 ((js-string-append-ascii ?- ?-)
	  #t)
	 ((if ?- (js-integer->jsstring ?-) (js-real->jsstring ?-))
	  #t)
	 (else
	  (when (isa? expr J2SExpr)
	     (j2sexpr-ascii? expr)))))

   (define (j2s-jsstring-append lhs rhs x y)
      (cond
	 ((eq? (j2s-type lhs) 'buffer)
	  (if (ascii? rhs y)
	      `(js-jsstring-append-buffer-ascii ,x ,y)
	      `(js-jsstring-append-buffer ,x ,y)))
	 ((context-get ctx :profile-mem)
	  `(js-jsstring-append-no-inline ,x ,y))
	 ((and (ascii? lhs x) (ascii? rhs y))
	  `(js-jsstring-append-ascii ,x ,y))
	 ((ascii? lhs x)
	  `(js-jsstring-append-ascii-xxx ,x ,y))
	 ((ascii? rhs x)
	  `(js-jsstring-append-xxx-ascii ,x ,y))
	 ((context-get ctx :optim-size)
	  `(js-jsstring-append-no-inline ,x ,y))
	 (else
	  `(js-jsstring-append ,x ,y))))
   
   (define (j2s-jsstring-append3 lhs mhs rhs mode return ctx)
      (let ((x (tostring (j2s-scheme lhs mode return ctx) (j2s-type lhs) ctx))
	    (u (tostring (j2s-scheme mhs mode return ctx) (j2s-type mhs) ctx))
	    (y (tostring (j2s-scheme rhs mode return ctx) (j2s-type rhs) ctx)))
	 (if (and (ascii? lhs x) (ascii? mhs u) (ascii? rhs y))
	     `(js-jsstring-append-ascii3 ,x ,u ,y)
	     `(js-jsstring-append3 ,x ,u ,y))))
   
   (define (str-append flip lhs rhs left right)
      (cond
	 ((equal? left (& "" (context-program ctx))) right)
	 ((equal? right (& "" (context-program ctx))) left)
	 (flip (j2s-jsstring-append rhs lhs right left))
	 (else (j2s-jsstring-append lhs rhs left right))))
   
   (define (add-string loc type left tl lhs right tr rhs mode return ctx flip)
      (str-append flip lhs rhs (tostring left tl ctx) (tostring right tr ctx)))

   (define (string-add? expr)
      (when (isa? expr J2SBinary)
	 (with-access::J2SBinary expr (op type lhs rhs)
	    (when (and (eq? op '+) (memq type '(string buffer)))
	       (or (memq (j2s-type lhs) '(string buffer))
		   (memq (j2s-type rhs) '(string buffer)))))))
   
   (define (string-add tl tr loc type lhs rhs mode return ctx)
      (cond
	 ((eq? type 'buffer)
	  (fast-add  tl tr loc type lhs rhs mode return ctx))
	 ((string-add? lhs)
	  (with-access::J2SBinary lhs ((llhs lhs) (lrhs rhs))
	     (j2s-jsstring-append3 llhs lrhs rhs mode return ctx)))
	 ((string-add? rhs)
	  (with-access::J2SBinary rhs ((lrhs lhs) (rrhs rhs))
	     (j2s-jsstring-append3 lhs lrhs rrhs mode return ctx)))
	 (else
	  (fast-add  tl tr loc type lhs rhs mode return ctx))))
   
   (define (fast-add tl tr loc type lhs rhs mode return ctx)
      (with-tmp lhs rhs mode return ctx
	 (lambda (left right)
	    (cond
	       ((memq tl '(string buffer))
		(add-string loc type left tl lhs right tr rhs
		    mode return ctx #f))
	       ((memq tr '(string buffer))
		(add-string loc type right tr rhs left tl lhs
		   mode return ctx #t))
	       ((eq? tl 'int32)
		(binop-int32-xxx '+ type lhs tl left rhs tr right ctx #f))
	       ((eq? tr 'int32)
		(binop-int32-xxx '+ type rhs tr right lhs tl left ctx #t))
	       ((eq? tl 'uint32)
		(binop-uint32-xxx '+ type lhs tl left rhs tr right ctx #f))
	       ((eq? tr 'uint32)
		(binop-uint32-xxx '+ type rhs tr right lhs tl left ctx #t))
	       ((eq? tl 'integer)
		(binop-integer-xxx '+ type lhs tl left rhs tr right ctx #f))
	       ((eq? tr 'integer)
		(binop-integer-xxx '+ type rhs tr right lhs tl left ctx #t))
	       ((eq? tl 'bint)
		(binop-bint-xxx '+ type lhs tl left rhs tr right ctx #f))
	       ((eq? tr 'bint)
		(binop-bint-xxx '+ type rhs tr right lhs tl left ctx #t))
	       ((eq? tl 'int53)
		(binop-int53-xxx '+ type lhs tl left rhs tr right ctx #f))
	       ((eq? tr 'int53)
		(binop-int53-xxx '+ type rhs tr right lhs tl left ctx #t))
	       ((eq? tl 'real)
		(binop-real-xxx '+ type lhs tl left rhs tr right ctx #f))
	       ((eq? tr 'real)
		(binop-real-xxx '+ type rhs tr right lhs tl left ctx #t))
	       ((eq? tl 'bigint)
		(binop-bigint-xxx '+ type lhs tl left rhs tr right ctx #f))
	       ((eq? tr 'bigint)
		(binop-bigint-xxx '+ type rhs tr right lhs tl left ctx #t))
	       ((and (memq type '(string buffer)) (eq? tl 'any) (eq? tr 'any))
		`(if (and (js-jsstring? ,left) (js-jsstring? ,right))
		     (js-jsstring-append ,left ,right)
		     ,(binop-any-any op type
			 (box left tl ctx)
			 (box right tr ctx)
			 #f)))
	       ((or (and (eq? tl 'number) (eq? tr 'any))
		    (and (eq? tl 'any) (eq? tr 'number)))
		(if-fixnums? left tl right tr
		   (binop-fixnum-fixnum/ctx ctx '+ type
		      (asfixnum left 'int53)
		      (asfixnum right 'int53)
		      #f)
		   (binop-any-any op type
		      (box left tl ctx)
		      (box right tr ctx)
		      #f)))
	       ((or (is-hint? lhs 'integer) (is-hint? rhs 'integer))
		(if-fixnums? left tl right tr
		   (binop-fixnum-fixnum/ctx ctx '+ type
		      (asfixnum left 'int53)
		      (asfixnum right 'int53)
		      #f)
		   (binop-any-any op type
		   (box left tl ctx)
		   (box right tr ctx)
		   #f)))
	       (else
		(binop-any-any op type
		   (box left tl ctx)
		   (box right tr ctx)
		   #f))))))

   (define (is-binary-string-add? x)
      (when (isa? x J2SBinary)
	 (with-access::J2SBinary x (op type)
	    (and (eq? op '+) (memq type '(string buffer))))))

   (define (j2s-scheme-as-string expr::J2SExpr mode return ctx)
      (let ((x (j2s-scheme expr mode return ctx)))
	 (if (memq (j2s-type expr) '(string buffer))
	     x
	     `(js-toprimitive-for-string ,x %this))))
   
   (define (small-add tl tr loc type lhs rhs mode return ctx)
      ;; when optimizing code size, avoid deep nested calls generated
      ;; for long string concatenation that forces register allocation
      ;; to cleanup a lot of the temporaries
      (if (is-binary-string-add? lhs)
	  (let* ((%str (gensym '%str))
		 (target (list %str '__dummy__)))
	     `(let (,target)
		 ,@(let loop ((expr lhs))
		      (if (is-binary-string-add? expr)
			  (with-access::J2SBinary expr (lhs rhs loc)
			     (let ((lx (loop lhs)))
				(with-access::J2SExpr lhs ((lloc loc))
				   (append lx
				      `((set! ,%str
					   (js-jsstring-append-no-inline ,%str
					      ,(j2s-scheme-as-string rhs mode return ctx))))))))
			  (begin
			     (set-cdr! target
				(list (j2s-scheme-as-string expr mode return ctx)))
			     '())))
		 ,(fast-add tl tr loc 'string
		     (instantiate::J2SHopRef
			(loc loc)
			(id %str)
			(type 'string))
		     rhs mode return ctx)))
	  (fast-add tl tr loc type lhs rhs mode return ctx)))
   
   (define (add loc type lhs rhs mode return ctx)
      (let ((tl (j2s-type lhs))
	    (tr (j2s-type rhs)))
	 (cond
	    ((and (or (memq tl '(string buffer)) (memq tr '(string buffer)))
		  (context-get ctx :optim-size))
	     (small-add tl tr loc type lhs rhs mode return ctx))
	    ((and (memq tl '(string buffer)) (memq tr '(string buffer)))
	     (string-add tl tr loc type lhs rhs mode return ctx))
	    (else
	     (fast-add tl tr loc type lhs rhs mode return ctx)))))

   (if (type-number? type)
       (js-arithmetic-addsub loc op type lhs rhs mode return ctx)
       (add loc type lhs rhs mode return ctx)))

;*---------------------------------------------------------------------*/
;*    js-arithmetic-addsub ...                                         */
;*---------------------------------------------------------------------*/
(define (js-arithmetic-addsub loc op::symbol type lhs::J2SExpr rhs::J2SExpr
	   mode return ctx)
   (with-tmp lhs rhs mode return ctx
      (lambda (left right)
	 (let ((tl (j2s-type lhs))
	       (tr (j2s-type rhs)))
	    (epairify loc
	       (cond
		  ((and (eq? type 'any)
			(or (and (eq? tl 'number) (memq tr '(int32 uint32)))
			    (and (memq tl '(int32 uint32)) (eq? tr 'number))))
		   ;; type is forced to any on prefix/suffix generic increments
		   ;; this case is used to generate smaller codes
		   (binop-any-any op type
		      (box left tl ctx)
		      (box right tr ctx)
		      #f))
		  ((eq? tl 'int32)
		   (binop-int32-xxx op type lhs tl left rhs tr right ctx #f))
		  ((eq? tr 'int32)
		   (binop-int32-xxx op type rhs tr right lhs tl left ctx #t))
		  ((eq? tl 'uint32)
		   (binop-uint32-xxx op type lhs tl left rhs tr right ctx #f))
		  ((eq? tr 'uint32)
		   (binop-uint32-xxx op type rhs tr right lhs tl left ctx #t))
		  ((eq? tl 'integer)
		   (binop-integer-xxx op type lhs tl left rhs tr right ctx #f))
		  ((eq? tr 'integer)
		   (binop-integer-xxx op type rhs tr right lhs tl left ctx #t))
		  ((eq? tl 'bint)
		   (binop-bint-xxx op type lhs tl left rhs tr right ctx #f))
		  ((eq? tr 'bint)
		   (binop-bint-xxx op type rhs tr right lhs tl left ctx #t))
		  ((eq? tl 'int53)
		   (binop-int53-xxx op type lhs tl left rhs tr right ctx #f))
		  ((eq? tr 'int53)
		   (binop-int53-xxx op type rhs tr right lhs tl left ctx #t))
		  ((eq? tl 'real)
		   (binop-real-xxx op type lhs tl left rhs tr right ctx #f))
		  ((eq? tr 'real)
		   (binop-real-xxx op type rhs tr right lhs tl left ctx #t))
		  ((eq? tl 'bigint)
		   (binop-bigint-xxx op type lhs tl left rhs tr right ctx #f))
		  ((eq? tr 'bigint)
		   (binop-bigint-xxx op type rhs tr right lhs tl left ctx #t))
		  ((and (is-hint? lhs 'real) (is-hint? rhs 'real))
		   (if-flonums? left tl right tr
		      (binop-flonum-flonum (real-op op type lhs rhs left right #f) type
			 (asreal left 'real)
			 (asreal right 'real)
			 #f)
		      (if (and (eq? tl 'number) (eq? tr 'number))
			  (if (and (fresh-real? lhs left) (fresh-real? rhs right))
			      (binop-number-number! op type
				   (box left tl ctx)
				   (box right tr ctx)
				   #f)
			      (binop-number-number op type
				 (box left tl ctx)
				 (box right tr ctx)
				 #f))
			  (binop-any-any op type
			     (box left tl ctx)
			     (box right tr ctx)
			     #f))))
		  ((and (eq? op '-) (or (is-hint? lhs 'real) (is-hint? rhs 'real)))
		   (if-flonums? left tl right tr
		      (binop-flonum-flonum (real-op op type lhs rhs left right #f) type
			 (asreal left 'real)
			 (asreal right 'real)
			 #f)
		      (if (and (eq? tl 'number) (eq? tr 'number))
			 (if (and (fresh-real? lhs left) (fresh-real? rhs right))
			     (binop-number-number! op type
				(box left tl ctx)
				(box right tr ctx)
				#f)
			     (binop-number-number op type
				(box left tl ctx)
				(box right tr ctx)
				#f))
			  (binop-any-any op type
			     (box left tl ctx)
			     (box right tr ctx)
			     #f))))
		  ((and (eq? tl 'number) (eq? tr 'number))
		   (if-fixnums? left tl right tr
		      (binop-fixnum-fixnum/ctx ctx op type
			 (asfixnum left 'int53)
			 (asfixnum right 'int53)
			 #f)
		      (if-flonums? left tl right tr
			 (binop-flonum-flonum (real-op op type lhs rhs left right #f) type
			    (asreal left 'real)
			    (asreal right 'real)
			    #f)
			 (if (and (fresh-real? lhs left) (fresh-real? rhs right))
			     (binop-number-number! op type
				(box left tl ctx)
				(box right tr ctx)
				#f)
			     (binop-number-number op type
				(box left tl ctx)
				(box right tr ctx)
				#f)))))
		  ((or (eq? tl 'number) (eq? tr 'number))
		   (if-fixnums? left tl right tr
		      (binop-fixnum-fixnum/ctx ctx op type
			 (asfixnum left 'int53)
			 (asfixnum right 'int53)
			 #f)
		      (if-flonums? left tl right tr
			 (binop-flonum-flonum (real-op op type lhs rhs left right #f) type
			    (asreal left 'real)
			    (asreal right 'real)
			    #f)
			 (binop-any-any op type
			    (box left tl ctx)
			    (box right tr ctx)
			    #f))))
		  ((and (eq? op '+) (memq type '(string buffer)))
		   `(if (and (js-jsstring? ,left) (js-jsstring? ,right))
			(js-jsstring-append ,left ,right)
			,(binop-any-any op type
			    (box left tl ctx)
			    (box right tr ctx)
			    #f)))
		  (else
		   (if-fixnums? left tl right tr
		      (binop-fixnum-fixnum/ctx ctx op type
			 (asfixnum left 'int53)
			 (asfixnum right 'int53)
			 #f)
		      (if-flonums? left tl right tr
			 (binop-flonum-flonum (real-op op type lhs rhs left right #f) type
			    (asreal left 'real)
			    (asreal right 'real)
			    #f)
			 (binop-any-any op type
			    (box left tl ctx)
			    (box right tr ctx)
			    #f))))))))))

;*---------------------------------------------------------------------*/
;*    js-arithmetic-mul ...                                            */
;*---------------------------------------------------------------------*/
(define (js-arithmetic-mul loc type lhs::J2SExpr rhs::J2SExpr
	   mode return ctx)
   
   (define (mulbx lhs rhs tl tr mode return ctx)
      (let ((bitrsh (j2s-exp2bx rhs mode return ctx))
	    (left (j2s-scheme lhs mode return ctx)))
	 (cond
	    ((and (eq? tl 'bigint) (eq? tr 'bigint))
	     (if bitrsh
		 `(bit-lshbx ,left ,bitrsh)
		 `(*bx ,left ,(j2s-scheme rhs mode return ctx))))
	    ((eq? tr 'bigint)
	     (if bitrsh
		 `(bit-lshjsbx ,left ,bitrsh %this)
		 `(*jsbx ,left ,(j2s-scheme rhs mode return ctx) %this)))
	    (else
	     `(*bxjs ,left ,(j2s-scheme rhs mode return ctx) %this)))))
   
   (let ((tl (j2s-type lhs))
	 (tr (j2s-type rhs)))
      (cond
	 ((eq? tl 'bigint)
	  (mulbx lhs rhs tl tr mode return ctx))
	 ((eq? tr 'bigint)
	  (mulbx rhs lhs tr tl mode return ctx))
	 (else
	  (with-tmp lhs rhs mode return ctx
	     (lambda (left right)
		(cond
		   ((and (eq? (j2s-type lhs) 'real) (eq? (j2s-type rhs) 'real))
		    (binop-flonum-flonum '* type left right #f))
		   ((and (eq? tl 'int32) (not (eq? type 'real)))
		    (binop-int32-xxx '* type lhs tl left rhs tr right ctx #f))
		   ((and (eq? tr 'int32) (not (eq? type 'real)))
		    (binop-int32-xxx '* type rhs tr right lhs tl left ctx #t))
		   ((and (eq? tl 'uint32) (not (eq? type 'real)))
		    (binop-uint32-xxx '* type lhs tl left rhs tr right ctx #f))
		   ((and (eq? tr 'uint32) (not (eq? type 'real)))
		    (binop-uint32-xxx '* type rhs tr right lhs tl left ctx #t))
		   ((and (eq? tl 'integer) (not (eq? type 'real)))
		    (binop-integer-xxx '* type lhs tl left rhs tr right ctx #f))
		   ((and (eq? tr 'integer) (not (eq? type 'real)))
		    (binop-integer-xxx '* type rhs tr right lhs tl left ctx #t))
		   ((and (eq? tl 'bint) (not (eq? type 'real)))
		    (binop-bint-xxx '* type lhs tl left rhs tr right ctx #f))
		   ((and (eq? tr 'bint) (not (eq? type 'real)))
		    (binop-bint-xxx '* type rhs tr right lhs tl left ctx #t))
		   ((and (eq? tl 'int53) (not (eq? type 'real)))
		    (binop-int53-xxx '* type lhs tl left rhs tr right ctx #f))
		   ((and (eq? tr 'int53) (not (eq? type 'real)))
		    (binop-int53-xxx '* type rhs tr right lhs tl left ctx #t))
		   ((eq? tl 'real)
		    (binop-real-xxx '* type lhs tl left rhs tr right ctx #f))
		   ((eq? tr 'real)
		    (binop-real-xxx '* type rhs tr right lhs tl left ctx #t))
		   ((eq? type 'real)
		    (if-flonums? left tl right tr
		       (binop-flonum-flonum (real-op '* type lhs rhs left right #f) type
			  (asreal left 'real)
			  (asreal right 'real)
			  #f)
		       (binop-any-any '* type
			  (tonumeric left tl ctx)
			  (tonumeric right tr ctx)
			  #f)))
		   ((or (is-hint? lhs 'real) (is-hint? rhs 'real))
		    (if-flonums? left tl right tr
		       (binop-flonum-flonum (real-op '* type lhs rhs left right #f) type
			  (asreal left 'real)
			  (asreal right 'real)
			  #f)
		       (binop-any-any '* type
			  (box left tl ctx)
			  (box right tr ctx)
			  #f)))
		   (else
		    (if-fixnums? left tl right tr
		       (binop-fixnum-fixnum/ctx ctx '* type
			  (asfixnum left 'int53)
			  (asfixnum right 'int53)
			  #f)
		       (if-flonums? left tl right tr
			  (binop-flonum-flonum (real-op '* type lhs rhs left right #f) type
			     (asreal left 'real)
			     (asreal right 'real)
			     #f)
			  (binop-any-any '* type
			     (box left tl ctx)
			     (box right tr ctx)
			     #f)))))))))))

;*---------------------------------------------------------------------*/
;*    js-arithmetic-expt ...                                           */
;*---------------------------------------------------------------------*/
(define (js-arithmetic-expt loc type lhs::J2SExpr rhs::J2SExpr
	   mode return ctx)
   (let ((tl (j2s-type lhs))
	 (tr (j2s-type rhs)))
      (if (and (eq? tl 'bigint) (eq? tr 'bigint))
	  (js-exptbx lhs rhs mode return ctx)
	  (with-tmp lhs rhs mode return ctx
	     (lambda (left right)
		(epairify loc
		   (cond
		      ((and (eq? tl 'int32) (eq? tr 'int32) (eq? type 'int32))
		       `(expts32 ,left ,right))
		      ((and (eq? tl 'uint32) (eq? tr 'uint32) (eq? type 'uint32))
		       `(exptu32 ,left ,right))
		      ((and (eq? tl 'int30) (eq? tr 'int30) (eq? type 'int30))
		       `(exptfx ,left ,right))
		      ((and (eq? tl 'real) (eq? tr 'real) (eq? type 'real))
		       `(exptfl ,left ,right))
		      (else
		       (let ((expr `(**js ,(box left tl ctx) ,(box right tr ctx) %this)))
			  (case type
			     ((uint32) (asuint32 expr 'real))
			     ((int32) (asint32 expr 'real))
			     ((fixnum int30) (asfixnum expr 'real))
			     (else expr)))))))))))

;*---------------------------------------------------------------------*/
;*    js-exptbx ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-exptbx lhs::J2SExpr rhs::J2SExpr mode return ctx)
   
   (define (exptbx lhs rhs)
      `(exptbx
	  ,(j2s-scheme lhs mode return ctx)
	  ,(j2s-scheme rhs mode return ctx)))

   (define (bitlshbx val)
      `(bit-lshbx #z1 ,(j2s-scheme val mode return ctx)))
   
   (if (isa? lhs J2SNumber)
       (with-access::J2SNumber lhs (val)
	  (if (and (=bx val #z2) (is-bigint-call? rhs))
	      (with-access::J2SCall rhs (args)
		 (bitlshbx (car args)))
	      (exptbx lhs rhs)))
       (exptbx lhs rhs)))

;*---------------------------------------------------------------------*/
;*    is-bigint-call? ...                                              */
;*---------------------------------------------------------------------*/
(define (is-bigint-call? this::J2SExpr)
   (when (isa? this J2SCall)
      (with-access::J2SCall this (fun args)
	 (and (is-builtin-ref? fun 'BigInt) (=fx (length args) 1)))))
   
;*---------------------------------------------------------------------*/
;*    js-arithmetic-div ...                                            */
;*---------------------------------------------------------------------*/
(define (js-arithmetic-div loc type lhs rhs
	   mode return ctx)

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

   (define (positive? n)
      (with-access::J2SExpr n (range)
	 (and (interval? range) (>= (interval-min range) #l0))))

   (define (/js x y)
      (if (eq? type 'real)
	  `(/jsfl ,x ,y %this)
	  `(/js ,x ,y %this)))
	  
   (define (div-power2 k)
      (let ((n (gensym 'n)))
	 (case (j2s-type lhs)
	    ((uint32)
	     `(let ((,n ,(j2s-scheme lhs mode return ctx)))
		 (if (=u32 (bit-andu32
			      ,n ,(fixnum->uint32 (-fx (bit-lsh 1 k) 1)))
			#u32:0)
		     (js-uint32-tointeger (bit-rshu32 ,n ,k))
		     (/fl (uint32->flonum ,n) (fixnum->flonum ,(bit-lsh 1 k))))))
	    ((int32)
	     `(let ((,n ,(j2s-scheme lhs mode return ctx)))
		 (if (=s32 (bit-ands32
			      ,n ,(fixnum->int32 (-fx (bit-lsh 1 k) 1)))
			#s32:0)
		     ,(if (positive? lhs)
			  `(js-int32-tointeger (bit-rshs32 ,n ,k))
			  `(js-int32-tointeger (/pow2s32 ,n ,k)))
		     (/fl (int32->flonum ,n) (fixnum->flonum ,(bit-lsh 1 k))))))
	    (else
	     `(let ((,n ,(j2s-scheme lhs mode return ctx)))
		 (if ,(if (memq (j2s-type lhs) '(int32 uint32))
			  `(=fx (bit-and ,n ,(-fx (bit-lsh 1 k) 1)) 0)
			  `(and ,(j2s-fixnum? n)
				(=fx (bit-and ,n ,(-fx (bit-lsh 1 k) 1)) 0)))
		     ,(if (positive? lhs)
			  `(bit-rsh ,n ,k)
			  `(/pow2fx ,n ,k))
		     ,(/js n (bit-lsh 1 k))))))))

   (define (div-power2fl k)
      (let ((n (gensym 'n)))
	 (case (j2s-type lhs)
	    ((uint32)
	     `(let ((,n ,(j2s-scheme lhs mode return ctx)))
		 (if (=u32 (bit-andu32
			      ,n ,(fixnum->uint32 (-fx (bit-lsh 1 k) 1)))
			#u32:0)
		     (uint32->flonum (bit-rshu32 ,n ,k))
		     (/fl (uint32->flonum ,n) (fixnum->flonum ,(bit-lsh 1 k))))))
	    ((int32)
	     `(let ((,n ,(j2s-scheme lhs mode return ctx)))
		 (if (=s32 (bit-ands32
			      ,n ,(fixnum->int32 (-fx (bit-lsh 1 k) 1)))
			#s32:0)
		     ,(if (positive? lhs)
			  `(int32->flonum (bit-rshs32 ,n ,k))
			  `(int32->flonum (/pow2s32 ,n ,k)))
		     (/fl (int32->flonum ,n) (fixnum->flonum ,(bit-lsh 1 k))))))
	    (else
	     `(let ((,n ,(j2s-scheme lhs mode return ctx)))
		 (if ,(if (memq (j2s-type lhs) '(int32 uint32))
			  `(=fx (bit-and ,n ,(-fx (bit-lsh 1 k) 1)) 0)
			  `(and ,(j2s-fixnum? n)
				(=fx (bit-and ,n ,(-fx (bit-lsh 1 k) 1)) 0)))
		     ,(if (positive? lhs)
			  `(fixnum->flonum (bit-rsh ,n ,k))
			  `(fixnum->flonum (/pow2fx ,n ,k)))
		     ,(/js n (bit-lsh 1 k))))))))

   (define (divs32 left right tl tr)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(uint32->int32 (/u32 ,left ,right)))
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(/s32 ,left ,right))
	 ((and (eq? tl 'int32) (eq? tr 'uint32) (inrange-int32? rhs))
	  `(/s32 ,left ,(asint32 right tr)))
	 ((and (eq? tr 'uint32) (inrange-int32? rhs))
	  (if-fixnum? left tl
	     `(fixnum->int32 (/fx ,(asfixnum left tl) ,(asfixnum right tr)))
	     `(fixnum->int32 (flonum->fixnum (/fl ,(asreal left tl) ,(asreal right tr))))))
	 ((eq? tr 'int32)
	  (if-fixnum? left tl
	     `(fixnum->int32 (/fx ,(asfixnum left tl) ,(asfixnum right tr)))
	     `(fixnum->int32 (flonum->fixnum (/fl ,(asreal left tl) ,(asreal right tr))))))
	 ((or (eq? tl 'real) (eq? tl 'real))
	  `(fixnum->int32 (flonum->fixnum (/fl ,(asreal left tl) ,(asreal right tr)))))
	 (else
	  (if-fixnums? left tl right tr
	     `(fixnum->int32 (/fx ,left ,right))
	     `(js-toint32 (/js ,left ,right %this) %this)))))

   (define (divu32 left right tl tr)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(/u32 ,left ,right))
	 ((and (eq? tl 'int53) (eq? tr 'int53))
	  `(fixnum->uint32 (/fx ,left ,right)))
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(int32->uint32 (/s32 ,left ,right)))
	 ((and (eq? tr 'int32) (inrange-uint32? rhs))
	  `(/u32 ,left ,(asuint32 right tr)))
	 (else
	  (if-fixnums? left tl right tr
	     `(fixnum->uint32 (/fx ,(asfixnum left tl) ,(asfixnum right tr)))
	     `(fixnum->uint32 (flonum->fixnum (/fl ,(asreal left tl) ,(asreal right tr))))))))

   (define (divfl left right tl tr)
      (cond
	 ((eq? tl 'uint32)
	  (if (eq? tr 'uint32)
	      `(if (and (not (=u32 ,right #u32:0))
			(=u32 (remainderu32 ,left ,right) #u32:0))
		   (uint32->flonum (/u32 ,left ,right))
		   (/fl ,(asreal left tl) ,(asreal right tr)))
	      `(/fl ,(asreal left tl) ,(todouble right tr ctx))))
	 ((eq? tl 'int32)
	  (if (eq? tr 'int32)
	      `(if (and (not (=s32 ,right #s32:0))
			(=s32 (remainders32 ,left ,right) #s32:0))
		   (int32->flonum (/s32 ,left ,right))
		   (/fl ,(asreal left tl) ,(asreal right tr)))
	      `(/fl ,(asreal left tl) ,(todouble right tr ctx))))
	 ((eq? tr 'uint32)
	  `(/fl ,(todouble left tl ctx) ,(asreal right tr)))
	 ((eq? tr 'int32)
	  `(/fl ,(todouble left tl ctx) ,(asreal right tr)))
	 ((eq? tl 'integer)
	  (if (eq? tr 'integer)
	      `(if (and (not (=fx ,right 0))
			(=fx (remainderfx ,left ,right) 0))
		   (fixnum->flonum (/fx ,left ,right))
		   (/fl ,(todouble left tl ctx) ,(todouble right tr ctx)))
	      `(/fl ,(todouble left tl ctx) ,(todouble right tr ctx))))
	 ((and (eq? tl 'int53) (m64? (context-conf ctx)))
	  (if (eq? tr 'integer)
	      `(if (and (not (=fx ,right 0))
			(=fx (remainderfx ,left ,right) 0))
		   (fixnum->flonum (/fx ,left ,right))
		   (/fl ,(todouble left tl ctx) ,(todouble right tr ctx)))
	      `(/fl ,(todouble left tl ctx) ,(todouble right tr ctx))))
	 ((or (eq? tl 'real) (eq? tr 'real))
	  `(/fl ,(todouble left tl ctx) ,(todouble right tr ctx)))
	 ((eq? tr 'integer)
	  (/js (todouble left tl ctx) (asreal right tr)))
	 ((eq? type 'real)
	  (if-flonums? left tl right tr
	     `(/fl ,left ,right)
	     (/js left right)))
	 (else
	  (if-fixnums? left tl right tr
	     `(if (and (not (=fx ,right 0))
		       (=fx (remainderfx ,left ,right) 0))
		  (/fx ,left ,right)
		  (/fl ,(asreal left 'bint) ,(asreal right 'bint)))
	     (if-flonums? left tl right tr
		`(/fl ,left ,right)
		(/js left right))))))

   (define (divbx lhs rhs tl tr)
      (let ((bitrsh (j2s-exp2bx rhs mode return ctx))
	    (left (j2s-scheme lhs mode return ctx)))
	 (cond
	    ((and (eq? tl 'bigint) (eq? tr 'bigint))
	     (if bitrsh
		 `(bit-rshbx ,left ,bitrsh)
		 `(/bx ,left ,(j2s-scheme rhs mode return ctx))))
	    ((eq? tr 'bigint)
	     (if bitrsh
		 `(bit-rshjsbx ,left ,bitrsh %this)
		 `(/jsbx ,left ,(j2s-scheme rhs mode return ctx) %this)))
	    (else
	     `(/bxjs ,left ,(j2s-scheme rhs mode return ctx) %this)))))

   (define (divjs left right tl tr)
      (cond
	 ((eq? tl 'uint32)
	  (if (eq? tr 'uint32)
	      `(if (and (not (=u32 ,right #u32:0))
			(=u32 (remainderu32 ,left ,right) #u32:0))
		   (js-uint32-tointeger (/u32 ,left ,right))
		   (/integer ,(asreal left tl)
		      ,(asreal right tr)))
	      `(/integer ,(asreal left tl) ,(todouble right tr ctx))))
	 ((eq? tl 'int32)
	  (if (eq? tr 'int32)
	      `(if (and (not (=s32 ,right #s32:0))
			(=s32 (remainders32 ,left ,right) #s32:0))
		   (js-int32-tointeger (/s32 ,left ,right))
		   (/integer ,(asreal left tl)
		      ,(asreal right tr)))
	      `(/integer ,(asreal left tl) ,(todouble right tr ctx))))
	 ((eq? tr 'uint32)
	  `(/integer ,(todouble left tl ctx) ,(asreal right tr)))
	 ((eq? tr 'int32)
	  `(/integer ,(todouble left tl ctx) ,(asreal right tr)))
	 ((eq? tl 'integer)
	  `(/integer ,(todouble left tl ctx) ,(todouble right tr ctx)))
	 ((or (eq? tl 'real) (eq? tr 'real))
	  `(/fl ,(todouble left tl ctx) ,(todouble right tr ctx)))
	 ((eq? tr 'integer)
	  `(/integer ,(todouble left tl ctx) ,(asreal right tr)))
	 ((eq? type 'real)
	  (if-flonums? left tl right tr
	     `(/fl ,left ,right)
	     (/js left right)))
	 (else
	  (if-fixnums? left tl right tr
	     `(/integer ,(asreal left 'bint) ,(asreal right 'bint))
	     (if-flonums? left tl right tr
		`(/fl ,left ,right)
		(/js left right))))))
   
   (let ((k (power2 rhs))
	 (tl (j2s-type lhs))
	 (tr (j2s-type rhs)))
      (cond
	 ((and k (not (memq type '(int32 uint32))))
	  (case type
	     ((real) (div-power2fl k))
	     (else (div-power2 k))))
	 ((or (eq? tl 'bigint) (eq? tr 'bigint))
	  (divbx lhs rhs tl tr))
	 (else
	  (with-tmp lhs rhs mode return ctx
	     (lambda (left right)
		(epairify loc
		   (case type
		      ((int32) (divs32 left right tl tr))
		      ((uint32) (divu32 left right tl tr))
		      ((real) (divfl left right tl tr))
		      (else (divjs left right tl tr))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-exp2bx ...                                                   */
;*    -------------------------------------------------------------    */
;*    If this is of the form exp(2n,b), returns b.                     */
;*---------------------------------------------------------------------*/
(define (j2s-exp2bx this::J2SExpr mode return ctx)
   
   (define (tofixnum this::J2SExpr)
      (cond
	 ((isa? this J2SCast)
	  (with-access::J2SCast this (expr)
	     (tofixnum expr)))
	 ((isa? this J2SNumber)
	  (with-access::J2SNumber this (val)
	     (cond
		((and (uint32? val) (inrange-int32? this))
		 (uint32->fixnum val))
		((int32? val)
		 (int32->fixnum val))
		((fixnum? val)
		 val)
		((and (bignum? val)
		      (<bx val (bit-lshbx #z1 30))
		      (>bx val (-bx #z0 (bit-lshbx #z1 30))))
		 (bignum->fixnum val))
		(else
		 #f))))
	 ((and (eq? (j2s-type this) 'uint32) (m64? (context-conf ctx)))
	  `(uint32->fixnum ,(j2s-scheme this mode return ctx)))
	 ((eq? (j2s-type this) 'int32)
	  `(int32->fixnum ,(j2s-scheme this mode return ctx)))
	 ((eq? (j2s-type this) 'int53)
	  (j2s-scheme this mode return ctx))
	 (else
	  (let ((tmp (gensym 'tmp)))
	     `(let ((,tmp ,(j2s-scheme this mode return ctx)))
		 (if (fixnum? ,tmp)
		     ,tmp
		     (js-raise-type-error %this "Cannot mix BigInt and other types, use explict conversions: ~a" ,tmp)))))))

   (cond
      ((isa? this J2SBinary)
       (with-access::J2SBinary this (op lhs rhs)
	  (when (and (eq? op '**) (isa? lhs J2SNumber))
	     (with-access::J2SNumber lhs (val)
		(when (=bx val #z2)
		   (cond
		      ((isa? rhs J2SNumber)
		       (with-access::J2SNumber rhs (val)
			  (bignum->fixnum val)))
		      ((is-bigint-call? rhs)
		       (with-access::J2SCall rhs (args)
			  (tofixnum (car args))))
		      (else
		       #f)))))))
      ((isa? this J2SParen)
       (with-access::J2SParen this (expr)
	  (j2s-exp2bx expr mode return ctx)))
      (else
       #f)))
		  
;*---------------------------------------------------------------------*/
;*    js-arithmetic-% ...                                              */
;*---------------------------------------------------------------------*/
(define (js-arithmetic-% loc type lhs rhs mode return ctx)
   
   (define (number totype)
      ;; if the range analysis has shown that the result is an uint, then
      ;; the library function will always return an int
      (if (eq? totype 'uint32) 'integer 'number))
   
   (define (%bx lhs rhs tl tr mode return ctx)
      (let ((bitrsh (j2s-exp2bx rhs mode return ctx))
	    (left (j2s-scheme lhs mode return ctx)))
	 (cond
	    ((and (eq? tl 'bigint) (eq? tr 'bigint))
	     (if bitrsh
		 `(bit-maskxn ,left ,bitrsh)
		 `(remainderbx ,left ,(j2s-scheme rhs mode return ctx))))
	    ((eq? tr 'bigint)
	     (if bitrsh
		 `(bit-masknn ,left ,bitrsh %this)
		 `(%$$NX ,left ,(j2s-scheme rhs mode return ctx) %this)))
	    (else
	     `(%$$XN ,left ,(j2s-scheme rhs mode return ctx) %this)))))

   (let ((tl (j2s-type lhs))
	 (tr (j2s-type rhs)))
      (if (or (eq? tl 'bigint) (eq? tr 'bigint))
	  (%bx lhs rhs tl tr mode return ctx)
	  (with-tmp lhs rhs mode return ctx
	     (lambda (left right)
		(epairify loc
		   (cond
		      ((and (eq? tl 'int32) (eq? tr 'int32))
		       (cond
			  ((inrange-positive? rhs)
			   (j2s-cast `(remainders32 ,left ,right)
			      rhs 'int32 type ctx))
			  ((=s32 right #s32:0)
			   +nan.0)
			  (else
			   (remainders32-minus-zero left right
			      type lhs rhs mode return ctx))))
		      ((and (eq? tl 'uint32) (eq? tr 'uint32))
		       (cond
			  ((inrange-positive? rhs)
			   (j2s-cast `(remainderu32 ,left ,right)
			      lhs 'uint32 type ctx))
			  (else
			   `(if (=u32 ,right #u32:0)
				+nan.0
				,(j2s-cast `(remainderu32 ,left ,right)
				    lhs 'uint32 type ctx)))))
		      ((and (eq? tl 'integer) (eq? tr 'integer))
		       (with-tmp lhs rhs mode return ctx
			  (lambda (left right)
			     (cond
				((and (j2s-number? right) (= right 0))
				 +nan.0)
				((m64? (context-conf ctx))
				 (j2s-cast `(%$$II ,left ,right)
				    #f (number type) type ctx))
				((and (inrange-int32? lhs) (inrange-int32? rhs))
				 (j2s-cast `(%$$II ,left ,right)
				    #f (number type) type ctx))
				(else
				 (j2s-cast `(%$$NN ,left ,right)
				    #f (number type) type ctx))))))
		      ((and (eq? tr 'uint32) (inrange-positive? lhs))
		       (cond
			  ((inrange-int32? rhs)
			   (cond
			      ((memq tl '(int32 uint32 bint))
			       (if (or (inrange-int30? rhs) (inrange-int30? lhs))
				   (j2s-cast
				      `(remainderfx ,(asfixnum left tl)
					  ,(asfixnum right tr))
				      lhs 'int30 type ctx)
				   (j2s-cast
				      `(remainderfx ,(asfixnum left tl)
					  ,(asfixnum right tr))
				      lhs 'bint type ctx)))
			      ((eq? tl 'int53)
			       (j2s-cast
				  `(remainderfx ,left ,(asfixnum right tr))
				  lhs 'bint type ctx))
			      ((eq? (number type) 'integer)
			       `(if ,(j2s-fixnum? left)
				    ,(j2s-cast
					`(remainderfx ,left ,(asfixnum right tr))
					lhs 'bint type ctx)
				    ,(j2s-cast `(%$$NZ ,(tonumeric left tl ctx)
						   ,(tonumeric right tr ctx))
					lhs (number type) type ctx)))
			      (else
			       `(if ,(j2s-fixnum? left)
				    ,(j2s-cast `(remainderfx ,left
						   ,(asfixnum right tr))
					lhs 'bint type ctx)
				    ,(j2s-cast `(%$$NZ ,(tonumeric left tl ctx)
						   ,(tonumeric right tr ctx))
					lhs (number type) type ctx)))))
			  ((m64? (context-conf ctx))
			   `(if ,(j2s-fixnum? left)
				,(j2s-cast `(remainderfx ,left
					       ,(asfixnum right tr))
				    lhs (number type) type ctx)
				,(j2s-cast `(%$$NZ ,(tonumeric64 left tl ctx)
					       ,(tonumeric64 right tr ctx))
				    lhs (number type) type ctx)))
			  (else
			   (j2s-cast `(%$$NZ ,(tonumeric32 left tl ctx)
					 ,(tonumeric32 right tr ctx))
			      lhs (number type) type ctx))))
		      ((eq? type 'real)
		       (cond
			  ((and (eq? tl 'real) (eq? tr 'real))
			   `(%$$FF ,left ,right))
			  ((eq? tl 'real)
			   (cond
			      ((and (m64? (context-conf ctx)) (eq? tr 'int53))
			       `(%$$FF ,left (fixnum->flonum ,right)))
			      (else
			       `(%$$FN ,left ,(tonumeric right tr ctx)))))
			  ((eq? tr 'real)
			   (cond
			      ((and (m64? (context-conf ctx)) (eq? tl 'int53))
			       `(%$$FF (fixnum->flonum ,left) ,right))
			      (else
			       `(%$$NF ,(tonumeric left tl ctx) ,right))))
			  (else
			   (j2s-cast `(%$$NN ,(tonumeric left tl ctx)
					 ,(tonumeric right tr ctx))
			      lhs (number type) type ctx))))
		      ((m64? (context-conf ctx))
		       (cond
			  ((and (j2s-number? right) (not (= right 0)))
			   (j2s-cast `(%$$NZ ,(tonumeric64 left tl ctx)
					 ,(tonumeric64 right tr ctx))
			      lhs (number type) type ctx))
			  ((and (type-integer? tl) (type-integer? tr))
			   (j2s-cast `(%$$II ,(tonumeric64 left tl ctx)
					 ,(tonumeric64 right tr ctx))
			      lhs (number type) type ctx))
			  (else
			   (j2s-cast `(%$$__ ,(tonumeric64 left tl ctx)
					 ,(tonumeric64 right tr ctx) %this)
			      lhs (number type) type ctx))))
		      (else
		       (cond
			  ((and (j2s-number? right) (not (= right 0)))
			   (j2s-cast `(%$$NZ ,(tonumeric32 left tl ctx)
					 ,(tonumeric32 right tr ctx))
			      lhs (number type) type ctx))
			  ((and (type-integer? tl) (type-integer? tr)
				(inrange-int32? lhs) (inrange-int32? rhs))
			   (j2s-cast `(%$$II ,(tonumeric32 left tl ctx)
					 ,(tonumeric32 right tr ctx))
			      lhs (number type) type ctx))
			  (else
			   (j2s-cast `(%$$__ ,(tonumeric32 left tl ctx)
					 ,(tonumeric32 right tr ctx) %this)
			      lhs (number type) type ctx)))))))))))

;*---------------------------------------------------------------------*/
;*    remainders32-minus-zero ...                                      */
;*    -------------------------------------------------------------    */
;*    -1 % -1 must produce -0.0                                        */
;*---------------------------------------------------------------------*/
(define (remainders32-minus-zero left right type lhs rhs mode return ctx)
   (let ((tmp (gensym 'tmp)))
      `(if (=s32 ,right #s32:0)
	   +nan.0
	   (let ((,tmp ,(j2s-cast `(remainders32 ,left ,right)
			   lhs 'int32 type ctx)))
	      (if (and (=s32 ,tmp #s32:0) (<s32 ,left #s32:0))
		  -0.0
		  ,tmp)))))

;*---------------------------------------------------------------------*/
;*    asint32 ...                                                      */
;*    -------------------------------------------------------------    */
;*    all the asXXX functions convert an expression that is statically */
;*    known to be convertible (because of the range analysis) into the */
;*    destination type.                                                */
;*---------------------------------------------------------------------*/
(define (asint32 val type::symbol)
   (case type
      ((int32)
       val)
      ((uint32)
       (if (uint32? val)
	   (uint32->int32 val)
	   (match-case val
	      ((int32->uint32 ?expr) expr)
	      (else `(uint32->int32 ,val)))))
      ((int53 integer)
       (if (fixnum? val)
	   (fixnum->int32 val)
	   (match-case val
	      ((int32->fixnum ?expr) expr)
	      (else `(fixnum->int32 ,val)))))
      ((real)
       (if (real? val)
	   (fixnum->int32 (flonum->fixnum val))
	   (match-case val
	      ((int32->flonum ?expr) expr)
	      (else `(fixnum->int32 (flonum->fixnum ,val))))))
      (else
       `(fixnum->int32 ,val))))

;*---------------------------------------------------------------------*/
;*    asuint32 ...                                                     */
;*---------------------------------------------------------------------*/
(define (asuint32 val type::symbol)
   (case type
      ((int32)
       (if (int32? val)
	   (int32->uint32 val)
	   (match-case val
	      ((uint32->int32 ?expr) expr)
	      (else `(int32->uint32 ,val)))))
      ((uint32)
       val)
      ((int53 integer)
       (if (fixnum? val)
	   (fixnum->uint32 val)
	   (match-case val
	      ((uint32->fixnum ?expr) expr)
	      (else `(fixnum->uint32 ,val)))))
      ((real)
       (if (real? val)
	   (fixnum->uint32 (flonum->fixnum val))
	   (match-case val
	      ((uint32->flonum ?expr) expr)
	      (else `(fixnum->uint32 (flonum->fixnum ,val))))))
      (else
       `(fixnum->uint32 ,val))))

;*---------------------------------------------------------------------*/
;*    asfixnum ...                                                     */
;*---------------------------------------------------------------------*/
(define (asfixnum val type::symbol)
   (case type
      ((int53)
       val)
      ((int32)
       (if (int32? val)
	   (int32->fixnum val)
	   (match-case val
	      ((fixnum->int32 ?expr) expr)
	      (else `(int32->fixnum ,val)))))
      ((uint32)
       (if (uint32? val)
	   (uint32->fixnum val)
	   (match-case val
	      ((fixnum->uint32 ?expr) expr)
	      (else `(uint32->fixnum ,val)))))
      ((real)
       (if (real? val)
	   (flonum->fixnum val)
	   (match-case val
	      ((fixnum->flonum ?expr) expr)
	      (else `(flonum->fixnum ,val)))))
      (else `(if (flonum? ,val) (flonum->fixnum ,val) ,val))))

;*---------------------------------------------------------------------*/
;*    asreal ...                                                       */
;*---------------------------------------------------------------------*/
(define (asreal val type::symbol)
   (case type
      ((int32)
       (if (int32? val)
	   (int32->flonum val)
	   (match-case val
	      ((flonum->int32 ?expr) expr)
	      (else `(int32->flonum ,val)))))
      ((uint32)
       (if (uint32? val)
	   (uint32->flonum val)
	   (match-case val
	      ((flonum->uint32 ?expr) expr)
	      ((fixnum->uint32 ?expr) `(fixnum->flonum ,expr))
	      (else `(uint32->flonum ,val)))))
      ((int53 bint)
       (if (fixnum? val)
	   (fixnum->flonum val)
	   (match-case val
	      ((flonum->fixnum ?expr) expr)
	      (else `(fixnum->flonum ,val)))))
      ((integer)
       (if (fixnum? val)
	   (fixnum->flonum val)
	   (match-case val
	      ((flonum->fixnum ?expr) expr)
	      (else `(fixnum->flonum ,val)))))
      ((number)
       (cond
	  ((fixnum? val) val)
	  ((flonum? val) val)
	  (else
	   (match-case val
	      ((flonum->fixnum ?expr) expr)
	      (else `(if ,(j2s-fixnum? val) (fixnum->flonum ,val) ,val))))))
      (else
       (if (real? val)
	   val
	   `(cond
	       (,(j2s-flonum? val) ,val)
	       (,(j2s-fixnum? val) (fixnum->flonum ,val))
	       (else (exact->inexact (js-tonumber ,val %this))))))))

;*---------------------------------------------------------------------*/
;*    coerceint32 ...                                                  */
;*    -------------------------------------------------------------    */
;*    The coerceXXX functions are used when the range of VAL is        */
;*    known to fit the types but the static type is not known.         */
;*---------------------------------------------------------------------*/
(define (coerceint32 val type::symbol ctx)
   `(if ,(j2s-fixnum? val)
	,(asint32 val type)
	(fixnum->int32 (flonum->fixnum ,val))))
   
;*---------------------------------------------------------------------*/
;*    coerceuint32 ...                                                 */
;*---------------------------------------------------------------------*/
(define (coerceuint32 val type::symbol ctx)
   `(if ,(j2s-fixnum? val)
	,(asuint32 val type)
	(fixnum->uint32 (flonum->fixnum ,val))))

;*---------------------------------------------------------------------*/
;*    coercereal ...                                                   */
;*---------------------------------------------------------------------*/
(define (coercereal val type::symbol ctx)
   `(if ,(j2s-fixnum? val) (fixnum->flonum ,val) ,val))

;*---------------------------------------------------------------------*/
;*    toflonum ...                                                     */
;*---------------------------------------------------------------------*/
(define (toflonum val type::symbol ctx)
   (cond
      ((fixnum? val)
       (fixnum->flonum val))
      ((int32? val)
       (int32->flonum val))
      ((uint32? val)
       (uint32->flonum val))
      ((real? val)
       val)
      (else
       (case type
	  ((int32) `(int32->flonum ,val))
	  ((uint32) `(uint32->flonum ,val))
	  ((integer int53 bint) `(fixnum->flonum ,val))
	  ((real) val)
	  ((number) `(if ,(j2s-fixnum? val) (fixnum->flonum ,val) ,val))
	  (else (error "toflonum" "Cannot convert type" type))))))

;*---------------------------------------------------------------------*/
;*    tonumeric ...                                                    */
;*---------------------------------------------------------------------*/
(define (tonumeric val type::symbol ctx)
   (if (m64? (context-conf ctx))
       (tonumeric64 val type ctx)
       (tonumeric32 val type ctx)))
       
;*---------------------------------------------------------------------*/
;*    tonumeric32 ...                                                  */
;*---------------------------------------------------------------------*/
(define (tonumeric32 val type::symbol ctx)
   (box32 val type ctx (lambda (val) `(js-tonumeric ,val %this))))

;*---------------------------------------------------------------------*/
;*    toint32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (toint32 val type ctx)
   (case type
      ((int32)
       val)
      ((uint32)
       (if (and (uint32? val) (<u32 val (bit-lshu32 #u32:1 30)))
	   (uint32->int32 val)
	   `(uint32->int32 ,val)))
      ((int53)
       (if (fixnum? val) (fixnum->int32 val) `(fixnum->int32 ,val)))
      (else
       (if (fixnum? val)
	   (fixnum->int32 val)
	   `(js-toint32 ,val %this)))))

;*---------------------------------------------------------------------*/
;*    touint32 ...                                                     */
;*---------------------------------------------------------------------*/
(define (touint32 val type ctx)
   (case type
      ((int32)
       (if (int32? val) (int32->uint32 val) `(int32->uint32 ,val)))
      ((uint32)
       val)
      ((integer)
       (if (fixnum? val)
	   (fixnum->uint32 val)
	   `(fixnum->uint32 ,val)))
      ((int53)
       (if (fixnum? val)
	   (fixnum->uint32 val)
	   `(fixnum->uint32 ,val)))
      ((real)
       (if (flonum? val)
	   (double->uint32 val)
	   `(fixnum->uint32 (flonum->fixnum ,val))))
      (else
       (if (fixnum? val)
	   (fixnum->uint32 val)
	   `(js-touint32 ,val %this)))))

;*---------------------------------------------------------------------*/
;*    double->uint32 ...                                               */
;*---------------------------------------------------------------------*/
(define (double->uint32::uint32 obj::double)
   (cond
      ((or (= obj +inf.0) (= obj -inf.0) (not (= obj obj)))
       #u32:0)
      ((<fl obj 0.)
       (llong->uint32
	  (+llong (bit-lshllong #l1 32)
	     (flonum->llong (*fl -1. (floorfl (absfl obj)))))))
      (else
       (llong->uint32
	  (+llong (bit-lshllong #l1 32)
	     (flonum->llong (floorfl (absfl obj))))))))

;*---------------------------------------------------------------------*/
;*    touint32/w-overflow ...                                          */
;*---------------------------------------------------------------------*/
(define (touint32/w-overflow val type ctx)
   (cond
      ((int32? val)
       (int32->uint32 val))
      ((uint32? val)
       val)
      ((fixnum? val)
       (fixnum->uint32 val))
      (else
       (case type
	  ((uint32) `(int32->uint32 ,val))
	  ((uint32) val)
	  (else (fixnum->uint32 val))))))

;*---------------------------------------------------------------------*/
;*    toint32/32 ...                                                   */
;*---------------------------------------------------------------------*/
(define (toint32/32 val type::symbol ctx)
   (case type
      ((int32) val)
      ((uint32) (if (uint32? val) (uint32->int32 val) `(uint32->int32 ,val)))
      ((integer) (if (fixnum? val) (fixnum->int32 val) `(fixnum->int32 ,val)))
      (else (if (fixnum? val) (fixnum->int32 val) `(fixnum->int32 ,val)))))

;*---------------------------------------------------------------------*/
;*    tolong32 ...                                                     */
;*---------------------------------------------------------------------*/
(define (tolong32 val type::symbol ctx)

   (define bit-shift32
      (-fx (context-get :int-size 30) 1))
   
   (define (int32fx? val)
      (and (>=s32 val (negs32 (bit-lshs32 #u32:1 bit-shift32)))
	   (<s32 val (bit-lshs32 #s32:1 bit-shift32))))

   (define (uint32fx? val)
      (<u32 val (bit-lshu32 #u32:1 bit-shift32)))
   
   (case type
      ((int32)
       (if (and (int32? val) (int32fx? val))
	   (int32->fixnum val)
	   `(int32->fixnum ,val)))
      ((uint32)
       (if (and (uint32? val) (uint32fx? val))
	   (uint32->fixnum val)
	   `(uint32->fixnum ,val)))
      (else val)))

;*---------------------------------------------------------------------*/
;*    tolong64 ...                                                     */
;*---------------------------------------------------------------------*/
(define (tolong64 val type::symbol ctx)
   (case type
      ((int32) (if (int32? val) (int32->fixnum val) `(int32->fixnum ,val)))
      ((uint32) (if (uint32? val) (uint32->fixnum val) `(uint32->fixnum ,val)))
      (else val)))

;*---------------------------------------------------------------------*/
;*    tonumeric64 ...                                                  */
;*---------------------------------------------------------------------*/
(define (tonumeric64 val type::symbol ctx)
   (box64 val type ctx (lambda (val) `(js-tonumeric ,val %this))))

;*---------------------------------------------------------------------*/
;*    todouble ...                                                     */
;*---------------------------------------------------------------------*/
(define (todouble val type::symbol ctx)
   (case type
      ((int32)
       (if (int32? val) (int32->flonum val) `(int32->flonum ,val)))
      ((uint32)
       (if (uint32? val) (uint32->flonum val) `(uint32->flonum ,val)))
      ((integer bint)
       (if (fixnum? val) (fixnum->flonum val) `(fixnum->flonum ,val)))
      ((real)
       val)
      ((number)
       `(if (fixnum? ,val) (fixnum->flonum ,val) ,val))
      ((int53)
       (if (m64? (context-conf ctx)) `(fixnum->flonum ,val) `(js-toflonum ,val %this)))
      (else
       `(if ,(j2s-flonum? val)
	    ,(let ((f (gensym 'f)))
	       `(let ((,(symbol-append f '|::double|) ,val))
		   ,f))
	    (js-tonumber-for-flonum ,val %this)))))

;*---------------------------------------------------------------------*/
;*    tostring ...                                                     */
;*---------------------------------------------------------------------*/
(define (tostring val type ctx)
   (case type
      ((string buffer)
       val)
      ((integer int53)
       `(js-integer->jsstring ,val))
      ((int32)
       `(js-integer->jsstring (int32->fixnum ,val)))
      ((uint32)
       `(js-integer->jsstring ,(asfixnum val 'uint32)))
      ((real)
       `(js-real->jsstring ,val))
      ((number)
       (let loop ((val val))
	  (if (symbol? val)
	      `(if (fixnum? ,val)
		   (js-integer->jsstring ,val)
		   (js-real->jsstring ,val))
	      (let ((tmp (gensym 't)))
		 `(let ((,tmp ,val))
		     ,(loop tmp))))))
      (else
       `(js-toprimitive-for-string ,val %this))))

;*---------------------------------------------------------------------*/
;*    fixnums? ...                                                     */
;*---------------------------------------------------------------------*/
(define (fixnums? left tl right tr)
   
   (define (type-fixnum? type)
      (memq type '(int30 int53 bint)))
   
   (cond
      ((or (flonum? left) (flonum? right)) #f)
      ((or (bignum? left) (bignum? right)) #f)
      ((type-fixnum? tl) (if (type-fixnum? tr) #t (j2s-fixnum? right)))
      ((type-fixnum? tr) (j2s-fixnum? left))
      ((and (memq tl '(int53 integer number any unknown))
	    (memq tr '(int53 integer number any unknown)))
       (cond
	  ((fixnum? left)
	   (if (fixnum? right) #t (j2s-fixnum? right)))
	  ((fixnum? right)
	   `(fixnum? ,left))
	  ((and (eq? left right) (symbol? left))
	   (j2s-fixnum? left))
	  ((eq? left right)
	   (j2s-fixnum? left))
	  (else
	   `(fixnums? ,left ,right))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    j2s-number? ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-number? o)
   (and (number? o) (not (bignum? o))))

;*---------------------------------------------------------------------*/
;*    if-fixnums? ...                                                  */
;*---------------------------------------------------------------------*/
(define (if-fixnums? left tl right tr then else)
   (let ((test (fixnums? left tl right tr)))
      (cond
	 ((eq? test #t) then)
	 ((eq? test #f) else)
	 (else `(if ,test ,then ,else)))))

;*---------------------------------------------------------------------*/
;*    if-fixnum? ...                                                   */
;*---------------------------------------------------------------------*/
(define (if-fixnum? left tl then else)
   (let ((test (cond
		  ((eq? tl 'integer) #t)
		  ((memq tl '(int53 number any unknown)) (j2s-fixnum? left))
		  (else #f))))
      (cond
	 ((eq? test #t) then)
	 ((eq? test #f) else)
	 (else `(if ,test ,then ,else)))))

;*---------------------------------------------------------------------*/
;*    j2s-flonum? ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-flonum? v)
   (cond
      ((flonum? v) #t)
      ((fixnum? v) #f)
      (else `(flonum? ,v))))
       
;*---------------------------------------------------------------------*/
;*    j2s-fixnum? ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-fixnum? v)
   (cond
      ((flonum? v) #f)
      ((fixnum? v) #t)
      (else `(fixnum? ,v))))
       
;*---------------------------------------------------------------------*/
;*    flonums? ...                                                     */
;*---------------------------------------------------------------------*/
(define (flonums? left tl right tr)

   (define (number-not-flonum? num)
      (and (j2s-number? num) (not (flonum? num))))
   
   (cond
      ((or (number-not-flonum? left) (number-not-flonum? right)) #f)
      ((eq? tl 'real) (if (eq? tr 'real) #t (j2s-flonum? right)))
      ((eq? tr 'real) (j2s-flonum? left))
      ((and (memq tl '(number any unknown))
	    (memq tr '(number any unknown)))
       (if (eq? left right)
	   (j2s-flonum? left)
	   `(and ,(j2s-flonum? left) ,(j2s-flonum? right))))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    if-flonums? ...                                                  */
;*---------------------------------------------------------------------*/
(define (if-flonums? left tl right tr then else)
   (let ((test (flonums? left tl right tr)))
      (cond
	 ((eq? test #t) then)
	 ((eq? test #f) else)
	 (else `(if ,test ,then ,else)))))

;*---------------------------------------------------------------------*/
;*    if-flonum? ...                                                   */
;*---------------------------------------------------------------------*/
(define (if-flonum? left tl then else)
   (let ((test (cond
		  ((eq? tl 'real) #t)
		  ((memq tl '(number any unknown)) (j2s-flonum? left))
		  (else #f))))
      (cond
	 ((eq? test #t) then)
	 ((eq? test #f) else)
	 (else `(if ,test ,then ,else)))))

;*---------------------------------------------------------------------*/
;*    if-bigint? ...                                                   */
;*---------------------------------------------------------------------*/
(define (if-bigint? left tl then else)
   (let ((test (cond
		  ((eq? tl 'bigint) #t)
		  ((memq tl '(number any unknown)) `(bignum? ,left))
		  (else #f))))
      (cond
	 ((eq? test #t) then)
	 ((eq? test #f) else)
	 (else `(if ,test ,then ,else)))))

;*---------------------------------------------------------------------*/
;*    binop-int32-xxx ...                                              */
;*---------------------------------------------------------------------*/
(define (binop-int32-xxx op type lhs tl left rhs tr right ctx flip)
   (case tr
      ((int32)
       (binop-int32-int32 op type
	  left right flip))
      ((uint32)
       (cond
	  ((inrange-int32? rhs)
	   (binop-int32-int32 op type
	      left (asint32 right tr) flip))
	  ((inrange-uint32? lhs)
	   (binop-uint32-uint32 op type
	      (asuint32 left tl) right flip))
	  ((m64? (context-conf ctx))
	   (binop-int53-int53 op type
	      (asfixnum left tl) (asfixnum right tr) flip))
	  (else
	   (case op
	      ((<)
	       `(if (<s32 ,left #s32:0)
		    ,(if flip #f #t)
		    ,(binop-uint32-xxx op type lhs 'uint32 `(int32->uint32 ,left)
			rhs tr right ctx flip)))
	      (else
	       (binop-number-number op type
		  (box left tl ctx) (box right tr ctx) flip))))))
      ((bint)
       (if (m64? (context-conf ctx))
	   (binop-int53-int53 op type
	      (asfixnum left tl) (asfixnum right tr) flip)
	   (binop-int32-int32 op type
	      left (asint32 right tr) flip)))
      ((integer)
       (cond
	  ((or (inrange-int30? rhs) (m64? (context-conf ctx)))
	   (binop-int53-int53 op type
	      (asfixnum left tl) (asfixnum right tr) flip))
	  ((inrange-int32? rhs)
	   (binop-int32-int32 op type
	      left (coerceint32 right tr ctx) flip))
	  ((and (inrange-uint32? lhs) (inrange-uint32? rhs))
	   (binop-uint32-uint32 op type
	      (asuint32 left tl) (coerceuint32 right tr ctx) flip))
	  (else
	   (binop-number-number op type
	      (box left tl ctx) right flip))))
      ((real)
       (binop-flonum-flonum op type
	  (asreal left tl) right flip))
      ((int53)
       (binop-int53-int53 op type
	  (asfixnum left tl) right flip))
      (else
       (cond
	  ((inrange-int30? rhs)
	   (binop-int32-int32 op type
	      left (asint32 right tr) flip))
	  ((inrange-int32? rhs)
	   (binop-int32-int32 op type
	      left (coerceint32 right tr ctx) flip))
	  ((and (inrange-uint32? rhs) (inrange-uint32? lhs))
	   (binop-uint32-uint32 op type
	      (asuint32 left tl) (coerceuint32 right tr ctx) flip))
	  ((and (inrange-uint32? rhs) (m64? (context-conf ctx)))
	   (binop-int53-int53 op type
	      (asfixnum left tl) (asfixnum right tr) flip))
	  ((cannot-integer? rhs)
	   (binop-any-any op type
	      (box left tl ctx) (box right tr ctx) flip))
	  (else
	   `(if ,(j2s-fixnum? right)
		,(binop-fixnum-fixnum/ctx ctx op type
		    (asfixnum left tl) right flip)
		,(binop-any-any op type
		   (box left tl ctx) (box right tr ctx) flip)))))))

;*---------------------------------------------------------------------*/
;*    binop-uint32-xxx ...                                             */
;*---------------------------------------------------------------------*/
(define (binop-uint32-xxx op type lhs tl left rhs tr right ctx flip)
   (case tr
      ((int32)
       (cond
	  ((m64? (context-conf ctx))
	   (binop-int53-int53 op type
	      (asfixnum left tl) (asfixnum right tr) flip))
	  ((inrange-int32? lhs)
	   (binop-int32-int32 op type
	      (asint32 left tl) right flip))
	  ((inrange-uint32? rhs)
	   (binop-uint32-uint32 op type
	      (asint32 left tl) right flip))
	  ((inrange-uint30? lhs)
	   (binop-int32-int32 op type
	      (asint32 left tl) right flip))
	  (else
	   (if (memq op '(> >=))
	       (if flip
		   `(and (>=s32 ,right #s32:0)
			 ,(binop-uint32-uint32 op type
			     left (asuint32 right tr) flip))
		   `(or (<s32 ,right #s32:0)
			,(binop-uint32-uint32 op type
			    left (asuint32 right tr) flip)))
	       (binop-number-number op type
		  (box left tl ctx) (box right tr ctx) flip)))))
      ((uint32)
       (cond
	  ((and (eq? type 'bool) (memq op '(< <= > >= == === != !==)))
	   (binop-uint32-uint32 op type left right flip))
	  ((and (not (eq? type 'uint32))
		(inrange-int32? lhs) (inrange-int32? rhs))
	   (binop-int32-int32 op type (asint32 left tl) (asint32 right tr) flip))
	  (else
	   (binop-uint32-uint32 op type left right flip))))
      ((bint)
       (cond
	  ((m64? (context-conf ctx))
	   (binop-int53-int53 op type
	      (asfixnum left tl) (asfixnum right tr) flip))
	  ((inrange-int32? lhs)
	   (binop-fixnum-fixnum op type
	      (asfixnum left tl) right flip))
	  ((inrange-uint32? rhs)
	   (binop-uint32-uint32 op type
	      left (asuint32 right tr) flip))
	  ((inrange-uint30? lhs)
	   (binop-fixnum-fixnum op type
	      (asfixnum left tl) right flip))
	  (else
	   `(if (>=fx ,right 0)
		,(binop-uint32-uint32 op type
		    left (asuint32 right tr) flip)
		,(binop-number-number op type
		    (box left tl ctx) right flip)))))
      ((integer)
       (cond
	  ((m64? (context-conf ctx))
	   (binop-int53-int53 op type
	      (asfixnum left tl) (asfixnum right tr) flip))
	  ((inrange-uint32? rhs)
	   (binop-uint32-uint32 op type
	      left (coerceuint32 right tr ctx) flip))
	  ((and (inrange-int32? lhs) (inrange-int32? rhs))
	   (binop-int32-int32 op type
	      (asint32 left tl) (coerceint32 right tr ctx) flip))
	  ((inrange-int30? lhs)
	   (binop-bint-xxx op type lhs 'bint (asfixnum left tl)
	      rhs tr right ctx flip))
	  (else
	   (binop-number-number op type
	      (box left tl ctx) right flip))))
      ((real)
       (binop-flonum-flonum op type
	  (asreal left tl) right flip))
      ((int53)
       (cond
	  ((and (memq op '(+ ++))
		(and (uint32? left) (=u32 left #u32:1))
		(not (inrange-int32? rhs))
		(not (inrange-uint32? rhs)))
	   `(+fx ,right 1))
	  ;; MS: 10jul2023, if type is an int53, there is
	  ;; no reason to check the oveflow
	  ;;(j2s-int53-op 'inc right type))
	  ((and (memq op '(- --))
		(and (uint32? left) (=u32 left #u32:1))
		(not (inrange-int32? rhs))
		(not (inrange-uint32? rhs))
		flip)
	   `(-fx ,right 1))
	  ;; MS: 10jul2023, if type is an int53, there is
	  ;; no reason to check the oveflow
	  ;;(j2s-int53-op 'dec right type))
	  (else
	   (binop-int53-int53 op type
	      (asfixnum left tl) right flip))))
      (else
       (cond
	  ((inrange-uint30? rhs)
	   (binop-uint32-uint32 op type
	      left (asuint32 right tr) flip))
	  ((inrange-uint32? rhs)
	   (binop-uint32-uint32 op type
	      left (coerceuint32 right tr ctx) flip))
	  ((and (inrange-int32? rhs) (inrange-int32? lhs))
	   (binop-int32-int32 op type
	      (asint32 left tl) (coerceint32 right tr ctx) flip))
	  ((and (inrange-int32? rhs) (m64? (context-conf ctx)))
	   (binop-int53-int53 op type
	      (asfixnum left tl) (asfixnum right tr) flip))
	  ((cannot-integer? rhs)
	   (binop-any-any op type
	       (box left tl ctx) (box right tr ctx) flip))
	  (else
	   `(if ,(j2s-fixnum? right)
		,(binop-fixnum-fixnum/ctx ctx op type
		    (asfixnum left tl) right flip)
		,(binop-any-any op type
		    (box left tl ctx) (box right tr ctx) flip)))))))

;*---------------------------------------------------------------------*/
;*    binop-int53-xxx ...                                              */
;*    -------------------------------------------------------------    */
;*    Only used on 64-bit platforms.                                   */
;*---------------------------------------------------------------------*/
(define (binop-int53-xxx op type lhs tl left rhs tr right ctx flip)
   (case tr
      ((int32)
       (binop-int53-int53 op type left (asfixnum right tr) flip))
      ((uint32)
       (binop-int53-int53 op type left (asfixnum right tr) flip))
      ((int53)
       (binop-int53-int53 op type left right flip))
      ((real)
       (if (memq type '(int32 uint32 integer bint real number))
	   (binop-number-number op type
	      (box left tl ctx) (box right tr ctx) flip)
	   (binop-any-any op type
	      (box left tl ctx) (box right tr ctx) flip)))
      (else
       `(if ,(j2s-fixnum? right)
	    ,(binop-int53-int53 op type left right flip)
	    ,(if (memq type '(int32 uint32 integer bint real number))
		 (binop-number-number op type
		    (box left tl ctx) (box right tr ctx) flip)
		 (binop-any-any op type
		    (box left tl ctx) (box right tr ctx) flip))))))

;*---------------------------------------------------------------------*/
;*    binop-bint-xxx ...                                               */
;*---------------------------------------------------------------------*/
(define (binop-bint-xxx op type lhs tl left rhs tr right ctx flip)
   (case tr
      ((int32)
       (binop-fixnum-fixnum/ctx ctx op type
	  left (asfixnum right tr) flip))
      ((uint32)
       (cond
	  ((inrange-int32? rhs)
	   (binop-fixnum-fixnum/ctx ctx op type
	      left (asfixnum right tr) flip))
	  ((inrange-uint32? lhs)
	   (binop-uint32-uint32 op type
	      (asuint32 left tl) right flip))
	  ((m64? (context-conf ctx))
	   (binop-int53-int53 op type
	      (asfixnum left tl) (asfixnum right tr) flip))
	  (else
	   (binop-number-number op type
	      left (box right tr ctx) flip))))
      ((bint)
       (binop-fixnum-fixnum/ctx ctx op type
	  left right flip))
      ((integer)
       (if (m64? (context-conf ctx))
	   (binop-int53-int53 op type
	      left right flip)
	   `(if ,(j2s-fixnum? right)
		,(binop-fixnum-fixnum op type
		    left right flip)
		,(binop-number-number op type
		    left right flip))))
      ((real)
       (binop-flonum-flonum op type 
	  (asreal left tl) right flip))
      (else
       (cond
	  ((and (eq? tr 'int53) (m64? (context-conf ctx)))
	   (binop-fixnum-fixnum/ctx ctx op type
	      left right flip))
	  ((inrange-int30? rhs)
	   (binop-fixnum-fixnum/ctx ctx op type
	      left (asfixnum right tr) flip))
	  ((inrange-int32? rhs)
	   (binop-int32-int32 op type
	      (asint32 left tl) (coerceint32 right tr ctx) flip))
	  ((and (inrange-uint32? rhs) (inrange-uint32? lhs))
	   (binop-uint32-uint32 op type
	      (asuint32 left tr) (coerceuint32 right tr ctx) flip))
	  ((and (inrange-uint32? rhs) (m64? (context-conf ctx)))
	   (binop-int53-int53 op type
	      left (asfixnum right tr) flip))
	  ((cannot-integer? rhs)
	   (binop-any-any op type
	      left (box right tr ctx) flip))
	  (else
	   `(if ,(j2s-fixnum? right)
		,(binop-fixnum-fixnum/ctx ctx op type
		    left right flip)
		,(if (memq type '(int32 uint32 integer bint real number))
		     (binop-number-number op type
			left (box right tr ctx) flip)
		     (binop-any-any op type
			left (box right tr ctx) flip))))))))

;*---------------------------------------------------------------------*/
;*    binop-integer-xxx ...                                            */
;*---------------------------------------------------------------------*/
(define (binop-integer-xxx op type lhs tl left rhs tr right ctx flip)
   (if (and (m64? (context-conf ctx)) (inrange-int53? lhs))
       (binop-bint-xxx op type lhs tl left rhs tr right ctx flip)
       (case tr
	  ((int32)
	   `(if ,(j2s-fixnum? left)
		,(binop-fixnum-fixnum op type
		   left (asfixnum right tr) flip)
		,(binop-number-number op type
		   left (box right tr ctx) flip)))
	  ((uint32)
	   (cond
	      ((inrange-int32? rhs)
	       `(if ,(j2s-fixnum? left)
		    ,(binop-int32-int32 op type
		       (asint32 left tl) (asfixnum right tr) flip)
		    ,(binop-number-number op type
		       left (box right tr ctx) flip)))
	      ((inrange-uint32? lhs)
	       `(if ,(j2s-fixnum? left)
		    ,(binop-uint32-uint32 op type
		       (asuint32 left tl) right flip)
		    ,(binop-number-number op type
		       left (box right tr ctx) flip)))
	      (else
	       (binop-number-number op type
		  left (box right tr ctx) flip))))
	  ((bint)
	   `(if ,(j2s-fixnum? left)
		,(binop-fixnum-fixnum op type
		   left right flip)
		,(binop-number-number op type
		    left right flip)))
	  ((int53)
	   `(if ,(j2s-fixnum? left)
		,(binop-fixnum-fixnum op type
		   left right flip)
		,(binop-number-number op type
		   left right flip)))
	  ((integer)
	   (if-fixnums? left tl right tr
	      (binop-fixnum-fixnum op type
		 left right flip)
	      (binop-number-number op type
		 left right flip)))
	  ((real)
	   (binop-flonum-flonum op type 
	      (coercereal left tl ctx) right flip))
	  (else
	   (cond
	      ((and (inrange-int30? rhs) (inrange-int30? lhs))
	       (binop-fixnum-fixnum op type
		  (asfixnum left tl) (asfixnum right tr) flip))
	      ((and (inrange-int32? rhs) (inrange-int32? lhs))
	       (binop-int32-int32 op type
		  (coerceint32 left tl ctx) (coerceint32 right tr ctx) flip))
	      ((and (inrange-uint32? rhs) (inrange-uint32? lhs))
	       (binop-uint32-uint32 op type
		  (coerceuint32 left tr ctx) (coerceuint32 right tr ctx) flip))
	      ((and (inrange-uint32? lhs) (inrange-uint30? rhs))
	       (binop-uint32-uint32 op type
		  (coerceuint32 left tl ctx) (asuint32 right tr) flip))
	      ((and (inrange-uint30? lhs) (inrange-uint32? rhs))
	       (binop-uint32-uint32 op type
		  (asuint32 left tl) (coerceuint32 right tr ctx) flip))
	      (else
	       `(if (fixnums? ,left ,right)
		    ,(binop-fixnum-fixnum op type
			left right flip)
		    ,(binop-any-any op type
			left (box right tr ctx) flip))))))))

;*---------------------------------------------------------------------*/
;*    fresh-real? ...                                                  */
;*---------------------------------------------------------------------*/
(define (fresh-real? n::J2SNode s)
   (let loop ((n n))
      (cond
	 ((number? s)
	  #f)
	 ((isa? n J2SUnary)
	  (with-access::J2SUnary n (expr)
	     (loop expr)))
	 ((isa? n J2SBinary)
	  (with-access::J2SBinary n (lhs rhs)
	     (or (loop lhs) (loop rhs))))
	 ((isa? n J2SParen)
	  (with-access::J2SParen n (expr)
	     (loop expr)))
	 ((isa? n J2SCast)
	  (with-access::J2SCast n (expr)
	     (loop expr)))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    real-op-lhs ...                                                  */
;*---------------------------------------------------------------------*/
(define (real-op-lhs op type lhs rhs left right flip)
   (cond
      ((eq? type 'real) op)
      ((not (memq op '(+ * - /))) op)
      ((fresh-real? lhs left) (symbol-append op (if flip 'r! 'l!)))
      (else op)))

;*---------------------------------------------------------------------*/
;*    real-op-rhs ...                                                  */
;*---------------------------------------------------------------------*/
(define (real-op-rhs op type lhs rhs left right flip)
   (cond
      ((eq? type 'real) op)
      ((not (memq op '(+ * - /))) op)
      ((fresh-real? rhs right) (symbol-append op (if flip 'l! 'r!)))
      (else op)))

;*---------------------------------------------------------------------*/
;*    real-op ...                                                      */
;*---------------------------------------------------------------------*/
(define (real-op op type lhs rhs left right flip)
   (cond
      ((eq? type 'real) op)
      ((not (memq op '(+ * - /))) op)
      ((fresh-real? lhs left) (symbol-append op (if flip 'r! 'l!)))
      ((fresh-real? rhs right) (symbol-append op (if flip 'l! 'r!)))
      (else op)))

;*---------------------------------------------------------------------*/
;*    binop-real-xxx ...                                               */
;*---------------------------------------------------------------------*/
(define (binop-real-xxx op type lhs tl left rhs tr right ctx flip)
   (case tr
      ((int32 uint32 bint int53)
       (binop-flonum-flonum op type
	  left (asreal right tr) flip))
      ((integer)
       (binop-flonum-flonum op type
	  left (coercereal right tr ctx) flip))
      ((real)
       (binop-flonum-flonum op type
	  left (asreal right tr) flip))
      (else
       (if-flonum? right tr 
	  (binop-flonum-flonum (real-op-rhs op type lhs rhs left right flip) type
	     left right flip)
	  (if (memq type '(int32 uint32 integer bint real number))
	      (binop-number-number op type
		 left (tonumeric right tr ctx) flip)
	      (binop-any-any op type
		 left (tonumeric right tr ctx) flip))))))

;*---------------------------------------------------------------------*/
;*    binop-bigint-xxx ...                                             */
;*---------------------------------------------------------------------*/
(define (binop-bigint-xxx op type lhs tl left rhs tr right ctx flip)
   (case tr
      ((int32 uint32 bint int53 integer flonum)
       (with-access::J2SExpr right (loc)
	  (raise
	     (instantiate::&type-error
		(proc "hopc")
		(msg (format "BigInt and number cannot be mixed"))
		(obj right)
		(type tr)
		(fname (cadr loc))
		(location (caddr loc))))))
      ((bigint)
       (binop-bigint-bigint op type left right flip))
      (else
       (if-bigint? right tr 
	  (binop-bigint-bigint op type left right flip)
	  (binop-any-any op type left (box right tr ctx) flip)))))

;*---------------------------------------------------------------------*/
;*    binop-number-xxx ...                                             */
;*---------------------------------------------------------------------*/
(define (binop-number-xxx op type lhs tl left right tr right ctx flip)
   (cond
      ((eq? tr 'number)
       (binop-number-number op type
	  left right flip))
      ((memq type '(int32 uint32 integer bint real number))
       (binop-number-number op type
	  left (box right tr ctx) flip))
      (else
       (binop-any-any op type
	  left (box right tr ctx) flip))))

;*---------------------------------------------------------------------*/
;*    binop-xxx-xxx ...                                                */
;*---------------------------------------------------------------------*/
(define (binop-flip op left right flip)
   (if flip `(,op ,right ,left) `(,op ,left ,right)))
   
(define (binop-int32-int32 op type left right flip)
   (let ((ops32 (cond
		   ((memq op '(== ===)) '=s32)
		   ((eq? op '<<=) '<=s32)
		   ((eq? op '>>=) '>=s32)
		   ((eq? op '++) '+s32)
		   ((eq? op '--) '-s32)
		   (else (symbol-append op 's32)))))
      (case type
	 ((int32)
	  (binop-flip ops32 left right flip))
	 ((uint32)
	  `(int32->uint32 ,(binop-flip ops32 left right flip)))
	 ((real)
	  (binop-flonum-flonum op type
	     (asreal left 'int32) (asreal right 'int32)
	     flip))
	 ((int53)
	  (binop-int53-int53 op type
	     (asfixnum left 'int32) (asfixnum right 'int32)
	     flip))
	 ((bool)
	  (binop-flip ops32 left right flip))
	 (else
	  (binop-flip (symbol-append ops32 '/overflow) left right flip)))))
   
(define (binop-uint32-uint32 op type left right flip)
   (let ((opu32 (cond
		   ((memq op '(== ===)) '=u32)
		   ((eq? op '<<=) '<=u32)
		   ((eq? op '>>=) '>=u32)
		   ((eq? op '++) '+u32)
		   ((eq? op '--) '-u32)
		   (else (symbol-append op 'u32)))))
      (case type
	 ((int32)
	  `(uint32->int32 ,(binop-flip opu32 left right flip)))
	 ((uint32)
	  (binop-flip opu32 left right flip))
	 ((real)
	  (binop-flonum-flonum op type
	     (asreal left 'uint32) (asreal right 'uint32)
	     flip))
	 ((int53)
	  (binop-int53-int53 op type
	     (asfixnum left 'uint32) (asfixnum right 'uint32)
	     flip))
	 ((bool)
	  (binop-flip opu32 left right flip))
	 (else
	  (binop-flip (symbol-append opu32 '/overflow) left right flip)))))

(define (binop-fixnum-fixnum/ctx ctx op type left right flip)
   (let ((op (case op
		((--) '-)
		((++) '+)
		(else op))))
      (if (m64? (context-conf ctx))
	  (binop-int53-int53 op type left right flip)
	  (binop-fixnum-fixnum op type left right flip))))
       
(define (binop-int53-int53 op type left right flip)
   (let ((tmp (binop-fixnum-fixnum op type left right flip)))
      (match-case tmp
	  ((+fx/overflow ?x 1) `(js-int53-inc ,x))
	  ((++fx/overflow ?x 1) `(js-int53-inc ,x))
	  ((++fx ?x 1) `(+fx ,x 1))
	  ((+fx/overflow ?x -1) `(js-int53-dec ,x))
	  ((++fx/overflow ?x -1) `(js-int53-dec ,x))
	  ((++fx ?x -1) `(-fx ,x 1))
	  ((+fx/overflow 1 ?x) `(js-int53-inc ,x))
	  ((++fx/overflow 1 ?x) `(js-int53-inc ,x))
	  ((++fx 1 ?x) `(+fx ,x 1))
	  ((-fx/overflow ?x 1) `(js-int53-dec ,x))
	  ((--fx/overflow ?x 1) `(js-int53-dec ,x))
	  ((--fx ?x 1) `(-fx ,x 1))
	  (else tmp))))

(define (binop-fixnum-fixnum op type left right flip)
   (let ((op (cond
		((memq op '(== ===)) '=fx)
		((eq? op '<<=) '<=fx)
		((eq? op '>>=) '>=fx)
		(else (symbol-append op 'fx)))))
      (case type
	 ((int32)
	  `(fixnum->int32 ,(binop-flip op left right flip)))
	 ((uint32)
	  `(fixnum->uint32 ,(binop-flip op left right flip)))
	 ((int53)
	  (binop-flip op left right flip))
	 ((real)
	  `(fixnum->flonum ,(binop-flip op left right flip)))
	 ((bool)
	  (binop-flip op left right flip))
	 (else
	  (binop-flip (symbol-append op '/overflow) left right flip)))))
   
(define (binop-flonum-flonum op type left right flip)
   (let ((op (cond
		((memq op '(== ===)) '=fl)
		((eq? op '--) '-fl)
		((eq? op '++) '+fl)
		(else (symbol-append op 'fl)))))
      (case type
	 ((int32)
	  `(fixnum->int32 (flonum->fixnum ,(binop-flip op left right flip))))
	 ((uint32)
	  `(fixnum->uint32 (flonum->fixnum ,(binop-flip op left right flip))))
	 ((bool)
	  (binop-flip op left right flip))
	 (else
	  (binop-flip op left right flip)))))

(define (binop-bigint-bigint op type left right flip)
   (let ((op (if (memq op '(== ===)) '=bx (symbol-append op 'bx))))
      (binop-flip op left right flip)))
   
(define (binop-number-number op type left right flip)
   (let ((op (if (memq op '(== ===)) '= op)))
      (case type
	 ((bool)
	  (binop-flip op left right flip))
	 ((int32)
	  `(js-number-toint32
	      ,(binop-flip (symbol-append op '/overflow) left right flip)))
	 ((uint32)
	  `(js-number-touint32
	      ,(binop-flip (symbol-append op '/overflow) left right flip)))
	 ((real)
	  (case op
	     ((* + -)
	      (binop-flip (symbol-append op '/overflowfl) left right flip))
	     ((++)
	      (binop-flip '+ left right flip))
	     ((--)
	      (binop-flip '- left right flip))
	     (else
	      `(js-toflonum
		  ,(binop-flip (symbol-append op '/overflow) left right flip)))))
	 (else
	  (if (memq op '(++ --))
	      (binop-any-any op type left right flip)
	      (binop-flip (symbol-append op '/overflow) left right flip))))))

(define (binop-number-number! op type left right flip)
   (let ((op (if (memq op '(== ===)) '= op))
	 (/ov (if (memq op '(+ - *)) '/overflow! '/overflow)))
      (case type
	 ((bool)
	  (binop-flip op left right flip))
	 ((int32)
	  `(js-number-toint32
	      ,(binop-flip (symbol-append op /ov) left right flip)))
	 ((uint32)
	  `(js-number-touint32
	      ,(binop-flip (symbol-append op /ov) left right flip)))
	 ((real)
	  `(js-toflonum
	      ,(binop-flip (symbol-append op /ov) left right flip)))
	 (else
	  (if (memq op '(++ --))
	      (binop-any-any op type left right flip)
	      (binop-flip (symbol-append op /ov) left right flip))))))
   
(define (binop-any-any op type left right flip)
   (case op
      ((===)
       (if flip
	   `(js-strict-equal? ,right ,left)
	   `(js-strict-equal? ,left ,right)))
      ((js-strict-equal-no-string?)
       (if flip
	   `(js-strict-equal-no-string? ,right ,left)
	   `(js-strict-equal-no-string? ,left ,right)))
      ((++ --)
       (let ((r (if flip left right))
	     (l (if flip right left))
	     (op (symbol-append 'js op)))
	  (if (isone? r)
	      (case type
		 ((bool)
		  `(,op ,l %this))
		 ((int32)
		  `(js-number-toint32 (,op ,l %this)))
		 ((uint32)
		  `(js-number-touint32 (,op ,l %this)))
		 ((real)
		  `(js-toflonum (,op ,l %this)))
		 (else
		  `(,op ,l %this)))
	      (error (format "binop:~a" op) "wrong rhs" (cons r flip)))))
      (else
       (let ((op (cond 
		    ((eq? op '==) 'js-equal?)
		    ((memq type '(integer number bool)) (symbol-append op 'js))
		    (else (symbol-append 'js op)))))
	  (case type
	     ((bool)
	      (if flip
		  `(,op ,right ,left %this)
		  `(,op ,left ,right %this)))
	     ((int32)
	      `(js-number-toint32
		  ,(if flip
		       `(,op ,right ,left %this)
		       `(,op ,left ,right %this))))
	     ((uint32)
	      `(js-number-touint32
		  ,(if flip
		       `(,op ,right ,left %this)
		       `(,op ,left ,right %this))))
	     ((real)
	      `(js-toflonum
		  ,(if flip
		       `(,op ,right ,left %this)
		       `(,op ,left ,right %this))))
	     (else
	      (if flip
		  `(,op ,right ,left %this)
		  `(,op ,left ,right %this))))))))

;*---------------------------------------------------------------------*/
;*    j2s-int53-op ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-int53-op op expr type)
   (let ((op (symbol-append 'js-int53- op)))
      (case type
	 ((int32) `(fixnum->int32 (,op ,expr)))
	 ((uint32) `(fixnum->uint32 (,op ,expr)))
	 ((int53) `(,op ,expr))
	 ((real) `(fixnum->flonum (,op ,expr)))
	 (else `(,op ,expr)))))

;*---------------------------------------------------------------------*/
;*    isone? ...                                                       */
;*---------------------------------------------------------------------*/
(define (isone? x)
   (cond
      ((fixnum? x) (=fx x 1))
      ((int32? x) (=s32 x #s32:1))
      ((uint32? x) (=u32 x #u32:1))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    isminusone? ...                                                  */
;*---------------------------------------------------------------------*/
(define (isminusone? x)
   (cond
      ((fixnum? x) (=fx x -1))
      ((int32? x) (=s32 x #s32:-1))
      (else #f)))
