;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-ops.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:21:19 2017                          */
;*    Last change :  Thu Dec 14 15:48:56 2017 (serrano)                */
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

   (define (bitnot loc expr)
      ;; optimize the pattern ~~expr that is sometime used to cast
      ;; an expression into a number
      (match-case expr
	 ((bit-nots32 ?expr) expr)
	 (else (epairify loc `(bit-nots32 ,expr)))))
   
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
	     (case type
		((int32)
		 (cond
		    ((int32? expr)
		     (if (=s32 expr #s32:0) -0.0 (negs32 expr)))
		    ((eq? typ 'int32)
		     (epairify loc `(negs32js ,expr)))
		    (else
		     (epairify loc `(js-toint32 (negjs ,expr %this))))))
		((uint32)
		 (cond
		    ((eq? typ 'int32)
		     (epairify loc `(negs32 ,expr)))
		    (else
		     (epairify loc `(js-touint32 (negjs ,expr %this))))))
		((eqv? expr 0)
		 -0.0)
		((integer)
		 (if (fixnum? expr)
		     (negfx expr)
		     (epairify loc `(negfx ,expr))))
		(else
		 (epairify loc `(negjs ,expr %this))))))
	 ((~)
	  ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.8
	  (if (eq? type 'int32)
	      (bitnot loc (j2s-scheme expr mode return conf hint totype))
	      `(bit-notjs ,(j2s-scheme expr mode return conf hint totype) %this)))
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
       `(and (fixnum? ,(fixnum-test left)) (fixnum? ,(fixnum-test right))))
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
		 (js-binop loc op left lhs right rhs conf)))
	   (js-binop2-add loc type lhs rhs mode return conf hint totype)))
      ((-)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop-arithmetic loc op left lhs right rhs conf)))
	   (js-arithmetic-addsub loc op type lhs rhs mode return conf hint totype)))
      ((*)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop-arithmetic loc op left lhs right rhs conf)))
	   (js-arithmetic-mul loc type lhs rhs mode return conf hint totype)))
      ((/)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop-arithmetic loc op left lhs right rhs conf)))
	   (js-arithmetic-div loc type lhs rhs mode return conf hint totype)))
      ((remainder)
       (js-arithmetic-remainder loc type lhs rhs mode return conf hint totype))
      ((%)
       (js-arithmetic-% loc type lhs rhs mode return conf hint totype))
      ((eq?)
       (binop lhs rhs mode return conf hint 'any
	  (lambda (left right)
	     `(eq? ,left ,right))))
      ((== === != !==)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left lhs right rhs conf)))
	   (js-equality loc op type lhs rhs mode return conf hint totype)))
      ((< <= > >=)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop loc op left lhs right rhs conf)))
	   (js-cmp loc op lhs rhs mode return conf hint totype)))
      ((& ^ BIT_OR >> >>> <<)
       (if (=fx (config-get conf :optim 0) 0)
	   (binop lhs rhs mode return conf hint 'any
	      (lambda (left right)
		 (js-binop-arithmetic loc op left lhs right rhs conf)))
	   (js-bitop loc op type lhs rhs mode return conf hint type)))
      ((remainderfx remainder)
       (binop lhs rhs mode return conf hint 'any
	  (lambda (left right)
	     `(,op ,left ,right))))
      
      ((OR)
       (let ((lhsv (gensym 'lhs)))
	  `(let ((,(type-ident lhsv (j2s-type-ref lhs))
		  ,(j2s-scheme lhs mode return conf hint totype)))
	      (if ,(if (eq? (j2s-type-ref lhs) 'bool)
		       lhsv
		       (j2s-cast lhsv lhs (j2s-type-ref lhs) 'bool conf))
		  ,(j2s-cast lhsv lhs (j2s-type-ref lhs) type conf)
		  ,(j2s-cast (j2s-scheme rhs mode return conf hint totype) rhs
		      (j2s-type-ref rhs) type conf)))))
      ((&&)
       (let ((lhsv (gensym 'lhs)))
	  `(let ((,(type-ident lhsv (j2s-type-ref lhs))
		  ,(j2s-scheme lhs mode return conf hint totype)))
	      (if ,(if (eq? (j2s-type-ref lhs) 'bool)
		       lhsv
		       (j2s-cast lhsv lhs (j2s-type-ref lhs) 'bool conf))
		  ,(j2s-cast (j2s-scheme rhs mode return conf hint totype) rhs
		      (j2s-type-ref rhs) type conf)
		  ,(j2s-cast lhsv lhs (j2s-type-ref lhs) type conf)))))
      (else
       (binop lhs rhs mode return conf hint 'any
	  (lambda (left right)
	     (js-binop loc op left lhs right rhs conf))))))

;*---------------------------------------------------------------------*/
;*    j2s-in? ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-in? loc id obj)
   (if (> (bigloo-debug) 0)
       `(js-in?/debug %this ',loc ,id ,obj)
       `(js-in? %this ,id ,obj)))

;*---------------------------------------------------------------------*/
;*    js-binop ...                                                     */
;*    -------------------------------------------------------------    */
;*    This function is called with left and right being either         */
;*    atoms or variable references. Hence it does not generate         */
;*    bindings.                                                        */
;*---------------------------------------------------------------------*/
(define (js-binop loc op lhs l rhs r conf)
   (case op
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
      ((+)
       `(js+ ,lhs ,rhs %this))
      ((<)
       `(<js ,lhs ,rhs %this))
      ((<=)
       `(<=js ,lhs ,rhs %this))
      ((>)
       `(>js ,lhs ,rhs %this))
      ((>=)
       `(>=js ,lhs ,rhs %this))
      ((- * / /num % & ^ >> >>> << OR &&)
       (error "js-binop" "should not be here" op))
      (else
       `(,op ,lhs ,rhs %this))))

;*---------------------------------------------------------------------*/
;*    js-binop-arithmetic ...                                          */
;*    -------------------------------------------------------------    */
;*    This function is called with left and right being either         */
;*    atoms or variable references. Hence it does not generate         */
;*    bindings.                                                        */
;*---------------------------------------------------------------------*/
(define (js-binop-arithmetic loc op left l right r conf)
   (tprint "CORRIGER ICI les to number entre * et <...")
   (let (lhs rhs)
      (if (m64? conf)
	  (begin
	     (set! lhs (tonumber64 left (j2s-type-ref l)))
	     (set! rhs (tonumber64 right (j2s-type-ref r))))
	  (begin
	     (set! lhs (tonumber32 left (j2s-type-ref l)))
	     (set! rhs (tonumber32 right (j2s-type-ref r)))))
      (case op
	 ((-)
	  `(-js ,lhs ,rhs %this))
	 ((*)
	  `(*js ,lhs ,rhs %this))
	 ((/)
	  `(js/ ,lhs ,rhs %this))
	 ((/num)
	  `(js/num ,lhs ,rhs))
	 ((%)
	  `(js% ,lhs ,rhs %this))
	 ((<)
	  `(<js ,lhs ,rhs %this))
	 ((<=)
	  `(<=js ,lhs ,rhs %this))
	 ((>)
	  `(>js ,lhs ,rhs %this))
	 ((>=)
	  `(>=js ,lhs ,rhs %this))
	 ((&)
	  `(bit-andjs ,lhs ,rhs %this))
	 ((BIT_OR)
	  `(bit-orjs ,lhs ,rhs %this))
	 ((^)
	  `(bit-xorjs ,lhs ,rhs %this))
	 ((>>)
	  `(bit-rshjs ,lhs ,rhs %this))
	 ((>>>)
	  `(bit-urshjs ,lhs ,rhs %this))
	 ((<<)
	  `(bit-lshjs ,lhs ,rhs %this))
	 (else
	  (error "js-binop-arihmetic" "should not be here" op)))))

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
	     `(let ((,(type-ident right (j2s-type-ref rhs)) ,scmrhs))
		 ,(gen scmlhs right))))
	 (testr
	  (let ((left (gensym 'lhs)))
	     `(let ((,(type-ident left (j2s-type-ref lhs)) ,scmlhs))
		 ,(gen left scmrhs))))
	 (else
	  (let ((left (gensym 'lhs))
		(right (gensym 'rhs)))
	     `(let* ((,(type-ident left (j2s-type-ref lhs)) ,scmlhs)
		     (,(type-ident right (j2s-type-ref rhs)) ,scmrhs))
		 ,(gen left right)))))))

;*---------------------------------------------------------------------*/
;*    js-cmp ...                                                       */
;*    -------------------------------------------------------------    */
;*    The compilation of the comparison functions.                     */
;*---------------------------------------------------------------------*/
(define (js-cmp loc o::symbol lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)

   (define (op o base)
      (case o
	 ((== === != !==)
	  (when (eq? base 'js)
	     (error "js-cmp" "should not be here" o))
	  (symbol-append '= base))
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

   (define (cmp/32js o lhs tl left rhs tr right)
      (if (memq o '(== === != !==))
	  `(= ,(tonumber32 left tl) ,(tonumber32 right tr))
	  `(,(opjs o) ,(tonumber32 left tl) ,(tonumber32 right tr) %this)))
   
   (define (cmp/32 o lhs tl left rhs tr right)
      (cond
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(,(ops32 o) ,left ,right))
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(,(opu32 o) ,left ,right))
	 ((and (memq tl '(int32 uint32)) (memq tr '(int32 uint32)))
	  `(,(opfx o) ,(tolong32 left tl) ,(tolong32 right tr)))
	 ((eq? tl 'int32)
	  `(if (fixnum? ,right)
	       (,(ops32 o) ,left (fixnum->int32 ,right))
	       ,(cmp/32js o lhs tl left rhs tr right)))
	 ((eq? tr 'int32)
	  `(if (fixnum? ,left)
	       (,(ops32 o) (fixnum->int32 ,left) ,right)
	       ,(cmp/32js o lhs tl left rhs tr right)))
	 ((eq? tl 'uint32)
	  `(if (fixnum? ,right)
	       (if (>=fx ,right 0)
		   (,(opu32 o) ,left (fixnum->uint32 ,right))
		   #f)
	       ,(if (memq tr '(int32 uint32 int53 integer real number))
		    (cmp/32js o lhs tl left rhs tr right)
		    (js-binop loc o left tl right tr conf))))
	 ((eq? tr 'uint32)
	  `(if (fixnum? ,left)
	       ,(if (range-positive? lhs)
		    `(,(opu32 o) (fixnum->uint32 ,left) ,right)
		    (if (inrange-int30? rhs)
			`(,(opfx o) ,left ,(uint32->fixnum right))
			`(if (>=fx ,left 0)
			     (,(opu32 o) (fixnum->uint32 ,left) ,right)
			     #t)))
	       ,(if (memq tl '(int32 uint32 int53 integer real number))
		    (cmp/32js o lhs tl left rhs tr right)
		    (js-binop loc o left tl right tr conf))))
	 ((and (memq tr '(int32 uint32 integer real number))
	       (memq tl '(int32 uint32 integer real number)))
	  (if-fixnums? left tl right tr
	     `(,(opfx o) ,left ,right)
	     (cmp/32js o lhs tl left right tr right)))
	 (else
	  (if-fixnums? left tl right tr
	      `(,(opfx o) ,left ,right)
	      (js-binop loc o left tl right tr conf)))))
   
   (define (cmp/64js o lhs tl left rhs tr right)
      (tprint `(= ,left ,right) " tl=" tl " tr=" tr)
      (if (memq o '(== === != !==))
	  `(= ,(tonumber64 left tl) ,(tonumber64 right tr))
	  `(,(opjs o) ,(tonumber64 left tl) ,(tonumber64 right tr) %this)))
   
   (define (cmp/64 o lhs tl left rhs tr right)
      (cond
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(,(ops32 o) ,left ,right))
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(,(opu32 o) ,left ,right))
	 ((and (memq tl '(int32 uint32 int53)) (memq tr '(int32 uint32 int53)))
	  `(,(opfx o) ,(tolong64 left tl) ,(tolong64 right tr)))
	 ((and (memq tl '(int32 uint32 int53))
	       (memq tr '(int32 uint32 int53 integer real number)))
	  `(if (fixnum? ,right)
	       (,(opfx o) ,(tolong64 left tl) ,right)
	       ,(cmp/64js o lhs tl left right tr right)))
	 ((and (memq tr '(int32 uint32 int53))
	       (memq tl '(int32 uint32 int53 integer real number)))
	  `(if (fixnum? ,left)
	       (,(opfx o) ,left ,(tolong64 right tr))
	       ,(cmp/64js o lhs tl left right tr right)))
	 ((and (memq tl '(int32 uint32 int53 integer real number))
	       (memq tr '(int32 uint32 int53 integer real number)))
	  (if-fixnums? left tl right tr
	     `(,(opfx o) ,left ,right)
	     (cmp/64js o lhs tl left right tr right)))
	 (else
	  (if-fixnums? left tl right tr
	     `(,(opfx o) ,left ,right)
	     (js-binop loc o left tl right tr conf)))))

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
	     (js-binop loc op left lhs right rhs conf))))
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
		 (js-binop loc op left lhs right rhs conf))))))
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
		(js-binop loc op left lhs right rhs conf)))))))

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
	       (cond
		  ((memq type '(int32 uint32 integer number))
		   (case op
		      ((>> <<)
		       `(,(bitop op) ,(toint32 left tl) ,(mask32 right rhs)))
		      ((>>>)
		       `(,(bitop op) ,(touint32 left tl) ,(mask32 right rhs)))
		      (else
		       `(,(bitop op) ,(toint32 left tl) ,(toint32 right tr)))))
		  ((memq tr '(int32 uint32 integer number))
		      (case op
			 ((>> <<)
			  (j2s-cast
			     `(,(bitop op)
			       (js-toint32 ,left %this) ,(mask32 right rhs))
			     #f 'int32 type conf))
			 ((>>>)
			  (j2s-cast
			     `(,(bitop op)
			       (js-touint32 ,left %this) ,(mask32 right rhs))
			     #f 'uint32 type conf))
			 (else
			  (j2s-cast
			     `(,(bitop op)
			       (js-toint32 ,left %this) ,(toint32 right tr))
			     #f 'int32 type conf))))
		  (else
		   `(,(bitopjs op) ,left ,right %this))))))))

;*---------------------------------------------------------------------*/
;*    js-plus ...                                                      */
;*---------------------------------------------------------------------*/
(define (js-plus loc type lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)
   
   (define (u32op) (symbol-append '+ 'u32))
   (define (fxop) (symbol-append '+ 'fx))
   (define (strop) (symbol-append 'js-jsstring '+ '?))
   (define (jsopfx) (symbol-append 'js '+ 'fx))
   (define (jsopfx32) (symbol-append 'js '+ 'fx32))
   (define (jsopfx64) (symbol-append 'js '+ 'fx64))
   (define (jsop) (symbol-append 'js '+))
   
   (define (js-binopfx x y conf)
      (case (config-get conf :long-size 32)
	 ((32) `(,(jsopfx32) ,x ,y))
	 ((64) `(,(jsopfx64) ,x ,y))
	 (else `(,(jsopfx) ,x ,y))))

   (define (tonumber sexp expr)
      (let ((etype (j2s-type-ref expr)))
	 (if (m64? conf)
	     (tonumber64 sexp etype)
	     (tonumber32 sexp etype))))
   
   (cond
      ((and (is-uint32? lhs) (is-uint32? rhs) (type-uint32? type) (u32? conf))
       (binop lhs rhs mode return conf hint 'uint32
	  (lambda (left right)
	     `(,(u32op) ,left ,right))))
      ((and (is-int53? lhs) (is-int53? rhs) (m64? conf))
       (if (type-int53? type)
	   (binop lhs rhs mode return conf hint type
	      (lambda (left right)
		 `(,(fxop) ,left ,right)))
	   (binop lhs rhs mode return conf hint type
	      (lambda (left right)
		 (js-binopfx left right conf)))))
      ((and (is-fx? lhs) (is-fx? rhs))
       (if (type-fixnum? type)
	   (binop lhs rhs mode return conf hint type
	      (lambda (left right)
		 `(,(fxop) ,left ,right)))
	   (binop lhs rhs mode return conf hint type
	      (lambda (left right)
		 (js-binopfx left right conf)))))
      ((and (or (eq? (j2s-type-ref lhs) 'string)
		(eq? (j2s-type-ref rhs) 'string)))
       (binop lhs rhs mode return conf hint type
	  (lambda (left right)
	     (cond
		((and (eq? (j2s-type-ref lhs) 'string)
		      (eq? (j2s-type-ref rhs) 'string))
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
		(js-binopfx left right conf)
		(js-binop-arithmetic loc '+ left lhs right rhs conf)))))
      ((and (is-number? lhs) (is-number? rhs))
       (binop lhs rhs mode return conf hint 'integer
	  (lambda (left right)
	     (scm-if (scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		(js-binopfx left right conf)
		`(,(jsop) ,(tonumber left lhs) ,(tonumber right rhs) %this)))))
      (else
       (binop lhs rhs mode return conf hint 'integer
	  (lambda (left right)
	     (if (and (maybe-number? lhs) (maybe-number? rhs))
		 `(if ,(scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		      ,(js-binopfx left right conf)
		      (,(jsop) ,left ,right %this))
		 `(,(jsop) ,left ,right %this)))))))

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

   (cond
      ((type-number? type)
       (js-arithmetic-addsub loc '+ type lhs rhs mode return conf hint totype))
      ((or (eq? type 'string)
	   (eq? (j2s-type-ref lhs) 'string) (eq? (j2s-type-ref rhs) 'string))
       (add-string loc type lhs rhs mode return conf hint totype))
      (else
       (js-plus loc type lhs rhs mode return conf hint totype))))

;*---------------------------------------------------------------------*/
;*    js-arithmetic-addsub ...                                         */
;*---------------------------------------------------------------------*/
(define (js-arithmetic-addsub loc op::symbol type lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)

   (define (opu32 op) (symbol-append op 'u32))
   (define (opu32/overflow op) (symbol-append op 'u32/overflow))
   (define (ops32 op) (symbol-append op 's32))
   (define (ops32/overflow op) (symbol-append op 's32/overflow))
   (define (opfx op) (symbol-append op 'fx))
   (define (opfx/overflow op) (symbol-append op 'fx/overflow))
   (define (op/overflow op) (symbol-append op '/overflow))
   (define (opjs op) (symbol-append op 'js))

   (define (addsub-generic/32 op type lhs tl left rhs tr right)
      (cond
	 ((and (eq? tl 'bint) (eq? tr 'bint))
	  `(,(opfx/overflow op) ,left ,right))
	 ((and (eq? tl 'bint) (eq? tr 'int32))
	  (if (inrange-int30? rhs)
	      `(,(opfx/overflow op) ,left ,(tolong32 right tr))
	      `(,(ops32/overflow op) ,(toint32/32 left tl) ,right)))
	 ((inrange-int32? lhs)
	  (if (inrange-int32? rhs)
	      `(,(ops32/overflow op) ,(toint32/32 left tl) ,(toint32/32 right tr))
	      `(if (fixnum? ,right)
		   (,(opfx/overflow op) ,(tolong32 left tl) ,right)
		   (,(opjs op) ,(tonumber32 left tl) ,(tonumber32 right tr) %this))))
	 ((inrange-int32? rhs)
	  `(if (fixnum? ,left)
	       (,(opfx/overflow op) ,left ,(tolong32 right tr))
	       (,(opjs op) ,(tonumber32 left tl) ,(tonumber32 right tr) %this)))
	 (else
	  (if-fixnums? left tl right tr
	     `(,(opfx/overflow op) ,(tolong32 left tl) ,(tolong32 right tr))
	     `(,(opjs op) ,(tonumber32 left tl) ,(tonumber32 right tr) %this)))))

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
		 (if (fixnum? ,n) (fixnum->int32 ,n) (flonum->int32 ,n)))))))
   
   (define (addsub-uint32/32 op lhs tl left rhs tr right)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(,(opu32 op) ,left ,right))
	 ((and (eq? tl 'uint32) (eq? tr 'int32) (inrange-uint32? rhs))
	  `(,(opu32 op) ,left ,(touint32/w-overflow right tr)))
	 ((and (eq? tl 'int32) (eq? tr 'uint32) (inrange-uint32? lhs))
	  `(,(opu32 op) ,(touint32/w-overflow left tl) ,right))
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
      (cond
	 ((and (eq? tl 'bint) (eq? tr 'bint))
	  `(,(opfx/overflow op) ,left ,right))
	 ((and (eq? tl 'bint) (eq? tr 'int32))
	  `(,(opfx op) ,left ,(tolong64 right tr)))
	 ((and (eq? tl 'bint) (eq? tr 'uint32))
	  `(,(opfx/overflow op) ,left ,(tolong64 right tr)))
	 ((and (inrange-int32? lhs) (inrange-int32? rhs))
	  `(,(opfx op) ,(tolong64 left tl) ,(tolong64 right tr)))
	 ((inrange-int53? lhs)
	  (if (inrange-int53? rhs)
	      `(,(opfx/overflow op) ,(tolong64 left tl) ,(tolong64 right tr))
	      `(if (fixnum? ,right)
		   (,(opfx op) ,(tolong64 left tl) ,right)
		   (,(opjs op) ,(tonumber64 left tl) ,(tonumber64 right tr) %this))))
	 ((inrange-int53? rhs)
	  `(if (fixnum? ,left)
	       (,(opfx/overflow op) ,left ,(tolong64 right tr))
	       (,(opjs op) ,(tonumber64 left tl) ,(tonumber64 right tr) %this)))
	 (else
	  (if-fixnums? left tl right tr
	     `(,(opfx/overflow op) ,(tolong64 left tl) ,(tolong64 right tr))
	     `(,(opjs op) ,(tonumber64 left tl) ,(tonumber64 right tr) %this)))))

   (define (addsub-number/64 op type lhs tl left rhs tr right)
      (cond
	 ((and (memq tl '(int32 uint32)) (memq tr '(int32 uint32)))
	  `(,(opfx/overflow op) ,(tolong64 left tl) ,(tolong64 right tr)))
	 ((and (eq? tl 'uint32) (eq? tr 'int32))
	  `(,(opfx/overflow op) (uint32->fixnum ,left) (int32->fixnum ,right)))
	 ((and (eq? tl 'number) (memq tr '(int32 uint32)))
	  `(if (fixnum? ,left)
	       (,(opfx/overflow op) ,left ,(tolong64 right tr))
	       (,(op/overflow op) ,(tonumber64 left tl) ,(tolong64 right tr))))
	 ((and (eq? tl 'integer) (eq? tr 'integer))
	  `(,(opfx/overflow op) ,left ,right))
	 ((and (eq? tl 'bint) (eq? tr 'bint))
	  `(,(opfx/overflow op) ,left ,right))
	 ((and (eq? tl 'bint) (eq? tr 'int32))
	  `(,(opfx op) ,left ,(tolong64 right tr)))
	 ((and (eq? tl 'bint) (eq? tr 'uint32))
	  `(,(opfx/overflow op) ,left ,(tolong64 right tr)))
	 (else
	  (if-fixnums? left tl right tr
	     `(,(opfx/overflow op) ,left ,right)
	     `(,(op/overflow op) ,(tonumber64 left tl) ,(tonumber64 right tr))))))

   (define (addsub-long/64 op lhs tl left rhs tr right)
      `(,(opfx op) ,(tolong64 left tl) ,(tolong64 right tr)))

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
	      ,(addsub-generic/64 op type lhs tl left rhs tr right)))))
      
   (define (addsub-int53/64 op lhs tl left rhs tr right)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(uint32->fixnum (+u32 ,left ,right)))
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(,(opfx op) (int32->fixnum ,left) (int32->fixnum ,right)))
	 ((and (memq tl '(int32 uint32 int53)) (memq tr '(int32 uint32 int53)))
	  `(,(opfx op) ,(tolong64 left tl) ,(tolong64 right tr)))
	 ((and (eq? tl 'integer) (eq? tr 'integer))
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
      (case type
	 ((int32)
	  (cond
	     ((and (eq? tl 'int32) (eq? tr 'int32))
	      `(*s32 ,left ,right))
	     ((and (eq? tl 'uint32) (eq? tr 'uint32))
	      `(uint32->int32 (*u32 ,left ,right)))
	     (else
	      `(fixnum->int32
		  `(*js ,(tonumber64 left tl)
		      ,(tonumber64 right tr) %this)))))
	 ((uint32)
	  (cond
	     ((and (eq? tl 'int32) (eq? tr 'int32))
	      `(int32->uint32 (*s32 ,left ,right)))
	     ((and (eq? tl 'uint32) (eq? tr 'uint32))
	      `(uint32->int32 (*u32 ,left ,right)))
	     (else
	      `(fixnum->int32
		  `(*js ,(tonumber64 left tl)
		      ,(tonumber64 right tr) %this)))))
	 ((integer)
	  (cond
	     ((and (eq? tl 'int32) (eq? tr 'int32))
	      `(int32->fixnum (*s32 ,left ,right)))
	     ((and (eq? tl 'uint32) (eq? tr 'uint32))
	      `(uint32->fixnum (*u32 ,left ,right)))
	     (else
	      `(*js ,(tonumber64 left tl)
		  ,(tonumber64 right tr) %this))))
	 ((number obj any object)
	  (cond
	     ((and (eq? tl 'int32) (eq? tr 'int32))
	      `(*/overflow (int32->fixnum ,left) (int32->fixnum ,right)))
	     ((and (eq? tl 'uint32) (eq? tr 'uint32))
	      `(*u32/overflow ,left ,right))
	     (else
	      `(*js ,(tonumber64 left tl) ,(tonumber64 right tr) %this))))
	 (else
	  (error "mul/32" "illegal integer type" type))))
   
   (define (mul/64 type lhs tl left rhs tr right)
      (case type
	 ((int32)
	  (cond
	     ((and (memq tl '(int32 uint32 int53))
		   (memq tr '(int32 uint32 int53)))
	      `(fixnum->int32
		  (*fx ,(tolong64 left tl) ,(tolong64 right tr))))
	     (else
	      `(fixnum->int32
		  `(*js ,(tonumber64 left tl) ,(tonumber64 right tr) %this)))))
	 ((uint32)
	  (cond
	     ((and (memq tl '(int32 uint32 int53))
		   (memq tr '(int32 uint32 int53)))
	      `(fixnum->uint32
		  (*fx ,(tolong64 left tl) ,(tolong64 right tr))))
	     (else
	      `(fixnum->uint32
		  (*js ,(tonumber64 left tl) ,(tonumber64 right tr) %this)))))
	 ((int53)
	  (if (and (memq tl '(int32 uint32 int53))
		   (memq tr '(int32 uint32 int53)))
	      `(*fx ,(tolong64 left tl) ,(tolong64 right tr))
	      `(*js ,(tonumber64 left tl) ,(tonumber64 right tr) %this)))
	 ((number obj any object)
	  (if (and (memq tl '(int32 uint32 int53))
		   (memq tr '(int32 uint32 int53)))
	      `(*/overflow ,(tolong64 left tl) ,(tolong64 right tr))
	      `(*js ,(tonumber64 left tl) ,(tonumber64 right tr) %this)))
	 (else
	  (error "mul/64" "illegal integer type" type))))
   
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

   (define (divs32 left right tl tr)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(uint32->int32 (/u32 ,left ,right)))
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(/s32 ,left ,right))
	 (else
	  (if-fixnums? left tl right tr
	     `(fixnum->int32 (/fx ,left ,right))
	     `(js-toint32 (/js ,left ,right %this) %this)))))

   (define (divu32 left right tl tr)
      (cond
	 ((and (eq? tl 'uint32) (eq? tr 'uint32))
	  `(/u32 ,left ,right))
	 ((and (eq? tl 'int32) (eq? tr 'int32))
	  `(int32->uint32 (/s32 ,left ,right)))
	 (else
	  (if-fixnums? left tl right tr
	     `(fixnum->uint32 (/fx ,left ,right))
	     `(js-touint32 (/js ,left ,right %this) %this)))))

   (define (divjs left right tl tr)
      (cond
	 ((eq? tl 'uint32)
	  (if (eq? tr 'uint32)
	      `(if (and (not (=u32 ,right #u32:0))
			(=u32 (remainderu32 ,left ,right) #u32:0))
		   (js-uint32-tointeger (/u32 ,left ,right))
		   (/fl (uint32->flonum ,left)
		      (uint32->flonum ,right)))
	      `(/fl (uint32->flonum ,left) ,(todouble right tr))))
	 ((eq? tl 'int32)
	  (if (eq? tr 'int32)
	      `(if (and (not (=s32 ,right #s32:0))
			(=s32 (remainders32 ,left ,right) #s32:0))
		   (js-int32-tointeger (/s32 ,left ,right))
		   (/fl (int32->flonum ,left)
		      (int32->flonum ,right)))
	      `(/fl (int32->flonum ,left) ,(todouble right tr))))
	 ((eq? tr 'uint32)
	  `(/fl ,(todouble left tl) (uint32->flonum ,right)))
	 ((eq? tr 'int32)
	  `(/fl ,(todouble left tl) (int32->flonum ,right)))
	 ((eq? tl 'integer)
	  (if (eq? tr 'integer)
	      `(if (and (not (=fx ,right 0))
			(=fx (remainderfx ,left ,right) 0))
		   (/fx ,left ,right)
		   (/fl (fixnum->flonum ,left) (fixnum->flonum ,right)))
	      `(/fl (fixnum->flonum ,left) ,(todouble right tr) %this)))
	 ((eq? tr 'integer)
	  `(/js ,(todouble left tl) (fixnum->flonum ,right)))
	 (else
	  (if-fixnums? left tl right tr
	     `(if (and (not (=fx ,right 0))
		       (=fx (remainderfx ,left ,right) 0))
		  (/fx ,left ,right)
		  (/fl (fixnum->flonum ,left)
		     (fixnum->flonum ,right)))
	     `(/js ,left ,right %this)))))
   
   (let ((k (power2 rhs)))
      (if k
	  (div-power2 k)
	  (binop lhs rhs mode return conf hint '*
	     (lambda (left right)
		(let ((tl (j2s-type-ref lhs))
		      (tr (j2s-type-ref rhs)))
		   (epairify loc
		      (case type
			 ((int32) (divs32 left right tl tr))
			 ((uint32) (divu32 left right tl tr))
			 (else (divjs left right tl tr))))))))))

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
;*    js-arithmetic-% ...                                              */
;*---------------------------------------------------------------------*/
(define (js-arithmetic-% loc type lhs rhs
	   mode return conf hint::pair-nil totype)
   (binop lhs rhs mode return conf hint '*
      (lambda (left right)
	 (let ((tl (j2s-type-ref lhs))
	       (tr (j2s-type-ref rhs)))
	    (epairify loc
	       (cond
		  ((and (eq? tl 'int32) (eq? tr 'int32))
		   `(if (=s32 ,right #s32:0)
			+nan.0
			(js-int32-tointeger (remainders32 ,left ,right))))
		  ((and (eq? tl 'uint32) (eq? tr 'uint32))
		   `(if (=u32 ,right #u32:0)
			+nan.0
			(js-uint32-tointeger (remainders32 ,left ,right))))
		  ((and (eq? tl 'integer) (eq? tr 'integer))
		   (binop lhs rhs mode return conf hint 'any
		      (lambda (left right)
			 (if (and (number? right) (= right 0))
			     +nan.0
			     `(js-%$$NN ,left ,right)))))
		  (else
		   (if (m64? conf)
		       (if (and (number? right) (not (= right 0)))
			   `(js-%$$NZ ,(tonumber64 left tl)
			       ,(tonumber64 right tr))
			   `(js-%$$NN ,(tonumber64 left tl)
			       ,(tonumber64 right tr)))
		       (if (and (number? right) (not (= right 0)))
			   `(js-%$$NZ ,(tonumber32 left tl)
			       ,(tonumber32 right tr))
			   `(js-%$$NN ,(tonumber32 left tl)
			       ,(tonumber32 right tr)))))))))))

;*---------------------------------------------------------------------*/
;*    overflow29 ...                                                   */
;*    -------------------------------------------------------------    */
;*    2^53-1 overflow                                                  */
;*    -------------------------------------------------------------    */
;*    See Hacker's Delight (second edition), H. Warren J.r,            */
;*    Chapter 4, section 4.1, page 68                                  */
;*---------------------------------------------------------------------*/
(define (overflow29 v::long)
   (let* ((a (-fx 0 (bit-lsh 1 29)))
	  (b (-fx (bit-lsh 1 29) 1))
	  (b-a (-fx b a)))
      (if (<=u32 (fixnum->uint32 (-fx v a)) (fixnum->uint32 b-a))
	  v
	  (fixnum->flonum v))))

;*---------------------------------------------------------------------*/
;*    tonumber32 ...                                                   */
;*---------------------------------------------------------------------*/
(define (tonumber32 val type::symbol)
   (case type
      ((int32)
       (if (int32? val)
	   (if (overflow29 (int32->fixnum val))
	       (int32->flonum val)
	       (int32->fixnum val))
	   `(if (overflow29 (int32->fixnum ,val))
		(int32->flonum ,val)
		(int32->fixnum ,val))))
      ((uint32)
       (if (uint32? val)
	   (if (<u32 val (bit-lshu32 #u32:1 29))
	       (uint32->fixnum val)
	       (uint32->flonum val))
	   `(if (<u32 ,val ,(bit-lshu32 #u32:1 29))
		(uint32->fixnum ,val)
		(uint32->flonum ,val))))
      (else val)))

;*---------------------------------------------------------------------*/
;*    toint32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (toint32 val type)
   (case type
      ((int32)
       val)
      ((uint32)
       (if (and (uint32? val) (<u32 val (bit-lshu32 #u32:1 30)))
	   (uint32->int32 val)
	   `(uint32->int32 ,val)))
      (else
       (if (and (fixnum? val) (fixnum? (overflow29 val)))
	   (fixnum->int32 val)
	   `(if (fixnum? ,val) (fixnum->int32 ,val) (js-toint32 ,val %this))))))

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
       (if (and (fixnum? val) (fixnum? (overflow29 val)))
	   (fixnum->uint32 val)
	   `(if (fixnum? ,val) (fixnum->uint32 ,val) (js-touint32 ,val %this))))))

;*---------------------------------------------------------------------*/
;*    touint32/w-overflow ...                                          */
;*---------------------------------------------------------------------*/
(define (touint32/w-overflow val type)
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
(define (toint32/32 val type::symbol)
   (case type
      ((int32) val)
      ((uint32) `(uint32->int32 ,val))
      (else `(fixnum->int32 ,val))))

;*---------------------------------------------------------------------*/
;*    tolong32 ...                                                     */
;*---------------------------------------------------------------------*/
(define (tolong32 val type::symbol)
   
   (define (int32-29? val)
      (and (>=s32 val (negs32 (bit-lshs32 #u32:1 29)))
	   (<s32 val (bit-lshs32 #s32:1 29))))

   (define (uint32-29? val)
      (<u32 val (bit-lshu32 #u32:1 29)))
   
   (case type
      ((int32)
       (if (and (int32? val) (int32-29? val))
	   (int32->fixnum val)
	   `(int32->fixnum ,val)))
      ((uint32)
       (if (and (uint32? val) (uint32-29? val))
	   (uint32->fixnum val)
	   `(uint32->fixnum ,val)))
      (else val)))

;*---------------------------------------------------------------------*/
;*    tolong64 ...                                                     */
;*---------------------------------------------------------------------*/
(define (tolong64 val type::symbol)
   (tonumber64 val type))

;*---------------------------------------------------------------------*/
;*    tonumber64 ...                                                   */
;*---------------------------------------------------------------------*/
(define (tonumber64 val type::symbol)
   (case type
      ((int32) (if (int32? val) (int32->fixnum val) `(int32->fixnum ,val)))
      ((uint32) (if (uint32? val) (uint32->fixnum val) `(uint32->fixnum ,val)))
      ((integer real number) val)
      (else `(js-tonumber ,val %this))))

;*---------------------------------------------------------------------*/
;*    todouble ...                                                     */
;*---------------------------------------------------------------------*/
(define (todouble val type::symbol)
   (case type
      ((int32)
       (if (int32? val) (int32->flonum val) `(int32->flonum ,val)))
      ((uint32)
       (if (uint32? val) (uint32->flonum val) `(uint32->flonum ,val)))
      ((integer)
       (if (fixnum? val) (fixnum->flonum val) `(fixnum->flonum ,val)))
      ((real)
       val)
      ((number)
       `(if (fixnum? ,val) (fixnum->flonum ,val) ,val))
      (else
       `(js-toflonum (js-tonumber ,val %this)))))

;*---------------------------------------------------------------------*/
;*    fixnums? ...                                                     */
;*---------------------------------------------------------------------*/
(define (fixnums? left tl right tr)
   (cond
      ((eq? tl 'integer) (if (eq? tr 'integer) #t `(fixnum? ,right)))
      ((eq? tr 'integer) `(fixnum? ,left))
      ((and (memq tl '(int32 uint32 int53 integer number any unknown))
	    (memq tr '(int32 uint32 int53 integer number any unknown)))
       `(and (fixnum? ,left) (fixnum? ,right)))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    if-fixnums? ...                                                  */
;*---------------------------------------------------------------------*/
(define (if-fixnums? left tl right tr then else)
   (let ((test (fixnums? left tl right tr)))
      (cond
	 ((eq? test #t) then)
	 ((eq? test #f) else)
	 (else `(if ,test ,then ,else)))))
