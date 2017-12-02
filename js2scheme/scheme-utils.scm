;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-utils.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:06:27 2017                          */
;*    Last change :  Sat Dec  2 19:06:06 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Utility functions for Scheme code generation                     */
;*=====================================================================*/

(module __js2scheme_scheme-utils
   
   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage)
   
   (export j2s-unresolved-put-workspace
           j2s-unresolved-del-workspace
	   j2s-unresolved-get-workspace
	   j2s-unresolved-call-workspace
	   
	   (epairify loc expr)
	   (epairify-deep loc expr)
	   (m64? conf)
	   (u32? conf)
	   (strict-mode? mode)
	   (j2s-fast-id id)
	   (j2s-scheme-id id pref)
	   (j2s-decl-scheme-id ::J2SDecl)
	   (js-need-global? ::J2SDecl scope mode)
	   (flatten-stmt stmt)
	   (flatten-nodes nodes)
	   (flatten-begin nodes)
	   (remove-undefined lst)
	   (j2s-this-cache? ::J2SDecl)
	   (j2s-minlen val)
	   
	   (typeof-this obj conf)
	   (maybe-number? expr::J2SNode)
	   (utype-ident ident utype conf #!optional compound)
	   (type-ident ident type)
	   (j2s-number val conf)
	   (j2s-error proc msg obj #!optional str)
	   (is-fixnum? expr::J2SExpr conf)
	   (is-number? expr::J2SExpr)
	   (is-integer? expr::J2SExpr)
	   (is-int30? expr::J2SExpr)
	   (is-int53? expr::J2SExpr)
	   (is-fx? expr::J2SExpr)
	   (is-uint32? expr::J2SExpr)
	   (is-uint53? expr::J2SExpr)
	   (is-string? expr::J2SExpr)

	   (j2s-jsstring val loc)
	   (js-string->jsstring ::bstring)
	   
	   (j2s-unresolved name cache throw)
	   (js-not expr)
	   
	   (js-pcache cache)
	   (js-object-get-name/cache::symbol clevel::long)
	   (js-object-put-name/cache!::symbol clevel::long)
	   
	   (j2s-get loc obj tyobj prop typrop cache #!optional (clevel 100))
	   (j2s-put! loc obj tyobj prop val mode cache #!optional (clevel 100))))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved-workspaces ...                                    */
;*---------------------------------------------------------------------*/
(define j2s-unresolved-put-workspace '%this)
(define j2s-unresolved-del-workspace '%this)
(define j2s-unresolved-get-workspace '%scope)
(define j2s-unresolved-call-workspace '%this)

;*---------------------------------------------------------------------*/
;*    epairify ...                                                     */
;*---------------------------------------------------------------------*/
(define (epairify loc expr)
   (econs (car expr) (cdr expr) loc))

;*---------------------------------------------------------------------*/
;*    epairify-deep ...                                                */
;*---------------------------------------------------------------------*/
(define (epairify-deep loc expr)
   (if (or (epair? expr) (not (pair? expr)))
       expr
       (econs (epairify-deep loc (car expr))
	  (epairify-deep loc (cdr expr))
	  loc)))

;*---------------------------------------------------------------------*/
;*    m64? ...                                                         */
;*---------------------------------------------------------------------*/
(define (m64? conf)
   (=fx (config-get conf :long-size 0) 64))

;*---------------------------------------------------------------------*/
;*    u32? ...                                                         */
;*---------------------------------------------------------------------*/
(define (u32? conf)
   (>=fx (config-get conf :optim 0) 4))

;*---------------------------------------------------------------------*/
;*    strict-mode? ...                                                 */
;*---------------------------------------------------------------------*/
(define (strict-mode? mode)
   (or (eq? mode 'strict) (eq? mode 'hopscript)))

;*---------------------------------------------------------------------*/
;*    j2s-fast-id ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-fast-id id)
   (symbol-append '@ id))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-id ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-id id pref)
   (cond
      ((char=? (string-ref (symbol->string! id) 0) #\%) id)
      ((memq id '(GLOBAL global arguments)) id)
      (else (symbol-append pref id))))

;*---------------------------------------------------------------------*/
;*    j2s-decl-scheme-id ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-decl-scheme-id decl::J2SDecl)
   (with-access::J2SDecl decl (_scmid id scope)
      (if _scmid
	  _scmid
	  (let ((sid (j2s-scheme-id id (if (eq? scope '%scope) '! '^))))
	     (set! _scmid sid)
	     sid))))

;*---------------------------------------------------------------------*/
;*    js-need-global? ...                                              */
;*---------------------------------------------------------------------*/
(define (js-need-global? decl::J2SDecl scope mode)
   (with-access::J2SDecl decl (usage)
      (or (not (j2s-let-opt? decl))
	  (memq 'eval usage)
	  (not (and (eq? scope '%scope) (eq? mode 'hopscript))))))

;*---------------------------------------------------------------------*/
;*    flatten-begin ...                                                */
;*---------------------------------------------------------------------*/
(define (flatten-begin nodes)
   (cond
      ((null? nodes) `(js-undefined))
      ((null? (cdr nodes)) (car nodes))
      (else `(begin ,@(remove-undefined (flatten-nodes nodes))))))

;*---------------------------------------------------------------------*/
;*    flatten-stmt ...                                                 */
;*---------------------------------------------------------------------*/
(define (flatten-stmt stmt)
   (when (and (pair? stmt) (eq? (car stmt) 'begin))
      (set-cdr! stmt (flatten-nodes (cdr stmt))))
   stmt)

;*---------------------------------------------------------------------*/
;*    flatten-nodes ...                                                */
;*---------------------------------------------------------------------*/
(define (flatten-nodes nodes)
   (append-map
      (lambda (l)
	 (if (and (pair? l) (eq? (car l) 'begin))
	     (flatten-nodes (cdr l))
	     (list l)))
      nodes))

;*---------------------------------------------------------------------*/
;*    remove-undefined ...                                             */
;*---------------------------------------------------------------------*/
(define (remove-undefined lst)
   (cond
      ((or (null? lst) (null? (cdr lst)))
       lst)
      ((equal? (car lst) '(js-undefined))
       (remove-undefined (cdr lst)))
      (else
       (let loop ((prev lst)
		  (run (cdr lst)))
	  (cond
	     ((or (null? run) (null? (cdr run)))
	      lst)
	     ((equal? (car run) '(js-undefined))
	      (set-cdr! prev (cdr run))
	      (loop prev (cdr run)))
	     (else
	      (loop run (cdr run))))))))

;*---------------------------------------------------------------------*/
;*    j2s-this-cache? ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-this-cache? this::J2SDecl)
   (with-access::J2SDecl this (usage usecnt)
      (and (>=fx usecnt 3)
	   (not (memq 'ref usage))
	   (not (memq 'call usage))
	   (not (memq 'new usage)))))

;*---------------------------------------------------------------------*/
;*    j2s-minlen ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-minlen val)
   (with-access::J2SFun val (params vararg name)
      (let ((len 0))
	 (for-each (lambda (p)
		      (when (or (not (isa? p J2SDeclInit))
				(with-access::J2SDeclInit p (val)
				   (nodefval? val)))
			 (set! len (+fx len 1))))
	    params)
	 (if (eq? vararg 'rest)
	     (-fx len 1)
	     len))))

;*---------------------------------------------------------------------*/
;*    typeof-this ...                                                  */
;*---------------------------------------------------------------------*/
(define (typeof-this obj conf)
   (let ((ty (j2s-type obj)))
      (if (eq? ty 'object)
	  (if (and (isa? obj J2SThis)
		   (with-access::J2SThis obj (decl)
		      (with-access::J2SDecl decl (vtype)
			 (eq? vtype 'this))))
	      'this
	      ty)
	  ty)))

;*---------------------------------------------------------------------*/
;*    maybe-number? ...                                                */
;*---------------------------------------------------------------------*/
(define (maybe-number? expr::J2SNode)
   (memq (j2s-type expr)
      '(index uint29 int30 fixnum int53 ufixnum integer number any)))

;*---------------------------------------------------------------------*/
;*    utype-ident ...                                                  */
;*---------------------------------------------------------------------*/
(define (utype-ident ident utype conf #!optional compound)

   (define (atomic-type? typ)
      (memq typ '(uint29 index uint32 length number integer
		  int30 int53 fixnum undefined bool null)))

   (cond
      ((or (eq? utype 'any) (eq? utype 'unknown))
       ident)
      (compound
       (symbol-append ident '|::| (type-name utype conf)))
      (else
       ident)))

;*---------------------------------------------------------------------*/
;*    type-ident ...                                                   */
;*---------------------------------------------------------------------*/
(define (type-ident ident type)
   (cond
      ((memq type '(int32 uint32)) (symbol-append ident '|::| type))
      ((memq type '(uint29)) (symbol-append ident '|::uint32|))
      ((memq type '(index length)) (symbol-append ident '|::uint32|))
      (else ident)))

;*---------------------------------------------------------------------*/
;*    j2s-number ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-number val conf)
   (cond
      ((elong? val)
       (cond
	  ((and (>=elong val (-elong #e0 (bit-lshelong #e1 29)))
		(<=elong val (bit-lshelong #e1 29)))
	   (elong->fixnum val))
	  ((=fx (config-get conf :long-size 0) 64)
	   (cond-expand
	      ((or bint30 bint32)
	       `(elong->fixnum ,val))
	      (else
	       (elong->fixnum val))))
	  (else
	   (elong->flonum val))))
      ((llong? val)
       (cond
	  ((and (>=llong val (-llong #l0 (bit-lshllong #l1 29)))
		(<=llong val (bit-lshllong #l1 29)))
	   (llong->fixnum val))
	  ((and (=fx (config-get conf :long-size 0) 64)
		(>=llong val (-llong #l0 (bit-lshllong #l1 53)))
		(<=llong val (bit-lshllong #l1 53)))
	   (cond-expand
	      ((or bint30 bint32)
	       `(llong->fixnum ,val))
	      (else
	       (llong->fixnum val))))
	  (else
	   (llong->flonum val))))
      ((bignum? val)
       (bignum->flonum val))
      ((fixnum? val)
       (cond-expand
	  ((or bint30 bint32)
	   val)
	  ((or bint61 62 64)
	   (cond
	      ((and (=fx (config-get conf :long-size 0) 64)
		    (<fx val (bit-lsh 1 53))
		    (>fx val (negfx (bit-lsh 1 53))))
	       val)
	      ((and (<fx val (-fx (bit-lsh 1 29) 1))
		    (>=fx val (negfx (bit-lsh 1 29))))
	       val)
	      (else
	       (fixnum->flonum val))))
	  (else
	   (error "j2s-scheme" "unknown integer size"
	      (config-get conf :long-size (bigloo-config 'elong-size))))))
      (else val)))

;*---------------------------------------------------------------------*/
;*    j2s-error ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-error proc msg obj #!optional str)
   (with-access::J2SNode obj (loc)
      (match-case loc
	 ((at ?fname ?loc)
	  (error/location proc msg (or str (j2s->list obj)) fname loc))
	 (else
	  (error proc msg obj)))))

;*---------------------------------------------------------------------*/
;*    is-number? ...                                                   */
;*---------------------------------------------------------------------*/
(define (is-number? expr::J2SExpr)
   (type-number? (j2s-type expr)))

;*---------------------------------------------------------------------*/
;*    is-integer? ...                                                  */
;*---------------------------------------------------------------------*/
(define (is-integer? expr::J2SExpr)
   (type-integer? (j2s-type expr)))

;*---------------------------------------------------------------------*/
;*    is-int30? ...                                                    */
;*---------------------------------------------------------------------*/
(define (is-int30? expr::J2SExpr)
   (type-int30? (j2s-type expr)))

;*---------------------------------------------------------------------*/
;*    is-int53? ...                                                    */
;*---------------------------------------------------------------------*/
(define (is-int53? expr::J2SExpr)
   (let ((ty (j2s-type expr)))
      (or (type-int53? ty) (eq? ty 'ufixnum))))

;*---------------------------------------------------------------------*/
;*    is-fx? ...                                                       */
;*---------------------------------------------------------------------*/
(define (is-fx? expr::J2SExpr)
   (type-fixnum? (j2s-type expr)))

;*---------------------------------------------------------------------*/
;*    is-fixnum? ...                                                   */
;*---------------------------------------------------------------------*/
(define (is-fixnum? expr::J2SExpr conf)
   (if (m64? conf)
       (or (type-integer? (j2s-type expr)) (type-int53? (j2s-type expr)))
       (or (type-int30? (j2s-type expr)) (eq? (j2s-type expr) 'ufixnum))))

;*---------------------------------------------------------------------*/
;*    is-uint32? ...                                                   */
;*---------------------------------------------------------------------*/
(define (is-uint32? expr::J2SExpr)
   (type-uint32? (j2s-type expr)))

;*---------------------------------------------------------------------*/
;*    is-uint53? ...                                                   */
;*---------------------------------------------------------------------*/
(define (is-uint53? expr::J2SExpr)
   (let ((ty (j2s-type expr)))
      (or (type-int53? ty) (eq? ty 'ufixnum))))

;*---------------------------------------------------------------------*/
;*    is-string? ...                                                   */
;*---------------------------------------------------------------------*/
(define (is-string? expr::J2SExpr)
   (eq? (j2s-type expr) 'string))

;*---------------------------------------------------------------------*/
;*    j2s-jsstring ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring val loc)
   (epairify loc
      (js-string->jsstring val)))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-unresolved name cache throw)
   (cond
      ((eq? name 'undefined)
       `(js-undefined))
      (cache
       `(js-global-object-get-name/cache ,j2s-unresolved-get-workspace ',name
	   ,(js-pcache cache)
	   ,(if (pair? throw) `',throw throw)
	   %this))
      (else
       `(js-global-object-get-name ,j2s-unresolved-get-workspace ',name
	   ,(if (pair? throw) `',throw throw)
	   %this))))

;*---------------------------------------------------------------------*/
;*    js-not ...                                                       */
;*---------------------------------------------------------------------*/
(define (js-not expr)
   (match-case expr
      (((kwote not) ?val) val)
      (else `(not ,expr))))

;*---------------------------------------------------------------------*/
;*    js-string->jsstring ...                                          */
;*---------------------------------------------------------------------*/
(define (js-string->jsstring val::bstring)
   (case (string-minimal-charset val)
      ((ascii) `(js-ascii->jsstring ,val))
      ((latin1 utf8) `(js-utf8->jsstring ,val))
      (else (error "string->jsstring" "unsupported encoding"
	       (string-minimal-charset val)))))

;*---------------------------------------------------------------------*/
;*    js-pcache ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache cache)
   `(js-pcache-ref %pcache ,cache))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache ...                                     */
;*---------------------------------------------------------------------*/
(define (js-object-get-name/cache::symbol clevel::long)
   (case clevel
      ((1) 'js-object-get-name/cache-level1)
      ((2) 'js-object-get-name/cache-level2)
      (else 'js-object-get-name/cache)))

;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache! ...                                    */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache!::symbol clevel::long)
   (case clevel
      ((1) 'js-object-put-name/cache-level1!)
      ((2) 'js-object-put-name/cache-level2!)
      (else 'js-object-put-name/cache!)))
   
;*---------------------------------------------------------------------*/
;*    j2s-get ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-get loc obj tyobj prop typrop cache #!optional (clevel 100))

   (define (maybe-string? prop typrop)
      (and (not (number? prop))
	   (not (type-number? typrop))
	   (not (eq? typrop 'array))))
	 
   (let ((prop (match-case prop
		  ((js-utf8->jsstring ?str) str)
		  ((js-ascii->jsstring ?str) str)
		  ((js-string->jsstring ?str) str)
		  (else prop))))
      (cond
	 ((> (bigloo-debug) 0)
	  (if (string? prop)
	      `(js-get/debug ,obj ',(string->symbol prop) %this ',loc)
	      `(js-get/debug ,obj ,prop %this ',loc)))
	 ((eq? tyobj 'array)
	  (if (type-number? typrop)
	      `(js-array-ref ,obj ,prop %this)
	      `(js-get ,obj ,prop %this)))
	 ((eq? tyobj 'string)
	  (cond
	     ((type-uint32? typrop)
	      `(js-jsstring-ref ,obj ,prop))
	     ((type-int32? typrop)
	      (if (or (symbol? prop) (number? prop))
		  `(if (>=fx ,prop 0)
		       (js-jsstring-ref ,obj (fixnum->uint32 ,prop))
		       (js-undefined))
		  (let ((tmp (gensym '%tmp)))
		     `(let ((,tmp ,prop))
			 (if (>=fx ,tmp 0)
			     (js-jsstring-ref ,obj (fixnum->uint32 ,tmp))
			     (js-undefined))))))
	     (else
	      `(js-get-string ,obj ,prop %this))))
	 (cache
	  (cond
	     ((string? prop)
	      (if (string=? prop "length")
		  `(js-get-length ,obj ,(js-pcache cache) %this)
		  (case tyobj
		     ((object this)
		      `(,(js-object-get-name/cache clevel) ,obj
			  ',(string->symbol prop) ,(js-pcache cache) %this))
		     ((global)
		      `(js-global-object-get-name/cache ,obj
			  ',(string->symbol prop) ,(js-pcache cache) #f %this))
		     (else
		      `(js-get-name/cache ,obj
			  ',(string->symbol prop) ,(js-pcache cache) %this)))))
	     ((maybe-string? prop typrop)
	      `(js-get/cache ,obj ,prop ,(js-pcache cache) %this))
	     (else
	      `(js-get ,obj ,prop %this))))
	 ((string? prop)
	  (if (string=? prop "length")
	      `(js-get-length ,obj #f %this)
	      `(js-get ,obj ',(string->symbol prop) %this)))
	 (else
	  `(js-get ,obj ,prop %this)))))

;*---------------------------------------------------------------------*/
;*    j2s-put! ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-put! loc obj tyobj prop val mode cache #!optional (clevel 100))
   (let ((prop (match-case prop
		  ((js-utf8->jsstring ?str) str)
		  ((js-ascii->jsstring ?str) str)
		  ((js-string->jsstring ?str) str)
		  (else prop))))
      (cond
	 ((> (bigloo-debug) 0)
	  (if (string? prop)
	      `(js-put/debug! ,obj ',(string->symbol prop) ,val ,mode %this ',loc)
	      `(js-put/debug! ,obj ,prop ,val ,mode %this ',loc)))
	 ((eq? tyobj 'array)
	  `(js-array-set! ,obj ,prop ,val ,(strict-mode? mode) %this))
	 (cache
	  (cond
	     ((string? prop)
	      (if (string=? prop "length")
		  `(js-put-length! ,obj ,val ,mode ,(js-pcache cache) %this)
		  (begin
		     (case tyobj
			((object global this)
			 `(,(js-object-put-name/cache! clevel)
			   ,obj
			     ',(string->symbol prop) ,val ,mode ,(js-pcache cache) %this))
			(else
			 `(js-put-name/cache! ,obj
			     ',(string->symbol prop) ,val ,mode ,(js-pcache cache) %this))))))
	     ((or (number? prop) (>=fx clevel 10))
	      `(js-put! ,obj ,prop ,val ,mode %this))
	     (else
	      `(js-put/cache! ,obj ,prop ,val ,mode ,(js-pcache cache) %this))))
	 (else
	  (cond
	     ((string? prop)
	      `(js-put! ,obj ',(string->symbol prop) ,val ,mode %this))
	     (else
	      `(js-put! ,obj ,prop ,val ,mode %this)))))))

