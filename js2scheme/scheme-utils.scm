;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-utils.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:06:27 2017                          */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Utility functions for Scheme code generation                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-utils
   
   (include "ast.sch"
	    "usage.sch"
	    "context.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_classutils
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-constant
	   __js2scheme_scheme)
   
   (export j2s-unresolved-put-workspace
           j2s-unresolved-del-workspace
	   j2s-unresolved-get-workspace
	   j2s-unresolved-call-workspace

	   (loc->point::long ::obj)
	   (loc->src::bstring ::obj)
	   
	   (epairify loc expr)
	   (epairify-deep loc expr)
	   (strict-mode? mode)

	   (comp-return ::obj)
	   (acc-return ::obj)
	   (in-eval?::bool ::obj)
	   
	   (j2s-fast-constructor-id id)
	   (j2s-scheme-id id pref)
	   (j2s-decl-name ::J2SDecl ::struct)
	   (j2s-decl-fast-id ::J2SDecl conf)
	   (j2s-decl-scm-id ::J2SDecl conf)
	   (j2s-class-id clazz::J2SClass ctx)
	   (j2s-escape-id ::symbol ::obj)
	   (js-need-global? ::J2SDecl scope mode)
	   (flatten-stmt stmt)
	   (flatten-nodes nodes)
	   (flatten-begin nodes)
	   (remove-undefined lst)
	   (j2s-this-cache? ::J2SDecl)
	   (j2s-minlen val)
	   
	   (typeof-this obj conf)
	   (maybe-number? expr::J2SNode)
	   (mightbe-number?::bool field::J2SExpr)
	   (cannot-integer?::bool field::J2SExpr)
	   
	   (mightbe-array?::bool ::J2SExpr)
	   (mightbe-string?::bool ::J2SExpr)
	   
	   (utype-ident ident utype ::pair-nil #!optional compound)
	   (vtype-ident ident vtype ::pair-nil #!optional compound)
	   (type-ident ident type ::pair-nil)
	   (j2s-number val conf)
	   (j2s-error proc msg obj #!optional str)
	   (is-fixnum? expr::J2SExpr ::struct)
	   (is-number? expr::J2SExpr)
	   (is-integer? expr::J2SExpr)
	   (is-int30? expr::J2SExpr)
	   (is-int53? expr::J2SExpr)
	   (is-fx? expr::J2SExpr)
	   (is-uint32? expr::J2SExpr)
	   (is-uint53? expr::J2SExpr)
	   (is-string? expr::J2SExpr)
	   (is-buffer-cast? ::J2SExpr)

	   (j2s-jsstring val loc ::struct)
	   
	   (j2s-unresolved name throw cache loc ::struct)
	   (js-not expr)
	   
	   (js-pcache cache)
	   
	   (j2s-get loc obj field tyobj prop typrop tyval conf cache
	      #!key optim (cspecs '(cmap pmap amap vtable)))
	   (j2s-put! loc obj field tyobj prop typrop val tyval mode conf
	      cache 
	      #!key optim (cspecs '(cmap pmap nmap amap vtable)) (cachefun #t))
	   
	   (j2s-new loc clazz args)
	   
	   (inrange-positive?::bool ::J2SExpr)
	   (inrange-positive-number?::bool ::J2SExpr)
	   (inrange-one?::bool ::J2SExpr)
	   (inrange-32?::bool ::J2SExpr)
	   (inrange-int30?::bool ::J2SExpr)
	   (inrange-uint30?::bool ::J2SExpr)
	   (inrange-int32?::bool ::J2SExpr)
	   (inrange-uint32?::bool ::J2SExpr)
	   (inrange-uint32-number?::bool ::J2SExpr)
	   (inrange-int53?::bool ::J2SExpr)

	   (boxed-type?::bool ::obj)
	   (box ::obj ::obj ::struct #!optional proc::obj)
	   (box32 ::obj ::obj ::struct  #!optional proc::obj)
	   (box64 ::obj ::obj ::struct #!optional proc::obj)

	   (expr-asuint32 expr::J2SExpr)
	   (uncast::J2SExpr ::J2SExpr)

	   (cancall?::bool ::J2SNode ::bool)
	   (optimized-ctor ::J2SNode ctx)

	   (with-tmp-flip flip lhs rhs mode return ::struct gen::procedure)
	   (with-tmp lhs rhs mode return ::struct gen::procedure)
	   (with-tmp-args ::pair-nil mode return ctx gen::procedure)

	   (importpath-var ::J2SImportPath)
	   (importpath-evar ::J2SImportPath)
	   (importpath-rvar ::J2SImportPath ::pair)))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved-workspaces ...                                    */
;*---------------------------------------------------------------------*/
(define j2s-unresolved-put-workspace '%this)
(define j2s-unresolved-del-workspace '%this)
(define j2s-unresolved-get-workspace '%scope)
(define j2s-unresolved-call-workspace '%this)

;*---------------------------------------------------------------------*/
;*    loc->point ...                                                   */
;*---------------------------------------------------------------------*/
(define (loc->point loc)
   (match-case loc
      ((at ?- ?point) point)
      (else -1)))

;*---------------------------------------------------------------------*/
;*    loc->src ...                                                     */
;*---------------------------------------------------------------------*/
(define (loc->src loc)
   (match-case loc
      ((at ?src ?-) src)
      (else "")))

;*---------------------------------------------------------------------*/
;*    epairify ...                                                     */
;*---------------------------------------------------------------------*/
(define (epairify loc expr)
   (if (pair? expr)
       (econs (car expr) (cdr expr) loc)
       expr))

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
;*    strict-mode? ...                                                 */
;*---------------------------------------------------------------------*/
(define (strict-mode? mode)
   (or (eq? mode 'strict) (eq? mode 'hopscript)))

;*---------------------------------------------------------------------*/
;*    comp-return ...                                                  */
;*---------------------------------------------------------------------*/
(define (comp-return x)
   (match-case x
      ((begin)
       '(js-undefined))
      ((begin . ?rest)
       (let ((inv (reverse rest)))
	  `(begin ,@(reverse (filter pair? (cdr inv))) ,(car inv))))
      (else
       x)))

;*---------------------------------------------------------------------*/
;*    acc-return ...                                                   */
;*---------------------------------------------------------------------*/
(define (acc-return expr)
   `(set! %acc ,expr))

;*---------------------------------------------------------------------*/
;*    in-eval? ...                                                     */
;*---------------------------------------------------------------------*/
(define (in-eval? r)
   (not (eq? r comp-return)))

;*---------------------------------------------------------------------*/
;*    j2s-profile-id ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-profile-id id loc ctx)
   (if (context-get ctx :profile-ident #f)
       (string->symbol (format "~a<@~a:~a@>" id (cadr loc) (caddr loc)))
       id))

;*---------------------------------------------------------------------*/
;*    j2s-fast-constructor-id ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-fast-constructor-id id)
   (symbol-append '@new- id))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-id ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-id id pref)
   (cond
      ((char=? (string-ref (symbol->string! id) 0) #\%) id)
      ((memq id '(GLOBAL global arguments)) id)
      (else (symbol-append pref id))))

;*---------------------------------------------------------------------*/
;*    j2s-decl-name ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-decl-name decl ctx)
   (with-access::J2SDecl decl (id)
      (& id (context-program ctx))))
   
;*---------------------------------------------------------------------*/
;*    j2s-decl-scm-id ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-decl-scm-id decl::J2SDecl ctx)

   (define (scheme-id decl::J2SDecl)
      (with-access::J2SDecl decl (_scmid id scope key)
	 (if _scmid
	     _scmid
	     (let ((sid (j2s-scheme-id id
			   (if (memq scope '(%scope tls))
			       '!
			       (string->symbol
				  (string-append "^"
				     (integer->string key) "-"))))))
		(set! _scmid sid)
		sid))))
   
   (let ((id (scheme-id decl)))
      (if (isa? decl J2SDeclFun)
	  (with-access::J2SDecl decl (loc)
	     (j2s-profile-id id loc ctx))
	  id)))

;*---------------------------------------------------------------------*/
;*    j2s-decl-fast-id ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-decl-fast-id decl::J2SDecl ctx)

   (define (j2s-fast-id decl::J2SDecl)
      (with-access::J2SDecl decl (id)
	 (if (eq? id '||)
	     '@_
	     (symbol-append '@ id))))
   
   (let ((id (j2s-fast-id decl)))
      (if (isa? decl J2SDeclFun)
	  (with-access::J2SDecl decl (loc)
	     (j2s-profile-id id loc ctx))
	  id)))

;*---------------------------------------------------------------------*/
;*    j2s-class-id ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-class-id this::J2SClass ctx)
   (with-access::J2SClass this (decl name)
      (if decl
	  (with-access::J2SDecl decl (_scmid)
	     (unless _scmid
		(set! _scmid (symbol-append '@ name)))
	     _scmid)
	  (symbol-append '@ name))))

;*---------------------------------------------------------------------*/
;*    j2s-escape-id ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-escape-id escape id)
   (if (symbol? id)
       (symbol-append escape '- id)
       escape))

;*---------------------------------------------------------------------*/
;*    js-need-global? ...                                              */
;*---------------------------------------------------------------------*/
(define (js-need-global? decl::J2SDecl scope mode)
   (with-access::J2SDecl decl (usage)
      (or (not (j2s-let-opt? decl))
	  (not (eq? scope 'record))
	  (decl-usage-has? decl '(eval))
	  (not (and (memq scope '(%scope tls)) (eq? mode 'hopscript))))))

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
   (with-access::J2SDecl this (usecnt)
      (and (>=fx usecnt 3)
	   (not (decl-usage-has? this '(ref call new instanceof))))))

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
      (if (not (memq ty '(any unknown)))
	  ty
	  (let ((tyv (j2s-vtype obj)))
	     (if (eq? tyv 'object)
		 (if (and (isa? obj J2SThis)
			  (with-access::J2SThis obj (decl)
			     (with-access::J2SDecl decl (vtype)
				(eq? vtype 'this))))
		     'this
		     tyv)
		 tyv)))))

;*---------------------------------------------------------------------*/
;*    maybe-number? ...                                                */
;*---------------------------------------------------------------------*/
(define (maybe-number? expr::J2SNode)
   (memq (j2s-vtype expr) '(any int32 uint32 int53 integer number real)))

;*---------------------------------------------------------------------*/
;*    mightbe-number? ...                                              */
;*---------------------------------------------------------------------*/
(define (mightbe-number?::bool field::J2SExpr)
   (or (is-number? field)
       (with-access::J2SExpr field (hint)
	  (any (lambda (h)
		  (let ((c (assq h hint)))
		     (when (pair? c)
			(>fx (cdr c) 0))))
	     '(index number integer)))
       (when (isa? field J2SBinary)
	  (with-access::J2SBinary field (lhs rhs op)
	     (when (eq? op '+)
		(or (mightbe-number? lhs) (mightbe-number? rhs)))))
       (when (eq? (j2s-type field) 'any)
	  (or (isa? field J2SPostfix) (isa? field J2SPrefix)))))

;*---------------------------------------------------------------------*/
;*    cannot-integer? ...                                              */
;*---------------------------------------------------------------------*/
(define (cannot-integer?::bool expr::J2SExpr)
   (let ((ty (j2s-vtype expr)))
      (cond
	 ((memq ty '(int32 uint32 int53 integer number))
	  #f)
	 ((not (eq? ty 'any))
	  #t)
	 (else
	  (with-access::J2SExpr expr (hint)
	     (let ((c (assq 'integer hint)))
		(and (pair? c) (<fx (cdr c) 0))))))))

;*---------------------------------------------------------------------*/
;*    mightbe-array? ...                                               */
;*---------------------------------------------------------------------*/
(define (mightbe-array?::bool obj::J2SExpr)
   (with-access::J2SExpr obj (hint type)
      (or (eq? type 'array)
	  (let ((c (assq 'array hint)))
	     (when (pair? c)
		(>fx (cdr c) 0))))))

;*---------------------------------------------------------------------*/
;*    mightbe-string? ...                                              */
;*---------------------------------------------------------------------*/
(define (mightbe-string?::bool obj::J2SExpr)
   (with-access::J2SExpr obj (hint type)
      (or (eq? type 'string)
	  (let ((c (assq 'string hint)))
	     (when (pair? c)
		(>fx (cdr c) 0))))))

;*---------------------------------------------------------------------*/
;*    utype-ident ...                                                  */
;*---------------------------------------------------------------------*/
(define (utype-ident ident utype conf #!optional compound)
   (cond
      ((or (eq? utype 'any) (eq? utype 'unknown))
       ident)
      (compound
       (symbol-append ident '|::| (type-name utype conf)))
      (else
       ident)))

;*---------------------------------------------------------------------*/
;*    vtype-ident ...                                                  */
;*---------------------------------------------------------------------*/
(define (vtype-ident ident utype conf #!optional compound)
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
(define (type-ident ident type conf)
   (cond
      ((memq type '(int32 uint32))
       (symbol-append ident '|::| (type-name type conf)))
      ((memq type '(bint))
       (symbol-append ident '|::bint|))
      ((eq? type 'any)
       (symbol-append ident '|::obj|))
      ((eq? type 'arguments)
       ;; The new optimization introduced in 13May2021 that optimizes
       ;; arguments aliasing makes it difficult to ensute that a JS
       ;; local variables of type "arguments" is a Scheme argument because
       ;; they might be symbols. It is then simpler to consider them as
       ;; untyped.
       ident)
      ((type-name type conf)
       =>
       (lambda (tname) (symbol-append ident '|::| tname)))
      (else
       ident)))

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
	  ((>=fx (config-get conf :int-size 0) 32)
	   (cond-expand
	      (bint30 `(elong->fixnum ,val))
	      (else (elong->fixnum val))))
	  (else
	   (elong->flonum val))))
      ((llong? val)
       (cond
	  ((and (>=llong val (-llong #l0 (bit-lshllong #l1 29)))
		(<=llong val (bit-lshllong #l1 29)))
	   (llong->fixnum val))
	  ((and (>=fx (config-get conf :int-size 0) 53)
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
	      ((and (>=fx (config-get conf :int-size 0) 53)
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
	      (config-get conf :int-size (bigloo-config 'int-size))))))
      (else val)))

;*---------------------------------------------------------------------*/
;*    j2s-error ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-error proc msg obj #!optional str)
   (with-access::J2SNode obj (loc)
      (match-case loc
	 ((at ?fname ?loc)
	  (error/location proc msg (or str (j2s->sexp obj)) fname loc))
	 (else
	  (error proc msg obj)))))

;*---------------------------------------------------------------------*/
;*    is-number? ...                                                   */
;*---------------------------------------------------------------------*/
(define (is-number? expr::J2SExpr)
   (or (isa? expr J2SNumber)
       (type-number? (j2s-type expr))))

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
(define (is-fixnum? expr::J2SExpr ctx)
   (if (m64? (context-conf ctx))
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
;*    is-buffer-cast? ...                                              */
;*---------------------------------------------------------------------*/
(define (is-buffer-cast? this)
   (and (isa? this J2SCast)
	(with-access::J2SCast this (expr)
	   (eq? (j2s-type expr) 'buffer))))

;*---------------------------------------------------------------------*/
;*    j2s-jsstring ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring val loc ctx)
   (epairify loc (& val (context-program ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-unresolved name throw cache loc ctx)
   (cond
      ((eq? name 'undefined)
       `(js-undefined))
      (cache
       `(js-global-object-get-name/cache ,j2s-unresolved-get-workspace
	   ,(& name (context-program ctx))
	   ,(if (pair? throw) `',throw throw)
	   %this
	   ,(js-pcache cache) ,(loc->point loc)))
      ((memq (context-get ctx :site) '(client tilde))
       name)
      (else
       `(js-global-object-get-name ,j2s-unresolved-get-workspace
	   ,(& name (context-program ctx))
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
;*    js-pcache ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache cache)
   `(js-pcache-ref %pcache ,cache))


;*---------------------------------------------------------------------*/
;*    record-private-field? ...                                        */
;*---------------------------------------------------------------------*/
(define (record-private-field? field)
   (with-access::J2SString field (private)
      (if (isa? private J2SClassElement)
	  (with-access::J2SClassElement private (clazz)
	     (isa? clazz J2SRecord)))))      

;*---------------------------------------------------------------------*/
;*    j2s-get ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-get loc obj field tyobj prop typrop tyval ctx cache
	   #!key optim (cspecs '(cmap pmap amap vtable)))

   (define conf (context-conf ctx))
   
   (define (js-get obj prop %this)
      (if (or (config-get conf :profile-cache #f)
	      (> (config-get conf :debug 0) 0))
	  `(js-get/debug ,obj ,prop %this ',loc)
	  `(js-get ,obj ,prop %this)))
   
   (define (maybe-string? prop typrop)
      (and (not (number? prop))
	   (not (type-number? typrop))
	   (not (memq typrop '(array object)))))

   (define (maybe-number? prop typrop)
      (or (number? prop)
	  (type-number? typrop)
	  (not (memq typrop '(string object)))))

   (define (js-get-proxy obj prop typrop propstr)
      (if (string? propstr)
	  `(js-get-proxy ,obj ,prop %this)
	  `(js-get-proxy ,obj ,(box prop typrop ctx) %this)))

   (define (js-array-get obj prop propstr typrop)
      (case typrop
	 ((uint32)
	  `(js-array-index-ref ,obj ,prop %this))
	 ((int32)
	  `(js-array-fixnum-ref ,obj (int32->fixnum ,prop) %this))
	 ((fixnum int53)
	  `(js-array-fixnum-ref ,obj ,prop %this))
	 (else
	  (cond
	     ((and (string? propstr) (string=? propstr "length"))
	      (if (eq? tyval 'uint32)
		  `(js-array-length ,obj)
		  (box `(js-array-length ,obj) 'uint32 ctx)))
	     ((mightbe-number? field)
	      `(js-array-ref ,obj ,prop %this))
	     (else
	      `(js-array-noindex-ref ,obj ,prop %this))))))

   (define (record-private-access obj field)
      (with-access::J2SString field (private val)
	 (if (isa? private J2SClassElement)
	     (with-access::J2SClassElement private (clazz)
		(let loop ((obj obj))
		   (if (symbol? obj)
		       `(if (,(class-predicate-id clazz) ,obj)
			    (js-object-inline-ref ,obj
			       ,(j2s-class-instance-get-property-index
				   clazz val))
			    (js-undefined))
		       (let ((tmp (gensym 'o)))
			  `(let ((,tmp ,obj))
			      ,(loop tmp)))))))))
   
   (let ((propstr (match-case prop
		     ((& ?str . ?-) str)
		     (else #f)))
	 (obj (box obj tyobj ctx)))
      (cond
	 ((> (config-get conf :debug 0) 0)
	  (if (string? propstr)
	      `(js-get/debug ,obj ,prop %this ',loc)
	      `(js-get/debug ,obj ,(box prop typrop ctx) %this ',loc)))
	 ((equal? propstr "__proto__")
	  `(js-getprototypeof ,obj %this
	      ,(format "~a:~a" (cadr loc) (caddr loc))))
	 ((eq? tyobj 'array)
	  (js-array-get obj prop propstr typrop))
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
	     ((and (string? prop) (string=? prop "length") (eq? tyval 'uint32))
	      `(js-jsstring-length ,obj))
	     (else
	      `(js-get-string ,obj ,prop %this))))
	 ((eq? tyobj 'proxy)
	  (js-get-proxy obj prop typrop propstr))
	 ((and cache cspecs)
	  (cond
	     ((string? propstr)
 	      (if (string=? propstr "length")
		  (case optim
		     ((string)
		      (if (eq? tyval 'uint32)
			  `(js-get-lengthu32-maybe-string ,obj %this ,(js-pcache cache))
			  `(js-get-length-maybe-string ,obj %this ,(js-pcache cache))))
		     ((arguments)
		      (if (eq? tyval 'uint32)
			  `(js-get-lengthu32-maybe-arguments ,obj %this ,(js-pcache cache))
			  `(js-get-length-maybe-arguments ,obj %this ,(js-pcache cache))))
		     (else
		      (if (eq? tyval 'uint32)
			  `(js-get-lengthu32 ,obj %this ,(js-pcache cache))
			  `(js-get-length ,obj %this ,(js-pcache cache)))))
		  (cond
		     ((eq? tyobj 'global)
		      `(js-global-object-get-name/cache ,obj ,prop #f %this
			  ,(js-pcache cache) ,(loc->point loc) ',cspecs))
		     ((type-object? tyobj)
		      `(js-get-jsobject-name/cache ,obj ,prop #f %this
			  ,(js-pcache cache) ,(loc->point loc) ',cspecs))
		     ((record-private-field? field)
		      (record-private-access obj field))
		     (else
		      `(js-get-name/cache ,obj ,prop #f %this
			  ,(js-pcache cache) ,(loc->point loc) ',cspecs)))))
	     ((memq typrop '(int32 uint32))
	      (js-get obj (box prop typrop ctx) '%this))
	     ((and (maybe-string? prop typrop)
		   (or (not (maybe-number? prop typrop))
		       (not (is-hint? field 'integer))))
	      (if (symbol? obj)
		  `(,(if (eq? tyobj 'object)
			 'js-get-jsobject/name-cache
			 'js-get/name-cache)
		    ,obj ,prop %this)
		  (let ((tmp (gensym '%tmp)))
		     `(let ((,tmp ,obj))
			 (,(if (eq? tyobj 'object)
			       'js-get-jsobject/name-cache
			       'js-get/name-cache)
			  ,tmp ,prop %this)))))
	     ((and (maybe-number? prop typrop)
		   (or (not (maybe-string? prop typrop))
		       (not (is-hint? field 'string))))
	      (let ((o (gensym '%obj))
		    (p (gensym '%prop)))
		 `(let ((,o ,obj)
			(,p ,(box prop typrop ctx)))
		     (if (js-array? ,o)
			 (js-array-ref ,o ,p %this)
			 ,(js-get obj p '%this)))))
	     (else
	      (js-get obj prop '%this))))
	 ((string? propstr)
	  (if (string=? propstr "length")
	      (if (eq? tyval 'uint32)
		  `(js-get-lengthu32 ,obj %this #f)
		  `(js-get-length ,obj %this #f))
	      `(js-get ,obj ,prop %this)))
	 ((and field (eq? optim 'array) (mightbe-number? field))
	  (let ((o (gensym '%obj))
		(p (gensym '%prop)))
	     `(let ((,o ,obj)
		    (,p ,prop))
		 (if (js-array? ,o)
		     ,(js-array-get o p propstr typrop)
		     ,(js-get o p '%this)))))
	 ((maybe-number? prop typrop)
	  (let ((o (gensym '%obj))
		(p (gensym '%prop)))
	     `(let ((,o ,obj)
		    (,p ,(box prop typrop ctx)))
		 (if (js-array? ,o)
		     (js-array-ref ,o ,p %this)
		     ,(js-get obj p '%this)))))
	 ((memq typrop '(int32 uint32))
	  (js-get obj (box prop typrop ctx) '%this))
	 (else
	  (js-get obj prop '%this)))))

;*---------------------------------------------------------------------*/
;*    j2s-put! ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-put! loc obj field tyobj prop typrop val tyval mode ctx cache
	   #!key optim (cspecs '(cmap pmap nmap amap vtable)) (cachefun #t))

   (define conf (context-conf ctx))
   
   (define (js-put! o p v mode %this)
      (if (or (config-get conf :profile-cache #f)
	      (> (config-get conf :debug 0) 0))
	  `(js-put/debug! ,o ,p ,v ,mode %this ',loc)
	  `(js-put! ,o ,p ,v ,mode %this)))
   
   (define (maybe-array-set! prop val)
      (let ((o (gensym '%obj))
	    (p (gensym '%prop))
	    (v (gensym '%val)))
	 `(let ((,o ,obj)
		(,p ,prop)
		(,v ,val))
	     (if (js-array? ,o)
		 (js-array-set! ,o ,p ,v ,mode %this)
		 ,(js-put! o p v mode '%this)))))

   (define (maybe-string? prop typrop)
      (and (not (number? prop))
	   (not (type-number? typrop))
	   (not (memq typrop '(array object)))))
   
   (define (maybe-number? prop typrop)
      (or (number? prop)
	  (type-number? typrop)
	  (not (memq typrop '(string object)))))

   (define (record-private-assig obj field val loc)
      (with-access::J2SString field (private (str val))
	 (if (isa? private J2SClassElement)
	     (with-access::J2SClassElement private (clazz)
		(let loop ((obj obj))
		   (if (symbol? obj)
		       `(if (,(class-predicate-id clazz) ,obj)
			    (js-object-inline-set! ,obj
			       ,(j2s-class-instance-get-property-index
				   clazz str)
			       ,val)
			    (js-raise-type-error/loc %this ',loc
			       ,(format "[[PUT]], sealed object ~a" str)
			       ,obj))
		       (let ((tmp (gensym 'o)))
			  `(let ((,tmp ,obj))
			      ,(loop tmp)))))))))

   (if (boxed-type? tyval)
       (if (number? val)
	   `(begin
	       ,(j2s-put! loc obj field tyobj prop typrop
		   (box val tyval ctx) 'any mode ctx
		   cache :optim optim :cspecs cspecs :cachefun cachefun)
	       ,val)
	   (let ((tmp (gensym)))
	      `(let ((,tmp ,val))
		  ,(j2s-put! loc obj field tyobj prop typrop
		      (box tmp tyval ctx) 'any mode ctx
		      cache :optim optim :cspecs cspecs :cachefun cachefun)
		  ,tmp)))
       (let ((propstr (match-case prop
			 ((& ?str . ?-) str)
			 (else #f))))
	  (cond
	     ((> (config-get conf :debug 0) 0)
	      (if (string? propstr)
		  `(js-put/debug! ,obj ,prop
		      ,val ,mode %this ',loc)
		  `(js-put/debug! ,obj ,(box prop typrop ctx)
		      ,val ,mode %this ',loc)))
	     ((equal? propstr "__proto__")
	      `(js-setprototypeof ,obj ,val %this "js2scheme"))
	     ((and (isa? tyobj J2SRecord)
		   propstr
		   (j2s-class-instance-get-property-index tyobj propstr))
	      =>
	      (lambda (idx)
		 `(js-object-inline-set! ,obj ,idx ,val)))
	     ((eq? tyobj 'array)
	      (case typrop
		 ((uint32)
		  `(js-array-index-set! ,obj ,prop
		      ,val ,(strict-mode? mode) %this))
		 ((int32)
		  `(js-array-fixnum-set! ,obj (int32->fixnum ,prop)
		      ,val ,(strict-mode? mode) %this))
		 ((string)
		  (if (eq? tyval 'length)
		      (with-access::J2SString field ((name val) loc)
			 (if (string=? name "length")
			     `(js-array-length-set! ,obj ,val ,mode %this)
			     (error/location "hopc"
				"Illegal assignment type \"length\""
				val
				(cadr loc) (caddr loc))))
		      `(js-array-string-set! ,obj ,prop
			  ,val ,(strict-mode? mode) %this)))
		 (else
		  (if (or (not field) (mightbe-number? field))
		      `(js-array-set! ,obj ,prop ,val
			  ,(strict-mode? mode) %this)
		      `(js-array-noindex-set! ,obj ,prop ,val
			  ,(strict-mode? mode) %this)))))
	     ((eq? tyobj 'jsvector)
	      (case typrop
		 ((uint32)
		  `(js-vector-index-set! ,obj ,prop ,val %this))
		 ((int32)
		  `(js-vector-index-set! ,obj (int32->uint32 ,prop) ,val %this))
		 ((int53)
		  `(js-vector-index-set! ,obj (fixnum->uint32 ,prop) ,val %this))
		 (else
		  `(js-vector-put! ,obj ,prop ,val %this))))
	     ((eq? tyobj 'arguments)
	      `(js-put! ,obj ,prop ,val ,mode %this))
	     ((and cache cspecs)
	      (cond
		 ((string? propstr)
		  (cond
		     ((string=? propstr "length")
		      `(js-put-length! ,obj ,val
			  ,mode ,(js-pcache cache) %this))
		     ((type-object? tyobj)
		      `(js-put-jsobject-name/cache! ,obj ,prop
			  ,val
			  ,mode %this
			  ,(js-pcache cache)
			  ,(loc->point loc) ',cspecs
			  ,cachefun))
		     ((record-private-field? field)
		      (record-private-assig obj field val loc))
		     (else
		      `(js-put-name/cache! ,obj ,prop
			  ,val
			  ,mode %this
			  ,(js-pcache cache) ,(loc->point loc)
			  ',cspecs
			  ,cachefun))))
		 ((memq typrop '(int32 uint32 int53 integer))
		  `(maybe-array-set! ,obj ,(box prop typrop ctx)
		      ,val ,mode %this))
		 ((or (number? prop) (null? cspecs))
		  (maybe-array-set! prop val))
		 ((and (memq tyobj '(object global this)) (eq? typrop 'string))
		  `(js-put-jsobject/name-cache! ,obj ,prop
		      ,val
		      ,mode %this
		      ,(loc->point loc) ',cspecs))
		 ((maybe-string? prop typrop)
		  (if (memq tyobj '(object global this))
		      `(js-put-jsobject/cache! ,obj ,prop
			  ,val ,mode %this
			  ,(loc->point loc) ,(loc->src loc))
		      `(js-put/cache! ,obj ,prop
			  ,val ,mode %this
			  ,(loc->point loc) ,(loc->src loc))))
		 (else
		  `(js-put! ,obj ,prop ,val ,mode %this))))
	     ((and field
		   (or (and (eq? optim 'array) (mightbe-number? field))
		       (and (maybe-number? field typrop)
			    (is-hint? field 'integer))))
	      (maybe-array-set! (box prop typrop ctx) val))
	     (else
	      (cond
		 ((string? propstr)
		  (js-put! obj prop val mode '%this))
		 ((and (eq? optim 'array) (memq typrop '(int32 uint32)))
		  (maybe-array-set! (box prop typrop ctx) val))
		 (else
		  (js-put! obj (box prop typrop ctx) val mode '%this))))))))

;*---------------------------------------------------------------------*/
;*    j2s-new ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-new loc clazz args)
   (if (> (bigloo-debug) 0)
       `(js-new/debug %this ',loc ,clazz ,@args)
       (let ((new (case (length args)
		     ((0) 'js-new0)
		     ((1) 'js-new1)
		     ((2) 'js-new2)
		     ((3) 'js-new3)
		     ((4) 'js-new4)
		     ((5) 'js-new5)
		     ((6) 'js-new6)
		     ((7) 'js-new7)
		     ((8) 'js-new8)
		     (else 'js-new))))
	  `(,new %this ,clazz ,@args))))

;*---------------------------------------------------------------------*/
;*    ranges                                                           */
;*---------------------------------------------------------------------*/
(define-macro (max-uint32)
   '(-u32 (bit-lshu32 #u32:1 31) #u32:1))
(define-macro (max-int32)
   '(int32->fixnum
     (uint32->int32 (-u32 (bit-lshu32 #u32:1 31) #u32:1))))
(define-macro (min-int32)
   '(int32->fixnum
     (-s32 (negs32 (uint32->int32 (-u32 (bit-lshu32 #u32:1 31) #u32:1))) #s32:1)))

;*---------------------------------------------------------------------*/
;*    inrange-positive? ...                                            */
;*---------------------------------------------------------------------*/
(define (inrange-positive? expr)
   (if (isa? expr J2SNumber)
       (with-access::J2SNumber expr (val)
	  (cond
	     ((uint32? val) #t)
	     ((int32? val) (>=s32 val #s32:0))
	     ((fixnum? val) (>fx val 0))
	     (else #f)))
       (with-access::J2SExpr expr (range type)
	  (if (interval? range)
	      (and (>=llong (interval-min range) #l0)
		   (eq? (interval-type range) 'integer))
	      (eq? type 'uint32)))))

;*---------------------------------------------------------------------*/
;*    inrange-positive-number? ...                                     */
;*---------------------------------------------------------------------*/
(define (inrange-positive-number? expr)
   (when (memq (j2s-type expr) '(integer number real int32 uint32))
      (with-access::J2SExpr expr (range)
	 (when (interval? range)
	    (>=llong (interval-min range) #l0)))))

;*---------------------------------------------------------------------*/
;*    inrange-one? ...                                                 */
;*---------------------------------------------------------------------*/
(define (inrange-one? expr)
   (if (isa? expr J2SNumber)
       (with-access::J2SNumber expr (val)
	  (cond
	     ((uint32? val) (<=u32 val #u32:1))
	     ((int32? val) (and (>=s32 val #s32:-1) (>=s32 val #s32:1)))
	     ((fixnum? val) (and (>=fx val -1) (<=fx val 1)))
	     (else #f)))
       (with-access::J2SExpr expr (range)
	  (when (interval? range)
	     (and (>=llong (interval-min range) #l-1)
		  (<=llong (interval-max range) #l1)
		  (eq? (interval-type range) 'integer))))))

;*---------------------------------------------------------------------*/
;*    inrange-32? ...                                                  */
;*---------------------------------------------------------------------*/
(define (inrange-32? expr)
   (with-access::J2SExpr expr (range)
      (and (interval? range)
	   (>= (interval-min range) #l0)
	   (< (interval-max range) #l32)
	   (eq? (interval-type range) 'integer))))

;*---------------------------------------------------------------------*/
;*    inrange-int30? ...                                               */
;*---------------------------------------------------------------------*/
(define (inrange-int30? expr)
   (if (isa? expr J2SNumber)
       (with-access::J2SNumber expr (val)
	  (cond
	     ((uint32? val)
	      (<u32 val (bit-lshu32 #u32:1 29)))
	     ((int32? val)
	      (and (>=s32 val (negs32 (bit-lshs32 #u32:1 29)))
		   (<s32 val (bit-lshs32 #s32:1 29))))
	     ((fixnum? val)
	      (and (>=fx val (negfx (bit-lsh 1 29)))
		   (<fx val (bit-lsh 1 29))))
	     (else #f)))
       (with-access::J2SExpr expr (range)
	  (when (interval? range)
	     (and (>=llong (interval-min range) (- (bit-lshllong #l1 29)))
		  (<llong (interval-max range) (bit-lshllong #l1 29))
		  (eq? (interval-type range) 'integer))))))

;*---------------------------------------------------------------------*/
;*    inrange-uint30? ...                                              */
;*---------------------------------------------------------------------*/
(define (inrange-uint30? expr)
   (if (isa? expr J2SNumber)
       (with-access::J2SNumber expr (val)
	  (cond
	     ((uint32? val)
	      (<u32 val (bit-lshu32 #u32:1 29)))
	     ((int32? val)
	      (and (>=s32 val 0) (<s32 val (bit-lshs32 #s32:1 29))))
	     ((fixnum? val)
	      (and (>=fx val 0) (<fx val (bit-lsh 1 29))))
	     (else #f)))
       (with-access::J2SExpr expr (range)
	  (when (interval? range)
	     (and (>=llong (interval-min range) 0)
		  (<llong (interval-max range) (bit-lshllong #l1 30))
		  (eq? (interval-type range) 'integer))))))

;*---------------------------------------------------------------------*/
;*    inrange-int32? ...                                               */
;*---------------------------------------------------------------------*/
(define (inrange-int32? expr)
   (if (isa? expr J2SNumber)
       (with-access::J2SNumber expr (val)
	  (cond
	     ((uint32? val)
	      (<u32 val (-u32 (bit-lshu32 #u32:1 31) #u32:1)))
	     ((int32? val)
	      #t)
	     ((fixnum? val)
	      (and (>=fx val (min-int32)) (<=fx val (max-int32))))
	     (else #f)))
       (with-access::J2SExpr expr (range type)
	  (if (interval? range)
	      (and (>=llong (interval-min range) (- (bit-lshllong #l1 31)))
		   (<llong (interval-max range) (bit-lshllong #l1 31))
		   (eq? (interval-type range) 'integer))
	      (eq? type 'int32)))))

;*---------------------------------------------------------------------*/
;*    inrange-uint32? ...                                              */
;*---------------------------------------------------------------------*/
(define (inrange-uint32? expr)
   (if (isa? expr J2SNumber)
       (with-access::J2SNumber expr (val)
	  (cond
	     ((uint32? val) #t)
	     ((int32? val) (>=s32 val #s32:0))
	     ((fixnum? val) (>=fx val 0))
	     (else #f)))
       (with-access::J2SExpr expr (range type)
	  (if (interval? range)
	      (and (>=llong (interval-min range) #l0)
		   (<llong (interval-max range) (bit-lshllong #l1 32))
		   (eq? (interval-type range) 'integer))
	      (eq? type 'uint32)))))

;*---------------------------------------------------------------------*/
;*    inrange-uint32-number? ...                                       */
;*---------------------------------------------------------------------*/
(define (inrange-uint32-number? expr)
   (with-access::J2SExpr expr (range)
      (when (inrange-positive-number? expr)
	 (<llong (interval-max range) (bit-lshllong #l1 32)))))

;*---------------------------------------------------------------------*/
;*    inrange-int53? ...                                               */
;*---------------------------------------------------------------------*/
(define (inrange-int53? expr)
   (if (isa? expr J2SNumber)
       (with-access::J2SNumber expr (val)
	  (cond
	     ((uint32? val) #t)
	     ((int32? val) #t)
	     ((fixnum? val)
	      (or (and (>=fx val (negfx (bit-lsh 1 29)))
		       (<fx val (bit-lsh 1 29)))
		  (let ((lval (fixnum->llong val)))
		     (and (>=llong lval (negllong (bit-lshllong #l1 53)))
			  (<llong lval (bit-lshllong #l1 53))))))
	     (else #f)))
       (with-access::J2SExpr expr (range type)
	  (if (interval? range)
	      (and (>=llong (interval-min range) (- (bit-lshllong #l1 53)))
		   (<llong (interval-max range) (bit-lshllong #l1 53))
		   (eq? (interval-type range) 'integer))
	      (memq type '(int32 uint32 integer bint))))))

;*---------------------------------------------------------------------*/
;*    boxed-type? ...                                                  */
;*---------------------------------------------------------------------*/
(define (boxed-type? type)
   (memq type '(uint32 int32)))

;*---------------------------------------------------------------------*/
;*    box ...                                                          */
;*---------------------------------------------------------------------*/
(define (box val type ctx #!optional proc::obj)
   (let ((conf (context-conf ctx)))
      (if (m64? conf)
	  (box64 val type ctx proc)
	  (box32 val type ctx proc))))

;*---------------------------------------------------------------------*/
;*    box32 ...                                                        */
;*---------------------------------------------------------------------*/
(define (box32 val type ctx #!optional proc::obj)
   (let ((conf (context-conf ctx)))
      (case type
	 ((int32)
	  (if (int32? val)
	      (if (and (>=llong (int32->llong val) (conf-min-int conf))
		       (<=llong (int32->llong val) (conf-max-int conf)))
		  (cond-expand
		     (bint30
		      (if (and (>=llong (int32->llong val)
				  (negllong (bit-lshllong #l1 29)))
			       (<llong (int32->llong val)
				  (bit-lshllong #l1 29)))
			  (int32->fixnum val)
			  `(int32->fixnum ,val)))
		     (else
		      (int32->fixnum val))))
	      `(overflowfx (int32->fixnum ,val))))
	 ((uint32)
	  (if (uint32? val)
	      (if (<=llong (uint32->llong val) (conf-max-int conf))
		  (uint32->fixnum val)
		  (uint32->flonum val))
	      `(if (<u32 ,val ,(llong->uint32 (conf-max-int conf)))
		   (uint32->fixnum ,val)
		   (uint32->flonum ,val))))
	 ((int53)
	  (if (fixnum? val)
	      (if (and (>=llong (fixnum->llong val) (conf-min-int conf))
		       (<=llong (fixnum->llong val) (conf-max-int conf)))
		  val
		  (fixnum->flonum val))
	      `(overflowfx ,val)))
	 ((integer real number)
	  val)
	 (else
	  (if (not proc) val (proc val))))))

;*---------------------------------------------------------------------*/
;*    box64 ...                                                        */
;*---------------------------------------------------------------------*/
(define (box64 val type ctx #!optional proc::obj)
   (case type
      ((int32) (if (int32? val) (int32->fixnum val) `(int32->fixnum ,val)))
      ((uint32) (if (uint32? val) (uint32->fixnum val) `(uint32->fixnum ,val)))
      ((int53 integer real number) val)
      (else (if (not proc) val (proc val)))))

;*---------------------------------------------------------------------*/
;*    expr-asuint32 ...                                                */
;*    -------------------------------------------------------------    */
;*    If an expression can be interpreted as an uint32 without         */
;*    cast, return that subexpression.                                 */
;*---------------------------------------------------------------------*/
(define (expr-asuint32 expr::J2SExpr)
   (cond
      ((eq? (j2s-type expr) 'uint32)
       expr)
      ((isa? expr J2SCast)
       (with-access::J2SCast expr (expr)
	  (expr-asuint32 expr)))
      ((inrange-uint32? expr)
       expr)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    uncast ...                                                       */
;*---------------------------------------------------------------------*/
(define (uncast::J2SExpr expr::J2SExpr)
   (if (isa? expr J2SCast)
       (with-access::J2SCast expr (expr)
	  (uncast expr))
       expr))

;*---------------------------------------------------------------------*/
;*    cancall? ...                                                     */
;*---------------------------------------------------------------------*/
(define (cancall? node accessp)
   (let ((cell (make-cell #f)))
      (cancall node accessp cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    cancall ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (cancall this::J2SNode accessp cell)
   (or (cell-ref cell) (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    cancall ::J2SCall ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (cancall this::J2SCall accessp cell)
   (cell-set! cell #t))

;*---------------------------------------------------------------------*/
;*    cancall ::J2SNew ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (cancall this::J2SNew accessp cell)
   (cell-set! cell #t))

;*---------------------------------------------------------------------*/
;*    cancall ::J2SAssig ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (cancall this::J2SAssig accessp cell)
   (with-access::J2SAssig this (lhs rhs)
      (if (isa? lhs J2SAccess)
	  (with-access::J2SAccess lhs (obj field)
	     (unless (isa? obj J2SThis)
		(cancall field accessp cell)
		(cancall rhs accessp cell)))
	  (begin
	     (cancall lhs accessp cell)
	     (cancall rhs accessp cell)))))

;*---------------------------------------------------------------------*/
;*    cancall ::J2SAccess ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (cancall this::J2SAccess accessp cell)
   (if accessp
       (cell-set! cell #t)
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    optimized-ctor ...                                               */
;*---------------------------------------------------------------------*/
(define (optimized-ctor this::J2SNode ctx)
   (when (context-get ctx :optim-ctor #f)
      (let loop ((this this))
	 (cond
	    ((isa? this J2SRef)
	     (with-access::J2SRef this (decl)
		(loop decl)))
	    ((isa? this J2SParen)
	     (with-access::J2SParen this (expr)
		(loop expr)))
	    ((isa? this J2SDeclFun)
	     (with-access::J2SDeclFun this (scope val)
		(unless (memq scope '(none letblock))
		   (when (and (decl-usage-has? this '(new))
			      (not (decl-usage-has? this '(call assig))))
		      (with-access::J2SFun val (rtype vararg)
			 (when (and (eq? rtype 'undefined) (not vararg))
			    this))))))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    with-tmp-flip ...                                                */
;*---------------------------------------------------------------------*/
(define (with-tmp-flip flip lhs rhs mode return ctx gen::procedure)

   (define (ultrasimple? expr)
      (cond
	 ((isa? expr J2SLiteral)
	  #t)
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (ultrasimple? expr)))
	 ((isa? expr J2SCast)
	  (with-access::J2SCast expr (expr)
	     (ultrasimple? expr)))
	 (else
	  #f)))
   
   (define (simple? expr)
      (cond
	 ((isa? expr J2SRef)
	  #t)
	 ((isa? expr J2SGlobalRef)
	  #f)
	 ((isa? expr J2SLiteral)
	  #t)
	 ((isa? expr J2SBinary)
	  (with-access::J2SBinary expr (lhs rhs)
	     (and (ultrasimple? lhs) (ultrasimple? rhs))))
	 ((isa? expr J2SUnary)
	  (with-access::J2SUnary expr (expr)
	     (ultrasimple? expr)))
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (simple? expr)))
	 ((isa? expr J2SCast)
	  (with-access::J2SCast expr (expr type)
	     (and (simple? expr) (eq? type (j2s-type expr)))))
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
   
   (let* ((scmlhs (j2s-scheme lhs mode return ctx))
	  (scmrhs (j2s-scheme rhs mode return ctx))
	  (ultrasimplelhs (ultrasimple? lhs))
	  (ultrasimplerhs (ultrasimple? rhs))
	  (simplelhs (simple? lhs))
	  (simplerhs (simple? rhs))
	  (testl (or (atom? scmlhs) (and (symbol? scmlhs) simplerhs)))
	  (testr (or (atom? scmrhs) (and (symbol? scmrhs) simplelhs)))
	  (conf (context-conf ctx)))
      (cond
	 ((and ultrasimplelhs ultrasimplerhs)
	  (gen scmlhs scmrhs))
	 ((and ultrasimplelhs simplerhs)
	  (gen scmlhs scmrhs))
	 ((and simplelhs ultrasimplerhs)
	  (gen scmlhs scmrhs))
	 ((and testl testr)
	  (gen scmlhs scmrhs))
	 ((ultrasimple? lhs)
	  (let ((right (gensym 'rhs)))
	     `(let ((,(type-ident right (j2s-type rhs) conf) ,scmrhs))
		 ,(gen scmlhs right))))
	 ((ultrasimple? rhs)
	  (let ((left (gensym 'lhs)))
	     `(let ((,(type-ident left (j2s-type lhs) conf) ,scmlhs))
		 ,(gen left scmrhs))))
	 (testl
	  (let ((right (gensym 'rhs)))
	     `(let ((,(type-ident right (j2s-type rhs) conf) ,scmrhs))
		 ,(gen scmlhs right))))
	 (testr
	  (let ((left (gensym 'lhs)))
	     `(let ((,(type-ident left (j2s-type lhs) conf) ,scmlhs))
		 ,(gen left scmrhs))))
	 (else
	  (let ((left (gensym 'lhs))
		(right (gensym 'rhs)))
	     (if flip 
		 `(let* ((,(type-ident right (j2s-type rhs) conf) ,scmrhs)
			 (,(type-ident left (j2s-type lhs) conf) ,scmlhs))
		     ,(gen left right))
		 `(let* ((,(type-ident left (j2s-type lhs) conf) ,scmlhs)
			 (,(type-ident right (j2s-type rhs) conf) ,scmrhs))
		     ,(gen left right))))))))

;*---------------------------------------------------------------------*/
;*    with-tmp ...                                                     */
;*---------------------------------------------------------------------*/
(define (with-tmp lhs rhs mode return ctx gen::procedure)
   (with-tmp-flip #f lhs rhs mode return ctx gen))

;*---------------------------------------------------------------------*/
;*    with-tmp-args ...                                                */
;*---------------------------------------------------------------------*/
(define (with-tmp-args args mode return ctx proc)
   (if (any side-effect? args)
       (let ((tmps (map (lambda (a)
			   (when (can-side-effect? a) (gensym '%a)))
		      args)))
	  `(let* ,(filter-map (lambda (t a)
				 (when t 
				    (list t (j2s-scheme a mode return ctx))))
		     tmps args)
	      ,(proc (map (lambda (t a)
			     (or t (j2s-scheme a mode return ctx)))
			tmps args))))
       (proc (map (lambda (a) (j2s-scheme a mode return ctx)) args))))

;*---------------------------------------------------------------------*/
;*    can-side-effect? ...                                             */
;*---------------------------------------------------------------------*/
(define (can-side-effect? node)
   (let ((cell (make-cell #f)))
      (side-effect node #t cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    side-effect? ...                                                 */
;*---------------------------------------------------------------------*/
(define (side-effect? node)
   (let ((cell (make-cell #f)))
      (side-effect node #f cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    side-effect ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (side-effect this::J2SNode mustp::bool cell)
   (call-default-walker)
   (cell-ref cell))

;*---------------------------------------------------------------------*/
;*    side-effect ::J2SRef ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (side-effect this::J2SRef mustp::bool cell)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (usage)
	 (when (and mustp (decl-usage-has? decl '(assig)))
	    (cell-set! cell #t)
	    #t))))

;*---------------------------------------------------------------------*/
;*    side-effect ::J2SUnresolvedRef ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (side-effect this::J2SUnresolvedRef mustp::bool cell)
   (cell-set! cell #t)
   #t)
   
;*---------------------------------------------------------------------*/
;*    side-effect ::J2SLiteral ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (side-effect this::J2SLiteral mustp::bool cell)
   #f)

;*---------------------------------------------------------------------*/
;*    side-effect ::J2SAssig ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (side-effect this::J2SAssig mustp::bool cell)
   (cell-set! cell #t)
   #t)

;*---------------------------------------------------------------------*/
;*    side-effect ::J2SCall ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (side-effect this::J2SCall mustp::bool cell)
   (cell-set! cell #t)
   #t)

;*---------------------------------------------------------------------*/
;*    side-effect ::J2SNew ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (side-effect this::J2SNew mustp::bool cell)
   (cell-set! cell #t)
   #t)

;*---------------------------------------------------------------------*/
;*    side-effect ::J2SBindExit ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (side-effect this::J2SBindExit mustp::bool cell)
   (cell-set! cell #t)
   #t)
   
;*---------------------------------------------------------------------*/
;*    side-effect ::J2SYield ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (side-effect this::J2SYield mustp::bool cell)
   (cell-set! cell #t)
   #t)
   
;*---------------------------------------------------------------------*/
;*    side-effect ::J2SPragma ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (side-effect this::J2SPragma mustp::bool cell)
   (cell-set! cell #t)
   #t)
   
;*---------------------------------------------------------------------*/
;*    side-effect ::J2SObjInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (side-effect this::J2SObjInit mustp::bool cell)
   (cell-set! cell #t)
   #t)

;*---------------------------------------------------------------------*/
;*    importpath-var ...                                               */
;*---------------------------------------------------------------------*/
(define (importpath-var i::J2SImportPath)
   (with-access::J2SImportPath i (index)
      (string->symbol (format "%import~a" index))))

;*---------------------------------------------------------------------*/
;*    importpath-evar ...                                              */
;*---------------------------------------------------------------------*/
(define (importpath-evar i::J2SImportPath)
   (with-access::J2SImportPath i (index)
      (string->symbol (format "%import-evars~a" index))))

;*---------------------------------------------------------------------*/
;*    importpath-rvar ...                                              */
;*---------------------------------------------------------------------*/
(define (importpath-rvar i::J2SImportPath rindexes)
   (with-access::J2SImportPath i (index)
      (string->symbol (format "%import-evars~a:~(:)" index rindexes))))
