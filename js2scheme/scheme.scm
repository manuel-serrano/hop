;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 11:47:51 2013                          */
;*    Copyright   :  2013-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Generate a Scheme program from out of the J2S AST.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme

   (include "ast.sch"
	    "usage.sch"
	    "context.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_classutils
	   __js2scheme_alpha
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_array
	   __js2scheme_scheme-tarray
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-check
	   __js2scheme_scheme-program
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-call
	   __js2scheme_scheme-switch
	   __js2scheme_scheme-ops
	   __js2scheme_scheme-test
	   __js2scheme_scheme-class
	   __js2scheme_scheme-string
	   __js2scheme_scheme-regexp
	   __js2scheme_scheme-symbol
	   __js2scheme_scheme-math
	   __js2scheme_scheme-date
	   __js2scheme_scheme-array
	   __js2scheme_scheme-arguments
	   __js2scheme_scheme-spread
	   __js2scheme_scheme-bexit
	   __js2scheme_scheme-try
	   __js2scheme_scheme-constant
	   __js2scheme_scheme-record)
   
   (export j2s-scheme-stage
	   j2s-scheme-eval-stage
	   (j2s-scheme-box ::obj ::symbol ::procedure ::struct)
	   (generic j2s-scheme ::obj ::symbol ::procedure ::struct)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-scheme-stage
   (instantiate::J2SStageProc
      (name "scheme")
      (comment "Scheme code generation")
      (proc (lambda (ast conf)
	       (j2s-scheme ast 'normal comp-return
		  (compiler-context
		     (cons* :debug-client (bigloo-debug) conf)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-eval-stage ...                                        */
;*---------------------------------------------------------------------*/
(define j2s-scheme-eval-stage
   (instantiate::J2SStageProc
      (name "scheme")
      (comment "Scheme code generation (eval)")
      (proc (lambda (ast conf)
	       (j2s-scheme ast 'normal (lambda (x) x)
		  (compiler-context
		     (cons* :debug-client (bigloo-debug) conf)))))))

;*---------------------------------------------------------------------*/
;*    eval-return ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.9          */
;*---------------------------------------------------------------------*/
(define-macro (eval-return type value target)
   `(if return ,value ,value))

;*---------------------------------------------------------------------*/
;*    j2s-nodes* ...                                                   */
;*    -------------------------------------------------------------    */
;*    Compile a list of nodes, returns a list of expressions.          */
;*---------------------------------------------------------------------*/
(define (j2s-nodes*::pair-nil loc nodes mode return ctx)
   
   (define (undefined? stmt::J2SStmt)
      (cond
	 ((isa? stmt J2SStmtExpr)
	  (with-access::J2SStmtExpr stmt (expr)
	     (isa? expr J2SUndefined)))
	 ((isa? stmt J2SNop)
	  #t)))
   
   (define (remove-undefined sexps)
      (filter (lambda (x)
		 (not (equal? x '(js-undefined))))
	 sexps))
   
   (let loop ((nodes nodes))
      (cond
	 ((null? nodes)
	  (epairify loc
	     (return '(js-undefined))))
	 ((not (pair? (cdr nodes)))
	  (let ((sexp (j2s-scheme (car nodes) mode return ctx)))
	     (match-case sexp
		((begin . (and (? pair?) ?sexps))
		 sexps)
		(else
		 (epairify loc 
		    (list (return sexp)))))))
	 ((undefined? (car nodes))
	  (loop (cdr nodes)))
	 (else
	  (let ((sexp (j2s-scheme (car nodes) mode return ctx)))
	     (match-case sexp
		((begin . ?sexps)
		 (epairify loc
		    (append (remove-undefined sexps) (loop (cdr nodes)))))
		(else
		 (epairify loc
		    (cons sexp (loop (cdr nodes)))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-box ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-box this mode return::procedure ctx)
   (j2s-as (j2s-scheme this mode return ctx)
      this (j2s-type this) 'any ctx))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::obj ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (j2s-scheme this mode return::procedure ctx)
   (if (pair? this)
       (map (lambda (e) (j2s-scheme e mode return ctx)) this)
       this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SVarDecls ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SVarDecls mode return ctx)
   (illegal-node "j2s-scheme" this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-decl ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-decl this::J2SDecl value writable mode return ctx)

   (define (hidden-class decl::J2SDecl)
      (when (isa? decl J2SDeclExtern)
	 (with-access::J2SDeclExtern decl (hidden-class)
	    (when (not hidden-class)
	       `(:hidden-class #f)))))

   (define (raise-on-write? decl::J2SDecl)
      (when (isa? decl J2SDeclExtern)
	 (with-access::J2SDeclExtern decl (raise-on-write)
	    raise-on-write)))
   
   (with-access::J2SDecl this (loc scope id vtype)
      (let ((ident (j2s-decl-scm-id this ctx)))
	 (epairify-deep loc
	    (cond
	       ((memq scope '(global %scope tls))
		(let ((fun-name (format "function:~a:~a"
				   (cadr loc) (caddr loc)))
		      (def (if (eq? scope 'tls) 'define-tls 'define)))
		   (if (and (not (isa? this J2SDeclExtern)) (in-eval? return))
		       `(js-decl-eval-put! %scope ,(j2s-decl-name this ctx)
			   ,value ,(strict-mode? mode) %this)
		       (if (js-need-global? this scope mode)
			   `(,def ,ident
			       (let ((%%tmp ,value))
				  (js-define %this %scope
				     ,(j2s-decl-name this ctx)
				     (lambda (%) ,ident)
				     ,(if (raise-on-write? this)
					  `(lambda (% %v)
					     (js-raise-type-error %this
						,(format "Cannot assign to read only property '~s' of object" id) this))
					  `(lambda (% %v) (set! ,ident %v)))
				     %source ,(caddr loc)
				     ,@(or (hidden-class this) '()))
				  %%tmp))
			   `(,def ,ident ,value)))))
	       ((memq scope '(letblock letvar kont))
		(if (decl-ronly? this)
		    `(,(vtype-ident ident vtype (context-conf ctx)) ,value)
		    `(,ident ,value)))
	       ((eq? scope 'unbound)
		#unspecified)
	       (else
		`(define ,ident ,value)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDecl mode return ctx)
   
   (define (j2s-scheme-param this)
      (with-access::J2SDecl this (vtype)
	 (vtype-ident (j2s-decl-scm-id this ctx) vtype (context-conf ctx))))
   
   (define (j2s-scheme-var this)
      (with-access::J2SDecl this (loc id writable)
	 (j2s-scheme-decl this '(js-undefined) writable mode return ctx)))

   (define (decl-init-val decl)
      (if (decl-usage-has? decl '(uninit))
	  '(js-make-let)
	  #unspecified))
   
   (define (j2s-scheme-let this)
      (with-access::J2SDecl this (loc scope id utype)
	 (epairify loc
	    (case scope
	       ((global)
		`(define ,(j2s-decl-scm-id this ctx) ,(decl-init-val this)))
	       ((export)
		(with-access::J2SDeclInit this (val export)
		   (with-access::J2SExport export (index)
		      `(vector-set! %evars ,index ,(decl-init-val this)))))
	       ((tls)
		`(define-tls ,(j2s-decl-scm-id this ctx) ,(decl-init-val this)))
	       (else
		(let ((var (j2s-decl-scm-id this ctx)))
		   `(,var ,(decl-init-val this))))))))

   (cond
      ((j2s-let? this)
       (j2s-scheme-let this))
      ((j2s-param? this)
       (j2s-scheme-param this))
      (else
       (j2s-scheme-var this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclImport ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclImport mode return ctx)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclInit mode return ctx)
   
   (define (j2s-scheme-var this)
      (with-access::J2SDeclInit this (loc val writable)
	 (let ((ident (j2s-decl-scm-id this ctx)))
	    (epairify loc
	       (if writable
		   `(begin
		       (set! ,ident ,(j2s-scheme val mode return ctx))
		       (js-undefined))
		   `(begin
		       ,(j2s-scheme val mode return ctx)
		       (js-undefined)))))))
   
   (define (j2s-scheme-let-opt this)
      (with-access::J2SDeclInit this (scope id)
	 (if (memq scope '(global %scope tls record export))
	     (j2s-let-decl-toplevel this mode return ctx)
	     (error "j2s-scheme"
		(format "Should not be top-level, wrong scope \"~a\"" scope)
		(j2s->sexp this)))))

   (define (j2s-scheme-export this)
      (with-access::J2SDeclInit this (val export)
	 (with-access::J2SExport export (index)
	    `(vector-set! %evars ,index
		,(j2s-scheme val mode return ctx)))))

   (cond
      ((j2s-param? this) (call-next-method))
      ((j2s-let-opt? this) (j2s-scheme-let-opt this))
      ((j2s-let-class? this) (j2s-scheme-let-opt this))
      ((j2s-export? this) (j2s-scheme-export this))
      ((j2s-let? this) (call-next-method))
      (else (j2s-scheme-var this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SExportDefault ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SExportDefault mode return ctx)
   (with-access::J2SExportDefault this (index expr)
      `(vector-set! %evars ,index
	  ,(j2s-scheme expr mode return ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-set! lhs::J2SRef rhs::J2SExpr val result mode return ctx init? loc)
   
   (define (set decl hint loc)
      (let ((tval (j2s-as val rhs (j2s-type rhs) (j2s-vtype lhs) ctx)))
	 (cond
	    ((not (and (j2s-let? decl) (not (j2s-let-opt? decl))))
	     `(set! ,(j2s-ref-sans-cast lhs mode return ctx) ,tval))
	    (init?
	     `(set! ,(j2s-decl-scm-id decl ctx) ,tval))
	    (else
	     `(js-let-set! ,(j2s-decl-scm-id decl ctx) ,tval ',loc %this)))))

   (with-access::J2SRef lhs (decl)
      (with-access::J2SDecl decl (writable writable scope id hint export)
	 (with-access::J2SExpr rhs (type)
	    (cond
	       ((or writable init?)
		(cond
		   ((and (memq scope '(global %scope tls)) (in-eval? return))
		    `(begin
			,(j2s-put! loc '%scope #f (j2s-vtype lhs)
			    (& id (context-program ctx)) 'propname
			    val type (strict-mode? mode) ctx #f
			    :optim #f
			    :cachefun (is-lambda? val type))
			,result))
		   (export
		    `(begin
			,(with-access::J2SExport export (index)
			    `(vector-set! %evars ,index ,val))
			,result))
		   (result
		    `(begin
			,(set decl hint loc)
			,result))
		   (else
		    (set decl hint loc))))
	       ((and (not writable) (memq mode '(strict hopscript)))
		`(with-access::JsGlobalObject %this (js-type-error)
		    ,(match-case loc
			((at ?fname ?pos)
			 `(js-raise
			     (js-new %this js-type-error
				,(j2s-jsstring "Assignment to constant variable."
				    loc ctx)
				,fname ,pos)))
			(else
			 `(js-raise
			     (js-new %this js-type-error
				,(j2s-jsstring "Assignment to constant variable."
				    loc ctx)))))))
	       (else
		val))))))
	      
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclExtern ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclExtern mode return ctx)
   (with-access::J2SDeclExtern this (loc id val bind writable usecnt sweepable)
      (cond
	 ((and (=fx usecnt 0) (memq sweepable '(always scheme)))
	  #unspecified)
	 (bind
          (j2s-scheme-decl this (j2s-scheme val mode return ctx)
	     writable mode return ctx))
	 (else
	  (j2s-scheme val mode return ctx)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCast ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCast mode return ctx)
   (with-access::J2SCast this (expr type static)
      (let loop ((expr expr))
	 (cond
	    (static
	     (j2s-scheme expr mode return ctx))
	    ((isa? expr J2SBinary)
	     (or (j2s-scheme-binary-as expr mode return ctx type)
		 (j2s-cast (j2s-scheme expr mode return ctx)
		    expr (j2s-type expr) type ctx)))
	    ((isa? expr J2SUnary)
	     (or (j2s-scheme-unary-as expr mode return ctx type)
		 (j2s-cast (j2s-scheme expr mode return ctx)
		    expr (j2s-type expr) type ctx)))
	    ((isa? expr J2SParen)
	     (with-access::J2SParen expr (expr)
		;; push the cast inside the parenthesis
		(loop expr)))
	    ((isa? expr J2SSequence)
	     ;; push the cast to the last sequence expression
	     (with-access::J2SSequence expr (exprs)
		(with-access::J2SCast this ((cexpr expr))
		   (set! cexpr (car (last-pair exprs)))
		   (set-car! (last-pair exprs) this)
		   (j2s-scheme expr mode return ctx))))
	    ((isa? expr J2SBindExit)
	     (j2s-scheme (bindexit-cast expr this) mode return ctx))
	    (else
	     (j2s-cast (j2s-scheme expr mode return ctx)
		expr (j2s-type expr) type ctx))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCheck ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCheck mode return ctx)
   (with-access::J2SCheck this (expr type)
      (let loop ((expr expr))
	 (cond
	    ((isa? expr J2SParen)
	     (with-access::J2SParen expr (expr)
		;; push the cast inside the parenthesis
		(loop expr)))
	    ((isa? expr J2SSequence)
	     ;; push the cast to the last sequence expression
	     (with-access::J2SSequence expr (exprs)
		(with-access::J2SCheck this ((cexpr expr))
		   (set! cexpr (car (last-pair exprs)))
		   (set-car! (last-pair exprs) this)
		   (j2s-scheme expr mode return ctx))))
	    (else
	     (j2s-check (j2s-scheme expr mode return ctx)
		expr (j2s-type expr) type ctx))))))

;*---------------------------------------------------------------------*/
;*    j2s-ref-sans-cast ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-ref-sans-cast this::J2SRef mode return ctx)
   (with-access::J2SRef this (decl loc type)
      (with-access::J2SDecl decl (scope id vtype export)
	 (cond
	    ((isa? decl J2SDeclImport)
	     (with-access::J2SDeclImport decl (export import id)
		(if (isa? export J2SRedirect)
		    (with-access::J2SImport import (ipath)
		       (let loop ((export export)
				  (rindexes '()))
			  (if (isa? export J2SRedirect)
			      (with-access::J2SRedirect export (import export (rindex index))
				 (loop export (cons rindex rindexes)))
			      (with-access::J2SExport export (index)
				 (with-access::J2SImport import (path)
				    `(js-import-ref
					,(importpath-rvar ipath (reverse rindexes))
					,index (@ ,(symbol->string id) ,path)))))))
		    (with-access::J2SExport export (index)
		       (with-access::J2SImport import (ipath path)
			  `(js-import-ref ,(importpath-evar ipath) ,index
			      (@ ,(symbol->string id) ,path)))))))
	    ((and export (or (not (decl-ronly? decl)) (not (j2s-let-opt? decl))))
	     (with-access::J2SExport export (index decl)
		`(vector-ref %evars ,index)))
	    ((j2s-let-opt? decl)
	     (j2s-decl-scm-id decl ctx))
	    ((j2s-let? decl)
	     (if (decl-usage-has? decl '(uninit))
		 (epairify loc
		    `(js-let-ref ,(j2s-decl-scm-id decl ctx) ',id ',loc %this))
		 (j2s-decl-scm-id decl ctx)))
	    ((and (memq scope '(global %scope tls export)) (in-eval? return))
	     (epairify loc
		`(js-global-object-get-name %scope
		    ,(& id (context-program ctx)) #f %this)))
	    (else
	     (j2s-decl-scm-id decl ctx))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRef mode return ctx)
   (with-access::J2SRef this (decl loc type)
      (with-access::J2SDecl decl (scope id vtype export)
	 (let ((sexp (j2s-ref-sans-cast this mode return ctx)))
	    (j2s-as sexp this vtype type ctx)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSuper ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSuper mode return ctx)
   (with-access::J2SSuper this (decl loc context)
      (cond
	 ((eq? context 'literal)
	  `(js-super ,(call-next-method) #f ',loc %this))
	 ((isa? (j2s-vtype this) J2SRecord)
	  (let ((super (j2s-class-super-val (j2s-vtype this))))
	     (if (isa? super J2SRecord)
		 (class-prototype-id super)
		 (j2s-error "super" "Illegal context" this))))
	  (else
	   '%super-prototype))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWithRef ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWithRef mode return ctx)
   (with-access::J2SWithRef this (id withs expr loc)
      (epairify loc
	 (let loop ((withs withs))
	    (if (null? withs)
		(j2s-scheme expr mode return ctx)
		`(if ,(j2s-in? loc (& id (context-program ctx)) (car withs) ctx)
		     ,(j2s-get loc (car withs) #f 'object
			 (& id (context-program ctx)) 'string 'any ctx #f)
		     ,(loop (cdr withs))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SHopRef ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SHopRef mode return ctx)
   (with-access::J2SHopRef this (id)
      id))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SThis ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SThis mode return ctx)
   (with-access::J2SThis this (loc type decl)
      (let ((id (j2s-decl-scm-id decl ctx)))
	 (if (and (j2s-let? decl) (not (j2s-let-opt? decl)))
	     `(js-let-ref ,id ,id ',loc %this)
	     id))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SKontRef ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SKontRef mode return ctx)
   (with-access::J2SKontRef this (loc gen index id)
      `(js-generator-ref ,gen ,index ,(symbol->string! id))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCond ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCond mode return ctx)
   (with-access::J2SCond this (loc test then else)
      (let ((test (j2s-test test mode return ctx)))
	 (cond
	    ((eq? test #t)
	     (j2s-scheme then mode return ctx))
	    ((eq? test #f)
	     (j2s-scheme else mode return ctx))
	    (else
	     (epairify loc
		`(if ,test
		     ,(j2s-scheme then mode return ctx)
		     ,(j2s-scheme else mode return ctx))))))))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved-put! ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-unresolved-put! field expr throw::bool mode::symbol return loc)
   ;; no need to type check obj as we statically know that it is an obj
   (cond
      ((and (in-eval? return)
	    (not (eq? j2s-unresolved-put-workspace
		    j2s-unresolved-get-workspace)))
       `(js-unresolved-eval-put! %scope ,field ,expr ,(strict-mode? mode) ',loc %this))
      ((strict-mode? mode)
       `(js-unresolved-put! ,j2s-unresolved-put-workspace ,field ,expr #t ',loc %this))
      (else
       `(js-put! ,j2s-unresolved-put-workspace ,field ,expr ,throw %this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUnresolvedRef ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUnresolvedRef mode return ctx)
   (with-access::J2SUnresolvedRef this (loc cache id)
      (cond
	 ((is-builtin-ref? this 'JSON)
	  (epairify loc
	     `(with-access::JsGlobalObject %this (js-json)
		 js-json)))
	 (else
	  (epairify loc
	     (j2s-unresolved id (or loc #t) cache loc ctx))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SArrayAbsent ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArrayAbsent mode return ctx)
   (with-access::J2SArrayAbsent this (loc)
      (epairify loc '(js-absent))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLiteralValue ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLiteralValue mode return ctx)
   (with-access::J2SLiteralValue this (val type)
      (cond
	 ((number? val)
	  (error "j2s-scheme" "should not find a number here" val))
	 (else
	  val))))

;*---------------------------------------------------------------------*/
;*    *int29* ...                                                      */
;*---------------------------------------------------------------------*/
(define *+ints29* (-s32 (bit-lshs32 #s32:1 29) #s32:1))
(define *-ints29* (-s32 #s32:0 (bit-lshs32 #s32:1 29)))
(define *+intf29* (fixnum->flonum (int32->fixnum *+ints29*)))
(define *-intf29* (fixnum->flonum (int32->fixnum *-ints29*)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNumber ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNumber mode return ctx)
   (with-access::J2SNumber this (val type loc)
      (cond
	 ((eq? type 'integer)
	  (cond
	     ((flonum? val) (flonum->fixnum val))
	     ((int32? val) (int32->fixnum val))
	     ((uint32? val) (uint32->fixnum val))
	     (else val)))
	 ((eq? type 'int32)
	  (cond
	     ((flonum? val) (flonum->int32 val))
	     ((int32? val) val)
	     ((uint32? val) (uint32->int32 val))
	     (else (fixnum->int32 val))))
	 ((eq? type 'uint32)
	  (cond
	     ((flonum? val) (flonum->uint32 val))
	     ((uint32? val) (int32->uint32 val))
	     ((uint32? val) val)
	     (else (fixnum->uint32 val))))
	 ((fixnum? val)
	  (cond
	     ((m64? (context-conf ctx))
	      val)
	     ((and (>=fx val (negfx (bit-lsh 1 29))) (<fx val (bit-lsh 1 29)))
	      val)
	     ((and (>=fx val (negfx (bit-lsh 1 31))) (<fx val (bit-lsh 1 31))
		   (=fx (context-get ctx :int-size 29) 32))
	      val)
	     (else
	      (fixnum->flonum val))))
	 ((and (flonum? val) (nanfl? val))
	  +nan.0)
	 (else
	  (cond
	     ((flonum? val) val)
	     ((uint32? val) (int32->flonum val))
	     ((uint32? val) (uint32->flonum val))
	     ((bignum? val) val)
	     (else (fixnum->flonum val)))))))

;*---------------------------------------------------------------------*/
;*    j2s-property-scheme ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-property-scheme this::J2SExpr mode return ctx)
   (j2s-scheme this mode return ctx))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLiteralCnst ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLiteralCnst mode return ctx)
   (with-access::J2SLiteralCnst this (index val)
      (if (and #f (isa? val J2SRegExp))
	  ;; MS 27may2019: I think it is wrong to duplicate the
	  ;; regular expression as the lastIndex has to be preserved
	  ;; from one call to another...
	  ;; 
	  ;; regexp are hybrid, the rx part is precompiled but the
	  ;; JS object is dynamically allocated
 	  `(let ((rx::JsRegExp (js-cnst-table-ref ,index)))
	      (let ((nrx::JsRegExp (duplicate::JsRegExp rx)))
		 (js-object-mode-set! nrx (js-object-default-mode))
		 (js-object-properties-set! nrx
		    (list-copy (js-object-properties rx)))
		 nrx))
	  `(js-cnst-table-ref ,index))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2STemplate ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2STemplate mode return ctx)
   
   (define (empty-string? expr)
      (cond
	 ((isa? expr J2SString)
	  (with-access::J2SString expr (val) (string=? val "")))
	 ((isa? expr J2SCast)
	  (with-access::J2SCast expr (expr) (empty-string? expr)))
	 (else
	  #f)))
   
   (with-access::J2STemplate this (loc exprs)
      (let ((xs (filter-map (lambda (expr)
			       (unless (empty-string? expr)
				  (j2s-scheme expr mode return ctx)))
		   exprs)))
	 (cond
	    ((null? xs)
	     (& "" (context-program ctx)))
	    ((null? (cdr xs))
	     (car xs))
	    (else
	     (let ((js-jsstring-append (if (context-get ctx :profile-mem)
					   'js-jsstring-append-no-inline
					   'js-jsstring-append)))
		(epairify loc
		   `(,js-jsstring-append ,(car xs)
		       ,(let loop ((xs (cdr xs)))
			   (if (null? (cdr xs))
			       (car xs)
			       (epairify loc
				  `(,js-jsstring-append ,(car xs)
				      ,(loop (cdr xs))))))))))))))
 
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNativeString ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNativeString mode return ctx)
   (with-access::J2SNativeString this (loc val)
      val))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SString ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SString mode return ctx)
   (with-access::J2SString this (loc val type)
      (if (eq? type 'buffer)
	  (epairify loc `(js-string->jsbuffer ,val))
	  (epairify loc (& this (context-program ctx))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRegExp ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRegExp mode return ctx)
   (with-access::J2SRegExp this (loc val flags)
      (epairify loc
	 `(with-access::JsGlobalObject %this (js-regexp)
	     ,(j2s-new loc 'js-regexp
		 (if (string-null? flags)
		     (list (j2s-jsstring val loc ctx))
		     (list (j2s-jsstring val loc ctx)
			(j2s-jsstring flags loc ctx))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCmap ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCmap mode return ctx)
   (with-access::J2SCmap this (loc val)
      ;; change the building of J2SCmap to build directly
      ;; a list of string (do this when the new branch is full ready)
      ;; see constant.scm
      (epairify loc
	 `(js-strings->cmap
	     (vector ,@(map symbol->string (vector->list val)))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNull ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNull mode return ctx)
   (with-access::J2SLiteral this (loc)
      (epairify loc '(js-null))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUndefined ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUndefined mode return ctx)
   (with-access::J2SLiteral this (loc)
      (epairify loc '(js-undefined))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SReturn mode return ctx)
   (with-access::J2SReturn this (loc expr tail exit from)
      (cond
	 (exit
	  (epairify loc
	     `(%jsexit ,(j2s-scheme expr mode return ctx))))
	 (tail
	  (j2s-scheme expr mode return ctx))
	 ((isa? from J2SBindExit)
	  (with-access::J2SBindExit from (lbl)
	     (let ((val (j2s-scheme expr mode return ctx)))
		(if lbl
		    (epairify loc `(,lbl ,val))
		    val))))
	 (else
	  (epairify loc
	     `(%return
		 ,(j2s-scheme expr mode return ctx)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SThrow ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SThrow mode return ctx)
   (with-access::J2SThrow this (loc expr)
      (epairify loc
	 (if (> (bigloo-debug) 0)
	     `(js-throw/debug ,(j2s-scheme expr mode return ctx)
		 ,(j2s-jsstring (cadr loc) loc ctx) ,(caddr loc) %worker)
	     `(js-throw ,(j2s-scheme expr mode return ctx)
		 ,(j2s-jsstring (cadr loc) loc ctx) ,(caddr loc))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWith ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWith mode return ctx)
   (with-access::J2SWith this (obj block id)
      `(let ((,id (js-toobject %this ,(j2s-scheme obj mode return ctx))))
	  ,(j2s-scheme block mode return ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPragma ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPragma mode return ctx)
   (with-access::J2SPragma this (loc expr lang vars vals)
      (case lang
	 ((scheme)
	  (if (null? vars)
	      (epairify-deep loc expr)
	      (epairify-deep loc
		 `(let ,(map (lambda (v e)
				`(,v ,(j2s-scheme e mode return ctx)))
			   vars vals)
		     ,expr))))
	 ((scheme-quote)
	  `',(epairify-deep loc expr))
	 (else
	  (if (j2s-new-target? this)
	      'new-target
	      "#unspecified")))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSequence mode return ctx)
   (with-access::J2SSequence this (loc exprs)
      (let loop ((exprs exprs))
	 (cond
	    ((null? (cdr exprs))
	     (j2s-scheme (car exprs) mode return ctx))
	    ((isa? (car exprs) J2SUndefined)
	     (loop (cdr exprs)))
	    (else
	     (epairify loc
		(flatten-begin
		   (j2s-scheme exprs mode return ctx))))))))

;*---------------------------------------------------------------------*/
;*    j2s-let-decl-toplevel ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-let-decl-toplevel::pair-nil d::J2SDeclInit mode return ctx)
   (with-access::J2SDeclInit d (val hint scope loc)
      (if (or (not (isa? val J2SFun))
	      (isa? val J2SSvc)
	      (decl-usage-has? d '(assig)))
	  (let ((ident (j2s-decl-scm-id d ctx)))
	     (if (decl-usage-has? d '(eval))
		 `(begin
		     (define ,(j2s-decl-scm-id d ctx)
			,(j2s-export-decl-val d (j2s-scheme val mode return ctx)))
		     (js-define %this ,scope ,(j2s-decl-name d ctx)
			(lambda (%) ,ident)
			(lambda (% %v) (set! ,ident %v))
			%source
			,(caddr loc)))
		 `(define ,(j2s-decl-scm-id d ctx)
		     ,(j2s-export-decl-val d (j2s-scheme val mode return ctx)))))
	  (j2s-let-decl-toplevel-fun d mode return ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-export-decl-val ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-export-decl-val decl val)
   (with-access::J2SDecl decl (export)
      (if export
	  (with-access::J2SExport export (index)
	     `(let ((val ,val))
		(vector-set! %evars ,index val)
		val))
	  val)))
      
;*---------------------------------------------------------------------*/
;*    j2s-let-decl-toplevel-fun ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-let-decl-toplevel-fun::pair-nil d::J2SDeclInit mode return ctx)
   (with-access::J2SDeclInit d (val hint scope loc export)
      (cond
	 ((decl-usage-has? d '(ref get new set eval))
	  (let* ((id (j2s-decl-fast-id d ctx))
		 (^id (j2s-decl-scm-id d ctx))
		 (fun (jsfun->lambda val mode return ctx #f)))
	     (with-access::J2SFun val (type generator)
		`(begin
		    ,@(if generator
			  `((define ,(j2s-generator-prototype-id val)
			       ,(j2s-generator-prototype->scheme val)))
			  '())
		    (define ,id ,fun)
		    (define ,^id
		       ,(if (eq? type 'procedure)
			    id
			    (let ((tmp (gensym 'p)))
			       `(let ((,tmp ,(jsfun->lambda val mode return ctx #f)))
				   ,(j2sfun->scheme val tmp #f mode return ctx)))))
		    ,@(if (decl-usage-has? d '(eval))
			  `((js-define %this ,scope ,(j2s-decl-name d ctx)
			       (lambda (%) ,^id)
			       (lambda (% %v) (set! ,^id %v))
			       %source
			       ,(caddr loc)))
			  '())
		    ,@(if export
			  (with-access::J2SExport export (index)
			     `((vector-set! %evars ,index ,id)))
			  '())))))
	 ((decl-usage-has? d '(call))
	  (with-access::J2SFun val (generator)
	     (let ((id (j2s-decl-fast-id d ctx)))
		(cond
		   (generator
		    `(begin
			(define ,(j2s-generator-prototype-id val)
			   ,(j2s-generator-prototype->scheme val))
			(define ,id
			   ,(jsfun->lambda val mode return ctx #f))
			,@(if export
			      (with-access::J2SExport export (index)
				 `((vector-set! %evars ,index
				      ,(j2s-generator-prototype-id val))))
			      '())))
		   (export
		    `(begin
			`(define ,id
			    ,(jsfun->lambda val mode return ctx #f))
			,(with-access::J2SExport export (index)
			    `(vector-set! %evars ,index ,id))))
		   (else
		    `(define ,id
			,(jsfun->lambda val mode return ctx #f)))))))
	 (else
	  '()))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLetBlock ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLetBlock mode return ctx)

   (define (j2s-bind-generator-prototype val decls)
      (with-access::J2SFun val (generator)
	 (if generator
	     (cons `(,(j2s-generator-prototype-id val)
		     ,(j2s-generator-prototype->scheme val))
		decls)
	     decls)))

   (define (j2s-stack-allocation-type val)
      (let loop ((expr val))
	 (cond
	    ((isa? expr J2SCast)
	     (with-access::J2SCast expr (expr)
		(loop expr)))
	    ((isa? expr J2SParen)
	     (with-access::J2SCast expr (expr)
		(loop expr)))
	    ((isa? expr J2SPragma)
	     (with-access::J2SPragma expr (expr)
		(cond
		   ((equal? expr '(js-make-stack-yield))
		    (values 'yield '(js-make-yield #unspecified #f %this)))
		   (else
		    (error "j2s-let-decl-stack-allocation"
		       "Illegal pragma form"
		       (j2s->sexp val))))))
	    (else
	     (error "j2s-let-decl-stack-allocation" "Illegal form"
		(j2s->sexp val))))))
      
   (define (j2s-let-decl-inner::pair-nil d::J2SDecl mode return ctx singledecl typed)
      (with-access::J2SDeclInit d (vtype loc)
	 (let* ((ident (j2s-decl-scm-id d ctx))
		(var (if typed (type-ident ident vtype (context-conf ctx)) ident))
		(val (j2sdeclinit-val-fun d)))
	    (cond
	       ((or (not (isa? val J2SFun))
		    (isa? val J2SSvc)
		    (not (decl-ronly? d)))
		`((,var ,(j2s-scheme val mode return ctx))))
	       ((decl-usage-has? d '(ref get new set))
		(with-access::J2SFun val (decl)
		   (j2s-bind-generator-prototype val
		      (if (isa? decl J2SDecl)
			  (let ((tmp (j2s-decl-fast-id d ctx))
				(proc (gensym 'proc))
				(^tmp (j2s-decl-scm-id decl ctx))
				(fun (jsfun->lambda val mode return ctx #f)))
			     `((,^tmp #unspecified)
			       (,tmp ,fun)
			       (,var (let ((,proc ,(j2sfun->scheme val tmp #f mode return ctx)))
					(set! ,^tmp ,proc)
					,proc))))
			  (let ((fun (jsfun->lambda val mode return ctx #f))
				(tmp (j2s-decl-fast-id d ctx)))
			     `((,tmp ,fun)
			       (,var ,(j2sfun->scheme val tmp #f mode return ctx))))))))
	       ((decl-usage-has? d '(call))
		(j2s-bind-generator-prototype val
		   `((,(j2s-decl-fast-id d ctx)
		      ,(jsfun->lambda val mode return ctx #f)))))
	       (else
		'())))))

   (define (move-stack-decl! decl::J2SDecl decls::pair-nil)
      ;; move decl after the last &ref decls
      (let loop ((decls decls))
	 (cond
	    ((null? decls)
	     (list decl))
	    ((decl-usage-has? (car decls) '(&ref))
	     (cons (car decls) (loop (cdr decls))))
	    (else
	     (cons decl decls)))))
   
   (with-access::J2SLetBlock this (loc decls nodes rec)
      (cond
	 ((null? decls)
	  (epairify loc
	     `(begin ,@(j2s-nodes* loc nodes mode return ctx))))
	 ((decl-usage-has? (car decls) '(&ref))
	  ;; an optimized stack allocation (see for instance genyield)
	  (with-access::J2SDeclInit (car decls) (id val)
	     (let ((id (symbol-append '& id)))
		(multiple-value-bind (atype arg)
		   (j2s-stack-allocation-type val)
		   (set! val (J2SHopRef/type id 'object))
		   (decl-usage-rem! (car decls) '&ref)
		   (set! decls (move-stack-decl! (car decls) (cdr decls)))
		   (epairify loc
		      `(,(symbol-append 'js-call-with-stack- atype)
			,arg (lambda (,id)
				,(j2s-scheme this mode return ctx))))))))
	 ((and (pair? decls)
	       (null? (cdr decls))
	       (pair? nodes)
	       (null? (cdr nodes))
	       (isa? (car nodes) J2SForIn)
	       (with-access::J2SDecl (car decls) (binder)
		  (eq? binder 'let-forin)))
	  ;; a for( let v in ... ) {} statement
	  (j2s-scheme (car nodes) mode return ctx))
	 ((any (lambda (decl::J2SDecl)
		  (with-access::J2SDecl decl (scope)
		     (memq scope '(global export))))
	     decls)
	  ;; top-level or function level block
	  (epairify loc
	     `(begin
		 ,@(map (lambda (d)
			   (cond
			      ((j2s-let-opt? d)
			       (j2s-let-decl-toplevel d mode return ctx))
			      ((isa? d J2SDeclFun)
			       (with-access::J2SDeclFun d (scope)
				  (set! scope 'global))
			       (j2s-scheme d mode return ctx))
			      (else
			       (with-access::J2SDecl d (scope)
				  (set! scope 'global))
			       (j2s-scheme d mode return ctx))))
		      decls)
		 ,@(j2s-scheme nodes mode return ctx))))
	 (else
	  ;; inner letblock, create a let block
	  (let* ((ds (append-map (lambda (d)
				    (cond
				       ((or (j2s-let-opt? d) (j2s-let-class? d))
					(j2s-let-decl-inner d
					   mode return ctx
					   (null? (cdr decls))
					   (not rec)))
				       ((isa? d J2SDeclFun)
					(with-access::J2SDeclFun d (binder)
					   (if (memq binder '(let let-opt))
					       (j2s-let-decl-inner d
						  mode return ctx
						  (null? (cdr decls))
						  (not rec))
					       (j2s-scheme d mode return ctx))))
				       (else
					(list (j2s-scheme d mode return ctx)))))
			decls))
		 (body (j2s-nodes* loc nodes mode return ctx))
		 (rec (or rec
			  (any (lambda (d)
				  (when (isa? d J2SDeclFun)
				     (with-access::J2SDeclFun d (val)
					(cond
					   ((isa? val J2SFun)
					    (with-access::J2SFun val (generator)
					       generator))
					   ((isa? val J2SMethod)
					    (with-access::J2SMethod val (function)
					       (with-access::J2SFun function (generator)
						  generator)))))))
			     decls))))
	     (epairify loc
		(cond
		   ((null? ds)
		    `(begin ,@body))
		   ((null? (cdr ds))
		    `(,(if rec 'letrec 'let) ,ds ,@body))
		   (else
		    `(,(if rec 'letrec* 'let*) ,ds ,@body)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SParen ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SParen mode return ctx)
   (with-access::J2SParen this (expr type)
      (j2s-scheme expr mode return ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-stmt-sans-begin ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-stmt-sans-begin::pair this::J2SStmt mode return ctx)
   (let ((sexp (j2s-scheme this mode return ctx)))
      (match-case sexp
	 ((begin . ?sexps) sexps)
	 (else (list sexp)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SStmt ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12           */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SStmt mode return ctx)
   (return this))

;*---------------------------------------------------------------------*/
;*    j2s-sequence ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-sequence loc nodes::pair-nil mode return ctx)
   
   (define (undefined? stmt::J2SStmt)
      (cond
	 ((isa? stmt J2SStmtExpr)
	  (with-access::J2SStmtExpr stmt (expr)
	     (isa? expr J2SUndefined)
	     (and (isa? expr J2SLiteral) (not (isa? expr J2SArray)))))
	 ((isa? stmt J2SNop)
	  #t)))
   
   (let loop ((nodes nodes))
      (cond
	 ((null? nodes)
	  (epairify loc
	     (return '(js-undefined))))
	 ((not (pair? (cdr nodes)))
	  (j2s-scheme (car nodes) mode return ctx))
	 ((undefined? (car nodes))
	  (loop (cdr nodes)))
	 ((any (lambda (n) (isa? n J2SDeclFun)) nodes)
	  (epairify loc
	     `(let ()
		 ,@(cdr
		      (flatten-begin
			 (j2s-scheme nodes mode return ctx))))))
	 (else
	  (epairify loc
	     (flatten-begin
		(j2s-scheme nodes mode return ctx)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SMeta ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SMeta mode return ctx)
   (with-access::J2SMeta this (stmt optim)
      (if (=fx optim 0)
	  `(%%noinline
	      ,(j2s-scheme stmt mode return
		  (new-compiler-context ctx :optim 0)))
	  (j2s-scheme stmt mode return ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSeq ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.1         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSeq mode return ctx)
   (with-access::J2SSeq this (loc nodes)
      (j2s-sequence loc nodes mode return ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBlock ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBlock mode return ctx)

   (define (begin-or-let loc bindings nodes)
      (if (null? bindings)
	  (j2s-sequence loc nodes mode return ctx)
	  (epairify loc
	     `(let ,(reverse! bindings)
		 ,@(j2s-nodes* loc nodes mode return ctx)))))
   
   (with-access::J2SBlock this (nodes loc)
      (let loop ((nodes nodes)
		 (bindings '()))
	 (cond
	    ((null? nodes)
	     (begin-or-let loc bindings nodes))
	    ((or (isa? (car nodes) J2SDeclFun)
		 (isa? (car nodes) J2SDeclExtern))
	     (begin-or-let loc bindings nodes))
	    ((isa? (car nodes) J2SDecl)
	     (with-access::J2SDecl (car nodes) (binder scope)
		(if (eq? binder 'var)
		    (begin
		       (set! scope 'letvar)
		       (loop (cdr nodes)
			  (cons (j2s-scheme (car nodes) mode return ctx)
			     bindings)))
		    (begin-or-let loc bindings nodes))))
	    (else
	     (begin-or-let loc bindings nodes))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNop ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.3         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNop mode return ctx)
   (with-access::J2SNop this (loc)
      (epairify loc
	 (return '(js-undefined)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SStmtExpr ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.4         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SStmtExpr mode return ctx)
   (with-access::J2SStmtExpr this (expr)
      (cond
	 ((isa? expr J2SIf)
	  (j2s-scheme expr mode return ctx))
	 ((isa? expr J2SCall)
	  (with-access::J2SCall expr (%info)
	     (set! %info 'void))
	  (return (j2s-scheme expr mode return ctx)))
	 (else
	  (return (j2s-scheme expr mode return ctx))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SIf ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.5         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SIf mode return ctx)
   
   (define (gen-if test then else)
      `(if ,(j2s-test test mode return ctx)
	   ,(j2s-scheme then mode return ctx)
	   ,(j2s-scheme else mode return ctx)))
   
   (with-access::J2SIf this (loc test then else)
      (epairify loc
	 (if (isa? else J2SNop)
	     `(if ,(j2s-test test mode return ctx)
		  ,(j2s-scheme then mode return ctx)
		  (js-undefined))
	     (let ((ts (if-check-cache-cascade-tests this)))
		(if (>fx (length ts) 2)
		    ;; optimizes this cascade
		    (let ((cmap (gensym 'cmap)))
		       (with-access::J2SCacheCheck (car ts) (obj)
			  ;; patch all the tests of the cascade
			  (for-each (lambda (t)
				       (with-access::J2SCacheCheck t (obj prop)
					  (set! prop 'cmap-proto-method)
					  (with-access::J2SRef obj (%info)
					     (set! %info cmap))))
			     ts)
			  ;; bind a temporary with the object hidden class
			  `(let ((,cmap (js-object-cmap ,(j2s-scheme obj mode return ctx))))
			      ,(gen-if test then else))))
		    (gen-if test then else)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPrecache ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPrecache mode return ctx)

   (define (precache-access this::J2SAccess)
      (with-access::J2SAccess this (obj field cache)
	 (let* ((scmobj (j2s-scheme obj mode return ctx))
		(precache `(eq? (with-access::JsObject ,scmobj (cmap) cmap)
			      (js-pcache-cmap ,(js-pcache cache)))))
	    (with-access::J2SRef obj (type)
	       (if (eq? type 'object)
		   precache
		   `(and (js-object? ,scmobj) ,precache))))))
   
   (define (precache-test this)
      (with-access::J2SPrecache this (accesses)
	 (let loop ((nodes accesses))
	    (let ((n (car nodes)))
	       (if (null? (cdr nodes))
		   (precache-access n)
		   `(and ,(precache-access (car n)) ,(loop (cdr nodes))))))))
   
   (with-access::J2SPrecache this (loc accesses then else)
      (epairify loc
	 `(if ,(precache-test this)
	      ,(j2s-scheme then mode return ctx)
	      ,(j2s-scheme else mode return ctx)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SIfIsRecord ...                                   */
;*    -------------------------------------------------------------    */
;*    This form is only introduced by the inlining of method calls.    */
;*    The TEST is always resolved statically and only the THEN         */
;*    or the ELSE branch is generated. In other words, no runtime      */
;*    test is generated and the two branches are never generated       */
;*    at compile-time.                                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SIfIsRecord mode return ctx)
   
   (define (j2s-vtype-uncast expr)
      (if (isa? expr J2SCast)
	  (with-access::J2SCast expr (expr)
	     (j2s-vtype-uncast expr))
	  (j2s-vtype expr)))
   
   (with-access::J2SIfIsRecord this (test then else)
      (if (isa? (j2s-vtype-uncast test) J2SRecord)
	  (j2s-scheme then mode return ctx)
	  (j2s-scheme else mode return ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDo ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.1       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDo mode return ctx)
   (with-access::J2SDo this (loc test body id
			       need-bind-exit-break need-bind-exit-continue)

      (define (comp-loop loop)
	 `(let ,loop ()
	       ,(if need-bind-exit-continue
		    (epairify-deep loc
		       `(bind-exit (,(j2s-escape-id '%continue id))
			   ,@(j2s-stmt-sans-begin body mode return ctx)))
		    (j2s-scheme body mode return ctx))
	       (if ,(j2s-test test mode return ctx)
		   (,loop)
		   (js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       ,(if need-bind-exit-continue
		    (epairify-deep loc
		       `(bind-exit (,(j2s-escape-id '%continue id))
			   ,@(j2s-stmt-sans-begin body mode acc-return ctx)))
		    (j2s-scheme body mode acc-return ctx))
	       (if ,(j2s-test test mode return ctx)
		   (,loop %acc)
		   %acc)))
      
      (let* ((doid (gensym 'do))
	     (loop (if (in-eval? return) (eval-loop doid) (comp-loop doid))))
	 (epairify-deep loc
	    (if need-bind-exit-break
		(epairify-deep loc `(bind-exit (,(j2s-escape-id '%break id)) ,loop))
		(epairify loc loop))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWhile ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.2       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWhile mode return ctx)
   (with-access::J2SWhile this (loc test body id
				  need-bind-exit-break need-bind-exit-continue)

      (define (comp-loop loop)
	 `(let ,loop ()
	       (if ,(j2s-test test mode return ctx)
		   ,(if need-bind-exit-continue
			(epairify-deep loc
			   `(begin
			       (bind-exit (,(j2s-escape-id '%continue id))
				  ,@(j2s-stmt-sans-begin body mode return ctx))
			       (,loop)))
			(epairify-deep loc
			   `(begin
			       ,@(j2s-stmt-sans-begin body mode return ctx)
			       (,loop))))
		   (js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       (if ,(j2s-test test mode return ctx)
		   ,(if need-bind-exit-continue
			(epairify-deep loc
			   `(begin
			       (bind-exit (,(j2s-escape-id '%continue id))
				  ,@(j2s-stmt-sans-begin body mode acc-return ctx))
			       (,loop %acc)))
			(epairify-deep loc
			   `(begin
			       ,@(j2s-stmt-sans-begin body mode acc-return ctx)
			       (,loop %acc))))
		   %acc)))
      
      (let* ((whileid (string->symbol
			 (format "while@~a:~a" (cadr loc) (caddr loc))))
	     (loop (if (in-eval? return) (eval-loop whileid) (comp-loop whileid))))
	 (epairify-deep loc
	    (if need-bind-exit-break
		(epairify-deep loc `(bind-exit (,(j2s-escape-id '%break id)) ,loop))
		(epairify loc loop))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SFor ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.3       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SFor mode return ctx)
   (with-access::J2SFor this (loc init test incr body id
				need-bind-exit-break
				need-bind-exit-continue
				need-bind-exit-continue-label)
      
      (define (comp-loop loop)
	 `(let ,loop ()
	       (if ,(j2s-test test mode return ctx)
		   (begin
		      ,(if need-bind-exit-continue
			   (epairify-deep loc
			      `(bind-exit (,(j2s-escape-id '%continue id))
				  ,(j2s-scheme body mode return ctx)))
			   (j2s-scheme body mode return ctx))
		      ,(j2s-scheme incr mode return ctx)
		      (,loop))
		   (js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       (if ,(j2s-test test mode return ctx)
		   (begin
		      ,(if need-bind-exit-continue
			   (epairify-deep loc
			      `(bind-exit (,(j2s-escape-id '%continue id))
				  ,(j2s-scheme body mode acc-return ctx)))
			   (j2s-scheme body mode acc-return ctx))
		      ,(j2s-scheme incr mode return ctx)
		      (,loop %acc))
		   %acc)))

      (let* ((forid (string->symbol
		       (format "for@~a:~a" (cadr loc) (caddr loc))))
	     (loop (if (in-eval? return) (eval-loop forid) (comp-loop forid))))
	 (epairify-deep loc
	    `(begin
		,@(if (isa? init J2SNop)
		      '()
		      (list (j2s-scheme init mode return ctx)))
		,(if need-bind-exit-break
		     (epairify-deep loc
			`(bind-exit (,(j2s-escape-id '%break id)) ,loop))
		     (epairify loc loop)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SForIn ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.4       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SForIn mode return ctx)

   (define (js-for-in op obj)
      (cond
	 ((eq? op 'in) 'js-for-in)
	 ((eq? (j2s-type obj) 'array) 'js-array-for-of)
	 (else 'js-for-of)))

   (define (close op close)
      (cond
	 ((eq? op 'in) '())
	 (close '(#t))
	 (else '(#f))))
   
   (define (for-in/break-comp tmp name props obj body set op)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(,(js-for-in op obj)
		      ,(j2s-scheme obj mode return ctx)
		      (lambda (,name %this)
			 ,set
			 ,(if need-bind-exit-continue
			      `(bind-exit (,(j2s-escape-id '%continue id))
				  ,(j2s-scheme body mode return ctx))
			      (j2s-scheme body mode return ctx)))
		      ,@(close op #t)
		      %this)))
	    (if need-bind-exit-break
		`(bind-exit (,(j2s-escape-id '%break id)) ,for)
		for))))
   
   (define (for-in/break-eval tmp name props obj body set op)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(let ((%acc (js-undefined)))
			(,(js-for-in op obj)
			 ,(j2s-scheme obj mode return ctx)
			 (lambda (,name %this)
			    ,set
			    ,(if need-bind-exit-continue
				 `(bind-exit (,(j2s-escape-id '%continue id))
				     ,(j2s-scheme body mode acc-return ctx))
				 (j2s-scheme body mode acc-return ctx)))
			 ,@(close op #t)
			 %this)
			%acc)))
	    (if need-bind-exit-break
		`(bind-exit (,(j2s-escape-id '%break id)) ,for)
		for))))

   (define (for-in/break tmp name props obj body set op)
      (if (in-eval? return)
	  (for-in/break-eval tmp name props obj body set op)
	  (for-in/break-comp tmp name props obj body set op)))

   (define (for-in/w-break-comp tmp name props obj body set op)
      `(,(js-for-in op obj) ,(j2s-scheme obj mode return ctx)
	  (lambda (,name %this)
	     ,set
	     ,(j2s-scheme body mode return ctx))
	  ,@(close op (throw? body))
	  %this))

   (define (for-in/w-break-eval tmp name props obj body set op)
      `(let ((%acc (js-undefined)))
	  (,(js-for-in op obj) ,(j2s-scheme obj mode return ctx)
	     (lambda (,name %this)
		,set
		,(j2s-scheme body mode acc-return ctx))
	     ,@(close op (throw? body))
	     %this)
	  %acc))

   (define (for-in/w-break tmp name props obj body set op)
      (if (in-eval? return)
	  (for-in/w-break-eval tmp name props obj body set op)
	  (for-in/w-break-comp tmp name props obj body set op)))

   (define (set lhs name loc)
      (let loop ((lhs lhs))
	 (cond
	    ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
	     (epairify loc
		(j2s-scheme-set! lhs lhs name #f mode return ctx #f loc)))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id loc)
		(epairify loc
		   (j2s-unresolved-put! (& id (context-program ctx))
		      name #f mode return loc))))
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field loc)
		(epairify loc
		   (j2s-put! loc (j2s-scheme obj mode return ctx)
		      field
		      (typeof-this obj ctx)
		      (j2s-scheme field mode return ctx)
		      (j2s-type field)
		      name 'any (strict-mode? mode) ctx #t
		      :optim (when (is-hint? obj 'array) 'array)
		      :cachefun #f))))
	    ((isa? lhs J2SWithRef)
	     (with-access::J2SWithRef lhs (id withs expr loc)
		(epairify loc
		   (let liip ((withs withs))
		      (if (null? withs)
			  (loop expr)
			  `(if ,(j2s-in? loc
				   (& id (context-program ctx)) (car withs) ctx)
			       ,(j2s-put! loc (car withs) #f
				   'object
				   (& id (context-program ctx)) 'propname
				   name 'any #f ctx #t
				   :optim #f
				   :cachefun #f)
			       ,(liip (cdr withs))))))))
	    ((isa? lhs J2SKontRef)
	     (with-access::J2SKontRef lhs (index gen loc)
		'#unspecified))
	    (else
	     (j2s-error "js2scheme" "Illegal lhs" this)))))
   
   (with-access::J2SForIn this (loc lhs obj body op
				  need-bind-exit-break need-bind-exit-continue)
      (cond
;* 	 ((eq? (j2s-type obj) 'array)                                  */
;* 	  (tprint "ICI"))                                              */
	 ((and (isa? lhs J2SRef)
	       (with-access::J2SRef lhs (decl)
		  (with-access::J2SDecl decl (binder)
		     (eq? binder 'let-forin))))
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (binder)
		(let* ((tmp (gensym 'forintmp))
		       (name (j2s-decl-scm-id decl ctx))
		       (props (gensym 'forinprops))
		       (set #unspecified))
		(epairify-deep loc
		   (if (or need-bind-exit-continue need-bind-exit-break)
		       (for-in/break tmp name props obj body set op)
		       (for-in/w-break tmp name props obj body set op)))))))
	 (else
	  (let* ((tmp (gensym 'forintmp))
		 (name (gensym 'forinname))
		 (props (gensym 'forinprops))
		 (set (set lhs name loc)))
	     (epairify-deep loc
		(if (or need-bind-exit-continue need-bind-exit-break)
		    (for-in/break tmp name props obj body set op)
		    (for-in/w-break tmp name props obj body set op))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLabel ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLabel mode return ctx)
   (with-access::J2SLabel this (body need-bind-exit-break id)
      (if need-bind-exit-break
	  `(bind-exit (,(j2s-escape-id '%break id)) 
	      ,(j2s-scheme body mode return ctx))
	  (j2s-scheme body mode return ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBreak ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBreak mode return ctx)
   (with-access::J2SBreak this (loc target)
      (with-access::J2SIdStmt target (id)
	 (epairify loc
	    `(,(j2s-escape-id '%break id)
	      ,(if (in-eval? return) '%acc '(js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SContinue ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SContinue mode return ctx)
   (with-access::J2SContinue this (loc target)
      (with-access::J2SLoop target (id)
	 (epairify loc
	    `(,(j2s-escape-id '%continue id)
	      ,(if (in-eval? return) '%acc '(js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssig mode return ctx)

   (define (maybe-array-set lhs::J2SAccess rhs::J2SExpr)
      (with-access::J2SAccess lhs (obj field cache loc)
	 (if (isa? lhs J2SRef)
	     `(if (js-array? ,(j2s-scheme obj mode return ctx))
		  ,(j2s-array-set! this mode return ctx)
		  ,(j2s-put! loc (j2s-scheme obj mode return ctx)
		      field
		      (typeof-this obj ctx)
		      (j2s-scheme field mode return ctx)
		      (j2s-type field)
		      (j2s-scheme rhs mode return ctx)
		      (j2s-type rhs)
		      (strict-mode? mode)
		      ctx
		      cache
		      :optim #f
		      :cachefun (or (is-function? rhs) (is-prototype? obj))))
	     (let* ((tmp (gensym 'assigtmp))
		    (access (duplicate::J2SAccess lhs (obj (J2SHopRef tmp))))
		    (tyo (j2s-type obj)))
		(cond
		   ((eq? tyo 'array)
		    (j2s-array-set! this mode return ctx))
		   ((memq tyo '(int8array uint8array))
		    (j2s-tarray-set! this mode return ctx))
		   (else
		    `(let ((,tmp ,(j2s-scheme obj mode return ctx)))
			(if (js-array? ,tmp)
			    ,(j2s-array-set!
				(duplicate::J2SAssig this
				   (lhs (duplicate::J2SAccess lhs
					   (obj (J2SHopRef tmp)))))
				mode return ctx)
			    ,(j2s-put! loc tmp
				field
				(typeof-this obj ctx)
				(j2s-scheme field mode return ctx)
				(j2s-type field)
				(j2s-scheme rhs mode return ctx)
				(j2s-type rhs)
				(strict-mode? mode)
				ctx
				cache
				:optim #f
				:cachefun (or (is-function? rhs)
					      (is-prototype? obj)))))))))))

   (define (j2s-record-set! this idx mode return ctx)
      (with-access::J2SAssig this (lhs rhs)
	 (with-access::J2SAccess lhs (obj field loc)
	    (epairify loc
	       `(js-object-inline-set! ,(j2s-scheme obj mode return ctx)
		   ,idx
		   ,(j2s-scheme rhs mode return ctx))))))
   
   (with-access::J2SAssig this (loc lhs rhs type)
      (let loop ((lhs lhs)
		 (rhs rhs))
	 (cond
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field cache cspecs (loca loc))
		(epairify loc
		   (cond
		      ((eq? (j2s-type obj) 'vector)
		       (j2s-vector-set! this mode return ctx))
		      ((eq? (j2s-type obj) 'jsvector)
		       (j2s-jsvector-set! this mode return ctx))
		      ((and (isa? (j2s-type obj) J2SRecord)
			    (with-access::J2SAccess lhs (field loc)
			       (with-access::J2SString field (val)
				  (j2s-class-instance-get-property-index
				     (j2s-type obj) val))))
		       =>
		       (lambda (idx)
			  (j2s-record-set! this idx mode return ctx)))
		      ((and (eq? (j2s-type obj) 'array) (maybe-number? field))
		       (j2s-array-set! this mode return ctx))
		      ((and (memq (j2s-type obj) '(int8array uint8array))
			    (maybe-number? field))
		       (j2s-tarray-set! this mode return ctx))
		      ((and (mightbe-number? field) (eq? (j2s-type obj) 'any))
		       (maybe-array-set lhs rhs))
		      ((eq? (j2s-type obj) 'arguments)
		       (j2s-arguments-set! this mode return ctx))
		      (else
		       (j2s-put! loca (j2s-scheme obj mode return ctx)
			  field
			  (typeof-this obj ctx)
			  (j2s-scheme field mode return ctx)
			  (j2s-type field)
			  (j2s-scheme rhs mode return ctx)
			  (j2s-type rhs)
			  (strict-mode? mode)
			  ctx
			  cache
			  :optim #f
			  :cspecs cspecs
			  :cachefun (or (is-function? rhs)
					(is-prototype? obj))))))))
	    ((and (isa? lhs J2SRef)
		  (or (not (isa? lhs J2SThis)) (isa? rhs J2SPragma)))
	     (with-access::J2SRef lhs (decl loc)
		(with-access::J2SDecl decl (hint vtype)
		   (let ((assig (j2s-scheme-set! lhs rhs
				   (j2s-scheme rhs mode return ctx)
				   (j2s-scheme (J2SRef decl :type type) mode return ctx)
				   mode return ctx #f loc)))
		      (if (pair? assig)
			  (epairify loc assig)
			  assig)))))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id loc)
		(let ((rhse (j2s-scheme rhs mode return ctx)))
		   (epairify loc
		      (if (boxed-type? (j2s-type rhs))
			  (let ((tmp (gensym 'assigtmp)))
			     `(let ((,tmp ,rhse))
				 ,(j2s-unresolved-put!
				     (& id (context-program ctx))
				     (box tmp (j2s-type rhs) ctx)
				     #f mode return loc)
				 ,tmp))
			  (j2s-unresolved-put! (& id (context-program ctx))
			     rhse #f mode return loc))))))
	    ((isa? lhs J2SHopRef)
	     (with-access::J2SHopRef lhs (id)
		(epairify loc
		   `(set! ,id ,(j2s-scheme rhs mode return ctx)))))
	    ((isa? lhs J2SWithRef)
	     (with-access::J2SWithRef lhs (id withs expr loc)
		(epairify loc
		   (let liip ((withs withs))
		      (if (null? withs)
			  (loop expr rhs)
			  `(if ,(j2s-in? loc
				   (& id (context-program ctx)) (car withs) ctx)
			       ,(j2s-put! loc (car withs) #f 'object
				   (& id (context-program ctx)) 'propname
				   (j2s-scheme rhs mode return ctx)
				   (j2s-type rhs)
				   #f ctx #f :optim #f :cachefun #f)
			       ,(liip (cdr withs))))))))
	    ((isa? lhs J2SUndefined)
	     (if (memq mode '(strict hopscript))
		 '(js-raise-type-error %this
		    "Cannot assign to read only property 'undefined' of object"
		   (js-undefined))
		 (j2s-scheme rhs mode return ctx)))
	    ((isa? lhs J2SParen)
	     (with-access::J2SParen lhs (expr)
		(loop expr rhs)))
	    ((isa? lhs J2SCast)
	     (with-access::J2SCast lhs (expr type loc)
		(loop expr (J2SCast type rhs))))
	    ((isa? lhs J2SKontRef)
	     (with-access::J2SKontRef lhs (gen index id)
		`(js-generator-set! ,gen ,index
		    ,(j2s-scheme rhs mode return ctx)
		    ,(symbol->string! id))))
	    (else
	     (j2s-error "assignment" "Illegal assignment" this))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-postpref ...                                          */
;*    -------------------------------------------------------------    */
;*    Generic generator for prefix and postfix operations.             */
;*    -------------------------------------------------------------    */
;*    !!! x++ not equivalent to x = x + 1 as x++ always converts       */
;*    to number.                                                       */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-postpref this::J2SAssig mode return ctx op retval)
   
   (define (min-cspecs cs mincs)
      (filter (lambda (c) (memq c mincs)) cs))

   (define (need-temp? expr::J2SExpr)
      (not (or (isa? expr J2SRef)
	       (isa? expr J2SHopRef)
	       (isa? expr J2SUnresolvedRef)
	       (isa? expr J2SLiteral))))
   
   (define (new-or-old tmp val comp)
      (if (eq? retval 'new)
	  (let ((aux (gensym 'res)))
	     `(let ((,aux ,val))
		 ,(comp aux aux)))
	  (comp val tmp)))
   
   (define (coerce cast::bool lhse lhs tylhs type ctx)
      (if cast
	  (j2s-cast lhse lhs tylhs type ctx)
	  (j2s-as lhse lhs tylhs type ctx)))

   (define (tune-cast-for-inc expr)
      (match-case expr
	 ((js-tonumeric ?expr ?this)
	  `(js-tonumeric-for-fixnum ,expr ,this))
	 (else
	  expr)))
   
   (define (ref-inc op lhs::J2SRef rhs::J2SExpr type cast loc)
      (let* ((prev (when (eq? retval 'old) (gensym 'prev)))
	     (lhse (j2s-scheme lhs mode return ctx)))
	 (if (eq? retval 'old)
	     (with-access::J2SBinary rhs ((rlhs lhs))
		(set! rlhs (J2SHopRef/type prev type))
		`(let ((,prev ,(tune-cast-for-inc
				  (coerce cast lhse lhs (j2s-type lhs) type ctx))))
		    ,(j2s-scheme-set! lhs rhs
			(j2s-scheme rhs mode return ctx)
			prev mode return ctx #f loc)))
	     ;; the value has been cast during the assignment into something
	     ;; compatible with the variable type, the value of the prefix
	     ;; expression, is then simply the variable value whose vtype
	     ;; is cast into the expression type.
	     (j2s-scheme-set! lhs rhs
		(j2s-scheme rhs mode return ctx)
		(j2s-cast lhse lhs (j2s-type lhs) type ctx)
		mode return ctx #f loc))))
   
   (define (unresolved-inc lhs inc)
      (with-access::J2SUnresolvedRef lhs (id cache loc)
	 (let ((tmp (gensym 'tmp)))
	    `(let ((,tmp ,(j2s-unresolved id (or loc #t) cache loc ctx)))
		(if (fixnum? ,tmp)
		    ,(new-or-old tmp `(+fx/overflow ,tmp ,inc)
			(lambda (val tmp)
			   `(begin
			       ,(j2s-unresolved-put!
				   (& id (context-program ctx))
				   (box val (j2s-type lhs) ctx)
				   #t mode return loc)
			       ,tmp)))
		    ,(new-or-old tmp `(js+ ,tmp ,inc %this)
			(lambda (val tmp)
			   `(let ((,tmp (js-tonumber ,tmp %this)))
			       ,(j2s-unresolved-put!
				   (& id (context-program ctx))
				   (box val (j2s-type lhs) ctx)
				   #t mode return loc)
			       ,tmp))))))))
   
   (define (aput-inc-sans-cache fexpr scmlhs rhs tyobj otmp prop op lhs field::J2SExpr cache inc cs cache-missp::bool loc)
      (let ((tmp (gensym 'aput)))
	 `(let ((,tmp ,scmlhs))
	     (if (fixnum? ,tmp)
		 ,(let ((tref (instantiate::J2SHopRef
				 (loc loc)
				 (id tmp)
				 (type 'bint))))
		     (new-or-old tmp
			(js-binop2 loc '++ 'number
			   tref rhs mode return ctx)
			(lambda (val tmp)
			   `(begin
			       ,(j2s-put! loc otmp #f tyobj
				   fexpr
				   (j2s-type field)
				   val 'number
				   (strict-mode? mode) ctx
				   cache
				   :optim 'array
				   :cspecs cs :cachefun #f)
			       ,tmp))))
		 ,(let* ((tmp2 (gensym 'tmp))
			 (tref (instantiate::J2SHopRef
				  (loc loc)
				  (id tmp2)
				  (type 'number))))
		     `(let ((,tmp2 (js-tonumber ,tmp %this)))
			 ,(new-or-old tmp2
			     (js-binop2 loc '++ 'any
				tref rhs mode return ctx)
			     (lambda (val tmp)
				`(begin
				    ,(j2s-put! loc otmp #f tyobj
					fexpr
					(j2s-type field)
					val 'number
					(strict-mode? mode) ctx
					cache
					:optim 'array
					:cspecs (min-cspecs cs '(cmap))
					:cachefun #f)
				    ,tmp)))))))))

   (define (aput-inc-jsvector fexpr scmlhs rhs tyobj otmp prop op lhs field::J2SExpr inc loc)
      (let ((tmp (gensym 'aput)))
	 `(let ((,tmp ,scmlhs))
	     ,(let loop ((field field))
		 (cond
		    ((memq (j2s-type field) '(uint32 int32 int53))
		     `(if (fixnum? ,tmp)
			  ,(let ((tref (instantiate::J2SHopRef
					  (loc loc)
					  (id tmp)
					  (type 'bint))))
			      (new-or-old tmp
				 (js-binop2 loc '++ 'number
				    tref rhs mode return ctx)
				 (lambda (val tmp)
				    `(begin
					,(j2s-put! loc otmp #f tyobj
					    fexpr (j2s-type field)
					    val 'number
					    #t ctx
					    #f :optim 'array)
					,tmp))))
			  ,(let* ((tmp2 (gensym 'tmp))
				  (tref (instantiate::J2SHopRef
					   (loc loc)
					   (id tmp2)
					   (type 'number))))
			      `(let ((,tmp2 (js-tonumber ,tmp %this)))
				  ,(new-or-old tmp2
				      (js-binop2 loc '++ 'any
					 tref rhs mode return ctx)
				      (lambda (val tmp)
					 `(begin
					     ,(j2s-put! loc otmp #f tyobj
						 fexpr (j2s-type field)
						 val 'number
						 #t ctx
						 #f :optim 'array)
					     ,tmp)))))))
		    ((isa? field J2SHopRef)
		     (let ((id (j2s-scheme field mode return ctx)))
			`(if (fixnum? ,id)
			     ,(loop (duplicate::J2SHopRef field (type 'int53)))
			     (js-raise-type-error %this
				"Cannot assign property '~s' of vector" ,id))))
		    ((isa? field J2SLiteral)
		     `(js-raise-type-error %this
			 "Cannot assign property '~s' of vector"
			 ,(j2s-scheme field mode return ctx)))
		    (else
		     (let ((ptmp (gensym)))
			`(let ((,ptmp ,(j2s-scheme field mode return ctx)))
			    ,(loop (instantiate::J2SHopRef
				      (loc loc)
				      (id ptmp)
				      (type (j2s-type field))))))))))))

   (define (aput-inc-jsrecord fexpr scmlhs rhs tyobj otmp prop op lhs field::J2SExpr inc loc)
      (let ((tmp (gensym 'aput)))
	 `(let ((,tmp ,scmlhs))
	     ,(if (eq? (j2s-type lhs) '(int53))
		  (let ((tref (instantiate::J2SHopRef
				 (loc loc)
				 (id tmp)
				 (type 'bint))))
		     (new-or-old tmp
			(js-binop2 loc '++ 'number
			   tref rhs mode return ctx)
			(lambda (val tmp)
			   `(begin
			       ,(j2s-put! loc otmp #f tyobj
				   fexpr (j2s-type field)
				   val 'number
				   #t ctx
				   #f)
			       ,tmp))))
		  `(if (fixnum? ,tmp)
		       ,(let ((tref (instantiate::J2SHopRef
				       (loc loc)
				       (id tmp)
				       (type 'bint))))
			   (new-or-old tmp
			      (js-binop2 loc '++ 'number
				 tref rhs mode return ctx)
			      (lambda (val tmp)
				 `(begin
				     ,(j2s-put! loc otmp #f tyobj
					 fexpr (j2s-type field)
					 val 'number
					 #t ctx
					 #f)
				     ,tmp))))
		       ,(let* ((tmp2 (gensym 'tmp))
			       (tref (instantiate::J2SHopRef
					(loc loc)
					(id tmp2)
					(type 'number))))
			   `(let ((,tmp2 (js-tonumber ,tmp %this)))
			       ,(new-or-old tmp2
				   (js-binop2 loc '++ 'any
				      tref rhs mode return ctx)
				   (lambda (val tmp)
				      `(begin
					  ,(j2s-put! loc otmp #f tyobj
					      fexpr (j2s-type field)
					      val 'number
					      #t ctx
					      #f)
					  ,tmp))))))))))
   
   (define (aput-inc tyobj otmp prop op lhs field::J2SExpr cache inc cs cache-missp::bool)
      (with-access::J2SAccess lhs (loc obj cspecs (loca loc) type cache)
	 (let* ((oref (instantiate::J2SHopRef
			 (loc loc)
			 (id otmp)
			 (type tyobj)))
		(oacc (duplicate::J2SAccess lhs
			 (cspecs cs)
			 (obj oref)
			 (field field)))
		(rhs (instantiate::J2SNumber
			(loc loc)
			(val inc)
			(type 'int32)))
		(scmlhs (j2s-scheme oacc mode return ctx))
		(fexpr (j2s-scheme field mode return ctx)))
	    (cond
	       ((eq? tyobj 'jsvector)
		(aput-inc-jsvector fexpr scmlhs rhs tyobj otmp prop op lhs field inc loc))
	       ((isa? tyobj J2SRecord)
		(aput-inc-jsrecord fexpr scmlhs rhs tyobj otmp prop op lhs field inc loc))
	       ((type-fixnum? type)
		(let* ((tmp (gensym 'aput))
		       (tref (instantiate::J2SHopRef
				(loc loc)
				(id tmp)
				(type (j2s-type lhs)))))
		   `(let ((,tmp ,scmlhs))
		       ,(new-or-old tmp
			   (js-binop2 loc '++ 'number
			      tref rhs mode return ctx)
			   (lambda (val tmp)
			      `(begin
				  ,(j2s-put! loc otmp #f tyobj
				      fexpr
				      (j2s-type field)
				      val 'number
				      (strict-mode? mode) ctx
				      cache
				      :optim 'array :cspecs cs :cachefun #f)
				  ,tmp))))))
	       ((not cache)
		(aput-inc-sans-cache fexpr scmlhs rhs tyobj otmp prop op lhs field cache inc cs cache-missp loc))
	       (else
		(let ((tmp (gensym 'aput))
		      (els (gensym '%els))
		      (idx (gensym '%idx)))
		   `(with-access::JsObject ,otmp (cmap elements)
		       (cond
			  ((eq? cmap (js-pcache-imap (js-pcache-ref %pcache ,cache)))
			   (let* ((,idx (js-pcache-iindex (js-pcache-ref %pcache ,cache))))
			      ,(if (eq? retval 'new)
				   (let ((new (gensym '%new)))
				      `(let* ((,tmp (js-object-inline-ref ,otmp ,idx))
					      (,new (if (fixnum? ,tmp)
							(,(if (=fx inc -1) 'js-int53-dec 'js-int53-inc) ,tmp)
							(js+ ,tmp ,inc %this))))
					  (js-object-inline-set! ,otmp ,idx ,new)
					  ,new))
				   `(let ((,tmp (js-object-inline-ref ,otmp ,idx)))
				       (js-object-inline-set! ,otmp ,idx 
					  (if (fixnum? ,tmp)
					      (,(if (=fx inc -1) 'js-int53-dec 'js-int53-inc) ,tmp)
					      (js+ ,tmp ,inc %this)))
				       ,tmp))))
			  ((eq? cmap (js-pcache-cmap (js-pcache-ref %pcache ,cache)))
			   (let* ((,els elements)
;* 				  (,idx (-fx (js-pcache-cindex (js-pcache-ref %pcache ,cache)) */
;* 					   (js-object-inline-length ,otmp))) */
				  (,idx (js-pcache-cindex (js-pcache-ref %pcache ,cache))))
			      ,(if (eq? retval 'new)
				   (let ((new (gensym '%new)))
				      `(let* ((,tmp (vector-ref ,els ,idx))
					      (,new (if (fixnum? ,tmp)
							(,(if (=fx inc -1) 'js-int53-dec 'js-int53-inc) ,tmp)
							(js+ ,tmp ,inc %this))))
					  (vector-set! ,els ,idx ,new)
					  ,new))
				   `(let ((,tmp (vector-ref ,els ,idx)))
				       (vector-set! ,els ,idx 
					  (if (fixnum? ,tmp)
					      (,(if (=fx inc -1) 'js-int53-dec 'js-int53-inc) ,tmp)
					      (js+ ,tmp ,inc %this)))
				       ,tmp))))
			  (else
			   ,(aput-inc-sans-cache fexpr scmlhs rhs tyobj otmp prop op lhs field cache inc cs cache-missp loc))))))))))
   
   (define (rhs-cache rhs)
      (if (isa? rhs J2SCast)
	  (with-access::J2SCast rhs (expr)
	     (rhs-cache expr))
	  (with-access::J2SBinary rhs (lhs)
	     (when (isa? lhs J2SAccess)
		(with-access::J2SAccess lhs (cache)
		   cache)))))
   
   (define (access-inc-sans-object/field otmp::symbol prop op::symbol lhs::J2SAccess rhs::J2SExpr inc::int field::J2SExpr)
      (with-access::J2SAccess lhs (obj cspecs cache (loca loc))
	 (cond
	    ((eq? (j2s-type obj) 'array)
	     (aput-inc 'array otmp prop op lhs field cache inc '() #f))
	    ((eq? (j2s-type obj) 'jsvector)
	     (aput-inc 'jsvector otmp prop op lhs field cache inc '() #f))
	    ((isa? (j2s-type obj) J2SRecord)
	     (aput-inc (j2s-type obj) otmp prop op lhs field cache inc '() #f))
	    ((not cache)
	     (aput-inc 'object otmp prop op lhs field cache inc '() #f))
	    ((or (not cache) (memq (j2s-type field) '(integer number)))
	     (warning "js2scheme" "no cache entry should have been generated" (j2s->sexp this))
	     (aput-inc 'object otmp prop op lhs field cache inc '() #f))
	    (else
	     (aput-inc 'object otmp prop op lhs field (rhs-cache rhs) inc cspecs #t)))))
   
   (define (access-inc-sans-object otmp::symbol prop op::symbol lhs::J2SAccess rhs::J2SExpr inc::int)
      ;; compile an expression such as e1[e2]++ when e1 is either an array or an object
      (with-access::J2SAccess lhs (field)
	 (if (or (isa? field J2SRef)
		 (and (isa? field J2SLiteral) (not (isa? field J2SArray))))
	     (access-inc-sans-object/field otmp prop op lhs rhs inc field)
	     (let* ((%field (gensym '%field)))
		`(let ((,%field ,(j2s-scheme field mode return ctx)))
		    ,(access-inc-sans-object/field otmp prop op lhs rhs inc
			(with-access::J2SExpr field (loc)
			   (instantiate::J2SHopRef
			      (loc loc)
			      (id %field)
			      (type (j2s-type field))))))))))

   (define (jsvector-inc op lhs::J2SAccess rhs::J2SExpr inc::int)
      ;; compile an expression such as e1[e2]++ when e1 is a jsvector
      (let loop ((lhs lhs))
	 (with-access::J2SAccess lhs (obj field loc)
	    (cond
	       ((need-temp? obj)
		;; e1[e2]++ => tmp = e1; tmp[e2]++
		(with-access::J2SExpr obj (loc)
		   (let* ((tmp (gensym 'vput))
			  (ref (instantiate::J2SHopRef
				  (loc loc)
				  (id tmp)
				  (type (j2s-type obj)))))
		      `(let ((,tmp ,(j2s-scheme obj mode return ctx)))
			  ,(loop (duplicate::J2SAccess lhs (obj ref)))))))
	       ((need-temp? field)
		;; e1[e2]++ => tmp = e2; e1[tmp]++
		(with-access::J2SExpr obj (loc)
		   (let* ((tmp (gensym 'vfield))
			  (ref (instantiate::J2SHopRef
				  (loc loc)
				  (id tmp)
				  (type (j2s-type field)))))
		      `(let ((,tmp ,(j2s-scheme field mode return ctx)))
			  ,(loop (duplicate::J2SAccess lhs (field ref)))))))
	       ((memq (j2s-type field) '(uint32 int32 int53))
		;; e1[int]++
		(let ((tmp (gensym 'val))
		      (inc (instantiate::J2SNumber
			      (loc loc)
			      (val inc)
			      (type 'int32))))
		   `(let ((,tmp ,(j2s-scheme lhs mode return ctx)))
		       (if (fixnum? ,tmp)
			   ,(let ((ref (instantiate::J2SHopRef
					   (loc loc)
					   (id tmp)
					   (type 'bint))))
			       (new-or-old tmp
				  (js-binop2 loc '++ 'number
				     ref inc mode return ctx)
				  (lambda (val res)
				     `(begin
					 ,(j2s-put! loc (j2s-scheme obj mode return ctx)
					     #f 'jsvector
					     (j2s-scheme field mode return ctx)
					     (j2s-type field)
					     val 'number
					     #t ctx #f :optim 'array)
					 ,res))))
			   ,(let* ((tmp2 (gensym 'tmp))
				   (tref (instantiate::J2SHopRef
					    (loc loc)
					    (id tmp2)
					    (type 'number))))
			       `(let ((,tmp2 (js-tonumber ,tmp %this)))
				   ,(new-or-old tmp2
				       (js-binop2 loc '++ 'any
					  tref inc mode return ctx)
				       (lambda (val res)
					  `(begin
					      ,(j2s-put! loc (j2s-scheme obj mode return ctx)
						  #f 'jsvector
						  (j2s-scheme field mode return ctx)
						  (j2s-type field)
						  val 'number
						  #t ctx #f :optim 'array)
					      ,res)))))))))
	       (else
		(let ((scmfield (j2s-scheme field mode return ctx)))
		   (with-access::J2SExpr field (type)
		      (set! type 'int53)
		      `(if (fixnum? ,scmfield)
			   ,(loop lhs)
			   (js-raise-type-error %this
			       "Cannot assign property '~s' of vector"
			       ,(j2s-scheme field mode return ctx))))))))))
      
   (define (object-array-inc op lhs::J2SAccess rhs::J2SExpr inc::int)
      (with-access::J2SAccess lhs (obj field cspecs cache loc)
	 (let ((otmp (gensym 'obj))
	       (prop (j2s-property-scheme field mode return ctx)))
	    `(let ((,otmp ,(j2s-scheme obj mode return ctx)))
		,(if prop
		     (cond
			((type-object? (j2s-type obj))
			 (access-inc-sans-object otmp prop op lhs rhs inc))
			(else
			 `(if (js-object? ,otmp)
			      ,(with-object obj
				  (lambda ()
				     (access-inc-sans-object otmp
					prop op lhs rhs inc)))
			      ,(j2s-put! loc otmp field 'any prop 'any 1 'any
				  (strict-mode? mode) ctx cache
				  :optim 'array :cspecs '() :cachefun #f))))
		     (let* ((ptmp (gensym 'iprop))
			    (pvar (J2SHopRef ptmp)))
			`(let ((,ptmp ,(j2s-scheme field mode return ctx)))
			    ,(if (type-object? (j2s-type obj))
				 (access-inc-sans-object otmp
				    pvar op lhs rhs inc)
				 `(if (js-object? ,otmp)
				      ,(with-object obj
					  (lambda ()
					     (access-inc-sans-object otmp
						pvar op lhs rhs inc)))
				      ,(j2s-put! loc otmp field 'any pvar 'any 1 'any
					  (strict-mode? mode)
					  ctx cache
					  :optim 'array
					  :cspecs '() :cachefun #f))))))))))
   
   (define (access-inc op lhs::J2SAccess rhs::J2SExpr inc::int)
      (with-access::J2SAccess lhs (obj)
	 (if (eq? (j2s-type obj) 'jsvector)
	     (jsvector-inc op lhs rhs inc)
	     (object-array-inc op lhs rhs inc))))

   (define (kontref-inc op lhs::J2SKontRef inc::int)
      ;; compile an expression such as e1[e2]++ when e1 is a jsvector
      (with-access::J2SKontRef lhs (index gen id loc)
	 (let ((tmp (gensym 'tmp)))
	    `(let ((,tmp (js-generator-ref ,gen ,index ,(symbol->string! id))))
		(if (fixnum? ,tmp)
		    ,(new-or-old tmp `(+fx/overflow ,tmp ,inc)
			(lambda (val tmp)
			   `(begin
			       (js-generator-set! ,gen ,index
				  ,(box val (j2s-type lhs) ctx)
				  ,(symbol->string! id))
			       ,tmp)))
		    ,(new-or-old tmp `(js+ ,tmp ,inc %this)
			(lambda (val tmp)
			   `(let ((,tmp (js-tonumber ,tmp %this)))
			       (js-generator-set! ,gen ,index
				  ,(box val (j2s-type lhs) ctx)
				  ,(symbol->string! id))
			       ,tmp))))))))

   (with-access::J2SAssig this (loc lhs rhs type)
      (epairify-deep loc
	 (let loop ((lhs lhs)
		    (cast #f))
	    (cond
	       ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
		(ref-inc op lhs rhs type cast loc))
	       ((isa? lhs J2SAccess)
		(access-inc op lhs rhs (if (eq? op '++) 1 -1)))
	       ((isa? lhs J2SUnresolvedRef)
		(unresolved-inc lhs (if (eq? op '++) 1 -1)))
	       ((isa? lhs J2SParen)
		(with-access::J2SParen lhs (expr)
		   (loop expr cast)))
	       ((isa? lhs J2SCast)
		(with-access::J2SCast lhs (expr)
		   (loop expr #t)))
	       ((isa? lhs J2SKontRef)
		(kontref-inc op lhs (if (eq? op '++) 1 -1)))
	       (else
		(j2s-error "j2sscheme"
		   (format "Illegal expression \"~a\"" op)
		   this)))))))
	   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPostfix ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.3.1       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPostfix mode return ctx)
   (with-access::J2SPostfix this (op)
      (j2s-scheme-postpref this mode return ctx op 'old)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPrefix ...                                       */
;*    -------------------------------------------------------------    */
;*    www.ecma-international.org/ecma-262/5.1/#sec-11.3.1prefix        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPrefix mode return ctx)
   (with-access::J2SPrefix this (op)
      (j2s-scheme-postpref this mode return ctx op 'new)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssigOp mode return ctx)
   
   (define (aput-assigop-cache-miss otmp::symbol typeo pro prov op
	      tl::symbol lhs::J2SAccess rhs::J2SExpr field cachep ctx)
      (with-access::J2SAssigOp this ((typea type) cache)
	 (with-access::J2SAccess lhs (cspecs obj field loc (typel type))
	    (with-access::J2SExpr obj (loc)
	       (let* ((oref (instantiate::J2SHopRef
			       (loc loc)
			       (id otmp)
			       ;; MS: 16 sep 2021 (used to be (j2s-type obj))
 			       ;; (type (j2s-type obj))
			       (type typeo)))
		      (lhs (J2SCast tl
			      (duplicate::J2SAccess lhs
				 (obj oref)
				 (field (if pro
					    (J2SHopRef/type pro
					       (j2s-type field))
					    field)))))
		      (vtmp (gensym 'tmp)))
		  `(let ((,(type-ident vtmp typea (context-conf ctx))
			  ,(js-binop2 loc op typea
			      lhs rhs mode return ctx)))
		      ,(j2s-put! loc otmp field (typeof-this obj ctx)
			  (or pro prov) (j2s-type field)
			  (j2s-cast vtmp #f typea typel ctx) typel
			  (strict-mode? mode) ctx
			  (and cachep cache)
			  :optim 'array
			  :cspecs (if (mightbe-number? field) '() cspecs)
			  :cachefun #f)
		      ,vtmp))))))
   
   (define (aput-assigop otmp::symbol typeof pro prov op
	      tl::symbol lhs::J2SAccess rhs::J2SExpr field cachep ctx)
      (with-access::J2SAssigOp this ((typea type))
	 (with-access::J2SAccess lhs (cspecs obj field loc (typel type) cache)
	    (with-access::J2SExpr obj ((typeo type) loc)
	       (if (and cachep cache (not (cancall? rhs #t)))
		   (let ((els (gensym '%els))
			 (idx (gensym '%idx))
			 (tmp (gensym '%tmp))
			 (res (gensym '%res)))
		      `(with-access::JsObject ,otmp (cmap elements)
			  (cond
			     ((eq? cmap (js-pcache-imap (js-pcache-ref %pcache ,cache)))
			      (let* ((,idx (js-pcache-iindex (js-pcache-ref %pcache ,cache)))
				     (,tmp (js-object-inline-ref ,otmp ,idx))
				     (,res ,(js-binop2 loc op typea
					       (instantiate::J2SHopRef
						  (loc loc)
						  (id tmp)
						  (type tl))
					       rhs mode return ctx)))
				 (js-object-inline-set! ,otmp ,idx ,res)
				 ,res))
			     ((eq? cmap (js-pcache-cmap (js-pcache-ref %pcache ,cache)))
			      (let* ((,els elements)
;* 				     (,idx (-fx (js-pcache-cindex (js-pcache-ref %pcache ,cache)) */
;* 					      (js-object-inline-length ,otmp))) */
				     (,idx (js-pcache-cindex (js-pcache-ref %pcache ,cache)))
				     (,tmp (vector-ref ,els ,idx))
				     (,res ,(js-binop2 loc op typea
					       (instantiate::J2SHopRef
						  (loc loc)
						  (id tmp)
						  (type tl))
					       rhs mode return ctx)))
				 (vector-set! ,els ,idx ,res)
				 ,res))
			     (else
			      ,(aput-assigop-cache-miss otmp typeo pro prov op
				  tl lhs rhs field cachep ctx)))))
		   (aput-assigop-cache-miss otmp typeof pro prov op
		      tl lhs rhs field cachep ctx))))))
   
   (define (access-assigop/otmp obj otmp::symbol op lhs::J2SAccess rhs::J2SExpr)
      ;; WARNING: because of the caching of cache misses that uses the
      ;; pmap test to cache misses, pmap cannot be used in assigop.
      (with-access::J2SAccess lhs (obj field cache cspecs loc)
	 (let* ((tl (j2s-vtype lhs))
		(prov (j2s-property-scheme field mode return ctx))
		(pro (match-case prov
			((& . ?-) #f)
			(else (gensym 'aprop))))
		(cspecs-safe (remq 'pmap cspecs)))
	    `(let* (,@(if pro (list `(,pro ,prov)) '()))
		,(cond
		    ((or (not cache) (is-integer? field))
		     (aput-assigop otmp (j2s-type obj) pro prov op
			tl lhs rhs field #f ctx))
		    ((or (memq (typeof-this obj ctx) '(object this global))
			 (isa? (typeof-this obj ctx) J2SClass))
		     (aput-assigop otmp (j2s-type obj) pro prov op
			tl lhs rhs field #t ctx))
		    (else
		     `(if (js-object? ,otmp)
			  ,(with-object obj
			      (lambda ()
				 `(with-access::JsObject ,otmp (cmap)
				     ,(aput-assigop otmp 'object pro prov op
					 tl lhs rhs field #t ctx))))
			  ,(aput-assigop otmp (j2s-type obj) pro prov op
			      tl (unoptimize lhs) (unoptimize rhs)
			      field #f
			      (new-compiler-context ctx :optim 0)))))))))
   
   (define (access-assigop op lhs::J2SAccess rhs::J2SExpr)
      (with-access::J2SAccess lhs (obj field cache)
	 (let ((tmpval (j2s-scheme obj mode return ctx)))
	    (if (symbol? tmpval)
		(access-assigop/otmp obj tmpval op lhs rhs)
		(let ((otmp (gensym 'obj)))
		   `(let ((,otmp ,tmpval))
		       ,(access-assigop/otmp obj otmp op lhs rhs)))))))

   (define (kontref-assigop op lhs::J2SKontRef rhs::J2SExpr)
      (with-access::J2SKontRef lhs (index gen loc id)
	 (with-access::J2SAssigOp this (type)
	    (let ((tmp (gensym 'tmp)))
	       `(let ((,tmp ,(js-binop2 loc op type lhs rhs mode return ctx)))
		   (js-generator-set! ,gen ,index ,tmp ,(symbol->string! id))
		   ,tmp)))))

   (define (buffer-assig-concat loc type lhs rhs)
      (with-access::J2SBinary rhs ((rlhs lhs) (rrhs rhs))
	 (let ((buffer (j2s-as (j2s-scheme lhs mode return ctx) lhs
			  (j2s-type lhs) type ctx)))
	    `(begin
		,(j2s-scheme-set! lhs this
		    (js-binop2 loc '+ type lhs rlhs mode return ctx)
		    buffer
		    mode return ctx #f loc)
		,(j2s-scheme-set! lhs this
		    (js-binop2 loc '+ type lhs rrhs mode return ctx)
		    buffer
		    mode return ctx #f loc)))))
   
   (with-access::J2SAssigOp this (loc lhs rhs op type)
      (epairify-deep loc
	 (let loop ((lhs lhs))
	    (cond
	       ((isa? lhs J2SAccess)
		(access-assigop op lhs rhs))
	       ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
		(if (and (eq? op '+)
			 (eq? (j2s-type lhs) 'buffer)
			 (isa? rhs J2SBinary)
			 (with-access::J2SBinary rhs (op lhs rhs)
			    (and (eq? op '+)
				 (or (eq? (j2s-type lhs) 'string)
				     (eq? (j2s-type rhs) 'string)))))
		    ;; adding twice to the buffer is likely to be more efficient
		    (buffer-assig-concat loc type lhs rhs)
		    (j2s-scheme-set! lhs this
		       (js-binop2 loc op type lhs rhs mode return ctx)
		       (j2s-as (j2s-scheme lhs mode return ctx) lhs
			  (j2s-type lhs) type ctx)
		       mode return ctx #f loc)))
	       ((isa? lhs J2SUnresolvedRef)
		(with-access::J2SUnresolvedRef lhs (id loc)
		   (let ((rhse (js-binop2 loc op type lhs rhs mode return ctx)))
		      (epairify loc
			 (if (boxed-type? type)
			     (let ((tmp (gensym)))
				`(let ((,tmp ,rhse))
				    ,(j2s-unresolved-put!
					(& id (context-program ctx))
					(box tmp type ctx)
					#t mode return loc)
				    ,tmp))
			     (j2s-unresolved-put! (& id (context-program ctx))
				rhse #t mode return loc))))))
	       ((isa? lhs J2SCast)
		(with-access::J2SCast lhs (expr type)
		   (loop expr)))
	       ((isa? lhs J2SKontRef)
		(kontref-assigop op lhs rhs))
	       (else
		(j2s-error "j2sscheme" "Illegal assignment"
		   (j2s->sexp this))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAccess mode return ctx)
   
   (define (get loc obj tmp field cache cspecs optim)
      (let ((tyo (typeof-this obj ctx)))
	 (j2s-get loc tmp field tyo
	    (j2s-property-scheme field mode return ctx)
	    (j2s-type field) (j2s-vtype this) ctx cache
	    :optim optim
	    :cspecs cspecs)))
   
   (define (canbe-array? obj)
      (memq (j2s-type obj) '(array any undefined unknown)))
   
   (define (canbe-string? obj)
      (when (memq (j2s-type obj) '(any unknown string))
	 (if (isa? obj J2SRef)
	     (with-access::J2SRef obj (hint)
		(let ((h (assq 'string hint)))
		   (or (not (pair? h)) (>=fx (cdr h) 0))))
	     #t)))
   
   (define (hint-string? obj)
      (when (memq (j2s-type obj) '(any unknown string))
	 (with-access::J2SExpr obj (hint)
	    (let ((cs (assq 'string hint))
		  (cb (assq 'bool hint))
		  (ca (assq 'array hint)))
	       (cond
		  ((and (pair? cs) (<fx (cdr cs) 0))
		   #f)
		  ((pair? cb)
		   (if (pair? cs)
		       (and (>=fx (cdr cb) 0) (<fx (cdr cb) (cdr cs)))
		       #f))
		  ((pair? cs)
		   (when (>=fx (cdr cs) 0)
		      (if (pair? ca)
			  (=fx (cdr cs) (cdr ca))
			  #t)))
		  ((pair? ca)
		   #f)
		  (else
		   #t))))))
   
   (define (canbe-arguments? obj)
      (memq (j2s-type obj) '(any undefined unknown object)))
   
   (define (index-obj-literal-ref this obj field cache cspecs loc)
      (let ((tmp (j2s-scheme obj mode return ctx)))
	 `(cond
	     ,@(if (and (canbe-array? obj)
			(not (eq? (j2s-type field) 'string)))
		`(((js-array? ,tmp)
		   ,(or (j2s-array-ref this mode return ctx)
			(get loc obj tmp field cache cspecs #f))))
		'())
	     ,@(if (and (canbe-string? obj) (hint-string? obj)
			(hint-string? this))
		`(((js-jsstring? ,tmp)
		   ,(or (j2s-string-ref this mode return ctx)
			(get loc obj tmp field cache cspecs #f))))
		'())
	     (else
	      ,(get loc obj tmp field cache cspecs #f)))))
   
   (define (index-obj-ref this obj field cache cspecs loc)
      (if (or (isa? field J2SRef) (isa? field J2SHopRef) (isa? field J2SLiteral))
	  (index-obj-literal-ref this obj field cache cspecs loc)
	  (let* ((tmp (gensym 'tmpf))
		 (lit (J2SHopRef/type tmp (j2s-type field)))
		 (access (J2SAccess obj lit)))
	     (with-access::J2SAccess access ((ahint hint))
		(with-access::J2SAccess this (hint)
		   (set! ahint hint))
		(with-access::J2SHopRef lit (hint)
		   (set! hint ahint)))
	     `(let ((,tmp ,(j2s-scheme field mode return ctx)))
		 ,(index-obj-literal-ref access obj lit cache cspecs loc)))))
   
   (define (index-ref obj field cache cspecs loc)
      (if (or (isa? obj J2SRef) (isa? obj J2SHopRef))
	  (index-obj-ref this obj field cache cspecs loc)
	  (let* ((tmp (gensym 'tmpo))
		 (ref (J2SHopRef/type tmp (j2s-type obj)))
		 (access (J2SAccess (J2SHopRef tmp) field)))
	     (with-access::J2SAccess access ((ahint hint))
		(with-access::J2SAccess this (hint)
		   (set! ahint hint))
		(with-access::J2SHopRef ref (hint)
		   (set! hint ahint)))
	     `(let ((,tmp ,(j2s-scheme obj mode return ctx)))
		 ,(index-obj-ref access ref field cache cspecs loc)))))
   
   (define (math-object? obj ctx)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (eq? decl (context-math ctx)))))
   
   (define (regexp-object? obj ctx)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (eq? decl (context-regexp ctx)))))
   
   (define (builtin-object? obj)
      (when (isa? obj J2SGlobalRef)
	 (with-access::J2SGlobalRef obj (decl)
	    (with-access::J2SDecl decl (id)
	       (when (decl-ronly? decl)
		  (memq id '(Object Function Math Array Boolean RegExp String Number)))))))

   (define (symbol-object? obj ctx)
      (cond
	 ((isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (eq? decl (context-math ctx))))
	 ((isa? obj J2SUnresolvedRef)
	  (with-access::J2SUnresolvedRef obj (id)
	     (eq? id 'Symbol)))))
   
   (define (get-builtin-object obj field mode return ctx)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (cond
	       ((string=? val "prototype")
		`(js-function-prototype-get
		    (js-undefined)
		    ,(j2s-scheme obj mode return ctx)
		    ,(& "prototype" (context-program ctx)) %this))
	       (else
		#f)))))
   
   (define (is-object-prototype? obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (when (string=? val "prototype")
	       (is-builtin-ref? obj 'Object)))))
   
   (define (hint-optim obj field)
      ;; try go guess, checking the object hint which
      ;; specialization might be best
      (with-access::J2SExpr obj (hint)
	 (let loop ((hint hint)
		    (optim (cons 'array 0)))
	    (cond
	       ((null? hint)
		;; if the hint is object but field is "length", favor arrays
		(if (and (eq? (car optim) 'object)
			 (isa? field J2SString)
			 (with-access::J2SString field (val)
			    (string=? val "length")))
		    'array
		    (car optim)))
	       ((>=fx (cdar hint) (cdr optim))
		;; always favor array over string
		(if (and (=fx (cdar hint) (cdr optim))
			 (eq? (car optim) 'array))
		    (loop (cdr hint) optim)
		    (loop (cdr hint) (car hint))))
	       (else
		(loop (cdr hint) optim))))))

   (define (get-optional-chaining this::J2SAccess)
      ;; find the left-most optional-chaining
      (with-access::J2SAccess this (obj)
	 (cond
	    ((j2s-chaining? obj)
	     this)
	    ((isa? obj J2SAccess)
	     (get-optional-chaining obj)))))
      
   (define (optional-chaining this::J2SAccess axs)
      (let loop ((chain this))
	 (with-access::J2SAccess chain (loc obj)
	    (if (eq? chain axs)
		(let ((tmp (gensym '%tmp))
		      (unary obj))
		   (set! obj (J2SHopRef tmp))
		   (epairify loc
		      (with-access::J2SUnary unary (expr)
			 `(let ((,tmp ,(j2s-scheme expr mode return ctx)))
			     (if (js-null-or-undefined? ,tmp)
				 (js-undefined)
				 ,(j2s-scheme this mode return ctx))))))
		(loop obj)))))

   (define (record-access obj field::J2SString idx loc)
      (epairify loc
	 `(js-object-inline-ref ,(j2s-scheme obj mode return ctx) ,idx)))

   (with-access::J2SAccess this (loc obj field cache cspecs type)
      (epairify-deep loc 
	 (cond
	    ((and (isa? (j2s-type obj) J2SRecord)
		  (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (j2s-class-instance-get-property-index
			(j2s-type obj) val)))
	     =>
	     (lambda (idx)
		(record-access obj field idx loc)))
	    ((get-optional-chaining this)
	     =>
	     (lambda (axs)
		(optional-chaining this axs)))
	    ((is-object-prototype? obj field)
	     `(js-object-proto %this))
	    ((eq? (j2s-type obj) 'vector)
	     (j2s-vector-ref this mode return ctx))
	    ((eq? (j2s-type obj) 'jsvector)
	     (j2s-jsvector-ref this mode return ctx))
	    ((eq? (j2s-type obj) 'array)
	     (or (j2s-rest-ref this mode return ctx)
		 (j2s-array-ref this mode return ctx)
		 (get loc obj (j2s-scheme obj mode return ctx)
		    field cache cspecs #f)))
 	    ((eq? (j2s-type obj) 'string)
	     (or (j2s-string-ref this mode return ctx)
		 (get loc obj (j2s-scheme obj mode return ctx)
		    field cache cspecs 'string)))
	    ((eq? (j2s-type obj) 'arguments)
	     (or (j2s-arguments-ref this mode return ctx)
		 (get loc obj (j2s-scheme obj mode return ctx)
		    field cache cspecs 'arguments)))
	    ((and (eq? (j2s-type obj) 'function)
		  (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (string=? val "prototype")))
	     `(js-function-prototype-get
		 (js-undefined)
		 ,(j2s-scheme obj mode return ctx)
		 ,(& "prototype" (context-program ctx)) %this))
	    ((and (mightbe-number? field)
		  (or (mightbe-array? obj) (mightbe-string? obj)))
	     (index-ref obj field cache cspecs loc))
	    ((and (builtin-object? obj)
		  (get-builtin-object obj field mode return ctx))
	     =>
	     (lambda (sexp) sexp))
	    ((and (math-object? obj ctx)
		  (j2s-math-object-get obj field mode return ctx))
	     =>
	     (lambda (sexp) sexp))
	    ((and (regexp-object? obj ctx)
		  (j2s-regexp-object-get obj field mode return ctx))
	     =>
	     (lambda (sexp) sexp))
	    ((and (symbol-object? obj ctx)
		  (j2s-symbol-object-get obj field mode return ctx))
	     =>
	     (lambda (sexp) sexp))
	    (else
	     (get loc obj (j2s-scheme obj mode return ctx)
		field cache cspecs
		(hint-optim obj field)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCacheCheck ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCacheCheck mode return ctx)
   
   (define (record-cache-check this::J2SCacheCheck)
      (with-access::J2SCacheCheck this (prop cache owner obj fields)
	 (with-access::J2SString (car fields) (val)
	    (let* ((rec (j2s-vtype obj))
		   (el (j2s-class-find-super-element rec val)))
	       (with-access::J2SRecord rec (cmap name)
		  (when el
		     (with-access::J2SClassElement el (prop static)
			(let ((index (class-class-method-index owner el)))
			   (when (and (not static) (isa? prop J2SMethodPropertyInit))
			      (if cmap
				  `(js-record-cache-check-proto-method ,(j2s-scheme obj mode return ctx)
				      ,(j2s-scheme cmap mode return ctx)
				      ,index
				      ,(format "~a.~a" name val))
				  `(with-access::JsFunction ,(j2s-class-id owner ctx) (constrmap)
				      (with-access::JsConstructMap ocmap ((omptable mptable))
					 (with-access::JsConstructMap constrmap ((fmptable mptable))
					    (eq? (vector-ref omptable ,index)
					       (vector-ref fmptable ,index)))))))))))))))

   (define (record-cmap-cache-check this::J2SCacheCheck)
      (with-access::J2SCacheCheck this (prop cache owner obj fields)
	 (when (and (isa? (j2s-vtype obj) J2SRecord)
		    (isa? owner J2SRecord)
		    (pair? fields)
		    (isa? (car fields) J2SString)
		    (null? (cdr fields)))
	    (with-access::J2SString (car fields) (val)
	       (let ((el (j2s-class-find-super-element (j2s-vtype obj) val)))
		  (with-access::J2SRecord owner (cmap name)
		     (when el
			(with-access::J2SRef obj (%info)
			   (with-access::J2SClassElement el (prop static)
			      (let ((index (class-class-method-index owner el)))
				 (when (and (not static) (isa? prop J2SMethodPropertyInit))
				    (if cmap
					`(js-record-cmap-cache-check-proto-method ,%info
					    ,(j2s-scheme cmap mode return ctx)
					    ,index
					    ,(format "~a.~a" name val))
					`(with-access::JsFunction ,(j2s-class-id owner ctx) (constrmap)
					    (with-access::JsConstructMap ocmap ((omptable mptable))
					       (with-access::JsConstructMap constrmap ((fmptable mptable))
						  (eq? (vector-ref omptable ,index)
						     (vector-ref fmptable ,index)))))))))))))))))
   
   (define (object-cache-check this::J2SCacheCheck)
      (with-access::J2SCacheCheck this (prop cache obj fields owner)
	 `(js-object-cache-check-proto-method ,(j2s-scheme obj mode return ctx)
	     (js-pcache-ref %pcache ,cache)
	     ,(if (memq prop '(proto-method proto-method-poly))
		  ''polymorphic
		  ''monomorphic)
	     ,(with-access::J2SString (car fields) (val)
		 (if (isa? owner J2SClass)
		     (with-access::J2SClass owner (name)
			(format "~a.~a" name val))
		     val)))))

   (define (object-cmap-cache-check this::J2SCacheCheck)
      (with-access::J2SCacheCheck this (prop cache obj fields owner)
	 (with-access::J2SRef obj (%info)
	    `(js-object-cmap-cache-check-proto-method ,%info
		(js-pcache-ref %pcache ,cache)
		,(with-access::J2SString (car fields) (val)
		    (if (isa? owner J2SClass)
			(with-access::J2SClass owner (name)
			   (format "~a.~a" name val))
			val))))))
   
   (with-access::J2SCacheCheck this (prop cache obj fields owner)
      (case prop
	 ((proto-method proto-method-mono proto-method-poly)
	  (or ;(record-cache-check this)
	      (object-cache-check this)))
	 ((record-method-mono)
	  (or ;(record-cache-check this)
	      (object-cache-check this)))
	 ((record-method-poly)
	  (record-cache-check this))
	 ((cmap-proto-method)
	  (or (record-cmap-cache-check this)
	      (object-cmap-cache-check this)))
	 ((instanceof)
	  `(eq? (js-pcache-cmap (js-pcache-ref %pcache ,cache))
	      (js-object-cmap ,(j2s-scheme obj mode return ctx))))
	 ((!instanceof)
	  `(eq? (js-pcache-xmap (js-pcache-ref %pcache ,cache))
	      (js-object-cmap ,(j2s-scheme obj mode return ctx))))
	 ((method)
	  `(and (eq? (js-pcache-function (js-pcache-ref %pcache ,cache))
		   ,(j2s-scheme obj mode return ctx))))
	 ((method-and-owner)
	  `(and (eq? (js-pcache-function (js-pcache-ref %pcache ,cache))
		   ,(j2s-scheme obj mode return ctx))
		(eq? (js-pcache-pmap (js-pcache-ref %pcache ,cache))
		   (js-object-cmap ,(j2s-scheme owner mode return ctx)))))
	 (else
	  (error "j2s-scheme" "Illegal J2SCacheCheck property" prop)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCacheUpdate ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCacheUpdate mode return ctx)
   (with-access::J2SCacheUpdate this (cache obj prop)
      (case prop
	 ((proto-method-mono)
	  `(with-access::JsPropertyCache (js-pcache-ref %pcache ,cache) (imap)
	      (let ((%cmap (js-object-cmap ,(j2s-scheme obj mode return ctx))))
		 (set! imap %cmap))))
	 ((proto-method proto-method-poly)
	  `(with-access::JsPropertyCache (js-pcache-ref %pcache ,cache) (imap emap cmap pmap nmap amap xmap)
	      (when (eq? xmap (js-not-a-pmap))
		 (let ((%cmap (js-object-cmap ,(j2s-scheme obj mode return ctx))))
		    (cond
		       ((eq? imap (js-not-a-pmap)) (set! imap %cmap))
		       ((eq? emap (js-not-a-pmap)) (set! emap %cmap))
		       ((eq? cmap (js-not-a-pmap)) (set! cmap %cmap))
		       ((eq? pmap (js-not-a-pmap)) (set! pmap %cmap))
		       ((eq? nmap (js-not-a-pmap)) (set! nmap %cmap))
		       ((eq? amap (js-not-a-pmap)) (set! amap %cmap))
		       (else (set! xmap %cmap)))))))
	 ((proto-reset)
	  `(with-access::JsPropertyCache (js-pcache-ref %pcache ,cache) (pmap)
	      (set! pmap (js-not-a-pmap))))
	 ((instanceof)
	  `(with-access::JsPropertyCache (js-pcache-ref %pcache ,cache) (cmap)
	      (when (js-object-mapped? ,(j2s-scheme obj mode return ctx))
		 (set! cmap
		    (js-object-cmap
		       ,(j2s-scheme obj mode return ctx))))))
	 ((!instanceof)
	  `(with-access::JsPropertyCache (js-pcache-ref %pcache ,cache) (xmap)
	      (when (js-object-mapped? ,(j2s-scheme obj mode return ctx))
		 (set! xmap
		    (js-object-cmap
		       ,(j2s-scheme obj mode return ctx))))))
	 (else
	  (error "j2s-scheme" "Illegal J2SCacheUpdate property" prop)))))

;*---------------------------------------------------------------------*/
;*    maybe-function? ...                                              */
;*---------------------------------------------------------------------*/
(define (maybe-function? expr::J2SNode)
   (memq (j2s-type expr) '(function any)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SInit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SInit mode return ctx)
   (with-access::J2SAssig this (loc lhs rhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (epairify-deep loc
		`(begin
		    ,(j2s-scheme-set! lhs rhs
			(j2s-scheme rhs mode return ctx)
			#f mode return ctx #t loc)
		    (js-undefined))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SObjInit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SObjInit mode return ctx)
   
   (define (j2s-propname name)
      (cond
	 ((isa? name J2SString)
	  (with-access::J2SString name (val)
	     (& val (context-program ctx))))
	 ((isa? name J2SNumber)
	  (with-access::J2SNumber name (val)
	     (if (fixnum? val)
		 (& val (context-program ctx))
		 `(js-toname ,(j2s-scheme val mode return ctx) %this))))
	 ((isa? name J2SPragma)
	  `(js-toname ,(j2s-scheme name mode return ctx) %this))
	 ((isa? name J2SLiteralCnst)
	  `(js-toname ,(j2s-scheme name mode return ctx) %this))
	 ((isa? name J2SLiteralValue)
	  (with-access::J2SLiteralValue name (val)
	     `(js-toname ,(j2s-scheme val mode return ctx) %this)))
	 (else
	  `(js-toname ,(j2s-scheme name mode return ctx) %this))))
   
   (define (literal-propname name)
      (cond
	 ((isa? name J2SString)
	  (with-access::J2SString name (val)
	     (& val (context-program ctx))))
	 ((isa? name J2SNumber)
	  (with-access::J2SNumber name (val)
	     (if (fixnum? val)
		 (& val (context-program ctx))
		 `(js-toname ,(j2s-scheme val mode return ctx) %this))))
	 ((isa? name J2SLiteralCnst)
	  (with-access::J2SLiteralCnst name (val)
	     (literal-propname val)))
	 ((isa? name J2SPragma)
	  `(js-toname ,(j2s-scheme name mode return ctx) %this))
	 ((isa? name J2SLiteralCnst)
	  `(js-toname ,(j2s-scheme name mode return ctx) %this))
	 ((isa? name J2SLiteralValue)
	  (with-access::J2SLiteralValue name (val)
	     `(js-toname ,(j2s-scheme val mode return ctx) %this)))
	 (else
	  `(js-toname ,(j2s-scheme name mode return ctx) %this))))
   
   (define (is-proto? name)
      (cond
	 ((isa? name J2SString)
	  (with-access::J2SString name (val)
	     (string=? val "__proto__")))
	 ((isa? name J2SLiteralCnst)
	  (with-access::J2SLiteralCnst name (val)
	     (is-proto? val)))
	 (else
	  #f)))
   
   (define (literal->jsobj inits)
      (let ((names (gensym 'names))
	    (elements (gensym 'elements))
	    (props (map (lambda (i)
			   (with-access::J2SDataPropertyInit i (loc name)
			      (literal-propname name)))
		      inits))
	    (vals (map (lambda (i)
			  (with-access::J2SDataPropertyInit i (val)
			     (j2s-scheme-box val mode return ctx)))
		     inits)))
	 (cond
	    ((null? props)
	     '(with-access::JsGlobalObject %this (js-initial-cmap)
	       (instantiateJsObject
		  (cmap js-initial-cmap)
		  (__proto__ (js-object-proto %this))
		  (elements ($create-vector 4)))))
	    ((every (match-lambda ((& (? string?) . ?-) #t) (else #f)) props)
	     `(let ((,names (vector ,@props))
		    (,elements (vector ,@vals)))
		 (js-literal->jsobject ,elements ,names %this)))
	    (else
	     (let ((len (length props)))
		`(let ((,names (cond-expand
				  (bigloo-c ($create-vector ,len))
				  (else (make-vector ,len))))
		       (,elements (cond-expand
				     (bigloo-c ($create-vector ,len))
				     (else (make-vector ,len)))))
		    ,@(append-map (lambda (idx name val)
				     `((vector-set! ,names ,idx ,name)
				       (vector-set! ,elements ,idx ,val)))
			 (iota len) props vals)
		    (js-literal->jsobject ,elements ,names %this)))))))

   (define (hashtable? this)
      (cond
	 ((isa? this J2SObjInit)
	  (with-access::J2SObjInit this (inits)
	     (every (lambda (prop)
		       (when (isa? prop J2SDataPropertyInit)
			  (with-access::J2SDataPropertyInit prop (name val)
			     (when (isa? name J2SString)
				(hashtable? val)))))
		inits)))
	 ((isa? this J2SArray)
	  (with-access::J2SArray this (exprs)
	     (every hashtable? exprs)))
	 ((isa? this J2SString)
	  #t)
	 ((isa? this J2SNumber)
	  #t)
	 ((isa? this J2SBool)
	  #t)
	 (else
	  #f)))

   (define (j2s-objinit-large this)
      (cond
	 ((isa? this J2SObjInit)
	  (with-access::J2SObjInit this (inits)
	     (map (lambda (prop)
		     (when (isa? prop J2SDataPropertyInit)
			(with-access::J2SDataPropertyInit prop (name val)
			   (with-access::J2SString name ((name val))
			      (cons name (j2s-objinit-large val))))))
		inits)))
	 ((isa? this J2SArray)
	  (with-access::J2SArray this (exprs)
	     (list->vector (map j2s-objinit-large exprs))))
	 ((isa? this J2SString)
	  (with-access::J2SString this (val)
	     val))
	 ((isa? this J2SNumber)
	  (with-access::J2SNumber this (val)
	     val))
	 ((isa? this J2SBool)
	  (with-access::J2SBool this (val)
	     val))
	 (else
	  '(js-undefined))))
   
   (define (large-literal->jsobj inits)
      (if (hashtable? this)
	  `(js-large-literal->jsobject ',(j2s-objinit-large this) %this)
	  (literal->jsobj inits)))
   
   (define (cmap->jsobj inits cmap)
      (let ((vals (map (lambda (i)
			  (with-access::J2SDataPropertyInit i (val)
			     (j2s-scheme-box val mode return ctx)))
		     inits)))
	 (cond
	    ((any (lambda (i)
		     (with-access::J2SDataPropertyInit i (val)
			(maybe-function? (uncast val))))
		inits)
	     (let ((ctorcmap (gensym)))
		`(let ((,ctorcmap ,(j2s-scheme cmap mode return ctx)))
		    (js-object-literal-init!
		       (instantiateJsObject
			  (cmap ,ctorcmap)
			  (__proto__ (js-object-proto %this))
			  (elements (subvector (with-access::JsConstructMap ,ctorcmap (ctor)
						  (if (cell? ctor)
						      (cell-ref ctor)
						      ,(length vals)))
				       ,@vals)))))))
	    ((null? vals)
	     (let ((omap (gensym 'cmap)))
		`(let ((,omap ,(j2s-scheme cmap mode return ctx)))
		    (with-access::JsConstructMap ,omap ((constrsize ctor))
		       (instantiateJsObject
			  (cmap ,omap)
			  (__proto__ (js-object-proto %this))
			  (elements (make-vector (cell-ref constrsize))))))))
	    (else
	     (let ((omap (gensym 'cmap)))
		`(let ((,omap ,(j2s-scheme cmap mode return ctx)))
		    (with-access::JsConstructMap ,omap ((constrsize ctor))
		       (instantiateJsObject
			  (cmap ,omap)
			  (__proto__ (js-object-proto %this))
			  (elements (subvector (cell-ref constrsize) ,@vals))))))))))
   
   (define (new->jsobj loc inits)
      (let ((tmp (gensym)))
	 `(with-access::JsGlobalObject %this (js-object)
	     (let ((,tmp ,(j2s-new loc 'js-object '())))
		,@(map (lambda (i)
			  (cond
			     ((isa? i J2SDataPropertyInit)
			      (with-access::J2SDataPropertyInit i (loc name val)
				 (cond
				    ((is-proto? name)
				     ;; __proto__ field is special during
				     ;; initialization, it must be assigned
				     ;; using the generic js-put! function
				     (j2s-put! loc tmp #f 'obj
					(& "__proto__" (context-program ctx))
					'propname
					(j2s-scheme val mode return ctx)
					(j2s-type val)
					(strict-mode? mode) ctx #f
					:optim #f
					:cachefun (is-function? val)))
				    ((isa? name J2SUndefined)
				     (with-access::J2SDataPropertyInit i (val)
					(with-access::J2SSpread val (expr)
					   `(js-object-literal-spread-assign! ,tmp
					       ,(j2s-scheme expr mode return ctx)
					       %this))))
				    (else
				     (epairify loc
					`(js-bind! %this ,tmp
					    ,(j2s-propname name)
					    :value ,(j2s-scheme val mode return ctx)
					    :writable #t
					    :enumerable #t
					    :configurable #t))))))
			     (else
			      (with-access::J2SAccessorPropertyInit i (loc name get set)
				 (epairify loc
				    `(js-bind! %this ,tmp
					,(j2s-propname name)
					:get ,(j2s-scheme get mode return ctx)
					:set ,(j2s-scheme set mode return ctx)
					:writable #t
					:enumerable #t
					:configurable #t))))))
		     inits)
		,tmp))))
   
   (with-access::J2SObjInit this (loc inits cmap)
      (epairify loc
	 (cond
	    (cmap
	     (cmap->jsobj inits cmap))
	    ((every (lambda (i)
		       (when (isa? i J2SDataPropertyInit)
			  (with-access::J2SDataPropertyInit i (name)
			     (and (not (is-proto? name))
				  (not (isa? name J2SUndefined))))))
		inits)
	     (if (>=fx (length inits) (context-get ctx :max-objinit-optim-size))
		 (large-literal->jsobj inits)
		 (literal->jsobj inits)))
	    (else
	     (new->jsobj loc inits))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDataPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDataPropertyInit mode return ctx)
   (with-access::J2SDataPropertyInit this (loc name val)
      (epairify loc
	 `(,(j2s-scheme name mode return ctx)
	   ,(j2s-scheme val mode return ctx)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNew ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNew mode return ctx)

   (define (old-new-builtin? clazz builtin)
      (cond
	 ((isa? clazz J2SUnresolvedRef)
	  (with-access::J2SUnresolvedRef clazz (id)
	     (eq? id builtin)))
	 ((isa? clazz J2SRef)
	  (with-access::J2SRef clazz (decl)
	     (when (isa? decl J2SDeclExtern)
		(with-access::J2SDeclExtern decl (id)
		   (when (and (eq? id builtin)
			      (not (decl-usage-has? decl '(assig)))))))))))
   
   (define (new-builtin? clazz builtin)
      (cond
	 ((is-builtin-ref? clazz builtin)
	  #t)
	 ((old-new-builtin? clazz builtin)
	  #f)
	 (else
	  #f)))

   (define (new-array? clazz)
      (new-builtin? clazz 'Array))

   (define (new-jsvector? clazz)
      (new-builtin? clazz 'Vector))

   (define (new-proxy? clazz)
      (new-builtin? clazz 'Proxy))

   (define (new-date? clazz)
      (new-builtin? clazz 'Date))

   (define (new-regexp? clazz)
      (new-builtin? clazz 'RegExp))

   (define (new-int8array? clazz)
      (new-builtin? clazz 'Int8Array))

   (define (new-uint8array? clazz)
      (new-builtin? clazz 'Uint8Array))

   (define (new-typeerror? clazz)
      (new-builtin? clazz 'TypeError))

   (define (new-record? clazz)
      (when (isa? clazz J2SRef)
	 (with-access::J2SRef clazz (decl)
	    (when (isa? decl J2SDeclClass)
	       (with-access::J2SDeclClass decl (val)
		  (when (isa? val J2SRecord)
		     val))))))

   (define (new-class? clazz)
      (when (isa? clazz J2SRef)
	 (with-access::J2SRef clazz (decl)
	    (when (isa? decl J2SDeclClass)
	       (with-access::J2SDeclClass decl (id val)
		  (when (isa? val J2SClass)
		     val))))))

   (define (constructor-no-call? decl)
      ;; does this constructor call another function?
      (let ((fun (j2sdeclinit-val-fun decl)))
	 (when (isa? fun J2SFun)
	    (with-access::J2SFun fun (body)
	       (not (cancall? body #f))))))

   (define (object-alloc clazz::J2SRef fun)
      (with-access::J2SRef clazz (decl loc)
	 (if (and (isa? decl J2SDeclFun)
		  (with-access::J2SDecl decl (scope)
		     (eq? scope '%scope)))
	     (if (cancall? decl #f)
		 `(js-object-alloc %this ,fun)
		 `(js-object-alloc-fast %this ,fun))
	     `(js-object-alloc %this ,fun))))
      
   (define (j2s-new-fast clazz args)
      (with-access::J2SRef clazz (decl loc)
	 (let* ((len (length args))
		(fun (j2s-scheme clazz mode return ctx))
		(fid (j2s-decl-fast-id decl ctx))
		(obj (gensym '%obj)))
	    `(let ((,obj ,(object-alloc clazz fun)))
		,(if (constructor-no-return? decl)
		     `(begin
			 (,fid ,obj ,@args)
			 ,(if (constructor-no-call? decl)
			      obj
			      `(js-new-return-fast ,fun ,obj)))
		     `(js-new-return ,fun (,fid ,obj ,@args) ,obj))))))

   (define (j2s-new-opt/args decl clazz args)
      (let ((fid (with-access::J2SDeclFun decl (id)
		    (j2s-fast-constructor-id id)))
	    (fun (j2s-scheme clazz mode return ctx))
	    (obj (gensym '%obj)))
	 (if (constructor-no-return? decl)
	     (if (constructor-no-call? decl)
		 `(,fid ,@args)
		 `(let ((,obj (,fid ,@args)))
		     (js-new-return-fast ,fun ,obj)))
	     `(let ((,obj (,fid ,@args)))
		 (js-new-return ,fun ,obj ,obj)))))
   
   (define (j2s-new-opt decl::J2SDeclFun clazz::J2SExpr args)
      (with-access::J2SDeclFun decl (val)
	 (with-access::J2SFun val (params)
	    (let* ((largs (length args))
		   (lparams (length params)))
	       (cond
		  ((>fx largs lparams)
		   (let ((tmps (map (lambda (a) (gensym '%a)) args)))
		      `(let* ,(map (lambda (tmp arg) `(,tmp ,arg)) tmps args)
			  ,(j2s-new-opt/args decl clazz (take tmps lparams)))))
		  ((=fx largs lparams)
		   (j2s-new-opt/args decl clazz args))
		  (else
		   (j2s-new-opt/args decl clazz
		      (append args
			 (make-list (-fx lparams largs) #unspecified)))))))))

   (define (j2s-new-proxy this mode return ctx)
      (with-access::J2SNew this (caches args)
	 `(,(if (pair? caches) 'js-new-proxy/caches 'js-new-proxy)
	   %this
	   ,@(map (lambda (a)
		       (j2s-scheme a mode return ctx))
		  args)
	     ,@(map (lambda (c)
		       `(js-pcache-ref %pcache ,c))
		  caches))))
	     
   (with-access::J2SNew this (loc caches clazz args type)
      (cond
	 ((any (lambda (n) (isa? n J2SSpread)) args)
	  (epairify loc
	     `(apply js-new %this ,(j2s-scheme clazz mode return ctx)
		 ,(j2s-spread->expr-list args mode return ctx))))
	 ((and (new-array? clazz)
	       (or (=fx (bigloo-debug) 0) (eq? type 'vector)))
	  (epairify loc
	     (j2s-new-array this mode return ctx)))
	 ((and (new-jsvector? clazz)
	       (or (=fx (bigloo-debug) 0) (eq? type 'vector)))
	  (epairify loc
	     (j2s-new-jsvector this mode return ctx)))
	 ((and (=fx (length args) 1)
	       (or (new-int8array? clazz)
		   (new-uint8array? clazz)))
	  (epairify loc
	     (j2s-new-tarray this mode return ctx)))
	 ((and (new-typeerror? clazz)
	       (pair? args)
	       (<=fx (length args) 3))
	  (epairify loc
	     (case (length args)
		((1)
		 `(js-type-error1
		     ,(j2s-scheme-box (car args) mode return ctx)
		     %this))
		((2)
		 `(js-type-error2
		     ,(j2s-scheme-box (car args) mode return ctx)
		     ,(j2s-scheme-box (cadr args) mode return ctx)
		     %this))
		(else
		 `(js-type-error
		     ,(j2s-scheme-box (car args) mode return ctx)
		     ,(j2s-scheme-box (cadr args) mode return ctx)
		     ,(j2s-scheme-box (caddr args) mode return ctx)
		     %this)))))
	 ((and (new-proxy? clazz) (=fx (length args) 2))
	  (epairify loc
	     (j2s-new-proxy this mode return ctx)))
	 ((and (new-date? clazz) (j2s-new-date this mode return ctx))
	  =>
	  (lambda (sexp)
	     (epairify loc sexp)))
	 ((and (new-regexp? clazz) (j2s-new-regexp this mode return ctx))
	  =>
	  (lambda (sexp)
	     (epairify loc sexp)))
	 ((optimized-ctor clazz ctx)
	  =>
	  (lambda (decl)
	     (epairify loc
		(j2s-new-opt decl clazz
		   ;; 8sep2021, seems wrong to box the arguments
		   ;; of optimized constructors
		   ;; (map (lambda (a) (j2s-scheme-box a mode return ctx))
		   (map (lambda (a) (j2s-scheme a mode return ctx))
		      args)))))
	 ((and (=fx (bigloo-debug) 0) (pair? caches)
	       (with-access::J2SRef clazz (decl loc)
		  (not (isa? decl J2SDeclExtern))))
	  (epairify loc
	     (j2s-new-fast clazz
		(map (lambda (a)
			(j2s-scheme-box a mode return ctx))
		   args))))
	 ((new-record? clazz)
	  =>
	  (lambda (rec)
	     (epairify loc
		(j2s-scheme-record-new this rec args mode return ctx))))
	 ((new-class? clazz)
	  =>
	  (lambda (clazz)
	     (epairify loc
		(j2s-scheme-class-new this clazz args mode return ctx))))
	 (else
	  (epairify loc
	     (j2s-new loc (j2s-scheme-box clazz mode return ctx)
		(map (lambda (a)
			(j2s-scheme-box a mode return ctx))
		   args)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SReturnYield ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SReturnYield mode return ctx)
   
   (define (identity-kont? kont)
      (or (not (isa? kont J2SKont))
	  (with-access::J2SKont kont (body param)
	     (when (isa? body J2SStmtExpr)
		(with-access::J2SStmtExpr body (expr)
		   (when (isa? expr J2SRef)
		      (with-access::J2SRef expr (decl)
			 (eq? decl param))))))))
   
    (with-access::J2SReturnYield this (loc expr kont generator)
       (epairify loc
	  `(,(if generator 'js-generator-yield* 'js-generator-yield)
	    %gen
	    %yield
	    ,(j2s-scheme expr mode return ctx)
	    ,(isa? kont J2SUndefined)
	    ,(if (identity-kont? kont)
		 'js-generator-done
		 (j2s-scheme kont mode return ctx))
	    %this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SKont ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SKont mode return ctx)
   (with-access::J2SKont this (loc param exn body)
      (epairify loc
	 `(lambda (,(j2s-scheme param mode return ctx)
		   ,(j2s-scheme exn mode return ctx)
		   %gen
		   %yield
		   %this)
	     ,(j2s-scheme body mode return ctx)))))

;*---------------------------------------------------------------------*/
;*    concat-tilde ...                                                 */
;*---------------------------------------------------------------------*/
(define (concat-tilde lst)
   (cond
      ((null? lst)
       '())
      ((isa? (car lst) J2SNode)
       (concat-tilde (cdr lst)))
      ((not (string? (car lst)))
       (cons (car lst) (concat-tilde (cdr lst))))
      (else
       (let loop ((prev lst)
		  (cursor (cdr lst)))
	  (cond
	     ((null? cursor)
	      (list (apply string-append lst)))
	     ((string? (car cursor))
	      (loop cursor (cdr cursor)))
	     ((isa? (car cursor) J2SNode)
	      (set-cdr! prev '())
	      (cons (apply string-append lst) (concat-tilde (cdr cursor))))
	     (else
	      (set-cdr! prev '())
	      (cons* (apply string-append lst)
		 (car cursor)
		 (concat-tilde (cdr cursor)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2STilde ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2STilde mode return ctx)
   (with-access::J2STilde this (loc stmt)
      (let* ((nctx (new-compiler-context ctx :site 'tilde))
	     (js-stmt (concat-tilde (j2s-js stmt #t #f mode return nctx)))
	     (js (cond
		    ((null? js-stmt)
		     "")
		    ((null? (cdr js-stmt))
		     (car js-stmt))
		    ((every string? js-stmt)
		     (apply string-append js-stmt))
		    (else
		     `(string-append ,@js-stmt))))
	     (expr (j2s-tilde->expression this mode return ctx)))
	 (epairify loc
	    `(instantiate::xml-tilde
		(lang 'javascript)
		(debug #f)
		(%js-expression ,expr)
		(body (vector
			 ',(if (>fx (bigloo-debug) 1) (j2s->sexp stmt) '())
			 '() '() '() ,js #f))
		(loc ',loc))))))

;*---------------------------------------------------------------------*/
;*    j2s-tilde->expression ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-tilde->expression this::J2STilde mode return ctx)
   (with-access::J2STilde this (loc stmt)
      (let* ((temp (gensym 'tilde))
	     (assign (j2s-stmt-assign stmt temp))
	     (js-stmt (concat-tilde (j2s-js assign #t #f mode return ctx)))
	     (str (cond
		     ((null? js-stmt)
		      "")
		     ((null? (cdr js-stmt))
		      (car js-stmt))
		     ((every string? js-stmt)
		      (apply string-append js-stmt))
		     (else
		      `(string-append ,@js-stmt)))))
	 (if (string? str)
	     (format "(function() { var ~a; ~a\nreturn ~a; }).call(this)" temp str temp)
	     `(string-append
		 ,(format "(function() { var ~a; " temp)
		 ,str
		 ,(format "\nreturn ~a; }).call(this)" temp))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDollar ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDollar mode return ctx)
   (with-access::J2SDollar this (loc)
      (match-case loc
	 ((at ?fname ?loc)
	  (error/location "hopscript" "Illegal $ expression" this
	     fname loc))
	 (else
	  (j2s-error "hopscript" "Illegal $ expression" this)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SOPTInitSeq ...                                   */
;*    -------------------------------------------------------------    */
;*    Optimized constructor initialization sequence.                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SOPTInitSeq mode return ctx)
   
   (define (init-expr node k)
      ;; see ctor.scm
      (let loop ((stmt node))
	 (with-access::J2SStmtExpr stmt (expr)
	    (cond
	       ((isa? expr J2SAssig)
		(with-access::J2SAssig expr (rhs)
		   (k rhs)))
	       ((isa? expr J2SBindExit)
		(with-access::J2SBindExit expr (stmt)
		   (if (isa? stmt J2SLetBlock)
		       (with-access::J2SLetBlock stmt (nodes)
			  (duplicate::J2SLetBlock stmt
			     (nodes (cons (loop (car nodes)) (cdr nodes)))))
		       (error "j2s-scheme" "wrong init expr"
			  (j2s->sexp node)))))
	       (else
		(error "j2s-scheme" "wrong init expr"
		   (j2s->sexp node)))))))
   
   (define (vector-inits n elements i offset nodes cmap)
      `(let* ((,elements (js-object-inline-elements ,n))
	      (,i ,offset))
	  ,@(map (lambda (init offset)
		    (j2s-scheme 
		       (init-expr init
			  (lambda (e)
			     `(vector-set! ,elements (+fx ,i ,offset)
				 ,(j2s-as (j2s-scheme e mode return ctx)
				     e (j2s-type e) 'any ctx))))
		       mode return ctx))
	       nodes (iota (length nodes)))
	  (set! cmap ,cmap)))
   
   (define (elements-init-sans-cmap nodes)
      `(begin
	  ,@(map (lambda (n)
		    (j2s-scheme n mode return ctx))
	       nodes)))

   (define (node-cache node)
      (with-access::J2SStmtExpr node (expr)
	 (with-access::J2SAssig expr (lhs)
	    (with-access::J2SAccess lhs (cache)
	       cache))))
   
   (define (elements-init n offset nodes %cmap cnt pcache)
      `(with-access::JsConstructMap cmap (props)
	  (let ((%cmap0 cmap))
	     ,(elements-init-sans-cmap nodes)
	     (when (<fx ,cnt 1000)
		(set! ,cnt (+fx ,cnt 1))
		(with-access::JsConstructMap cmap (props)
		   (when (and (js-object-no-setter? ,n)
			      (not (eq? (js-pcache-emap
					   (js-pcache-ref %pcache
					      ,(node-cache
						  (car (last-pair nodes)))))
				      (js-not-a-pmap)))
			      (not (eq? (js-pcache-emap
					   (js-pcache-ref %pcache
					      ,(node-cache
						  (car nodes))))
				      (js-not-a-pmap)))
			      (=fx ,(-fx (length nodes) 1)
				 (-fx (with-access::JsConstructMap
					    (js-pcache-emap
					       (js-pcache-ref %pcache
						  ,(node-cache
						      (car (last-pair nodes)))))
					    (props)
					 (vector-length props))
				    (with-access::JsConstructMap
					  (js-pcache-emap
					     (js-pcache-ref %pcache
						,(node-cache (car nodes))))
					    (props)
					 (vector-length props)))))
		      (set! ,offset
			 (js-pcache-eindex
			    (js-pcache-ref %pcache ,(node-cache (car nodes)))))
		      (set! ,%cmap cmap)
		      (js-validate-pmap-pcache! (js-pcache-ref %pcache ,pcache))
		      (with-access::JsPropertyCache (js-pcache-ref %pcache ,pcache) (nmap)
			 (set! nmap %cmap0))))))))
   
   (define (init-ref this n)
      (with-access::J2SOPTInitSeq this (loc ref nodes cmap offset cnt cache)
	 (let ((i (gensym '%i))
	       (elements (gensym '%elements)))
	    (if cmap
		`(with-access::JsObject ,n (cmap)
		    (if (eq? cmap (js-pcache-nmap (js-pcache-ref %pcache ,cache)))
			,(vector-inits n elements i offset nodes cmap)
			,(elements-init n offset nodes cmap cnt cache)))
		(elements-init-sans-cmap nodes)))))
   
   (with-access::J2SOPTInitSeq this (loc ref nodes cmap offset cnt cache)
      (cond
	 ((not (context-get ctx :optim-initseq))
	  (elements-init-sans-cmap nodes))
	 ((isa? ref J2SRef)
	  (init-ref this (j2s-scheme ref mode return ctx)))
	 (else
	  (let ((%ref (gensym '%ref)))
	     `(let ((,%ref ,(j2s-scheme ref mode return ctx)))
		 ,(init-ref this ref)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDProducer ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDProducer mode return ctx)
   (with-access::J2SDProducer this (expr size)
      (let ((sexpr (j2s-scheme expr mode return ctx)))
	 (cond
	    ((=fx size -1)
	     sexpr)
	    ((eq? (j2s-type expr) 'array)
	     sexpr)
	    (else
	     `(js-iterator-to-array ,sexpr ,size %this))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDConsumer ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDConsumer mode return ctx)
   (with-access::J2SDConsumer this (expr)
      (j2s-scheme expr mode return ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SImport ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SImport mode return ctx)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SImportDynamic ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SImportDynamic mode return ctx)
   (with-access::J2SImportDynamic this (loc path base loc)
      (epairify loc
	 `(nodejs-import-module-dynamic %worker %this %module
	     ,(j2s-scheme path mode return ctx)
	     ,base
	     ,(context-get ctx :commonjs-export)
	     ',loc))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SImportNamespace ...                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SImportNamespace mode return ctx)
   (with-access::J2SImportNamespace this (loc import)
      (with-access::J2SImport import (ipath)
	 (with-access::J2SImportPath ipath (path)
	    (epairify loc
	       `(nodejs-module-namespace ,(importpath-var ipath)
		   %worker %this))))))

;*---------------------------------------------------------------------*/
;*    throw? ...                                                       */
;*---------------------------------------------------------------------*/
(define (throw? node)
   (let ((cell (make-cell #f)))
      (canthrow node '() cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    canthrow ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (canthrow this::J2SNode stack cell)
   (or (cell-ref cell) (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    canthrow ::J2SThrow ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (canthrow this::J2SThrow stack cell)
   (cell-set! cell #t))

;*---------------------------------------------------------------------*/
;*    canthrow ::J2SAccess ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (canthrow this::J2SAccess stack cell)
   (cell-set! cell #t))

;*---------------------------------------------------------------------*/
;*    canthrow ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (canthrow this::J2SCall stack cell)
   (with-access::J2SCall this (fun args)
      (if (not (isa? fun J2SRef))
	  (cell-set! cell #t)
	  (with-access::J2SRef fun (decl)
	     (for-each (lambda (a) (canthrow a stack cell)) args)
	     (unless (cell-ref cell)
		(cond
		   ((not (isa? decl J2SDeclFun))
		    (cell-set! cell #t))
		   ((not (memq decl stack))
		    (with-access::J2SDeclFun decl (val)
		       (with-access::J2SFun val (body)
			  (canthrow val (cons decl stack) cell))))))))))
	  
;*---------------------------------------------------------------------*/
;*    with-object ...                                                  */
;*---------------------------------------------------------------------*/
(define (with-object expr::J2SExpr thunk)
   (with-access::J2SExpr expr (type)
      (let ((otype type))
	 (if (isa? expr J2SRef)
	     (with-access::J2SRef expr (decl)
		(with-access::J2SDecl decl (vtype)
		   (if (decl-ronly? decl)
		       (let ((ovtype vtype))
			  (set! vtype 'object)
			  (set! type 'object)
			  (unwind-protect
			     (thunk)
			     (begin
				(set! vtype ovtype)
				(set! type otype))))
		       (thunk))))
	     (unwind-protect
		(thunk)
		(set! type otype))))))

;*---------------------------------------------------------------------*/
;*    is-lambda? ...                                                   */
;*---------------------------------------------------------------------*/
(define (is-lambda? val type)
   ;; val is a scheme expression, we have to do a little bit of
   ;; pattern matching to find it's a function or not
   (when (eq? type 'function)
      (match-case val
	 ((let ?- (js-make-function-strict-lazy . ?-)) #t)
	 ((let ?- (js-make-function-strict . ?-)) #t)
	 ((let ?- (js-make-function . ?-)) #t)
	 (else #f))))

;*---------------------------------------------------------------------*/
;*    is-prototype? ...                                                */
;*---------------------------------------------------------------------*/
(define (is-prototype? obj)
   (when (isa? obj J2SAccess)
      (with-access::J2SAccess obj (field)
	 (when (isa? field J2SString)
	    (with-access::J2SString field (val)
	       (string=? val "prototype"))))))

;*---------------------------------------------------------------------*/
;*    is-function? ...                                                 */
;*---------------------------------------------------------------------*/
(define (is-function? val::J2SExpr)
   (or (isa? val J2SFun)
       (isa? val J2SMethod)
       (and (isa? val J2SRef)
	    (with-access::J2SRef val (decl)
	       (isa? decl J2SDeclFun)))))

;*---------------------------------------------------------------------*/
;*    unoptimize ...                                                   */
;*---------------------------------------------------------------------*/
(define (unoptimize expr)
   (let ((nexpr (j2s-alpha expr '() '())))
      (uncache! nexpr)))

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SAccess ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SAccess)
   (with-access::J2SAccess this (cache)
      (set! cache #f)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SPrefix ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SPrefix)
   (with-access::J2SPrefix this (cache)
      (set! cache #f)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SPostfix ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SPostfix)
   (with-access::J2SPostfix this (cache)
      (set! cache #f)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SAssigOp ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SAssigOp)
   (with-access::J2SAssigOp this (cache)
      (set! cache #f)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    if-check-cache-cascade-tests ...                                 */
;*    -------------------------------------------------------------    */
;*    Returns the list of all the tests of a cachecheck cascade.       */
;*    -------------------------------------------------------------    */
;*    The purpose of this analysis is to transform a cascade:          */
;*                                                                     */
;*    (if (js-class-check-cache-method obj c0 ...)                     */
;*        ...                                                          */
;*        (if (js-class-check-cache-method obj c1 ...)                 */
;*            ...                                                      */
;*    	      (if (js-class-check-cache-method obj c1 ...)             */
;*    	          ...)))                                               */
;*                                                                     */
;*    into:                                                            */
;*                                                                     */
;*    (let ((cmap (js-object-cmap obj)))                               */
;*       (if (js-class-cmap-check-cache-method cmap c0 ...)            */
;*           ...                                                       */
;*           (if (js-class-cmap-check-cache-method cmap c1 ...)        */
;*               ...                                                   */
;*    	         (if (js-class-cmap-check-cache-method cmap c1 ...)    */
;*    	             ...))))                                           */
;*---------------------------------------------------------------------*/
(define (if-check-cache-cascade-tests::pair-nil this::J2SIf)
   (with-access::J2SIf this (test else)
      (if (isa? test J2SCacheCheck)
	  (with-access::J2SCacheCheck test ((o0 obj) prop)
	     (if (and (isa? o0 J2SRef) (eq? prop 'proto-method))
		 (with-access::J2SRef o0 ((d0 decl))
		    (let loop ((else else)
			       (ts (list test)))
		       (if (isa? else J2SIf)
			   (with-access::J2SIf else (test else)
			      (if (isa? test J2SCacheCheck)
				  (with-access::J2SCacheCheck test ((o1 obj) prop)
				     (if (and (isa? o1 J2SRef) (eq? prop 'proto-method))
					 (with-access::J2SRef o1 ((d1 decl))
					    (if (eq? d0 d1)
						(loop else
						   (cons test ts))
						ts))
					 ts))
				  ts))
			   ts)))
		 '()))
	  '())))

