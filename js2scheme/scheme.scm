;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 11:47:51 2013                          */
;*    Last change :  Sat Jan 27 09:00:15 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Generate a Scheme program from out of the J2S AST.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_array
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-program
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-ops
	   __js2scheme_scheme-test
	   __js2scheme_scheme-class
	   __js2scheme_scheme-string
	   __js2scheme_scheme-regexp
	   __js2scheme_scheme-math
	   __js2scheme_scheme-date
	   __js2scheme_scheme-array)
   
   (export j2s-scheme-stage
	   j2s-scheme-eval-stage
	   (generic j2s-scheme ::obj ::symbol ::procedure ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    builtin-method ...                                               */
;*---------------------------------------------------------------------*/
(define-struct builtin-method jsname met ttype args %this)

;*---------------------------------------------------------------------*/
;*    j2s-builtin-methods ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-builtin-methods
   ;; jsname, scmname|procedure, (this.types), [optional-args] %this
   (map (lambda (e)
	   (apply builtin-method e))
      `(;; string methods
	("fromCharCode" js-jsstring-fromcharcode String (any) %this)
	("charAt" js-jsstring-charat string (any) %this)
	("charAt" js-jsstring-maybe-charat any (any) %this)
	("charCodeAt" ,j2s-jsstring-charcodeat string (any) %this)
	("charCodeAt" js-jsstring-maybe-charcodeat any (any) %this)
	("indexOf" js-jsstring-indexof string (string (any 0)) %this)
	("indexOf" js-jsstring-maybe-indexof any (any (any 0)) %this)
	("lastIndexOf" js-jsstring-lastindexof string (string (any +nan.0)) %this)
	("lastIndexOf" js-jsstring-maybe-lastindexof string (any (any +nan.0)) %this)
	("substring" js-jsstring-substring string (any any) %this)
	("substring" js-jsstring-maybe-substring any (any any) %this)
	("substr" js-jsstring-substr string (any any) %this)
	("substr" js-jsstring-maybe-substr any (any any) %this)
	("toUpperCase" js-jsstring-touppercase string () #f)
	("toUpperCase" js-jsstring-maybe-touppercase any () %this)
	("toLocaleUpperCase" js-jsstring-tolocaleuppercase string () #f)
	("toLocaleUpperCase" js-jsstring-maybe-tolocaleuppercase any () %this)
	("toLowerCase" js-jsstring-tolowercase string () #f)
	("toLowerCase" js-jsstring-maybe-tolowercase any () %this)
	("toLocaleLowerCase" js-jsstring-tolocalelowercase string () #f)
	("toLocaleLowerCase" js-jsstring-maybe-tolocalelowercase any () %this)
	("split" js-jsstring-split string (string (any (js-undefined))) %this)
	("split" js-jsstring-maybe-split any (any (any (js-undefined))) %this)
	("replace" ,j2s-jsstring-replace-regexp string (regexp any) %this)
	("replace" ,j2s-jsstring-replace-string string (string any) %this)
	("replace" ,j2s-jsstring-replace string (any any) %this)
	("replace" js-jsstring-maybe-replace any (any any) %this)
	("match" js-jsstring-match string (any) %this)
	("match" js-jsstring-maybe-match any (any) %this)
	("naturalCompare" js-jsstring-naturalcompare string (string) %this)
	("naturalCompare" js-jsstring-maybe-naturalcompare any (any) %this)
	("localeCompare" js-jsstring-localecompare string (string) %this)
	("localeCompare" js-jsstring-maybe-localecompare any (any) %this)
	("trim" js-jsstring-trim string () #f)
	("trim" js-jsstring-maybe-trim any () %this)
	("slice" js-jsstring-slice string (any any) %this)
	("slice" js-jsstring-maybe-slice any (any any) %this)
	;; regexp
	("test" ,j2s-regexp-test regexp (any) %this)
	;; array methods
;* 	("concat" js-array-concat array (any) %this)                   */
;* 	("concat" js-array-maybe-concat any (any) %this)               */
	("push" js-array-push array (any) %this)
	("push" js-array-maybe-push any (any) %this)
	("pop" js-array-pop array () %this)
	("pop" js-array-maybe-pop any () %this)
;* 	("slice" js-array-slice array () %this)                        */
;* 	("slice" js-array-maybe-slice any () %this)                    */
	("fill" js-array-fill array (any (any 0) (any #unspecified)) %this)
	("fill" js-array-maybe-fill any (any (any 0) (any #unspecified)) %this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-scheme-stage
   (instantiate::J2SStageProc
      (name "scheme")
      (comment "Scheme code generation")
      (proc (lambda (ast conf)
	       (j2s-scheme ast 'normal comp-return
		  (append conf
		     (list :%vectors '())
		     `(:debug-client ,(bigloo-debug)))
		  '())))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-eval-stage ...                                        */
;*---------------------------------------------------------------------*/
(define j2s-scheme-eval-stage
   (instantiate::J2SStageProc
      (name "scheme")
      (comment "Scheme code generation (eval)")
      (proc (lambda (ast conf)
	       (j2s-scheme ast 'normal (lambda (x) x)
		  (append conf
		     (list :%vectors '())
		     `(:debug-client ,(bigloo-debug)))
		  '())))))

;*---------------------------------------------------------------------*/
;*    comp-return ...                                                  */
;*---------------------------------------------------------------------*/
(define (comp-return x)
   (match-case x
      ((begin . ?rest)
       `(begin ,@(filter pair? rest)))
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
;*    eval-return ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.9          */
;*---------------------------------------------------------------------*/
(define-macro (eval-return type value target)
   `(if return ,value ,value))

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
;*    j2s-toobject ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-toobject loc arg)
   (if (> (bigloo-debug) 0)
       `(js-toobject/debug %this ',loc ,arg)
       `(js-toobject %this ,arg)))

;*---------------------------------------------------------------------*/
;*    j2s-nodes* ...                                                   */
;*    -------------------------------------------------------------    */
;*    Compile a list of nodes, returns a list of expressions.          */
;*---------------------------------------------------------------------*/
(define (j2s-nodes*::pair-nil loc nodes mode return conf hint)
   
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
	  (let ((sexp (j2s-scheme (car nodes) mode return conf hint)))
	     (match-case sexp
		((begin . (and (? pair?) ?sexps))
		 sexps)
		(else
		 (epairify loc 
		    (list (return sexp)))))))
	 ((undefined? (car nodes))
	  (loop (cdr nodes)))
	 (else
	  (let ((sexp (j2s-scheme (car nodes) mode return conf hint)))
	     (match-case sexp
		((begin . ?sexps)
		 (epairify loc
		    (append (remove-undefined sexps) (loop (cdr nodes)))))
		(else
		 (epairify loc
		    (cons sexp (loop (cdr nodes)))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::obj ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (j2s-scheme this mode return::procedure conf hint)
   (if (pair? this)
       (map (lambda (e) (j2s-scheme e mode return conf hint)) this)
       this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SVarDecls ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SVarDecls mode return conf hint)
   (illegal-node "j2s-scheme" this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-decl ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-decl this::J2SDecl value writable mode return conf)
   (with-access::J2SDecl this (loc scope id utype ronly)
      (let ((ident (j2s-decl-scheme-id this)))
	 (epairify-deep loc
	    (cond
	       ((memq scope '(global %scope))
		(let ((fun-name (format "function:~a:~a"
				   (cadr loc) (caddr loc))))
		   (if (and (not (isa? this J2SDeclExtern)) (in-eval? return))
		       `(js-decl-eval-put! %scope
			   ',id ,value ,(strict-mode? mode) %this)
		       (if (js-need-global? this scope mode)
			   `(begin
			       (define ,ident ,value)
			       (js-define %this ,scope ',id
				  (lambda (%) ,ident)
				  (lambda (% %v) (set! ,ident %v))
				  %source
				  ,(caddr loc)))
			   `(define ,ident ,value)))))
	       ((memq scope '(letblock letvar))
		(if ronly
		    `(,(utype-ident ident utype conf) ,value)
		    `(,ident ,value)))
	       (else
		`(define ,ident ,value)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDecl mode return conf hint)
   
   (define (j2s-scheme-param this)
      (with-access::J2SDecl this (utype)
	 (utype-ident (j2s-decl-scheme-id this) utype conf)))
   
   (define (j2s-scheme-var this)
      (with-access::J2SDecl this (loc id writable)
	 (j2s-scheme-decl this '(js-undefined) writable mode return conf)))
   
   (define (j2s-scheme-let this)
      (with-access::J2SDecl this (loc scope id utype ronly)
	 (epairify loc
	    (if (memq scope '(global))
		`(define ,(j2s-decl-scheme-id this) (js-make-let))
		(let ((var (j2s-decl-scheme-id this)))
		   `(,var (js-make-let)))))))

   (cond
      ((j2s-let? this)
       (j2s-scheme-let this))
      ((j2s-param? this)
       (j2s-scheme-param this))
      (else
       (j2s-scheme-var this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclInit mode return conf hint)
   
   (define (j2s-scheme-var this)
      (with-access::J2SDeclInit this (loc val writable)
	 (let ((ident (j2s-decl-scheme-id this)))
	    (epairify loc
	       (if writable
		   `(begin
		       (set! ,ident ,(j2s-scheme val mode return conf hint))
		       (js-undefined))
		   `(begin
		       ,(j2s-scheme val mode return conf hint)
		       (js-undefined)))))))
   
   (define (j2s-scheme-let-opt this)
      (with-access::J2SDeclInit this (scope id)
	 (if (memq scope '(global %scope))
	     (j2s-let-decl-toplevel this mode return conf)
	     (error "js-scheme" "Should not be here (not global)"
		(j2s->list this)))))
   
   (cond
      ((j2s-param? this) (call-next-method))
      ((j2s-let-opt? this) (j2s-scheme-let-opt this))
      ((j2s-let? this) (call-next-method))
      (else (j2s-scheme-var this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-set! lhs::J2SRef val tyval::symbol result mode return conf init? loc)
   
   (define (set decl hint utype loc)
      (cond
	 ((not (and (j2s-let? decl) (not (j2s-let-opt? decl))))
	  `(set! ,(j2s-scheme lhs mode return conf hint) ,val))
	 (init?
	  `(set! ,(j2s-decl-scheme-id decl) ,val))
	 (else
	  `(js-let-set! ,(j2s-decl-scheme-id decl) ,val ',loc %this))))

   (with-access::J2SRef lhs (decl)
      (with-access::J2SDecl decl (writable immutable scope id hint utype)
	 (cond
	    ((or writable (and (isa? decl J2SDeclInit) (not immutable)))
	     (cond
		((and (memq scope '(global %scope)) (in-eval? return))
		 `(begin
		     ,(j2s-put! loc '%scope (j2s-vtype lhs)
			 `',id 'propname
			 val tyval (strict-mode? mode) conf #f)
		     ,result))
		(result
		 `(begin
		     ,(set decl hint utype loc)
		     ,result))
		(else
		 (set decl hint utype loc))))
	    ((and immutable (memq mode '(strict hopscript)))
	     `(with-access::JsGlobalObject %this (js-type-error)
		 ,(match-case loc
		     ((at ?fname ?pos)
		      `(js-raise
			  (js-new %this js-type-error
			     ,(j2s-jsstring
				 "Assignment to constant variable."
				 loc)
			     ,fname ,pos)))
		     (else
		      `(js-raise
			  (js-new %this js-type-error
			     ,(j2s-jsstring
				 "Assignment to constant variable."
				 loc)))))))
	    (else
	     val)))))
	      
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclExtern ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclExtern mode return conf hint)
   (with-access::J2SDeclExtern this (loc id name val bind writable)
      (cond
	 (bind
          (j2s-scheme-decl this (j2s-scheme val mode return conf hint)
	     writable mode return conf))
	 (else
	  (j2s-scheme val mode return conf hint)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCast ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCast mode return conf hint)
   (with-access::J2SCast this (expr type)
      (cond
	 ((isa? expr J2SBinary)
	  (or (j2s-scheme-binary-as expr mode return conf hint type)
	      (j2s-cast (j2s-scheme expr mode return conf hint)
		 expr (j2s-vtype expr) type conf)))
	 ((isa? expr J2SUnary)
	  (or (j2s-scheme-unary-as expr mode return conf hint type)
	      (j2s-cast (j2s-scheme expr mode return conf hint)
		 expr (j2s-vtype expr) type conf)))
	 (else
	  (j2s-cast (j2s-scheme expr mode return conf hint)
	     expr (j2s-vtype expr) type conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRef mode return conf hint)
   (with-access::J2SRef this (decl loc type)
      (with-access::J2SDecl decl (scope id vtype)
	 (cond
	    ((j2s-let-opt? decl)
	     (j2s-decl-scheme-id decl))
	    ((j2s-let? decl)
	     (epairify loc
		`(js-let-ref ,(j2s-decl-scheme-id decl) ',id ',loc %this)))
	    ((and (memq scope '(global %scope)) (in-eval? return))
	     (epairify loc
		`(js-global-object-get-name %scope ',id #f %this)))
	    (else
	     (j2s-decl-scheme-id decl))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSuper ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSuper mode return conf hint)
   (with-access::J2SSuper this (decl loc clazz)
      (if (eq? clazz '__proto__)
	  `(js-super ,(call-next-method) ',loc %this)
	  '%super)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWithRef ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWithRef mode return conf hint)
   (with-access::J2SWithRef this (id withs expr loc)
      (epairify loc
	 (let loop ((withs withs))
	    (if (null? withs)
		(j2s-scheme expr mode return conf hint)
		`(if ,(j2s-in? loc `',id (car withs))
		     ,(j2s-get loc (car withs) 'object `',id 'string 'any conf #f)
		     ,(loop (cdr withs))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SHopRef ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SHopRef mode return conf hint)
   (with-access::J2SHopRef this (id)
      id))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SThis ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SThis mode return conf hint)
   (with-access::J2SThis this (loc type decl)
      (let ((id (j2s-decl-scheme-id decl)))
	 (if (and (j2s-let? decl) (not (j2s-let-opt? decl)))
	     `(js-let-ref ,id ,id ',loc %this)
	     id))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCond ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCond mode return conf hint)
   (with-access::J2SCond this (loc test then else)
      (epairify loc
	 `(if ,(j2s-test test mode return conf)
	      ,(j2s-scheme then mode return conf hint)
	      ,(j2s-scheme else mode return conf hint)))))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved-put! ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-unresolved-put! field expr throw::bool mode::symbol return)
   ;; no need to type check obj as we statically know that it is an obj
   (cond
      ((and (in-eval? return)
	    (not (eq? j2s-unresolved-put-workspace
		    j2s-unresolved-get-workspace)))
       `(js-unresolved-eval-put! %scope ,field
	   ,expr ,(strict-mode? mode) %this))
      ((strict-mode? mode)
       `(js-unresolved-put! ,j2s-unresolved-put-workspace ,field
	   ,expr #t %this))
      (else
       `(js-put! ,j2s-unresolved-put-workspace ,field ,expr ,throw %this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUnresolvedRef ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUnresolvedRef mode return conf hint)
   (with-access::J2SUnresolvedRef this (loc cache id)
      (epairify loc
	 (j2s-unresolved id cache loc))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SArrayAbsent ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArrayAbsent mode return conf hint)
   (with-access::J2SArrayAbsent this (loc)
      (epairify loc '(js-absent))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLiteralValue ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLiteralValue mode return conf hint)
   (with-access::J2SLiteralValue this (val type)
      (cond
	 ((number? val) (error "j2s-scheme" "should not find a number here" val))
	 (else val))))

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
(define-method (j2s-scheme this::J2SNumber mode return conf hint)
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
	     ((m64? conf)
	      val)
	     ((and (>=fx val (negfx (bit-lsh 1 29))) (<fx val (bit-lsh 1 29)))
	      val)
	     (else
	      (fixnum->flonum val))))
	 ((not (flonum? val))
	  (error "j2s-scheme ::J2SNumber"
	     (format "bad number type ~a/~a" type (typeof val))
	     (j2s->list this)))
	 ((and (flonum? val) (nanfl? val))
	  "NaN")
	 (else
	  (cond
	     ((flonum? val) val)
	     ((uint32? val) (int32->flonum val))
	     ((uint32? val) (uint32->flonum val))
	     (else (fixnum->flonum val)))))))

;*---------------------------------------------------------------------*/
;*    j2s-property-scheme ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-property-scheme this::J2SExpr mode return conf)
   (cond
      ((isa? this J2SLiteralCnst)
       (with-access::J2SLiteralCnst this (val)
	  (with-access::J2SLiteralValue val (val)
	     val)))
      ((isa? this J2SString)
       (with-access::J2SString this (val)
	  (if (string-minimal-charset val)
	      val
	      (j2s-scheme this mode return conf '()))))
      (else
       (j2s-scheme this mode return conf '()))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLiteralCnst ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLiteralCnst mode return conf hint)
   (with-access::J2SLiteralCnst this (index val)
      (if (isa? val J2SRegExp)
	  ;; regexp are hybrid, the rx part is precompiled but the
	  ;; JS object is dynamically allocated
 	  `(let ((rx::JsRegExp (vector-ref-ur %cnsts ,index)))
	      (let ((nrx::JsRegExp (duplicate::JsRegExp rx)))
		 (js-object-mode-set! nrx (js-object-default-mode))
		 (js-object-properties-set! nrx
		    (list-copy (js-object-properties rx)))
		 nrx))
	  `(vector-ref-ur %cnsts ,index))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2STemplate ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2STemplate mode return conf hint)
   (with-access::J2STemplate this (loc exprs)
      (epairify loc
	 `(js-stringlist->jsstring
	     (list
		,@(map (lambda (expr)
			  (if (isa? expr J2SString)
			      (with-access::J2SString expr (val)
				 val)
			      (with-access::J2SNode expr (loc)
				 (epairify loc
				    `(js-tostring
					,(j2s-scheme expr mode return conf hint)
					%this)))))
		     exprs))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNativeString ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNativeString mode return conf hint)
   (with-access::J2SNativeString this (loc val)
      val))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SString ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SString mode return conf hint)
   (with-access::J2SString this (loc val)
      (j2s-jsstring val loc)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRegExp ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRegExp mode return conf hint)
   (with-access::J2SRegExp this (loc val flags)
      (epairify loc
	 `(with-access::JsGlobalObject %this (js-regexp)
	     ,(j2s-new loc 'js-regexp
		 (if (string-null? flags)
		     (list (j2s-jsstring val loc))
		     (list (j2s-jsstring val loc)
			(j2s-jsstring flags loc))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCmap ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCmap mode return conf hint)
   (with-access::J2SCmap this (loc val)
      (epairify loc
	 `(js-names->cmap ',val))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNull ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNull mode return conf hint)
   (with-access::J2SLiteral this (loc)
      (epairify loc '(js-null))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUndefined ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUndefined mode return conf hint)
   (with-access::J2SLiteral this (loc)
      (epairify loc '(js-undefined))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SReturn mode return conf hint)
   (with-access::J2SReturn this (loc expr tail exit from)
      (cond
	 (exit
	  (epairify loc
	     `(%jsexit ,(j2s-scheme expr mode return conf hint))))
	 (tail
	  (j2s-scheme expr mode return conf hint))
	 ((isa? from J2SBindExit)
	  (with-access::J2SBindExit from (lbl)
	     (epairify loc
		`(,lbl
		    ,(j2s-scheme expr mode return conf hint)))))
	 (else
	  (epairify loc
	     `(%return
		 ,(j2s-scheme expr mode return conf hint)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBindExit ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBindExit mode return conf hint)
   (with-access::J2SBindExit this (lbl stmt loc)
      (if lbl
	  (epairify loc
	     `(bind-exit (,lbl)
		 ,(j2s-scheme stmt mode return conf hint)))
	  (j2s-scheme stmt mode return conf hint))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SThrow ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SThrow mode return conf hint)
   (with-access::J2SThrow this (loc expr)
      (epairify loc
	 (if (> (bigloo-debug) 0)
	     `(js-throw/debug ,(j2s-scheme expr mode return conf hint)
		 ,(j2s-jsstring (cadr loc) loc) ,(caddr loc) %worker)
	     `(js-throw ,(j2s-scheme expr mode return conf hint)
		 ,(j2s-jsstring (cadr loc) loc) ,(caddr loc))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2STry ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2STry mode return conf hint)
   (with-access::J2STry this (loc body catch finally)
      (epairify-deep loc
	 (let* ((trybody (j2s-scheme body mode return conf hint))
		(trie (if (isa? catch J2SNop)
			  (j2s-scheme body mode return conf hint)
			  (with-access::J2SCatch catch (loc param body)
			     (epairify-deep loc
				`(with-handler
				    (lambda (,(j2s-scheme param mode return conf hint))
				       ,(j2s-scheme body mode return conf hint))
				    ,trybody))))))
	    (if (isa? finally J2SNop)
		trie
		`(unwind-protect
		    ,trie
		    ,(j2s-scheme finally mode return conf hint)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWith ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWith mode return conf hint)
   (with-access::J2SWith this (obj block id)
      `(let ((,id (js-toobject %this ,(j2s-scheme obj mode return conf hint))))
	  ,(j2s-scheme block mode return conf hint))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPragma ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPragma mode return conf hint)
   (with-access::J2SPragma this (loc expr lang)
      (if (eq? lang 'scheme)
	  (epairify-deep loc expr)
  "#unspecified")))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSequence mode return conf hint)
   (with-access::J2SSequence this (loc exprs)
      (let loop ((exprs exprs))
	 (cond
	    ((null? (cdr exprs))
	     (j2s-scheme (car exprs) mode return conf hint))
	    ((isa? (car exprs) J2SUndefined)
	     (loop (cdr exprs)))
	    (else
	     (epairify loc
		(flatten-begin
		   (j2s-scheme exprs mode return conf hint))))))))

;*---------------------------------------------------------------------*/
;*    j2s-let-decl-toplevel ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-let-decl-toplevel::pair-nil d::J2SDeclInit mode return conf)
   (with-access::J2SDeclInit d (val usage id hint scope loc)
      (let ((ident (j2s-decl-scheme-id d)))
	 (cond
	    ((or (not (isa? val J2SFun))
		 (isa? val J2SSvc)
		 (memq 'assig usage))
	     `(define ,ident ,(j2s-scheme val mode return conf hint)))
	    ((usage? '(ref get new set) usage)
	     (let ((fun (jsfun->lambda val mode return conf
			   `(js-get ,ident 'prototype %this) #f))
		   (tmp (j2s-fast-id id)))
		`(begin
		    (define ,tmp ,fun)
		    (define ,ident
		       ,(j2sfun->scheme val tmp tmp mode return conf))
		    ,@(if (memq 'eval usage)
			  `((js-define %this ,scope ',id
			       (lambda (%) ,ident)
			       (lambda (% %v) (set! ,ident %v))
			       %source
			       ,(caddr loc)))
			  '()))))
	    ((memq 'call usage)
	     `(define ,(j2s-fast-id id)
		 ,(jsfun->lambda val mode return conf
		     `(js-get ,(j2s-fast-id id) 'prototype %this) #f)))
	    (else
	     '())))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLetBlock ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLetBlock mode return conf hint)
   
   (define (j2s-let-decl-inner::pair-nil d::J2SDecl mode return conf singledecl
	      typed)
      (with-access::J2SDeclInit d (val usage id vtype ronly utype)
	 (let* ((ident (j2s-decl-scheme-id d))
		(var (type-ident ident vtype)))
	    (cond
	       ((or (not (isa? val J2SFun))
		    (isa? val J2SSvc)
		    (memq 'assig usage))
		(list `(,var ,(j2s-scheme val mode return conf hint))))
	       ((usage? '(ref get new set) usage)
		(let ((fun (jsfun->lambda val mode return conf
			      `(js-get ,ident 'prototype %this) #f))
		      (tmp (j2s-fast-id id)))
		   `((,tmp ,fun)
		     (,var ,(j2sfun->scheme val tmp tmp mode return conf)))))
	       ((memq 'call usage)
		`((,(j2s-fast-id id)
		   ,(jsfun->lambda val mode return conf (j2s-fun-prototype val) #f))))
	       (else
		'())))))
   
   (with-access::J2SLetBlock this (loc decls nodes rec)
      (cond
	 ((null? decls)
	  (epairify loc
	     `(begin ,@(j2s-nodes* loc nodes mode return conf hint))))
	 ((any (lambda (decl::J2SDecl)
		  (with-access::J2SDecl decl (scope)
		     (memq scope '(global))))
	     decls)
	  ;; top-level or function level block
	  (epairify loc
	     `(begin
		 ,@(map (lambda (d)
			   (cond
			      ((j2s-let-opt? d)
			       (j2s-let-decl-toplevel d mode return conf))
			      ((isa? d J2SDeclFun)
			       (with-access::J2SDeclFun d (scope)
				  (set! scope 'global))
			       (j2s-scheme d mode return conf hint))
			      (else
			       (with-access::J2SDecl d (scope)
				  (set! scope 'global))
			       (j2s-scheme d mode return conf hint))))
		      decls)
		 ,@(j2s-scheme nodes mode return conf hint))))
	 (else
	  ;; inner letblock, create a let block
	  (let ((ds (append-map (lambda (d)
				   (cond
				      ((j2s-let-opt? d)
				       (j2s-let-decl-inner d
					  mode return conf
					  (null? (cdr decls))
					  (not rec)))
				      ((isa? d J2SDeclFun)
				       (j2s-scheme d mode return conf hint))
				      (else
				       (list (j2s-scheme d mode return conf hint)))))
		       decls))
		(body (j2s-nodes* loc nodes mode return conf hint)))
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
(define-method (j2s-scheme this::J2SParen mode return conf hint)
   (with-access::J2SParen this (expr type)
      (j2s-scheme expr mode return conf (list type))))

;*---------------------------------------------------------------------*/
;*    j2s-stmt-sans-begin ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-stmt-sans-begin::pair this::J2SStmt mode return conf hint)
   (let ((sexp (j2s-scheme this mode return conf hint)))
      (match-case sexp
	 ((begin . ?sexps) sexps)
	 (else (list sexp)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SStmt ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12           */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SStmt mode return conf hint)
   (return this))

;*---------------------------------------------------------------------*/
;*    j2s-sequence ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-sequence loc nodes::pair-nil mode return conf hint)
   
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
	  (j2s-scheme (car nodes) mode return conf hint))
	 ((undefined? (car nodes))
	  (loop (cdr nodes)))
	 (else
	  (epairify loc
	     (flatten-begin
		(j2s-scheme nodes mode return conf hint)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SMeta ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SMeta mode return conf hint)
   (with-access::J2SMeta this (stmt optim)
      (if (=fx optim 0)
	  `(%%noinline
	      ,(j2s-scheme stmt mode return (cons* :optim 0 conf) hint))
	  (j2s-scheme stmt mode return conf hint))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSeq ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.1         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSeq mode return conf hint)
   (with-access::J2SSeq this (loc nodes)
      (j2s-sequence loc nodes mode return conf hint)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBlock ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBlock mode return conf hint)

   (define (begin-or-let loc bindings nodes)
      (if (null? bindings)
	  (j2s-sequence loc nodes mode return conf hint)
	  (epairify loc
	     `(let ,(reverse! bindings)
		 ,@(j2s-nodes* loc nodes mode return conf hint)))))
   
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
			  (cons (j2s-scheme (car nodes) mode return conf hint)
			     bindings)))
		    (begin-or-let loc bindings nodes))))
	    (else
	     (begin-or-let loc bindings nodes))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNop ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.3         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNop mode return conf hint)
   (with-access::J2SNop this (loc)
      (epairify loc
	 (return '(js-undefined)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SStmtExpr ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.4         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SStmtExpr mode return conf hint)
   (with-access::J2SStmtExpr this (expr)
      (if (isa? expr J2SIf)
	  (j2s-scheme expr mode return conf hint)
	  (return (j2s-scheme expr mode return conf hint)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SIf ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.5         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SIf mode return conf hint)
   (with-access::J2SIf this (loc test then else)
      (epairify loc
	 (if (isa? else J2SNop)
	     `(if ,(j2s-test test mode return conf)
		  ,(j2s-scheme then mode return conf hint)
		  (js-undefined))
	     `(if ,(j2s-test test mode return conf)
		  ,(j2s-scheme then mode return conf hint)
		  ,(j2s-scheme else mode return conf hint))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPrecache ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPrecache mode return conf hint)

   (define (precache-access this::J2SAccess)
      (with-access::J2SAccess this (obj field cache)
	 (let* ((scmobj (j2s-scheme obj mode return conf hint))
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
	      ,(j2s-scheme then mode return conf hint)
	      ,(j2s-scheme else mode return conf hint)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDo ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.1       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDo mode return conf hint)
   (with-access::J2SDo this (loc test body id
			       need-bind-exit-break need-bind-exit-continue)

      (define (comp-loop loop)
	 `(let ,loop ()
	       ,(if need-bind-exit-continue
		    (epairify-deep loc
		       `(bind-exit (,(escape-name '%continue id))
			   ,@(j2s-stmt-sans-begin body mode return conf hint)))
		    (j2s-scheme body mode return conf hint))
	       (if ,(j2s-test test mode return conf)
		   (,loop)
		   (js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       ,(if need-bind-exit-continue
		    (epairify-deep loc
		       `(bind-exit (,(escape-name '%continue id))
			   ,@(j2s-stmt-sans-begin body mode acc-return conf hint)))
		    (j2s-scheme body mode acc-return conf hint))
	       (if ,(j2s-test test mode return conf)
		   (,loop %acc)
		   %acc)))
      
      (let* ((doid (gensym 'do))
	     (loop (if (in-eval? return) (eval-loop doid) (comp-loop doid))))
	 (epairify-deep loc
	    (if need-bind-exit-break
		(epairify-deep loc `(bind-exit (,(escape-name '%break id)) ,loop))
		(epairify loc loop))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWhile ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.2       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWhile mode return conf hint)
   (with-access::J2SWhile this (loc test body id
				  need-bind-exit-break need-bind-exit-continue)

      (define (comp-loop loop)
	 `(let ,loop ()
	       (if ,(j2s-test test mode return conf)
		   ,(if need-bind-exit-continue
			(epairify-deep loc
			   `(begin
			       (bind-exit (,(escape-name '%continue id))
				  ,@(j2s-stmt-sans-begin body mode return conf hint))
			       (,loop)))
			(epairify-deep loc
			   `(begin
			       ,@(j2s-stmt-sans-begin body mode return conf hint)
			       (,loop))))
		   (js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       (if ,(j2s-test test mode return conf)
		   ,(if need-bind-exit-continue
			(epairify-deep loc
			   `(begin
			       (bind-exit (,(escape-name '%continue id))
				  ,@(j2s-stmt-sans-begin body mode acc-return conf hint))
			       (,loop %acc)))
			(epairify-deep loc
			   `(begin
			       ,@(j2s-stmt-sans-begin body mode acc-return conf hint)
			       (,loop %acc))))
		   %acc)))
      
      (let* ((whileid (gensym 'while))
	     (loop (if (in-eval? return) (eval-loop whileid) (comp-loop whileid))))
	 (epairify-deep loc
	    (if need-bind-exit-break
		(epairify-deep loc `(bind-exit (,(escape-name '%break id)) ,loop))
		(epairify loc loop))))))

;*---------------------------------------------------------------------*/
;*    escape-name ...                                                  */
;*---------------------------------------------------------------------*/
(define (escape-name escape id)
   (if (symbol? id)
       (symbol-append escape '- id)
       escape))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SFor ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.3       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SFor mode return conf hint)
   (with-access::J2SFor this (loc init test incr body id
				need-bind-exit-break
				need-bind-exit-continue
				need-bind-exit-continue-label)
      
      (define (comp-loop loop)
	 `(let ,loop ()
	       (if ,(j2s-test test mode return conf)
		   (begin
		      ,(if need-bind-exit-continue
			   (epairify-deep loc
			      `(bind-exit (,(escape-name '%continue id))
				  ,(j2s-scheme body mode return conf hint)))
			   (j2s-scheme body mode return conf hint))
		      ,(j2s-scheme incr mode return conf '(void))
		      (,loop))
		   (js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       (if ,(j2s-test test mode return conf)
		   (begin
		      ,(if need-bind-exit-continue
			   (epairify-deep loc
			      `(bind-exit (,(escape-name '%continue id))
				  ,(j2s-scheme body mode acc-return conf hint)))
			   (j2s-scheme body mode acc-return conf hint))
		      ,(j2s-scheme incr mode return conf '(void))
		      (,loop %acc))
		   %acc)))

      (let* ((forid (gensym 'for))
	     (loop (if (in-eval? return) (eval-loop forid) (comp-loop forid))))
	 (epairify-deep loc
	    `(begin
		,@(if (isa? init J2SNop)
		      '()
		      (list (j2s-scheme init mode return conf hint)))
		,(if need-bind-exit-break
		     (epairify-deep loc
			`(bind-exit (,(escape-name '%break id)) ,loop))
		     (epairify loc loop)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SForIn ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.4       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SForIn mode return conf hint)

   (define (js-for-in op)
      (if (eq? op 'in) 'js-for-in 'js-for-of))

   (define (close op close)
      (cond
	 ((eq? op 'in) '())
	 (close '(#t))
	 (else '(#f))))
   
   (define (for-in/break-comp tmp name props obj body set op)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(let ((%acc (js-undefined)))
			(,(js-for-in op) ,(j2s-scheme obj mode return conf hint)
			   (lambda (,name)
			      ,set
			      ,(if need-bind-exit-continue
				   `(bind-exit (,(escape-name '%continue id))
				       ,(j2s-scheme body mode acc-return conf hint))
				   (j2s-scheme body mode acc-return conf hint)))
			   ,@(close op #t)
			   %this)
			%acc)))
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,for)
		for))))

   (define (for-in/break-eval tmp name props obj body set op)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(,(js-for-in op) ,(j2s-scheme obj mode return conf hint)
			(lambda (,name)
			   ,set
			   ,(if need-bind-exit-continue
				`(bind-exit (,(escape-name '%continue id))
				    ,(j2s-scheme body mode return conf hint))
				(j2s-scheme body mode return conf hint)))
			,@(close op #t)
			%this)))
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,for)
		for))))

   (define (for-in/break tmp name props obj body set op)
      (if (in-eval? return)
	  (for-in/break-eval tmp name props obj body set op)
	  (for-in/break-comp tmp name props obj body set op)))

   (define (for-in/w-break-comp tmp name props obj body set op)
      `(,(js-for-in op) ,(j2s-scheme obj mode return conf hint)
	  (lambda (,name)
	     ,set
	     ,(j2s-scheme body mode return conf hint))
	  ,@(close op (throw? body))
	  %this))

   (define (for-in/w-break-eval tmp name props obj body set op)
      `(let ((%acc (js-undefined)))
	  (,(js-for-in op) ,(j2s-scheme obj mode return conf hint)
	     (lambda (,name)
		,set
		,(j2s-scheme body mode acc-return conf hint))
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
		(j2s-scheme-set! lhs name 'any #f mode return conf #f loc)))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(epairify loc
		   (j2s-unresolved-put! `',id name #f mode return))))
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field loc)
		(epairify loc
		   (j2s-put! loc (j2s-scheme obj mode return conf hint)
		      (typeof-this obj conf)
		      (j2s-scheme field mode return conf hint)
		      (j2s-vtype field)
		      name 'any (strict-mode? mode) conf #f))))
	    ((isa? lhs J2SWithRef)
	     (with-access::J2SWithRef lhs (id withs expr loc)
		(epairify loc
		   (let liip ((withs withs))
		      (if (null? withs)
			  (loop expr)
			  `(if ,(j2s-in? loc `',id (car withs))
			       ,(j2s-put! loc (car withs)
				   'object
				   (symbol->string id) 'propname
				   name 'any #f conf #f)
			       ,(liip (cdr withs))))))))
	    (else
	     (j2s-error "j2sscheme" "Illegal lhs" this)))))
   
   (with-access::J2SForIn this (loc lhs obj body op
				  need-bind-exit-break need-bind-exit-continue)
      (let* ((tmp (gensym))
	     (name (gensym))
	     (props (gensym))
	     (set (set lhs name loc)))
	 (epairify-deep loc
	    (if (or need-bind-exit-continue need-bind-exit-break)
		(for-in/break tmp name props obj body set op)
		(for-in/w-break tmp name props obj body set op))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLabel ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLabel mode return conf hint)
   (with-access::J2SLabel this (body need-bind-exit-break id)
      (if need-bind-exit-break
	  `(bind-exit (,(escape-name '%break id)) 
	      ,(j2s-scheme body mode return conf hint))
	  (j2s-scheme body mode return conf hint))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBreak ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBreak mode return conf hint)
   (with-access::J2SBreak this (loc target)
      (with-access::J2SIdStmt target (id)
	 (epairify loc
	    `(,(escape-name '%break id)
	      ,(if (in-eval? return) '%acc '(js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SContinue ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SContinue mode return conf hint)
   (with-access::J2SContinue this (loc target)
      (with-access::J2SLoop target (id)
	 (epairify loc
	    `(,(escape-name '%continue id)
	      ,(if (in-eval? return) '%acc '(js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSwitch ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSwitch mode return conf hint)
   (with-access::J2SSwitch this (loc key cases id need-bind-exit-break)
      
      (define (test-switch tleft tright)
	 (if (and (memq tleft '(number integer)) (memq tright '(number integer)))
	     '=
	     'js-strict-equal?))
      
      (define (comp-switch-cond-clause case tmp ttmp body tleft)
	 (with-access::J2SCase case (loc expr)
	    (if (isa? case J2SDefault)
		(epairify loc `(else ,body))
		(epairify loc
		   `(,(js-binop2 loc '=== 'bool
			 (instantiate::J2SHopRef
			    (loc loc)
			    (id tmp)
			    (vtype ttmp)
			    (type ttmp))
			 expr
			 mode return conf hint)
		     ,body)))))

      (define (comp-switch-case-clause case body tleft)
	 (with-access::J2SCase case (loc expr)
	    (if (isa? case J2SDefault)
		(epairify loc `(else ,body))
		(let ((test `(,(j2s-scheme expr mode return conf hint))))
		   (epairify loc `(,test ,body))))))

      (define (empty? seq::J2SSeq)
	 (with-access::J2SSeq seq (nodes)
	    (null? nodes)))
      
      (define (comp-switch-clause-body case funs)
	 (with-access::J2SCase case (loc body cascade)
	    (epairify loc
	       (if (empty? body)
		   (if (and cascade (pair? (cdr funs)))
		       ;; must check null if default is not the last clause
		       `(,(cadr funs))
		       '(js-undefined))
		   `(begin
		       ,(j2s-scheme body mode return conf hint)
		       ,@(if (and cascade (pair? (cdr funs)))
			     ;; must check null if default is not the last clause
			     `((,(cadr funs)))
			     '()))))))

      (define (comp-switch-clause-bodies cases funs)
	 (let loop ((cases cases)
		    (funs funs)
		    (in-cascade #f)
		    (bindings '())
		    (bodies '()))
	    (if (null? cases)
		(values bindings (reverse! bodies))
		(with-access::J2SCase (car cases) (loc cascade)
		   (if in-cascade
		       (let* ((body (epairify loc `(,(car funs))))
			      (fun `(lambda ()
				       ,(comp-switch-clause-body (car cases)
					   funs)))
			      (binding (list (car funs) fun)))
			  (loop (cdr cases) (cdr funs)
			     cascade
			     (cons binding bindings)
			     (cons body bodies)))
		       (let ((body (epairify loc
				      (comp-switch-clause-body (car cases)
					 funs))))
			  (loop (cdr cases) (cdr funs)
			     cascade
			     bindings
			     (cons body bodies))))))))

      (define (mapc proc cases bodies)
	 (let loop ((cases cases)
		    (bodies bodies)
		    (default #f)
		    (res '()))
	    (cond
	       ((null? cases)
		(if default
		    (reverse! (cons default res))
		    (reverse! res)))
	       ((isa? (car cases) J2SDefault)
		(loop (cdr cases) (cdr bodies)
		   (proc (car cases) (car bodies))
		   res))
	       (else
		(loop (cdr cases) (cdr bodies)
		   default
		   (cons (proc (car cases) (car bodies)) res))))))
		
      (define (comp-switch-cond key cases)
	 (let ((tmp (gensym 'tmp))
	       (ttmp (j2s-vtype key))
	       (funs (map (lambda (c) (gensym 'fun)) cases))
	       (tleft (j2s-vtype key)))
	    (multiple-value-bind (bindings bodies)
	       (comp-switch-clause-bodies cases funs)
	       `(let* ((,tmp ,(j2s-scheme key mode return conf hint))
		       ,@bindings)
		   (cond
		      ,@(mapc (lambda (c body)
				 (comp-switch-cond-clause c tmp ttmp body tleft))
			 cases bodies))))))

      (define (comp-switch-case key cases)
	 (let ((funs (map (lambda (c) (gensym 'fun)) cases))
	       (tleft (j2s-vtype key)))
	    (multiple-value-bind (bindings bodies)
	       (comp-switch-clause-bodies cases funs)
	       `(let* ,bindings
		   (case ,(j2s-scheme key mode return conf hint)
		      ,@(mapc (lambda (c body)
				 (comp-switch-case-clause c body tleft))
			 cases bodies))))))
      
      (define (scheme-case? key cases)
	 (let ((t (j2s-vtype key)))
	    (when (or (memq t '(integer index ufixnum uint32 uint29))
		      (and (eq? t 'int53) (m64? conf)))
	       (every (lambda (c)
			 (or (isa? c J2SDefault)
			     (with-access::J2SCase c (expr)
				(when (isa? expr J2SNumber)
				   (with-access::J2SNumber expr (val)
				      (fixnum? val))))))
		  cases))))
      
      (define (comp-switch)
	 (if (scheme-case? key cases)
	     (comp-switch-case key cases)
	     (comp-switch-cond key cases)))
      
      (define (eval-switch)
	 (let ((elsebody #f)
	       (elsefun #f)
	       (tmp (gensym 'tmp))
	       (funs (map (lambda (c) (gensym 'fun)) cases)))
	    `(let* ((,tmp ,(j2s-scheme key mode return conf hint))
		    (%acc (js-undefined))
		    ,@(map (lambda (case fun)
			      (with-access::J2SCase case (loc body)
				 (epairify loc
				    `(,fun
					(lambda ()
					   ,(j2s-scheme body mode acc-return conf hint))))))
			 cases funs))
		(cond
		   ,@(filter-map (lambda (case::J2SCase fun)
				    (with-access::J2SCase case (loc expr body)
				       (cond
					  ((isa? case J2SDefault)
					   (set! elsebody body)
					   (set! elsefun fun)
					   #f)
					  (else
					   (epairify loc
					      `((js-strict-equal? ,tmp
						   ,(j2s-scheme expr mode return conf hint))
						,@(map (lambda (f) `(,f))
						     (memq fun funs))))))))
		      cases funs)
		   ,(epairify loc
		     `(else
		       ,@(if elsebody
			     (map (lambda (f) `(,f)) (memq elsefun funs))
			     '((js-undefined)))
		       %acc))))))
      
      (let ((switch (if (in-eval? return) (eval-switch) (comp-switch))))
	 (epairify-deep loc
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,switch)
		switch)))))

;*---------------------------------------------------------------------*/
;*    j2s-is-string? ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-is-string? field str)
   (when (isa? field J2SString)
      (with-access::J2SString field (val)
	 (string=? val str))))

;*---------------------------------------------------------------------*/
;*    read-only-function ...                                           */
;*---------------------------------------------------------------------*/
(define (read-only-function ref::J2SRef)
   (with-access::J2SRef ref (decl usage)
      (cond
	 ((isa? decl J2SDeclSvc)
	  #f)
	 ((isa? decl J2SDeclFun)
	  (with-access::J2SDecl decl (ronly)
	     (when ronly decl)))
	 ((j2s-let-opt? decl)
	  (with-access::J2SDeclInit decl (usage id val scope)
	     (when (isa? val J2SFun)
		(not (memq 'assig usage)) decl)))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCall mode return conf hint)
   
   (define (array-push obj arg)
      (let ((scmobj (j2s-scheme obj mode return conf hint))
	    (scmarg (j2s-scheme arg mode return conf hint)))
	 (if (isa? obj J2SAref)
	     (with-access::J2SAref obj (array alen)
		(let ((scmarr (j2s-decl-scheme-id array))
		      (scmalen (j2s-decl-scheme-id alen)))
		   `(let ((%l:len (js-array-length ,scmobj))
			  (%o:item ,scmarg))
		       (if (<u32 %l:len (fixnum->uint32 ,scmalen))
			   (let ((%t:tmp (uint32->fixnum %l:len)))
			      (vector-set-ur! ,scmarr %t:tmp %o:item)
			      (js-array-update-length! ,scmobj (+fx 1 %t:tmp))
			      %o:item)
			   (js-array-push ,scmobj ,scmarg %this)))))
	     `(js-array-push ,scmobj ,scmarg %this))))

   (define (find-builtin-method obj field args)
      
      (define (is-type-or-class? ty obj tyobj)
	 (cond
	    ((eq? ty 'any)
	     #t)
	    ((eq? ty tyobj)
	     #t)
	    ((isa? obj J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef obj (id)
		(eq? ty id)))))
      
      (define (j2s-type-sans-cast expr)
	 (if (isa? expr J2SCast)
	     (with-access::J2SCast expr (expr)
		(j2s-type-sans-cast expr))
	     (j2s-vtype expr)))
      
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (let ((tyobj (j2s-type obj)))
	       (find (lambda (m)
			(when (string=? (builtin-method-jsname m) val)
			   (let ((ty (builtin-method-ttype m)))
			      (when (is-type-or-class? ty obj tyobj)
				 (let loop ((args args)
					    (params (builtin-method-args m)))
				    (cond
				       ((null? args)
					(or (null? params)
					    (every pair? params)))
				       ((null? params)
					#f)
				       (else
					(let ((tya (j2s-type-sans-cast
						      (car args)))
					      (tyf (if (pair? (car params))
						       (caar params)
						       (car params))))
					   (when (or (eq? tyf 'any)
						     (eq? tyf tya))
					      (loop (cdr args)
						 (cdr params)))))))))))
		  j2s-builtin-methods)))))


   (define (call-builtin-method obj::J2SExpr field::J2SExpr args)
      (let ((m (find-builtin-method obj field args)))
	 (when m
	    (let* ((met (builtin-method-met m))
		   (opt (builtin-method-args m))
		   (arity (length opt))
		   (len (length args)))
	       (cond
		  ((symbol? met)
		   ;; builtin simple
		   `(,met ,(j2s-scheme obj mode return conf hint)
		       ,@(map (lambda (arg)
				 (j2s-scheme arg mode return conf hint))
			    args)
		       ,@(if (=fx len arity)
			     '()
			     (let* ((lopt (length opt))
				    (nopt (-fx arity len)))
				(map cadr (list-tail opt (-fx lopt nopt)))))
		       ,@(if (builtin-method-%this m)
			     '(%this)
			     '())))
		  ((procedure? met)
		   ;; builtin procedure
		   (met obj
		      (append args
			 (if (=fx len arity)
			     '()
			     (let* ((lopt (length opt))
				    (nopt (-fx arity len)))
				(map cadr (list-tail opt (-fx lopt nopt)))))
			 (if (builtin-method-%this m)
			     '(%this)
			     '()))
		      mode return conf hint))
		  (else
		   (error "js2scheme" "illegal builtin method" m)))))))

	 
   (define (call-super-method fun args)
      (call-unknown-function fun '(this) args))

   (define (call-ref-method self ccache ocache fun::J2SAccess obj::J2SExpr args)
      (with-access::J2SAccess fun (loc field)
	 (cond
	    ((isa? self J2SSuper)
	     (call-super-method fun args))
	    ((and ccache (= (bigloo-debug) 0))
	     (cond
		((isa? field J2SString)
		 (let ((call (if (eq? (j2s-vtype obj) 'object)
				 'js-object-method-call-name/cache
				 'js-method-call-name/cache)))
		    (with-access::J2SString field (val)
		       `(,call
			   ,j2s-unresolved-call-workspace
			   ,(j2s-scheme obj mode return conf hint)
			   ',(string->symbol val)
			   ,(js-pcache ccache)
			   ,(js-pcache ocache)
			   ,@(map (lambda (arg)
				     (j2s-scheme arg mode return conf hint))
				args)))))
		(else
		 (call-unknown-function fun
		    (list (j2s-scheme obj mode return conf hint))
		    args))))
	    (else
	     (call-unknown-function fun
		(list (j2s-scheme obj mode return conf hint))
		args)))))

   (define (call-globalref-method self ccache ocache fun::J2SAccess obj::J2SExpr args)
      (with-access::J2SGlobalRef self (id decl)
	 (with-access::J2SDecl decl (usage)
	    (unless (memq 'assig usage)
	       (case id
		  ((Math)
		   (j2s-math-builtin-method fun args
		      mode return conf hint))
		  (else
		   #f))))))

   (define (call-new-method obj::J2SNew field args mode return conf hint)
      (with-access::J2SNew obj (clazz)
	 (when (isa? clazz J2SGlobalRef)
	    (with-access::J2SGlobalRef clazz (id decl)
	       (with-access::J2SDecl decl (usage)
		  (unless (memq 'assig usage)
		     (case id
			((Date)
			 (j2s-date-new-method obj field args mode return
			    conf hint))
			(else
			 #f))))))))
   
   (define (call-method ccache ocache fun::J2SAccess args)
      (with-access::J2SAccess fun (loc obj field)
	 (let loop ((obj obj))
	    (cond
	       ((call-builtin-method obj field args)
		=>
		(lambda (sexp) sexp))
	       ((isa? obj J2SRef)
		(call-ref-method obj ccache ocache fun obj args))
	       ((isa? obj J2SGlobalRef)
		(or (call-globalref-method obj ccache ocache fun obj args)
		    (call-ref-method obj ccache ocache fun obj args)))
	       ((isa? obj J2SParen)
		(with-access::J2SParen obj (expr)
		   (loop expr)))
	       ((and (isa? obj J2SNew)
		     (call-new-method obj field args mode return conf hint))
		=>
		(lambda (sexp) sexp))
	       (else
		(let* ((tmp (gensym 'obj))
		       (ttmp (type-ident tmp (j2s-vtype obj))))
		   `(let ((,ttmp ,(j2s-scheme obj mode return conf hint)))
		       ,(call-ref-method obj
			   ccache ocache
			   (duplicate::J2SAccess fun
			      (obj (instantiate::J2SPragma
				      (loc loc)
				      (expr tmp))))
			   (instantiate::J2SHopRef
			      (type (j2s-vtype obj))
			      (loc loc)
			      (id tmp))
			   args))))))))
   
   (define (call-hop-function fun::J2SHopRef thisarg args)
      `(,(j2s-scheme fun mode return conf hint)
	,@(j2s-scheme args mode return conf hint)))

   (define (j2s-self thisarg)
      (map (lambda (t) (j2s-scheme t mode return conf hint)) thisarg))

   (define (call-rest-function fun::J2SFun thisarg::pair-nil f %gen args)
      ;; call a function that accepts a rest argument
      (with-access::J2SFun fun (params vararg)
	 (let loop ((params params)
		    (args args)
		    (actuals '()))
	    (cond
	       ((null? (cdr params))
		;; the rest argument
		`(,f ,@%gen ,@(j2s-self thisarg) ,@(reverse! actuals)
		    (js-vector->jsarray
		       (vector ,@(j2s-scheme args mode return conf hint))
		       %this)))
	       ((null? args)
		(with-access::J2SDecl (car params) (loc)
		   (loop (cdr params) '()
		      (cons '(js-undefined) actuals))))
	       (else
		(loop (cdr params) (cdr args)
		   (cons (j2s-scheme (car args) mode return conf hint)
		      actuals)))))))

   (define (call-fix-function fun::J2SFun thisarg::pair-nil f %gen args)
      ;; call a function that accepts a fix number of arguments
      (with-access::J2SFun fun (params vararg thisp)
	 (let ((lenf (length params))
	       (lena (length args)))
	    (cond
	       ((=fx lenf lena)
		;; matching arity
		`(,f ,@%gen
		    ,@(j2s-self thisarg)
		    ,@(map (lambda (a p)
			      (with-access::J2SDecl p (utype)
				 (j2s-scheme a mode return conf hint)))
			 args params)))
	       ((>fx lena lenf)
		;; too many arguments ignore the extra values,
		;; but still evaluate extra expressions
		(let ((temps (map (lambda (i)
				     (string->symbol
					(string-append "%a"
					   (integer->string i))))
				(iota lena))))
		   `(let* ,(map (lambda (t a)
				   `(,t ,(j2s-scheme a mode return conf hint))) temps args)
		       (,f ,@%gen ,@(j2s-self thisarg)
			  ,@(take temps lenf)))))
	       (else
		;; argument missing
		`(,f ,@(j2s-self thisarg)
		    ,@(j2s-scheme args mode return conf hint)
		    ,@(make-list (-fx lenf lena) '(js-undefined))))))))

   (define (check-hopscript-fun-arity val::J2SFun id args)
      (with-access::J2SFun val (params vararg loc name mode)
	 (when (eq? mode 'hopscript)
	    (let ((lp (length params))
		  (la (length args)))
	       (unless (=fx lp la)
		  (case vararg
		     ((rest)
		      (unless (>=fx la (-fx (j2s-minlen val)  1))
			 (j2s-error id
			    (format "wrong number of arguments, minimum expected: ~a" (j2s-minlen val) la)
			    this (format "~a provided" la))))
		     ((arguments)
		      #t)
		     (else
		      (unless (and (>=fx la (j2s-minlen val)) (<=fx la lp))
			 (j2s-error id
			    (format "wrong number of arguments, minimum expected: ~a" (j2s-minlen val))
			    this
			    (format "~a provided" la))))))))))

   (define (call-fun-function fun::J2SFun thisarg::pair-nil protocol f %gen::pair-nil args::pair-nil)
      (with-access::J2SFun fun (params vararg idthis)
	 (case (if (eq? protocol 'bounce) 'bounce vararg)
	    ((arguments)
	     `(,f ,@%gen ,@(if idthis (j2s-self thisarg) '())
		 ,@(j2s-scheme args mode return conf hint)))
	    ((rest)
	     (call-rest-function fun (if idthis thisarg '()) f %gen args))
	    (else
	     (call-fix-function fun (if idthis thisarg '()) f %gen args)))))

   (define (call-with-function fun::J2SWithRef args)
      (with-access::J2SWithRef fun (id withs loc)
	 (let loop ((withs withs))
	    (if (null? withs)
		(call-unknown-function fun '((js-undefined)) args)
		`(if ,(j2s-in? loc `',id (car withs))
		     ,(call-unknown-function
			 (j2s-get loc (car withs) 'object `',id 'string 'any conf #f)
			(list (car withs)) args)
		     ,(loop (cdr withs)))))))

   (define (call-pragma fun::J2SPragma args)
      (with-access::J2SPragma fun (expr)
	 `(,expr %this ,@(j2s-scheme args mode return conf hint))))

   (define (typed-generator? decl::J2SDeclFun)
      (with-access::J2SDeclFun decl (parent)
	 (when (isa? parent J2SDeclFun)
	    (with-access::J2SDeclFun parent (val)
	       (with-access::J2SFun val (generator)
		  generator)))))

   (define (call-known-function protocol fun::J2SDecl thisarg::pair-nil args)
      (cond
	 ((isa? fun J2SDeclFun)
	  (with-access::J2SDeclFun fun (id val usage)
	     (check-hopscript-fun-arity val id args)
	     (let ((%gen (if (typed-generator? fun) '(%gen) '())))
		(call-fun-function val thisarg protocol
		   (j2s-fast-id id) %gen args))))
	 ((j2s-let-opt? fun)
	  (with-access::J2SDeclInit fun (id val)
	     (call-fun-function val thisarg protocol
		(j2s-fast-id id) '() args)))
	 (else
	  (error "js-scheme" "Should not be here" (j2s->list fun)))))

   (define (call-unknown-function fun self::pair-nil args)
      (let* ((len (length args))
	     (call (if (>=fx len 11)
		       'js-calln
		       (string->symbol (format "js-call~a" len)))))
	 (with-access::J2SCall this (loc cache)
	    (cond
	       ((> (bigloo-debug) 0)
		`(,(symbol-append call '/debug)
		  ,j2s-unresolved-call-workspace
		  ',loc
		  ,(j2s-scheme fun mode return conf hint)
		  ,@self
		  ,@(j2s-scheme args mode return conf hint)))
	       (cache
		`(js-call/cache
		    ,j2s-unresolved-call-workspace
		    ,(j2s-scheme fun mode return conf hint)
		    ,(js-pcache cache)
		    ,@self
		    ,@(j2s-scheme args mode return conf hint)))
	       (else
		`(,call ,j2s-unresolved-call-workspace
		    ,(j2s-scheme fun mode return conf hint)
		    ,@self
		    ,@(j2s-scheme args mode return conf hint)))))))

   (define (call-eval-function fun args)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.1.1
      `(%js-direct-eval 
	  ,(if (null? args)
	       '(js-undefined)
	       (j2s-scheme (car args) mode return conf hint))
	  ,(strict-mode? mode)
	  %this this %scope))

   (define (is-eval? fun)
      (with-access::J2SUnresolvedRef fun (id)
	 (eq? id 'eval)))

   (define (call-unresolved-function fun thisarg args)
      (if (is-eval? fun)
	  (call-eval-function fun args)
	  (call-unknown-function fun
	     (j2s-scheme thisarg mode return conf hint) args)))

   (with-access::J2SCall this (loc fun thisarg args protocol cache)
      (let loop ((fun fun))
	 (epairify loc
	    (cond
	       ((isa? fun J2SAccess)
		(with-access::J2SAccess fun ((ocache cache))
		   (call-method cache ocache fun args)))
	       ((isa? fun J2SParen)
		(with-access::J2SParen fun (expr)
		   (loop expr)))
	       ((isa? fun J2SHopRef)
		(call-hop-function fun thisarg args))
	       ((isa? fun J2SSuper)
		(j2s-scheme-super this mode return conf hint))
	       ((and (isa? fun J2SFun) (not (j2sfun-id fun)))
		(call-fun-function fun thisarg protocol
		   (jsfun->lambda fun mode return conf (j2s-fun-prototype fun) #f)
		   '()
		   args))
	       ((isa? fun J2SUnresolvedRef)
		(call-unresolved-function fun thisarg args))
	       ((isa? fun J2SWithRef)
		(call-with-function fun args))
	       ((isa? fun J2SPragma)
		(call-pragma fun args))
	       ((not (isa? fun J2SRef))
		(call-unknown-function fun
		   (j2s-scheme thisarg mode return conf hint) args))
	       ((read-only-function fun)
		=>
		(lambda (fun)
		   (call-known-function protocol fun thisarg args)))
	       (else
		(call-unknown-function fun
		   (j2s-scheme thisarg mode return conf hint) args)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssig mode return conf hint)

   (define (maybe-array-set lhs::J2SAccess rhs::J2SExpr)
      (with-access::J2SAccess lhs (obj field cache loc)
	 (if (isa? lhs J2SRef)
	     `(if (isa? ,(j2s-scheme obj mode return conf hint) JsArray)
		  ,(j2s-array-set! this mode return conf hint)
		  ,(j2s-put! loc (j2s-scheme obj mode return conf hint)
		      (typeof-this obj conf)
		      (j2s-scheme field mode return conf hint)
		      (j2s-vtype field)
		      (j2s-scheme rhs mode return conf hint)
		      (j2s-vtype rhs)
		      (strict-mode? mode)
		      conf
		      cache))
	     (let* ((tmp (gensym 'tmp))
		    (access (duplicate::J2SAccess lhs (obj (J2SHopRef tmp)))))
		`(let ((,tmp ,(j2s-scheme obj mode return conf hint)))
		    (if (isa? ,tmp JsArray)
			,(j2s-array-set! this mode return conf hint)
			,(j2s-put! loc tmp
			    (typeof-this obj conf)
			    (j2s-scheme field mode return conf hint)
			    (j2s-vtype field)
			    (j2s-scheme rhs mode return conf hint)
			    (j2s-vtype rhs)
			    (strict-mode? mode)
			    conf
			    cache)))))))

   (with-access::J2SAssig this (loc lhs rhs)
      (let loop ((lhs lhs))
	 (cond
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field cache (loca loc))
		(epairify loc
		   (cond
		      ((eq? (j2s-vtype obj) 'vector)
		       (j2s-vector-set! this mode return conf hint))
		      ((and (eq? (j2s-vtype obj) 'array) (maybe-number? field))
		       (j2s-array-set! this mode return conf hint))
		      ((is-number? field)
		       (maybe-array-set lhs rhs))
		      (else
		       (j2s-put! loca (j2s-scheme obj mode return conf hint)
			  (typeof-this obj conf)
			  (j2s-scheme field mode return conf hint)
			  (j2s-vtype field)
			  (j2s-scheme rhs mode return conf hint)
			  (j2s-vtype rhs)
			  (strict-mode? mode)
			  conf
			  cache))))))
	    ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
	     (with-access::J2SRef lhs (decl loc type)
		(with-access::J2SDecl decl (hint vtype)
		   (let ((assig (j2s-scheme-set! lhs
				   (j2s-scheme rhs mode return conf hint)
				   (j2s-vtype rhs)
				   (j2s-scheme lhs mode return conf hint)
				   mode return conf #f loc)))
		      (if (pair? assig)
			  (epairify loc assig)
			  assig)))))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(epairify loc
		   (j2s-unresolved-put! `',id
		      (j2s-scheme rhs mode return conf hint) #f mode return))))
	    ((isa? lhs J2SHopRef)
	     (with-access::J2SHopRef lhs (id)
		(epairify loc
		   `(set! ,id ,(j2s-scheme rhs mode return conf hint)))))
	    ((isa? lhs J2SWithRef)
	     (with-access::J2SWithRef lhs (id withs expr loc)
		(epairify loc
		   (let liip ((withs withs))
		      (if (null? withs)
			  (loop expr)
			  `(if ,(j2s-in? loc `',id (car withs))
			       ,(j2s-put! loc (car withs) 'object
				   (symbol->string id) 'propname
				   (j2s-scheme rhs mode return conf hint)
				   (j2s-vtype rhs)
				   #f conf #f)
			       ,(liip (cdr withs))))))))
	    ((isa? lhs J2SUndefined)
	     (j2s-scheme rhs mode return conf hint))
	    ((isa? lhs J2SParen)
	     (with-access::J2SParen lhs (expr)
		(loop expr)))
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
(define (j2s-scheme-postpref this::J2SAssig mode return conf hint op retval)

   (define (new-or-old tmp val comp)
      (if (eq? retval 'new)
	  (let ((aux (gensym 'res)))
	     `(let ((,aux ,val))
		 ,(comp aux aux)))
	  (comp val tmp)))

   (define (var++ op var tyv typ num prev loc)
      (if (type-number? typ)
	  (J2SBinary/type op tyv (J2SHopRef/type var tyv typ) num)
	  `(if (fixnum? ,var)
	       ,(J2SBinary/type op 'integer (J2SHopRef/type var 'any 'bint) num)
	       ,(if prev
		    `(begin
			(set! ,prev
			   ,(J2SCast 'number (J2SHopRef/type var 'any 'any)))
			,(J2SBinary/type op 'number
			    (J2SHopRef/type prev 'any 'number) num))
		    (J2SBinary/type op 'number
		       (J2SCast 'number (J2SHopRef/type var 'any 'any))
		       num)))))
      
   (define (ref++ op lhs::J2SRef num prev loc mode return conf hint)
      (let ((var (j2s-scheme lhs mode return conf hint))
	    (vty (j2s-vtype lhs))
	    (ty (j2s-type lhs)))
	 (if (symbol? var)
	     (var++ op var vty ty num prev loc)
	     (let ((tmp (gensym 'tmp)))
		`(let ((,tmp ,var))
		    ,(var++ op tmp vty ty num prev loc))))))
   
   (define (ref-inc op lhs::J2SRef inc::int type loc)
      (let* ((vty (j2s-vtype lhs))
	     (num (J2SNumber/type 'uint32 1))
	     (op (if (=fx inc 1) '+ '-))
	     (prev (when (eq? retval 'old) (gensym 'prev)))
	     (rhse (j2s-scheme
		      (if (type-number? vty)
			  (J2SBinary/type op vty lhs num)
			  (ref++ op lhs num prev loc mode return conf hint))
		      mode return conf hint))
	     (lhse (j2s-scheme lhs mode return conf hint)))
	 (if (eq? retval 'old)
	     (let ((res prev))
		`(let ((,res ,lhse))
		    ,(j2s-scheme-set! lhs rhse vty res mode return conf #f loc)))
	     (j2s-scheme-set! lhs rhse vty lhse mode return conf #f loc))))
   
   (define (unresolved-inc op lhs inc)
      (with-access::J2SUnresolvedRef lhs (id cache loc)
	 (let ((tmp (gensym 'tmp)))
	    `(let ((,tmp ,(j2s-unresolved id cache loc)))
		(if (fixnum? ,tmp)
		    ,(new-or-old tmp `(+fx/overflow ,tmp ,inc)
		       (lambda (val tmp)
			  `(begin
			      ,(j2s-unresolved-put! `',id val #t mode return)
			      ,tmp)))
		    ,(new-or-old tmp `(js+ ,tmp ,inc %this)
		       (lambda (val tmp)
			  `(let ((,tmp (js-tonumber ,tmp %this)))
			      ,(j2s-unresolved-put! `',id val #t mode return)
			      ,tmp))))))))
   
   (define (aput-inc otmp pro prov op lhs cache inc cl #!optional force-type)
      (with-access::J2SAccess lhs (loc obj field clevel (loca loc))
	 (with-access::J2SExpr obj (type loc)
	    (let* ((tmp (gensym 'aput))
		   (oref (instantiate::J2SHopRef
			    (loc loc)
			    (id otmp)
			    (type (or force-type type))))
		   (oacc (duplicate::J2SAccess lhs
			    (clevel (min cl clevel))
			    (obj oref)
			    (field field)))
		   (rhs (instantiate::J2SNumber
			   (loc loc)
			   (val inc)
			   (type 'int32)))
		   (field (if pro (J2SHopRef pro) field))
		   (scmlhs (j2s-scheme oacc mode return conf hint)))
	       (cond
		  ((type-fixnum? type)
		   (let ((tref (instantiate::J2SHopRef
				  (loc loc)
				  (id tmp)
				  (type 'bint))))
		      `(let ((,tmp ,scmlhs))
			  ,(new-or-old tmp
			      (js-binop2 loc '+ 'number ;; (typeof-this obj conf)
				 tref rhs mode return conf hint)
			      (lambda (val tmp)
				 `(begin
				     ,(j2s-put! loc otmp
					 (or force-type (typeof-this obj conf))
					 (or pro prov)
					 (j2s-vtype field)
					 val 'number
					 (strict-mode? mode) conf
					 cache (min cl clevel))
				     ,tmp))))))
		  (else
		   
		   `(let ((,tmp ,scmlhs))
		       (if (fixnum? ,tmp)
			   ,(let ((tref (instantiate::J2SHopRef
					   (loc loc)
					   (id tmp)
					   (type 'bint))))
			       (new-or-old tmp
				  (js-binop2 loc '+ 'number ;;(typeof-this obj conf)
				     tref rhs mode return conf hint)
				  (lambda (val tmp)
				     `(begin
					 ,(j2s-put! loc otmp
					     (or force-type (typeof-this obj conf))
					     (or pro prov)
					     (j2s-vtype field)
					     val 'number
					     (strict-mode? mode) conf
					     cache (min cl clevel))
					 ,tmp))))
			   ,(let* ((tmp2 (gensym 'tmp))
				   (tref (instantiate::J2SHopRef
					    (loc loc)
					    (id tmp2)
					    (type 'number))))
			       `(let ((,tmp2 (js-tonumber ,tmp %this)))
				   ,(new-or-old tmp2
				       (js-binop2 loc '+ 'number ;;(typeof-this obj conf)
					  tref rhs mode return conf hint)
				       (lambda (val tmp)
					  `(begin
					      ,(j2s-put! loc otmp
						  (or force-type (typeof-this obj conf))
						  (or pro prov)
						  (j2s-vtype field)
						  val 'number
						  (strict-mode? mode) conf
						  cache (min cl clevel))
					      ,tmp2)))))))))))))

   (define (rhs-cache rhs)
      (if (isa? rhs J2SCast)
	  (with-access::J2SCast rhs (expr)
	     (rhs-cache expr))
	  (with-access::J2SBinary rhs (lhs)
	     (when (isa? lhs J2SAccess)
		(with-access::J2SAccess lhs (cache)
		   cache)))))
   
   (define (access-inc op lhs::J2SAccess rhs::J2SExpr inc::int)
      (with-access::J2SAccess lhs (obj field clevel cache (loca loc))
	 (let* ((tmp (gensym 'tmp))
		(otmp (gensym 'obj))
		(prov (j2s-property-scheme field mode return conf))
		(pro (when (pair? prov) (gensym 'pro)))
		(rhscache (rhs-cache rhs)))
	    `(let* ((,otmp ,(j2s-scheme obj mode return conf hint))
		    ,@(if pro (list `(,pro ,prov)) '()))
		,(cond
		    ((<=fx clevel 2)
		     (aput-inc otmp pro prov op lhs rhscache inc clevel))
		    ((not cache)
		     (aput-inc otmp pro prov op lhs cache inc 100))
		    ((memq (typeof-this obj conf) '(object this global))
		     `(with-access::JsObject ,otmp (cmap)
			 (let ((%omap cmap))
			    (if (eq? (js-pcache-cmap ,(js-pcache cache)) cmap)
				,(aput-inc otmp pro prov op lhs cache inc 1)
				,(aput-inc otmp pro prov op lhs rhscache inc 2)))))
		    (else
		     `(if (and (js-object? ,otmp)
			       (with-access::JsObject ,otmp (cmap)
				  (eq? (js-pcache-cmap ,(js-pcache cache)) cmap)))
			  ,(aput-inc otmp pro prov op lhs cache inc 1 'object)
			  (let ((%omap (with-access::JsObject ,otmp (cmap) cmap)))
			     ,(aput-inc otmp pro prov op lhs rhscache inc 2)))))))))
   
   (with-access::J2SAssig this (loc lhs rhs type)
      (epairify-deep loc
	 (let loop ((lhs lhs))
	    (cond
	       ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
		(ref-inc op lhs (if (eq? op '++) 1 -1) type loc))
	       ((isa? lhs J2SAccess)
		(access-inc op lhs rhs (if (eq? op '++) 1 -1)))
	       ((isa? lhs J2SUnresolvedRef)
		(unresolved-inc op lhs (if (eq? op '++) 1 -1)))
	       ((isa? lhs J2SParen)
		(with-access::J2SParen lhs (expr)
		   (loop expr)))
	       ((isa? lhs J2SCast)
		(with-access::J2SCast lhs (expr)
		   (loop expr)))
	       (else
		(j2s-error "j2sscheme"
		   (format "Illegal expression \"~a\"" op)
		   this)))))))
	   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPostfix ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.3.1       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPostfix mode return conf hint)
   (with-access::J2SPostfix this (op)
      (j2s-scheme-postpref this mode return conf hint op 'old)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPrefix ...                                       */
;*    -------------------------------------------------------------    */
;*    www.ecma-international.org/ecma-262/5.1/#sec-11.3.1prefix        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPrefix mode return conf hint)
   (with-access::J2SPrefix this (op)
      (j2s-scheme-postpref this mode return conf hint op 'new)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssigOp mode return conf hint)
   
   (define (aput-assigop otmp pro prov op
	      tl::symbol lhs::J2SAccess rhs::J2SExpr cl)
      (with-access::J2SAssigOp this ((typea type))
	 (with-access::J2SAccess lhs (obj field loc cache clevel (typel type))
	    (with-access::J2SExpr obj ((typeo type) loc)
	       (let* ((oref (instantiate::J2SHopRef
			       (loc loc)
			       (id otmp)
			       (type typeo)
			       (itype typeo)))
		      (lhs (J2SCast tl
			      (duplicate::J2SAccess lhs
				 (clevel (min cl clevel))
				 (obj oref)
				 (field (if pro
					    (J2SHopRef/type pro
					       (j2s-vtype field)
					       (j2s-type field))
					    field)))))
		      (vtmp (gensym 'tmp)))
		  `(let ((,(type-ident vtmp typea)
			  ,(js-binop2 loc op typea
			      lhs rhs mode return conf hint)))
		      ,(j2s-put! loc otmp (typeof-this obj conf)
			  (or pro prov) (j2s-vtype field)
			  (j2s-cast vtmp #f typea typel conf) typel
			  (strict-mode? mode) conf
			  cache (if (is-number? field) 100 (min cl clevel)))
		      ,vtmp))))))
      
   (define (access-assigop/otmp otmp op tl::symbol lhs::J2SAccess rhs::J2SExpr)
      (with-access::J2SAccess lhs (obj field cache clevel)
	 (let* ((prov (j2s-property-scheme field mode return conf))
		(pro (when (pair? prov) (gensym 'prop))))
	    `(let* (,@(if pro (list `(,pro ,prov)) '()))
		,(cond
		    ((<=fx clevel 2)
		     (aput-assigop otmp pro prov op
			tl lhs rhs clevel))
		    ((or (not cache) (is-integer? field))
		     (aput-assigop otmp pro prov op
			tl lhs rhs 100))
		    ((memq (typeof-this obj conf) '(object this global))
		     `(with-access::JsObject ,otmp (cmap)
			 (let ((%omap cmap))
			    (if (eq? (js-pcache-cmap ,(js-pcache cache)) %omap)
				,(aput-assigop otmp pro prov op
				    tl lhs rhs 1)
				,(aput-assigop otmp pro prov op
				    tl lhs rhs 2)))))
		    (else
		     `(if (js-object? ,otmp)
			  (with-access::JsObject ,otmp (cmap)
			     (let ((%omap cmap))
				(if (eq? (js-pcache-cmap ,(js-pcache cache))
				       %omap)
				    ,(aput-assigop otmp pro prov op
					tl lhs rhs 1)
				    ,(aput-assigop otmp pro prov op
					tl lhs rhs 2))))
			  (let ((%omap (js-not-a-cmap)))
			     ,(aput-assigop otmp pro prov op
				 tl lhs rhs 2)))))))))

   (define (access-assigop op tl::symbol lhs::J2SAccess rhs::J2SExpr)
      (with-access::J2SAccess lhs (obj field cache clevel)
	 (if (isa? obj J2SRef)
	     (access-assigop/otmp
		(j2s-scheme obj mode return conf hint)
		op tl lhs rhs)
	     (let ((otmp (gensym 'obj)))
		`(let ((,otmp ,(j2s-scheme obj mode return conf hint)))
		    ,(access-assigop/otmp otmp op tl lhs rhs))))))
   
      
   (with-access::J2SAssigOp this (loc lhs rhs op type)
      (epairify-deep loc
	 (let ((tl (j2s-vtype lhs)))
	    (let loop ((lhs lhs))
	       (cond
		  ((isa? lhs J2SAccess)
		   (access-assigop op tl lhs rhs))
		  ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
		   (with-access::J2SRef lhs (decl)
		      (with-access::J2SDecl decl (hint utype)
			 (j2s-scheme-set! lhs
			    (js-binop2 loc op tl lhs rhs mode return conf hint)
			    tl
			    (j2s-scheme lhs mode return conf '())
			    mode return conf #f loc))))
		  ((isa? lhs J2SUnresolvedRef)
		   (with-access::J2SUnresolvedRef lhs (id)
		      (j2s-unresolved-put! `',id
			 (js-binop2 loc op type lhs rhs mode return conf '())
			 #t mode return)))
		  ((isa? lhs J2SCast)
		   (with-access::J2SCast lhs (expr type)
		      (loop expr)))
		  (else
		   (j2s-error "j2sscheme" "Illegal assignment"
		      (j2s->list this)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAccess mode return conf hint)
   
   (define (get obj field cache clevel loc)
      (let ((tyo (typeof-this obj conf)))
	 (j2s-get loc (j2s-scheme obj mode return conf hint) tyo
	    (j2s-property-scheme field mode return conf)
	    (j2s-vtype field) (j2s-vtype this) conf cache clevel)))

   (define (index-ref obj field cache clevel loc)
      (if (isa? obj J2SRef)
	  (let ((tmp (j2s-scheme obj mode return conf hint)))
	     `(cond
		 ((isa? ,tmp JsArray)
		  ,(or (j2s-array-ref this mode return conf hint)
		       (get obj field cache clevel loc)))
		 ((js-jsstring? ,tmp)
		  ,(or (j2s-string-ref this mode return conf hint)
		       (get obj field cache clevel loc)))
		 (else
		  ,(get obj field cache clevel loc))))
	  (let ((tmp (gensym 'tmp)))
	     `(let ((,tmp ,(j2s-scheme obj mode return conf hint)))
		 (cond
		    ((isa? ,tmp JsArray)
		     ,(let ((access (J2SAccess (J2SHopRef tmp) field)))
			 (or (j2s-array-ref access mode return conf hint)
			     (get (J2SHopRef tmp) field cache clevel loc))))
		    ((js-jsstring? ,tmp)
		     ,(let ((access (J2SAccess (J2SHopRef tmp) field)))
			 (or (j2s-string-ref access mode return conf hint)
			     (get (J2SHopRef tmp) field cache clevel loc))))
		    (else
		     ,(get (J2SHopRef tmp) field cache clevel loc)))))))
   
   (with-access::J2SAccess this (loc obj field cache clevel type)
      (epairify-deep loc 
	 (cond
	    ((eq? (j2s-type obj) 'vector)
	     (j2s-vector-ref this mode return conf hint))
	    ((eq? (j2s-type obj) 'array)
	     (or (j2s-array-ref this mode return conf hint)
		 (get obj field cache clevel loc)))
	    ((eq? (j2s-type obj) 'string)
	     (or (j2s-string-ref this mode return conf hint)
		 (get obj field cache clevel loc)))
	    ((is-number? field)
	     (index-ref obj field cache clevel loc))
	    (else
	     (get obj field cache clevel loc))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCacheCheck ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCacheCheck mode return conf hint)
   (with-access::J2SCacheCheck this (prop cache obj)
      (case prop
	 ((proto-method)
	  `(eq? (js-pcache-pmap (js-pcache-ref %pcache ,cache))
	      (js-object-cmap ,(j2s-scheme obj mode return conf hint))))
	 ((instanceof)
	  `(eq? (js-pcache-cmap (js-pcache-ref %pcache ,cache))
	      (js-object-cmap ,(j2s-scheme obj mode return conf hint))))
	 (else
	  (error "j2s-scheme" "Illegal J2SCacheCheck property" prop)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCacheUpdate ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCacheUpdate mode return conf hint)
   (with-access::J2SCacheUpdate this (cache obj prop)
      (case prop
	 ((proto-method)
	  `(with-access::JsPropertyCache (js-pcache-ref %pcache ,cache) (pmap)
	      (set! pmap
		 (js-object-cmap
		    ,(j2s-scheme obj mode return conf hint)))))
	 ((instanceof)
	  `(with-access::JsPropertyCache (js-pcache-ref %pcache ,cache) (cmap)
	      (set! cmap
		 (js-object-cmap
		    ,(j2s-scheme obj mode return conf hint)))))
	 (else
	  (error "j2s-scheme" "Illegal J2SCacheUpdate property" prop)))))

;*---------------------------------------------------------------------*/
;*    maybe-function? ...                                              */
;*---------------------------------------------------------------------*/
(define (maybe-function? expr::J2SNode)
   (memq (j2s-vtype expr) '(function any)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SInit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SInit mode return conf hint)
   (with-access::J2SAssig this (loc lhs rhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (hint)
		(epairify-deep loc
		   `(begin
		       ,(j2s-scheme-set! lhs
			   (j2s-scheme rhs mode return conf hint)
			   (j2s-vtype rhs)
			   #f mode return conf #t loc)
		       (js-undefined)))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SObjInit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SObjInit mode return conf hint)
   
   (define (j2s-propname name)
      (cond
	 ((isa? name J2SString)
	  (with-access::J2SString name (val)
	     (let ((str (string-for-read val)))
		(if (string=? str val)
		    `(quote ,(string->symbol val))
		    `(string->symbol ,val)))))
	 ((isa? name J2SNumber)
	  (with-access::J2SNumber name (val)
	     (if (fixnum? val)
		 `(quote ,(string->symbol (number->string val)))
		 `(js-toname ,(j2s-scheme val mode return conf hint) %this))))
	 ((isa? name J2SPragma)
	  `(js-toname ,(j2s-scheme name mode return conf hint) %this))
	 ((isa? name J2SLiteralCnst)
	  `(js-toname ,(j2s-scheme name mode return conf hint) %this))
	 ((isa? name J2SLiteralValue)
	  (with-access::J2SLiteralValue name (val)
	     `(js-toname ,(j2s-scheme val mode return conf hint) %this)))
	 (else
	  `(js-toname ,(j2s-scheme name mode return conf hint) %this))))
   
   (define (literal-propname name)
      (cond
	 ((isa? name J2SString)
	  (with-access::J2SString name (val)
	     (let ((str (string-for-read val)))
		`(quote ,(string->symbol val)))))
	 ((isa? name J2SNumber)
	  (with-access::J2SNumber name (val)
	     (if (fixnum? val)
		 `(quote ,(string->symbol (number->string val)))
		 `(js-toname ,(j2s-scheme val mode return conf hint) %this))))
	 ((isa? name J2SLiteralCnst)
	  (with-access::J2SLiteralCnst name (val)
	     (literal-propname val)))
	 ((isa? name J2SPragma)
	  `(js-toname ,(j2s-scheme name mode return conf hint) %this))
	 ((isa? name J2SLiteralCnst)
	  `(js-toname ,(j2s-scheme name mode return conf hint) %this))
	 ((isa? name J2SLiteralValue)
	  (with-access::J2SLiteralValue name (val)
	     `(js-toname ,(j2s-scheme val mode return conf hint) %this)))
	 (else
	 `(js-toname ,(j2s-scheme name mode return conf hint) %this))))
   
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
			     (j2s-scheme val mode return conf hint)))
		     inits)))
	 `(let ((,names ,(if (every symbol? props)
			     `(quote ,(list->vector props))
			     `(vector ,@props)))
		(,elements (vector ,@vals)))
	     (js-literal->jsobject ,elements ,names %this))))

   (define (cmap->jsobj inits cmap)
      (let ((vals (map (lambda (i)
			  (with-access::J2SDataPropertyInit i (val)
			     (j2s-scheme val mode return conf hint)))
		     inits)))
	 (if (any (lambda (i)
		     (with-access::J2SDataPropertyInit i (val)
			(maybe-function? (uncast val))))
		inits)
	     `(with-access::JsGlobalObject %this (__proto__)
		 (js-object-literal-init!
		    (instantiateJsObject
		       (cmap ,(j2s-scheme cmap mode return conf hint))
		       (elements (vector ,@vals))
		       (__proto__ __proto__))))
	     `(with-access::JsGlobalObject %this (__proto__)
		 (instantiateJsObject
		    (cmap ,(j2s-scheme cmap mode return conf hint))
		    (elements (vector ,@vals))
		    (__proto__ __proto__))))))
   
   (define (new->jsobj loc inits)
      (let ((tmp (gensym)))
	 `(with-access::JsGlobalObject %this (js-object)
	     (let ((,tmp ,(j2s-new loc 'js-object '())))
		,@(map (lambda (i)
			  (cond
			     ((isa? i J2SDataPropertyInit)
			      (with-access::J2SDataPropertyInit i (loc name val)
				 (if (is-proto? name)
				     ;; __proto__ field is special during
				     ;; initialization, it must be assigned
				     ;; using the generic js-put! function
				     (j2s-put! loc tmp 'obj
					"__proto__" 'propname
					(j2s-scheme val mode return conf hint)
					(j2s-vtype val)
					(strict-mode? mode) conf #f)
				     (epairify loc
					`(js-bind! %this ,tmp
					    ,(j2s-propname name)
					    :value ,(j2s-scheme val mode return conf hint)
					    :writable #t
					    :enumerable #t
					    :configurable #t)))))
			     (else
			      (with-access::J2SAccessorPropertyInit i (loc name get set)
				 (epairify loc
				    `(js-bind! %this ,tmp
					,(j2s-propname name)
					:get ,(j2s-scheme get mode return conf hint)
					:set ,(j2s-scheme set mode return conf hint)
					:writable #t
					:enumerable #t
					:configurable #t))))))
		     inits)
		,tmp))))
   
   (with-access::J2SObjInit this (loc inits cmap)
      (epairify loc
	 (if cmap
	     (cmap->jsobj inits cmap)
	     (if (every (lambda (i)
			   (when (isa? i J2SDataPropertyInit)
			      (with-access::J2SDataPropertyInit i (name)
				 (not (is-proto? name)))))
		    inits)
		 (literal->jsobj inits)
		 (new->jsobj loc inits))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDataPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDataPropertyInit mode return conf hint)
   (with-access::J2SDataPropertyInit this (loc name val)
      (epairify loc
	 `(,(j2s-scheme name mode return conf hint)
	   ,(j2s-scheme val mode return conf hint)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNew ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNew mode return conf hint)
   
   (define (new-array? clazz)
      (when (isa? clazz J2SUnresolvedRef)
	 (with-access::J2SUnresolvedRef clazz (id)
	    (eq? id 'Array))))

   (define (constructor-no-return? decl)
      ;; does this constructor never returns something else than UNDEF?
      (let ((fun (cond
		    ((isa? decl J2SDeclFun)
		     (with-access::J2SDeclFun decl (val) val))
		    ((j2s-let-opt? decl)
		     (with-access::J2SDeclInit decl (val) val)))))
	 (when (isa? fun J2SFun)
	    (with-access::J2SFun fun (rtype)
	       (eq? rtype 'undefined)))))
      
   (define (j2s-new-fast cache clazz args)
      (with-access::J2SRef clazz (decl)
	 (let* ((len (length args))
		(fun (j2s-scheme clazz mode return conf hint))
		(fid (with-access::J2SDecl decl (id) (j2s-fast-id id)))
		(args (map (lambda (a)
			      (j2s-scheme a mode return conf hint))
			 args))
		(proto `(js-object-get-name/cache ,fun 'prototype
			   ,(js-pcache cache) %this))
		(obj (gensym '%obj)))
	    `(let ((,obj (js-object-alloc ,fun)))
		,(if (constructor-no-return? decl)
		     `(begin
			 (,fid ,obj ,@args)
			 ,obj)
		     `(js-new-return ,fun (,fid ,obj ,@args) ,obj))))))
   
   (with-access::J2SNew this (loc cache clazz args type)
      (cond
	 ((and (new-array? clazz)
	       (or (=fx (bigloo-debug) 0) (eq? type 'vector)))
	  (epairify loc
	     (j2s-new-array this mode return conf hint)))
	 ((and (=fx (bigloo-debug) 0) cache)
	  (epairify loc
	     (j2s-new-fast cache clazz args)))
	 (else
	  (epairify loc
	     (j2s-new loc (j2s-scheme clazz mode return conf hint)
		(j2s-scheme args mode return conf hint)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SReturnYield ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SReturnYield mode return conf hint)
   
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
	   %gen ,(j2s-scheme expr mode return conf hint)
	     ,(isa? kont J2SUndefined)
	     ,(if (identity-kont? kont)
		  #f
		  (j2s-scheme kont mode return conf hint))
	     %this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SKont ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SKont mode return conf hint)
   (with-access::J2SKont this (loc param exn body)
      (epairify loc
	 `(lambda (,(j2s-scheme param mode return conf hint)
		   ,(j2s-scheme exn mode return conf hint))
	     ,(j2s-scheme body mode return conf hint)))))

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
(define-method (j2s-scheme this::J2STilde mode return conf hint)
   (with-access::J2STilde this (loc stmt)
      (let* ((js-stmt (concat-tilde (j2s-js stmt #t #f mode return conf)))
	     (js (cond
		    ((null? js-stmt)
		     "")
		    ((null? (cdr js-stmt))
		     (car js-stmt))
		    ((every string? js-stmt)
		     (apply string-append js-stmt))
		    (else
		     `(string-append ,@js-stmt))))
	     (expr (j2s-tilde->expression this mode return conf)))
	 (epairify loc
	    `(instantiate::xml-tilde
		(lang 'javascript)
		(%js-expression ,expr)
		(body (vector
			 ',(if (>fx (bigloo-debug) 1) (j2s->list stmt) '())
			 '() '() '() ,js #f))
		(loc ',loc))))))

;*---------------------------------------------------------------------*/
;*    j2s-tilde->expression ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-tilde->expression this::J2STilde mode return conf)
   (with-access::J2STilde this (loc stmt)
      (let* ((temp (gensym))
	     (assign (j2s-stmt-assign stmt temp))
	     (js-stmt (concat-tilde (j2s-js assign #t #f mode return conf)))
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
(define-method (j2s-scheme this::J2SDollar mode return conf hint)
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
(define-method (j2s-scheme this::J2SOPTInitSeq mode return conf hint)
   
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
			  (j2s->list node)))))
	       (else
		(error "j2s-scheme" "wrong init expr"
		   (j2s->list node)))))))
   
   (with-access::J2SOPTInitSeq this (loc ref nodes cmap0 cmap1)
      (let ((%ref (gensym '%ref))
	    (cmap (gensym '%cmap0))
	    (i (gensym '%i))
	    (elements (gensym '%elements)))
	 `(let ((,%ref ,(j2s-scheme ref mode return conf hint)))
	     (with-access::JsObject ,%ref (cmap elements)
		(let ((,cmap cmap))
		   (if (or (eq? ,cmap ,cmap0) (eq? ,cmap ,cmap1))
		       ;; cache hit
		       (with-access::JsConstructMap ,cmap (props)
			  (let ((,i (vector-length props))
				(,elements elements))
			     ,@(map (lambda (init offset)
				       (j2s-scheme 
					  (init-expr init
					     (lambda (e)
						(with-access::J2SExpr e (loc)
						   `(vector-set! ,elements (+fx ,i ,offset) ,e))))	
				  mode return conf hint))
				  nodes (iota (length nodes)))
			     (with-access::JsObject ,%ref ((omap cmap))
				(set! omap ,cmap1))))
		       ;; cache miss
		       (with-access::JsConstructMap ,cmap (props)
			  (let ((len0 (vector-length props)))
			     ,@(map (lambda (n)
				       (j2s-scheme n mode return conf hint))
				  nodes)
			     (with-access::JsConstructMap cmap (props)
				(when (=fx (+fx len0 ,(length nodes))
					 (vector-length props))
				   (set! ,cmap0 ,cmap)
				   (set! ,cmap1 cmap))))))))))))

;*---------------------------------------------------------------------*/
;*    throw? ...                                                       */
;*---------------------------------------------------------------------*/
(define (throw? node)
   (let ((cell (make-cell #f)))
      (canthrow node '() cell)))

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
	  
      
