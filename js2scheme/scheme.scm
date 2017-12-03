;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 11:47:51 2013                          */
;*    Last change :  Sun Dec  3 22:45:02 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
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
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-ops
	   __js2scheme_scheme-test
	   __js2scheme_scheme-class
	   __js2scheme_scheme-string
	   __js2scheme_scheme-math
	   __js2scheme_scheme-date
	   __js2scheme_scheme-array)
   
   (export j2s-scheme-stage
	   j2s-scheme-eval-stage
	   (generic j2s-scheme ::obj ::symbol ::procedure ::obj ::obj ::symbol)))

;*---------------------------------------------------------------------*/
;*    inline-method ...                                                */
;*---------------------------------------------------------------------*/
(define-struct inline-method jsname met ttype args %this)

;*---------------------------------------------------------------------*/
;*    j2s-inline-methods ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-inline-methods
   ;; jsname, scmname, (this.types), [optional-args] %this
   (map (lambda (e)
	   (apply inline-method e))
      `(;; string methods
	("fromCharCode" js-jsstring-fromcharcode String (any) %this)
	("charAt" js-jsstring-charat string (any) %this)
	("charAt" js-jsstring-maybe-charat any (any) %this)
	("charCodeAt" js-jsstring-charcodeat string (any) %this)
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
;*    cast-table ...                                                   */
;*---------------------------------------------------------------------*/
(define cast-table
   ;; from x to -> conv
   `((int29
	(number js-uint29-tointeger)
	(any js-uint29-tointeger))
     (uint29
	((int32 ,js-uint32->int32)
	 (uint32 ,js-id)
	 (number js-uint29-tointeger)
	 (any js-uint29-tointeger)))
     (uint32
	((uint29 ,js-id)
	 (int32 ,js-uint32->int32)
	 (number js-uint32-tointeger)
	 (any js-uint32-tointeger)))
     (index
	((uint29 ,js-id)
	 (int32 ,js-uint32->int32)
	 (number js-uint32-tointeger)
	 (any js-uint32-tointeger)))
     (int30
	((int32 ,js-id)
	 (number js-int32-tointeger)
	 (any js-int32-tointeger)))
     (int32
	((int30 ,js-id)
	 (uint32 ,js-int32->uint32)
	 (number js-int32-tointeger)
	 (any js-int32-tointeger)))
     (int53
	((int30 ,js-id)
	 (uint32 ,js-int53->uint32)
	 (int32 ,js-int53->uint32)
	 (number js-int53-tointeger)
	 (any js-int53-tointeger)))))

;; cast ancillary functions
(define (js-id v)
   v)
(define (js-uint32->int32 v)
   (if (uint32? v) (uint32->int32 v) `(uint32->int32 ,v)))
(define (js-int32->uint32 v)
   (if (int32? v) (int32->uint32 v) `(int32->uint32 ,v)))
(define (js-int53->int32 v)
   (tprint "TODO")
   (if (number? v) (number->int32 v) `(number->int32 ,v)))
(define (js-int53->uint32 v)
   (tprint "TODO")
   (if (number? v) (number->uint32 v) `(number->uint32 ,v)))

;*---------------------------------------------------------------------*/
;*    cast ...                                                         */
;*---------------------------------------------------------------------*/
(define (cast expr conf from to)
   
   (define (err)
      (error "cast" (format "illegal cast ~a -> ~a" from to) expr))
   
   (define (tostring expr)
      (match-case expr
	 ((js-jsstring-ref ?str ?idx)
	  (set-car! expr 'js-jsstring-ref-as-string)
	  expr)
	 ((js-string-ref ?str ?idx ?this)
	  (set-car! expr 'js-string-ref-as-string)
	  expr)
	 (else
	  `(js-tojsstring ,expr %this))))
   
   (define (default)
      ;;(tprint "cast expr=" expr " from=" from " to=" to)
      (if (or (eq? from to) (eq? to '*))
	  expr
	  (case from
	     ((uint29)
	      (case to
		 ((uint32 index length) (fixnum->uint32 expr))
		 ((bool) `(>u32 ,expr #u32:0))
		 (else expr)))
	     ((index uint32 length)
	      (case to
		 ((uint32 index length) expr)
		 ((bool) `(> ,expr 0))
		 (else expr)))
	     ((int30)
	      (case to
		 ((index uint32 length) (fixnum->uint32 expr))
		 ((bool) `(not (= ,expr 0)))
		 (else expr)))
	     ((int53)
	      (case to
		 ((index uint32 length) (err))
		 ((bool) `(not (= ,expr 0)))
		 (else expr)))
	     ((fixnum)
	      (case to
		 ((index uint32 length) (js-fixnum->uint32 expr))
		 ((bool) `(not (=fx ,expr 0)))
		 (else expr)))
	     ((integer number)
	      (case to
		 ((index uint32 length) (js-fixnum->uint32 expr))
		 ((bool) `(not (= ,expr 0)))
		 (else expr)))
	     (else
	      (case to
		 ((index uint32 length) (js-fixnum->uint32 expr))
		 ((string) (tostring expr))
		 ((bool) `(js-totest ,expr))
		 (else expr))))))

   (let ((fen (assq from cast-table)))
      (if (pair? fen)
	  (let ((ten (assq to (cadr fen))))
	     (if (pair? ten)
		 (if (symbol? (cadr ten))
		     `(,(cadr ten) ,expr)
		     ((cadr ten) expr))
		 (default)))
	  (default))))
	  
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
		  '()
		  'any)))))

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
		  '()
		  'any)))))

;*---------------------------------------------------------------------*/
;*    comp-return ...                                                  */
;*---------------------------------------------------------------------*/
(define (comp-return x)
   x)

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
;*    j2s-+fx ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-+fx x y conf)
   (case (config-get conf :long-size 0)
      ((32)
       `(js+fx32 ,x ,y))
      ((64)
       `(js+fx64 ,x ,y))
      (else
       `(js+fx ,x ,y))))

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
(define (j2s-nodes*::pair-nil loc nodes mode return conf hint totype)
   
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
	  (let ((sexp (j2s-scheme (car nodes) mode return conf hint totype)))
	     (match-case sexp
		((begin . (and (? pair?) ?sexps))
		 sexps)
		(else
		 (epairify loc 
		    (list (return sexp)))))))
	 ((undefined? (car nodes))
	  (loop (cdr nodes)))
	 (else
	  (let ((sexp (j2s-scheme (car nodes) mode return conf hint totype)))
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
(define-generic (j2s-scheme this mode return::procedure conf hint totype::symbol)
   (if (pair? this)
       (map (lambda (e) (j2s-scheme e mode return conf hint totype)) this)
       this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SProgram ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SProgram mode return conf hint totype)
   
   (define (exit-body body)
      (if (config-get conf :return-as-exit)
	  `((bind-exit (%jsexit) ,@body))
	  body))
   
   (define (%cnsts-debug cnsts)
      `(vector
	  ,@(map (lambda (n)
		    (let ((s (j2s-scheme n mode return conf hint totype)))
		       (if (isa? n J2SRegExp)
			   (with-access::J2SRegExp n (loc val flags inline)
			      (if inline
				  `(with-access::JsRegExp ,s (rx) rx)
				  s))
			   s)))
	       cnsts)))
   
   (define (%cnsts-intext cnsts)
      
      (define %this
	 '(js-new-global-object))
      
      (define (j2s-constant this::J2SLiteralValue)
	 (cond
	    ((isa? this J2SString)
	     (with-access::J2SString this (val)
		(vector 0 val)))
	    ((isa? this J2SRegExp)
	     (with-access::J2SRegExp this (loc val flags inline)
		(vector (if inline 3 1) val flags)))
	    ((isa? this J2SCmap)
	     (with-access::J2SCmap this (val)
		(vector 2 val)))
	    (else
	     (error "j2s-constant" "wrong literal" this))))
      
      `(js-constant-init
	  ,(obj->string (list->vector (map j2s-constant cnsts))) %this))
   
   (define (%cnsts cnsts)
      ;; this must be executed after the code is compiled as this
      ;; compilation might change or add new constants.
      (if (>fx (bigloo-debug) 0)
	  (%cnsts-debug cnsts)
	  (%cnsts-intext cnsts)))
   
   (define (define-pcache pcache-size)
      `(%define-pcache ,pcache-size))
   
   (define (j2s-module module body)
      (with-access::J2SProgram this (mode pcache-size cnsts globals)
	 (list
	    module
	    (define-pcache pcache-size)
	    `(define %pcache (js-make-pcache ,pcache-size))
	    '(define %source (or (the-loading-file) "/"))
	    '(define %resource (dirname %source))
	    `(define (hopscript %this this %scope %module)
		(define %worker (js-current-worker))
		(define %cnsts ,(%cnsts cnsts))
		,@globals
		,@(exit-body body))
	    ;; for dynamic loading
	    'hopscript)))
   
   (define (j2s-main-module name body)
      (let ((module `(module ,(string->symbol name)
			(eval (library hop)
			   (library hopscript)
			   (library nodejs))
			(library hop hopscript nodejs)
			(cond-expand (enable-libuv (library libuv)))
			(main main))))
	 (with-access::J2SProgram this (mode pcache-size %this path cnsts globals)
	    (list
	       module
	       (define-pcache pcache-size)
	       '(hop-sofile-compile-policy-set! 'static)
	       `(define %pcache (js-make-pcache ,pcache-size))
	       '(hopjs-standalone-set! #t)
	       `(define %this (nodejs-new-global-object))
	       `(define %source ,path)
	       '(define %resource (dirname %source))
	       `(define (main args)
		   (define %worker
		      (js-init-main-worker! %this #f nodejs-new-global-object))
		   (hopscript-install-expanders!)
		   (bigloo-library-path-set! ',(bigloo-library-path))
		   (js-worker-push-thunk! %worker "nodejs-toplevel"
		      (lambda ()
			 (define %scope (nodejs-new-scope-object %this))
			 (define this
			    (with-access::JsGlobalObject %this (js-object)
			       (js-new0 %this js-object)))
			 (define %module
			    (nodejs-module ,(basename path) ,path %worker %this))
			 (define %cnsts ,(%cnsts cnsts))
			 ,@globals
			 ,@(exit-body body)))
		   (when (string-contains (or (getenv "HOPTRACE") "")
			    "hopscript:cache")
		      (log-cache-miss!)
		      (register-exit-function!
			 (lambda (n)
			    (show-cache-misses)
			    n)))
		   (when (string-contains (or (getenv "HOPTRACE") "")
			    "hopscript:function")
		      (log-function! ,(config-get conf :profile #f))
		      (register-exit-function!
			 (lambda (n)
			    (show-functions)
			    n)))
		   (when (string-contains (or (getenv "HOPTRACE") "")
			    "hopscript:alloc")
		      (register-exit-function!
			 (lambda (n)
			    (show-allocs)
			    n)))
		   (thread-join! (thread-start-joinable! %worker)))))))
   
   (with-access::J2SProgram this (module main nodes headers decls
					 mode name pcache-size cnsts globals)
      (let ((body (flatten-nodes
		     (append
			(j2s-scheme headers mode return conf hint totype)
			(j2s-scheme decls mode return conf hint totype)
			(j2s-scheme nodes mode return conf hint totype)))))
	 (cond
	    (module
	     ;; a module whose declaration is in the source
	     (j2s-module module body))
	    ((not name)
	     ;; a mere expression
	     `(lambda (%this this %scope %module)
		 ,(define-pcache pcache-size)	       
		 (define %pcache (js-make-pcache ,pcache-size))
		 (define %worker (js-current-worker))
		 (define %source (or (the-loading-file) "/"))
		 (define %resource (dirname %source))
		 (define %cnsts ,(%cnsts cnsts))
		 ,@globals
		 ,@(exit-body body)))
	    (main
	     ;; generate a main hopscript module
	     (j2s-main-module name body))
	    (else
	     ;; generate the module clause
	     (let ((module `(module ,(string->symbol name)
			       (library hop hopscript js2scheme nodejs)
			       (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))))
		(j2s-module module body)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SVarDecls ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SVarDecls mode return conf hint totype)
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
(define-method (j2s-scheme this::J2SDecl mode return conf hint totype)
   
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
(define-method (j2s-scheme this::J2SDeclInit mode return conf hint totype)
   
   (define (j2s-scheme-var this)
      (with-access::J2SDeclInit this (loc val writable)
	 (let ((ident (j2s-decl-scheme-id this)))
	    (epairify loc
	       (if writable
		   `(begin
		       (set! ,ident ,(j2s-scheme val mode return conf hint totype))
		       (js-undefined))
		   `(begin
		       ,(j2s-scheme val mode return conf hint totype)
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
(define (j2s-scheme-set! lhs val result mode return conf init? loc)
   
   (define (set decl hint utype loc)
      (cond
	 ((not (and (j2s-let? decl) (not (j2s-let-opt? decl))))
	  `(set! ,(j2s-scheme lhs mode return conf hint utype) ,val))
	 (init?
	  `(set! ,(j2s-decl-scheme-id decl) ,val))
	 (else
	  `(js-let-set! ,(j2s-decl-scheme-id decl) ,val ',loc %this))))
   
   (with-access::J2SRef lhs (decl)
      (cond
	 ((isa? lhs J2SRef)
	  (with-access::J2SDecl decl (writable immutable scope id hint utype)
	     (cond
		((or writable (and (isa? decl J2SDeclInit) (not immutable)))
		 (cond
		    ((and (memq scope '(global %scope)) (in-eval? return))
		     `(begin
			 ,(j2s-put! loc '%scope (j2s-type lhs) `',id
			     val (strict-mode? mode) #f)
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
		 val))))
	 ((not result)
	  (with-access::J2SDecl decl (loc)
	     (set decl '() 'any loc)))
	 (else
	  (with-access::J2SDecl decl (loc)
	     `(begin
		 ,(set decl '() 'any loc)
		 ,result))))))
	      
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclExtern ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclExtern mode return conf hint totype)
   (with-access::J2SDeclExtern this (loc id name val bind writable)
      (cond
	 (bind
          (j2s-scheme-decl this (j2s-scheme val mode return conf hint totype)
	     writable mode return conf))
	 (else
	  (j2s-scheme val mode return conf hint totype)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCast ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCast mode return conf hint totype)
   (with-access::J2SCast this (expr (to type))
      (with-access::J2SExpr expr ((from type))
	 (cast (j2s-scheme expr mode return conf hint totype) conf from to))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRef mode return conf hint totype)
   (with-access::J2SRef this (decl loc)
      (with-access::J2SDecl decl (scope id utype)
	 (cond
	    ((j2s-let-opt? decl)
	     (j2s-decl-scheme-id decl))
	    ((j2s-let? decl)
	     `(js-let-ref ,(j2s-decl-scheme-id decl) ',id ',loc %this))
	    ((and (memq scope '(global %scope)) (in-eval? return))
	     `(js-global-object-get-name %scope ',id #f %this))
	    (else
	     (j2s-decl-scheme-id decl))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSuper ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSuper mode return conf hint totype)
   (with-access::J2SSuper this (decl loc clazz)
      (if (eq? clazz '__proto__)
	  `(js-super ,(call-next-method) ',loc %this)
	  '%super)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWithRef ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWithRef mode return conf hint totype)
   (with-access::J2SWithRef this (id withs expr loc)
      (epairify loc
	 (let loop ((withs withs))
	    (if (null? withs)
		(j2s-scheme expr mode return conf hint totype)
		`(if ,(j2s-in? loc `',id (car withs))
		     ,(j2s-get loc (car withs) 'object `',id 'string #f)
		     ,(loop (cdr withs))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SHopRef ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SHopRef mode return conf hint totype)
   (with-access::J2SHopRef this (id)
      id))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SThis ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SThis mode return conf hint totype)
   (with-access::J2SThis this (loc type decl)
      (let ((id (j2s-decl-scheme-id decl)))
	 (if (and (j2s-let? decl) (not (j2s-let-opt? decl)))
	     `(js-let-ref ,id ,id ',loc %this)
	     id))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCond ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCond mode return conf hint totype)
   (with-access::J2SCond this (loc test then else)
      (epairify loc
	 `(if ,(j2s-test test mode return conf)
	      ,(j2s-scheme then mode return conf hint totype)
	      ,(j2s-scheme else mode return conf hint totype)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SComprehension ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SComprehension mode return conf hint totype)
   (with-access::J2SComprehension this (loc test expr decls iterables)
      (if (not (strict-mode? mode))
	  (match-case loc
	     ((at ?fname ?loc)
	      `(with-access::JsGlobalObject %this (js-syntax-error)
		  (js-raise
		     (js-new %this js-syntax-error
			,(j2s-jsstring
			    "comprehension only supported in strict mode"
			    loc)
			,fname ,loc))))
	     (else
	      `(with-access::JsGlobalObject %this (js-syntax-error)
		  (js-raise
		     (js-new %this js-syntax-error
			,(j2s-jsstring
			    "comprehension only supported in strict mode"
			    loc))))))
	  (let* ((names (map j2s-decl-scheme-id decls))
		 (iters (map (lambda (iter)
				(j2s-scheme iter mode return conf hint totype))
			   iterables))
		 (fun `(lambda (this ,@names)
			  ,(j2s-scheme expr mode return conf hint totype)))
		 (ast-pred (call-with-output-string
			      (lambda (op) (ast->json test op))))
		 (ast-expr (call-with-output-string
			      (lambda (op) (ast->json expr op))))
		 (ast-decls (map (lambda (decl)
				    (call-with-output-string
				       (lambda (op) (ast->json decl op))))
			       decls)))
	     (epairify loc
		(if (not (isa? test J2SBool))
		    (let ((test `(lambda (this ,@names)
				    ,(j2s-scheme test mode return conf hint totype))))
		       `(js-array-comprehension %this (list ,@iters)
			   ,fun ,test
			   ',names ,ast-pred ,ast-expr (list ,@ast-decls)))
		    (with-access::J2SBool test (val)
		       (if val
			   `(js-array-comprehension %this (list ,@iters)
			       ,fun #t
			       ',names ,ast-pred ,ast-expr (list ,@ast-decls))
			   `(js-empty-vector->jsarray %this)))))))))

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
(define-method (j2s-scheme this::J2SUnresolvedRef mode return conf hint totype)
   (with-access::J2SUnresolvedRef this (loc cache id)
      (epairify loc
	 (j2s-unresolved id cache loc))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SArrayAbsent ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArrayAbsent mode return conf hint totype)
   (with-access::J2SArrayAbsent this (loc)
      (epairify loc '(js-absent))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLiteralValue ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLiteralValue mode return conf hint totype)
   (with-access::J2SLiteralValue this (val type)
      (cond
	 ((number? val) (error "j2s-scheme" "should not find a number here" val))
	 (else val))))

;*---------------------------------------------------------------------*/
;*    *int29* ...                                                      */
;*---------------------------------------------------------------------*/
(define *+ints29* (-s32 (bit-lshs32 #s32:1 29) #s32:1))
(define *-ints29* (-s32 #s32:0 (bit-lshs32 #s32:1 29)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNumber ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNumber mode return conf hint totype)
   (with-access::J2SNumber this (val type)
      (cond
	 ((memq type '(index length uint32 uint29 ufixnum))
	  (if (flonum? val)
	      (flonum->uint32 val)
	      (fixnum->uint32 val)))
	 ((memq type '(int29 int30 int32 fixnum))
	  (if (flonum? val) (flonum->int32 val) (fixnum->int32 val)))
	 ((fixnum? val)
	  (cond
	     ((m64? conf) val)
	     ((and (>= val *+ints29*) (<= val *-ints29*)) val)
	     (else (fixnum->flonum val))))
	 ((nanfl? val)
	  "NaN")
	 (else
	  val))))

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
	      (j2s-scheme this mode return conf '() 'any))))
      (else
       (j2s-scheme this mode return conf '() 'any))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLiteralCnst ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLiteralCnst mode return conf hint totype)
   (with-access::J2SLiteralCnst this (index val)
      (if (isa? val J2SRegExp)
	  ;; regexp are hybrid, the rx part is precompiled but the
	  ;; JS object is dynamically allocated
 	  `(let ((rx (vector-ref-ur %cnsts ,index)))
	      (let ((nrx (duplicate::JsRegExp rx)))
		 (js-object-mode-set! nrx (js-object-default-mode))
		 (js-object-properties-set! nrx
		    (list-copy (js-object-properties rx)))
		 nrx))
	  `(vector-ref-ur %cnsts ,index))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2STemplate ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2STemplate mode return conf hint totype)
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
					,(j2s-scheme expr mode return conf hint totype)
					%this)))))
		     exprs))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNativeString ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNativeString mode return conf hint totype)
   (with-access::J2SNativeString this (loc val)
      val))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SString ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SString mode return conf hint totype)
   (with-access::J2SString this (loc val)
      (j2s-jsstring val loc)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRegExp ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRegExp mode return conf hint totype)
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
(define-method (j2s-scheme this::J2SCmap mode return conf hint totype)
   (with-access::J2SCmap this (loc val)
      (epairify loc
	 `(js-names->cmap ',val))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNull ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNull mode return conf hint totype)
   (with-access::J2SLiteral this (loc)
      (epairify loc '(js-null))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUndefined ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUndefined mode return conf hint totype)
   (with-access::J2SLiteral this (loc)
      (epairify loc '(js-undefined))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SReturn mode return conf hint totype)
   (with-access::J2SReturn this (loc expr tail exit lbl)
      (cond
	 (exit
	  (epairify loc
	     `(%jsexit ,(j2s-scheme expr mode return conf hint totype))))
	 (tail
	  (j2s-scheme expr mode return conf hint totype))
	 (else
	  (epairify loc
	     `(,(or lbl '%return)
	       ,(j2s-scheme expr mode return conf hint totype)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBindExit ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBindExit mode return conf hint totype)
   (with-access::J2SBindExit this (lbl stmt loc)
      (epairify loc
	 `(bind-exit (,lbl)
	     ,(j2s-scheme stmt mode return conf hint totype)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SThrow ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SThrow mode return conf hint totype)
   (with-access::J2SThrow this (loc expr)
      (epairify loc
	 (if (> (bigloo-debug) 0)
	     `(js-throw/debug ,(j2s-scheme expr mode return conf hint totype)
		 ,(j2s-jsstring (cadr loc) loc) ,(caddr loc) %worker)
	     `(js-throw ,(j2s-scheme expr mode return conf hint totype)
		 ,(j2s-jsstring (cadr loc) loc) ,(caddr loc))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2STry ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2STry mode return conf hint totype)
   (with-access::J2STry this (loc body catch finally)
      (epairify-deep loc
	 (let* ((trybody (j2s-scheme body mode return conf hint totype))
		(trie (if (isa? catch J2SNop)
			  (j2s-scheme body mode return conf hint totype)
			  (with-access::J2SCatch catch (loc param body)
			     (epairify-deep loc
				`(with-handler
				    (lambda (,(j2s-scheme param mode return conf hint totype))
				       ,(j2s-scheme body mode return conf hint totype))
				    ,trybody))))))
	    (if (isa? finally J2SNop)
		trie
		`(unwind-protect
		    ,trie
		    ,(j2s-scheme finally mode return conf hint totype)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWith ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWith mode return conf hint totype)
   (with-access::J2SWith this (obj block id)
      `(let ((,id (js-toobject %this ,(j2s-scheme obj mode return conf hint totype))))
	  ,(j2s-scheme block mode return conf hint totype))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPragma ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPragma mode return conf hint totype)
   (with-access::J2SPragma this (loc expr lang)
      (if (eq? lang 'scheme)
	  (epairify-deep loc expr)
  "#unspecified")))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSequence mode return conf hint totype)
   (with-access::J2SSequence this (loc exprs)
      (let loop ((exprs exprs))
	 (cond
	    ((null? (cdr exprs))
	     (j2s-scheme (car exprs) mode return conf hint totype))
	    ((isa? (car exprs) J2SUndefined)
	     (loop (cdr exprs)))
	    (else
	     (epairify loc
		(flatten-begin
		   (j2s-scheme exprs mode return conf hint totype))))))))

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
	     `(define ,ident ,(j2s-scheme val mode return conf hint 'any)))
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
(define-method (j2s-scheme this::J2SLetBlock mode return conf hint totype)
   
   (define (j2s-let-decl-inner::pair-nil d::J2SDecl mode return conf singledecl)
      (with-access::J2SDeclInit d (val usage id vtype ronly utype)
	 (let* ((ident (j2s-decl-scheme-id d))
		(var (type-ident ident vtype)))
	    (cond
	       ((or (not (isa? val J2SFun)) (isa? val J2SSvc) (memq 'assig usage))
		(list `(,var ,(j2s-scheme val mode return conf hint utype))))
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
	     `(begin ,@(j2s-nodes* loc nodes mode return conf hint totype))))
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
			       (j2s-scheme d mode return conf hint totype))
			      (else
			       (with-access::J2SDecl d (scope)
				  (set! scope 'global))
			       (j2s-scheme d mode return conf hint totype))))
		      decls)
		 ,@(j2s-scheme nodes mode return conf hint totype))))
	 (else
	  ;; inner letblock, create a let block
	  (let ((ds (append-map (lambda (d)
				   (cond
				      ((j2s-let-opt? d)
				       (j2s-let-decl-inner d
					  mode return conf
					  (null? (cdr decls))))
				      ((isa? d J2SDeclFun)
				       (j2s-scheme d mode return conf hint totype))
				      (else
				       (list (j2s-scheme d mode return conf hint totype)))))
		       decls))
		(body (j2s-nodes* loc nodes mode return conf hint totype)))
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
(define-method (j2s-scheme this::J2SParen mode return conf hint totype)
   (with-access::J2SParen this (expr type)
      (j2s-scheme expr mode return conf (list type) type)))

;*---------------------------------------------------------------------*/
;*    j2s-stmt-sans-begin ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-stmt-sans-begin::pair this::J2SStmt mode return conf hint totype)
   (let ((sexp (j2s-scheme this mode return conf hint totype)))
      (match-case sexp
	 ((begin . ?sexps) sexps)
	 (else (list sexp)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SStmt ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12           */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SStmt mode return conf hint totype)
   (return this))

;*---------------------------------------------------------------------*/
;*    j2s-sequence ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-sequence loc nodes::pair-nil mode return conf hint totype)
   
   (define (undefined? stmt::J2SStmt)
      (cond
	 ((isa? stmt J2SStmtExpr)
	  (with-access::J2SStmtExpr stmt (expr)
	     (or (isa? expr J2SUndefined)
		 (and (isa? expr J2SLiteral) (not (isa? expr J2SArray))))))
	 ((isa? stmt J2SNop)
	  #t)))
   
   (let loop ((nodes nodes))
      (cond
	 ((null? nodes)
	  '())
	 ((not (pair? (cdr nodes)))
	  (j2s-scheme (car nodes) mode return conf hint totype))
	 ((undefined? (car nodes))
	  (loop (cdr nodes)))
	 (else
	  (epairify loc
	     (flatten-begin
		(j2s-scheme nodes mode return conf hint totype)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SMeta ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SMeta mode return conf hint totype)
   (with-access::J2SMeta this (stmt optim)
      (if (=fx optim 0)
	  `(%%noinline
	      ,(j2s-scheme stmt mode return (cons* :optim 0 conf) hint totype))
	  (j2s-scheme stmt mode return conf hint totype))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSeq ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.1         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSeq mode return conf hint totype)
   (with-access::J2SSeq this (loc nodes)
      (j2s-sequence loc nodes mode return conf hint totype)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBlock ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBlock mode return conf hint totype)

   (define (begin-or-let loc bindings nodes)
      (if (null? bindings)
	  (j2s-sequence loc nodes mode return conf hint totype)
	  (epairify loc
	     `(let ,(reverse! bindings)
		 ,@(j2s-nodes* loc nodes mode return conf hint totype)))))
   
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
			  (cons (j2s-scheme (car nodes) mode return conf hint totype)
			     bindings)))
		    (begin-or-let loc bindings nodes))))
	    (else
	     (begin-or-let loc bindings nodes))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNop ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.3         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNop mode return conf hint totype)
   (with-access::J2SNop this (loc)
      (epairify loc
	 (return '(js-undefined)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SStmtExpr ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.4         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SStmtExpr mode return conf hint totype)
   (with-access::J2SStmtExpr this (expr)
      (let ((exprs (j2s-scheme expr mode return conf hint totype)))
	 (match-case exprs
	    ((begin ?expr (js-undefined))
	     expr)
	    ((begin ?expr (? symbol?))
	     expr)
	    ((begin (and ?l ??-) (js-undefined))
	     `(begin ,@l))
	    (else
	     exprs)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SExprStmt ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SExprStmt mode return conf hint totype)
   (with-access::J2SExprStmt this (stmt)
      (j2s-scheme stmt mode return conf hint totype)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SIf ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.5         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SIf mode return conf hint totype)
   (with-access::J2SIf this (loc test then else)
      (let ((tmp (gensym)))
	 (epairify loc
	    (if (isa? else J2SNop)
		`(if ,(j2s-test test mode return conf)
		     ,(j2s-scheme then mode return conf hint totype)
		     (js-undefined))
		`(if ,(j2s-test test mode return conf)
		     ,(j2s-scheme then mode return conf hint totype)
		     ,(j2s-scheme else mode return conf hint totype)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPrecache ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPrecache mode return conf hint totype)

   (define (precache-access this::J2SAccess)
      (with-access::J2SAccess this (obj field cache)
	 (let* ((scmobj (j2s-scheme obj mode return conf hint totype))
		(precache `(eq? (with-access::JsObject ,scmobj (cmap) cmap)
			      (js-pcache-cmap ,(js-pcache cache)))))
	    (with-access::J2SRef obj (type)
	       (if (eq? type 'object)
		   precache
		   `(and (isa? ,scmobj JsObject) ,precache))))))
   
   (define (precache-test this)
      (with-access::J2SPrecache this (accesses)
	 (let loop ((nodes accesses))
	    (let ((n (car nodes)))
	       (if (null? (cdr nodes))
		   (precache-access n)
		   `(and ,(precache-access (car n)) ,(loop (cdr nodes))))))))
   
   (with-access::J2SPrecache this (loc accesses then else)
      (let ((tmp (gensym)))
	 (epairify loc
	    `(if ,(precache-test this)
		 ,(j2s-scheme then mode return conf hint totype)
		 ,(j2s-scheme else mode return conf hint totype))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDo ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.1       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDo mode return conf hint totype)
   (with-access::J2SDo this (loc test body id
			       need-bind-exit-break need-bind-exit-continue)

      (define (comp-loop loop)
	 `(let ,loop ()
	       ,(if need-bind-exit-continue
		    (epairify-deep loc
		       `(bind-exit (,(escape-name '%continue id))
			   ,@(j2s-stmt-sans-begin body mode return conf hint totype)))
		    (j2s-scheme body mode return conf hint totype))
	       (if ,(j2s-test test mode return conf)
		   (,loop)
		   (js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       ,(if need-bind-exit-continue
		    (epairify-deep loc
		       `(bind-exit (,(escape-name '%continue id))
			   ,@(j2s-stmt-sans-begin body mode acc-return conf hint totype)))
		    (j2s-scheme body mode acc-return conf hint totype))
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
(define-method (j2s-scheme this::J2SWhile mode return conf hint totype)
   (with-access::J2SWhile this (loc test body id
				  need-bind-exit-break need-bind-exit-continue)

      (define (comp-loop loop)
	 `(let ,loop ()
	       (if ,(j2s-test test mode return conf)
		   ,(if need-bind-exit-continue
			(epairify-deep loc
			   `(begin
			       (bind-exit (,(escape-name '%continue id))
				  ,@(j2s-stmt-sans-begin body mode return conf hint totype))
			       (,loop)))
			(epairify-deep loc
			   `(begin
			       ,@(j2s-stmt-sans-begin body mode return conf hint totype)
			       (,loop))))
		   (js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       (if ,(j2s-test test mode return conf)
		   ,(if need-bind-exit-continue
			(epairify-deep loc
			   `(begin
			       (bind-exit (,(escape-name '%continue id))
				  ,@(j2s-stmt-sans-begin body mode acc-return conf hint totype))
			       (,loop %acc)))
			(epairify-deep loc
			   `(begin
			       ,@(j2s-stmt-sans-begin body mode acc-return conf hint totype)
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
(define-method (j2s-scheme this::J2SFor mode return conf hint totype)
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
				  ,(j2s-scheme body mode return conf hint totype)))
			   (j2s-scheme body mode return conf hint totype))
		      ,(j2s-scheme incr mode return conf 'void 'void)
		      (,loop))
		   (js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       (if ,(j2s-test test mode return conf)
		   (begin
		      ,(if need-bind-exit-continue
			   (epairify-deep loc
			      `(bind-exit (,(escape-name '%continue id))
				  ,(j2s-scheme body mode acc-return conf hint totype)))
			   (j2s-scheme body mode acc-return conf hint totype))
		      ,(j2s-scheme incr mode return conf 'void 'void)
		      (,loop %acc))
		   %acc)))

      (let* ((forid (gensym 'for))
	     (loop (if (in-eval? return) (eval-loop forid) (comp-loop forid))))
	 (epairify-deep loc
	    `(begin
		,@(if (isa? init J2SNop)
		      '()
		      (list (j2s-scheme init mode return conf hint totype)   ))
		,(if need-bind-exit-break
		     (epairify-deep loc
			`(bind-exit (,(escape-name '%break id)) ,loop))
		     (epairify loc loop)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SForIn ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.4       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SForIn mode return conf hint totype)

   (define (for-in/break-comp tmp name props obj body set)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(let ((%acc (js-undefined)))
			(js-for-in ,(j2s-scheme obj mode return conf hint totype)
			   (lambda (,name)
			      ,set
			      ,(if need-bind-exit-continue
				   `(bind-exit (,(escape-name '%continue id))
				       ,(j2s-scheme body mode acc-return conf hint totype))
				   (j2s-scheme body mode acc-return conf hint totype)))
			   %this)
			%acc)))
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,for)
		for))))

   (define (for-in/break-eval tmp name props obj body set)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(js-for-in ,(j2s-scheme obj mode return conf hint totype)
			(lambda (,name)
			   ,set
			   ,(if need-bind-exit-continue
				`(bind-exit (,(escape-name '%continue id))
				    ,(j2s-scheme body mode return conf hint totype))
				(j2s-scheme body mode return conf hint totype)))
			%this)))
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,for)
		for))))

   (define (for-in/break tmp name props obj body set)
      (if (in-eval? return)
	  (for-in/break-eval tmp name props obj body set)
	  (for-in/break-comp tmp name props obj body set)))

   (define (for-in/w-break-comp tmp name props obj body set)
      `(js-for-in ,(j2s-scheme obj mode return conf hint totype)
	  (lambda (,name)
	     ,set
	     ,(j2s-scheme body mode return conf hint totype))
	  %this))

   (define (for-in/w-break-eval tmp name props obj body set)
      `(let ((%acc (js-undefined)))
	  (js-for-in ,(j2s-scheme obj mode return conf hint totype)
	     (lambda (,name)
		,set
		,(j2s-scheme body mode acc-return conf hint totype))
	     %this)
	  %acc))

   (define (for-in/w-break tmp name props obj body set)
      (if (in-eval? return)
	  (for-in/w-break-eval tmp name props obj body set)
	  (for-in/w-break-comp tmp name props obj body set)))

   (define (set lhs name loc)
      (let loop ((lhs lhs))
	 (cond
	    ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
	     (epairify loc (j2s-scheme-set! lhs name #f mode return conf #f loc)))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(epairify loc
		   (j2s-unresolved-put! `',id name #f mode return))))
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field loc)
		(epairify loc
		   (j2s-put! loc (j2s-scheme obj mode return conf hint totype)
		      (typeof-this obj conf)
		      (j2s-scheme field mode return conf hint totype)
		      name
		      (strict-mode? mode)
		      #f))))
	    ((isa? lhs J2SWithRef)
	     (with-access::J2SWithRef lhs (id withs expr loc)
		(epairify loc
		   (let liip ((withs withs))
		      (if (null? withs)
			  (loop expr)
			  `(if ,(j2s-in? loc `',id (car withs))
			       ,(j2s-put! loc (car withs)
				   'object
				   (symbol->string id)
				   name #f #f)
			       ,(liip (cdr withs))))))))
	    (else
	     (j2s-error "j2sscheme" "Illegal lhs" this)))))
   
   (with-access::J2SForIn this (loc lhs obj body
				  need-bind-exit-break need-bind-exit-continue)
      (let* ((tmp (gensym))
	     (name (gensym))
	     (props (gensym))
	     (set (set lhs name loc)))
	 (epairify-deep loc
	    (if (or need-bind-exit-continue need-bind-exit-break)
		(for-in/break tmp name props obj body set)
		(for-in/w-break tmp name props obj body set))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLabel ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLabel mode return conf hint totype)
   (with-access::J2SLabel this (body need-bind-exit-break id)
      (if need-bind-exit-break
	  `(bind-exit (,(escape-name '%break id)) 
	      ,(j2s-scheme body mode return conf hint totype))
	  (j2s-scheme body mode return conf hint totype))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBreak ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBreak mode return conf hint totype)
   (with-access::J2SBreak this (loc target)
      (with-access::J2SIdStmt target (id)
	 (epairify loc
	    `(,(escape-name '%break id)
	      ,(if (in-eval? return) '%acc '(js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SContinue ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SContinue mode return conf hint totype)
   (with-access::J2SContinue this (loc target)
      (with-access::J2SLoop target (id)
	 (epairify loc
	    `(,(escape-name '%continue id)
	      ,(if (in-eval? return) '%acc '(js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSwitch ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSwitch mode return conf hint totype)
   (with-access::J2SSwitch this (loc key cases id need-bind-exit-break)
      
      (define (test-switch tleft tright)
	 (if (and (memq tleft '(number integer)) (memq tright '(number integer)))
	     '=
	     'js-strict-equal?))
      
      (define (comp-switch-cond-clause case tmp body tleft)
	 (with-access::J2SCase case (loc expr)
	    (if (isa? case J2SDefault)
		(epairify loc `(else ,body))
		(let* ((op (test-switch tleft (j2s-type expr)))
		       (test `(,op ,tmp ,(j2s-scheme expr mode return conf hint totype))))
		   (epairify loc `(,test ,body))))))

      (define (comp-switch-case-clause case body tleft)
	 (with-access::J2SCase case (loc expr)
	    (if (isa? case J2SDefault)
		(epairify loc `(else ,body))
		(let ((test `(,(j2s-scheme expr mode return conf hint totype))))
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
		       ,(j2s-scheme body mode return conf hint totype)
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
	       (funs (map (lambda (c) (gensym)) cases))
	       (tleft (j2s-type key)))
	    (multiple-value-bind (bindings bodies)
	       (comp-switch-clause-bodies cases funs)
	       `(let* ((,tmp ,(j2s-scheme key mode return conf hint totype))
		       ,@bindings)
		   (cond
		      ,@(mapc (lambda (c body)
				 (comp-switch-cond-clause c tmp body tleft))
			 cases bodies))))))

      (define (comp-switch-case key cases)
	 (let ((funs (map (lambda (c) (gensym)) cases))
	       (tleft (j2s-type key)))
	    (multiple-value-bind (bindings bodies)
	       (comp-switch-clause-bodies cases funs)
	       `(let* ,bindings
		   (case ,(j2s-scheme key mode return conf hint totype)
		      ,@(mapc (lambda (c body)
				 (comp-switch-case-clause c body tleft))
			 cases bodies))))))
      
      (define (scheme-case? key cases)
	 (let ((t (j2s-type key)))
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
	       (funs (map (lambda (c) (gensym)) cases)))
	    `(let* ((,tmp ,(j2s-scheme key mode return conf hint totype))
		    (%acc (js-undefined))
		    ,@(map (lambda (case fun)
			      (with-access::J2SCase case (loc body)
				 (epairify loc
				    `(,fun
					(lambda ()
					   ,(j2s-scheme body mode acc-return conf hint totype))))))
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
						   ,(j2s-scheme expr mode return conf hint totype))
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
;*    j2s-inline-method ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-inline-method field args self tyobj)
   
   (define (is-type-or-class? ty self tyobj)
      (cond
	 ((eq? ty 'any)
	  #t)
	 ((eq? ty tyobj)
	  #t)
	 ((isa? self J2SUnresolvedRef)
	  (with-access::J2SUnresolvedRef self (id)
	     (eq? ty id)))))

   (when (isa? field J2SString)
      (with-access::J2SString field (val)
	 (find (lambda (method)
		  (when (string=? (inline-method-jsname method) val)
		     (let ((ty (inline-method-ttype method)))
			(when (is-type-or-class? ty self tyobj)
			   (let loop ((args args)
				      (formals (inline-method-args method)))
			      (cond
				 ((null? args)
				  (or (null? formals) (every pair? formals)))
				 ((null? formals)
				  #f)
				 (else
				  (let ((tya (j2s-type (car args)))
					(tyf (if (pair? (car formals))
						 (caar formals)
						 (car formals))))
				     (when (or (eq? tyf 'any) (eq? tyf tya))
					(loop (cdr args) (cdr formals)))))))))))
	    j2s-inline-methods))))

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
(define-method (j2s-scheme this::J2SCall mode return conf hint totype)
   
   (define (array-push obj arg)
      (let ((scmobj (j2s-scheme obj mode return conf hint totype))
	    (scmarg (j2s-scheme arg mode return conf hint totype)))
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

   (define (call-super-method fun args)
      (call-unknown-function fun '(this) args))

   (define (call-ref-method self ccache ocache fun::J2SAccess obj::J2SExpr args)
      (with-access::J2SAccess fun (loc field)
	 (cond
	    ((isa? self J2SSuper)
	     (call-super-method fun args))
	    ((j2s-inline-method field args self (j2s-type obj))
	     =>
	     (lambda (m)
		(let* ((met (inline-method-met m))
		       (opt (inline-method-args m))
		       (arity (length opt))
		       (len (length args)))
		   (if (symbol? met)
		       `(,met ,(j2s-scheme obj mode return conf hint totype)
			   ,@(map (lambda (arg)
				     (j2s-scheme arg mode return conf hint totype))
				args)
			   ,@(if (=fx len arity)
				 '()
				 (let* ((lopt (length opt))
					(nopt (-fx arity len)))
				    (map cadr (list-tail opt (-fx lopt nopt)))))
			   ,@(if (inline-method-%this m)
				 '(%this)
				 '()))
		       (met obj
			  (append args
			     (if (=fx len arity)
				 '()
				 (let* ((lopt (length opt))
					(nopt (-fx arity len)))
				    (map cadr (list-tail opt (-fx lopt nopt)))))
			     (if (inline-method-%this m)
				 '(%this)
				 '()))
			  mode return conf hint totype)))))
	    ((and ccache (= (bigloo-debug) 0))
	     (cond
		((isa? field J2SString)
		 (let ((call (if (eq? (j2s-type obj) 'object)
				 'js-object-method-call-name/cache
				 'js-method-call-name/cache)))
		    (with-access::J2SString field (val)
		       `(,call
			   ,j2s-unresolved-call-workspace
			   ,(j2s-scheme obj mode return conf hint totype)
			   ',(string->symbol val)
			   ,(js-pcache ccache)
			   ,(js-pcache ocache)
			   ,@(map (lambda (arg)
				     (j2s-scheme arg mode return conf hint totype))
				args)))))
		(else
		 (call-unknown-function fun
		    (list (j2s-scheme obj mode return conf hint totype))
		    args))))
	    (else
	     (call-unknown-function fun
		(list (j2s-scheme obj mode return conf hint totype))
		args)))))

   (define (call-globalref-method self ccache ocache fun::J2SAccess obj::J2SExpr args)
      (with-access::J2SGlobalRef self (id decl)
	 (with-access::J2SDecl decl (usage)
	    (unless (memq 'assig usage)
	       (case id
		  ((Math)
		   (j2s-math-inline-method fun args
		      mode return conf hint totype))
		  (else
		   #f))))))

   (define (call-new-method obj::J2SNew field args mode return conf hint totype)
      (with-access::J2SNew obj (clazz)
	 (when (isa? clazz J2SGlobalRef)
	    (with-access::J2SGlobalRef clazz (id decl)
	       (with-access::J2SDecl decl (usage)
		  (unless (memq 'assig usage)
		     (case id
			((Date)
			 (j2s-date-new-method obj field args mode return
			    conf hint totype))
			(else
			 #f))))))))
   
   (define (call-method ccache ocache fun::J2SAccess args)
      (with-access::J2SAccess fun (loc obj field)
	 (let loop ((obj obj))
	    (cond
	       ((isa? obj J2SRef)
		(call-ref-method obj ccache ocache fun obj args))
	       ((isa? obj J2SGlobalRef)
		(or (call-globalref-method obj ccache ocache fun obj args)
		    (call-ref-method obj ccache ocache fun obj args)))
	       ((isa? obj J2SParen)
		(with-access::J2SParen obj (expr)
		   (loop expr)))
	       ((and (isa? obj J2SNew)
		     (call-new-method obj field args mode return conf hint totype))
		=>
		(lambda (sexp) sexp))
	       (else
		(let ((tmp (gensym)))
		   `(let ((,tmp ,(j2s-scheme obj mode return conf hint totype)))
		       ,(call-ref-method obj
			   ccache ocache
			   (duplicate::J2SAccess fun
			      (obj (instantiate::J2SPragma
				      (loc loc)
				      (expr tmp))))
			   (instantiate::J2SHopRef
			      (type (j2s-type obj))
			      (loc loc)
			      (id tmp))
			   args))))))))
   
   (define (call-hop-function fun::J2SHopRef thisarg args)
      `(,(j2s-scheme fun mode return conf hint 'any)
	,@(j2s-scheme args mode return conf hint 'any)))

   (define (j2s-self thisarg)
      (map (lambda (t) (j2s-scheme t mode return conf hint totype)) thisarg))

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
		       (vector ,@(j2s-scheme args mode return conf hint totype))
		       %this)))
	       ((null? args)
		(with-access::J2SDecl (car params) (loc)
		   (loop (cdr params) '()
		      (cons '(js-undefined) actuals))))
	       (else
		(loop (cdr params) (cdr args)
		   (cons (j2s-scheme (car args) mode return conf hint totype)
		      actuals)))))))

   (define (call-fix-function fun::J2SFun thisarg::pair-nil f %gen args)
      ;; call a function that accepts a fix number of arguments
      (with-access::J2SFun fun (params vararg)
	 (let ((lenf (length params))
	       (lena (length args)))
	    (cond
	       ((=fx lenf lena)
		;; matching arity
		`(,f ,@%gen ,@(j2s-self thisarg)
		    ,@(map (lambda (a p)
			      (with-access::J2SDecl p (utype)
				 (j2s-scheme a mode return conf hint utype)))
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
				   `(,t ,(j2s-scheme a mode return conf hint totype))) temps args)
		       (,f ,@%gen ,@(j2s-self thisarg)
			  ,@(take temps lenf)))))
	       (else
		;; argument missing
		`(,f ,@(j2s-self thisarg)
		    ,@(j2s-scheme args mode return conf hint totype)
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
		 ,@(j2s-scheme args mode return conf hint totype)))
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
			 (j2s-get loc (car withs) 'object `',id 'string #f)
			(list (car withs)) args)
		     ,(loop (cdr withs)))))))

   (define (call-pragma fun::J2SPragma args)
      (with-access::J2SPragma fun (expr)
	 `(,expr %this ,@(j2s-scheme args mode return conf hint totype))))

   (define (typed-generator? decl::J2SDeclFun)
      (with-access::J2SDeclFun decl (parent)
	 (when (isa? parent J2SDeclFun)
	    (with-access::J2SDeclFun parent (val)
	       (with-access::J2SFun val (generator)
		  generator)))))

   (define (call-known-function protocol fun::J2SDecl thisarg::pair-nil args)
      (cond
	 ((isa? fun J2SDeclFun)
	  (with-access::J2SDeclFun fun (id val)
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
		  ,(j2s-scheme fun mode return conf hint totype)
		  ,@self
		  ,@(j2s-scheme args mode return conf hint totype)))
	       (cache
		`(js-call/cache
		    ,j2s-unresolved-call-workspace
		    ,(j2s-scheme fun mode return conf hint totype)
		    ,(js-pcache cache)
		    ,@self
		    ,@(j2s-scheme args mode return conf hint totype)))
	       (else
		`(,call ,j2s-unresolved-call-workspace
		    ,(j2s-scheme fun mode return conf hint totype)
		    ,@self
		    ,@(j2s-scheme args mode return conf hint totype)))))))

   (define (call-eval-function fun args)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.1.1
      `(%js-direct-eval 
	  ,(if (null? args)
	       '(js-undefined)
	       (j2s-scheme (car args) mode return conf hint totype))
	  ,(strict-mode? mode)
	  %this this %scope))

   (define (is-eval? fun)
      (with-access::J2SUnresolvedRef fun (id)
	 (eq? id 'eval)))

   (define (call-unresolved-function fun thisarg args)
      (if (is-eval? fun)
	  (call-eval-function fun args)
	  (call-unknown-function fun
	     (j2s-scheme thisarg mode return conf hint totype) args)))

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
		(j2s-scheme-super this mode return conf hint totype))
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
		   (j2s-scheme thisarg mode return conf hint totype) args))
	       ((read-only-function fun)
		=>
		(lambda (fun) (call-known-function protocol fun thisarg args)))
	       (else
		(call-unknown-function fun
		   (j2s-scheme thisarg mode return conf hint totype) args)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssig mode return conf hint totype)

   (define (uint32 type expr)
      (if (eq? type 'uint32)
	  expr
	  `(fixnum->uint32 ,expr)))

   (define (aset/cache lhs rhs)
      ;; an optimized array set in a loop (see array.scm)
      (with-access::J2SAccess lhs (obj field)
	 (with-access::J2SAref obj (array alen amark deps)
	    (let ((scmarray (j2s-decl-scheme-id array))
		  (scmalen (j2s-decl-scheme-id alen))
		  (scmfield (j2s-scheme field mode return conf hint (j2s-type field)))
		  (scmobj (j2s-scheme obj mode return conf hint 'array))
		  (scmrhs (j2s-scheme rhs mode return conf hint 'any))
		  (tyfield (j2s-type field)))
	       (case tyfield
		  ((index uint29 ufixnu)
		   (let ((idx (j2s-scheme field mode return conf hint 'uint32)))
		      (if amark
			  `(JS-ARRAY-INDEX-MARK-SET! ,scmobj
			      ,(uint32 tyfield idx) ,scmrhs
			      ,scmarray ,scmalen
			      ,(j2s-decl-scheme-id amark)
			      ,(strict-mode? mode)
			      %this)
			  `(JS-ARRAY-INDEX-FAST-SET! ,scmobj
			      ,(uint32 tyfield idx) ,scmrhs
			      ,scmarray ,scmalen
			      ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			      ,(strict-mode? mode)
			      %this))))
		  ((fixnum)
		   (if amark
		       `(JS-ARRAY-FIXNUM-MARK-SET! ,scmobj ,scmfield ,scmrhs
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   ,(strict-mode? mode)
			   %this)
		       `(JS-ARRAY-FIXNUM-MARK-SET! ,scmobj ,scmfield ,scmrhs
			   ,scmarray ,scmalen
			   ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			   ,(strict-mode? mode)
			   %this)))
		  (else
		   (if amark
		       `(JS-ARRAY-MARK-SET! ,scmobj ,scmfield ,scmrhs
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   ,(strict-mode? mode)
			   %this)
		       `(JS-ARRAY-FAST-SET! ,scmobj ,scmfield ,scmrhs
			   ,scmarray ,scmalen
			   ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			   ,(strict-mode? mode)
			   %this))))))))

   (define (aset/w-cache lhs rhs)
      ;; an optimized array set in a loop (see array.scm)
      (with-access::J2SAccess lhs (obj field)
	 (with-access::J2SAref obj (array alen)
	    (case (j2s-type field)
	       ((index)
		`(js-array-index-set! ,(j2s-scheme obj mode return conf hint totype)
		    (fixnum->uint32 ,(j2s-scheme field mode return conf hint totype))
		    ,(j2s-scheme rhs mode return conf hint totype)
		    ,(strict-mode? mode)
		    %this))
	       ((fixnum)
		`(js-array-fixnum-set! ,(j2s-scheme obj mode return conf hint totype)
		    ,(j2s-scheme field mode return conf hint totype)
		    ,(j2s-scheme rhs mode return conf hint totype)
		    ,(strict-mode? mode)
		    %this))
	       (else
		`(js-array-set! ,(j2s-scheme obj mode return conf hint totype)
		    ,(j2s-scheme field mode return conf hint totype)
		    ,(j2s-scheme rhs mode return conf hint totype)
		    ,(strict-mode? mode)
		    %this))))))
   
   (define (array-set lhs::J2SAccess rhs::J2SExpr)
      (with-access::J2SAccess lhs (obj field cache (loca loc))
	 (let ((tyf (j2s-type field)))
	    (cond
	       ((isa? obj J2SAref)
		(if *j2s-array-cache*
		    (aset/cache lhs rhs)
		    (aset/w-cache lhs rhs)))
	       ((and (eq? tyf 'int53) (m64? conf))
		`(js-array-fixnum-set!
		    ,(j2s-scheme obj mode return conf hint totype)
		    ,(j2s-scheme field mode return conf hint totype)
		    ,(j2s-scheme rhs mode return conf hint totype)
		    ,(strict-mode? mode)
		    %this))
	       (else
		(case tyf
		   ((index ufixnum uint29)
		    `(js-array-index-set!
			,(j2s-scheme obj mode return conf hint totype)
			(fixnum->uint32
			   ,(j2s-scheme field mode return conf hint totype))
			,(j2s-scheme rhs mode return conf hint totype)
			,(strict-mode? mode)
			%this))
		   ((fixnum)
		    `(js-array-fixnum-set!
			,(j2s-scheme obj mode return conf hint totype)
			,(j2s-scheme field mode return conf hint totype)
			,(j2s-scheme rhs mode return conf hint totype)
			,(strict-mode? mode)
			%this))
		   (else
		    `(js-array-set!
			,(j2s-scheme obj mode return conf hint totype)
			,(j2s-scheme field mode return conf hint totype)
			,(j2s-scheme rhs mode return conf hint totype)
			,(strict-mode? mode)
			%this))))))))

   (define (maybe-array-set lhs::J2SAccess rhs::J2SExpr)
      (with-access::J2SAccess lhs (obj field cache loc)
	 (if (isa? lhs J2SRef)
	     `(if (isa? ,(j2s-scheme obj mode return conf hint totype) JsArray)
		  ,(array-set lhs rhs)
		  (j2s-put! loca (j2s-scheme obj mode return conf hint totype)
		     (typeof-this obj conf)
		     (j2s-scheme field mode return conf hint totype)
		     (j2s-scheme rhs mode return conf hint totype)
		     (strict-mode? mode)
		     cache)))
	 (let* ((tmp (gensym 'tmp))
		(access (duplicate::J2SAccess lhs (obj (J2SHopRef tmp)))))
	    `(let ((,tmp ,(j2s-scheme obj mode return conf hint totype)))
		(if (isa? ,tmp JsArray)
		    ,(array-set access rhs)
		    ,(j2s-put! loc tmp
			(typeof-this obj conf)
			(j2s-scheme field mode return conf hint totype)
			(j2s-scheme rhs mode return conf hint totype)
			(strict-mode? mode)
			cache))))))
   
   (with-access::J2SAssig this (loc lhs rhs)
      (let loop ((lhs lhs))
	 (cond
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field cache (loca loc))
		(epairify loc
		   (cond
		      ((eq? (j2s-type obj) 'vector)
		       (j2s-vector-set! this mode return conf hint totype))
		      ((and (eq? (j2s-type obj) 'array) (maybe-number? field))
		       (array-set lhs rhs))
		      ((is-number? field)
		       (maybe-array-set lhs rhs))
		      (else
		       (j2s-put! loca (j2s-scheme obj mode return conf hint totype)
			  (typeof-this obj conf)
			  (j2s-scheme field mode return conf hint totype)
			  (j2s-scheme rhs mode return conf hint totype)
			  (strict-mode? mode)
			  cache))))))
	    ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
	     (with-access::J2SRef lhs (decl loc)
		(with-access::J2SDecl decl (hint)
		   (let ((assig (j2s-scheme-set! lhs
				   (j2s-scheme rhs mode return conf hint totype)
				   (j2s-scheme lhs mode return conf hint totype)
				   mode return conf #f loc)))
		      (if (pair? assig)
			  (epairify loc assig)
			  assig)))))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(epairify loc
		   (j2s-unresolved-put! `',id
		      (j2s-scheme rhs mode return conf hint totype) #f mode return))))
	    ((isa? lhs J2SHopRef)
	     (with-access::J2SHopRef lhs (id)
		(epairify loc
		   `(set! ,id ,(j2s-scheme rhs mode return conf hint totype)))))
	    ((isa? lhs J2SWithRef)
	     (with-access::J2SWithRef lhs (id withs expr loc)
		(epairify loc
		   (let liip ((withs withs))
		      (if (null? withs)
			  (loop expr)
			  `(if ,(j2s-in? loc `',id (car withs))
			       ,(j2s-put! loc (car withs) 'object
				   (symbol->string id)
				   (j2s-scheme rhs mode return conf hint totype) #f #f)
			       ,(liip (cdr withs))))))))
	    ((isa? lhs J2SUndefined)
	     (j2s-scheme rhs mode return conf hint totype))
	    ((isa? lhs J2SParen)
	     (with-access::J2SParen lhs (expr)
		(loop expr)))
	    (else
	     (j2s-error "assignment" "Illegal assignment" this))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-postref ...                                           */
;*    -------------------------------------------------------------    */
;*    Generic generator for prefix and postfix operations.             */
;*    -------------------------------------------------------------    */
;*    !!! x++ not equivalent to x = x + 1 as x++ always converts       */
;*    to number.                                                       */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-postpref this::J2SAssig mode return conf hint totype
	   op retval)

   (define (new-or-old tmp val comp)
      (if (eq? retval 'new)
	  (let ((aux (gensym 'res)))
	     `(let ((,aux ,val))
		 ,(comp aux aux)))
	  (comp val tmp)))
   
   (define (var++ inc lhs tmp loc)
      (if (eq? totype 'void)
	  (j2s-scheme-set! lhs (inc tmp) #f mode return conf #f loc)
	  (new-or-old tmp (inc tmp)
	     (lambda (val tmp)
		(j2s-scheme-set! lhs val tmp mode return conf #f loc)))))
   
   (define (var++/suf op lhs type typesuf loc)
      (let ((tmp (gensym 'tmp))
	    (op (symbol-append (if (eq? op '++) '+ '-) typesuf)))
	 `(let ((,tmp ,(j2s-scheme lhs mode return conf hint totype)))
	     ,(var++ (lambda (x) `(,op ,x 1)) lhs tmp loc))))
   
   (define (ref-inc op lhs::J2SRef inc type loc)
      (let* ((typ (j2s-type lhs))
	     (num (J2SNumber/type typ inc))
	     (inc (j2s-scheme
		     (if (type-number? typ)
			 (J2SBinary/type '+ typ lhs num)
			 (J2SBinary/type '+ typ (J2SCast 'number lhs) num))
		     mode return conf hint totype)))
	 (cond
	    ((eq? totype 'void)
	     ;; no need return the assignment value
	     (j2s-scheme-set! lhs inc #f mode return conf #f loc))
	    ((type-number? type)
	     `(begin
		 ,(j2s-scheme-set! lhs inc #f mode return conf #f loc)
		 ,(j2s-scheme lhs mode return conf hint totype)))
	    (else
	     (let ((tmp (gensym 'tmp)))
		`(let ((,tmp ,inc))
		    ,(j2s-scheme-set! lhs inc #f mode return conf #f loc)
		    ,tmp))))))
;* 	                                                               */
;* 	                                                               */
;*       (cond                                                         */
;* 	 ((and (type-uint32? type) (is-uint32? lhs))                   */
;* 	  (var++/suf op lhs 'uint32 'u32 loc))                         */
;* 	 ((and (type-int53? type) (is-uint53? lhs) (m64? conf))        */
;* 	  (var++/suf op lhs 'int53 'fx loc))                           */
;* 	 ((and (type-fixnum? type) (is-fx? lhs))                       */
;* 	  (var++/suf op lhs 'fixnum 'fx loc))                          */
;* 	 ((and (type-int30? type) (is-int30? lhs))                     */
;* 	  (var++/suf op lhs 'int30 'fx loc))                           */
;* 	 ((and (memq type '(fixnum)) (memq (j2s-type lhs) '(fixnum)))  */
;* 	  (var++/suf op lhs 'fixnum 'fx loc))                          */
;* 	 ((memq (j2s-type lhs) '(integer number uint29 ufixnum fixnum index)) */
;* 	  (let ((tmp (gensym 'tmp)))                                   */
;* 	     `(let ((,tmp ,(j2s-scheme lhs mode return conf hint totype))) */
;* 		 ,(var++ (lambda (x)                                   */
;* 			    (j2s-num-op (if (eq? op '++) '+ '-) x 1 lhs lhs conf)) */
;* 		     lhs tmp loc))))                                   */
;* 	 (else                                                         */
;* 	  (let ((tmp (gensym 'tmp)))                                   */
;* 	     `(let ((,tmp ,(j2s-scheme lhs mode return conf hint totype))) */
;* 		 (if (fixnum? ,tmp)                                    */
;* 		     ,(var++ (lambda (x)                               */
;* 				(j2s-+fx x (if (eq? op '++) 1 -1) conf)) */
;* 			 lhs tmp loc)                                  */
;* 		     (let ((,tmp (js-tonumber ,tmp %this)))            */
;* 			,(var++ (lambda (x)                            */
;* 				   `(js+ ,x ,(if (eq? op '++) 1 -1) %this)) */
;* 			    lhs tmp loc))))))))                        */
   
   (define (unresolved-inc op lhs inc)
      (with-access::J2SUnresolvedRef lhs (id cache loc)
	 (let ((tmp (gensym 'tmp)))
	    `(let ((,tmp ,(j2s-unresolved id cache loc)))
		(if (fixnum? ,tmp)
		    ,(new-or-old tmp (j2s-+fx tmp inc conf)
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
	    (let* ((tmp (gensym 'tmp))
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
			   (type 'int30)))
		   (field (if pro (J2SHopRef pro) field))
		   (scmlhs (j2s-scheme oacc mode return conf hint totype)))
	       (cond
		  ((type-fixnum? type)
		   (let ((tref (instantiate::J2SHopRef
				  (loc loc)
				  (id tmp)
				  (type 'fixnum))))
		      `(let ((,tmp ,scmlhs))
			  ,(new-or-old tmp
			      (js-binop2 loc '+ (typeof-this obj conf)
				 tref rhs mode return conf hint totype)
			      (lambda (val tmp)
				 `(begin
				     ,(j2s-put! loc otmp
					 (or force-type (typeof-this obj conf))
					 (or pro prov)
					 val
					 (strict-mode? mode)
					 cache (min cl clevel))
				     ,tmp))))))
		  (else
		   `(let ((,tmp ,scmlhs))
		       (if (fixnum? ,tmp)
			   ,(let ((tref (instantiate::J2SHopRef
					   (loc loc)
					   (id tmp)
					   (type 'fixnum))))
			       (new-or-old tmp
				  (js-binop2 loc '+ (typeof-this obj conf)
				     tref rhs mode return conf hint totype)
				  (lambda (val tmp)
				     `(begin
					 ,(j2s-put! loc otmp
					     (or force-type (typeof-this obj conf))
					     (or pro prov)
					     val
					     (strict-mode? mode)
					     cache (min cl clevel))
					 ,tmp))))
			   ,(let* ((tmp2 (gensym 'tmp))
				   (tref (instantiate::J2SHopRef
					    (loc loc)
					    (id tmp2)
					    (type 'number))))
			       `(let ((,tmp2 (js-tonumber ,tmp %this)))
				   ,(new-or-old tmp2
				       (js-binop2 loc '+ (typeof-this obj conf)
					  tref rhs mode return conf hint totype)
				       (lambda (val tmp)
					  `(begin
					      ,(j2s-put! loc otmp
						  (or force-type (typeof-this obj conf))
						  (or pro prov)
						  val
						  (strict-mode? mode)
						  cache (min cl clevel))
					      ,tmp2)))))))))))))

   (define (rhs-cache rhs)
      (with-access::J2SBinary rhs (lhs)
	 (when (isa? lhs J2SAccess)
	    (with-access::J2SAccess lhs (cache)
	       cache))))
   
   (define (access-inc op lhs rhs inc)
      (with-access::J2SAccess lhs (obj field clevel cache (loca loc))
	 (let* ((tmp (gensym 'tmp))
		(otmp (gensym 'obj))
		(prov (j2s-property-scheme field mode return conf))
		(pro (when (pair? prov) (gensym 'pro)))
		(rhscache (rhs-cache rhs)))
	    `(let* ((,otmp ,(j2s-scheme obj mode return conf hint totype))
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
		     `(if (and (isa? ,otmp JsObject)
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
	       (else
		(j2s-error "j2sscheme"
		   (format "Illegal expression \"~a\"" op)
		   this)))))))
	   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPostfix ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.3.1       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPostfix mode return conf hint totype)
   (with-access::J2SPostfix this (op)
      (j2s-scheme-postpref this mode return conf hint totype op 'old)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPrefix ...                                       */
;*    -------------------------------------------------------------    */
;*    www.ecma-international.org/ecma-262/5.1/#sec-11.3.1prefix        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPrefix mode return conf hint totype)
   (with-access::J2SPrefix this (op)
      (j2s-scheme-postpref this mode return conf hint totype op 'new)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssigOp mode return conf hint totype)

   (define (aput-assigop otmp pro prov op lhs rhs cl)
      (with-access::J2SAccess lhs (obj field loc cache clevel)
	 (with-access::J2SExpr obj (type loc)
	    (let* ((oref (instantiate::J2SHopRef
			    (loc loc)
			    (id otmp)
			    (type type)
			    (itype type)))
		   (lhs (duplicate::J2SAccess lhs
			   (clevel (min cl clevel))
			   (obj oref)
			   (field (if pro (J2SHopRef pro) field)))))
	       (j2s-put! loc otmp (typeof-this obj conf)
		  (or pro prov)
		  (js-binop2 loc op (typeof-this obj conf)
		     lhs rhs mode return conf hint totype)
		  (strict-mode? mode)
		  cache (if (is-number? field) 100 (min cl clevel)))))))

   (define (access-assigop op lhs rhs)
      (with-access::J2SAccess lhs (obj field cache clevel)
	 (let* ((otmp (gensym 'obj))
		(prov (j2s-property-scheme field mode return conf))
		(pro (when (pair? prov) (gensym 'pro))))
	    `(let* ((,otmp ,(j2s-scheme obj mode return conf hint totype))
		    ,@(if pro (list `(,pro ,prov)) '()))
		,(cond
		    ((<=fx clevel 2)
		     (aput-assigop otmp pro prov op lhs rhs clevel))
		    ((or (not cache) (is-integer? field))
		     (aput-assigop otmp pro prov op lhs rhs 100))
		    ((memq (typeof-this obj conf) '(object this global))
		     `(with-access::JsObject ,otmp (cmap)
			 (let ((%omap cmap))
			    (if (eq? (js-pcache-cmap ,(js-pcache cache)) %omap)
				,(aput-assigop otmp pro prov op lhs rhs 1)
				,(aput-assigop otmp pro prov op lhs rhs 2)))))
		    (else
		     `(if (isa? ,otmp JsObject)
			  (with-access::JsObject ,otmp (cmap)
			     (let ((%omap cmap))
				(if (eq? (js-pcache-cmap ,(js-pcache cache))
				       %omap)
				    ,(aput-assigop otmp pro prov op lhs rhs 1)
				    ,(aput-assigop otmp pro prov op lhs rhs 2))))
			  (let ((%omap #unspecified))
			     ,(aput-assigop otmp pro prov op lhs rhs 2)))))))))
   
   (with-access::J2SAssigOp this (loc lhs rhs op type)
      (epairify-deep loc
	 (cond
	    ((isa? lhs J2SAccess)
	     (access-assigop op lhs rhs))
	    ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (hint utype)
		   (j2s-scheme-set! lhs
		      (js-binop2 loc op type lhs rhs mode return conf hint utype)
		      (j2s-scheme lhs mode return conf '() utype)
		      mode return conf #f loc))))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(j2s-unresolved-put! `',id
		   (js-binop2 loc op type lhs rhs mode return conf '() type)
		   #t mode return)))
	    (else
	     (j2s-error "j2sscheme" "Illegal assignment" this))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAccess mode return conf hint totype)
   
   (define (get obj field cache clevel loc)
      (let ((tyo (typeof-this obj conf)))
	 (j2s-get loc (j2s-scheme obj mode return conf hint totype) tyo
	    (j2s-property-scheme field mode return conf)
	    (j2s-type field) cache clevel)))

   (define (index-ref obj field cache clevel loc)
      (if (isa? obj J2SRef)
	  (let ((tmp (j2s-scheme obj mode return conf hint totype)))
	     `(cond
		 ((isa? ,tmp JsArray)
		  ,(or (j2s-array-ref this mode return conf hint totype)
		       (get obj field cache clevel loc)))
		 ((js-jsstring? ,tmp)
		  ,(or (j2s-string-ref this mode return conf hint totype)
		       (get obj field cache clevel loc)))
		 (else
		  ,(get obj field cache clevel loc))))
	  (let ((tmp (gensym 'tmp)))
	     `(let ((,tmp ,(j2s-scheme obj mode return conf hint totype)))
		 (cond
		    ((isa? ,tmp JsArray)
		     ,(let ((access (J2SAccess (J2SHopRef tmp) field)))
			 (or (j2s-array-ref access mode return conf hint totype)
			     (get obj field cache clevel loc))))
		    ((js-jsstring? ,tmp)
		     ,(let ((access (J2SAccess (J2SHopRef tmp) field)))
			 (or (j2s-string-ref access mode return conf hint totype)
			     (get obj field cache clevel loc))))
		    (else
		     ,(get (J2SHopRef tmp) field cache clevel loc)))))))
   
   (with-access::J2SAccess this (loc obj field cache clevel type)
      (epairify-deep loc 
	 (cond
	    ((eq? (j2s-type obj) 'vector)
	     (j2s-vector-ref this mode return conf hint totype))
	    ((eq? (j2s-type obj) 'array)
	     (or (j2s-array-ref this mode return conf hint totype)
		 (get obj field cache clevel loc)))
	    ((eq? (j2s-type obj) 'string)
	     (or (j2s-string-ref this mode return conf hint totype)
		 (get obj field cache clevel loc)))
	    ((is-number? field)
	     (index-ref obj field cache clevel loc))
	    (else
	     (get obj field cache clevel loc))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCacheCheck ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCacheCheck mode return conf hint totype)
   (with-access::J2SCacheCheck this (prop cache obj)
      (case prop
	 ((proto-method)
	  `(eq? (js-pcache-pmap (js-pcache-ref %pcache ,cache))
	      (js-object-cmap ,(j2s-scheme obj mode return conf hint totype))))
	 (else
	  (error "j2s-scheme" "Illegal J2SCacheCheck property" prop)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCacheUpdate ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCacheUpdate mode return conf hint totype)
   (with-access::J2SCacheUpdate this (cache obj prop)
      (case prop
	 ((proto-method)
	  `(with-access::JsPropertyCache (js-pcache-ref %pcache ,cache) (pmap)
	      (set! pmap
		 (js-object-cmap
		    ,(j2s-scheme obj mode return conf hint totype)))))
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
(define-method (j2s-scheme this::J2SInit mode return conf hint totype)
   (with-access::J2SAssig this (loc lhs rhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (hint)
		(epairify-deep loc
		   `(begin
		       ,(j2s-scheme-set! lhs
			   (j2s-scheme rhs mode return conf hint totype)
			   #f mode return conf #t loc)
		       (js-undefined)))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SObjInit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SObjInit mode return conf hint totype)
   
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
		 `(js-toname ,(j2s-scheme val mode return conf hint totype) %this))))
	 ((isa? name J2SPragma)
	  `(js-toname ,(j2s-scheme name mode return conf hint totype) %this))
	 ((isa? name J2SLiteralCnst)
	  `(js-toname ,(j2s-scheme name mode return conf hint totype) %this))
	 ((isa? name J2SLiteralValue)
	  (with-access::J2SLiteralValue name (val)
	     `(js-toname ,(j2s-scheme val mode return conf hint totype) %this)))
	 (else
	  `(js-toname ,(j2s-scheme name mode return conf hint totype) %this))))
   
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
		 `(js-toname ,(j2s-scheme val mode return conf hint totype) %this))))
	 ((isa? name J2SLiteralCnst)
	  (with-access::J2SLiteralCnst name (val)
	     (literal-propname val)))
	 ((isa? name J2SPragma)
	  `(js-toname ,(j2s-scheme name mode return conf hint totype) %this))
	 ((isa? name J2SLiteralCnst)
	  `(js-toname ,(j2s-scheme name mode return conf hint totype) %this))
	 ((isa? name J2SLiteralValue)
	  (with-access::J2SLiteralValue name (val)
	     `(js-toname ,(j2s-scheme val mode return conf hint totype) %this)))
	 (else
	 `(js-toname ,(j2s-scheme name mode return conf hint totype) %this))))
   
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
			     (j2s-scheme val mode return conf hint totype)))
		     inits)))
	 `(let ((,names ,(if (every symbol? props)
			     `(quote ,(list->vector props))
			     `(vector ,@props)))
		(,elements (vector ,@vals)))
	     (js-literal->jsobject ,elements ,names %this))))

   (define (cmap->jsobj inits cmap)
      (let ((vals (map (lambda (i)
			  (with-access::J2SDataPropertyInit i (val)
			     (j2s-scheme val mode return conf hint totype)))
		     inits)))
	 (if (any (lambda (i)
		     (with-access::J2SDataPropertyInit i (val)
			(maybe-function? val)))
		inits)
	     `(with-access::JsGlobalObject %this (__proto__)
		 (js-object-literal-init!
		    (instantiateJsObject
		       (cmap ,(j2s-scheme cmap mode return conf hint totype))
		       (elements (vector ,@vals))
		       (__proto__ __proto__))))
	     `(with-access::JsGlobalObject %this (__proto__)
		 (instantiateJsObject
		    (cmap ,(j2s-scheme cmap mode return conf hint totype))
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
					"__proto__"
					(j2s-scheme val mode return conf hint totype)
					(strict-mode? mode) #f)
				     (epairify loc
					`(js-bind! %this ,tmp
					    ,(j2s-propname name)
					    :value ,(j2s-scheme val mode return conf hint totype)
					    :writable #t
					    :enumerable #t
					    :configurable #t)))))
			     (else
			      (with-access::J2SAccessorPropertyInit i (loc name get set)
				 (epairify loc
				    `(js-bind! %this ,tmp
					,(j2s-propname name)
					:get ,(j2s-scheme get mode return conf hint totype)
					:set ,(j2s-scheme set mode return conf hint totype)
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
(define-method (j2s-scheme this::J2SDataPropertyInit mode return conf hint totype)
   (with-access::J2SDataPropertyInit this (loc name val)
      (epairify loc
	 `(,(j2s-scheme name mode return conf hint totype) ,(j2s-scheme val mode return conf hint totype)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNew ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNew mode return conf hint totype)
   
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
		(fun (j2s-scheme clazz mode return conf hint totype))
		(fid (with-access::J2SDecl decl (id) (j2s-fast-id id)))
		(args (map (lambda (a)
			      (j2s-scheme a mode return conf hint totype))
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
	     (j2s-new-array this mode return conf hint totype)))
	 ((and (=fx (bigloo-debug) 0) cache)
	  (epairify loc
	     (j2s-new-fast cache clazz args)))
	 (else
	  (epairify loc
	     (j2s-new loc (j2s-scheme clazz mode return conf hint totype)
		(j2s-scheme args mode return conf hint totype)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SReturnYield ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SReturnYield mode return conf hint totype)
   
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
	   %gen ,(j2s-scheme expr mode return conf hint totype)
	     ,(isa? kont J2SUndefined)
	     ,(if (identity-kont? kont)
		  #f
		  (j2s-scheme kont mode return conf hint totype))
	     %this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SKont ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SKont mode return conf hint totype)
   (with-access::J2SKont this (loc param exn body)
      (epairify loc
	 `(lambda (,(j2s-scheme param mode return conf hint totype)
		   ,(j2s-scheme exn mode return conf hint totype))
	     ,(j2s-scheme body mode return conf hint totype)))))

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
(define-method (j2s-scheme this::J2STilde mode return conf hint totype)
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
(define-method (j2s-scheme this::J2SDollar mode return conf hint totype)
   (with-access::J2SDollar this (loc)
      (match-case loc
	 ((at ?fname ?loc)
	  (error/location "hopscript" "Illegal $ expression" this
	     fname loc))
	 (else
	  (j2s-error "hopscript" "Illegal $ expression" this)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SOPTInitSeq ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SOPTInitSeq mode return conf hint totype)
   
   (define (init-expr node)
      ;; see ctor.scm
      (with-access::J2SStmtExpr node (expr)
	 (with-access::J2SAssig expr (rhs)
	    rhs)))
   
   (with-access::J2SOPTInitSeq this (loc ref nodes cmap0 cmap1)
      (let ((%ref (gensym '%ref))
	    (cmap (gensym '%cmap0))
	    (i (gensym '%i))
	    (elements (gensym '%elements)))
	 `(let ((,%ref ,(j2s-scheme ref mode return conf hint totype)))
	     (with-access::JsObject ,%ref (cmap elements)
		(let ((,cmap cmap))
		   (if (or (eq? ,cmap ,cmap0) (eq? ,cmap ,cmap1))
		       ;; cache hit
		       (with-access::JsConstructMap ,cmap (props)
			  (let ((,i (vector-length props))
				(,elements elements))
			     ,@(map (lambda (init offset)
				       `(vector-set! ,elements (+fx ,i ,offset)
					   ,(j2s-scheme (init-expr init)
					       mode return conf hint totype)))
				  nodes (iota (length nodes)))
			     (with-access::JsObject ,%ref ((omap cmap))
				(set! omap ,cmap1))))
		       ;; cache miss
		       (with-access::JsConstructMap ,cmap (props)
			  (let ((len0 (vector-length props)))
			     ,@(map (lambda (n)
				       (j2s-scheme n
					  mode return conf hint totype))
				  nodes)
			     (with-access::JsConstructMap cmap (props)
				(when (=fx (+fx len0 ,(length nodes))
					 (vector-length props))
				   (set! ,cmap0 ,cmap)
				   (set! ,cmap1 cmap))))))))))))
   
   
