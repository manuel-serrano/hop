;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/scheme.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 11:47:51 2013                          */
;*    Last change :  Sun Mar 19 17:42:16 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Generate a Scheme program from out of the J2S AST.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage)

   (import __js2scheme_array)
   
   (export j2s-scheme-stage
	   j2s-scheme-eval-stage
	   (generic j2s-scheme ::obj ::symbol ::procedure ::obj ::obj ::symbol)))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved-workspaces ...                                    */
;*---------------------------------------------------------------------*/
(define j2s-unresolved-put-workspace '%this)
(define j2s-unresolved-del-workspace '%this)
(define j2s-unresolved-get-workspace '%scope)
(define j2s-unresolved-call-workspace '%this)

;*---------------------------------------------------------------------*/
;*    inline-method ...                                                */
;*---------------------------------------------------------------------*/
(define-struct inline-method jsname scmid ttype args %this)

;*---------------------------------------------------------------------*/
;*    j2s-inline-methods ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-inline-methods
   ;; jsname, scmname, (this.types), [optional-args] %this
   (map (lambda (e)
	   (apply inline-method e))
      '(;; string methods
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
	("replace" js-jsstring-replace string (any string) %this)
	("replace" js-jsstring-maybe-replace any (any any) %this)
	("match" js-jsstring-match string (any) %this)
	("match" js-jsstring-maybe-match any (any) %this)
	("naturalCompare" js-jsstring-naturalcompare string (string) %this)
	("naturalCompare" js-jsstring-maybe-naturalcompare any (any) %this)
	("localeCompare" js-jsstring-localecompare string (string) %this)
	("localeCompare" js-jsstring-maybe-localecompare any (any) %this)
	("trim" js-jsstring-trim string () #f)
	("trim" js-jsstring-maybe-trim any () %this)
	;; array methods
	("push" js-array-push array (any) %this)
	("push" js-array-maybe-push any (any) %this)
	("pop" js-array-pop array () %this)
	("pop" js-array-maybe-pop any () %this))))

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
;*    js-uint32->jsnum ...                                             */
;*---------------------------------------------------------------------*/
(define (js-uint32->jsnum expr conf)
   (let ((lgsz (config-get conf :long-size 30)))
      (cond
	 ((and (uint32? expr) (<u32 expr (bit-lshu32 #u32:1 (-fx lgsz 1))))
	  (uint32->fixnum expr))
	 ((>fx lgsz 32)
	  `(uint32->fixnum ,expr))
	 (else
	  `(js-uint32->jsnum ,expr)))))

;*---------------------------------------------------------------------*/
;*    js-uint32->fixnum ...                                            */
;*---------------------------------------------------------------------*/
(define (js-uint32->fixnum expr conf)
   (cond
      ((uint32? expr)
       (uint32->fixnum expr))
      (else
       `(uint32->fixnum ,expr))))

;*---------------------------------------------------------------------*/
;*    js-fixnum->uint32 ...                                            */
;*---------------------------------------------------------------------*/
(define (js-fixnum->uint32 expr)
   (if (fixnum? expr)
       (fixnum->uint32 expr)
       `(fixnum->uint32 ,expr)))

;*---------------------------------------------------------------------*/
;*    cast ...                                                         */
;*---------------------------------------------------------------------*/
(define (cast expr conf from to)
   
   (define (err)
      (error "cast" (format "illegal cast ~a -> ~a" from to) expr))
   
   (if (or (eq? from to) (eq? to '*))
       expr
       (case from
	  ((uint29)
	   (case to
	      ((uint29 index uint32 length) expr)
	      (else (js-uint32->fixnum expr conf))))
	  ((index uint32 length)
	   (case to
	      ((uint29 index uint32 length) expr)
	      (else (js-uint32->jsnum expr conf))))
	  ((int30)
	   (case to
	      ((uint29 index uint32 length) (fixnum->uint32 expr))
	      (else expr)))
	  ((int53)
	   (case to
	      ((uint29 index uint32 length int30) (err))
	      (else expr)))
	  ((fixnum)
	   (case to
	      ((uint29 index uint32 length) (js-fixnum->uint32 expr))
	      (else expr)))
	  ((integer number)
	   (case to
	      ((uint29 index uint32 length) (js-fixnum->uint32 expr))
	      (else expr)))
	  (else
	   (case to
	      ((uint29 index uint32 length) (js-fixnum->uint32 expr))
	      (else expr))))))
	  
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
;*    flatten-begin ...                                                */
;*---------------------------------------------------------------------*/
(define (flatten-begin nodes)
   (cond
      ((null? nodes) `(js-undefined))
      ((null? (cdr nodes)) (car nodes))
      (else `(begin ,@(remove-undefined (flatten-nodes nodes))))))

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
;*    epairify ...                                                     */
;*---------------------------------------------------------------------*/
(define (epairify loc expr)
   (econs (car expr) (cdr expr) loc))

;*---------------------------------------------------------------------*/
;*    j2s-+fx ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-+fx x y conf)
   (case (config-get conf :long-size 0)
      ((32)
       `(js+fx32 ,x ,y))
      (else
       `(js+fx ,x ,y))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-id ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-id id pref)
   (cond
      ((char=? (string-ref (symbol->string! id) 0) #\%) id)
      ((memq id '(GLOBAL arguments)) id)
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
;*    strict-mode? ...                                                 */
;*---------------------------------------------------------------------*/
(define (strict-mode? mode)
   (or (eq? mode 'strict) (eq? mode 'hopscript)))

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
;*    j2s-in? ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-in? loc id obj)
   (if (> (bigloo-debug) 0)
       `(js-in?/debug %this ',loc ,id ,obj)
       `(js-in? %this ,id ,obj)))

;*---------------------------------------------------------------------*/
;*    j2s-fast-id ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-fast-id id)
   (symbol-append '@ id))

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
		    (j2s-scheme n mode return conf hint totype))
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
	     (with-access::J2SRegExp this (loc val flags)
		(vector 1 val flags)))
	    ((isa? this J2SCmap)
	     (with-access::J2SCmap this (val)
		(vector 2 val)))
	    (else
	     (error "j2s-constant" "wrong literal" this))))

      `(js-constant-init
	  ,(obj->string (list->vector (map j2s-constant cnsts))) %this))

   (define (%cnsts cnsts)
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
			   (library nodejs)
			   (library js2scheme)
			   (library hopwidget))
			(library hop hopscript nodejs js2scheme hopwidget)
			(cond-expand
			   (enable-libuv
			    (library libuv)))
			(main main))))
	 (with-access::J2SProgram this (mode pcache-size %this path cnsts globals)
	    (list
	       module
	       (define-pcache pcache-size)
	       '(hop-sofile-compile-policy-set! 'static)
	       `(define %pcache (js-make-pcache ,pcache-size))
	       `(define %this (nodejs-new-global-object))
	       `(define %source ,path)
	       '(define %resource (dirname %source))
	       
	       `(define (main args)
		   (define %worker
		      (js-init-main-worker! %this #f nodejs-new-global-object))
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
		       `(begin
			   (define ,ident ,value)
			   (js-bind! %this ,scope ',id
			      :configurable #f
			      :get (js-make-function %this
				      (lambda (%) ,ident)
				      1 ,fun-name)
			      :set ,(when writable
				       `(js-make-function %this
					   (lambda (% %v)
					      (set! ,ident %v))
					   2 ,fun-name)))))))
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
	 (if (memq scope '(global))
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
(define (j2s-scheme-set! lhs val result mode return conf)
   
   (define (set decl hint utype)
      (if (and (j2s-let? decl) (not (j2s-let-opt? decl)))
	  ;; `(js-let-set! ,(j2s-decl-scheme-id decl) ,val)
	  `(set! ,(j2s-decl-scheme-id decl) ,val)
	  `(set! ,(j2s-scheme lhs mode return conf hint utype) ,val)))
   
   (with-access::J2SRef lhs (decl)
      (cond
	 ((isa? lhs J2SRef)
	  (with-access::J2SDecl decl (writable scope id loc hint utype)
	     (if (or writable (isa? decl J2SDeclInit))
		 (cond
		    ((and (memq scope '(global %scope)) (in-eval? return))
		     `(begin
			 ,(j2s-put! loc '%scope (j2s-type lhs) `',id
			     val (strict-mode? mode) #f)
			 ,result))
		    (result
		     `(begin
			 ,(set decl hint utype)
			 ,result))
		    (else
		     (set decl hint utype)))
		 val)))
	 ((not result)
	  (set decl '() 'any))
	 (else
	  `(begin
	      ,(set decl '() 'any)
	      ,result)))))
	      
;*---------------------------------------------------------------------*/
;*    j2s-function-src ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-function-src loc val::J2SFun conf)
   (when (or (<fx (config-get conf :optim 0) 2)
	     (config-get conf :force-location))
      (match-case loc
	 ((at ?path ?start)
	  (let ((m (config-get conf :mmap-src)))
	     `'(,loc . ,(when (mmap? m)
			   (with-access::J2SFun val (body)
			      (with-access::J2SBlock body (endloc)
				 (match-case endloc
				    ((at ?- ?end)
				     (mmap-substring m
					(fixnum->elong start)
					(+elong 1 (fixnum->elong end))))))))))))))

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
;*    j2s-declfun-prototype ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-declfun-prototype this::J2SDeclFun)
   (with-access::J2SDeclFun this (parent)
      (let* ((decl (if parent parent this))
	     (scmid (j2s-decl-scheme-id decl)))
	 `(js-get ,scmid 'prototype %this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclFun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclFun mode return conf hint totype)
   
   (define (make-function this::J2SDeclFun)
      (with-access::J2SDeclFun this (loc id scope val usage ronly)
	 (with-access::J2SFun val (params mode vararg body name generator
				     constrsize)
	    (let* ((fastid (j2s-fast-id id))
		   (lparams (length params))
		   (arity (if vararg -1 (+fx 1 lparams)))
		   (minlen (if (eq? mode 'hopscript) (j2s-minlen val) -1))
		   (len (if (eq? vararg 'rest) (-fx lparams 1) lparams))
		   (src (j2s-function-src loc val conf)))
	       (cond
		  (generator
		   `(js-make-function %this
		       ,fastid ,len ,(symbol->string! id)
		       :src ,(j2s-function-src loc val conf)
		       :rest ,(eq? vararg 'rest)
		       :arity ,arity
		       :minlen ,minlen
		       :strict ',mode
		       :alloc (lambda (o)
				 (js-object-alloc o %this))
		       :prototype ,(j2s-fun-prototype val)
		       :__proto__ ,(j2s-fun-__proto__ val)
		       :construct ,fastid
		       :constrsize ,constrsize))
		  (src
		   `(js-make-function %this
		       ,fastid ,len ,(symbol->string! id)
		       :src ,src
		       :rest ,(eq? vararg 'rest)
		       :arity ,arity
		       :minlen ,minlen
		       :strict ',mode
		       :alloc (lambda (o) (js-object-alloc o %this))
		       :construct ,fastid
		       :constrsize ,constrsize))
		  (else
		   `(js-make-function-simple %this
		       ,fastid ,len ,(symbol->string! id)
		       ,arity ,minlen
		       ',mode ,(eq? vararg 'rest)
		       ,constrsize)))))))
   
   (define (no-closure? this::J2SDeclFun)
      (with-access::J2SDeclFun this (usage ronly val)
	 (when ronly
	    (with-access::J2SFun val (generator)
	       (unless generator
		  (and (not (memq 'new usage))
		       (not (memq 'ref usage))
		       (not (memq 'get usage))))))))

   (define (constructor-only? this::J2SDeclFun)
      (with-access::J2SDeclFun this (usage ronly val)
	 (when ronly
	    (with-access::J2SFun val (generator)
	       (unless generator
		  (and (memq 'new usage)
		       (not (memq 'ref usage))
		       (not (memq 'call usage))))))))
   
   (with-access::J2SDeclFun this (loc id scope val usage ronly)
      (with-access::J2SFun val (params mode vararg body name generator)
	 (let* ((scmid (j2s-decl-scheme-id this))
		(fastid (j2s-fast-id id)))
	    (epairify-deep loc
	       (case scope
		  ((none)
		   `(define ,fastid
		       ,(jsfun->lambda val mode return conf
			   (j2s-declfun-prototype this)
			   (constructor-only? this))))
		  ((letblock)
		   (let ((def `(,fastid ,(jsfun->lambda val mode return conf
					    (j2s-declfun-prototype this)
					    (constructor-only? this)))))
		      (if (no-closure? this)
			  (list def)
			  (list def `(,scmid ,(make-function this))))))
		  ((global %scope)
		   `(begin
		       (define ,fastid
			  ,(jsfun->lambda val mode return conf
			      (j2s-declfun-prototype this)
			      (constructor-only? this)))
		       ,@(if (no-closure? this)
			     '()
			     `((define ,scmid
				  (js-bind! %this ,scope ',id
				     :configurable #f
				     :value ,(make-function this)))))))
		  
		  (else
		   `(begin
		       (define ,fastid
			  ,(jsfun->lambda val mode return conf
			      (j2s-declfun-prototype this)
			      (constructor-only? this)))
		       ,@(if (no-closure? this)
			     '()
			     `((define ,scmid
				  ,(make-function this))))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclSvc ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclSvc mode return conf hint totype)
   (with-access::J2SDeclSvc this (loc id val scope)
      (let ((scmid (j2s-decl-scheme-id this))
	    (fastid (j2s-fast-id id)))
	 (epairify-deep loc
	    (if (memq scope '(global %scope))
		`(begin
		    (define ,fastid ,(jssvc->scheme val id scmid mode return conf))
		    (define ,scmid 
		       (js-bind! %this ,scope ',id
			  :configurable #f
			  :writable #f
			  :value ,fastid)))
		`(begin
		    (define ,fastid ,(jssvc->scheme val id scmid mode return conf))
		    (define ,scmid ,fastid)))))))

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
   (with-access::J2SThis this (loc type)
      'this))

;*---------------------------------------------------------------------*/
;*    j2s-test ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-test test::J2SExpr mode return conf)
   (let ((ty (j2s-type test)))
      (cond
	 ((eq? ty 'bool)
	  (j2s-bool-test test mode return conf))
	 ((eq? ty 'object)
	  #t)
	 ((notbool-expr? test)
	  `(js-toboolean ,(j2s-scheme test mode return conf '(bool) 'any)))
	 (else
	  `(js-totest ,(j2s-scheme test mode return conf '(bool) 'any))))))
   
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
;*    pcache ...                                                       */
;*---------------------------------------------------------------------*/
(define (pcache cache)
   `(js-pcache-ref %pcache ,cache))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-unresolved name cache throw)
   (cond
      ((eq? name 'undefined)
       `(js-undefined))
      (cache
       `(js-global-object-get-name/cache ,j2s-unresolved-get-workspace ',name
	   ,(pcache cache)
	   ,(if (pair? throw) `',throw throw)
	   %this))
      (else
       `(js-global-object-get-name ,j2s-unresolved-get-workspace ',name
	   ,(if (pair? throw) `',throw throw)
	   %this))))

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
	 ((and (flonum? val) (nanfl? val)) "NaN")
	 ((eq? type 'number) (j2s-num val))
	 (else val))))

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
	  ((>=fx (config-get conf :long-size 0) 64)
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
	  ((and (>=fx (config-get conf :long-size 0) 64)
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
	   (j2s-num val))
	  ((or bint61 62 64)
	   (if (and (<fx val (bit-lsh 1 53)) (>fx val (negfx (bit-lsh 1 53))))
	       (j2s-num val)
	       (fixnum->flonum val)))
	  (else
	   (error "j2s-scheme" "unknown integer size"
	      (bigloo-config 'elong-size)))))
      (else val)))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNumber ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNumber mode return conf hint totype)
   (with-access::J2SNumber this (val type)
      (cond
	 ;;((memq type '(uint29 index length))
	 ((memq type '(index length))
	  (cond
	     ((u32? conf)
	      (cond
		 ((uint32? val) val)
		 ((fixnum? val) (fixnum->uint32 val))
		 ((elong? val) (elong->uint32 val))
		 ((flonum? val) (flonum->uint32 val))
		 (else (error "j2s-scheme" "wrong number" (j2s->list this)))))
	     ((fixnum? val)
	      (cond
		 ((>=fx (config-get conf :long-size 0) 64)
		  val)
		 ((<fx val (bit-lsh 1 29))
		  val)
		 (else
		  (fixnum->flonum val))))
	     ((elong? val)
	      (cond
		 ((>=fx (config-get conf :long-size 0) 64)
		  val)
		 ((<fx val (bit-lshelong #e1 29))
		  (elong->fixnum val))
		 (else
		  (elong->flonum val))))
	     (else
	      (flonum->uint32 (exact->inexact val)))))
	 ((eq? type 'fixnum)
	  (cond
	     ((fixnum? val) val)
	     ((elong? val) `(elong->fixnum ,val))
	     ((uint32? val) `(uint32->fixnum ,val))
	     ((flonum? val) `(flonum->fixnum ,val))
	     ((bignum? val) `(flonum->fixnum ,(bignum->flonum val)))
	     (else (error "j2s-scheme" "wrong number" (j2s->list this)))))
	 (else
	  (j2s-number val conf)))))

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
	  ;; JS object is dynamically allocation
 	  `(let ((rx (vector-ref-ur %cnsts ,index)))
	      (with-access::JsRegExp rx (properties)
		 (duplicate::JsRegExp rx
		     (properties (list-copy properties)))))
	  `(vector-ref-ur %cnsts ,index))))

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
;*    j2s-jsstring ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring val loc)
   (epairify loc
      (js-string->jsstring val)))

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
;*    j2s-scheme ::J2SArray ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArray mode return conf hint totype)

   (define (unique? conf vec)
      (let ((vectors (config-get conf :%vectors)))
	 (unless (member vec vectors)
	    (config-put! conf :%vectors (cons vec vectors))
	    #t)))
   
   (with-access::J2SArray this (loc exprs)
      (let ((sexprs (j2s-scheme exprs mode return conf hint totype)))
	 (cond
	    ((null? sexprs)
	     `(js-empty-vector->jsarray %this))
	    ((and (every (lambda (x)
			    (or (number? x) (string? x) (boolean? x)))
		     sexprs)
		  (unique? conf sexprs))
	     (epairify loc `(js-vector->jsarray ',(list->vector sexprs) %this)))
	    ((any (lambda (x) (isa? x J2SArrayAbsent)) exprs)
	     (epairify loc `(js-vector->sparse-jsarray (vector ,@sexprs) %this)))
	    (else
	     (epairify loc `(js-vector->jsarray (vector ,@sexprs) %this)))))))

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
;*    return-body ...                                                  */
;*---------------------------------------------------------------------*/
(define (return-body body)
   `(bind-exit (%return) ,(flatten-stmt body)))

;*---------------------------------------------------------------------*/
;*    jsfun-normal-vararg-body ...                                     */
;*---------------------------------------------------------------------*/
(define (jsfun-normal-vararg-body this::J2SFun body id rest)

   (define (init-argument val indx)
      `(js-arguments-define-own-property arguments ,indx
	  (instantiate::JsValueDescriptor
	     (name (string->symbol (integer->string ,indx)))
	     (value ,val)
	     (writable #t)
	     (configurable #t)
	     (enumerable #t))))

   (define (init-alias-argument argument rest indx)
      (let ((id (j2s-decl-scheme-id argument)))
	 `(begin
	     (set! ,id (car ,rest))
	     (js-arguments-define-own-property arguments ,indx
		(instantiate::JsAccessorDescriptor
		   (name (string->symbol (integer->string ,indx)))
		   (get (js-make-function %this
			   (lambda (%) ,id) 0 "get"))
		   (set (js-make-function %this
			   (lambda (% %v) (set! ,id %v)) 1 "set"))
		   (%get (lambda (%) ,id))
		   (%set (lambda (% %v) (set! ,id %v)))
		   (configurable #t)
		   (enumerable #t))))))
   
   (with-access::J2SFun this (params)
      `(let ((arguments
		(js-arguments %this
		   (make-vector (length ,rest) (js-absent)))))
	  ,@(if (pair? params)
		(map (lambda (param)
			(with-access::J2SDecl param (loc)
			   (epairify loc
			      `(define ,(j2s-decl-scheme-id param)
				  (js-undefined)))))
		   params)
		'())
	  ,(when (pair? params)
	      `(when (pair? ,rest)
		  ,(init-alias-argument (car params) rest 0)
		  (set! ,rest (cdr ,rest))
		  ,(let loop ((params (cdr params))
			      (i 1))
		      (if (null? params)
			  #unspecified
			  `(when (pair? ,rest)
			      ,(init-alias-argument (car params) rest i)
			      (set! ,rest (cdr ,rest))
			      ,(loop (cdr params) (+fx i 1)))))))
	  (let loop ((,rest ,rest)
		     (i ,(length params)))
	     (when (pair? ,rest)
		,(init-argument `(car ,rest) 'i)
		(loop (cdr ,rest) (+fx i 1))))
	  (js-define-own-property arguments 'callee
	     (instantiate::JsValueDescriptor
		(name 'callee)
		(value (js-make-function %this
			  ,(j2s-fast-id id) 0 ,(symbol->string! id)))
		(writable #t)
		(configurable #t)
		(enumerable #f))
	     #f
	     %this)
	  ,body)))

;*---------------------------------------------------------------------*/
;*    jsfun-strict-vararg-body ...                                     */
;*---------------------------------------------------------------------*/
(define (jsfun-strict-vararg-body this::J2SFun body id rest)
   (with-access::J2SFun this (params)
      `(let ((arguments (js-strict-arguments %this ,rest)))
	  ,@(if (pair? params)
		(map (lambda (param)
			(with-access::J2SDecl param (loc)
			   (epairify loc
			      `(define ,(j2s-decl-scheme-id param)
				  (js-undefined)))))
		   params)
		'())
	  ,(when (pair? params)
	      `(when (pair? ,rest)
		  (set! ,(j2s-decl-scheme-id (car params)) (car ,rest))
		  ,(let loop ((params (cdr params)))
		      (if (null? params)
			  #unspecified
			  `(when (pair? (cdr ,rest))
			      (set! ,rest (cdr ,rest))
			      (set! ,(j2s-decl-scheme-id (car params))
				 (car ,rest))
			      ,(loop (cdr params)))))))
	  ,body)))
   
;*---------------------------------------------------------------------*/
;*    jsfun->lambda ...                                                */
;*---------------------------------------------------------------------*/
(define (jsfun->lambda this::J2SFun mode return conf proto constructor-only::bool)

   (define (lambda-or-labels %gen this id args body)
      ;; in addition to the user explicit arguments, this and %gen
      ;; are treated as:
      ;;   - some typed functions are optimized and they don't expect a this
      ;;     argument
      ;;   - some typed functions implement a generator body and they take
      ;;     as extra argument the generator
      (let* ((targs (if this (cons this args) args))
	     (gtargs (if %gen (cons '%gen targs) targs)))
	 (if id
	     (let ((%id (j2s-fast-id id)))
		`(labels ((,%id ,gtargs ,body)) ,%id))
	     `(lambda ,gtargs ,body))))
   
   (define (fixarg-lambda fun id body)
      (with-access::J2SFun fun (idgen idthis params)
	 (let ((args (j2s-scheme params mode return conf '() 'any)))
	    (lambda-or-labels idgen idthis id args body))))
   
   (define (rest-lambda fun id body)
      (with-access::J2SFun fun (idgen idthis params)
	 (let ((args (j2s-scheme params mode return conf '() 'any)))
	    (lambda-or-labels idgen idthis id args body))))
   
   (define (normal-vararg-lambda fun id body)
      ;; normal mode: arguments is an alias
      (let ((id (or id (gensym 'fun)))
	    (rest (gensym 'arguments)))
	 (with-access::J2SFun fun (idgen idthis)
	    (lambda-or-labels idgen idthis id rest
	       (jsfun-normal-vararg-body fun body id rest)))))
   
   (define (strict-vararg-lambda fun id body)
      ;; strict mode: arguments is initialized on entrance
      (let ((rest (gensym 'arguments)))
	 (with-access::J2SFun fun (idgen idthis)
	    (lambda-or-labels idgen idthis id rest
	       (jsfun-strict-vararg-body fun body id rest)))))

   (define (generator-body body)
      `(letrec ((%gen (js-make-generator
			 (lambda (%v %e) ,body)
			 ,proto
			 %this)))
	  %gen))

   (define (this-body thisp body mode)
      (if (and thisp (config-get conf optim-this: #f))
	  (cond
	     ((or (not (j2s-this-cache? thisp)) (not constructor-only))
	      (flatten-stmt (j2s-scheme body mode return conf '() 'any)))
	     ((eq? (with-access::J2SDecl thisp (vtype) vtype) 'object)
	      (with-access::J2SDecl thisp (vtype)
		 (set! vtype 'this))
	      (let ((stmt (j2s-scheme body mode return conf '() 'any)))
		 `(with-access::JsObject this (cmap elements)
		     (let ((%thismap cmap)
			   (%thiselements elements))
			,(flatten-stmt stmt)))))
	     (else
	      (with-access::J2SDecl thisp (vtype)
		 (set! vtype 'this))
	      (let ((stmt (j2s-scheme body mode return conf '() 'any)))
		 `(let (%thismap %thiselements)
		     (unless (eq? this (js-undefined))
			(with-access::JsObject this (cmap elements)
			   (set! %thismap cmap)
			   (set! %thiselements elements)))
		     ,(flatten-stmt stmt)))))
	  (flatten-stmt (j2s-scheme body mode return conf '() 'any))))

   (with-access::J2SFun this (loc body need-bind-exit-return vararg mode params generator thisp)
      (let* ((id (j2sfun-id this))
	     (body (cond
		      (generator
		       (with-access::J2SNode body (loc)
			  (epairify loc
			     (generator-body (this-body thisp body mode)))))
		      (need-bind-exit-return
		       (with-access::J2SNode body (loc)
			  (epairify loc
			     (return-body (this-body thisp body mode)))))
		      (else
		       (let ((bd (this-body thisp body mode)))
			  (with-access::J2SNode body (loc)
			     (epairify loc
				(if (pair? bd) bd `(begin ,bd))))))))
	     (fun (cond
		     ((not vararg)
		      (fixarg-lambda this id body))
		     ((eq? vararg 'rest)
		      (rest-lambda this id body))
		     ((eq? mode 'normal)
		      (normal-vararg-lambda this id body))
		     (else
		      (strict-vararg-lambda this id body)))))
	 (epairify-deep loc fun))))

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
;*    j2s-fun-prototype ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-fun-prototype this)
   (with-access::J2SFun this (generator)
      (if generator
	  `(with-access::JsGlobalObject %this (js-generator-prototype)
	      (instantiate::JsObject
		 (__proto__ js-generator-prototype)))
	  #f)))
   
;*---------------------------------------------------------------------*/
;*    j2s-fun-__proto__ ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-fun-__proto__ this)
   (with-access::J2SFun this (generator)
      (if generator
	  `(with-access::JsGlobalObject %this (js-generator-prototype)
	      js-generator-prototype)
	  #f)))
   
;*---------------------------------------------------------------------*/
;*    j2sfun->scheme ...                                               */
;*---------------------------------------------------------------------*/
(define (j2sfun->scheme this::J2SFun tmp mode return conf)
   (with-access::J2SFun this (loc name params mode vararg mode generator
				constrsize)
      (let* ((id (j2sfun-id this))
	     (lparams (length params))
	     (len (if (eq? vararg 'rest) (-fx lparams 1) lparams))
	     (arity (if vararg -1 (+fx 1 (length params))))
	     (minlen (if (eq? mode 'hopscript) (j2s-minlen this) -1))
	     (src (j2s-function-src loc this conf))
	     (prototype (j2s-fun-prototype this))
	     (__proto__ (j2s-fun-__proto__ this)))
	 (epairify-deep loc
	    (if (or src prototype __proto__)
		`(js-make-function %this
		    ,tmp ,len ,(symbol->string! (or name (j2s-decl-scheme-id id)))
		    :src ,src
		    :rest ,(eq? vararg 'rest)
		    :arity ,arity
		    :prototype ,prototype
		    :__proto__ ,__proto__
		    :strict ',mode
		    :minlen ,minlen
		    :alloc (lambda (o) (js-object-alloc o %this))
		    :construct ,tmp
		    :constrsize ,constrsize)
		`(js-make-function-simple %this
		    ,tmp ,len
		    ,(symbol->string! (or name (j2s-decl-scheme-id id)))
		    ,arity ,minlen ',mode ,(eq? vararg 'rest) ,constrsize))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SFun mode return conf hint totype)
   (with-access::J2SFun this (loc name params mode vararg generator)
      (let* ((id (j2sfun-id this))
	     (tmp (gensym id))
	     (arity (if vararg -1 (+fx 1 (length params))))
	     (fundef (if generator
			 (let ((tmp2 (gensym id)))
			    `(letrec* ((,tmp ,(jsfun->lambda this mode return conf
						 `(js-get ,tmp2 'prototype %this)
						 #f))
				       (,tmp2 ,(j2sfun->scheme this tmp mode return conf)))
				,tmp2))
			 `(let ((,tmp ,(jsfun->lambda this mode return conf
					  (j2s-fun-prototype this) #f)))
			     ,(j2sfun->scheme this tmp mode return conf)))))
	 (epairify-deep loc
	    (if id
		(let ((scmid (j2s-scheme-id id '^)))
		   `(let ((,scmid (js-undefined)))
		       (set! ,scmid ,fundef)
		       ,scmid))
		fundef)))))

;*---------------------------------------------------------------------*/
;*    jssvc->scheme ::J2SSvc ...                                       */
;*---------------------------------------------------------------------*/
(define (jssvc->scheme this::J2SSvc id scmid mode return conf)

   (define (j2sscheme-service this tmp scmid path args arity mode return)
      
      (define (jscript-funcall init)
	 ;; see runtime/service_expd.sch
	 "HopService( ~s, ~s )")

      (define (service-debug name loc body)
	 (if (>fx (bigloo-debug) 0)
	     `(lambda () (js-service/debug ',name ',loc ,body))
	     body))

      (define (service-body this::J2SSvc)
	 (with-access::J2SSvc this (loc body need-bind-exit-return name mode)
	    (if need-bind-exit-return
		(with-access::J2SNode body (loc)
		   (epairify loc
		      (return-body
			 (j2s-scheme body mode return conf '() 'any))))
		(flatten-stmt
		   (j2s-scheme body mode return conf '() 'any)))))

      (define (service-fix-proc->scheme this args)
	 (with-access::J2SSvc this (name loc vararg)
	    (let ((imp `(lambda ,(cons 'this args)
			   (js-worker-exec @worker ,(symbol->string scmid)
			      ,(service-debug name loc
				  `(lambda ()
				      ,(service-body this)))))))
	       (epairify-deep loc
		  `(lambda (this . args)
		      (map! (lambda (a) (js-obj->jsobject a %this)) args)
		      ,(case vararg
			  ((arguments)
			   `(let* ((arguments (js-strict-arguments %this args))
				   (fun ,imp))
			       (js-apply-service% fun this args
				  ,(length args))))
			  ((rest)
			   `(let ((fun ,imp))
			       (js-apply-rest% %this fun this args
				  ,(-fx (length args) 1) (+fx 1 (length args)))))
			  (else
			   `(let ((fun ,imp))
			       (js-apply-service% fun this args
				  ,(length args))))))))))
      
      (define (service-call-error this::J2SSvc)
	 (with-access::J2SSvc this (loc name)
	    `(js-raise
		(with-access::JsGlobalObject %this (js-type-error)
		   ,(match-case loc
		       ((at ?fname ?loc)
			`(js-new %this js-type-error
			    ,(j2s-jsstring
				(format "wrong service call \"~s\"" name)
				loc)
			    ,fname ,loc))
		       (else
			`(js-new %this js-type-error
			    ,(j2s-jsstring
				(format "wrong service call \"~s\"" name)
				loc))))))))
      
      (define (service-dsssl-proc->scheme this)
	 (with-access::J2SSvc this (loc init name)
	    (with-access::J2SObjInit init (inits)
	       (let ((imp `(lambda (this #!key ,@(map init->formal inits))
			      (js-worker-exec @worker ,(symbol->string scmid)
				 ,(service-debug name loc
				     `(lambda ()
					 ,(service-body this)))))))
		  (epairify-deep loc
		     `(lambda (this . args)
			 (let ((fun ,imp))
			    (cond
			       ((null? args)
				(fun this))
			       ((and (isa? (car args) JsObject)
				     (null? (cdr args)))
				(apply fun this
				   (js-jsobject->plist (car args) %this)))
			       ((keyword? (car args))
				(apply fun this
				   (js-dsssl-args->jsargs args %this)))
			       ((and (null? (cdr args))
				     (pair? (car args))
				     (keyword? (caar args)))
				(apply fun this
				   (js-dsssl-args->jsargs (car args) %this)))
			       (else
				,(service-call-error this))))))))))
      
      (define (service-proc->scheme this args)
	 (with-access::J2SSvc this (init)
	    (if (isa? init J2SObjInit)
		(service-dsssl-proc->scheme this)
		(service-fix-proc->scheme this args))))
      
      (with-access::J2SSvc this (init register name)
	 (let ((proc (service-proc->scheme this args)))
	    `(let ((@worker (js-current-worker)))
		(js-make-service %this ,tmp ',scmid ,register #f ,arity @worker
		   (instantiate::hop-service
		      (ctx %this)
		      (proc ,proc)
		      (javascript ,(jscript-funcall init))
		      (path ,path)
		      (id ',(or id scmid))
		      (wid ',(or id scmid))
		      (args ,(if (isa? init J2SObjInit)
				 `'(#!key ,@args)
				 `',args))
		      (resource %resource)
		      (source %source)))))))
   
   (define (init->formal init::J2SDataPropertyInit)
      (with-access::J2SDataPropertyInit init (name val)
	 (with-access::J2SString name ((name val))
	    (list (string->symbol name)
	       (j2s-scheme val mode return conf '() 'any)))))
   
   (define (svc-proc-entry this)
      (with-access::J2SSvc this (name params loc path mode)
	 (let ((params (j2s-scheme params mode return conf '() 'any))
	       (tmpp (gensym 'servicep))
	       (tmps (gensym 'services)))
	    `(letrec* ((,tmpp (lambda (this . args)
				 (with-access::JsService ,tmps (svc)
				    (with-access::hop-service svc (path)
				       (js-make-hopframe %this this path args)))))
		       (,tmps ,(j2sscheme-service this tmpp
				  (or scmid name tmpp)
				  (epairify loc
				     `(make-hop-url-name
					 ,(if (symbol? path)
					      (symbol->string path)
					      '(gen-service-url :public #t))))
				  params -1
				  mode return)))
		,tmps))))

   (define (svc->scheme this)
      (with-access::J2SSvc this (name params loc path mode register import)
	 (let ((args (j2s-scheme params mode return conf '() 'any))
	       (lam (jsfun->lambda this mode return conf
		       (j2s-fun-prototype this) #f))
	       (conf (cons* :force-location #t conf)))
	    (match-case lam
	       ((labels (and ?bindings ((?id . ?-))) ?id)
		`(labels ,bindings
		    (js-create-service %this
		       ,(j2sfun->scheme this id mode return conf)
		       ,(when (symbol? path) (symbol->string path))
		       ',loc
		       ,register ,import (js-current-worker))))
	       (else
		`(js-create-service %this
		   ,(j2sfun->scheme this lam mode return conf)
		   ,(when (symbol? path) (symbol->string path))
		   ',loc
		   ,register ,import (js-current-worker)))))))

   (with-access::J2SSvc this (loc)
      (if (config-get conf dsssl: #f) 
	  (epairify-deep loc (svc-proc-entry this))
	  (epairify-deep loc (svc->scheme this)))))
	   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSvc ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSvc mode return conf hint totype)
   (with-access::J2SSvc this (loc)
      (epairify loc
	 (jssvc->scheme this #f #f mode return conf))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SReturn mode return conf hint totype)
   (with-access::J2SReturn this (loc expr tail exit)
      (cond
	 (exit
	  (epairify loc
	     `(%jsexit ,(j2s-scheme expr mode return conf hint totype))))
	 (tail
	  (j2s-scheme expr mode return conf hint totype))
	 (else
	  (epairify loc
	     `(%return ,(j2s-scheme expr mode return conf hint totype)))))))

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
		(flatten-begin (j2s-scheme exprs mode return conf hint totype))))))))

;*---------------------------------------------------------------------*/
;*    j2s-let-decl-toplevel ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-let-decl-toplevel::pair-nil d::J2SDeclInit mode return conf)
   (with-access::J2SDeclInit d (val usage id hint)
      (let ((ident (j2s-decl-scheme-id d)))
	 (cond
	    ((or (not (isa? val J2SFun)) (isa? val J2SSvc) (memq 'assig usage))
	     `(define ,ident ,(j2s-scheme val mode return conf hint 'any)))
	    ((or (memq 'ref usage) (memq 'get usage) (memq 'new usage))
	     (let ((fun (jsfun->lambda val mode return conf
			   `(js-get ,ident 'prototype %this) #f))
		   (tmp (j2s-fast-id id)))
		`(begin
		    (define ,tmp ,fun)
		    (define ,ident ,(j2sfun->scheme val tmp mode return conf)))))
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
      (with-access::J2SDeclInit d (val usage id utype ronly)
	 (let* ((ident (j2s-decl-scheme-id d))
		(var (utype-ident ident utype conf singledecl)))
	    (cond
	       ((or (not (isa? val J2SFun)) (isa? val J2SSvc) (memq 'assig usage))
		(list `(,var ,(j2s-scheme val mode return conf hint utype))))
	       ((or (memq 'ref usage) (memq 'get usage) (memq 'new usage))
		(let ((fun (jsfun->lambda val mode return conf
			      `(js-get ,ident 'prototype %this) #f))
		      (tmp (j2s-fast-id id)))
		   `((,tmp ,fun)
		     (,var ,(j2sfun->scheme val tmp mode return conf)))))
	       ((memq 'call usage)
		`((,(j2s-fast-id id)
		   ,(jsfun->lambda val mode return conf (j2s-fun-prototype val) #f))))
	       (else
		'())))))
   
   (with-access::J2SLetBlock this (loc decls nodes)
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
		    `(letrec ,ds ,@body))
		   (else
		    `(letrec* ,ds ,@body)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-num ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-num num)
   num)

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
       (let ((c1 (if (is-fixnum? lhs conf) #t `(fixnum? ,left)))
	     (c2 (if (is-fixnum? rhs conf) #t `(fixnum? ,right))))
	  (cond
	     ((and (eq? c1 #t) (eq? c2 #t))
	      `(,(fx op) ,left ,right))
	     (else
	      `(if (and ,c1 ,c2)
		   (,(fx op) ,left ,right)
		   (,op ,left ,right))))))))

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
	  (equal? expr '(js-null))))
   
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
;*    js-bitop ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-bitop loc op::symbol type lhs::J2SExpr rhs::J2SExpr
	   mode return conf hint::pair-nil totype)
   
   (define (fxop op)
      (case op
	 ((&) 'bit-ands32)
	 ((BIT_OR) 'bit-ors32)
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
   
   (define (return expr)
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
		    (return
		       `(,(fxop op) ,(fx->int32 left) ,(bit-andfx right 31))))))
	     (else
	      (binop lhs rhs mode return conf hint type
		 (lambda (left right)
		    (return 
		       `(,(fxop op) ,(fx->int32 left) ,(fx->int32 right))))))))
	 ((or (type-fixnum? tr) (eq? tr 'int32))
	  (case op
	     ((<< >> >>>)
	      (binop lhs rhs mode return conf hint type
		 (lambda (left right)
		    `(if (fixnum? ,left)
			 ,(return
			     `(,(fxop op) ,(fx->int32 left) ,(bit-andfx right 31)))
			 ,(js-binop loc op left right)))))
	     (else
	      (binop lhs rhs mode return conf hint type
		 (lambda (left right)
		    `(if (fixnum? ,left)
			 ,(return 
			     `(,(fxop op) ,(fx->int32 left) ,(fx->int32 right)))
			 ,(js-binop loc op left right)))))))
	 ((memq op '(BIT_OR &))
	  (binop lhs rhs mode return conf hint 'any
	     (lambda (left right)
		`(if (and (fixnum? ,left) (fixnum? ,right))
		     ,(return
			`(,(fxop op) ,(fx->int32 left) ,(fx->int32 right)))
		     ,(js-binop loc op left right)))))
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
		     (js-tostring (js-toprimitive ,right 'any %this) %this)))
		(else
		 `(js-jsstring-append
		     (js-tostring (js-toprimitive ,left 'any %this) %this)
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
		(j2s-num-op op left right lhs rhs conf)))))
      (else
       (binop lhs rhs mode return conf hint 'integer
	  (lambda (left right)
	     (if (and (maybe-number? lhs) (maybe-number? rhs))
		 `(if ,(scm-and (scm-fixnum? left lhs) (scm-fixnum? right rhs))
		      ,(js-binopfx op left right conf)
		      (,(jsop op) ,left ,right %this))
		 `(,(jsop op) ,left ,right %this)))))))

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
   (cond
      ((eq? left #t) (if (eq? right #t) #t right))
      ((eq? right #t) left)
      ((and left right) `(and ,left ,right))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    scm-if ...                                                       */
;*---------------------------------------------------------------------*/
(define (scm-if test then otherwise)
   (if (eq? test #t)
       then
       `(if ,test ,then ,otherwise)))

;*---------------------------------------------------------------------*/
;*    js-binop2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-binop2 loc op type lhs rhs mode return conf hint::pair-nil totype)
   
   (define (j2s-aref-length? expr::J2SExpr)
      (when (isa? expr J2SAccess)
	 (with-access::J2SAccess expr (obj field)
	    (when (j2s-field-length? field)
	       (isa? obj J2SAref)))))

   (define (j2s-cast-aref-length? expr::J2SExpr)
      (when (isa? expr J2SCast)
	 (with-access::J2SCast expr (expr)
	    (j2s-aref-length? expr))))

   (define (is-fixnum/conf? expr)
      (is-fixnum? expr conf))

   (define (aref::J2SAref expr::J2SAccess)
      (with-access::J2SAccess expr (obj field)
	 obj))
   
   (define (cast-aref::J2SAref expr::J2SCast)
      (when (isa? expr J2SCast)
	 (with-access::J2SCast expr (expr)
	    (aref expr))))
   
   (case op
      ((+ - *)
       (js-arithmetic loc op type lhs rhs mode return conf hint type))
      ((< <= > >=)
       (js-cmp loc op lhs rhs mode return conf hint totype))
      ((& BIT_OR >> >>> <<)
       (js-bitop loc op type lhs rhs mode return conf hint type))
      ((%)
       (cond
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
      ((== === != !==)
       (cond
	  ((and (is-uint32? lhs) (is-uint32? rhs))
	   (cond
	      ((j2s-aref-length? rhs)
	       (with-access::J2SAref (aref rhs) (field alen)
		  (let ((test `(or (=u32 %lhs ,(j2s-decl-scheme-id alen))
				   (=u32 %lhs ,(j2s-scheme rhs mode return conf '(fixnum) 'any)))))
		     `(let ((%lhs ,(j2s-scheme lhs mode return conf '(fixnum) 'any)))
			 ,(if (memq op '(!= !==))
			      `(not ,test)
			      test)))))
	      ((j2s-aref-length? lhs)
	       (with-access::J2SAref (aref lhs) (field alen)
		  (let ((test `(or (=u32 ,(j2s-decl-scheme-id alen) %rhs)
				   (=u32 ,(j2s-scheme rhs mode return conf '(fixnum) 'any)
				      %rhs))))
		     `(let ((%rhs ,(j2s-scheme rhs mode return conf '(fixnum) 'any)))
			 ,(if (memq op '(!= !==))
			      `(not ,test)
			      test)))))
	      (else
	       (let ((res `(=u32 ,(j2s-scheme lhs mode return conf '(fixnum) 'any)
			      ,(j2s-scheme rhs mode return conf '(fixnum) 'any))))
		  (if (memq op '(!= !==))
		      `(not ,res)
		      res)))))
	  ((and (is-fixnum/conf? lhs) (is-fixnum/conf? rhs))
	   (cond
	      ((j2s-cast-aref-length? rhs)
	       (with-access::J2SAref (cast-aref rhs) (field alen)
		  (let ((test `(or (=fx %lhs ,(j2s-decl-scheme-id alen))
				   (=fx %lhs ,(j2s-scheme rhs mode return conf '(fixnum) 'any)))))
		     `(let ((%lhs ,(j2s-scheme lhs mode return conf '(fixnum) 'any)))
			 ,(if (memq op '(!= !==))
			      `(not ,test)
			      test)))))
	      ((j2s-cast-aref-length? lhs)
	       (with-access::J2SAref (cast-aref lhs) (field alen)
		  (let ((test `(or (=fx ,(j2s-decl-scheme-id alen) %rhs)
				   (=fx ,(j2s-scheme rhs mode return conf '(fixnum) 'any)
				      %rhs))))
		     `(let ((%rhs ,(j2s-scheme rhs mode return conf '(fixnum) 'any)))
			 ,(if (memq op '(!= !==))
			      `(not ,test)
			      test)))))
	      (else
	       (let ((res (binop lhs rhs mode return conf '(fixnum) 'any
			     (lambda (left right)
				`(=fx ,left ,right)))))
		  (if (memq op '(!= !==))
		      `(not ,res)
		      res)))))
	  ((and (is-number? lhs) (is-number? rhs))
	   (cond
	      ((j2s-cast-aref-length? rhs)
	       (with-access::J2SAref (cast-aref rhs) (field alen)
		  (let ((test `(or (= %lhs ,(j2s-decl-scheme-id alen))
				   (= %lhs ,(j2s-scheme rhs mode return conf '(number) 'any)))))
		     `(let ((%lhs ,(j2s-scheme lhs mode return conf '(number) 'any)))
			 ,(if (memq op '(!= !==))
			      `(not ,test)
			      test)))))
	      ((j2s-cast-aref-length? lhs)
	       (with-access::J2SAref (cast-aref lhs) (field alen)
		  (let ((test `(or (= ,(j2s-decl-scheme-id alen) %rhs)
				   (= ,(j2s-scheme rhs mode return conf '(number) 'any)
				      %rhs))))
		     `(let ((%rhs ,(j2s-scheme rhs mode return conf '(number) 'any)))
			 ,(if (memq op '(!= !==))
			      `(not ,test)
			      test)))))
	      (else
	       (let ((res (binop lhs rhs mode return conf hint 'any
			     (lambda (left right)
				(j2s-num-op '= left right lhs rhs conf)))))
		  (if (memq op '(!= !==))
		      `(not ,res)
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
;*    j2s-scheme ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBinary mode return conf hint totype)
   (with-access::J2SBinary this (loc op lhs rhs type hint)
      (epairify-deep loc
	 (js-binop2 loc op type lhs rhs mode return conf hint type))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SParen ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SParen mode return conf hint totype)
   (with-access::J2SParen this (expr type)
      (j2s-scheme expr mode return conf (list type) type)))

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
;*    j2s-scheme ::J2SPostfix ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.3.1       */
;*    -------------------------------------------------------------    */
;*    !!! x++ not equivalent to x = x + 1 as x++ always converts       */
;*    to number.                                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPostfix mode return conf hint totype)

   (define (int++ type typesuf)
      (with-access::J2SPostfix this (loc lhs op)
	 (let ((scmlhs (j2s-scheme lhs mode return conf '(int) type))
	       (op (symbol-append (if (eq? op '++) '+ '-) typesuf))
	       (inc (cast 1 conf 'fixnum type)))
	    (if (eq? totype 'void)
		(j2s-scheme-set! lhs `(,op ,scmlhs ,inc) #f mode return conf)
		(let ((tmp (gensym 'tmp)))
		   `(let ((,tmp ,scmlhs))
		       ,(j2s-scheme-set! lhs
			   (epairify loc `(,op ,tmp ,inc))
			   tmp
			   mode return conf)
		       ,tmp))))))
   
   (with-access::J2SPostfix this (loc lhs op type)
      (let ((inc (j2s-num (if (eq? op '++) (cast 1 conf 'fixnum type) -1))))
	 (let loop ((lhs lhs))
	    (cond
	       ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
		(epairify-deep loc
		   (cond
		      ((and (type-uint32? type) (is-uint32? lhs))
		       (int++ 'uint32 'u32))
		      ((and (type-int53? type) (is-uint53? lhs) (m64? conf))
		       (int++ 'int53 'fx))
		      ((and (type-fixnum? type) (is-fx? lhs))
		       (int++ 'fixnum 'fx))
		      ((and (type-int30? type) (is-int30? lhs))
		       (int++ 'int30 'fx))
		      ((and (memq type '(fixnum))
			    (memq (j2s-type lhs) '(fixnum)))
		       (int++ 'fixnum 'fx))
		      ((memq (j2s-type lhs) '(integer number uint29 ufixnum fixnum index))
		       (if (eq? hint 'void)
			   (j2s-scheme-set! lhs
			      (epairify loc
				 (j2s-num-op '+ inc
				    (j2s-scheme lhs mode return conf '(int) type)
				    lhs lhs conf))
			      #f
			      mode return conf)
			   (let ((tmp (gensym 'tmp)))
			      `(let ((,tmp ,(j2s-scheme lhs mode return conf hint totype)))
				  ,(j2s-scheme-set! lhs
				      (epairify loc
					 (j2s-num-op '+ inc tmp lhs lhs conf))
				      tmp
				      mode return conf)
				  ,tmp))))
		      (else
		       (let ((tmp (gensym 'tmp)))
			  `(let ((,tmp (js-tonumber ,(j2s-scheme lhs mode return conf hint totype) %this)))
			      ,(j2s-scheme-set! lhs
				  (epairify loc `(js+ ,inc ,tmp %this))
				  tmp
				  mode return conf)))))))
	       ((isa? lhs J2SAccess)
		(with-access::J2SAccess lhs ((o obj) field cache (loca loc))
		   (let ((tmp (gensym 'tmp))
			 (obj (gensym 'obj))
			 (pro (gensym 'prop1))
			 (prov (if (isa? field J2SString)
				   (with-access::J2SString field (val)
				      val)
				   (j2s-property-scheme field mode return conf)))
			 (tyo (typeof-this o conf)))
		      (epairify-deep loc
			 `(let* ((,obj ,(j2s-scheme o mode return conf hint totype))
				 ,@(if (string? prov) '() (list `(,pro ,prov)))
				 (,tmp (js-tonumber
					  ,(j2s-get loca obj tyo
					      (if (string? prov) prov pro)
					      (j2s-type field)
					      cache)
					  %this)))
			     ,(j2s-put! loca obj
				 (if (strict-mode? mode) 'object tyo)
				 (if (string? prov) prov pro)
				`(js+ ,inc ,tmp %this) (strict-mode? mode)
				cache)
			     ,tmp)))))
	       ((isa? lhs J2SUnresolvedRef)
		(with-access::J2SUnresolvedRef lhs (id cache loc)
		   (let ((tmp (gensym 'tmp)))
		      (epairify-deep loc
			 `(let ((,tmp (js-tonumber
					 ,(j2s-unresolved id cache loc)
					 %this)))
			     ,(j2s-unresolved-put! `',id `(+ ,inc ,tmp)
				 #t mode return)
			     ,tmp)))))
	       ((isa? lhs J2SParen)
		(with-access::J2SParen lhs (expr)
		   (loop expr)))
	       (else
		(j2s-error "j2sscheme"
		   (format "Illegal postfix \"~a\"" op)
		   this)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPrefix ...                                       */
;*    -------------------------------------------------------------    */
;*    www.ecma-international.org/ecma-262/5.1/#sec-11.3.1prefix        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPrefix mode return conf hint totype)
   (with-access::J2SPrefix this (loc lhs op type)
      (let ((inc (j2s-num (if (eq? op '++) 1 -1))))
	 (let loop ((lhs lhs))
	    (cond
	       ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
		(epairify loc
		   (j2s-scheme-set! lhs
		      (epairify loc
			 `(js+ ,inc
			     (js-tonumber
				,(j2s-scheme lhs mode return conf '(number) type)
				%this)  %this))
		      (j2s-scheme lhs mode return conf '(number) type)
		      mode return conf)))
	       ((isa? lhs J2SAccess)
		(with-access::J2SAccess lhs ((o obj) field cache (loca loc))
		   (let* ((tmp (gensym 'tmp))
			  (obj (gensym 'obj))
			  (pro (gensym 'prop))
			  (res (gensym 'res))
			  (prov (if (isa? field J2SString)
				    (with-access::J2SString field (val)
				       val)
				    (j2s-scheme field mode return conf hint totype)))
			  (tyo (typeof-this obj conf)))
		      (epairify-deep loc
			 `(let* ((,obj ,(j2s-scheme o mode return conf hint totype))
				 ,@(if (string? prov) '() (list `(,pro ,prov)))
				 (,tmp (js-tonumber
					  ,(j2s-get loca obj tyo
					      (if (string? prov) prov pro)
					      (j2s-type field)
					      cache)
					  %this))
				 (,res (js+ ,inc ,tmp %this)))
			     ,(j2s-put! loca obj
				 (if (strict-mode? mode) 'object tyo)
				 (if (string? prov) prov pro)
				 res
				 (strict-mode? mode) cache)
			     ,res)))))
	       ((isa? lhs J2SUnresolvedRef)
		(with-access::J2SUnresolvedRef lhs (id cache loc)
		   (let ((tmp (gensym 'tmp)))
		      (epairify-deep loc
			 `(let ((,tmp (js+ ,inc
					 (js-tonumber ,(j2s-unresolved id cache loc)
					    %this )
					 %this)))
			     ,(j2s-unresolved-put! `',id tmp #t mode return)
			     ,tmp)))))
	       ((isa? lhs J2SParen)
		(with-access::J2SParen lhs (expr)
		   (loop expr)))
	       (else
		(j2s-error "j2sscheme"
		   (format "Illegal prefix \"~a\"" op)
		   this)))))))

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
	     (isa? expr J2SUndefined)))
	 ((isa? stmt J2SNop)
	  #t)))
   
   (let loop ((nodes nodes))
      (cond
	 ((null? nodes)
	  (epairify loc
	     (return '(js-undefined))))
	 ((not (pair? (cdr nodes)))
	  (j2s-scheme (car nodes) mode return conf hint totype))
	 ((undefined? (car nodes))
	  (loop (cdr nodes)))
	 (else
	  (epairify loc
	     (flatten-begin (j2s-scheme nodes mode return conf hint totype)))))))

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
		 (isa? (car nodes) J2SDeclFunCnst)
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
      (if (isa? expr J2SIf)
	  (j2s-scheme expr mode return conf hint totype)
	  (return (j2s-scheme expr mode return conf hint totype)))))

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
	     (epairify loc (j2s-scheme-set! lhs name #f mode return conf)))
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
(define (j2s-inline-method field args type)
   (when (isa? field J2SString)
      (with-access::J2SString field (val)
	 (find (lambda (method)
		  (when (string=? (inline-method-jsname method) val)
		     (let ((ty (inline-method-ttype method)))
			(when (or (eq? ty 'any) (eq? ty type))
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
	 ((isa? decl J2SDeclFunCnst)
	  decl)
	 ((j2s-let-opt? decl)
	  (with-access::J2SDeclInit decl (usage id val)
	     (when (isa? val J2SFun)
		(unless (memq 'assig usage) decl))))
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

   (define (call-ref-method ccache ocache fun::J2SAccess obj::J2SExpr args)
      (with-access::J2SAccess fun (loc field)
	 (cond
	    ((j2s-inline-method field args (j2s-type obj))
	     =>
	     (lambda (m)
		(let* ((id (inline-method-scmid m))
		       (opt (inline-method-args m))
		       (arity (length opt))
		       (len (length args)))
		   `(,id ,(j2s-scheme obj mode return conf hint totype)
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
			     '())))))
	    ((and ccache (= (bigloo-debug) 0))
	     (cond
		((isa? field J2SString)
		 (let ((call (if (eq? (j2s-type obj) 'object)
				 'js-object-call-name/cache
				 'js-call-name/cache)))
		    (with-access::J2SString field (val)
		       `(,call
			   ,j2s-unresolved-call-workspace
			   ,(j2s-scheme obj mode return conf hint totype)
			   ',(string->symbol val)
			   ,(pcache ccache)
			   ,(pcache ocache)
			   ,@(map (lambda (arg)
				     (j2s-scheme arg mode return conf hint totype))
				args)))))
		(else
		 (call-unknown-function fun
		    (j2s-scheme obj mode return conf hint totype)
		    args))))
	    (else
	     (call-unknown-function fun
		(j2s-scheme obj mode return conf hint totype)
		args)))))
   
   (define (call-method ccache ocache fun::J2SAccess args)
      (with-access::J2SAccess fun (loc obj field)
	 (if (isa? obj J2SRef)
	     (call-ref-method ccache ocache fun obj args)
	     (let ((tmp (gensym)))
		`(let ((,tmp ,(j2s-scheme obj mode return conf hint totype)))
		    ,(call-ref-method
			ccache ocache
			(duplicate::J2SAccess fun
			   (obj (instantiate::J2SPragma
				   (loc loc)
				   (expr tmp))))
			(instantiate::J2SHopRef
			   (type (j2s-type obj))
			   (loc loc)
			   (id tmp))
			args))))))
   
   (define (call-hop-function fun::J2SHopRef args)
      `(,(j2s-scheme fun mode return conf hint 'any)
	,@(j2s-scheme args mode return conf hint 'any)))

   (define (j2s-this this)
      (cond
	 ((eq? this #unspecified) '((js-undefined)))
	 (this (list (j2s-scheme this mode return conf hint totype)))
	 (else '())))

   (define (call-rest-function fun::J2SFun this f %gen args)
      ;; call a function that accepts a rest argument
      (with-access::J2SFun fun (params vararg)
	 (let loop ((params params)
		    (args args)
		    (actuals '()))
	    (cond
	       ((null? (cdr params))
		;; the rest argument
		`(,f ,@%gen ,@(j2s-this this) ,@(reverse! actuals)
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

   (define (call-fix-function fun::J2SFun this f %gen args)
      ;; call a function that accepts a fix number of arguments
      (with-access::J2SFun fun (params vararg)
	 (let ((lenf (length params))
	       (lena (length args)))
	    (cond
	       ((=fx lenf lena)
		;; matching arity
		`(,f ,@%gen
		    ,@(j2s-this this)
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
		       (,f ,@%gen ,@(j2s-this this) ,@(take temps lenf)))))
	       (else
		;; argument missing
		`(,f ,@(j2s-this this)
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

   (define (call-fun-function fun::J2SFun this protocol f %gen::pair-nil args::pair-nil)
      (with-access::J2SFun fun (params vararg idthis)
	 (case (if (eq? protocol 'bounce) 'bounce vararg)
	    ((arguments)
	     `(,f ,@%gen ,@(if idthis (j2s-this this) '())
		 ,@(j2s-scheme args mode return conf hint totype)))
	    ((rest)
	     (call-rest-function fun (and idthis this) f %gen args))
	    (else
	     (call-fix-function fun (and idthis this) f %gen args)))))

   (define (call-with-function fun::J2SWithRef args)
      (with-access::J2SWithRef fun (id withs loc)
	 (let loop ((withs withs))
	    (if (null? withs)
		(call-unknown-function fun '(js-undefined) args)
		`(if ,(j2s-in? loc `',id (car withs))
		     ,(call-unknown-function
			 (j2s-get loc (car withs) 'object `',id 'string #f)
			(car withs) args)
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

   (define (call-known-function protocol fun::J2SDecl this args)
      (cond
	 ((isa? fun J2SDeclFun)
	  (with-access::J2SDeclFun fun (id val)
	     (check-hopscript-fun-arity val id args)
	     (let ((%gen (if (typed-generator? fun) '(%gen) '())))
		(call-fun-function val this protocol
		   (j2s-fast-id id) %gen args))))
	 ((isa? fun J2SDeclFunCnst)
	  (with-access::J2SDeclFunCnst fun (id val)
	     (check-hopscript-fun-arity val id args)
	     (call-fun-function val this protocol
		(j2s-fast-id id) '() args)))
	 ((j2s-let-opt? fun)
	  (with-access::J2SDeclInit fun (id val)
	     (call-fun-function val this protocol
		(j2s-fast-id id) '() args)))
	 (else
	  (error "js-scheme" "Should not be here" (j2s->list fun)))))

   (define (call-unknown-function fun thisarg args)
      (let* ((len (length args))
	     (call (if (>=fx len 9)
		       'js-calln
		       (string->symbol (format "js-call~a" len)))))
	 (with-access::J2SCall this (loc cache)
	    (cond
	       ((> (bigloo-debug) 0)
		`(,(symbol-append call '/debug)
		  ,j2s-unresolved-call-workspace
		  ',loc
		  ,(j2s-scheme fun mode return conf hint totype)
		  ,thisarg
		  ,@(j2s-scheme args mode return conf hint totype)))
	       (cache
		`(js-call/cache
		    ,j2s-unresolved-call-workspace
		    ,(j2s-scheme fun mode return conf hint totype)
		    ,(pcache cache)
		    ,thisarg
		    ,@(j2s-scheme args mode return conf hint totype)))
	       (else
		`(,call ,j2s-unresolved-call-workspace
		    ,(j2s-scheme fun mode return conf hint totype)
		    ,thisarg
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

   (define expr this)

   (with-access::J2SCall this (loc fun this args protocol cache)
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
		(call-hop-function fun args))
	       ((and (isa? fun J2SFun) (not (j2sfun-id fun)))
		(call-fun-function fun this protocol
		   (jsfun->lambda fun mode return conf (j2s-fun-prototype fun) #f)
		   '()
		   args))
	       ((isa? fun J2SUnresolvedRef)
		(if (is-eval? fun)
		    (call-eval-function fun args)
		    (call-unknown-function fun '(js-undefined) args)))
	       ((isa? fun J2SWithRef)
		(call-with-function fun args))
	       ((isa? fun J2SPragma)
		(call-pragma fun args))
	       ((not (isa? fun J2SRef))
		(call-unknown-function fun '(js-undefined) args))
	       ((read-only-function fun)
		=>
		(lambda (fun) (call-known-function protocol fun this args)))
	       (else
		(call-unknown-function fun '(js-undefined) args)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssig mode return conf hint totype)
   
   (define (aset/cache lhs rhs)
      ;; an optimized array set in a loop (see array.scm)
      (with-access::J2SAccess lhs (obj field)
	 (with-access::J2SAref obj (array alen mark deps)
	    (let ((scmarray (j2s-decl-scheme-id array))
		  (scmalen (j2s-decl-scheme-id alen))
		  (scmfield (j2s-scheme field mode return conf hint (j2s-type field)))
		  (scmobj (j2s-scheme obj mode return conf hint 'array))
		  (scmrhs (j2s-scheme rhs mode return conf hint 'any)))
	       (case (j2s-type field)
		  ((index uint29)
		   (let ((idx (j2s-scheme field mode return conf hint 'uint32)))
		      (if mark
			  `(JS-ARRAY-INDEX-MARK-SET! ,scmobj
			      ,idx ,scmrhs
			      ,scmarray ,scmalen
			      ,(j2s-decl-scheme-id mark)
			      ,(strict-mode? mode)
			      %this)
			  `(JS-ARRAY-INDEX-FAST-SET! ,scmobj
			      ,idx ,scmrhs
			      ,scmarray ,scmalen
			      ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			      ,(strict-mode? mode)
			      %this))))
		  ((ufixnum)
		   (let ((idx (j2s-scheme field mode return conf hint 'ufixnum)))
		      (if mark
			  `(JS-ARRAY-INDEX-MARK-SET! ,scmobj
			      (fixnum->uint32 ,idx) ,scmrhs
			      ,scmarray ,scmalen
			      ,(j2s-decl-scheme-id mark)
			      ,(strict-mode? mode)
			      %this)
			  `(JS-ARRAY-INDEX-FAST-SET! ,scmobj
			      (fixnum->uint32 ,idx) ,scmrhs
			      ,scmarray ,scmalen
			      ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			      ,(strict-mode? mode)
			      %this))))
		  ((fixnum)
		   (if mark
		       `(JS-ARRAY-FIXNUM-MARK-SET! ,scmobj ,scmfield ,scmrhs
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id mark)
			   ,(strict-mode? mode)
			   %this)
		       `(JS-ARRAY-FIXNUM-MARK-SET! ,scmobj ,scmfield ,scmrhs
			   ,scmarray ,scmalen
			   ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			   ,(strict-mode? mode)
			   %this)))
		  (else
		   (if mark
		       `(JS-ARRAY-MARK-SET! ,scmobj ,scmfield ,scmrhs
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id mark)
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
		   ((index)
		    `(js-array-index-set!
			,(j2s-scheme obj mode return conf hint totype)
			(fixnum->uint32
			   ,(j2s-scheme field mode return conf hint totype))
			,(j2s-scheme rhs mode return conf hint totype)
			,(strict-mode? mode)
			%this))
		   ((fixnum ufixnum uint29)
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
   
   (with-access::J2SAssig this (loc lhs rhs)
      (let loop ((lhs lhs))
	 (cond
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field cache (loca loc))
		(epairify loc
		   (if (and (eq? (j2s-type obj) 'array) (maybe-number? field))
		       (array-set lhs rhs)
		       (j2s-put! loca (j2s-scheme obj mode return conf hint totype)
			  (typeof-this obj conf)
			  (j2s-scheme field mode return conf hint totype)
			  (j2s-scheme rhs mode return conf hint totype)
			  (strict-mode? mode)
			  cache)))))
	    ((and (isa? lhs J2SRef)  (not (isa? lhs J2SThis)))
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (hint)
		   (let ((assig (j2s-scheme-set! lhs
				   (j2s-scheme rhs mode return conf hint totype)
				   (j2s-scheme lhs mode return conf hint totype)
				   mode return conf)))
		      (if (pair? assig)
			  (epairify loc assig)
			  assig)))))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(epairify loc
		   (j2s-unresolved-put! `',id
		      (j2s-scheme rhs mode return conf hint totype) #f mode return))))
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
;*    j2s-scheme ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAccess mode return conf hint totype)
   
   (define (aref/cache this::J2SAccess)
      (with-access::J2SAccess this (obj field)
	 (with-access::J2SAref obj (array alen mark deps)
	    (let ((scmarray (j2s-decl-scheme-id array))
		  (scmalen (j2s-decl-scheme-id alen))
		  (scmfield (j2s-scheme field mode return conf hint (j2s-type field)))
		  (scmobj (j2s-scheme obj mode return conf hint 'array)))
	       (case (j2s-type field)
		  ((index uint29)
		   (let ((idx (j2s-scheme field mode return conf hint 'uint32)))
		      (if mark
			  `(JS-ARRAY-INDEX-MARK-REF ,scmobj
			      ,idx
			      ,scmarray ,scmalen
			      ,(j2s-decl-scheme-id mark)
			      %this)
			  `(JS-ARRAY-INDEX-FAST-REF ,scmobj
			      ,idx
			      ,scmarray ,scmalen 
			      ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			      %this))))
		  ((ufixnum)
		   (let ((idx (j2s-scheme field mode return conf hint 'ufixnum)))
		      (if mark
			  `(JS-ARRAY-INDEX-MARK-REF ,scmobj
			      (fixnum->uint32 ,idx)
			      ,scmarray ,scmalen
			      ,(j2s-decl-scheme-id mark)
			      %this)
			  `(JS-ARRAY-INDEX-FAST-REF ,scmobj
			      (fixnum->uint32 ,idx)
			      ,scmarray ,scmalen 
			      ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			      %this))))
		  ((fixnum)
		   (if mark
		       `(JS-ARRAY-FIXNUM-MARK-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id mark)
			   %this)
		       `(JS-ARRAY-FIXNUM-FAST-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id mark)
			   %this)))
		  (else
		   (if mark
		       `(JS-ARRAY-MARK-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id mark)
			   %this)
		       `(JS-ARRAY-FAST-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			   %this))))))))
   
   (define (aref/w-cache this::J2SAccess)
      (with-access::J2SAccess this (obj field)
	 (case (j2s-type field)
	    ((index uint29)
	     `(js-array-index-ref ,(j2s-scheme obj mode return conf hint 'array)
		 ,(j2s-scheme field mode return conf hint 'uint32)
		 %this))
	    ((ufixnum)
	     `(js-array-index-ref ,(j2s-scheme obj mode return conf hint 'array)
		 ,(js-fixnum->uint32 (j2s-scheme field mode return conf hint 'uint32))
		 %this))
	    ((fixnum)
	     `(js-array-fixnum-ref ,(j2s-scheme obj mode return conf hint totype)
		 ,(j2s-scheme field mode return conf hint totype)
		 %this))
	    (else
	     `(js-array-ref ,(j2s-scheme obj mode return conf hint totype)
		 ,(j2s-scheme field mode return conf hint totype)
		 %this)))))
   
   (define (aref this::J2SAccess)
      (if *j2s-array-cache*
	  (aref/cache this)
	  (aref/w-cache this)))
   
   (define (array-ref obj field)
      (cond
	 ((isa? obj J2SAref)
	  (aref this))
	 ((is-fixnum? field conf)
	  (aref/w-cache this))
	 ((eq? (j2s-type field) 'index)
	  `(js-array-ref-ur ,(j2s-scheme obj mode return conf hint totype)
	      ,(js-uint32->fixnum
		  (j2s-scheme field mode return conf hint totype) conf)
	      %this))
	 (else
	  `(js-array-ref ,(j2s-scheme obj mode return conf hint totype)
	      ,(j2s-scheme field mode return conf hint totype)
	      %this))))
   
   (with-access::J2SAccess this (loc obj field cache type)
      (epairify-deep loc 
	 (cond
	    ((and (eq? (j2s-type obj) 'array) (maybe-number? field))
	     (array-ref obj field))
	    ((and (eq? (j2s-type obj) 'array) (j2s-field-length? field))
	     (let ((x `(js-array-length
			  ,(j2s-scheme obj mode return conf hint 'array))))
		(if (memq type '(index uint32 length))
		    x
		    (js-uint32->jsnum x conf))))
	    ((and (eq? (j2s-type obj) 'string) (j2s-field-length? field))
	     (let ((x `(js-jsstring-codeunit-length
			  ,(j2s-scheme obj mode return conf hint totype))))
		(if (memq type '(index uint32 length))
		    x
		    (js-uint32->jsnum x conf))))
	    (else
	     (let ((tyo (typeof-this obj conf)))
		(j2s-get loc (j2s-scheme obj mode return conf hint totype) tyo
		   (j2s-property-scheme field mode return conf)
		   (j2s-type field) cache)))))))

;*---------------------------------------------------------------------*/
;*    maybe-number? ...                                                */
;*---------------------------------------------------------------------*/
(define (maybe-number? expr::J2SNode)
   (memq (j2s-type expr)
      '(index uint29 int30 fixnum int53 ufixnum integer number any)))

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
			   #f mode return conf)
		       (js-undefined)))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssigOp mode return conf hint totype)
   (with-access::J2SAssigOp this (loc lhs rhs op type)
      (epairify-deep loc
	 (cond
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field loc cache)
		(let ((tobj (gensym 'obj))
		      (pro (gensym 'pro))
		      (prov (j2s-scheme field mode return conf hint totype))
		      (left (gensym 'left))
		      (right (gensym 'right)))
		   `(let* ((,tobj ,(j2s-scheme obj mode return conf hint totype))
			   ,@(if (string? prov) '() (list `(,pro ,prov)))
			   (,left ,(j2s-get loc tobj (j2s-type obj)
				      (if (string? prov) prov pro)
				      (j2s-type field)
				      cache))
			   (,right ,(j2s-scheme rhs mode return conf '() 'any)))
		       ,(j2s-put! loc tobj (typeof-this obj conf)
			   (if (string? prov) prov prov)
			   (js-binop loc op left right)
			   (strict-mode? mode)
			   #f)))))
	    ((and (isa? lhs J2SRef) (not (isa? lhs J2SThis)))
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (hint utype)
		   (j2s-scheme-set! lhs
		      (js-binop2 loc op type lhs rhs mode return conf hint utype)
		      (j2s-scheme lhs mode return conf '() utype)
		      mode return conf))))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(j2s-unresolved-put! `',id
		   (js-binop2 loc op type lhs rhs mode return conf '() type)
		   #t mode return)))
	    (else
	     (j2s-error "j2sscheme" "Illegal assignment" this))))))

;*---------------------------------------------------------------------*/
;*    j2s-get ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-get loc obj tyobj prop typrop cache)

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
	  `(js-get-string ,obj ,prop %this))
	 (cache
	  (cond
	     ((string? prop)
	      (if (string=? prop "length")
		  `(js-get-length ,obj ,(pcache cache) %this)
		  (case tyobj
		     ((this)
		      `(js-this-get-name/cache ,obj
			  ',(string->symbol prop) ,(pcache cache) %this))
		     ((object)
		      `(js-object-get-name/cache ,obj
			  ',(string->symbol prop) ,(pcache cache) %this))
		     ((global)
		      `(js-global-object-get-name/cache ,obj
			  ',(string->symbol prop) ,(pcache cache) #f %this))
		     (else
		      `(js-get-name/cache ,obj
			  ',(string->symbol prop) ,(pcache cache) %this)))))
	     ((maybe-string? prop typrop)
	      `(js-get/cache ,obj ,prop ,(pcache cache) %this))
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
(define (j2s-put! loc obj tyobj prop val mode cache)
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
		  `(js-put-length! ,obj ,val ,mode ,(pcache cache) %this)
		  (begin
		     (case tyobj
			((this)
			 `(js-this-put-name/cache! ,obj
			     ',(string->symbol prop) ,val ,mode ,(pcache cache) %this))
			((object global)
			 `(js-object-put-name/cache! ,obj
			     ',(string->symbol prop) ,val ,mode ,(pcache cache) %this))
			(else
			 `(js-put-name/cache! ,obj
			     ',(string->symbol prop) ,val ,mode ,(pcache cache) %this))))))
	     ((number? prop)
	      `(js-put! ,obj ,prop ,val ,mode %this))
	     (else
	      `(js-put/cache! ,obj ,prop ,val ,mode ,(pcache cache) %this))))
	 (else
	  (cond
	     ((string? prop)
	      `(js-put! ,obj ',(string->symbol prop) ,val ,mode %this))
	     (else
	      `(js-put! ,obj ,prop ,val ,mode %this)))))))

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
	 `(with-access::JsGlobalObject %this (__proto__)
	     (js-object-literal-init!
		(instantiate::JsObject
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
				     (j2s-put! loc tmp (typeof-this val conf)
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

   (define (j2s-new-array args)
      (cond
	 ((null? args)
	  '(js-empty-vector->jsarray %this))
	 ((and (is-integer? (car args)) (null? (cdr args)))
	  `(js-array-construct/length %this (js-array-alloc %this)
	      ,(j2s-scheme (car args) mode return conf hint totype)))
	 ((null? (cdr args))
	  `(js-array-construct %this (js-array-alloc %this)
	      (list ,@(j2s-scheme args mode return conf hint totype))))
	 (else
	  `(js-vector->jsarray
	      (vector ,@(j2s-scheme args mode return conf hint totype))
	      %this))))

   (define (constructor-no-return? decl)
      ;; does this constructor never returns something else than UNDEF?
      (let ((fun (cond
		    ((isa? decl J2SDeclFun)
		     (with-access::J2SDeclFun decl (val) val))
		    ((isa? decl J2SDeclFunCnst)
		     (with-access::J2SDeclFunCnst decl (val) val))
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
			   ,(pcache cache) %this))
		(obj (gensym '%obj)))
	    `(let ((,obj (js-new-fast %this ,fun ,proto)))
		,(if (constructor-no-return? decl)
		     `(begin
			 (,fid ,obj ,@args)
;* 			 (js-new-return-fast ,fun ,obj))               */
			 ,obj)
		     `(js-new-return ,fun (,fid ,obj ,@args) ,obj))))))
   
   (with-access::J2SNew this (loc cache clazz args)
      (cond
	 ((and (=fx (bigloo-debug) 0) (new-array? clazz))
	  (epairify loc
	     (j2s-new-array args)))
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
		       (with-access::JsConstructMap ,cmap (names)
			  (let ((,i (vector-length names))
				(,elements elements))
			     ,@(map (lambda (init offset)
				       `(vector-set! ,elements (+fx ,i ,offset)
					   ,(j2s-scheme (init-expr init)
					       mode return conf hint totype)))
				  
				  
				  nodes (iota (length nodes)))
			     (set! cmap ,cmap1)))
		       ;; cache miss
		       (with-access::JsConstructMap ,cmap (names)
			  (let ((len0 (vector-length names)))
			     ,@(map (lambda (n)
				       (j2s-scheme n
					  mode return conf hint totype))
				  nodes)
			     (with-access::JsConstructMap cmap (names)
				(when (=fx (+fx len0 ,(length nodes))
					 (vector-length names))
				   (set! ,cmap0 ,cmap)
				   (set! ,cmap1 cmap))))))))))))
   
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
;*    notbool-expr? ...                                                */
;*---------------------------------------------------------------------*/
(define (notbool-expr? this::J2SNode)
   (let ((ty (j2s-type this)))
      (and (symbol? ty) (not (eq? ty 'bool) )
	   (not (eq? ty 'obj))
	   (not (eq? ty 'any)))))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SNode ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SNode mode return conf)
   (j2s-scheme this mode return conf '(bool) 'bool))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SExpr mode return conf)
   (with-access::J2SExpr this (type)
      (if (eq? type 'bool)
	  (j2s-scheme this mode return conf '(bool) 'bool)
	  (j2s-test this mode return conf))))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SBinary ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SBinary mode return conf)
   (with-access::J2SBinary this (op lhs rhs loc)
      (case op
	 ((&&)
	  (epairify loc
	     `(and ,(j2s-test lhs mode return conf)
		   ,(j2s-test rhs mode return conf))))
	 ((OR)
	  (epairify loc
	     `(or ,(j2s-test lhs mode return conf)
		  ,(j2s-test rhs mode return conf))))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SUnary ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SUnary mode return conf)
   (with-access::J2SUnary this (op expr loc)
      (case op
	 ((!)
	  (epairify loc `(not ,(j2s-bool-test expr mode return conf))))
	 (else
	  (call-next-method)))))
