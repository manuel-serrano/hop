;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/scheme.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 11:47:51 2013                          */
;*    Last change :  Tue Mar  1 09:09:04 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
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
   
   (export j2s-scheme-stage
	   j2s-scheme-eval-stage
	   (generic j2s-scheme ::obj ::symbol ::procedure ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved-workspaces ...                                    */
;*---------------------------------------------------------------------*/
(define j2s-unresolved-put-workspace '%this)
(define j2s-unresolved-del-workspace '%this)
(define j2s-unresolved-get-workspace '%scope)
(define j2s-unresolved-call-workspace '%this)

;*---------------------------------------------------------------------*/
;*    j2s-scheme-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-scheme-stage
   (instantiate::J2SStageProc
      (name "scheme")
      (comment "Scheme code generation")
      (proc (lambda (ast conf)
	       (j2s-scheme ast 'normal comp-return
		  (append conf `(:debug-client ,(bigloo-debug))) '())))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-eval-stage ...                                        */
;*---------------------------------------------------------------------*/
(define j2s-scheme-eval-stage
   (instantiate::J2SStageProc
      (name "scheme")
      (comment "Scheme code generation (eval)")
      (proc (lambda (ast conf)
	       (j2s-scheme ast 'normal (lambda (x) x)
		  (append conf `(:debug-client ,(bigloo-debug))) '())))))

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
;*    epairify ...                                                     */
;*---------------------------------------------------------------------*/
(define (epairify loc expr)
   (econs (car expr) (cdr expr) loc))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-id ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-id id)
   (cond
      ((char=? (string-ref (symbol->string! id) 0) #\%) id)
      ((memq id '(GLOBAL arguments)) id)
      (else (symbol-append '^ id))))

;*---------------------------------------------------------------------*/
;*    j2s-decl-scheme-id ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-decl-scheme-id decl::J2SDecl)
   (with-access::J2SDecl decl (_scmid id)
      (if _scmid
	  _scmid
	  (let ((sid (j2s-scheme-id id)))
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
;*    j2s-scheme ::obj ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (j2s-scheme this mode return::procedure conf hint)
   (if (pair? this)
       (map (lambda (e) (j2s-scheme e mode return conf hint)) this)
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
(define-method (j2s-scheme this::J2SProgram mode return conf hint)

   (define (exit-body body)
      (if (config-get conf :return-as-exit)
	  `((bind-exit (%jsexit) ,@body))
	  body))

   (define (%cnsts-debug cnsts)
      `(vector
	  ,@(map (lambda (n)
		    (j2s-scheme n mode return conf hint))
	       cnsts)))
   
   (define (%cnsts-intext cnsts)
      
      (define %this
	 '(js-new-global-object))
      
      (define (j2s-constant this::J2SLiteralValue)
	 (cond
;* 	    ((isa? this J2SString)                                     */
;* 	     (with-access::J2SString this (val)                        */
;* 		(js-string->jsstring val)))                            */
;* 	    ((isa? this J2SRegExp)                                     */
;* 	     (with-access::J2SRegExp this (loc val flags)              */
;* 		(with-access::JsGlobalObject %this (js-regexp)         */
;* 		   (let ((rx (js-string->jsstring val)))               */
;* 		      (if flags                                        */
;* 			  (js-new2 %this js-regexp rx                  */
;* 			     (js-string->jsstring flags))              */
;* 			  (js-new1 %this js-regexp rx))))))            */
	    ((isa? this J2SString)
	     (with-access::J2SString this (val)
                (let ((ui (utf8-index val)))
                   (vector 0 val (or ui (string-length val))))))
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
      (with-access::J2SProgram this (mode pcache-size cnsts)
	 (list
	    module
	    (define-pcache pcache-size)
	    `(define %pcache (js-make-pcache ,pcache-size))
	    '(define %source (or (the-loading-file) "/"))
	    '(define %resource (dirname %source))
	    `(define (hopscript %this this %scope %module)
		(define %worker (js-current-worker))
		(define %cnsts ,(%cnsts cnsts))
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
	 (with-access::J2SProgram this (mode pcache-size %this path cnsts)
	    (list
	       module
	       (pcache pcache-size)	       
	       `(define %pcache (js-make-pcache ,pcache-size))
	       `(define %this (nodejs-new-global-object))
	       `(define %source ,path)
	       '(define %resource (dirname %source))
	       
	       `(define (main args)
		   (define %worker
		      (js-init-main-worker! %this #f nodejs-new-global-object))
		   (define %scope (nodejs-new-scope-object %this))
		   (define this
		     (with-access::JsGlobalObject %this (js-object)
			(js-new0 %this js-object)))
		   (define %module (nodejs-module ,(basename path) ,path %worker %this))
		   (define %cnsts ,(%cnsts cnsts))
		   (js-worker-push-thunk! %worker "nodejs-toplevel"
		      (lambda () ,@(exit-body body)))
		   ;; (js-worker-terminate! %worker #f)
		   (thread-join! (thread-start-joinable! %worker)))))))
	 

   (with-access::J2SProgram this (module main nodes headers decls
					 mode name pcache-size cnsts)
      (let ((body (flatten-nodes
		     (append
			(j2s-scheme headers mode return conf hint)
			(j2s-scheme decls mode return conf hint)
			(j2s-scheme nodes mode return conf hint)))))
	 (cond
	    (module
	     ;; a module whose declaration is in the source
	     (j2s-module module body))
	    ((not name)
	     ;; a mere expression
	     `(lambda (%this this %scope %module)
		 (define %pcache (js-make-pcache ,pcache-size))
		 (define %worker (js-current-worker))
		 (define %source (or (the-loading-file) "/"))
		 (define %resource (dirname %source))
		 (define %cnsts ,(%cnsts cnsts))
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
(define-method (j2s-scheme this::J2SVarDecls mode return conf hint)
   (illegal-node "j2s-scheme" this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-decl ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-decl this::J2SDecl value writable mode return)
   (with-access::J2SDecl this (loc scope id)
      (let ((ident (j2s-decl-scheme-id this)))
	 (epairify-deep loc
	    (if (memq scope '(global %scope))
		(let ((fun-name (string->symbol
				   (format "function:~a:~a"
				      (cadr loc) (caddr loc)))))
		   (if (and (not (isa? this J2SDeclExtern)) (in-eval? return))
		       `(js-decl-eval-put! %scope
			   ',id ,value ,(strict-mode? mode) %this)
		       `(begin
			   (define ,ident ,value)
			   (js-bind! %this ,scope ',id
			      :configurable #f
			      :get (js-make-function %this
				      (lambda (%) ,ident)
				      1 ',fun-name)
			      :set ,(when writable
				      `(js-make-function %this
					  (lambda (% %v)
					     (set! ,ident %v))
					  2 ',fun-name))))))
		`(define ,ident ,value))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDecl mode return conf hint)
   
   (define (j2s-scheme-param this)
      (j2s-decl-scheme-id this))
   
   (define (j2s-scheme-var this)
      (with-access::J2SDecl this (loc id writable)
	 (j2s-scheme-decl this '(js-undefined) writable mode return)))
   
   (define (j2s-scheme-let this)
      (with-access::J2SDecl this (loc scope id)
	 (epairify loc
	    (if (memq scope '(global fun))
		`(define ,(j2s-decl-scheme-id this) (js-make-let))
		`(,(j2s-decl-scheme-id this) (js-make-let))))))
   
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
	 (if (memq scope '(global fun))
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
   
   (define (set decl hint)
      (if (and (j2s-let? decl) (not (j2s-let-opt? decl)))
	  `(js-let-set! ,(j2s-decl-scheme-id decl) ,val)
	  `(set! ,(j2s-scheme lhs mode return conf hint) ,val)))
   
   (with-access::J2SRef lhs (decl)
      (cond
	 ((isa? lhs J2SRef)
	  (with-access::J2SDecl decl (writable scope id loc type hint)
	     (if (or writable (isa? decl J2SDeclInit))
		 (cond
		    ((and (memq scope '(global %scope)) (in-eval? return))
		     `(begin
			 ,(j2s-put! loc '%scope `',id val (strict-mode? mode) #f)
			 ,result))
		    (result
		     `(begin
			 ,(set decl (if type (list type) hint))
			 ,result))
		    (else
		     (set decl (if type (list type) hint))))
		 val)))
	 ((not result)
	  (set decl '()))
	 (else
	  `(begin
	      ,(set decl '())
	      ,result)))))
	      
;*---------------------------------------------------------------------*/
;*    j2s-function-src ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-function-src loc val::J2SFun conf)
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
				     (+elong 1 (fixnum->elong end)))))))))))))

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
(define-method (j2s-scheme this::J2SDeclFun mode return conf hint)
   (with-access::J2SDeclFun this (loc id scope val)
      (with-access::J2SFun val (params mode vararg body name generator)
	 (let* ((scmid (j2s-decl-scheme-id this))
		(fastid (j2s-fast-id id))
		(lparams (length params))
		(arity (if vararg -1 (+fx 1 lparams)))
		(minlen (if (eq? mode 'hopscript) (j2s-minlen val) -1))
		(len (if (eq? vararg 'rest) (-fx lparams 1) lparams)))
	    (epairify-deep loc
	       (cond
		  ((eq? scope 'none)
		   `(define ,fastid
		       ,(jsfun->lambda val mode return conf
			   (j2s-declfun-prototype this))))
		  ((memq scope '(global %scope))
		   `(begin
		       (define ,fastid
			  ,(jsfun->lambda val mode return conf
			      (j2s-declfun-prototype this)))
		       (define ,scmid
			  (js-bind! %this ,scope ',id
			     :configurable #f
			     :value (js-make-function %this
				       ,fastid ,len ',id
				       :src ,(j2s-function-src loc val conf)
				       :rest ,(eq? vararg 'rest)
				       :arity ,arity
				       :minlen ,minlen
				       :strict ',mode
				       :alloc (lambda (o)
						 (js-object-alloc o %this))
				       :prototype ,(j2s-fun-prototype val)
				       :__proto__ ,(j2s-fun-__proto__ val)
				       :construct ,fastid)))))
		  (else
		   `(begin
		       (define ,fastid
			  ,(jsfun->lambda val mode return conf
			      (j2s-declfun-prototype this)))
		       (define ,scmid
			  (js-make-function %this
			     ,fastid ,len ',id
			     :src ,(j2s-function-src loc val conf)
			     :rest ,(eq? vararg 'rest)
			     :arity ,arity
			     :minlen ,minlen
			     :strict ',mode
			     :alloc (lambda (o) (js-object-alloc o %this))
			     :prototype ,(j2s-fun-prototype val)
			     :__proto__ ,(j2s-fun-__proto__ val)
			     :construct ,fastid))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclSvc ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclSvc mode return conf hint)
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
(define-method (j2s-scheme this::J2SDeclExtern mode return conf hint)
   (with-access::J2SDeclExtern this (loc id name val bind writable)
      (cond
	 (bind
          (j2s-scheme-decl this (j2s-scheme val mode return conf hint)
	     writable mode return))
	 (else
	  (j2s-scheme val mode return conf hint)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRef mode return conf hint)
   (with-access::J2SRef this (decl loc)
      (with-access::J2SDecl decl (scope id)
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
(define-method (j2s-scheme this::J2SWithRef mode return conf hint)
   (with-access::J2SWithRef this (id withs expr loc)
      (epairify loc
	 (let loop ((withs withs))
	    (if (null? withs)
		(j2s-scheme expr mode return conf hint)
		`(if ,(j2s-in? loc `',id (car withs))
		     ,(j2s-get loc (car withs) 'object `',id 'string #f)
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
   (with-access::J2SThis this (loc)
      'this))

;*---------------------------------------------------------------------*/
;*    j2s-test ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-test test::J2SExpr mode return conf)
   (cond
      ((bool-expr? test)
       (j2s-bool-test test mode return conf))
      ((notbool-expr? test)
       `(js-toboolean ,(j2s-scheme test mode return conf '(bool))))
      (else
       (let ((tmp (gensym)))
	  `(let ((,tmp ,(j2s-scheme test mode return conf '(bool))))
	      (if (boolean? ,tmp) ,tmp (js-totest ,tmp)))))))
   
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
;*    j2s-scheme ::J2SComprehension ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SComprehension mode return conf hint)
   (with-access::J2SComprehension this (loc test expr decls iterables)
      (if (not (strict-mode? mode))
	  (match-case loc
	     ((at ?fname ?loc)
	      `(with-access::JsGlobalObject %this (js-syntax-error)
		  (js-raise
		     (js-new %this js-syntax-error
			,(j2s-jsstring
			   "comprehension only supported in strict mode")
			,fname ,loc))))
	     (else
	      `(with-access::JsGlobalObject %this (js-syntax-error)
		  (js-raise
		     (js-new %this js-syntax-error
			,(j2s-jsstring
			   "comprehension only supported in strict mode"))))))
	  (let* ((names (map j2s-decl-scheme-id decls))
		 (iters (map (lambda (iter)
				(j2s-scheme iter mode return conf hint))
			   iterables))
		 (fun `(lambda (this ,@names)
			  ,(j2s-scheme expr mode return conf hint)))
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
				    ,(j2s-scheme test mode return conf hint))))
		       `(js-array-comprehension %this (list ,@iters)
			   ,fun ,test
			   ',names ,ast-pred ,ast-expr (list ,@ast-decls)))
		    (with-access::J2SBool test (val)
		       (if val
			   `(js-array-comprehension %this (list ,@iters)
			       ,fun #t
			       ',names ,ast-pred ,ast-expr (list ,@ast-decls))
			   `(js-vector->jsarray '#() %this)))))))))

;*---------------------------------------------------------------------*/
;*    pcache ...                                                       */
;*---------------------------------------------------------------------*/
(define (pcache cache)
   `(js-pcache-ref %pcache ,cache))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-unresolved name cache throw)
   (if cache
       `(js-global-object-get-name/cache ,j2s-unresolved-get-workspace ',name
	   ,(pcache cache)
	   ,(if (pair? throw) `',throw throw)
	   %this)
       `(js-global-object-get-name ,j2s-unresolved-get-workspace ',name
	   ,(if (pair? throw) `',throw throw)
	   %this)))

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
      (set! type 'obj)
      (cond
	 ((and (flonum? val) (nanfl? val)) "NaN")
	 ((eq? type 'number) (j2s-num val))
	 (else val))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNumber ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNumber mode return conf hint)
   (with-access::J2SNumber this (val)
      (cond
	 ((elong? val)
	  (elong->flonum val))
	 ((llong? val)
	  (llong->flonum val))
	 ((bignum? val)
	  (bignum->flonum val))
	 ((fixnum? val)
	  (cond-expand
	     (bint30
	      (j2s-num val))
	     (else
	      (if (or (>=fx val (bit-lsh 1 30)) (<fx val (negfx (bit-lsh 1 30))))
		  (fixnum->flonum val)
		  (j2s-num val)))))
	 (else val))))

;*---------------------------------------------------------------------*/
;*    j2s-property-scheme ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-property-scheme this::J2SExpr mode return conf)
   (if (isa? this J2SLiteralCnst)
       (with-access::J2SLiteralCnst this (val)
	  (with-access::J2SLiteralValue val (val)
	     val))
       (j2s-scheme this mode return conf '())))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLiteralCnst ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLiteralCnst mode return conf hint)
   (with-access::J2SLiteralCnst this (index)
      `(vector-ref-ur %cnsts ,index)))

;*---------------------------------------------------------------------*/
;*    utf8-index ...                                                   */
;*---------------------------------------------------------------------*/
(define (utf8-index str)
   (let ((len (string-length str)))
      (let loop ((i 0))
	 (when (<fx i len)
	    (if (>fx (char->integer (string-ref str i)) 127)
		i
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    j2s-jsstring ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring val)
   val)

;*---------------------------------------------------------------------*/
;*    j2s-scheme-string ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-string val loc)
   (j2s-jsstring val))
;*    (let ((ui (utf8-index val)))                                     */
;*       (if (not ui)                                                  */
;* 	  ;; this is an ascii string                                   */
;* 	  (epairify loc                                                */
;* 	     `(js-string->jsstring                                     */
;* 		 (string-ascii-sentinel-set! ,val ,(string-length val)))) */
;* 	  ;; this is an utf8 string                                    */
;* 	  (epairify loc                                                */
;* 	     `(js-string->jsstring                                     */
;* 		 (string-ascii-sentinel-set! ,val ,ui))))))            */

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
				 (let ((ui (utf8-index val)))
				    (if (not ui)
					;; this is an ascii string
					`(string-ascii-sentinel-set!
					    ,val ,(string-length val))
					;; this is an utf8 string
					`(string-ascii-sentinel-set!
					    ,val ,ui))))
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
      (j2s-scheme-string val loc)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRegExp ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRegExp mode return conf hint)
   (with-access::J2SRegExp this (loc val flags)
      (epairify loc
	 `(with-access::JsGlobalObject %this (js-regexp)
	     ,(j2s-new loc 'js-regexp
		 (if (string-null? flags)
		     (list (j2s-scheme-string val loc))
		     (list (j2s-scheme-string val loc)
			(j2s-scheme-string flags loc))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCmap ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCmap mode return conf hint)
   (with-access::J2SCmap this (loc val)
      (epairify loc
	 `(js-names->cmap ',val))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SArray ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArray mode return conf hint)
   (with-access::J2SArray this (loc exprs)
      (let ((sexprs (j2s-scheme exprs mode return conf hint)))
	 (if (every (lambda (x)
			(or (number? x) (string? x) (boolean? x)))
		sexprs)
	     (epairify loc `(js-vector->jsarray ',(list->vector sexprs) %this))
	     (epairify loc `(js-vector->jsarray (vector ,@sexprs) %this))))))

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
			   (lambda (%) ,id) 0 'get))
		   (set (js-make-function %this
			   (lambda (% %v) (set! ,id %v)) 1 'set))
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
		(value (js-make-function %this ,(j2s-fast-id id) 0 ',id))
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
(define (jsfun->lambda this::J2SFun mode return conf proto)

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
	 (let ((args (j2s-scheme params mode return conf '())))
	    (lambda-or-labels idgen idthis id args body))))
   
   (define (rest-lambda fun id body)
      (with-access::J2SFun fun (idgen idthis params)
	 (let ((args (j2s-scheme params mode return conf '())))
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

   (with-access::J2SFun this (loc body need-bind-exit-return vararg mode params generator)
      (let* ((id (j2sfun-id this))
	     (body (cond
		      (generator
		       (with-access::J2SNode body (loc)
			  (epairify loc
			     (generator-body
				(j2s-scheme body mode return conf '())))))
		      (need-bind-exit-return
		       (with-access::J2SNode body (loc)
			  (epairify loc
			     (return-body
				(j2s-scheme body mode return conf '())))))
		      (else
		       (flatten-stmt
			  (j2s-scheme body mode return conf '())))))
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
   (with-access::J2SFun this (loc name params mode vararg mode generator)
      (let* ((id (j2sfun-id this))
	     (lparams (length params))
	     (len (if (eq? vararg 'rest) (-fx lparams 1) lparams))
	     (arity (if vararg -1 (+fx 1 (length params))))
	     (minlen (if (eq? mode 'hopscript) (j2s-minlen this) -1)))
	 (epairify-deep loc
	    `(js-make-function %this
		,tmp ,len ',(or name (j2s-decl-scheme-id id))
		:src ,(j2s-function-src loc this conf)
		:rest ,(eq? vararg 'rest)
		:arity ,arity
		:prototype ,(j2s-fun-prototype this)
		:__proto__ ,(j2s-fun-__proto__ this)
		:strict ',mode
		:minlen ,minlen
		:alloc (lambda (o) (js-object-alloc o %this))
		:construct ,tmp)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SFun mode return conf hint)
   (with-access::J2SFun this (loc name params mode vararg generator)
      (let* ((id (j2sfun-id this))
	     (tmp (gensym id))
	     (arity (if vararg -1 (+fx 1 (length params))))
	     (fundef (if generator
			 (let ((tmp2 (gensym id)))
			    `(letrec* ((,tmp ,(jsfun->lambda this mode return conf
						 `(js-get ,tmp2 'prototype %this)))
				       (,tmp2 ,(j2sfun->scheme this tmp mode return conf)))
				,tmp2))
			 `(let ((,tmp ,(jsfun->lambda this mode return conf
					  (j2s-fun-prototype this))))
			     ,(j2sfun->scheme this tmp mode return conf)))))
	 (epairify-deep loc
	    (if id
		(let ((scmid (j2s-scheme-id id)))
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
			 (j2s-scheme body mode return conf '()))))
		(flatten-stmt
		   (j2s-scheme body mode return conf '())))))

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
			       (format "wrong service call \"~s\"" name))
			    ,fname ,loc))
		       (else
			`(js-new %this js-type-error
			    ,(j2s-jsstring
			       (format "wrong service call \"~s\"" name)))))))))
      
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
	       (j2s-scheme val mode return conf '())))))
   
   (define (svc-proc-entry this)
      (with-access::J2SSvc this (name params loc path mode)
	 (let ((params (j2s-scheme params mode return conf '()))
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
	 (let ((args (j2s-scheme params mode return conf '()))
	       (lam (jsfun->lambda this mode return conf
		       (j2s-fun-prototype this))))
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
(define-method (j2s-scheme this::J2SSvc mode return conf hint)
   (with-access::J2SSvc this (loc)
      (epairify loc
	 (jssvc->scheme this #f #f mode return conf))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SReturn mode return conf hint)
   (with-access::J2SReturn this (loc expr tail exit)
      (cond
	 (exit
	  (epairify loc
	     `(%jsexit ,(j2s-scheme expr mode return conf hint))))
	 (tail
	  (j2s-scheme expr mode return conf hint))
	 (else
	  (epairify loc
	     `(%return ,(j2s-scheme expr mode return conf hint)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SThrow ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SThrow mode return conf hint)
   (with-access::J2SThrow this (loc expr)
      (epairify loc
	 `(js-throw ,(j2s-scheme expr mode return conf hint)
	     ,(j2s-jsstring (cadr loc)) ,(caddr loc)))))
   
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
      (if (pair? (cdr exprs))
	  (epairify loc `(begin ,@(j2s-scheme exprs mode return conf hint)))
	  (j2s-scheme (car exprs) mode return conf hint))))

;*---------------------------------------------------------------------*/
;*    j2s-let-decl-toplevel ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-let-decl-toplevel::pair-nil d::J2SDeclInit mode return conf)
   (with-access::J2SDeclInit d (val usage id type hint)
      (let ((ident (j2s-decl-scheme-id d))
	    (hint (if type (list type) hint)))
	 (cond
	    ((or (not (isa? val J2SFun)) (memq 'assig usage))
	     `(define ,ident ,(j2s-scheme val mode return conf hint)))
	    ((or (memq 'ref usage) (memq 'new usage))
	     (let ((fun (jsfun->lambda val mode return conf
			   `(js-get ,ident 'prototype %this)))
		   (tmp (j2s-fast-id id)))
		`(begin
		    (define ,tmp ,fun)
		    (define ,ident ,(j2sfun->scheme val tmp mode return conf)))))
	    ((memq 'call usage)
	     `(define ,(j2s-fast-id id)
		 ,(jsfun->lambda val mode return conf
		     `(js-get ,(j2s-fast-id id) 'prototype %this))))
	    (else
	     '())))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLetBlock ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLetBlock mode return conf hint)
   
   (define (j2s-let-decl-inner::pair-nil d::J2SDecl mode return conf)
      (with-access::J2SDeclInit d (val usage id)
	 (let ((ident (j2s-decl-scheme-id d)))
	    (cond
	       ((or (not (isa? val J2SFun)) (memq 'assig usage))
		(list `(,ident ,(j2s-scheme val mode return conf hint))))
	       ((or (memq 'ref usage) (memq 'new usage))
		(let ((fun (jsfun->lambda val mode return conf
			      `(js-get ,ident 'prototype %this)))
		      (tmp (j2s-fast-id id)))
		   `((,tmp ,fun)
		     (,ident ,(j2sfun->scheme val tmp mode return conf)))))
	       ((memq 'call usage)
		`((,(j2s-fast-id id)
		   ,(jsfun->lambda val mode return conf (j2s-fun-prototype val)))))
	       (else
		'())))))
   
   (with-access::J2SLetBlock this (loc decls nodes)
      (if (any (lambda (decl::J2SDecl)
		  (with-access::J2SDecl decl (scope)
		     (memq scope '(global fun))))
	     decls)
	  ;; top-level or function level block
	  (epairify loc
	     `(begin
		 ,@(map (lambda (d)
			   (if (j2s-let-opt? d)
			       (j2s-let-decl-toplevel d mode return conf)
			       (j2s-scheme d mode return conf hint)))
		      decls)
		 ,@(j2s-scheme nodes mode return conf hint)))
	  ;; inner letblock, create a let block
	  (let ((opt (if (any (lambda (d) (j2s-let-opt? d)) decls)
			 'letrec* 'let))
		(body (j2s-scheme nodes mode return conf hint)))
	     (epairify loc
		`(,opt ,(append-map (lambda (d)
				       (if (j2s-let-opt? d)
					   (j2s-let-decl-inner d mode return conf)
					   (list (j2s-scheme d mode return conf hint))))
			   decls)
		    ,@(if (pair? body) body '(#unspecified))))))))

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
;*---------------------------------------------------------------------*/
(define (j2s-num-op op left right)
   
   (define (fx op)
      (case op
	 ((+ - /)
	  (symbol-append 'js op 'fx))
	 (else
	  (symbol-append op 'fx))))
   
   (let ((l (gensym 'left))
	 (r (gensym 'right)))
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
	  `(if (and (fixnum? ,left) (fixnum? ,right))
	       (,(fx op) ,left ,right)
	       (,op ,left ,right))))))

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
;*    js-binop2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-binop2 loc op lhs rhs mode return conf hint)
   
   (define (best-hint hints)
      (when (pair? hints)
	 (let loop ((hint (car hints))
		    (hints (cdr hints)))
	    (cond
	       ((null? hints) (car hint))
	       ((>fx (cdar hints) (cdr hint)) (loop (car hint) (cdr hints)))
	       (else (loop hint (cdr hints)))))))
   
   (define (is-number? expr)
      (memq (j2s-type expr) '(number integer)))
   
   (define (maybe-number? expr)
      (let ((ty (j2s-type expr)))
	 (or (not ty) (memq ty '(number integer obj)))))
   
   (define (atom? expr)
      (or (number? expr) (string? expr) (boolean? expr)))
   
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
	 (else
	  #f)))
   
   (define (binop lhs rhs hint::pair-nil gen::procedure)
      (let* ((scmlhs (j2s-scheme lhs mode return conf hint))
	     (scmrhs (j2s-scheme rhs mode return conf hint))
	     (testl (or (atom? scmlhs) (and (symbol? scmlhs) (simple? rhs))))
	     (testr (or (atom? scmrhs) (and (symbol? scmrhs) (simple? lhs)))))
	 (cond
	    ((and testl testr)
	     (gen scmlhs scmrhs))
	    (testl
	     (let ((right (gensym 'right)))
		`(let ((,right ,scmrhs))
		    ,(gen scmlhs right))))
	    (testr
	     (let ((left (gensym 'left)))
		`(let ((,left ,scmlhs))
		    ,(gen left scmrhs))))
	    (else
	     (let ((left (gensym 'left))
		   (right (gensym 'right)))
		`(let ((,left ,scmlhs)
		       (,right ,scmrhs))
		    ,(gen left right)))))))
   
   (case op
      ((+)
       (cond
	  ((and (is-number? lhs) (is-number? rhs))
	   (binop lhs rhs hint
	      (lambda (left right)
		 (j2s-num-op '+ left right))))
	  ((and (eq? (j2s-type lhs) 'string) (eq? (j2s-type rhs) 'string))
	   (let ((scmlhs (j2s-scheme lhs mode return conf hint))
		 (scmrhs (j2s-scheme rhs mode return conf hint)))
	      `(js-jsstring-append ,scmlhs ,scmrhs)))
	  (else
	   (let ((scmlhs (j2s-scheme lhs mode return conf hint))
		 (scmrhs (j2s-scheme rhs mode return conf hint)))
	      (case hint
		 ((number)
		  (binop lhs rhs hint
		     (lambda (left right)
			`(if (and (fixnum? ,left) (fixnum? ,right))
			     (js+fx ,left ,right)
			     (js+ ,left ,right %this)))))
		 ((string)
		  (let ((left (gensym 'left))
			(right (gensym 'right)))
		     `(let ((,left ,scmlhs)
			    (,right ,scmrhs))
			 (if (and (isa? ,left JsStringLiteral)
				  (isa? ,right JsStringLiteral))
			     (js-jsstring-append ,left ,right)
			     (js+ ,left ,right %this)))))
		 (else
		  (cond
		     ((and (symbol? scmlhs) (symbol? scmrhs))
		      (if (and (maybe-number? lhs) (maybe-number? rhs))
			  `(if (and (fixnum? ,scmlhs) (fixnum? ,scmrhs))
			       (js+fx ,scmlhs ,scmrhs)
			       (js+ ,scmlhs ,scmrhs %this))
			  `(js+ ,scmlhs ,scmrhs %this)))
		     ((symbol? scmlhs)
		      (let ((left scmlhs)
			    (right (gensym 'right)))
			 `(let ((,right ,scmrhs))
			     ,(if (and (maybe-number? lhs) (maybe-number? rhs))
				  `(if (and (fixnum? ,left) (fixnum? ,right))
				       (js+fx ,left ,right)
				       (js+ ,left ,right %this))
				  `(js+ ,left ,right %this)))))
		     ((symbol? scmrhs)
		      (let ((left (gensym 'left))
			    (right scmrhs))
			 `(let ((,left ,scmlhs))
			     ,(if (and (maybe-number? lhs) (maybe-number? rhs))
				  `(if (and (fixnum? ,left) (fixnum? ,right))
				       (js+fx ,left ,right)
				       (js+ ,left ,right %this))
				  `(js+ ,left ,right %this)))))
		     (else
		      (let ((left (gensym 'left))
			    (right (gensym 'right)))
			 `(let ((,left ,scmlhs)
				(,right ,scmrhs))
			     ,(if (and (maybe-number? lhs) (maybe-number? rhs))
				  `(if (and (fixnum? ,left) (fixnum? ,right))
				       (js+fx ,left ,right)
				       (js+ ,left ,right %this))
				  `(js+ ,left ,right %this))))))))))))
      ((%)
       (cond
	  ((and (is-number? lhs) (is-number? rhs))
	   (binop lhs rhs hint
	      (lambda (left right)
		 (if (and (number? right) (not (= right 0)))
		     `(js-%$$NZ ,left ,right)
		     `(js-%$$NN ,left ,right)))))
	  (else
	   (binop lhs rhs hint
	      (lambda (left right)
		 (js-binop loc op left right))))))
      ((== === != !==)
       (cond
	  ((and (is-number? lhs) (is-number? rhs))
	   (let ((res (binop lhs rhs hint
			 (lambda (left right)
			    (j2s-num-op '= left right)))))
	      (if (memq op '(!= !==))
		  `(not ,res)
		  res)))
	  ((and (eq? (j2s-type lhs) 'string) (eq? (j2s-type rhs) 'string))
	   (binop lhs rhs '(string)
	      (lambda (left right)
		 (js-binop loc op left right))))
	  ((and (eq? (j2s-type lhs) 'bool) (eq? (j2s-type rhs) 'bool))
	   (binop lhs rhs '(bool)
	      (lambda (left right)
		 `(eq? ,left ,right))))
	  (else
	   (binop lhs rhs hint
	      (lambda (left right)
		 (js-binop loc op left right))))))
      ((- * < <= > >=)
       (cond
	  ((and (is-number? lhs) (is-number? rhs))
	   (binop lhs rhs hint
	      (lambda (left right)
		 (j2s-num-op op left right))))
	  (else
	   (binop lhs rhs '(number)
	      (lambda (left right)
		 (js-binop loc op left right))))))
      ((/)
       (binop lhs rhs hint
	  (lambda (left right)
	     (js-binop loc op left right))))
      ((OR)
       (let ((lhsv (gensym 'lhs)))
	  `(let ((,lhsv ,(j2s-scheme lhs mode return conf hint)))
	      (if (js-totest ,lhsv)
		  ,lhsv
		  ,(j2s-scheme rhs mode return conf hint)))))
      ((&&)
       (let ((lhsv (gensym 'lhs)))
	  `(let ((,lhsv ,(j2s-scheme lhs mode return conf hint)))
	      (if (js-totest ,lhsv)
		  ,(j2s-scheme rhs mode return conf hint)
		  ,lhsv))))
      (else
       (binop lhs rhs hint
	  (lambda (left right)
	     (js-binop loc op left right))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBinary mode return conf hint)
   (with-access::J2SBinary this (loc op lhs rhs)
      (epairify-deep loc
	 (js-binop2 loc op lhs rhs mode return conf hint))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SParen ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SParen mode return conf hint)
   (with-access::J2SParen this (expr)
      (j2s-scheme expr mode return conf hint)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUnary ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUnary mode return conf hint)

   (define (err id)
      (with-access::J2SUnary this (loc)
	 (match-case loc
	    ((at ?fname ?loc)
	     `(with-access::JsGlobalObject %this (js-syntax-error)
		 (js-raise
		    (js-new %this js-syntax-error
		       ,(j2s-jsstring
			  (format "Delete of an unqualified identifier in strict mode: \"~a\"" id))
		       ,fname ,loc))))
	    (else
	     `(with-access::JsGlobalObject %this (js-syntax-error)
		 (js-raise
		    (js-new %this js-syntax-error
		       ,(j2s-jsstring
			  (format "Delete of an unqualified identifier in strict mode: \"~a\"" id)))))))))

   (define (delete->scheme expr)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.7
      (cond
	 ((isa? expr J2SWithRef)
	  (with-access::J2SWithRef expr (id withs expr loc)
	     (let loop ((withs withs))
		(if (null? withs)
		    `(begin ,(j2s-scheme expr mode return conf hint) #f)
		    `(if ,(j2s-in? loc `',id (car withs))
			 (js-delete! ,(j2s-scheme (car withs) mode return conf hint)
			    ',(j2s-scheme id mode return conf hint)
			    #f
			    %this)
			 ,(loop (cdr withs)))))))
	 ((isa? expr J2SAccess)
	  (with-access::J2SAccess expr (obj field)
	     `(js-delete! ,(j2s-scheme obj mode return conf hint)
		 ,(j2s-scheme field mode return conf hint)
		 ,(strict-mode? mode)
		 %this)))
	 ((isa? expr J2SUnresolvedRef)
	  (if (strict-mode? mode)
	      (with-access::J2SUnresolvedRef expr (id)
		 (err id))
	      (with-access::J2SUnresolvedRef expr (id)
		 `(js-delete! ,j2s-unresolved-del-workspace ',id #f %this))))
	 ((isa? expr J2SRef)
	  (if (strict-mode? mode)
	      (with-access::J2SRef expr (decl)
		 (with-access::J2SDecl decl (id)
		    (err id)))
	      '(begin #f)))
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (delete->scheme expr)))
	 (else
	  `(begin ,(j2s-scheme expr mode return conf hint) #t))))

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
	  `(js-typeof ,(j2s-scheme expr mode return conf hint)))))
   
   (with-access::J2SUnary this (loc expr op)
      (epairify loc
	 (case op
	    ((!)
	     `(if ,(j2s-test expr mode return conf) #f #t))
	    ((typeof)
	     (typeof->scheme expr))
	    ((void)
	     `(begin ,(j2s-scheme expr mode return conf hint) (js-undefined)))
	    ((delete)
	     (delete->scheme expr))
	    ((+)
	     ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.6
	     (let ((expr (j2s-scheme expr mode return conf hint)))
		(if (eqv? expr 0)
		    `(begin +0.0)
		    `(js-tonumber ,expr %this))))
	    ((-)
	     ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.7
	     (let ((expr (j2s-scheme expr mode return conf hint)))
		(cond
		   ((eqv? expr 0)
		    `(begin -0.0))
		   ((number? expr)
		    `(begin ,(- expr)))
		   (else
		    `(js-neg ,expr %this)))))
	    ((~)
	     ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.8
	     `(js-bitnot ,(j2s-scheme expr mode return conf hint) %this))
	    (else
	     `(,op ,(j2s-scheme expr mode return conf hint)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPostfix ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.3.1       */
;*    -------------------------------------------------------------    */
;*    !!! x++ not equivalent to x = x + 1 as x++ always converts       */
;*    to number.                                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPostfix mode return conf hint)
   (with-access::J2SPostfix this (loc lhs op)
      (let ((inc (j2s-num (if (eq? op '++) 1 -1))))
	 (let loop ((lhs lhs))
	    (cond
	       ((isa? lhs J2SRef)
		(let ((tmp (gensym 'tmp)))
		   (epairify-deep loc
		      (if (eq? (j2s-type lhs) 'number)
			  `(let ((,tmp ,(j2s-scheme lhs mode return conf hint)))
			      ,(j2s-scheme-set! lhs
				  (epairify loc (j2s-num-op '+ inc tmp))
				  tmp
				  mode return conf)
			      ,tmp)
			  `(let ((,tmp (js-tonumber ,(j2s-scheme lhs mode return conf hint) %this)))
			      ,(j2s-scheme-set! lhs
				  (epairify loc `(js+ ,inc ,tmp %this))
				  tmp
				  mode return conf))))))
	       ((isa? lhs J2SAccess)
		(with-access::J2SAccess lhs ((o obj) field cache (loca loc))
		   (let ((tmp (gensym 'tmp))
			 (obj (gensym 'obj))
			 (pro (gensym 'prop))
			 (prov (j2s-property-scheme field mode return conf)))
		      (epairify-deep loc
			 `(let* ((,obj ,(j2s-scheme o mode return conf hint))
				 ,@(if (string? prov) '() (list `(,pro ,prov)))
				 (,tmp (js-tonumber
					  ,(j2s-get loca obj (j2s-type obj)
					      (if (string? prov) prov pro)
					      (j2s-type field)
					      cache)
					  %this)))
			     ,(j2s-put! loca obj (if (string? prov) prov pro)
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
(define-method (j2s-scheme this::J2SPrefix mode return conf hint)
   (with-access::J2SPrefix this (loc lhs op)
      (let ((inc (j2s-num (if (eq? op '++) 1 -1))))
	 (let loop ((lhs lhs))
	    (cond
	       ((isa? lhs J2SRef)
		(epairify loc
		   (j2s-scheme-set! lhs
		      (epairify loc
			 `(js+ ,inc
			     (js-tonumber
				,(j2s-scheme lhs mode return conf '(number))
				%this)  %this))
		      (j2s-scheme lhs mode return conf '(number))
		      mode return conf)))
	       ((isa? lhs J2SAccess)
		(with-access::J2SAccess lhs ((o obj) field cache (loca loc))
		   (let ((tmp (gensym 'tmp))
			 (obj (gensym 'obj))
			 (pro (gensym 'prop))
			 (res (gensym 'res))
			 (prov (j2s-scheme field mode return conf hint)))
		      (epairify-deep loc
			 `(let* ((,obj ,(j2s-scheme o mode return conf hint))
				 ,@(if (string? prov) '() (list `(,pro ,prov)))
				 (,tmp (js-tonumber
					  ,(j2s-get loca obj (j2s-type obj)
					      (if (string? prov) prov pro)
					      (j2s-type field)
					      cache)
					  %this))
				 (,res (js+ ,inc ,tmp %this)))
			     ,(j2s-put! loca obj
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
;*    j2s-scheme ::J2SStmt ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12           */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SStmt mode return conf hint)
   (return this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSeq ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.1         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSeq mode return conf hint)
   (with-access::J2SSeq this (loc nodes)
      (let ((nodes nodes))
	 (cond
	    ((null? nodes)
	     (epairify loc
		(return '(js-undefined))))
	    ((pair? (cdr nodes))
	     (epairify loc
		`(begin ,@(j2s-scheme nodes mode return conf hint))))
	    (else
	     (j2s-scheme (car nodes) mode return conf hint))))))

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
;*    j2s-scheme ::J2SExprStmt ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SExprStmt mode return conf hint)
   (with-access::J2SExprStmt this (stmt)
      (j2s-scheme stmt mode return conf hint)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SIf ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.5         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SIf mode return conf hint)
   (with-access::J2SIf this (loc test then else)
      (let ((tmp (gensym)))
	 (epairify loc
	    (if (isa? else J2SNop)
		`(if ,(j2s-test test mode return conf)
		     ,(j2s-scheme then mode return conf hint)
		     (js-undefined))
		`(if ,(j2s-test test mode return conf)
		     ,(j2s-scheme then mode return conf hint)
		     ,(j2s-scheme else mode return conf hint)))))))

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
			   ,(j2s-scheme body mode return conf hint)))
		    (j2s-scheme body mode return conf hint))
	       (if ,(j2s-test test mode return conf)
		   (,loop)
		   '(js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       ,(if need-bind-exit-continue
		    (epairify-deep loc
		       `(bind-exit (,(escape-name '%continue id))
			   ,(j2s-scheme body mode acc-return conf hint)))
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
				  ,(j2s-scheme body mode return conf hint))
			       (,loop)))
			(epairify-deep loc
			   `(begin
			       ,(j2s-scheme body mode return conf hint)
			       (,loop))))
		   '(js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       (if ,(j2s-test test mode return conf)
		   ,(if need-bind-exit-continue
			(epairify-deep loc
			   `(begin
			       (bind-exit (,(escape-name '%continue id))
				  ,(j2s-scheme body mode acc-return conf hint))
			       (,loop %acc)))
			(epairify-deep loc
			   `(begin
			       ,(j2s-scheme body mode acc-return conf hint)
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
		      ,(j2s-scheme incr mode return conf hint)
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
		      ,(j2s-scheme incr mode return conf hint)
		      (,loop %acc))
		   %acc)))

      (let* ((forid (gensym 'for))
	     (loop (if (in-eval? return) (eval-loop forid) (comp-loop forid))))
	 (epairify-deep loc
	    `(begin
		,(j2s-scheme init mode return conf hint)
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

   (define (for-in/break-comp tmp name props obj body set)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(let ((%acc (js-undefined)))
			(js-for-in ,(j2s-scheme obj mode return conf hint)
			   (lambda (,name)
			      ,set
			      ,(if need-bind-exit-continue
				   `(bind-exit (,(escape-name '%continue id))
				       ,(j2s-scheme body mode acc-return conf hint))
				   (j2s-scheme body mode acc-return conf hint)))
			   %this)
			%acc)))
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,for)
		for))))

   (define (for-in/break-eval tmp name props obj body set)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(js-for-in ,(j2s-scheme obj mode return conf hint)
			(lambda (,name)
			   ,set
			   ,(if need-bind-exit-continue
				`(bind-exit (,(escape-name '%continue id))
				    ,(j2s-scheme body mode return conf hint))
				(j2s-scheme body mode return conf hint)))
			%this)))
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,for)
		for))))

   (define (for-in/break tmp name props obj body set)
      (if (in-eval? return)
	  (for-in/break-eval tmp name props obj body set)
	  (for-in/break-comp tmp name props obj body set)))

   (define (for-in/w-break-comp tmp name props obj body set)
      `(js-for-in ,(j2s-scheme obj mode return conf hint)
	  (lambda (,name)
	     ,set
	     ,(j2s-scheme body mode return conf hint))
	  %this))

   (define (for-in/w-break-eval tmp name props obj body set)
      `(let ((%acc (js-undefined)))
	  (js-for-in ,(j2s-scheme obj mode return conf hint)
	     (lambda (,name)
		,set
		,(j2s-scheme body mode acc-return conf hint))
	     %this)
	  %acc))

   (define (for-in/w-break tmp name props obj body set)
      (if (in-eval? return)
	  (for-in/w-break-eval tmp name props obj body set)
	  (for-in/w-break-comp tmp name props obj body set)))

   (define (set lhs name loc)
      (let loop ((lhs lhs))
	 (cond
	    ((isa? lhs J2SRef)
	     (epairify loc (j2s-scheme-set! lhs name #f mode return conf)))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(epairify loc
		   (j2s-unresolved-put! `',id name #f mode return))))
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field loc)
		(epairify loc
		   (j2s-put! loc (j2s-scheme obj mode return conf hint)
		       (j2s-scheme field mode return conf hint)
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
			       ,(j2s-put! loc (car withs) (symbol->string id)
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
(define-method (j2s-scheme this::J2SLabel mode return conf hint)
   (with-access::J2SLabel this (body)
      (j2s-scheme body mode return conf hint)))

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
      
      (define (comp-switch)
	 (let ((elseclause #f)
	       (elsefun #f)
	       (tmp (gensym 'tmp))
	       (funs (map (lambda (c) (gensym)) cases)))
	    `(let* ((,tmp ,(j2s-scheme key mode return conf hint))
		    ,@(map (lambda (case fun)
			      (with-access::J2SCase case (loc body)
				 (epairify loc
				    `(,fun
					(lambda ()
					   ,(j2s-scheme body mode return conf hint))))))
			 cases funs))
		(cond
		   ,@(filter-map (lambda (case::J2SCase fun)
				    (with-access::J2SCase case (loc expr body)
				       (cond
					  ((isa? case J2SDefault)
					   (set! elseclause expr)
					   (set! elsefun fun)
					   #f)
					  (else
					   (epairify loc
					      `((js-strict-equal? ,tmp ,(j2s-scheme expr mode return conf hint))
						,@(map (lambda (f) `(,f))
						     (memq fun funs))))))))
		      cases funs)
		   ,(epairify loc
		     `(else
		       ,@(if elseclause
			     (map (lambda (f) `(,f)) (memq elsefun funs))
			     '((js-undefined)))))))))

      (define (eval-switch)
	 (let ((elseclause #f)
	       (elsefun #f)
	       (tmp (gensym 'tmp))
	       (funs (map (lambda (c) (gensym)) cases)))
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
					   (set! elseclause expr)
					   (set! elsefun fun)
					   #f)
					  (else
					   (epairify loc
					      `((js-strict-equal? ,tmp ,(j2s-scheme expr mode return conf hint))
						,@(map (lambda (f) `(,f))
						     (memq fun funs))))))))
		      cases funs)
		   ,(epairify loc
		     `(else
		       ,@(if elseclause
			     (map (lambda (f) `(,f)) (memq elsefun funs))
			     '((js-undefined)))
		       %acc))))))
      
      (let ((switch (if (in-eval? return) (eval-switch) (comp-switch))))
	 (epairify-deep loc
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,switch)
		switch)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCall mode return conf hint)
   
   (define (read-only-function ref::J2SRef)
      (with-access::J2SRef ref (decl)
	 (cond
	    ((isa? decl J2SDeclFun)
	     (with-access::J2SDecl decl (writable)
		(unless writable
		   decl)))
	    ((isa? decl J2SDeclFunCnst)
	     decl)
	    ((j2s-let-opt? decl)
	     (with-access::J2SDeclInit decl (usage id val)
		(when (isa? val J2SFun)
		   (unless (memq 'assig usage) decl))))
	    (else
	     #f))))
   
   (define (call-method fun::J2SAccess args)
      (with-access::J2SAccess fun (loc obj field)
	 (if (isa? obj J2SRef)
	     (call-unknown-function fun
		(j2s-toobject loc (j2s-scheme obj mode return conf hint))
		args)
	     (let ((tmp (gensym)))
		`(let ((,tmp ,(j2s-scheme obj mode return conf hint)))
		    ,(call-unknown-function
			(duplicate::J2SAccess fun
			   (obj (instantiate::J2SPragma
				   (loc loc)
				   (expr tmp))))
			(j2s-toobject loc tmp) args))))))
   
   (define (call-hop-function fun::J2SHopRef args)
      `(,(j2s-scheme fun mode return conf hint) ,@(j2s-scheme args mode return conf hint)))

   (define (j2s-this this)
      (cond
	 ((eq? this #unspecified) '((js-undefined)))
	 (this (list (j2s-scheme this mode return conf hint)))
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

   (define (call-fix-function fun::J2SFun this f %gen args)
      ;; call a function that accepts a fix number of arguments
      (with-access::J2SFun fun (params vararg)
	 (let ((lenf (length params))
	       (lena (length args)))
	    (cond
	       ((=fx lenf lena)
		;; matching arity
		`(,f ,@%gen ,@(j2s-this this)
		    ,@(j2s-scheme args mode return conf hint)))
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
		       (,f ,@%gen ,@(j2s-this this) ,@(take temps lenf)))))
	       (else
		;; argument missing
		`(,f ,@(j2s-this this)
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
			 (j2s-error id "wrong number of arguments"
			    this (cons la (j2s-minlen val)))))
		     ((arguments)
		      #t)
		     (else
		      (unless (and (>=fx la (j2s-minlen val)) (<=fx la lp))
			 (j2s-error id "wrong number of arguments"
			    this (cons la (j2s-minlen val)))))))))))

   (define (call-fun-function fun::J2SFun this f %gen::pair-nil args::pair-nil)
      (with-access::J2SFun fun (params vararg)
	 (case vararg
	    ((arguments)
	     `(,f ,@%gen ,@(j2s-this this)
		 ,@(j2s-scheme args mode return conf hint)))
	    ((rest)
	     (call-rest-function fun this f %gen args))
	    (else
	     (call-fix-function fun this f %gen args)))))

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
	 `(,expr %this ,@(j2s-scheme args mode return conf hint))))

   (define (typed-generator? decl::J2SDeclFun)
      (with-access::J2SDeclFun decl (parent)
	 (when (isa? parent J2SDeclFun)
	    (with-access::J2SDeclFun parent (val)
	       (with-access::J2SFun val (generator)
		  generator)))))

   (define (call-known-function fun::J2SDecl this args)
      (cond
	 ((isa? fun J2SDeclFun)
	  (with-access::J2SDeclFun fun (id val)
	     (check-hopscript-fun-arity val id args)
	     (let ((%gen (if (typed-generator? fun) '(%gen) '())))
		(call-fun-function val this (j2s-fast-id id) %gen args))))
	 ((isa? fun J2SDeclFunCnst)
	  (with-access::J2SDeclFunCnst fun (id val)
	     (check-hopscript-fun-arity val id args)
	     (call-fun-function val this (j2s-fast-id id) '() args)))
	 ((j2s-let-opt? fun)
	  (with-access::J2SDeclInit fun (id val)
	     (call-fun-function val this (j2s-fast-id id) '() args)))
	 (else
	  (error "js-scheme" "Should not be here" (j2s->list fun)))))

   (define (call-unknown-function fun thisarg args)
      (let* ((len (length args))
	     (call (if (>=fx len 9)
		       'js-calln
		       (string->symbol (format "js-call~a" (length args))))))
	 (if (> (bigloo-debug) 0)
	     (with-access::J2SCall this (loc)
		`(,(symbol-append call '/debug)
		  ,j2s-unresolved-call-workspace
		  ',loc
		  ,(j2s-scheme fun mode return conf hint) ,thisarg
		  ,@(j2s-scheme args mode return conf hint)))
	     `(,call ,j2s-unresolved-call-workspace
		 ,(j2s-scheme fun mode return conf hint) ,thisarg
		 ,@(j2s-scheme args mode return conf hint)))))

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
   
   (with-access::J2SCall this (loc fun this args)
      (let loop ((fun fun))
	 (epairify loc
	    (cond
	       ((isa? fun J2SAccess)
		(call-method fun args))
	       ((isa? fun J2SParen)
		(with-access::J2SParen fun (expr)
		   (loop expr)))
	       ((isa? fun J2SHopRef)
		(call-hop-function fun args))
	       ((and (isa? fun J2SFun) (not (j2sfun-id fun)))
		(call-fun-function fun this
		   (jsfun->lambda fun mode return conf (j2s-fun-prototype fun))
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
		(lambda (fun) (call-known-function fun this args)))
	       (else
		(call-unknown-function fun '(js-undefined) args)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssig mode return conf hint)
   (with-access::J2SAssig this (loc lhs rhs)
      (let loop ((lhs lhs))
	 (cond
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field cache (loca loc))
		(cond
		   ((and (eq? (j2s-type obj) 'array)
			 (memq (j2s-type field) '(integer number)))
		    (epairify-deep loc
		       `(js-array-set! ,(j2s-scheme obj mode return conf hint)
			   ,(j2s-scheme field mode return conf hint)
			   ,(j2s-scheme rhs mode return conf hint)
			   %this)))
		   (else
		    (epairify loc
		       (j2s-put! loca (j2s-scheme obj mode return conf hint)
			  (j2s-scheme field mode return conf hint)
			  (j2s-scheme rhs mode return conf hint)
			  (strict-mode? mode)
			  cache))))))
	    ((isa? lhs J2SRef)
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (type hint)
		   (let ((hint (if type (list type) hint)))
		      (let ((assig (j2s-scheme-set! lhs
				      (j2s-scheme rhs mode return conf hint)
				      (j2s-scheme lhs mode return conf hint)
				      mode return conf)))
			 (if (pair? assig)
			     (epairify loc assig)
			     assig))))))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(epairify loc
		   (j2s-unresolved-put! `',id
		      (j2s-scheme rhs mode return conf hint) #f mode return))))
	    ((isa? lhs J2SWithRef)
	     (with-access::J2SWithRef lhs (id withs expr loc)
		(epairify loc
		   (let liip ((withs withs))
		      (if (null? withs)
			  (loop expr)
			  `(if ,(j2s-in? loc `',id (car withs))
			       ,(j2s-put! loc (car withs) (symbol->string id)
				   (j2s-scheme rhs mode return conf hint) #f #f)
			       ,(liip (cdr withs))))))))
	    ((isa? lhs J2SUndefined)
	     (j2s-scheme rhs mode return conf hint))
	    ((isa? lhs J2SParen)
	     (with-access::J2SParen lhs (expr)
		(loop expr)))
	    (else
	     (j2s-error "assignment" "Illegal assignment" this))))))


;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAccess mode return conf hint)
   (with-access::J2SAccess this (loc obj field cache)
      (cond
	 ((and (eq? (j2s-type obj) 'array) (j2s-field-length? field))
	  (epairify-deep loc
	     `(js-array-length ,(j2s-scheme obj mode return conf hint))))
	 ((and (eq? (j2s-type obj) 'array) (memq (j2s-type field) '(integer number)))
	  (epairify-deep loc
	     `(js-array-ref ,(j2s-scheme obj mode return conf hint)
		 ,(j2s-scheme field mode return conf hint)
		 %this)))
	 ((and (eq? (j2s-type obj) 'string) (j2s-field-length? field))
	  (epairify-deep loc
	     `(utf8-codeunit-length
		 (js-jsstring->string
		    ,(j2s-scheme obj mode return conf hint)))))
	 (else
	  (epairify-deep loc
	     (j2s-get loc (j2s-scheme obj mode return conf hint) (j2s-type obj)
		(j2s-property-scheme field mode return conf)
		(j2s-type field) cache))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SInit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SInit mode return conf hint)
   (with-access::J2SAssig this (loc lhs rhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (type hint)
		(let ((hint (if type (list type) hint)))
		   (epairify-deep loc
		      `(begin
			  ,(j2s-scheme-set! lhs
			      (j2s-scheme rhs mode return conf hint)
			      #f mode return conf)
			  (js-undefined))))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssigOp mode return conf hint)
   (with-access::J2SAssigOp this (loc lhs rhs op)
      (epairify-deep loc
	 (cond
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field loc)
		(let ((tobj (gensym 'obj))
		      (pro (gensym 'pro))
		      (prov (j2s-scheme field mode return conf hint))
		      (left (gensym 'left))
		      (right (gensym 'right)))
		   `(let* ((,tobj ,(j2s-scheme obj mode return conf hint))
			   ,@(if (string? prov) '() (list `(,pro ,prov)))
			   (,left ,(j2s-get loc tobj (j2s-type obj)
				      (if (string? prov) prov pro) (j2s-type field)
				      #f))
			   (,right ,(j2s-scheme rhs mode return conf '())))
		       ,(j2s-put! loc tobj (if (string? prov) prov prov)
			   (js-binop loc op left right)
			   (strict-mode? mode)
			   #f)))))
	    ((isa? lhs J2SRef)
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (type hint)
		   (let ((hint (if type (list type) hint)))
		      (j2s-scheme-set! lhs
			 (js-binop2 loc op lhs rhs mode return conf hint)
			 (j2s-scheme lhs mode return conf '())
			 mode return conf)))))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(j2s-unresolved-put! `',id
		   (js-binop2 loc op lhs rhs mode return conf '())
		   #t mode return)))
	    (else
	     (j2s-error "j2sscheme" "Illegal assignment" this))))))

;*---------------------------------------------------------------------*/
;*    j2s-get ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-get loc obj tyobj prop typrop cache)
   (let ((prop (match-case prop
		  ((js-string->jsstring ?str) str)
		  (else prop))))
      (cond
	 ((> (bigloo-debug) 0)
	  (if (string? prop)
	      `(js-get/debug ,obj ',(string->symbol prop) %this ',loc)
	      `(js-get/debug ,obj ,prop %this ',loc)))
	 ((and (eq? tyobj 'vector) (eq? typrop 'number))
	  `(js-get ,obj ,prop %this))
	 (cache
	  (cond
	     ((string? prop)
	      (if (eq? tyobj 'object)
		  `(js-object-get-name/cache ,obj
		      ',(string->symbol prop) ,(pcache cache) %this)
		  `(js-get-name/cache ,obj
		      ',(string->symbol prop) ,(pcache cache) %this)))
	     ((number? prop)
	      `(js-get ,obj ,prop %this))
	     (else
	      `(js-get/cache ,obj ,prop ,(pcache cache) %this))))
	 (else
	  `(js-get ,obj ,prop %this)))))

;*---------------------------------------------------------------------*/
;*    j2s-put! ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-put! loc obj prop val mode cache)
   (cond
      ((> (bigloo-debug) 0)
       (if (string? prop)
	   `(js-put/debug! ,obj ',(string->symbol prop) ,val ,mode %this ',loc)
	   `(js-put/debug! ,obj ,prop ,val ,mode %this ',loc)))
      (cache
       (cond
	  ((string? prop)
	   `(js-put-name/cache! ,obj ',(string->symbol prop) ,val ,mode
	       ,(pcache cache) %this))
	  ((number? prop)
	   `(js-put! ,obj ,prop ,val ,mode %this))
	  (else
	   `(js-put/cache! ,obj ,prop ,val ,mode ,(pcache cache) %this))))
      (else
       (cond
	  ((string? prop)
	   `(js-put! ,obj ',(string->symbol prop) ,val ,mode %this))
	  (else
	   `(js-put! ,obj ,prop ,val ,mode %this))))))

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
	 (else
	  (with-access::J2SLiteralValue name (val)
	     `(js-toname ,(j2s-scheme val mode return conf hint) %this)))))
   
   (define (literal-propname name)
      (cond
	 ((isa? name J2SString)
	  (with-access::J2SString name (val)
	     (let ((str (string-for-read val)))
		(string->symbol val))))
	 ((isa? name J2SNumber)
	  (with-access::J2SNumber name (val)
	     (if (fixnum? val)
		 (string->symbol (number->string val))
		 `(js-toname ,(j2s-scheme val mode return conf hint) %this))))
	 ((isa? name J2SLiteralCnst)
	  (with-access::J2SLiteralCnst name (val)
	     (literal-propname val)))
	 ((isa? name J2SPragma)
	  `(js-toname ,(j2s-scheme name mode return conf hint) %this))
	 ((isa? name J2SLiteralCnst)
	  `(js-toname ,(j2s-scheme name mode return conf hint) %this))
	 (else
	  (with-access::J2SLiteralValue name (val)
	     `(js-toname ,(j2s-scheme val mode return conf hint) %this)))))
   
   (define (is-proto? name)
      (when (isa? name J2SString)
	 (with-access::J2SString name (val)
	    (string=? val "__proto__"))))
   
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
	 `(with-access::JsGlobalObject %this (__proto__)
	     (instantiate::JsObject
		(cmap ,(j2s-scheme cmap mode return conf hint))
		(elements (vector ,@vals))
		(__proto__ __proto__)
		(extensible #t)))))
   
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
				     (j2s-put! loc tmp "__proto__"
					(j2s-scheme val mode return conf hint)
					(strict-mode? mode) #f)
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
	 `(,(j2s-scheme name mode return conf hint) ,(j2s-scheme val mode return conf hint)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNew ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNew mode return conf hint)
   (with-access::J2SNew this (loc clazz args)
      (epairify loc
	 (j2s-new loc (j2s-scheme clazz mode return conf hint)
	    (j2s-scheme args mode return conf hint)))))

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
      (and (symbol? ty) (not (eq? ty 'bool) ) (not (eq? ty 'obj)))))

;*---------------------------------------------------------------------*/
;*    bool-expr? ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (bool-expr? this::J2SNode)
   #f)

;*---------------------------------------------------------------------*/
;*    bool-expr? ::J2SExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (bool-expr? this::J2SExpr)
   (with-access::J2SExpr this (type)
      (eq? type 'bool)))

;*---------------------------------------------------------------------*/
;*    bool-expr? ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (bool-expr? this::J2SBinary)
   (with-access::J2SBinary this (op lhs rhs)
      (or (and (memq op '(&& OR)) (bool-expr? lhs) (bool-expr? rhs))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    bool-expr? ::J2SUnary ...                                        */
;*---------------------------------------------------------------------*/
(define-method (bool-expr? this::J2SUnary)
   (with-access::J2SUnary this (op expr)
      (or (and (eq? op '!) (bool-expr? expr))
	  (call-next-method))))
   
;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SNode ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SNode mode return conf)
   (j2s-scheme this mode return conf '(bool)))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SExpr mode return conf)
   (with-access::J2SExpr this (type)
      (if (eq? type 'bool)
	  (j2s-scheme this mode return conf '(bool))
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
