;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/scheme.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 11:47:51 2013                          */
;*    Last change :  Tue Jul 15 07:30:30 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
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
	   __js2scheme_compile
	   __js2scheme_stage)
   
   (export j2s-scheme-stage
	   j2s-scheme-eval-stage
	   (generic j2s-scheme ::obj ::symbol ::procedure ::obj)
	   (j2s-scheme-id id)
	   (j2s-scheme-unserialize)
	   (j2s-scheme-unjson)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-scheme-stage
   (instantiate::J2SStageProc
      (name "scheme")
      (comment "Scheme code generation")
      (proc (lambda (ast conf)
	       (j2s-scheme ast 'normal comp-return
		  (append conf `(:debug-client ,(bigloo-debug))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-eval-stage ...                                        */
;*---------------------------------------------------------------------*/
(define j2s-scheme-eval-stage
   (instantiate::J2SStageProc
      (name "scheme")
      (comment "Scheme code generation (eval)")
      (proc (lambda (ast conf)
	       (j2s-scheme ast 'normal (lambda (x) x)
		  (append conf `(:debug-client ,(bigloo-debug))))))))

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
   (if (memq id '(raise error eval quote module))
       (symbol-append '^ id)
       id))

;*---------------------------------------------------------------------*/
;*    j2s-name ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-name name id)
   (or name (j2s-scheme-id id)))

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
   (symbol-append '% id))

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
(define-generic (j2s-scheme this mode return::procedure conf)
   (if (pair? this)
       (map (lambda (e) (j2s-scheme e mode return conf)) this)
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
;*    j2s-scheme-unserialize ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-unserialize)
   `(define (%unserialize alist)
       (js-service-unserialize alist %this)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-unjson ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-unjson)
   `(define (%unjson ip)
       (js-service-unjson ip %this)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SProgram ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SProgram mode return conf)

   (define (j2s-module module body)
      (with-access::J2SProgram this (nodes mode pcache-size)
	 (list
	    module
	    `(define %pcache (make-pcache ,pcache-size))
	    '(define %source (or (the-loading-file) "/"))
	    '(define %resource (dirname %source))
	    `(define (hopscript %this this %scope %module)
		
		,(j2s-scheme-unserialize)
		,(j2s-scheme-unjson)
		
		(define %worker (js-current-worker))
		
		,@body))))

   (define (j2s-main-module name body)
      (let ((module `(module ,(string->symbol name)
			(eval (library hop)
			   (library hopscript)
			   (library nodejs)
			   (library js2scheme))
			(library hop hopscript nodejs js2scheme)
			(main main))))
	 (with-access::J2SProgram this (nodes mode pcache-size %this path)
	    (list
	       module
	       `(define %pcache (make-pcache ,pcache-size))
	       `(define %this (nodejs-new-global-object))
	       `(define %source ,path)
	       '(define %resource (dirname %source))
	       
	       (j2s-scheme-unserialize)
	       (j2s-scheme-unjson)
	       
	       `(define (main args)
		   (define %worker (js-init-main-worker! %this))
		   (define %scope (js-clone %this))
		   (define this
		     (with-access::JsGlobalObject %this (js-object)
			(js-new0 %this js-object)))
		   (define %module (nodejs-module ,(basename path) ,path %this))
		   ,@body
		   (js-worker-terminate! %worker)
		   (thread-join! (thread-start-joinable! %worker)))))))
	 

   (with-access::J2SProgram this (module main nodes mode name pcache-size)
      (let ((body (flatten-nodes (j2s-scheme nodes mode return conf))))
	 (cond
	    (module
	     ;; a module whose declaration is in the source
	     (j2s-module module body))
	    ((not name)
	     ;; a mere expression
	     `(lambda (%this this %scope %module)
		 ,(j2s-scheme-unserialize)
		 ,(j2s-scheme-unjson)
		 (define %pcache (make-pcache ,pcache-size))
		 (define %worker (js-current-worker))
		 (define %source (or (the-loading-file) "/"))
		 (define %resource (dirname %source))
		 
		 ,@body))
	    (main
	     ;; generate a main hopscript module
	     (j2s-main-module name body))
	    (else
	     ;; generate the module clause
	     (let ((module `(module ,(string->symbol name)
			       (library hop hopscript js2scheme)
			       (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))))
		(j2s-module module body)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SVarDecls ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SVarDecls mode return conf)
   (illegal-node "j2s-scheme" this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-decl ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-decl this::J2SDecl value writable mode return)
   (with-access::J2SDecl this (loc id name global)
      (let ((ident (j2s-name name id)))
	 (epairify-deep loc
	    (if global
		(let ((fun-name (string->symbol
				   (format "function:~a:~a"
				      (cadr loc) (caddr loc)))))
		   (if (in-eval? return)
		       (j2s-unresolved-put! '%scope `',ident
			  value #f 'normal)
		       `(begin
			   (define ,ident ,value)
			   (js-bind! %this ,global ',id
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
(define-method (j2s-scheme this::J2SDecl mode return conf)
   (with-access::J2SDecl this (loc id writable)
      (j2s-scheme-decl this '(js-undefined) writable mode return)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-set! lhs val result mode return conf)
   (cond
      ((isa? lhs J2SRef)
       (with-access::J2SRef lhs (decl)
	  (with-access::J2SDecl decl (writable global id)
	     (if writable
		 (cond
		    ((and global (in-eval? return))
		     `(begin
			 ,(j2s-put! '%scope `',id val (eq? mode 'strict) #f)
			 ,result))
		    (result
		     `(begin
			 (set! ,(j2s-scheme lhs mode return conf) ,val)
			 ,result))
		    (else
		     `(set! ,(j2s-scheme lhs mode return conf) ,val)))
		 val))))
      ((not result)
       `(set! ,(j2s-scheme lhs mode return conf) ,val))
      (else
       `(begin
	   (set! ,(j2s-scheme lhs mode return conf) ,val)
	   ,result))))
	      
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclInit mode return conf)
   (with-access::J2SDeclInit this (loc id name val writable)
      (let ((ident (j2s-name name id)))
	 (epairify loc
	    (if writable
		`(begin
		    (set! ,ident ,(j2s-scheme val mode return conf))
		    (js-undefined))
		`(begin
		    ,(j2s-scheme val mode return conf)
		    (js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclFun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclFun mode return conf)
   (with-access::J2SDeclFun this (loc id name val global)
      (with-access::J2SFun val (params mode vararg)
	 (let* ((scmid (j2s-name name id))
		(fastid (j2s-fast-id id))
		(arity (if vararg -1 (+fx 1 (length params)))))
	    (epairify-deep loc
	       (if global 
		   `(begin
		       (define ,fastid
			  ,(jsfun->scheme val mode return conf))
		       (define ,scmid
			  (js-bind! %this ,global ',id
			     :configurable #f
			     :value (js-make-function %this
				       ,fastid
				       ,(length params) ',id
				       :arity ,arity
				       :constrarity ,arity
				       :strict ,(eq? mode 'strict)
				       :alloc (lambda (o)
						 (js-object-alloc o %this))
				       :construct ,fastid))))
		   `(begin
		       (define ,fastid
			  ,(jsfun->scheme val mode return conf))
		       (define ,scmid
			  (js-make-function %this
			     ,fastid ,(length params)
			     ',id
			     :arity ,arity
			     :constrarity ,arity
			     :strict ,(eq? mode 'strict)
			     :alloc (lambda (o) (js-object-alloc o %this))
			     :construct ,fastid)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclSvc ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclSvc mode return conf)
   (with-access::J2SDeclSvc this (loc id name val global)
      (let ((scmid (j2s-name name id))
	    (fastid (j2s-fast-id id)))
	 (epairify-deep loc
	    `(begin
		(define ,fastid ,(jssvc->scheme val scmid mode return conf))
		(define ,scmid 
		   (js-bind! %this ,global ',id
		      :configurable #f
		      :writable #f
		      :value ,fastid)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclExtern ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclExtern mode return conf)
   (with-access::J2SDeclExtern this (loc id name val bind writable)
      (if bind
          (j2s-scheme-decl this (j2s-scheme val mode return conf) writable mode return)
          (epairify loc
             `(define ,(j2s-name name id) ,(j2s-scheme val mode return conf))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SInit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SInit mode return conf)
   (with-access::J2SInit this (lhs rhs loc)
      (epairify loc
	 (j2s-scheme-set! lhs (j2s-scheme rhs mode return conf)
	    '(js-undefined) mode return conf))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRef mode return conf)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (id name global)
	 (if (and global (in-eval? return))
	     `(js-get-global-object-name %scope ',id #f %this)
	     (j2s-name name id)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWithRef ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWithRef mode return conf)
   (with-access::J2SWithRef this (id withs expr loc)
      (epairify loc
	 (let loop ((withs withs))
	    (if (null? withs)
		(j2s-scheme expr mode return conf)
		`(if ,(j2s-in? loc `',id (car withs))
		     ,(j2s-get loc (car withs) `',id #f)
		     ,(loop (cdr withs))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SHopRef ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SHopRef mode return conf)
   (with-access::J2SHopRef this (id)
      id))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SThis ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SThis mode return conf)
   (with-access::J2SThis this (loc)
      'this))

;*---------------------------------------------------------------------*/
;*    j2s-test ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-test test::J2SExpr mode return conf)
   (if (bool-expr? test)
       (j2s-bool-test test mode return conf)
       (let ((tmp (gensym)))
	  `(let ((,tmp ,(j2s-scheme test mode return conf)))
	      (if (boolean? ,tmp) ,tmp (js-totest ,tmp))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCond ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCond mode return conf)
   (with-access::J2SCond this (loc test then else)
	 (epairify loc
	    `(if ,(j2s-test test mode return conf)
		 ,(j2s-scheme then mode return conf)
		 ,(j2s-scheme else mode return conf)))))

;*---------------------------------------------------------------------*/
;*    pcache ...                                                       */
;*---------------------------------------------------------------------*/
(define (pcache cache)
   `(pcache-ref %pcache ,cache))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-unresolved name cache throw)
   (if cache
       `(js-get-global-object-name/cache %scope ',name
	   ,(pcache cache)
	   ,(if (pair? throw) `',throw throw)
	   %this)
       `(js-get-global-object-name %scope ',name
	   ,(if (pair? throw) `',throw throw)
	   %this)))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved-put! ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-unresolved-put! obj field expr throw::bool mode::symbol)
   (if (eq? mode 'strict)
       `(js-unresolved-put! ,obj ,field ,expr #t %this)
       `(js-put! ,obj ,field ,expr ,throw %this)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUnresolvedRef ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUnresolvedRef mode return conf)
   (with-access::J2SUnresolvedRef this (loc cache id)
      (epairify loc
	 (j2s-unresolved id cache loc))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SArrayAbsent ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArrayAbsent mode return conf)
   (with-access::J2SArrayAbsent this (loc)
      (epairify loc '(js-absent))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLiteralValue ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLiteralValue mode return conf)
   (with-access::J2SLiteralValue this (val)
      val))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNumber ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNumber mode return conf)
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
	      val)
	     (else
	      (if (or (>=fx val (bit-lsh 1 30)) (<fx val (negfx (bit-lsh 1 30))))
		  (fixnum->flonum val)
		  val))))
	 (else val))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SString ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SString mode return conf)
   (with-access::J2SString this (loc val)
      val))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRegExp ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRegExp mode return conf)
   (with-access::J2SRegExp this (loc val flags)
      (epairify loc
	 `(with-access::JsGlobalObject %this (js-regexp)
	     ,(j2s-new loc 'js-regexp 
		 (if (string-null? flags)
		     (list val)
		     (list val flags)))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SArray ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArray mode return conf)
   (with-access::J2SArray this (loc exprs)
      (let ((sexprs (j2s-scheme exprs mode return conf)))
	 (if (every (lambda (x)
			(or (number? x) (string? x) (boolean? x)))
		sexprs)
	     (epairify loc `(js-vector->jsarray ',(list->vector sexprs) %this))
	     (epairify loc `(js-vector->jsarray (vector ,@sexprs) %this))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNull ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNull mode return conf)
   (with-access::J2SLiteral this (loc)
      (epairify loc '(js-null))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUndefined ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUndefined mode return conf)
   (with-access::J2SLiteral this (loc)
      (epairify loc '(js-undefined))))

;*---------------------------------------------------------------------*/
;*    return-body ...                                                  */
;*---------------------------------------------------------------------*/
(define (return-body body)
   `(bind-exit (%return)
       ,body))

;*---------------------------------------------------------------------*/
;*    jsfun->scheme ...                                                */
;*---------------------------------------------------------------------*/
(define (jsfun->scheme this::J2SFun mode return conf)

   (define (lambda-or-labels id args body)
      (if id
	  (let ((%id (symbol-append '% id)))
	     `(labels ((,%id ,(cons 'this args) ,body)) ,%id))
	  `(lambda ,(cons 'this args)
	      ,body)))
   
   (define (param-scheme-id param)
      (with-access::J2SDecl param (id name)
	 (j2s-name name id)))
   
   (define (fixarg-lambda id params body)
      (let ((args (j2s-scheme params mode return conf)))
	 (lambda-or-labels id args body)))
   
   (define (init-alias-argument argument rest indx)
      (let ((id (param-scheme-id argument)))
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
   
   (define (init-argument val indx)
      `(js-arguments-define-own-property arguments ,indx
	  (instantiate::JsValueDescriptor
	     (name (string->symbol (integer->string ,indx)))
	     (value ,val)
	     (writable #t)
	     (configurable #t)
	     (enumerable #t))))
   
   (define (normal-vararg-lambda id params::pair-nil body)
      ;; normal mode: arguments is an alias
      (let ((rest (gensym 'rest))
	    (id (or id (gensym 'fun))))
	 (lambda-or-labels id rest
	    `(let ((arguments
		      (js-arguments %this
			 (make-vector (length ,rest) (js-absent)))))
		,@(if (pair? params)
		      (map (lambda (param)
			      (with-access::J2SDecl param (id name loc)
				 (epairify loc
				    `(define ,(j2s-name name id)
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
				,(symbol-append '% id) 0 ',id))
		      (writable #t)
		      (configurable #t)
		      (enumerable #f))
		   #f
		   %this)
		,body))))
   
   (define (strict-vararg-lambda id params::pair-nil body)
      ;; strict mode: arguments is initialized on entrance
      (let ((rest (gensym 'rest)))
	 (lambda-or-labels id rest
	    `(let ((arguments (js-strict-arguments %this ,rest)))
		,@(if (pair? params)
		      (map (lambda (param)
			      (with-access::J2SDecl param (id name loc)
				 (epairify loc
				    `(define ,(j2s-name name id)
					(js-undefined)))))
			 params)
		      '())
		,(when (pair? params)
		    `(when (pair? ,rest)
			(set! ,(param-scheme-id (car params)) (car ,rest))
			,(let loop ((params (cdr params)))
			    (if (null? params)
				#unspecified
				`(when (pair? (cdr ,rest))
				    (set! ,rest (cdr ,rest))
				    (set! ,(param-scheme-id (car params))
				       (car ,rest))
				    ,(loop (cdr params)))))))
		,body))))

   (with-access::J2SFun this (loc params body need-bind-exit-return vararg mode)
      (let* ((id (j2sfun-id this))
	     (body (if need-bind-exit-return
		       (with-access::J2SNode body (loc)
			  (epairify loc
			     (return-body
				(flatten-stmt (j2s-scheme body mode return conf)))))
		       (flatten-stmt (j2s-scheme body mode return conf))))
	     (fun (cond
		     ((not vararg)
		      (fixarg-lambda id params body))
		     ((eq? mode 'normal)
		      (normal-vararg-lambda id params body))
		     (else
		      (strict-vararg-lambda id params body)))))
	 (epairify-deep loc fun))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SFun mode return conf)
   (with-access::J2SFun this (loc params mode vararg)
      (let* ((id (j2sfun-id this))
	     (tmp (gensym))
	     (arity (if vararg -1 (+fx 1 (length params))))
	     (fundef `(let ((,tmp ,(jsfun->scheme this mode return conf)))
			 (js-make-function %this
			    ,tmp
			    ,(length params)
			    ',(string->symbol
				(format "function:~a:~a" (cadr loc) (caddr loc)))
			    :arity ,arity
			    :constrarity ,arity
			    :strict ,(eq? mode 'strict)
			    :alloc (lambda (o) (js-object-alloc o %this))
			    :construct ,tmp))))
	 (epairify-deep loc
	    (if id
		`(let ((,id (js-undefined)))
		    (set! ,id ,fundef)
		    ,id)
		fundef)))))

;*---------------------------------------------------------------------*/
;*    jssvc->scheme ::J2SSvc ...                                       */
;*---------------------------------------------------------------------*/
(define (jssvc->scheme this::J2SSvc id mode return conf)
   
   (define (j2sscheme-service this tmp id path args arity mode return)
      
      (define (jscript-funcall init)
	 ;; see runtime/service_expd.sch
	 (if (isa? init J2SObjInit)
	     "(sc_lambda = function ( argument ) { return new HopFrame( hop_apply_url( ~s, hop_object_to_dsssl_args( argument ) ) ); },
              sc_lambda.resource = function( file ) { return ~s + \"/\" + file; },
              sc_lambda)"
	     "(sc_lambda = function () { return new HopFrame( hop_apply_url( ~s, arguments ) ); },
              sc_lambda.resource = function( file ) { return ~s + \"/\" + file; },
              sc_lambda)"))
      
      (define (service-proc->scheme this)
	 (with-access::J2SSvc this (loc body need-bind-exit-return)
	    (let* ((body (if need-bind-exit-return
			     (with-access::J2SNode body (loc)
				(epairify loc
				   (return-body
				      (j2s-scheme body mode return conf))))
			     (j2s-scheme body mode return conf)))
		   (fun `(lambda ,args
			    (let ((req (current-request)))
			       (js-worker-exec %worker
				  (lambda ()
				     (thread-request-set! (current-thread) req)
				     ,(flatten-stmt body)))))))
	       (epairify-deep loc fun))))
      
      (with-access::J2SSvc this (init register)
	 `(js-make-service %this ,tmp ',id
	     ,register
	     ,arity
	     (instantiate::hop-service
		(proc ,(service-proc->scheme this))
		(javascript ,(jscript-funcall init))
		(path ,path)
		(id ',id)
		(wid ',id)
		(args ',args)
		(resource %resource)
		(source %source)
		(decoder %unserialize)
		(unjson %unjson)))))
   
   (define (init->formal init::J2SDataPropertyInit)
      (with-access::J2SDataPropertyInit init (name val)
	 (with-access::J2SString name ((name val))
	    (list (string->symbol name) (j2s-scheme val mode return conf)))))
   
   (define (init->actual init::J2SDataPropertyInit)
      (with-access::J2SDataPropertyInit init (name val)
	 (with-access::J2SString name ((name val))
	    (list (string->keyword name) (string->symbol name)))))
   
   (define (svc-proc-entry this params actuals arity)
      (with-access::J2SSvc this (loc)
	 (let ((tmpp (gensym 'servicep))
	       (tmps (gensym 'services)))
	    `(letrec* ((,tmpp (lambda (this ,@params #!rest rest)
				 (with-access::JsService ,tmps (svc)
				    (js-make-hopframe %this
				       (hop-apply-service-url svc 
					  (if (and (pair? rest)
						   (isa? (car rest) JsObject))
					      (js-object->keyword-arguments
						 (car rest) %this)
					      (list ,@actuals)))))))
		       (,tmps ,(j2sscheme-service this tmpp (or id tmpp)
				  (epairify loc
				     `(make-hop-url-name
					 ,(if (symbol? id)
					      (symbol->string id)
					      '(gen-service-url :public #t))))
				  params arity
				  mode return)))
		,tmps))))
   
   (define (svc-fix-proc-entry this)
      (with-access::J2SSvc this (params)
	 (let ((params (j2s-scheme params mode return conf)))
	    (svc-proc-entry this params params (length params)))))
   
   (define (svc-dsssl-proc-entry this)
      (with-access::J2SSvc this (init)
	 (with-access::J2SObjInit init (inits)
	    (let ((params (cons '#!key (map init->formal inits)))
		  (actuals (append-map init->actual inits)))
	       (svc-proc-entry this params actuals -1)))))
   
   (with-access::J2SSvc this (loc init)
      (epairify-deep loc
	 (if (isa? init J2SObjInit)
	     (svc-dsssl-proc-entry this)
	     (svc-fix-proc-entry this)))))
	   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSvc ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSvc mode return conf)
   (with-access::J2SSvc this (loc)
      (epairify loc
	 (jssvc->scheme this #f mode return conf))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SParam ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SParam mode return conf)
   (with-access::J2SParam this (id name)
      (j2s-name name id)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SReturn mode return conf)
   (with-access::J2SReturn this (loc expr tail)
      (if tail
	  (j2s-scheme expr mode return conf)
	  (epairify loc
	     `(%return ,(j2s-scheme expr mode return conf))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SThrow ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SThrow mode return conf)
   (with-access::J2SThrow this (loc expr)
      (epairify loc
	 `(raise ,(j2s-scheme expr mode return conf)))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2STry ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2STry mode return conf)
   (with-access::J2STry this (loc body catch finally)
      (epairify-deep loc
	 (let* ((trybody (j2s-scheme body mode return conf))
		(trie (if (isa? catch J2SNop)
			  (j2s-scheme body mode return conf)
			  (with-access::J2SCatch catch (loc param body)
			     (epairify-deep loc
				`(with-handler
				    (lambda (,(j2s-scheme param mode return conf))
				       ,(j2s-scheme body mode return conf))
				    ,trybody))))))
	    (if (isa? finally J2SNop)
		trie
		`(unwind-protect
		    ,trie
		    ,(j2s-scheme finally mode return conf)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWith ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWith mode return conf)
   (with-access::J2SWith this (obj block id)
      `(let ((,id (js-toobject %this ,(j2s-scheme obj mode return conf))))
	  ,(j2s-scheme block mode return conf))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPragma ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPragma mode return conf)
   (with-access::J2SPragma this (loc expr)
      (epairify-deep loc expr)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSequence mode return conf)
   (with-access::J2SSequence this (loc exprs)
      (if (pair? (cdr exprs))
	  (epairify loc `(begin ,@(j2s-scheme exprs mode return conf)))
	  (j2s-scheme (car exprs) mode return conf))))

;*---------------------------------------------------------------------*/
;*    js-binop ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-binop loc op lhsv rhsv)
   (case op
      ((OR)
       (let ((lhs (gensym 'lhs)))
	  `(let ((,lhs ,lhsv))
	      (if (js-totest ,lhs)
		  ,lhs
		  ,rhsv))))
      ((&&)
       (let ((lhs (gensym 'lhs)))
	  `(let ((,lhs ,lhsv))
	      (if (js-totest ,lhs)
		  ,rhsv
		  ,lhs))))
      (else
       (let ((lhs (gensym 'lhs))
	     (rhs (gensym 'rhs)))
	  `(let* ((,lhs ,lhsv)
		  (,rhs ,rhsv))
	      ,(case op
		  ((+)
		   `(js+ ,lhs ,rhs %this))
		  ((-)
		   `(js- ,lhs ,rhs %this))
		  ((*)
		   `(js* ,lhs ,rhs %this ))
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
		  (else
		   `(,op ,lhs ,rhs %this))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBinary mode return conf)
   (with-access::J2SBinary this (loc lhs rhs op)
      (epairify-deep loc
	 (js-binop loc op
	    (j2s-scheme lhs mode return conf) (j2s-scheme rhs mode return conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SParen ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SParen mode return conf)
   (with-access::J2SParen this (expr)
      (j2s-scheme expr mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUnary ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUnary mode return conf)

   (define (err id)
      (with-access::J2SUnary this (loc)
	 (match-case loc
	    ((at ?fname ?loc)
	     `(with-access::JsGlobalObject %this (js-syntax-error)
		 (js-raise
		    (js-new %this js-syntax-error
		       ,(format "Delete of an unqualified identifier in strict mode: \"~a\"" id)
		       ,fname ,loc))))
	    (else
	     `(with-access::JsGlobalObject %this (js-syntax-error)
		 (js-raise
		    (js-new %this js-syntax-error
		       ,(format "Delete of an unqualified identifier in strict mode: \"~a\"" id))))))))

   (define (delete->scheme expr)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.7
      (cond
	 ((isa? expr J2SWithRef)
	  (with-access::J2SWithRef expr (id withs expr loc)
	     (let loop ((withs withs))
		(if (null? withs)
		    `(begin ,(j2s-scheme expr mode return conf) #f)
		    `(if ,(j2s-in? loc `',id (car withs))
			 (js-delete! ,(j2s-scheme (car withs) mode return conf)
			    ',(j2s-scheme id mode return conf)
			    #f
			    %this)
			 ,(loop (cdr withs)))))))
	 ((isa? expr J2SAccess)
	  (with-access::J2SAccess expr (obj field)
	     `(js-delete! ,(j2s-scheme obj mode return conf)
		 ,(j2s-scheme field mode return conf)
		 ,(eq? mode 'strict)
		 %this)))
	 ((isa? expr J2SUnresolvedRef)
	  (if (eq? mode 'strict)
	      (with-access::J2SUnresolvedRef expr (id)
		 (err id))
	      (with-access::J2SUnresolvedRef expr (id)
		 `(js-delete! %scope ',id #f %this))))
	 ((isa? expr J2SRef)
	  (if (eq? mode 'strict)
	      (with-access::J2SRef expr (decl)
		 (with-access::J2SDecl decl (id)
		    (err id)))
	      '(begin #f)))
	 ((isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (delete->scheme expr)))
	 (else
	  `(begin ,(j2s-scheme expr mode return conf) #t))))

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
	  `(js-typeof ,(j2s-scheme expr mode return conf)))))
   
   (with-access::J2SUnary this (loc expr op)
      (epairify loc
	 (case op
	    ((!)
	     `(if ,(j2s-test expr mode return conf) #f #t))
	    ((typeof)
	     (typeof->scheme expr))
	    ((void)
	     `(begin ,(j2s-scheme expr mode return conf) (js-undefined)))
	    ((delete)
	     (delete->scheme expr))
	    ((+)
	     ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.6
	     (let ((expr (j2s-scheme expr mode return conf)))
		(if (eqv? expr 0)
		    `(begin +0.0)
		    `(js-tonumber ,expr %this))))
	    ((-)
	     ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.7
	     (let ((expr (j2s-scheme expr mode return conf)))
		(if (eqv? expr 0)
		    `(begin -0.0)
		    `(js-neg ,expr %this))))
	    ((~)
	     ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.8
	     `(js-bitnot ,(j2s-scheme expr mode return conf) %this))
	    (else
	     `(,op ,(j2s-scheme expr mode return conf)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPostfix ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.3.1       */
;*    -------------------------------------------------------------    */
;*    !!! x++ not equivalent to x = x + 1 as x++ always converts       */
;*    to number.                                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPostfix mode return conf)
   (with-access::J2SPostfix this (loc lhs op)
      (let ((inc (if (eq? op '++) 1 -1)))
	 (let loop ((lhs lhs))
	    (cond
	       ((isa? lhs J2SRef)
		(let ((tmp (gensym 'tmp)))
		   (epairify-deep loc
		      `(let ((,tmp (js-tonumber ,(j2s-scheme lhs mode return conf) %this)))
			  ,(j2s-scheme-set! lhs
			      (epairify loc `(js+ ,inc ,tmp %this))
			      tmp
			      mode return conf)))))
	       ((isa? lhs J2SAccess)
		(with-access::J2SAccess lhs ((o obj) field cache (loca loc))
		   (let ((tmp (gensym 'tmp))
			 (obj (gensym 'obj))
			 (pro (gensym 'prop))
			 (prov (j2s-scheme field mode return conf)))
		      (epairify-deep loc
			 `(let* ((,obj ,(j2s-scheme o mode return conf))
				 ,@(if (string? prov) '() (list `(,pro ,prov)))
				 (,tmp (js-tonumber
					  ,(j2s-get loca obj
					      (if (string? prov) prov pro)
					      cache)
					  %this)))
			     (js-put! ,obj ,(if (string? prov) prov pro)
				(js+ ,inc ,tmp %this) ,(eq? mode 'strict) %this)
			     ,tmp)))))
	       ((isa? lhs J2SUnresolvedRef)
		(with-access::J2SUnresolvedRef lhs (id cache loc)
		   (let ((tmp (gensym 'tmp)))
		      (epairify-deep loc
			 `(let ((,tmp (js-tonumber ,(j2s-unresolved id cache loc)
					 %this)))
			     ,(j2s-unresolved-put! '%scope `',id
				 `(+ ,inc ,tmp)
				 #t mode)
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
(define-method (j2s-scheme this::J2SPrefix mode return conf)
   (with-access::J2SPrefix this (loc lhs op)
      (let ((inc (if (eq? op '++) 1 -1)))
	 (let loop ((lhs lhs))
	    (cond
	       ((isa? lhs J2SRef)
		(epairify loc
		   (j2s-scheme-set! lhs
		      (epairify loc
			 `(js+ ,inc (js-tonumber ,(j2s-scheme lhs mode return conf) %this)  %this))
		      (j2s-scheme lhs mode return conf)
		      mode return conf)))
	       ((isa? lhs J2SAccess)
		(with-access::J2SAccess lhs ((o obj) field cache (loca loc))
		   (let ((tmp (gensym 'tmp))
			 (obj (gensym 'obj))
			 (pro (gensym 'prop))
			 (prov (j2s-scheme field mode return conf)))
		      (epairify-deep loc
			 `(let* ((,obj ,(j2s-scheme o mode return conf))
				 ,@(if (string? prov) '() (list `(,pro ,prov)))
				 (,tmp (js-tonumber
					  ,(j2s-get loca obj
					      (if (string? prov) prov pro)
					      cache)
					  %this)))
			     (js-put! ,obj ,(if (string? prov) prov pro)
				(js+ ,inc ,tmp %this) ,(eq? mode 'strict) %this)
			     (+ ,inc ,tmp))))))
	       ((isa? lhs J2SUnresolvedRef)
		(with-access::J2SUnresolvedRef lhs (id cache loc)
		   (let ((tmp (gensym 'tmp)))
		      (epairify-deep loc
			 `(let ((,tmp (js+ ,inc
					 (js-tonumber ,(j2s-unresolved id cache loc)
					    %this )
					 %this)))
			     ,(j2s-unresolved-put! '%scope `',id tmp #t mode)
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
(define-method (j2s-scheme this::J2SStmt mode return conf)
   (return this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSeq ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.1         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSeq mode return conf)
   (with-access::J2SSeq this (loc nodes)
      (let ((nodes nodes))
	 (cond
	    ((null? nodes)
	     (epairify loc
		(return '(js-undefined))))
	    ((pair? (cdr nodes))
	     (epairify loc
		`(begin ,@(j2s-scheme nodes mode return conf))))
	    (else
	     (j2s-scheme (car nodes) mode return conf))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNop ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.3         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNop mode return conf)
   (with-access::J2SNop this (loc)
      (epairify loc
	 (return '(js-undefined)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SStmtExpr ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.4         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SStmtExpr mode return conf)
   (with-access::J2SStmtExpr this (expr)
      (if (isa? expr J2SIf)
	  (j2s-scheme expr mode return conf)
	  (return (j2s-scheme expr mode return conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SIf ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.5         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SIf mode return conf)
   (with-access::J2SIf this (loc test then else)
      (let ((tmp (gensym)))
	 (epairify loc
	    `(if ,(j2s-test test mode return conf)
		 ,(j2s-scheme then mode return conf)
		 ,(j2s-scheme else mode return conf))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDo ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.1       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDo mode return conf)
   (with-access::J2SDo this (loc test body id
			       need-bind-exit-break need-bind-exit-continue)

      (define (comp-loop loop)
	 `(let ,loop ()
	       ,(if need-bind-exit-continue
		    (epairify-deep loc
		       `(bind-exit (,(escape-name '%continue id))
			   ,(j2s-scheme body mode return conf)))
		    (j2s-scheme body mode return conf))
	       (if ,(j2s-test test mode return conf)
		   (,loop)
		   '(js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       ,(if need-bind-exit-continue
		    (epairify-deep loc
		       `(bind-exit (,(escape-name '%continue id))
			   ,(j2s-scheme body mode acc-return conf)))
		    (j2s-scheme body mode acc-return conf))
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
(define-method (j2s-scheme this::J2SWhile mode return conf)
   (with-access::J2SWhile this (loc test body id
				  need-bind-exit-break need-bind-exit-continue)

      (define (comp-loop loop)
	 `(let ,loop ()
	       (if ,(j2s-test test mode return conf)
		   ,(if need-bind-exit-continue
			(epairify-deep loc
			   `(begin
			       (bind-exit (,(escape-name '%continue id))
				  ,(j2s-scheme body mode return conf))
			       (,loop)))
			(epairify-deep loc
			   `(begin
			       ,(j2s-scheme body mode return conf)
			       (,loop))))
		   '(js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       (if ,(j2s-test test mode return conf)
		   ,(if need-bind-exit-continue
			(epairify-deep loc
			   `(begin
			       (bind-exit (,(escape-name '%continue id))
				  ,(j2s-scheme body mode acc-return conf))
			       (,loop %acc)))
			(epairify-deep loc
			   `(begin
			       ,(j2s-scheme body mode acc-return conf)
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
(define-method (j2s-scheme this::J2SFor mode return conf)
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
				  ,(j2s-scheme body mode return conf)))
			   (j2s-scheme body mode return conf))
		      ,(j2s-scheme incr mode return conf)
		      (,loop))
		   (js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       (if ,(j2s-test test mode return conf)
		   (begin
		      ,(if need-bind-exit-continue
			   (epairify-deep loc
			      `(bind-exit (,(escape-name '%continue id))
				  ,(j2s-scheme body mode acc-return conf)))
			   (j2s-scheme body mode acc-return conf))
		      ,(j2s-scheme incr mode return conf)
		      (,loop %acc))
		   %acc)))

      (let* ((forid (gensym 'for))
	     (loop (if (in-eval? return) (eval-loop forid) (comp-loop forid))))
	 (epairify-deep loc
	    `(begin
		,(j2s-scheme init mode return conf)
		,(if need-bind-exit-break
		     (epairify-deep loc
			`(bind-exit (,(escape-name '%break id)) ,loop))
		     (epairify loc loop)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SForIn ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.4       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SForIn mode return conf)

   (define (for-in/break-comp tmp name props obj body set)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(let ((%acc (js-undefined)))
			(js-for-in ,(j2s-scheme obj mode return conf)
			   (lambda (,name)
			      ,set
			      ,(if need-bind-exit-continue
				   `(bind-exit (,(escape-name '%continue id))
				       ,(j2s-scheme body mode acc-return conf))
				   (j2s-scheme body mode acc-return conf)))
			   %this)
			%acc)))
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,for)
		for))))

   (define (for-in/break-eval tmp name props obj body set)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(js-for-in ,(j2s-scheme obj mode return conf)
			(lambda (,name)
			   ,set
			   ,(if need-bind-exit-continue
				`(bind-exit (,(escape-name '%continue id))
				    ,(j2s-scheme body mode return conf))
				(j2s-scheme body mode return conf)))
			%this)))
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,for)
		for))))

   (define (for-in/break tmp name props obj body set)
      (if (in-eval? return)
	  (for-in/break-eval tmp name props obj body set)
	  (for-in/break-comp tmp name props obj body set)))

   (define (for-in/w-break-comp tmp name props obj body set)
      `(js-for-in ,(j2s-scheme obj mode return conf)
	  (lambda (,name)
	     ,set
	     ,(j2s-scheme body mode return conf))
	  %this))

   (define (for-in/w-break-eval tmp name props obj body set)
      `(let ((%acc (js-undefined)))
	  (js-for-in ,(j2s-scheme obj mode return conf)
	     (lambda (,name)
		,set
		,(j2s-scheme body mode acc-return conf))
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
		   (j2s-unresolved-put! '%scope `',id name #f mode))))
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field)
		(epairify loc
		   `(js-put! ,(j2s-scheme obj mode return conf)
		       ,(j2s-scheme field mode return conf)
		       ,name
		       ,(eq? mode 'strict)
		       %this))))
	    ((isa? lhs J2SWithRef)
	     (with-access::J2SWithRef lhs (id withs expr loc)
		(epairify loc
		   (let liip ((withs withs))
		      (if (null? withs)
			  (loop expr)
			  `(if ,(j2s-in? loc `',id (car withs))
			       ,(j2s-put! (car withs) (symbol->string id)
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
(define-method (j2s-scheme this::J2SLabel mode return conf)
   (with-access::J2SLabel this (body)
      (j2s-scheme body mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBreak ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBreak mode return conf)
   (with-access::J2SBreak this (loc target)
      (with-access::J2SIdStmt target (id)
	 (epairify loc
	    `(,(escape-name '%break id)
	      ,(if (in-eval? return) '%acc '(js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SContinue ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SContinue mode return conf)
   (with-access::J2SContinue this (loc target)
      (with-access::J2SLoop target (id)
	 (epairify loc
	    `(,(escape-name '%continue id)
	      ,(if (in-eval? return) '%acc '(js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSwitch ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSwitch mode return conf)
   (with-access::J2SSwitch this (loc key cases id need-bind-exit-break)
      
      (define (comp-switch)
	 (let ((elseclause #f)
	       (elsefun #f)
	       (tmp (gensym 'tmp))
	       (funs (map (lambda (c) (gensym)) cases)))
	    `(let* ((,tmp ,(j2s-scheme key mode return conf))
		    ,@(map (lambda (case fun)
			      (with-access::J2SCase case (loc body)
				 (epairify loc
				    `(,fun
					(lambda ()
					   ,(j2s-scheme body mode return conf))))))
			 cases funs))
		(cond
		   ,@(filter-map (lambda (case::J2SCase fun)
				    (with-access::J2SCase case (loc expr body)
				       (cond
					  ((nil? expr)
					   (set! elseclause expr)
					   (set! elsefun fun)
					   #f)
					  (else
					   (epairify loc
					      `((js-strict-equal? ,tmp ,(j2s-scheme expr mode return conf))
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
	    `(let* ((,tmp ,(j2s-scheme key mode return conf))
		    (%acc (js-undefined))
		    ,@(map (lambda (case fun)
			      (with-access::J2SCase case (loc body)
				 (epairify loc
				    `(,fun
					(lambda ()
					   ,(j2s-scheme body mode acc-return conf))))))
			 cases funs))
		(cond
		   ,@(filter-map (lambda (case::J2SCase fun)
				    (with-access::J2SCase case (loc expr body)
				       (cond
					  ((nil? expr)
					   (set! elseclause expr)
					   (set! elsefun fun)
					   #f)
					  (else
					   (epairify loc
					      `((js-strict-equal? ,tmp ,(j2s-scheme expr mode return conf))
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
(define-method (j2s-scheme this::J2SCall mode return conf)
   
   (define (read-only-function ref::J2SRef)
      (with-access::J2SRef ref (decl)
	 (when (isa? decl J2SDeclCnstFun)
	    decl)))
   
   (define (call-method fun::J2SAccess args)
      (with-access::J2SAccess fun (loc obj field)
	 (if (isa? obj J2SRef)
	     (call-unknown-function fun
		(j2s-toobject loc (j2s-scheme obj mode return conf))
		args)
	     (let ((tmp (gensym)))
		`(let ((,tmp ,(j2s-scheme obj mode return conf)))
		    ,(call-unknown-function
			(duplicate::J2SAccess fun
			   (obj (instantiate::J2SPragma
				   (loc loc)
				   (expr tmp))))
			(j2s-toobject loc tmp) args))))))
   
   (define (call-hop-function fun::J2SHopRef args)
      `(,(j2s-scheme fun mode return conf) ,@(j2s-scheme args mode return conf)))

   (define (call-fun-function fun::J2SFun f args)
      (with-access::J2SFun fun (params vararg)
	 (if vararg
	     `(,f (js-undefined) ,@(j2s-scheme args mode return conf))
	     (let ((lenf (length params))
		   (lena (length args)))
		(cond
		   ((=fx lenf lena)
		    ;; matching arity
		    `(,f (js-undefined) ,@(j2s-scheme args mode return conf)))
		   ((>fx lena lenf)
		    ;; too many arguments ignore the extra values
		    `(,f (js-undefined) ,@(j2s-scheme (take args lenf) mode return conf)))
		   (else
		    ;; argument missing
		    `(,f
			(js-undefined)
			,@(j2s-scheme args mode return conf)
			,@(make-list (-fx lenf lena) '(js-undefined)))))))))

   (define (call-with-function fun::J2SWithRef args)
      (with-access::J2SWithRef fun (id withs loc)
	 (let loop ((withs withs))
	    (if (null? withs)
		(call-unknown-function fun '(js-undefined) args)
		`(if ,(j2s-in? loc `',id (car withs))
		     ,(call-unknown-function (j2s-get loc (car withs) `',id #f)
			(car withs) args)
		     ,(loop (cdr withs)))))))

   (define (call-pragma fun::J2SPragma args)
      (with-access::J2SPragma fun (expr)
	 `(,expr %this ,@(j2s-scheme args mode return conf))))

   (define (call-known-function fun::J2SDeclCnstFun args)
      (with-access::J2SDeclCnstFun fun (id fun)
	 (call-fun-function fun (j2s-fast-id id) args)))

   (define (call-unknown-function fun thisarg args)
      (let* ((len (length args))
	     (call (if (>=fx len 9)
		       'js-calln
		       (string->symbol (format "js-call~a" (length args))))))
	 (if (> (bigloo-debug) 0)
	     (with-access::J2SCall this (loc)
		`(,(symbol-append call '/debug) %this ',loc
		  ,(j2s-scheme fun mode return conf) ,thisarg
		  ,@(j2s-scheme args mode return conf)))
	     `(,call %this ,(j2s-scheme fun mode return conf) ,thisarg
		 ,@(j2s-scheme args mode return conf)))))

   (define (call-eval-function fun args)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.1.1
      `(%js-direct-eval 
	  ,(if (null? args)
	       '(js-undefined)
	       (j2s-scheme (car args) mode return conf))
	  ,(eq? mode 'strict)
	  %this this %scope))

   (define (is-eval? fun)
      (with-access::J2SUnresolvedRef fun (id)
	 (eq? id 'eval)))
   
   (with-access::J2SCall this (loc fun args)
      (epairify loc
	 (cond
	    ((isa? fun J2SAccess)
	     (call-method fun args))
	    ((isa? fun J2SHopRef)
	     (call-hop-function fun args))
	    ((isa? fun J2SFun)
	     (call-fun-function fun (jsfun->scheme fun mode return conf) args))
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
	     (lambda (fun) (call-known-function fun args)))
	    (else
	     (call-unknown-function fun '(js-undefined) args))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssig mode return conf)
   (with-access::J2SAssig this (loc lhs rhs)
      (let loop ((lhs lhs))
	 (cond
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field cache)
		(epairify loc
		   (j2s-put! (j2s-scheme obj mode return conf)
		      (j2s-scheme field mode return conf)
		      (j2s-scheme rhs mode return conf)
		      (eq? mode 'strict)
		      cache))))
	    ((isa? lhs J2SRef)
	     (let ((assig (j2s-scheme-set! lhs
			     (j2s-scheme rhs mode return conf)
			     (j2s-scheme lhs mode return conf)
			     mode return conf)))
		(if (pair? assig)
		    (epairify loc assig)
		    assig)))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(epairify loc
		   (j2s-unresolved-put! '%scope `',id
		      (j2s-scheme rhs mode return conf) #f mode))))
	    ((isa? lhs J2SWithRef)
	     (with-access::J2SWithRef lhs (id withs expr loc)
		(epairify loc
		   (let liip ((withs withs))
		      (if (null? withs)
			  (loop expr)
			  `(if ,(j2s-in? loc `',id (car withs))
			       ,(j2s-put! (car withs) (symbol->string id)
				   (j2s-scheme rhs mode return conf) #f #f)
			       ,(liip (cdr withs))))))))
	    ((isa? lhs J2SUndefined)
	     (j2s-scheme rhs mode return conf))
	    ((isa? lhs J2SParen)
	     (with-access::J2SParen lhs (expr)
		(loop expr)))
	    (else
	     (j2s-error "assignment" "Illegal assignment" this))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SInit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SInit mode return conf)
   (with-access::J2SAssig this (loc lhs rhs)
      (if (isa? lhs J2SRef)
	  (epairify-deep loc
	     `(begin
		 ,(j2s-scheme-set! lhs (j2s-scheme rhs mode return conf)
		     #f mode return conf)
		 (js-undefined)))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssigOp mode return conf)
   (with-access::J2SAssigOp this (loc lhs rhs op)
      (epairify-deep loc
	 (cond
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field)
		(let ((tobj (gensym 'obj))
		      (pro (gensym 'pro))
		      (prov (j2s-scheme field mode return conf)))
		   `(let ((,tobj ,(j2s-scheme obj mode return conf))
			  ,@(if (string? prov) '() (list `(,pro ,prov))))
		       (js-put! ,tobj ,(if (string? prov) prov prov)
			  ,(js-binop loc op
			      (j2s-get loc tobj (if (string? prov) prov pro) #f)
			      (j2s-scheme rhs mode return conf))
			  ,(eq? mode 'strict)
			  %this)))))
	    ((isa? lhs J2SRef)
	     (j2s-scheme-set! lhs
		(js-binop loc op
		   (j2s-scheme lhs mode return conf) (j2s-scheme rhs mode return conf))
		(j2s-scheme lhs mode return conf)
		mode return conf))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(j2s-unresolved-put! '%scope `',id
		   (js-binop loc op
		      (j2s-scheme lhs mode return conf)
		      (j2s-scheme rhs mode return conf))
		   #t mode)))
	    (else
	     (j2s-error "j2sscheme" "Illegal assignment" this))))))

;*---------------------------------------------------------------------*/
;*    j2s-get ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-get loc obj prop cache)
   (cond
      ((> (bigloo-debug) 0)
       (if (string? prop)
	   `(js-get/debug ,obj ',(string->symbol prop) %this ',loc)
	   `(js-get/debug ,obj ,prop %this ',loc)))
      (cache
       (if (string? prop)
	   `(js-get-name/cache ,obj ',(string->symbol prop) ,(pcache cache) %this)
	   `(js-get/cache ,obj ,prop ,(pcache cache) %this)))
      (else
       `(js-get ,obj ,prop %this))))

;*---------------------------------------------------------------------*/
;*    j2s-put! ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-put! obj prop val mode cache)
   (if cache
       (cond
	  ((string? prop)
	   `(js-put-name/cache! ,obj ',(string->symbol prop) ,val ,mode
	       ,(pcache cache) %this))
	  (else
	   `(js-put/cache! ,obj ,prop ,val ,mode ,(pcache cache) %this)))
       (cond
	  ((string? prop)
	   `(js-put! ,obj ',(string->symbol prop) ,val ,mode %this))
	  (else
	   `(js-put! ,obj ,prop ,val ,mode %this)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAccess mode return conf)
   (with-access::J2SAccess this (loc obj field cache)
      (epairify-deep loc
	 (j2s-get loc (j2s-scheme obj mode return conf)
	    (j2s-scheme field mode return conf) cache))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SObjInit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SObjInit mode return conf)
   
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
		 `(js-toname ,(j2s-scheme val mode return conf) %this))))
	 (else
	  (with-access::J2SLiteralValue name (val)
	     `(js-toname ,(j2s-scheme val mode return conf) %this)))))
   
   (with-access::J2SObjInit this (loc inits)
      (let ((tmp (gensym)))
	 (epairify loc
	    `(with-access::JsGlobalObject %this (js-object)
		(let ((,tmp ,(j2s-new loc 'js-object '())))
		   ,@(map (lambda (i)
			     (if (isa? i J2SDataPropertyInit)
				 (with-access::J2SDataPropertyInit i (loc name val)
				    (epairify loc
				       `(js-bind! %this ,tmp
					   ,(j2s-propname name)
					   :value ,(j2s-scheme val mode return conf)
					   :writable #t
					   :enumerable #t
					   :configurable #t)))
				 (with-access::J2SAccessorPropertyInit i (loc name get set)
				    (epairify loc
				       `(js-bind! %this ,tmp
					   ,(j2s-propname name)
					   :get ,(j2s-scheme get mode return conf)
					   :set ,(j2s-scheme set mode return conf)
					   :writable #t
					   :enumerable #t
					   :configurable #t)))))
			inits)
		   ,tmp))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDataPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDataPropertyInit mode return conf)
   (with-access::J2SDataPropertyInit this (loc name val)
      (epairify loc
	 `(,(j2s-scheme name mode return conf) ,(j2s-scheme val mode return conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNew ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNew mode return conf)
   (with-access::J2SNew this (loc clazz args)
      (epairify loc
	 (j2s-new loc (j2s-scheme clazz mode return conf)
	    (j2s-scheme args mode return conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2STilde ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2STilde mode return conf)
   
   (define (concat lst)
      (cond
	 ((null? lst)
	  '())
	 ((isa? (car lst) J2SNode)
	  (concat (cdr lst)))
	 ((not (string? (car lst)))
	  (cons (car lst) (concat (cdr lst))))
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
		 (cons (apply string-append lst)
		    (concat (cdr cursor))))
		(else
		 (set-cdr! prev '())
		 (cons* (apply string-append lst)
		    (car cursor)
		    (concat (cdr cursor)))))))))
   
   (with-access::J2STilde this (loc stmt)
      (let ((js-stmt (concat (j2s-js stmt #f #f mode return conf))))
	 (epairify loc
	    `(instantiate::xml-tilde
		(body (vector
			  ',(j2s->list stmt) '() '() '()
			  ,(cond
			     ((null? js-stmt)
			      "")
			     ((null? (cdr js-stmt))
			      (car js-stmt))
			     ((every string? js-stmt)
			      (apply string-append js-stmt))
			     (else
			      `(string-append ,@js-stmt)))))
;* 		(body #unspecified)                                    */
;* 		(%js-statement ,(cond                                  */
;* 				   ((null? js-stmt)                    */
;* 				    "")                                */
;* 				   ((null? (cdr js-stmt))              */
;* 				    (xml-tilde-debug loc               */
;* 				       (car js-stmt)))                 */
;* 				   (else                               */
;* 				    (xml-tilde-debug loc               */
;* 				       (apply string-append js-stmt))))) */
;* 		(%js-expression ,(cond                                 */
;* 				    ((null? js-stmt)                   */
;* 				     "")                               */
;* 				    ((null? (cdr js-stmt))             */
;* 				     (xml-tilde-debug loc              */
;* 					(car js-stmt)))                */
;* 				    (else                              */
;* 				     (xml-tilde-debug loc              */
;* 					(apply string-append js-stmt))))) */
;* 		(%js-return ,(cond                                     */
;* 				((null? js-stmt)                       */
;* 				 "return")                             */
;* 				((null? (cdr js-stmt))                 */
;* 				 (xml-tilde-debug loc                  */
;* 				    (string-append "return " (car js-stmt)))) */
;* 				(else                                  */
;* 				 (xml-tilde-debug loc                  */
;* 				    (apply string-append "return " js-stmt))))) */
		(loc ',loc))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDollar ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDollar mode return conf)
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
(define (j2s-error proc msg obj)
   (with-access::J2SNode obj (loc)
      (match-case loc
	 ((at ?fname ?loc)
	  (error/location proc msg (j2s->list obj) fname loc))
	 (else
	  (error proc msg obj)))))

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
   (j2s-scheme this mode return conf))

;*---------------------------------------------------------------------*/
;*    j2s-bool-test ::J2SBinary ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-bool-test this::J2SBinary mode return conf)
   (with-access::J2SBinary this (op lhs rhs loc)
      (case op
	 ((&&)
	  (epairify loc
	     `(and ,(j2s-bool-test lhs mode return conf)
		   ,(j2s-bool-test rhs mode return conf))))
	 ((OR)
	  (epairify loc
	     `(or ,(j2s-bool-test lhs mode return conf)
		  ,(j2s-bool-test rhs mode return conf))))
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
