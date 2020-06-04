;*=====================================================================*/
;*    serrano/prgm/project/hop/3.3.x/js2scheme/scheme-fun.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:04:57 2017                          */
;*    Last change :  Tue Jun  2 07:30:39 2020 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript functions                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-fun

   (include "ast.sch"
	    "usage.sch"
	    "context.sch"
	    "../hopscript/arity.sch")

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-constant)

   (export (j2s-scheme-closure ::J2SDecl mode return ::struct)
	   (jsfun->lambda ::J2SFun mode return ::struct proto ::bool)
	   (j2sfun->scheme ::J2SFun tmp tmpm mode return ::struct)
	   (j2s-fun-prototype ::J2SFun)
	   (j2s-function-arity ::J2SFun ::struct)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclFun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclFun mode return ctx)

   (define (constructor-only? this::J2SDeclFun)
      (with-access::J2SDeclFun this (val)
	 (when (decl-ronly? this)
	    (when (isa? val J2SFun)
	       (with-access::J2SFun val (generator)
		  (unless generator
		     (and (decl-usage-has? this '(new))
			  (not (decl-usage-has? this '(ref get call instanceof))))))))))

   (define (lambda? id)
      (or (memq id '(lambda lambda::obj))
	  (when (symbol? id)
	     (string-prefix? "lambda::" (symbol->string id)))))

   (define (type-lambda lambd id)
      (if (memq lambd '(lambda lambda::obj))
	  id
	  (symbol-append id
	     (string->symbol (substring (symbol->string lambd) 6)))))
      
   (define (beautiful-define expr)
      (match-case expr
	 ((define ?id (labels ((?id ?args . ?body)) ?id))
	  `(define ,(cons id args) ,@body))
	 ((define ?id ((and ?lambd (? lambda?)) ?args . ?body))
	  `(define ,(cons (type-lambda lambd id) args) ,@body))
	 (else
	  expr)))

   (define (global-declfun this val scmid fastid)
      (with-access::J2SDeclFun this (loc id vtype)
	 `(begin
	     ,(beautiful-define
		 `(define ,fastid
		     ,(jsfun->lambda val mode return ctx
			 (j2s-declfun-prototype this ctx)
			 (decl-usage-has? this '(new)))))
	     ,@(if (optimized-ctor this ctx)
		   `(,(beautiful-define
			 `(define ,(j2s-fast-constructor-id id)
			     ,(j2sfun->ctor val mode return ctx this))))
		   '())
	     ,@(cond
		  ((eq? vtype 'procedure)
		   `((define ,scmid ,fastid)))
		  ((j2s-fun-no-closure? this)
		   '())
		  (else
		   `((define ,scmid #unspecified)))))))

   (define (regular-declfun this val scmid fastid)
      (with-access::J2SDeclFun this (loc id vtype)
	 `(begin
	     ,(beautiful-define
		 `(define ,fastid
		     ,(jsfun->lambda val mode return ctx
			 (j2s-declfun-prototype this ctx)
			 (decl-usage-has? this '(new)))))
	     ,@(if (optimized-ctor this ctx)
		   `(,(beautiful-define
			 `(define ,(j2s-fast-constructor-id id)
			     ,(j2sfun->ctor val mode return ctx this))))
		   '())
	     ,@(cond
		  ((eq? vtype 'procedure)
		   `((define ,scmid ,fastid)))
		  ((j2s-fun-no-closure? this)
		   '())
		  (else
		   `((define ,scmid 
			,(j2s-make-function this mode return ctx))))))))
   
   (with-access::J2SDeclFun this (loc id scope val exports vtype)
      (let ((val (declfun-fun this)))
	 (with-access::J2SFun val (params mode vararg body name generator)
	    (let* ((scmid (j2s-decl-scheme-id this))
		   (fastid (j2s-profile-id (j2s-fast-id id) loc ctx)))
	       (epairify-deep loc
		  (case scope
		     ((none)
		      (beautiful-define
			 `(define ,fastid
			     ,(jsfun->lambda val mode return ctx
				 (j2s-declfun-prototype this ctx)
				 (decl-usage-has? this '(new))))))
		     ((letblock)
		      (let ((def `(,fastid ,(jsfun->lambda val mode return ctx
					       (j2s-declfun-prototype this ctx)
					       (decl-usage-has? this '(new))))))
			 (cond
			    ((eq? vtype 'procedure)
			     (list def `(,scmid ,fastid)))
			    ((j2s-fun-no-closure? this)
			     (list def))
			    (else
			     (list def
				`(,scmid ,(j2s-make-function this
					     mode return ctx)))))))
		     ((global %scope)
		      (global-declfun this val scmid fastid))
		     ((export)
		      (with-access::J2SExport (car exports) (index)
			 (append
			    (regular-declfun this val scmid fastid)
			    `((vector-set! %evars ,index ,scmid)))))
		     (else
		      (regular-declfun this val scmid fastid)))))))))

;*---------------------------------------------------------------------*/
;*    declfun-fun ...                                                  */
;*---------------------------------------------------------------------*/
(define (declfun-fun this::J2SDeclFun)
   (with-access::J2SDeclFun this (val)
      (cond
	 ((isa? val J2SFun)
	  val)
	 ((isa? val J2SMethod) 
	  (with-access::J2SMethod val (function) function))
	 (else
	  (error "declfun-fun" "bad val" (j2s->list val))))))

;*---------------------------------------------------------------------*/
;*    j2s-function-arity ...                                           */
;*    -------------------------------------------------------------    */
;*    See js-function-arity                                            */
;*---------------------------------------------------------------------*/
(define (j2s-function-arity this::J2SFun ctx)
   
   (define (count-params params)
      (let ((opt 0)
	    (req 0))
	 (for-each (lambda (p)
		      (if (or (not (isa? p J2SDeclInit))
			      (with-access::J2SDeclInit p (val)
				 (nodefval? val)))
			  (set! req (+fx req 1))
			  (set! opt (+fx opt 1))))
	    params)
	 (values req opt)))
   
   (with-access::J2SFun this (vararg params argumentsp name)
      (multiple-value-bind (req opt)
	 (count-params params)
	 (cond
	    ((eq? vararg 'arguments)
	     (if (context-get ctx :optim-arguments)
		 (with-access::J2SDeclArguments argumentsp (alloc-policy)
		    (if (eq? alloc-policy 'lazy)
			(js-function-arity req opt 'arguments-lazy)
			(js-function-arity req opt 'arguments-eager)))
		 (js-function-arity req opt 'arguments)))
	    ((eq? vararg 'rest)
	     (with-access::J2SDeclRest (car (last-pair params)) (alloc-policy)
		(if (eq? alloc-policy 'lazy)
		    (js-function-arity req opt 'rest-lazy)
		    (js-function-arity req opt 'rest))))
	    ((=fx opt 0)
	     (js-function-arity req opt))
	    (else
	     (js-function-arity req opt 'optional))))))

;*---------------------------------------------------------------------*/
;*    j2s-make-function ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-make-function this::J2SDeclFun mode return ctx)

   (define (allocator::symbol this::J2SDecl)
      (with-access::J2SFun (declfun-fun this) (new-target)
	 (cond
	    (new-target 'js-object-alloc/new-target)
	    ((decl-usage-has? this '(new get set))  'js-object-alloc)
	    (else 'js-object-alloc-lazy))))

   (define (make-function-sans-alloc this::J2SDeclFun)
      (with-access::J2SDeclFun this (loc id scope val key)
	 (let ((val (declfun-fun this)))
	    (with-access::J2SFun val (params mode vararg body name generator
					constrsize method new-target)
	       (let* ((fastid (j2s-profile-id (j2s-fast-id id) loc ctx))
		      (lparams (length params))
		      (arity (j2s-function-arity val ctx))
		      (len (if (eq? vararg 'rest) (-fx lparams 1) lparams))
		      (alloc (allocator this))
		      (src (j2s-function-src loc val ctx)))
		  (cond
		     (generator
		      `(js-make-function %this ,fastid
			  ,len ,(& id (context-program ctx))
			  :src ,src
			  :arity ,arity
			  :strict ',mode
			  :alloc ,alloc
			  :prototype ,(j2s-fun-prototype val)
			  :__proto__ ,(j2s-fun-__proto__ val)
			  :construct ,fastid
			  :constrsize ,constrsize))
		     ((and src (memq mode '(strict hopscript)))
		      `(,(if (eq? alloc 'js-object-alloc-lazy)
			     'js-make-function-strict-lazy
			     'js-make-function-strict)
			%this ,fastid 
			,len ,(& id (context-program ctx))
			:src ,src
			:constrsize ,constrsize
			:method ,(if method
				     (jsfun->lambda method
					mode return ctx #f #f)
				     fastid)
			:arity ,arity
			,@(if (eq? alloc 'js-object-alloc-lazy)
			      '()
			      `(:alloc ,alloc))))
		     (src
		      `(js-make-function %this ,fastid
			  ,len ,(& id (context-program ctx))
			  :src ,src
			  :arity ,arity
			  :strict ',mode
			  :alloc ,alloc
			  :construct ,fastid
			  :constrsize ,constrsize
			  :method ,(when method
				      (jsfun->lambda method
					 mode return ctx #f #f))))
		     ((and (eq? vararg 'arguments)
			   (memq mode '(strict hopscript)))
		      `(,(if (eq? alloc 'js-object-alloc-lazy)
			     'js-make-function-strict-lazy
			     'js-make-function-strict)
			%this ,fastid
			,len ,(& id (context-program ctx))
			:constrsize ,constrsize
			:method ,(if method
				     (jsfun->lambda method
					mode return ctx #f #f)
				     fastid)
			:arity ,arity
			,@(if (eq? alloc 'js-object-alloc-lazy)
			      '()
			      `(:alloc ,alloc))))
		     ((eq? vararg 'arguments)
		      `(js-make-function %this ,fastid
			  ,len ,(& id (context-program ctx))
			  :arity ,arity
			  :strict ',mode 
			  :constrsize ,constrsize
			  :alloc ,alloc
			  :method ,(when method
				      (jsfun->lambda method
					 mode return ctx #f #f))))
		     ((and method (memq mode '(strict hopscript)))
		      `(,(if (eq? alloc 'js-object-alloc-lazy)
			     'js-make-function-strict-lazy
			     'js-make-function-strict)
			%this ,fastid
			,len ,(& id (context-program ctx))
			:constrsize ,constrsize
			:method ,(if method
				     (jsfun->lambda method
					mode return ctx #f #f)
				     fastid)
			:arity ,arity
			,@(if (eq? alloc 'js-object-alloc-lazy)
			      '()
			      `(:alloc ,alloc))))
		     (method
		      `(js-make-function %this ,fastid
			  ,len ,(& id (context-program ctx))
			  :arity ,arity
			  :strict ',mode
			  :alloc ,alloc
			  :construct ,fastid
			  :constrsize ,constrsize
			  :method ,(when method
				      (jsfun->lambda method
					 mode return ctx #f #f))))
		     ((or (decl-usage-has? this '(new ref)) new-target)
		      `(js-make-function %this ,fastid
			  ,len ,(& id (context-program ctx))
			  :arity ,arity
			  :strict ',mode
			  :alloc ,alloc
			  :construct ,fastid
			  :constrsize ,constrsize))
		     (else
		      `(js-make-function-simple %this ,fastid
			  ,len ,(& id (context-program ctx))
			  ,arity
			  ',mode
			  ,constrsize))))))))

   (with-access::J2SDeclFun this (val)
      (let ((fun (make-function-sans-alloc this)))
	 (if (decl-usage-has? this '(new ref))
	     (with-access::J2SFun (declfun-fun this) (body loc)
		(if (cancall? this)
		    fun
		    (epairify loc
		       `(js-function-set-constrmap! ,fun))))
	     fun))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-closure ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-closure this::J2SDecl mode return ctx)
   (when (and (isa? this J2SDeclFun) (not (isa? this J2SDeclSvc)))
      (with-access::J2SDeclFun this (loc id scope val vtype)
	 (let ((val (declfun-fun this)))
	    (with-access::J2SFun val (params mode vararg body name generator)
	       (let* ((scmid (j2s-decl-scheme-id this))
		      (fastid (j2s-profile-id (j2s-fast-id id) loc ctx)))
		  (unless (j2s-fun-no-closure? this)
		     (case scope
			((none)
			 #f)
			((letblock)
			 #f)
			((global %scope)
			 (if (eq? vtype 'procedure)
			     #unspecified
			     (epairify-deep loc
				`(set! ,scmid
				    ,(if (js-need-global? this scope mode)
					 `(js-bind! %this ,scope
					     ,(j2s-scheme-name id ctx)
					     :configurable #f
					     :value ,(j2s-make-function this
							mode return ctx))
					 (j2s-make-function this
					    mode return ctx))))))
			(else
			 #f)))))))))
      
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclSvc ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclSvc mode return ctx)
   (with-access::J2SDeclSvc this (loc id val scope)
      (let* ((scmid (j2s-decl-scheme-id this))
	     (fastid (j2s-profile-id (j2s-fast-id id) loc ctx))
	     (lamid (j2s-fast-id fastid))
	     (fun (if (isa? val J2SFun)
		      val
		      (with-access::J2SMethod val (function)
			 function)))
	     (lam (jsfun->lambda fun mode return ctx
		     (j2s-fun-prototype fun) #f)))
	 (epairify-deep loc
	    (if (and (memq scope '(global %scope))
		     (js-need-global? this scope mode))
		`(begin
		    (define ,lamid ,lam)
		    (define ,fastid
		       ,(jssvc->scheme fun lamid id scmid mode return ctx))
		    (define ,scmid
		       (js-bind! %this ,scope ,(j2s-scheme-name id ctx)
			  :configurable #f
			  :writable #f
			  :value ,fastid)))
		`(begin
		    (define ,lamid ,lam)
		    (define ,fastid
		       ,(jssvc->scheme fun lamid id scmid mode return ctx))
		    (define ,scmid
		       ,fastid)))))))

;*---------------------------------------------------------------------*/
;*    jsfun->lambda/body ...                                           */
;*---------------------------------------------------------------------*/
(define (jsfun->lambda/body this::J2SFun mode return ctx body)

   (define (type-this idthis thisp)
      (if (and idthis (isa? thisp J2SDecl))
	  (with-access::J2SDecl thisp (vtype)
	     (type-ident idthis vtype (context-conf ctx)))
	  idthis))
   
   (define (lambda-or-labels rtype %gen this id args body loc)
      ;; in addition to the user explicit arguments, this and %gen
      ;; are treated as:
      ;;   - some typed functions are optimized and they don't expect a this
      ;;     argument
      ;;   - some typed functions implement a generator body and they take
      ;;     as extra argument the generator
      (let* ((targs (if this (cons this args) args))
	     (gtargs (if %gen (cons '%gen targs) targs)))
	 (if id
	     (let ((%id (j2s-profile-id (j2s-fast-id id) loc ctx)))
		`(labels ((,%id ,gtargs ,body)) ,%id))
	     `(,(type-ident 'lambda rtype (context-conf ctx)) ,gtargs ,body))))

   (define (j2s-type-scheme p)
      (let ((a (j2s-scheme p mode return ctx)))
	 (with-access::J2SDecl p (vtype)
	    (type-ident a vtype (context-conf ctx)))))
		    
   (define (fixarg-lambda fun id body)
      (with-access::J2SFun fun (idgen idthis thisp params rtype loc)
	 (let ((args (map j2s-type-scheme params)))
	    (lambda-or-labels rtype idgen
	       (type-this idthis thisp)
	       (unless (isa? fun J2SArrow) id) args body loc))))
   
   (define (rest-lambda fun id body)
      (with-access::J2SFun fun (idgen idthis thisp params rtype loc)
	 (let ((args (j2s-scheme params mode return ctx)))
	    (lambda-or-labels rtype idgen
	       (type-this idthis thisp)
	       (unless (isa? fun J2SArrow) id) args body loc))))
   
   (define (normal-vararg-lambda fun id body)
      ;; normal mode: arguments is an alias
      (let ((id (or id (gensym 'fun))))
	 (with-access::J2SFun fun (idgen idthis thisp rtype vararg argumentsp loc)
	    (let ((rest (if (isa? argumentsp J2SDeclArguments)
			    (with-access::J2SDeclArguments argumentsp (argid)
			       argid)
			    (gensym 'arguments))))
	       (lambda-or-labels rtype idgen
		  (type-this idthis thisp)
		  (unless (isa? fun J2SArrow) id)
		  (if (and (eq? vararg 'arguments)
			   (context-get ctx :optim-arguments))
		      (list rest)
		      rest)
		  (jsfun-normal-vararg-body fun body id rest ctx)
		  loc)))))
   
   (define (strict-vararg-lambda fun id body)
      ;; strict mode: arguments is initialized on entrance
      (with-access::J2SFun fun (idgen idthis thisp rtype vararg argumentsp loc)
	 (let ((rest (if (isa? argumentsp J2SDeclArguments)
			 (with-access::J2SDeclArguments argumentsp (argid)
			    argid)
			 (gensym 'arguments))))
	    (lambda-or-labels rtype idgen (type-this idthis thisp)
	       (unless (isa? fun J2SArrow) id)
	       (if (and (eq? vararg 'arguments)
			(context-get ctx :optim-arguments))
		   (list rest)
		   rest)
	       (jsfun-strict-vararg-body fun body id rest ctx)
	       loc))))

   (with-access::J2SFun this (loc vararg mode params generator name ismethodof)
      (let* ((id (or (j2sfun-id this)
		     (and (context-get ctx :function-nice-name #f)
			  name)))
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
;*    jsfun->lambda ...                                                */
;*---------------------------------------------------------------------*/
(define (jsfun->lambda this::J2SFun mode return ctx::struct proto ctor::bool)

   (define (generator-body body)
      `(letrec ((%gen (js-make-generator (lambda (%v %e) ,body) ,proto %this)))
	  %gen))

   (define (this-body thisp body mode)
      (let ((ctx (new-compiler-context ctx optim-initseq: (not ctor))))
	 (if (and thisp (context-get ctx optim-this: #f))
	     (cond
		((or (not (j2s-this-cache? thisp)) (not ctor))
		 (flatten-stmt (j2s-scheme body mode return ctx)))
		((eq? (with-access::J2SDecl thisp (vtype) vtype) 'object)
		 (with-access::J2SDecl thisp (vtype)
		    (set! vtype 'this))
		 (let ((stmt (j2s-scheme body mode return ctx)))
		    `(with-access::JsObject this (cmap elements)
			(let ((%thismap cmap)
			      (%thiselements elements))
			   ,(flatten-stmt stmt)))))
		(else
		 (with-access::J2SDecl thisp (vtype)
		    (set! vtype 'this))
		 (let ((stmt (j2s-scheme body mode return ctx)))
		    `(let (%thismap %thiselements)
			(unless (eq? this (js-undefined))
			   (with-access::JsObject this (cmap elements)
			      (set! %thismap cmap)
			      (set! %thiselements elements)))
			,(flatten-stmt stmt)))))
	     (flatten-stmt (j2s-scheme body mode return ctx)))))

   (with-access::J2SFun this (loc body need-bind-exit-return vararg mode params generator thisp)
      (let ((body (cond
		     (generator
		      (with-access::J2SNode body (loc)
			 (epairify loc
			    (generator-body
			       (this-body thisp body mode)))))
		     (need-bind-exit-return
		      (with-access::J2SNode body (loc)
			 (epairify loc
			    (return-body
			       (this-body thisp body mode)))))
		     (else
		      (let ((bd (this-body thisp body mode)))
			 (with-access::J2SNode body (loc)
			    (epairify loc
			       (if (pair? bd) bd `(begin ,bd)))))))))
	 (jsfun->lambda/body this mode return ctx body))))

;*---------------------------------------------------------------------*/
;*    j2sfun->ctor ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2sfun->ctor this::J2SFun mode return ctx decl::J2SDecl)
   
   (define (object-alloc this::J2SFun)
      (with-access::J2SFun this (body expr new-target)
	 (let ((f (j2s-decl-scheme-id decl)))
	    (cond
	       (new-target
		`(js-object-alloc/new-target %this ,f))
	       ((cancall? body)
		`(js-object-alloc %this ,f))
	       (else
		`(js-object-alloc-fast %this ,f))))))
   
   (with-access::J2SFun this (loc body vararg mode params generator thisp)
      (with-access::J2SDecl thisp (id vtype itype)
	 (set! vtype 'object)
	 (set! itype 'object)
	 (let ((nfun (duplicate::J2SFun this
			(rtype 'object)
			(idthis #f)
			(thisp #f)))
	       (id (j2s-decl-scheme-id thisp))
	       (body (let ((ctx (new-compiler-context ctx optim-initseq: #t)))
			`(let ((,id ,(object-alloc this)))
			    ,(j2s-scheme (ctor-body! body)
				mode return ctx)
			    ,id)))
	       (proto (j2s-declfun-prototype decl ctx)))
	    (jsfun->lambda/body nfun mode return ctx body)))))

;*---------------------------------------------------------------------*/
;*    return-body ...                                                  */
;*---------------------------------------------------------------------*/
(define (return-body body)
   `(bind-exit (%return) ,(flatten-stmt body)))

;*---------------------------------------------------------------------*/
;*    jsfun-strict-vararg-body ...                                     */
;*---------------------------------------------------------------------*/
(define (jsfun-strict-vararg-body this::J2SFun body id rest ctx)
   
   (define (optim-arguments-prelude argumentsp params body)
      (with-access::J2SDeclArguments argumentsp (alloc-policy argid mode)
	 `(let ((arguments ,(if (eq? alloc-policy 'lazy)
				`',mode
				`(js-strict-arguments-vector %this ,rest))))
	     (let* ((%len (vector-length ,argid))
		    ,@(map (lambda (p i)
			      (list (j2s-decl-scheme-id p)
				 `(if (<fx ,i %len)
				      (vector-ref ,argid ,i)
				      (js-undefined))))
			 params (iota (length params))))
		,body))))
   
   (define (regular-arguments-prelude argumentsp params body)
      `(let ((arguments (js-strict-arguments %this ,rest)))
	  ,@(if (pair? params)
		(map (lambda (param)
			(with-access::J2SDecl param (loc)
			   (epairify loc
			      `(define ,(j2s-decl-scheme-id param)
				  (js-undefined)))))
		   params)
		'())
	  ,@(if (pair? params)
		`((when (pair? ,rest)
		     (set! ,(j2s-decl-scheme-id (car params)) (car ,rest))
		     ,(let loop ((params (cdr params)))
			 (if (null? params)
			     #unspecified
			     `(when (pair? (cdr ,rest))
				 (set! ,rest (cdr ,rest))
				 (set! ,(j2s-decl-scheme-id (car params))
				    (car ,rest))
				 ,(loop (cdr params)))))))
		'())
	  ,body))
   
   (with-access::J2SFun this (params argumentsp)
      (if (context-get ctx :optim-arguments)
	  (optim-arguments-prelude argumentsp params body)
	  (regular-arguments-prelude argumentsp params body))))
   
;*---------------------------------------------------------------------*/
;*    j2sfun->scheme ...                                               */
;*---------------------------------------------------------------------*/
(define (j2sfun->scheme this::J2SFun tmp tmpm mode return ctx)

   (define (allocator this::J2SFun)
      (with-access::J2SFun this (new-target)
	 (cond
	    ((isa? this J2SSvc) 'js-not-a-constructor-alloc)
	    (new-target 'js-object-alloc/new-target)
	    (else 'js-object-alloc-lazy))))

   (with-access::J2SFun this (loc name params mode vararg mode generator
				constrsize method new-target type)
      (let* ((lparams (length params))
	     (len (if (eq? vararg 'rest) (-fx lparams 1) lparams))
	     (arity (j2s-function-arity this ctx))
	     (src (j2s-function-src loc this ctx))
	     (prototype (j2s-fun-prototype this))
	     (__proto__ (j2s-fun-__proto__ this))
	     (alloc (allocator this)))
	 (epairify-deep loc
	    (cond
	       ((eq? type 'procedure)
		(jsfun->lambda this mode return ctx
		   (j2s-fun-prototype this) #f))
	       ((and (isa? this J2SArrow)
		     (not (context-get ctx :profile-call #f)))
		`(,(if (eq? mode 'hopscript)
		       'js-make-procedure-hopscript
		       'js-make-procedure)
		  %this
		  ,(jsfun->lambda this mode return ctx
		      (j2s-fun-prototype this) #f)
		  ,arity))
	       ((and (or src prototype __proto__ method new-target)
		     (memq mode '(strict hopscript))
		     (not prototype)
		     (not __proto__))
		`(,(if (eq? alloc 'js-object-alloc-lazy)
		       'js-make-function-strict-lazy
		       'js-make-function-strict)
		  %this ,tmp ,len
		  ,(& name (context-program ctx))
		  :src ,src
		  :constrsize ,constrsize
		  :method ,(or tmpm
			       (and method (jsfun->lambda method mode return ctx #f #f))
			       tmp)
		  :arity ,arity
		  ,@(if (eq? alloc 'js-object-alloc-lazy)
			'()
			`(:alloc ,alloc))))
	       ((or src prototype __proto__ method new-target)
		`(js-make-function %this ,tmp ,len
		    ,(& name (context-program ctx))
		    :src ,src
		    :arity ,arity
		    :prototype ,prototype
		    :__proto__ ,__proto__
		    :strict ',mode
		    :alloc ,alloc
		    :constrsize ,constrsize
		    :method ,(or tmpm (and method (jsfun->lambda method mode return ctx #f #f)))))
	       ((and (eq? vararg 'arguments)
		     (memq mode '(strict hopscript)))
		`(,(if (eq? alloc 'js-object-alloc-lazy)
		       'js-make-function-strict-lazy
		       'js-make-function-strict)
		  %this ,tmp ,len
		  ,(& name (context-program ctx))
		  :constrsize ,constrsize
		  :method ,tmp
		  :arity ,arity
		  ,@(if (eq? alloc 'js-object-alloc-lazy)
			`()
			`(:alloc ,alloc))))
	       ((eq? vararg 'arguments)
		`(js-make-function %this ,tmp ,len
		    ,(& name (context-program ctx))
		    :arity ,arity ,
		    :strict ',mode
		    :alloc ,alloc
		    :constrsize ,constrsize))
	       (else
		`(js-make-function-simple %this ,tmp ,len
		    ,(& name (context-program ctx))
		    ,arity ',mode ,constrsize)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SFun mode return ctx)
   (with-access::J2SFun this (loc name params mode vararg generator method type decl)
      (let* ((id (j2sfun-id this))
	     (tmp (if (eq? name '||)
		      (gensym (format "~a:~a-"
				 (basename (cadr loc)) (caddr loc)))
		      name))
	     (tmpm (when method (symbol-append name '&)))
	     (arity (if vararg -1 (+fx 1 (length params))))
	     (fundef (cond
			((eq? type 'procedure)
			 (j2sfun->scheme this tmp tmpm mode return ctx))
			(generator
			 (let ((tmp2 (gensym id)))
			    `(letrec* (,@(if method
					     `((,tmpm ,(jsfun->lambda method mode return ctx #f #f)))
					     '())
					 (,tmp ,(jsfun->lambda this mode return ctx
						   `(js-get ,tmp2
						       ,(& "prototype" (context-program ctx))
						       %this)
						   #f))
					 (,tmp2 ,(j2sfun->scheme this tmp tmpm mode return ctx)))
				,tmp2)))
			(else
			 `(let (,@(if method
				      `((,tmpm ,(jsfun->lambda method mode return ctx #f #f)))
				      '())
				  (,tmp ,(jsfun->lambda this mode return ctx
					    (j2s-fun-prototype this) #f)))
			     ,(j2sfun->scheme this tmp tmpm mode return ctx))))))
	 (epairify-deep loc
	    (if id
		(let ((scmid (j2s-decl-scheme-id decl)))
		   `(let ((,scmid (js-undefined)))
		       (set! ,scmid ,fundef)
		       ,scmid))
		fundef)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SMethod ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SMethod mode return ctx)
   (with-access::J2SMethod this (function)
      (j2s-scheme function mode return ctx)))

;*---------------------------------------------------------------------*/
;*    jssvc->scheme ::J2SSvc ...                                       */
;*---------------------------------------------------------------------*/
(define (jssvc->scheme this::J2SSvc lam id scmid mode return ctx)

   (define (j2sscheme-service this tmp scmid path args arity mode return)
      
      (define (jscript-funcall init)
	 ;; see runtime/service_expd.sch
	 "HopService( ~s, ~s )")

      (define (service-debug name loc body)
	 (if (>fx (context-get ctx :debug 0) 0)
	     `(lambda () (js-service/debug ',name ',loc ,body))
	     body))

      (define (service-body this::J2SSvc)
	 (with-access::J2SSvc this (loc body need-bind-exit-return name mode)
	    (if need-bind-exit-return
		(with-access::J2SNode body (loc)
		   (epairify loc
		      (return-body
			 (j2s-scheme body mode return ctx))))
		(flatten-stmt
		   (j2s-scheme body mode return ctx)))))

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
				loc ctx)
			    ,fname ,loc))
		       (else
			`(js-new %this js-type-error
			    ,(j2s-jsstring
				(format "wrong service call \"~s\"" name)
				loc ctx))))))))
      
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
			       ((and (js-object? (car args))
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
	       (j2s-scheme val mode return ctx)))))
   
   (define (svc-proc-entry this)
      (with-access::J2SSvc this (name params loc path mode)
	 (let ((params (j2s-scheme params mode return ctx))
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
	 (let ((args (j2s-scheme params mode return ctx))
	       (ctx (new-compiler-context ctx :force-location #t)))
	    (match-case lam
	       ((labels (and ?bindings ((?id . ?-))) ?id)
		`(labels ,bindings
		    (js-create-service %this
		       ,(j2sfun->scheme this id #f mode return ctx)
		       ,(when (symbol? path) (symbol->string path))
		       ',loc
		       ,register ,import (js-current-worker))))
	       (else
		`(js-create-service %this
		   ,(j2sfun->scheme this lam #f mode return ctx)
		   ,(when (symbol? path) (symbol->string path))
		   ',loc
		   ,register ,import (js-current-worker)))))))

   (with-access::J2SSvc this (loc)
      (if (context-get ctx dsssl: #f) 
	  (epairify-deep loc (svc-proc-entry this))
	  (epairify-deep loc (svc->scheme this)))))
	   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSvc ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSvc mode return ctx)
   (with-access::J2SSvc this (loc)
      (let ((lam (jsfun->lambda this mode return ctx
		    (j2s-fun-prototype this) #f)))
	 (epairify loc
	    (jssvc->scheme this lam #f #f mode return ctx)))))

;*---------------------------------------------------------------------*/
;*    jsfun-normal-vararg-body ...                                     */
;*---------------------------------------------------------------------*/
(define (jsfun-normal-vararg-body this::J2SFun body id rest ctx)
   
   (define (init-argument val indx)
      `(js-arguments-define-own-property arguments ,indx
	  (instantiate::JsValueDescriptor
	     (name (js-integer-name->jsstring ,indx))
	     (value ,val)
	     (writable #t)
	     (configurable #t)
	     (enumerable #t))))
   
   (define (init-alias-argument argument init indx)
      (let ((id (j2s-decl-scheme-id argument)))
	 `(begin
	     ,@(if init `((set! ,id ,init)) '())
	     (js-arguments-define-own-property arguments ,indx
		(instantiate::JsAccessorDescriptor
		   (name (js-integer-name->jsstring ,indx))
		   (get (js-make-function %this
			   (lambda (%) ,id) 0
			   ,(& "get" (context-program ctx))))
		   (set (js-make-function %this
			   (lambda (% %v) (set! ,id %v)) 1
			   ,(& "set" (context-program ctx))))
		   (%get (lambda (%) ,id))
		   (%set (lambda (% %v) (set! ,id %v)))
		   (configurable #t)
		   (enumerable #t))))))
   
   (define (optim-arguments-prelude argumentsp params body loc)
      (with-access::J2SDeclArguments argumentsp (argid alloc-policy mode)
	 (if (eq? alloc-policy 'lazy)
	     `(let* ((%len (vector-length ,argid))
		     (arguments ',mode)
		     ,@(map (lambda (p i)
			       (list (j2s-decl-scheme-id p)
				  `(if (<fx ,i %len)
				       (vector-ref ,argid ,i)
				       (js-undefined))))
			  params (iota (length params))))
		 ,body)
	     `(let* ((%len (vector-length ,argid))
		     (arguments (js-arguments %this ,argid))
		     ,@(map (lambda (p i)
			       (list (j2s-decl-scheme-id p)
				  `(if (<fx ,i %len)
				       (vector-ref ,argid ,i)
				       (js-undefined))))
			  params (iota (length params))))
		 ,@(map (lambda (param i)
			   `(when (<fx ,i %len)
			       ,(init-alias-argument param #f i)))
		      params (iota (length params)))
		 (let loop ((,rest ,rest)
			    (%i ,(length params)))
		    (when (pair? ,rest)
		       ,(init-argument `(car ,rest) '%i)
		       (loop (cdr ,rest) (+fx %i 1))))
		 (js-bind! %this arguments ,(& "callee" (context-program ctx))
		    :value (js-make-function %this
			      ,(j2s-profile-id (j2s-fast-id id) loc ctx)
			      0 ,(& id (context-program ctx)))
		    :enumerable #f)
		 ,body))))
	     
   (define (regular-arguments-prelude argumentsp params body loc)
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
		  ,(init-alias-argument (car params) `(car ,rest) 0)
		  (set! ,rest (cdr ,rest))
		  ,(let loop ((params (cdr params))
			      (i 1))
		      (if (null? params)
			  #unspecified
			  `(when (pair? ,rest)
			      ,(init-alias-argument (car params)
				  `(car ,rest) i)
			      (set! ,rest (cdr ,rest))
			      ,(loop (cdr params) (+fx i 1)))))))
	  (let loop ((,rest ,rest)
		     (%i ,(length params)))
	     (when (pair? ,rest)
		,(init-argument `(car ,rest) '%i)
		(loop (cdr ,rest) (+fx %i 1))))
	  (js-bind! %this arguments ,(& "callee" (context-program ctx))
	     :value (js-make-function %this
		       ,(j2s-profile-id (j2s-fast-id id) loc ctx) 0
		       ,(& id (context-program ctx)))
	     :enumerable #f)
	  ,body))
   
   (with-access::J2SFun this (params argumentsp loc)
      (if (context-get ctx :optim-arguments)
	  (optim-arguments-prelude argumentsp params body loc)
	  (regular-arguments-prelude argumentsp params body loc))))

;*---------------------------------------------------------------------*/
;*    j2s-function-src ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-function-src loc val::J2SFun ctx)
   (with-access::J2SFun val (src body)
      (when src
	 (match-case loc
	    ((at ?path ?start)
	     (let ((m (config-get-mmap (context-conf ctx) path)))
		`'(,loc . ,(when (mmap? m)
			      (with-access::J2SBlock body (endloc)
				 (match-case endloc
				    ((at ?file ?end)
				     (when (and (string=? (mmap-name m) file)
						(string=? path file)
						(<fx start end)
						(>=fx start 0)
						(<fx (+fx 1 end) (mmap-length m)))
					(mmap-substring m
					    (fixnum->elong start)
					    (+fx 1 (fixnum->elong end)))))))))))
	    (else
	     (error "j2s-function-src" "bad location" loc))))))

;*---------------------------------------------------------------------*/
;*    j2s-fun-prototype ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-fun-prototype this)
   (with-access::J2SFun this (generator)
      (if generator
	  `(with-access::JsGlobalObject %this (js-generator-prototype)
	      (instantiateJsObject
		 (__proto__ js-generator-prototype)
		 (elements ($create-vector 1))))
	  #f)))
   
;*---------------------------------------------------------------------*/
;*    j2s-fun-__proto__ ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-fun-__proto__ this)
   (with-access::J2SFun this (generator)
      (if generator
	  `(with-access::JsGlobalObject %this (js-generatorfunction-prototype)
	      js-generatorfunction-prototype)
	  #f)))
   
;*---------------------------------------------------------------------*/
;*    j2s-declfun-prototype ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-declfun-prototype this::J2SDeclFun ctx)
   (with-access::J2SDeclFun this (parent)
      (let* ((decl (if parent parent this))
	     (scmid (j2s-decl-scheme-id decl)))
	 `(js-get ,scmid ,(& "prototype" (context-program ctx)) %this))))

;*---------------------------------------------------------------------*/
;*    j2s-fun-no-closure? ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-fun-no-closure? this::J2SDeclFun)
   (with-access::J2SDeclFun this (val id)
      (when (decl-ronly? this)
	 (when (isa? val J2SFun)
	    (with-access::J2SFun val (generator type)
	       (unless generator
		  (not (decl-usage-has? this '(new ref get set instanceof)))))))))

;*---------------------------------------------------------------------*/
;*    ctor-body! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (ctor-body! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    ctor-body! ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (ctor-body! this::J2SReturn)
   (with-access::J2SReturn this (tail exit expr loc)
      (cond
	 ((or tail exit)
	  (J2SStmtExpr (ctor-body! expr)))
	 ((isa? expr J2SUndefined)
	  (J2SNop))
	 (else
	  (J2SStmtExpr expr)))))

;*---------------------------------------------------------------------*/
;*    ctor-body! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (ctor-body! this::J2SFun)
   this)

;*---------------------------------------------------------------------*/
;*    ctor-body! ::J2SMeta ...                                         */
;*    -------------------------------------------------------------    */
;*    Normal methods start testing THIS type (unless in strict         */
;*    mode). This test is useless for constructor. The                 */
;*    CTOR-BODY! ::J2SIF method removes it.                            */
;*    -------------------------------------------------------------    */
;*    See this.scm for the form of the META statement.                 */
;*---------------------------------------------------------------------*/
(define-walk-method (ctor-body! this::J2SMeta)
   (with-access::J2SMeta this (meta stmt loc)
      (if (eq? meta 'unstrict-this)
	  (ctor-body! (J2SStmtExpr (J2SUndefined)))
	  (call-next-method))))
