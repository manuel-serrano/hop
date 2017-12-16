;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-fun.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:04:57 2017                          */
;*    Last change :  Sat Dec 16 06:24:08 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript functions                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-fun

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils)

   (export (jsfun->lambda ::J2SFun mode return conf proto ::bool)
	   (j2sfun->scheme ::J2SFun tmp ctor mode return conf)
	   (j2s-fun-prototype ::J2SFun)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclFun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclFun mode return conf hint)
   
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
		   `(js-make-function %this ,fastid
		       ,len ,(symbol->string! id)
		       :src ,src
		       :rest ,(eq? vararg 'rest)
		       :arity ,arity
		       :minlen ,minlen
		       :strict ',mode
		       :alloc js-object-alloc
		       :prototype ,(j2s-fun-prototype val)
		       :__proto__ ,(j2s-fun-__proto__ val)
		       :construct ,fastid
		       :constrsize ,constrsize))
		  (src
		   `(js-make-function %this ,fastid
		       ,len ,(symbol->string! id)
		       :src ,src
		       :rest ,(eq? vararg 'rest)
		       :arity ,arity
		       :minlen ,minlen
		       :strict ',mode
		       :alloc js-object-alloc
		       :construct ,fastid
		       :constrsize ,constrsize))
		  ((eq? vararg 'arguments)
		   `(js-make-function %this ,fastid
		       ,len ,(symbol->string! id)
		       :rest ,(eq? vararg 'rest)
		       :arity ,arity
		       :minlen ,minlen
		       :strict ',mode 
		       :constrsize ,constrsize))
		  (else
		   `(js-make-function-simple %this ,fastid
		       ,len ,(symbol->string! id)
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
				  ,(if (js-need-global? this scope mode)
				       `(js-bind! %this ,scope ',id
					   :configurable #f
					   :value ,(make-function this))
				       (make-function this)))))))
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
(define-method (j2s-scheme this::J2SDeclSvc mode return conf hint)
   (with-access::J2SDeclSvc this (loc id val scope)
      (let ((scmid (j2s-decl-scheme-id this))
	    (fastid (j2s-fast-id id)))
	 (epairify-deep loc
	    (if (and (memq scope '(global %scope))
		     (js-need-global? this scope mode))
		`(begin
		    (define ,fastid
		       ,(jssvc->scheme val id scmid mode return conf))
		    (define ,scmid
		       (js-bind! %this ,scope ',id
			  :configurable #f
			  :writable #f
			  :value ,fastid)))
		`(begin
		    (define ,fastid ,(jssvc->scheme val id scmid mode return conf))
		    (define ,scmid ,fastid)))))))

;*---------------------------------------------------------------------*/
;*    jsfun->lambda ...                                                */
;*---------------------------------------------------------------------*/
(define (jsfun->lambda this::J2SFun mode return conf proto ctor-only::bool)

   (define (lambda-or-labels rtype %gen this id args body)
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
	     `(,(type-ident 'lambda rtype) ,gtargs ,body))))

   (define (j2s-type-scheme p)
      (let ((a (j2s-scheme p mode return conf '())))
	 (with-access::J2SDecl p (vtype)
	    (type-ident a vtype))))
		    
   (define (fixarg-lambda fun id body)
      (with-access::J2SFun fun (idgen idthis params rtype)
	 (let ((args (map j2s-type-scheme params)))
	    (lambda-or-labels rtype idgen idthis id args body))))
   
   (define (rest-lambda fun id body)
      (with-access::J2SFun fun (idgen idthis params rtype)
	 (let ((args (j2s-scheme params mode return conf '())))
	    (lambda-or-labels rtype idgen idthis id args body))))
   
   (define (normal-vararg-lambda fun id body)
      ;; normal mode: arguments is an alias
      (let ((id (or id (gensym 'fun)))
	    (rest (gensym 'arguments)))
	 (with-access::J2SFun fun (idgen idthis rtype)
	    (lambda-or-labels rtype idgen idthis id rest
	       (jsfun-normal-vararg-body fun body id rest)))))
   
   (define (strict-vararg-lambda fun id body)
      ;; strict mode: arguments is initialized on entrance
      (let ((rest (gensym 'arguments)))
	 (with-access::J2SFun fun (idgen idthis rtype)
	    (lambda-or-labels rtype idgen idthis id rest
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
	     ((or (not (j2s-this-cache? thisp)) (not ctor-only))
	      (flatten-stmt (j2s-scheme body mode return conf '())))
	     ((eq? (with-access::J2SDecl thisp (vtype) vtype) 'object)
	      (with-access::J2SDecl thisp (vtype)
		 (set! vtype 'this))
	      (let ((stmt (j2s-scheme body mode return conf '())))
		 `(with-access::JsObject this (cmap elements)
		     (let ((%thismap cmap)
			   (%thiselements elements))
			,(flatten-stmt stmt)))))
	     (else
	      (with-access::J2SDecl thisp (vtype)
		 (set! vtype 'this))
	      (let ((stmt (j2s-scheme body mode return conf '())))
		 `(let (%thismap %thiselements)
		     (unless (eq? this (js-undefined))
			(with-access::JsObject this (cmap elements)
			   (set! %thismap cmap)
			   (set! %thiselements elements)))
		     ,(flatten-stmt stmt)))))
	  (flatten-stmt (j2s-scheme body mode return conf '()))))

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
;*    return-body ...                                                  */
;*---------------------------------------------------------------------*/
(define (return-body body)
   `(bind-exit (%return) ,(flatten-stmt body)))

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
;*    j2sfun->scheme ...                                               */
;*---------------------------------------------------------------------*/
(define (j2sfun->scheme this::J2SFun tmp ctor mode return conf)
   (with-access::J2SFun this (loc name params mode vararg mode generator
				constrsize method)
      (let* ((id (j2sfun-id this))
	     (lparams (length params))
	     (len (if (eq? vararg 'rest) (-fx lparams 1) lparams))
	     (arity (if vararg -1 (+fx 1 (length params))))
	     (minlen (if (eq? mode 'hopscript) (j2s-minlen this) -1))
	     (src (j2s-function-src loc this conf))
	     (prototype (j2s-fun-prototype this))
	     (__proto__ (j2s-fun-__proto__ this)))
	 (epairify-deep loc
	    (cond
	       ((or src prototype __proto__ method)
		`(js-make-function %this ,tmp ,len
		    ,(symbol->string! (or name (j2s-decl-scheme-id id)))
		    :src ,src
		    :rest ,(eq? vararg 'rest)
		    :arity ,arity
		    :prototype ,prototype
		    :__proto__ ,__proto__
		    :strict ',mode
		    :minlen ,minlen
		    :alloc js-object-alloc
		    :construct ,ctor
		    :constrsize ,constrsize
		    :method ,(when method
				(jsfun->lambda method mode return conf #f #f))))
	       ((eq? vararg 'arguments)
		`(js-make-function %this ,tmp ,len
		    ,(symbol->string! (or name (j2s-decl-scheme-id id)))
		    :rest ,(eq? vararg 'rest)
		    :arity ,arity ,
		    :minlen minlen
		    :strict ',mode
		    :constrsize ,constrsize))
	       (else
		`(js-make-function-simple %this ,tmp ,len
		    ,(symbol->string! (or name (j2s-decl-scheme-id id)))
		    ,arity ,minlen ',mode ,(eq? vararg 'rest) ,constrsize)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SFun mode return conf hint)
   (with-access::J2SFun this (loc name params mode vararg generator method)
      (let* ((id (j2sfun-id this))
	     (tmp (gensym id))
	     (arity (if vararg -1 (+fx 1 (length params))))
	     (fundef (if generator
			 (let ((tmp2 (gensym id)))
			    `(letrec* ((,tmp ,(jsfun->lambda this mode return conf
						 `(js-get ,tmp2 'prototype %this)
						 #f))
				       (,tmp2 ,(j2sfun->scheme this tmp tmp mode return conf)))
				,tmp2))
			 `(let ((,tmp ,(jsfun->lambda this mode return conf
					  (j2s-fun-prototype this) #f)))
			     ,(j2sfun->scheme this tmp tmp mode return conf)))))
	 (epairify-deep loc
	    (if id
		(let ((scmid (j2s-scheme-id id '^)))
		   `(let ((,scmid (js-undefined)))
		       (set! ,scmid ,fundef)
		       ,scmid))
		fundef)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SMethod ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SMethod mode return conf hint)
   (with-access::J2SMethod this (function)
      (j2s-scheme function mode return conf hint)))

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
		       (j2s-fun-prototype this) #f))
	       (conf (cons* :force-location #t conf)))
	    (match-case lam
	       ((labels (and ?bindings ((?id . ?-))) ?id)
		`(labels ,bindings
		    (js-create-service %this
		       ,(j2sfun->scheme this id #f mode return conf)
		       ,(when (symbol? path) (symbol->string path))
		       ',loc
		       ,register ,import (js-current-worker))))
	       (else
		`(js-create-service %this
		   ,(j2sfun->scheme this lam #f mode return conf)
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
;*    j2s-function-src ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-function-src loc val::J2SFun conf)
   (with-access::J2SFun val (src body)
      (when src
	 (match-case loc
	    ((at ?path ?start)
	     (let ((m (config-get conf :mmap-src)))
		`'(,loc . ,(when (mmap? m)
			      (with-access::J2SBlock body (endloc)
				 (match-case endloc
				    ((at ?- ?end)
				     (mmap-substring m
					(fixnum->elong start)
					(+elong 1 (fixnum->elong end))))))))))
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
;*    j2s-declfun-prototype ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-declfun-prototype this::J2SDeclFun)
   (with-access::J2SDeclFun this (parent)
      (let* ((decl (if parent parent this))
	     (scmid (j2s-decl-scheme-id decl)))
	 `(js-get ,scmid 'prototype %this))))
