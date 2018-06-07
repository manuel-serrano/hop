;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-program.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 18 08:03:25 2018                          */
;*    Last change :  Thu Jun  7 08:28:50 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Program node compilation                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-program

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SProgram ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SProgram mode return conf)
   
   (define (j2s-master-module module scmcnsts body)
      (with-access::J2SProgram this (mode pcache-size call-size globals)
	 (list module
	    `(%define-pcache ,pcache-size)
	    `(define %pcache
		(js-make-pcache ,pcache-size ,(config-get conf :filename)))
	    '(define %source (or (the-loading-file) "/"))
	    '(define %resource (dirname %source))
	     (when (config-get conf :profile-call #f)
		`(define %call-log (make-vector ,call-size 0)))
	     (when (config-get conf :profile-call #f)
		`(define %call-locations ',(call-locations this)))
	    `(define (hopscript %this this %scope %module)
		(define %worker (js-current-worker))
		(define %cnsts ,scmcnsts)
		,@globals
		,@(exit-body body conf))
	    ;; for dynamic loading
	    'hopscript)))

   (define (j2s-slave-module module scmcnsts body)
      (with-access::J2SProgram this (mode pcache-size call-size globals)
	 (list (append module `((option (register-srfi! 'hopjs-worker-slave))))
	    '(define %source (or (the-loading-file) "/"))
	    '(define %resource (dirname %source))
	    `(define (hopscript %this this %scope %module)
		(define %pcache
		   (js-make-pcache ,pcache-size ,(config-get conf :filename)))
		,@(if (config-get conf :profile-call #f)
		      `((define %call-log (make-vector ,call-size 0))
			(define %call-locations ',(call-locations this)))
		      '())
		(define %worker (js-current-worker))
		(define %cnsts ,scmcnsts)
		,@globals
		,@(exit-body body conf))
	    ;; for dynamic loading
	    'hopscript)))

   (define (j2s-module module scmcnsts body)
      (if (config-get conf :worker-slave)
	  (j2s-slave-module module scmcnsts body)
	  (j2s-master-module module scmcnsts body)))

   (define (j2s-main-module/workers name scmcnsts body)
      (let ((module `(module ,(string->symbol name)
			(eval (library hop)
			   (library hopscript)
			   (library nodejs))
			(library hop hopscript nodejs)
			(cond-expand (enable-libuv (library libuv)))
			(main main))))
	 (with-access::J2SProgram this (mode pcache-size call-size %this path globals)
	    (list module
	       `(%define-pcache ,pcache-size)
	       '(hop-sofile-compile-policy-set! 'static)
	       `(define %pcache
		   (js-make-pcache ,pcache-size ,(config-get conf :filename)))
		(when (config-get conf :profile-call #f)
		   `(define %call-log (make-vector ,call-size 0)))
		(when (config-get conf :profile-call #f)
		   `(define %call-locations ',(call-locations this)))
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
			 (define %cnsts ,scmcnsts)
			 ,@globals
			 ,@(exit-body body conf)))
		   ,(profilers conf)
		   (thread-join! (thread-start-joinable! %worker)))))))

   
   
   (with-access::J2SProgram this (module main nodes headers decls
					 mode name pcache-size call-size
					 cnsts globals)
      (let ((scmheaders (j2s-scheme headers mode return conf))
	    (scmdecls (j2s-scheme decls mode return conf))
	    (scmnodes (j2s-scheme nodes mode return conf))
	    (scmcnsts (%cnsts cnsts mode return conf)))
	 (if (and main (not (config-get conf :worker #t)))
	     (j2s-main-sans-worker-module this name
		scmcnsts
		(flatten-nodes (append scmheaders scmdecls))
		(flatten-nodes scmnodes)
		conf)
	     (let ((body (flatten-nodes (append scmheaders scmdecls scmnodes))))
		(cond
		   (module
		    ;; a module whose declaration is in the source
		    (j2s-module module scmcnsts body))
		   ((not name)
		    ;; a mere expression
		    `(lambda (%this this %scope %module)
			(%define-pcache ,pcache-size)	       
			(define %pcache
			   (js-make-pcache ,pcache-size ,(config-get conf :filename)))
			,@(if (config-get conf :profile-call #f)
			      `((define %call-log (make-vector ,call-size 0))
				(define %call-locations ',(call-locations this)))
			      '())
			(define %worker (js-current-worker))
			(define %source (or (the-loading-file) "/"))
			(define %resource (dirname %source))
			(define %cnsts ,scmcnsts)
			,@globals
			,@(exit-body body conf)))
		   (main
		    ;; generate a main hopscript module 
		    (j2s-main-module/workers name scmcnsts body))
		   (else
		    ;; generate the module clause
		    (let ((module `(module ,(string->symbol name)
				      (library hop hopscript js2scheme nodejs)
				      (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))))
		       (j2s-module module scmcnsts body)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-main-sans-worker-module ...                                  */
;*---------------------------------------------------------------------*/
(define (j2s-main-sans-worker-module this name scmcnsts toplevel body conf)
   (let ((module `(module ,(string->symbol name)
		     (eval (library hop)
			(library hopscript)
			(library nodejs))
		     (library hop hopscript nodejs)
		     (cond-expand (enable-libuv (library libuv)))
		     (main main))))
      (with-access::J2SProgram this (mode pcache-size call-size %this path globals)
	 `(,module (%define-pcache ,pcache-size)
	     (define %pcache
		(js-make-pcache ,pcache-size ,(config-get conf :filename)))
	     ,@(if (config-get conf :profile-call #f)
		   `((define %call-log (make-vector ,call-size 0))
		     (define %call-locations ',(call-locations this)))
		   '())
	     (hop-sofile-compile-policy-set! 'static)
	     (hopjs-standalone-set! #t)
	     (define %this (nodejs-new-global-object))
	     (define %source ,path)
	     (define %resource (dirname %source))
	     (define %scope (nodejs-new-scope-object %this))
	     (define this
		(with-access::JsGlobalObject %this (js-object)
		   (js-new0 %this js-object)))
	     (define %worker
		(js-init-main-worker! %this #f nodejs-new-global-object))
	     (define %module
		(nodejs-module ,(basename path) ,path %worker %this))
	     (define %cnsts ,scmcnsts)
	     ,@globals
	     ,@toplevel
	     (define (main args)
		,(profilers conf)
		(hopscript-install-expanders!)
		(bigloo-library-path-set! ',(bigloo-library-path))
		(set! !process (nodejs-process %worker %this))
		,@(exit-body body conf))))))

;*---------------------------------------------------------------------*/
;*    profilers ...                                                    */
;*---------------------------------------------------------------------*/
(define (profilers conf)
   (when (config-get conf :profile #f)
      `(js-profile-init ',(filter-config conf)
	  ,(if (config-get conf :profile-call #f)
	       '(vector %source %call-log %call-locations)
	       #f))))

;*---------------------------------------------------------------------*/
;*    filter-config ...                                                */
;*---------------------------------------------------------------------*/
(define (filter-config conf)
   (let loop ((conf conf)
	      (res '()))
      (cond
	 ((null? conf)
	  (reverse! res))
	 ((or (number? (cadr conf)) (string? (cadr conf)) (boolean? (cadr conf)))
	  (loop (cddr conf) (cons* (cadr conf) (car conf) res)))
	 (else
	  (loop (cddr conf) res)))))
   
;*---------------------------------------------------------------------*/
;*    exit-body ...                                                    */
;*---------------------------------------------------------------------*/
(define (exit-body body conf)
   (if (config-get conf :return-as-exit)
       `((bind-exit (%jsexit) ,@body))
       body))

;*---------------------------------------------------------------------*/
;*    %cnsts ...                                                       */
;*---------------------------------------------------------------------*/
(define (%cnsts cnsts mode return conf)
   
   (define (%cnsts-debug cnsts)
      `(vector
	  ,@(map (lambda (n)
		    (let ((s (j2s-scheme n mode return conf)))
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
		(vector (if inline 5 4) val flags loc)))
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
   
   ;; this must be executed after the code is compiled as this
   ;; compilation might change or add new constants.
   (if (>fx (bigloo-debug) 0)
       (%cnsts-debug cnsts)
       (%cnsts-intext cnsts)))

;*---------------------------------------------------------------------*/
;*    call-locations ...                                               */
;*---------------------------------------------------------------------*/
(define (call-locations this::J2SProgram)
   (with-access::J2SProgram this (call-size)
      (collect-call-locations this (make-vector call-size -1))))

;*---------------------------------------------------------------------*/
;*    collect-call-locations ::J2SNode ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-call-locations this::J2SNode vec)
   (call-default-walker)
   vec)

;*---------------------------------------------------------------------*/
;*    collect-call-locations ::J2SCall ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-call-locations this::J2SCall vec)
   (with-access::J2SCall this (profid loc)
      (when (>fx profid 0)
	 (match-case loc
	    ((at ?- ?point)
	     (vector-set! vec profid point)))))
   vec)
