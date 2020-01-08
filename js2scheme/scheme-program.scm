;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-program.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 18 08:03:25 2018                          */
;*    Last change :  Wed Jan  8 16:58:47 2020 (serrano)                */
;*    Copyright   :  2018-20 Manuel Serrano                            */
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
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-utils
	   __js2scheme_checksum))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SProgram ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SProgram mode return conf)
   
   (define (j2s-master-module module cnsttable esexports esimports body)
      (with-access::J2SProgram this (mode pcache-size call-size globals cnsts loc)
	 (list module
	    ;; (&begin!) must not be a constant! (_do not_ use quote)
	    `(define __js_strings (&begin!))
	    `(%define-cnst-table ,(length cnsts))
	    `(%define-pcache ,pcache-size)
	    `(define %pcache
		(js-make-pcache-table ,pcache-size ,(config-get conf :filename)
		   ,@(if (config-get conf :profile-cache #f)
			 (list `',(j2s-profile-cache this conf))
			 '())))
	    '(define %source (or (the-loading-file) "/"))
	    '(define %resource (dirname %source))
	    (when (config-get conf :profile-call #f)
	       `(define %call-log (make-vector ,call-size #l0)))
	    (when (config-get conf :profile-cmap #f)
	       `(define %cmap-log (make-vector ,call-size '())))
	    (when (config-get conf :profile-call #f)
	       `(define %call-locations ',(call-locations this)))
	    (epairify-deep loc
	       `(define (hopscript %this this %scope %module)
		   ,@(filter fundef? globals)
		   ,@(filter fundef? body)
		   (define __js_strings (&init!))
		   (define js-string-names (js-get-js-string-names))
		   (define js-integer-names (js-get-js-integer-names))
		   (define %worker (js-current-worker))
		   (define %cnst-table ,cnsttable)
		   ,@esimports
		   ,esexports
		   ,@(filter nofundef? globals)
		   ,@(exit-body (filter nofundef? body) conf)))
	    '(&end!)
	    ;; for dynamic loading
	    'hopscript)))
   
   (define (j2s-slave-module module cnsttable esexports esimports body)
      (with-access::J2SProgram this (mode pcache-size call-size globals cnsts loc)
	 (list (append module `((option (register-srfi! 'hopjs-worker-slave))))
	    ;; (&begin!) must not be a constant! (_do not_ use quote)
	    `(define __js_strings (&begin!))
	    '(define %source (or (the-loading-file) "/"))
	    '(define %resource (dirname %source))
	    (epairify-deep loc
	       `(define (hopscript %this this %scope %module)
		   (define __js_strings (&init!))
		   ,@(filter fundef? globals)
		   ,@(filter fundef? body)
		   (define js-string-names (js-get-js-string-names))
		   (define js-integer-names (js-get-js-integer-names))
		   (define %pcache
		      (js-make-pcache-table ,pcache-size
			 ,(config-get conf :filename)
			 ,@(if (config-get conf :profile-cache #f)
			       (list `',(j2s-profile-cache this conf))
			       '())))
		   ,@(if (config-get conf :profile-call #f)
			 `((define %call-log (make-vector ,call-size #l0))
			   (define %call-locations ',(call-locations this)))
			 '())
		   ,@(if (config-get conf :profile-cmap #f)
			 `((define %cmap-log (make-vector ,call-size '())))
			 '())
		   (define %worker (js-current-worker))
		   (define %cnst-table ,cnsttable)
		   ,@esimports
		   ,esexports
		   ,@(filter nofundef? globals)
		   ,@(exit-body (filter nofundef? body) conf)))
	    '(&end!)
	    ;; for dynamic loading
	    'hopscript)))
   
   (define (j2s-module module cnsttable esexports esimports body)
      (if (config-get conf :worker-slave)
	  (j2s-slave-module module cnsttable esexports esimports body)
	  (j2s-master-module module cnsttable esexports esimports body)))
   
   (define (j2s-main-module/workers name cnsttable esexports esimports body)
      (with-access::J2SProgram this (mode pcache-size call-size path
				       globals cnsts loc)
	 (let* ((jsmod (js-module/main loc name))
		(jsthis `(with-access::JsGlobalObject %this (js-object)
			    (js-new0 %this js-object)))
		(thunk `(lambda ()
			   ,@(filter fundef? globals)
			   ,@(filter fundef? body)
			   (define _ (set! __js_strings (&init!)))
			   (define %cnst-table ,cnsttable)
			   (define %scope (nodejs-new-scope-object %this))
			   (define this ,jsthis)
			   ,@esimports
			   ,esexports
			   ,@(filter nofundef? globals)
			   ,@(exit-body (filter nofundef? body) conf))))
	    `(,jsmod
		;; (&begin!) must not be a constant! (_do not_ use quote)
		,`(define __js_strings (&begin!))
		(%define-cnst-table ,(length cnsts))
		(%define-pcache ,pcache-size)
		(hop-sofile-compile-policy-set! 'static)
		(define %pcache
		   (js-make-pcache-table ,pcache-size
		      ,(config-get conf :filename)
		      ,@(if (config-get conf :profile-cache #f)
			    (list `',(j2s-profile-cache this conf))
			    '())))
		,@(if (config-get conf :profile-call #f)
		      `((define %call-log (make-vector ,call-size #l0)))
		      '())
		,@(if (config-get conf :profile-cmap #f)
		      `((define %cmap-log (make-vector ,call-size '())))
		      '())
		,@(if (config-get conf :profile-call #f)
		      `((define %call-locations ',(call-locations this)))
		      '())
		(hopjs-standalone-set! #t)
		(define %source ,path)
		(define %resource (dirname %source))
		,@(if (config-get conf :libs-dir #f)
		      `((hop-sofile-directory-set! ,(config-get conf :libs-dir #f)))
		      '())
		(define (main args)
		   (bigloo-library-path-set! ',(bigloo-library-path))
		   (hop-port-set! -1)
		   (hop-ssl-port-set! -1)
		   (hopscript-install-expanders!)
		   (multiple-value-bind (%worker %this %module)
		      (js-main-worker! ,name ,(absolute path) #f
			 nodejs-new-global-object nodejs-new-module)
		      (js-worker-push-thunk! %worker "nodejs-toplevel"
			 ,(if (config-get conf :function-nice-name #f)
			      (let ((id (string->symbol "#")))
				 `(let ((,id ,thunk))
				     ,id))
			      thunk))
		      ,(profilers this conf)
		      ,(js-wait-worker '%worker)))
		(&end!)))))

   (with-access::J2SProgram this (module main nodes headers decls
					 mode name pcache-size call-size
					 cnsts globals loc)
      (let* ((esimports (j2s-module-imports this))
	     (esexports (j2s-module-exports this))
	     (conf (cons* :array (j2s-find-extern-decl headers 'Array)
		      :string (j2s-find-extern-decl headers 'String)
		      :regexp (j2s-find-extern-decl headers 'RegExp)
		      :math (j2s-find-extern-decl headers 'Math)
		      :program this
		      conf))
	     (scmheaders (j2s-scheme headers mode return conf))
	     (scmdecls (j2s-scheme decls mode return conf))
	     (scmclos (filter-map (lambda (d)
				     (j2s-scheme-closure d mode return conf))
			 decls))
	     (scmnodes (j2s-scheme nodes mode return conf))
	     (cnsttable (%cnst-table cnsts mode return conf)))
	 (cond
	    ((and main (not (config-get conf :worker #t)))
	     (j2s-main-sans-worker-module this name
		cnsttable
		(flatten-nodes (append scmheaders scmdecls scmclos))
 		esexports esimports
		(flatten-nodes scmnodes)
		conf))
	    (else
	     (let ((body (flatten-nodes
			    (append scmheaders scmdecls scmclos scmnodes))))
		(cond
		   (module
		    ;; a module whose declaration is in the source
		    (j2s-module module cnsttable esexports esimports body))
		   ((not name)
		    ;; a mere expression
		    (epairify-deep loc
		       `(lambda (%this this %scope %module)
			   (&with!
			      ,@(filter fundef? globals)
			      ,@(filter fundef? body)
			      (%define-cnst-table ,(length cnsts))
			      (%define-pcache ,pcache-size)	       
			      (define %pcache
				 (js-make-pcache-table ,pcache-size
				    ,(config-get conf :filename)
				    ,@(if (config-get conf :profile-cache #f)
					  (list `',(j2s-profile-cache this conf))
					  '())))
			      ,@(if (config-get conf :profile-call #f)
				    `((define %call-log (make-vector ,call-size #l0))
				      (define %call-locations ',(call-locations this)))
				    '())
			      ,@(if (config-get conf :profile-cmap #f)
				    `((define %cmap-log (make-vector ,call-size '())))
				    '())
			      (define %worker (js-current-worker))
			      (define %source (or (the-loading-file) "/"))
			      (define %resource (dirname %source))
			      (define %cnst-table ,cnsttable)
			      ,@esimports
			      ,esexports
			      ,@(filter nofundef? globals)
			      ,@(exit-body (filter nofundef? body) conf)))))
		   (main
		    ;; generate a main hopscript module 
		    (j2s-main-module/workers name cnsttable
		       esexports esimports body))
		   (else
		    ;; generate the module clause
		    (let ((mod (js-module loc name)))
		       (j2s-module mod cnsttable esexports esimports body))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-main-sans-worker-module ...                                  */
;*---------------------------------------------------------------------*/
(define (j2s-main-sans-worker-module this name cnsttable toplevel
	   esexports esimports body conf)
   (with-access::J2SProgram this (mode pcache-size call-size %this path globals
				    cnsts loc name)
      (let ((module (js-module/main loc name)))
	 (epairify-deep loc
	    `(,module 
		;;; (&begin!) must not be a constant! (_do not_ use quote)
		,@(filter fundef? globals)
		,@(filter fundef? body)
		(define __js_strings (&begin!))
		(%define-cnst-table ,(length cnsts))
		(%define-pcache ,pcache-size)
		(define %pcache
		   (js-make-pcache-table ,pcache-size
		      ,(config-get conf :filename)
		      ,@(if (config-get conf :profile-cache #f)
			    (list `',(j2s-profile-cache this conf))
			    '())))
		,@(if (config-get conf :profile-call #f)
		      `((define %call-log (make-vector ,call-size #l0))
			(define %call-locations ',(call-locations this)))
		      '())
		,@(if (config-get conf :profile-cmap #f)
		      `((define %cmap-log (make-vector ,call-size '())))
		      '())
		(hop-sofile-compile-policy-set! 'static)
		(hopjs-standalone-set! #t)
		(define %this (nodejs-new-global-object :name ,name))
		(define %source ,path)
		(define %resource (dirname %source))
		(define %scope (nodejs-new-scope-object %this))
		(define this
		   (with-access::JsGlobalObject %this (js-object)
		      (js-new0 %this js-object)))
		(define %worker
		   (js-main-worker! ,name ,(absolute path) #f
		      nodejs-new-global-object nodejs-new-module))
		(define %module
		   (nodejs-new-module ,(basename path) ,(absolute path)
		      %worker %this))
		(define %cnst-table ,cnsttable)
		,@esimports
		,esexports
		,@(filter nofundef? globals)
		,@toplevel
		,@(if (config-get conf :libs-dir #f)
		      `((hop-sofile-directory-set! ,(config-get conf :libs-dir #f)))
		      '())
		(define (main args)
		   ,`(define __js_strings (&init!))
		   ,(profilers this conf)
		   (hopscript-install-expanders!)
		   (hop-port-set! -1)
		   (hop-ssl-port-set! -1)
		   (bigloo-library-path-set! ',(bigloo-library-path))
		   (set! !process (nodejs-process %worker %this))
		   ,@(exit-body (filter nofundef? body) conf))
		(&end!))))))

;*---------------------------------------------------------------------*/
;*    j2s-module-imports ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-module-imports this::J2SProgram)

   (define (import-iprgm i) (with-access::J2SImport i (iprgm) iprgm))
   (define (import-mvar i) (with-access::J2SImport i (mvar) mvar))

   (define (evar-ident idx)
      (string->symbol (format "%import-evars-~a" idx)))

   (define (reindex! this::J2SProgram iprgm::J2SProgram reindex)
      (with-access::J2SProgram this (%info)
	 (set! %info (cons (cons iprgm  reindex) %info))))

   (define (module-import-es6 im idx)
      (with-access::J2SImport im (mvar ivar path iprgm)
	 (with-access::J2SProgram iprgm (mode)
	    (let ((impid (evar-ident idx)))
	       (reindex! this iprgm idx)
	       (set! mvar `(vector-ref %imports ,idx))
	       (set! ivar impid)
	       `(define ,impid
		   (with-access::JsModule (vector-ref %imports ,idx) (evars)
		      evars))))))
   
   (define (module-imports prgm::J2SProgram)
      (with-access::J2SProgram prgm (imports)
	 (map (lambda (im idx)
		 (with-access::J2SImport im (mvar ivar path iprgm)
		    (with-access::J2SProgram iprgm (mode)
		       (module-import-es6 im idx))))
	    imports (iota (length imports)))))

   (define (redirect-only?::bool iprgm::J2SProgram)
      ;; true iff iprgm only redirect exports
      ;; without exporting bindings itself
      (with-access::J2SProgram iprgm (exports)
	 (every (lambda (e)
		   (with-access::J2SExport e (from) from))
	    exports)))
      
   (define (module-import-redirect iprgm::J2SProgram mvar eidx)
      (with-access::J2SProgram iprgm (imports (ipath path))
	 (let ((nres '())
	       (nqueue '()))
	    (for-each (lambda (im idx)
			 (with-access::J2SImport im (names iprgm path)
			    (when (and (pair? names) (eq? (car names) 'redirect))
			       (unless (redirect-only? iprgm)
				  (let ((evid (evar-ident eidx)))
				     (reindex! this iprgm eidx)
				     (set! eidx (+fx eidx 1))
				     (let ((x `(with-access::JsModule ,mvar (imports)
						 (with-access::JsModule
						       (vector-ref imports ,idx)
						       (evars)
						    ,(format "redirect: ~a ~a" ipath path)
						    evars))))
					(set! nres (cons x nres)))))
			       (let ((q (cons iprgm `(vector-ref (with-access::JsModule ,mvar (imports) imports) ,idx))))
				  (set! nqueue (append! nqueue (list q)))))))
	       imports (iota (length imports)))
	    (values nqueue nres))))
   
   (define (module-redirect prgm::J2SProgram)
      (with-access::J2SProgram prgm (imports)
	 (let loop ((queue (map (lambda (i)
				   (with-access::J2SImport i (iprgm mvar)
				      (cons iprgm mvar)))
			      imports))
		    (idx (length imports))
		    (res '())
		    (stack '()))
	    (if (null? queue)
		(reverse! res)
		(let ((iprgm (caar queue))
		      (mvar (cdar queue)))
		   (multiple-value-bind (nqueue nres)
		      (module-import-redirect iprgm mvar idx)
		      (loop (append! (cdr queue) nqueue)
			 (+fx idx (length nres))
			 (append nres res)
			 (cons iprgm stack))))))))

   (define (import-module im)
      (with-access::J2SImport im (respath iprgm loc names iprgm)
	 (with-access::J2SProgram iprgm (mode path exports)
	    (if (eq? mode 'hop)
		`(nodejs-import-module-hop %worker %this %module
		    ,respath
		    ,(j2s-program-checksum! iprgm)
		    ',loc
		    ',(list->vector
			 (map (lambda (e)
				 (with-access::J2SExport e (alias decl)
				    (with-access::J2SDecl decl (vtype)
				       (cons alias vtype))))
			    exports)))
		`(nodejs-import-module %worker %this %module
		    ,respath
		    ,(j2s-program-checksum! iprgm)
		    ',loc)))))

   (with-access::J2SProgram this (imports path %info)
      (set! %info '())
      ;; WARNING !!! the evaluation order matters module-imports _must_ be
      ;; called before module-redirect (as module-imports assigned the mvar
      ;; properties used by module-redirect).
      (let* ((mimports (module-imports this))
	     (mredirects (module-redirect this)))
	 (if (and (null? imports) (null? mredirects))
	     '()
	     (cons
		`(define %imports
		    (with-access::JsModule %module (imports)
		       (set! imports (vector ,@(map import-module imports)))
		       imports))
		(append
		   mimports
		   `((with-access::JsModule %module (redirects)
			(set! redirects
			   (vector
			      ,@(map (lambda (i) (evar-ident i))
				   (iota (length imports)))
			      ,@mredirects))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-module-exports ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-module-exports this::J2SProgram)
   
   (define (redirect-index this::J2SProgram id iprgm::J2SProgram loc)
      ;; Find the root program (module) that exports id, and return
      ;; the current index relative to this imports of its evars vector.
      ;; This function works hand in hand with J2S-MODULE-IMPORTS
      (with-access::J2SProgram iprgm (path exports)
	 (let ((x (find (lambda (e)
			   (with-access::J2SExport e ((eid id))
			      (eq? eid id)))
		     exports)))
	    (if (not x)
		(raise
		   (instantiate::&error
		      (proc path)
		      (msg (format "Cannot find redirection for `~a'" id))
		      (obj path)
		      (fname (cadr loc))
		      (location (caddr loc))))
		(with-access::J2SExport x (from)
		   (cond
		      ((isa? from J2SProgram)
		       (redirect-index this id from loc))
		      ((eq? from 'hop)
		       (with-access::J2SProgram this (%info)
			  (let ((c (assq iprgm %info)))
			     (cdr c))))
		      (else
		       (with-access::J2SProgram this (%info)
			  (let ((c (assq iprgm %info)))
			     (cdr c))))))))))
   
   (define (export e::J2SExport)
      (with-access::J2SExport e (index decl from id alias)
	 (with-access::J2SDecl decl ((w writable) loc)
	    (cond
	       ((isa? from J2SProgram)
		`(vector (& ,(symbol->string alias)) (cons ,index ,(redirect-index this id from loc)) ,w))
	       (else
		`(vector (& ,(symbol->string alias)) ,index ,w))))))
   
   (with-access::J2SProgram this (exports imports path checksum)
      (let ((idx (j2sprogram-get-export-index this))
	    (cs (j2s-program-checksum! this)))
	 (cond
	    ((pair? exports)
	     `(define %evars
		 (with-access::JsModule %module (evars exports checksum)
		    (set! checksum ,(j2s-program-checksum! this))
		    (set! exports (list ,@(map export exports)))
		    ,@(if (>fx idx 0)
			  `((set! evars (make-vector ,idx (js-undefined))))
			  '())
		    evars)))
	    ((=fx cs 0)
	     #unspecified)
	    (else
	     `(with-access::JsModule %module (checksum)
		 (set! checksum ,cs)))))))

;*---------------------------------------------------------------------*/
;*    js-module/main ...                                               */
;*---------------------------------------------------------------------*/
(define (js-module/main loc name)
   (epairify-deep loc
      `(module ,(string->symbol name)
	  (eval (library hop) (library hopscript) (library nodejs))
	  (library hop hopscript nodejs)
	  (cond-expand (enable-libuv (library libuv)))
	  (main main))))

;*---------------------------------------------------------------------*/
;*    js-module ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-module loc name)
   (epairify-deep loc
      `(module ,(string->symbol name)
	  (library hop hopscript js2scheme nodejs)
	  (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))))

;*---------------------------------------------------------------------*/
;*    js-wait-worker ...                                               */
;*---------------------------------------------------------------------*/
(define (js-wait-worker worker)
   `(thread-join! ,worker))<
	   
;*---------------------------------------------------------------------*/
;*    profilers ...                                                    */
;*---------------------------------------------------------------------*/
(define (profilers this conf)
   (when (or (config-get conf :profile-call #f)
	     (config-get conf :profile-cmap #f)
	     (config-get conf :profile-cache #f)
	     (config-get conf :profile-hint #f)
	     (config-get conf :profile-alloc #f))
      `(js-profile-init ',(cons* :hash (j2ssum this) (filter-config conf))
	  ,(cond
	      ((and (config-get conf :profile-call #f)
		    (config-get conf :profile-cmap #f))
	       '(vector %source %call-log %cmap-log %call-locations))
	      ((config-get conf :profile-call #f)
	       '(vector %source %call-log #f %call-locations))
	      ((config-get conf :profile-cmap #f)
	       '(vector %source #f %cmap-log %call-locations))
	      (else
	       #f))
	  ,(when (config-get conf :profile-symtable #f)
	      `',(profile-symtable this)))))

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
;*    %cnst-table ...                                                  */
;*---------------------------------------------------------------------*/
(define (%cnst-table cnsts mode return conf)
   
   (define (%cnst-table-debug cnsts)
      `(js-constant-init (js-cnst-table)
	  (vector
	     ,@(map (lambda (n)
		       (let ((s (j2s-scheme n mode return conf)))
			  (if (isa? n J2SRegExp)
			      (with-access::J2SRegExp n (loc val flags inline)
				 (if inline
				     `(with-access::JsRegExp ,s (rx) rx)
				     s))
			      s)))
		  cnsts))
	  %this))

   (define (data-property-cnst init)
      (with-access::J2SDataPropertyInit init (val)
	 (if (isa? val J2SLiteralValue)
	     (with-access::J2SLiteralValue val (val)
		val)
	     (with-access::J2SLiteralCnst val (index)
		(cons index index)))))
   
   (define (%cnst-table-intext cnsts)
      
      (define (j2s-constant this::J2SExpr)
	 (cond
	    ((isa? this J2SString)
	     (with-access::J2SString this (val)
		(let ((n (if (eq? (string-minimal-charset val) 'ascii) 6 7)))
		   (vector n val))))
	    ((isa? this J2SRegExp)
	     (with-access::J2SRegExp this (loc val flags inline)
		(vector (if inline 5 4) val flags loc)))
	    ((isa? this J2SRegExp)
	     (with-access::J2SRegExp this (loc val flags inline)
		(vector (if inline 3 1) val flags)))
	    ((isa? this J2SCmap)
	     (with-access::J2SCmap this (val)
		(vector 2 (vector-map symbol->string val))))
	    ((isa? this J2SObjInit)
	     (with-access::J2SObjInit this (inits cmap)
		(with-access::J2SLiteralCnst cmap (index)
		   (vector 8 index
		      (list->vector (map data-property-cnst inits))))))
	    (else
	     (error "j2s-constant" "wrong literal" this))))
      
      `(js-constant-init (js-cnst-table)
	  ,(obj->string (list->vector (map j2s-constant cnsts))) %this))
   
   ;; this must be executed after the code is compiled as this
   ;; compilation might change or add new constants.
   (if (>fx (config-get conf :debug 0) 0)
       (%cnst-table-debug cnsts)
       (%cnst-table-intext cnsts)))

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
      (when (>=fx profid 0)
	 (match-case loc
	    ((at ?- ?point)
	     (vector-set! vec profid point)))))
   vec)

;*---------------------------------------------------------------------*/
;*    profile-symtable ...                                             */
;*---------------------------------------------------------------------*/
(define (profile-symtable this::J2SProgram)
   (sort (lambda (x y) (<fx (car x) (car y)))
      (with-access::J2SProgram this (call-size)
	 (collect-functions* this))))

;*---------------------------------------------------------------------*/
;*    collect-functions* ::J2SNode ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-functions* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-functions* ::J2SDeclFun ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-functions* this::J2SDeclFun)
   (with-access::J2SDeclFun this (profid loc id val parent)
      (with-access::J2SFun val (body)
	 (if parent
	     (collect-functions* body)
	     (cons (cons (caddr loc) id) (collect-functions* body))))))

;*---------------------------------------------------------------------*/
;*    collect-functions* ::J2SAssig ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-functions* this::J2SAssig)
   (with-access::J2SAssig this (lhs rhs loc)
      (let loop ((rhs rhs))
	 (cond
	    ((isa? rhs J2SFun)
	     (with-access::J2SFun rhs (body)
		(cons (cons (+fx 2 (caddr loc)) (expr->id lhs))
		   (collect-functions* body))))
	    ((isa? rhs J2SMethod)
	     (with-access::J2SMethod rhs (function)
		(loop function)))
	    (else
	     (call-default-walker))))))

;*---------------------------------------------------------------------*/
;*    collect-functions* ::J2SCall ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-functions* this::J2SCall)
   (with-access::J2SCall this (fun loc)
      (if (isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     (if (isa? decl J2SDeclFun)
		 (with-access::J2SDecl decl (id)
		    (cons (cons (caddr loc) id) (call-default-walker)))
		 (call-default-walker)))
	  (call-default-walker))))
	       
;*---------------------------------------------------------------------*/
;*    collect-fuctions* ::J2SFun ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-functions* this::J2SFun)
   (with-access::J2SFun this (loc body)
      (cons (cons (caddr loc) 'anonymous)
	 (collect-functions* body))))

;*---------------------------------------------------------------------*/
;*    expr->id ::J2SExpr ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (expr->id this::J2SExpr)
   '?)

;*---------------------------------------------------------------------*/
;*    expr->id ::J2SRef ...                                            */
;*---------------------------------------------------------------------*/
(define-method (expr->id this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (id)
	 id)))

;*---------------------------------------------------------------------*/
;*    expr->id ::J2SAccess ...                                         */
;*---------------------------------------------------------------------*/
(define-method (expr->id this::J2SAccess)
   (with-access::J2SAccess this (obj field)
     (symbol-append (expr->id obj) '|.| (expr->id field)))) 

;*---------------------------------------------------------------------*/
;*    expr->id ::J2SString ...                                         */
;*---------------------------------------------------------------------*/
(define-method (expr->id this::J2SString)
   (with-access::J2SString this (val)
      (string->symbol val)))

;*---------------------------------------------------------------------*/
;*    j2s-program-checksum! ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-program-checksum! prgm::J2SProgram)
   (checksum prgm))

;*---------------------------------------------------------------------*/
;*    absolute ...                                                     */
;*---------------------------------------------------------------------*/
(define (absolute path)
   (make-file-name (pwd) path))

;*---------------------------------------------------------------------*/
;*    j2s-find-extern-decl ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-find-extern-decl decls searchid)
   (find (lambda (d)
	    (when (isa? d J2SDeclExtern)
	       (with-access::J2SDeclExtern d (id)
		  (eq? id searchid))))
      decls))

;*---------------------------------------------------------------------*/
;*    j2s-profile-cache ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-profile-cache this::J2SProgram conf)
   (with-access::J2SProgram this (pcache-size)
      (let ((profile-info-table (make-vector pcache-size '#(-1 "" get))))
	 (let loop ((i (-fx pcache-size 1)))
	    (when (>=fx i 0)
	       (vector-set! profile-info-table i
		  `#(,(- i) "" get))
	       (loop (-fx i 1))))
	 (profile-cache-info-init this profile-info-table)
	 profile-info-table)))

;*---------------------------------------------------------------------*/
;*    profile-unresolved ...                                           */
;*---------------------------------------------------------------------*/
(define (profile-unresolved this::J2SUnresolvedRef table usage)
   (with-access::J2SUnresolvedRef this (cache loc id)
      (when cache
	 (match-case loc
	    ((at ?- ?point)
	     (vector-set! table cache `#(,point ,(symbol->string id) ,usage)))))))

;*---------------------------------------------------------------------*/
;*    profile-access ...                                               */
;*---------------------------------------------------------------------*/
(define (profile-access this::J2SAccess table usage #!optional cache)
   (with-access::J2SAccess this (obj field (acache cache) loc)
      (let ((c (or cache acache)))
	 (if (isa? field J2SString)
	     (when c
		(with-access::J2SString field (val)
		   (match-case loc
		      ((at ?- ?point)
		       (vector-set! table c `#(,point ,val ,usage))))))
	     (profile-cache-info-init field table)))
      (profile-cache-info-init obj table)))

;*---------------------------------------------------------------------*/
;*    profile-cache-info-init ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-cache-info-init this::J2SNode table)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    profile-cache-info-init ::J2SAccess ...                          */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-cache-info-init this::J2SAccess table)
   (profile-access this table 'get))

;*---------------------------------------------------------------------*/
;*    profile-cache-info-init ::J2SUnresolvedRef ...                   */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-cache-info-init this::J2SUnresolvedRef table)
   (profile-unresolved this table 'get))
   
;*---------------------------------------------------------------------*/
;*    profile-cache-info-init ::J2SAssig ...                           */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-cache-info-init this::J2SAssig table)
   (with-access::J2SAssig this (lhs rhs)
      (cond
	 ((isa? lhs J2SAccess)
	  (profile-access lhs table 'put))
	 ((isa? lhs J2SUnresolvedRef)
	  (profile-unresolved lhs table 'put))
	 (else
	  (profile-cache-info-init lhs table)))
      (profile-cache-info-init rhs table)))
	  
;*---------------------------------------------------------------------*/
;*    profile-cache-info-init ::J2SCall ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-cache-info-init this::J2SCall table)
   (with-access::J2SCall this (fun args thisarg cache loc)
      (if (isa? fun J2SAccess)
	  (begin
	     (profile-access fun table 'get)
	     (when cache
		(profile-access fun table 'call cache)))
	  (profile-cache-info-init fun table))
      (for-each (lambda (a) (profile-cache-info-init a table)) args)
      (for-each (lambda (a) (profile-cache-info-init a table)) thisarg)))

;*---------------------------------------------------------------------*/
;*    profile-cache-info-init ::J2SCacheCheck ...                      */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-cache-info-init this::J2SCacheCheck table)
   (with-access::J2SCacheCheck this (prop cache loc)
      (match-case loc
	 ((at ?- ?point)
	  (vector-set! table cache `#(,point ,(symbol->string prop) get)))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    profile-cache-info-init ::J2SCacheUpdate ...                     */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-cache-info-init this::J2SCacheUpdate table)
   (with-access::J2SCacheUpdate this (prop loc cache)
      (match-case loc
	 ((at ?- ?point)
	  (vector-set! table cache `#(,point ,(symbol->string prop) get)))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    profile-cache-info-init ::J2SNew ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-cache-info-init this::J2SNew table)
   (with-access::J2SNew this (caches loc clazz)
      (when (pair? caches)
	 (match-case loc
	    ((at ?- ?point)
	     (vector-set! table (car caches) `#(,point "get" get))
	     (when (pair? (cdr caches))
		(vector-set! table (cadr caches) `#(,point "put" get))
		(when (pair? (cddr caches))
		   (vector-set! table (cadr caches) `#(,point "apply" get))))))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    profile-cache-info-init ::J2SOPTInitSeq ...                      */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-cache-info-init this::J2SOPTInitSeq table)
   (with-access::J2SOPTInitSeq this (cache loc ref)
      (match-case loc
	 ((at ?- ?point)
	  (vector-set! table cache `#(,point "xxx" put)))))
   (call-default-walker))
   
;*---------------------------------------------------------------------*/
;*    fundef? ...                                                      */
;*---------------------------------------------------------------------*/
(define (fundef? e)
   (match-case e
      ((define (?- . ?arg) . ?-) #t)
      ((define ?- (labels ((?id . ?-)) ?id)) #t)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    nofundef? ...                                                    */
;*---------------------------------------------------------------------*/
(define (nofundef? e)
   (not (fundef? e)))
