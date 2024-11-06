;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-program.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 18 08:03:25 2018                          */
;*    Last change :  Wed Nov  6 08:04:30 2024 (serrano)                */
;*    Copyright   :  2018-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Program node compilation                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-program

   (include "ast.sch"
	    "context.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_usage
	   __js2scheme_checksum
	   __js2scheme_scheme
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-constant
	   __js2scheme_scheme-record))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SProgram ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SProgram mode return ctx)
   
   (define (j2s-master-module module cnsttable
	      esexports esredirects esimports
	      scmheaders scmrecords
	      records irecords
	      body)
      (with-access::J2SProgram this (pcache-size call-size cnsts globals loc)
	 `(,(append module
	       (if (pair? irecords)
		   `((static ,@(map j2s-record-declaration irecords)))
		   '())
	       (if (pair? records)
		   `((export ,@(map j2s-record-declaration records)))
		   '()))
	   (define __js_strings #f)
	   (define __js_rxcaches #f)
	   (%define-cnst-table ,(length cnsts))
	   (%define-pcache ,pcache-size)
	   (define %sourcepath ,(context-get ctx :filename))
	   (define %pcache
	      (js-make-pcache-table ,pcache-size
		 %sourcepath
		 ,@(if (or (context-get ctx :profile-cache #f)
			   (context-get ctx :profile-location #f))
		       (list `',(j2s-profile-cache this))
		       '())))
	   ,@(j2s-profile-headers ctx this)
	   (define %source ,(context-get ctx :source))
	   (define %resource (dirname %source))
	   ,@(map j2s-record-predicate records)
	   ,@(js-declare-tls this ctx)
	   ,(epairify-deep loc
	       `(define (hopscript %this this %scope %module)
		   (define __js_strings ,(j2s-jsstring-init this))
		   (define __js_rxcaches ,(j2s-regexp-caches-init this))
		   (define js-string-names (js-get-js-string-names))
		   (define js-integer-names (js-get-js-integer-names))
		   (define %worker (js-current-worker))
		   (define %cnst-table ,cnsttable)
		   ,@(js-define-tls this ctx)
		   ,@(j2s-tls-headers scmheaders)
		   (cond-expand
		      ((not bigloo-eval) (pragma "HOP_REWRITE_INIT($1)" ,pcache-size)))
		   (letrec* ,(j2s-let-headers scmheaders)
		      ,@(j2s-expr-headers scmheaders)
		      ;;,@globals
		      "master"
		      ,esexports
		      ,@esimports
		      ,@esredirects
		      ,@(exit-body ctx scmrecords
			   (filter fundef? body) (filter nofundef? body)))))
	   ;; for dynamic loading
	   hopscript)))
   
   (define (j2s-slave-module module cnsttable
	      esexports esredirects esimports
	      scmheaders scmrecords
	      records irecords
	      body)
      (with-access::J2SProgram this (pcache-size call-size cnsts globals loc)
	 `(,(append module
	       (if (pair? irecords)
		   `((static ,@(map j2s-record-declaration irecords)))
		   '())
	       (if (pair? records)
		   `((export ,@(map j2s-record-declaration records)))
		   '())
	       '((option (register-srfi! 'hopjs-worker-slave))))
	    ;; (&begin!) must not be a constant! (_do not_ use quote)
	    (define __js_strings #f)
	    (define __js_rxcaches #f)
	    (define %sourcepath ,(context-get ctx :filename))
	    (define %source ,(context-get ctx :source))
	    (define %resource (dirname %source))
	    ,@(map j2s-record-predicate records)
	     ,@(js-declare-tls this ctx)
	    ,(epairify-deep loc
		`(define (hopscript %this this %scope %module)
		    (define __js_strings ,(j2s-jsstring-init this))
		    (define __js_rxcaches ,(j2s-regexp-caches-init this))
		    (define js-string-names (js-get-js-string-names))
		    (define js-integer-names (js-get-js-integer-names))
		    (define %pcache
		       (js-make-pcache-table ,pcache-size
			  %sourcepath
			  ,@(if (or (context-get ctx :profile-cache #f)
				    (context-get ctx :profile-location #f))
				(list `',(j2s-profile-cache this))
				'())))
		    ,@(j2s-profile-headers ctx this)
		    (define %worker (js-current-worker))
		    (define %cnst-table ,cnsttable)
		    ,@(js-define-tls this ctx)
		    ,@(j2s-tls-headers scmheaders)
		    (cond-expand
		       ((not bigloo-eval) (pragma "HOP_REWRITE_INIT($1)" ,pcache-size)))
		    (letrec* ,(j2s-let-headers scmheaders)
		       ,@(j2s-expr-headers scmheaders)
		       ;;,@globals
		       "slave"
		       ,esexports
		       ,@esimports
		       ,@esredirects
		       ,@(exit-body ctx scmrecords
			    (filter fundef? body) (filter nofundef? body)))))
	    ;; for dynamic loading
	    hopscript)))
   
   (define (j2s-module module cnsttable esexports esredirects esimports scmheaders scmrecords records irecords body)
      (if (context-get ctx :worker-slave)
	  (j2s-slave-module module cnsttable
	     esexports esredirects esimports
	     scmheaders scmrecords
	     records irecords
	     body)
	  (j2s-master-module module cnsttable
	     esexports esredirects esimports
	     scmheaders scmrecords
	     records irecords
	     body)))

   (define (j2s-expr module cnsttable esexports esredirects esimports scmheaders scmrecords records irecords body)
      (with-access::J2SProgram this (globals loc cnsts pcache-size call-size)
	 (epairify-deep loc
	    `(lambda (%this this %scope %module)
		,@(js-declare-tls this ctx)
		(define __js_strings ,(j2s-jsstring-init this))
		(define __js_rxcaches ,(j2s-regexp-caches-init this))
		,@(map (lambda (rec)
			  `(define-class ,(cdr (j2s-record-declaration rec))))
		     records)
		(%define-cnst-table ,(length cnsts))
		(%define-pcache ,pcache-size)	       
		(define %sourcepath ,(context-get ctx :filename))
		(define %pcache
		   (js-make-pcache-table ,pcache-size
		      %sourcepath
		      ,@(if (or (context-get ctx :profile-cache #f)
				(context-get ctx :profile-location #f))
			    (list `',(j2s-profile-cache this))
			    '())))
		,@(j2s-profile-headers ctx this)
		(define %worker (js-current-worker))
		(define %source ,(context-get ctx :source))
		(define %resource (dirname %source))
		(define %cnst-table ,cnsttable)
		,@(js-define-tls this ctx)
		,@(j2s-tls-headers scmheaders)
		,@scmheaders
		;; ,@globals
		,esexports
		,@esimports
		,@esredirects
		,@(exit-body ctx scmrecords
		     (filter fundef? body) (filter nofundef? body))))))
   
   (define (j2s-let-globals globals)
      ;; transforms a list of (define id var) into a list of (id var)
      (map cdr globals))

   (with-access::J2SProgram this (path module main nodes headers decls
				    mode name pcache-size call-size
				    cnsts loc
				    %info)
      (set! %info '())
      (let* ((nctx (compiler-context-set! ctx
		      :array (j2s-find-extern-decl headers 'Array)
		      :string (j2s-find-extern-decl headers 'String)
		      :regexp (j2s-find-extern-decl headers 'RegExp)
		      :math (j2s-find-extern-decl headers 'Math)
		      :map (j2s-find-extern-decl headers 'Map)
		      :weakmap (j2s-find-extern-decl headers 'WeakMap)
		      :set (j2s-find-extern-decl headers 'Set)
		      :weakset (j2s-find-extern-decl headers 'WeakSet)
		      :object (j2s-find-extern-decl headers 'Object)
		      :bigint (j2s-find-extern-decl headers 'BigInt)
		      :promise (j2s-find-extern-decl headers 'Promise)
		      :program this))
	     (esimports (j2s-module-imports this nctx))
	     (esredirects (j2s-module-redirects this nctx))
	     (esexports (j2s-module-exports this nctx))
	     (scmheaders (j2s-scheme headers mode return nctx))
	     (scmdecls (j2s-scheme decls mode return nctx))
	     (scmclos '())
	     (scmnodes (j2s-scheme nodes mode return nctx))
	     (cnsttable (%cnst-table cnsts mode return nctx))
	     (records (j2s-collect-records* this))
	     (irecords (j2s-collect-irecords* this))
	     (scmrecords (append-map (lambda (rec)
					(j2s-record-prototype-method rec
					   mode return ctx))
			    records)))
	 (cond
	    ((and main (context-get nctx :tls))
	     (tprint "not implemented yet")
	     (exit 0))
	    ((and main (not (context-get nctx :worker #t)))
	     (j2s-main-sans-worker-module this cnsttable
		(flatten-nodes (append scmheaders scmdecls scmclos))
 		esexports esredirects esimports scmrecords records
		(flatten-nodes scmnodes)
		nctx))
	    (else
	     (let ((body (flatten-nodes (append scmdecls scmclos scmnodes))))
		(cond
		   (module
		    ;; a module whose declaration is in the source
		    (j2s-module module cnsttable
		       esexports esredirects esimports
		       scmheaders scmrecords records irecords
		       body))
		   ((not name)
		    ;; a mere expression
		    (j2s-expr module cnsttable
		       esexports esredirects esimports
		       scmheaders scmrecords records irecords body))
		   (main
		    ;; generate a main hopscript module 
		    (j2s-main-worker-module this cnsttable
		       esexports esredirects esimports
		       scmheaders scmrecords records irecords
		       body nctx))
		   (else
		    ;; generate the module clause
		    (let ((mod (js-module this ctx)))
		       (j2s-module mod cnsttable
			  esexports esredirects esimports
			  scmheaders scmrecords records irecords
			  body))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-tls-headers ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-tls-headers headers)
   (filter-map (lambda (hd)
		  (match-case hd
		     ((define-tls ?id ?val) hd)
		     (else #f)))
      headers))

;*---------------------------------------------------------------------*/
;*    j2s-let-headers ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-let-headers headers)
   (filter-map (lambda (hd)
		  (match-case hd
		     ((define ?id ?val) `(,id ,val))
		     ((define-tls ?id ?val) #f)
		     (else #f)))
      headers))

;*---------------------------------------------------------------------*/
;*    j2s-expr-headers ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-expr-headers headers)
   (filter-map (lambda (expr)
		  (match-case expr
		     ((define ?id ?val) #f)
		     ((define-tls ?id ?val) #f)
		     ((js-undefined) #f)
		     (#unspecified #f)
		     (else expr)))
      headers))
   
;*---------------------------------------------------------------------*/
;*    j2s-profile-headers ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-profile-headers ctx this::J2SProgram)
   (with-access::J2SProgram this (call-size)
      (filter (lambda (x) x)
	 (list
	    (when (or (context-get ctx :profile-cache #f)
		      (context-get ctx :profile-snapshot #f))
	       '(define %log-event-count 0))
	    (cond
	       ((context-get ctx :profile-snapshot #f)
		`(define %log-event-max-count
		  (let ((l (getenv "HOP_PROFILE_SNAPSHOT_LENGTH"))
			(d ,(context-get ctx :profile-snapshot-length
			       10000000)))
		     (if (string? l)
			 (or (string->integer l) d)
			 d))))
	       ((or (context-get ctx :profile-cache #f)
		    (context-get ctx :profile-location #f))
		'(define %log-event-max-count -1)))
	    (when (context-get ctx :profile-call #f)
	       `(define %call-log (make-vector ,call-size #l0)))
	    (when (context-get ctx :profile-cmap #f)
	       `(define %cmap-log (make-vector ,call-size '())))
	    (when (context-get ctx :profile-call #f)
	       `(define %call-locations ',(call-locations this)))))))
	       
;*---------------------------------------------------------------------*/
;*    j2s-main-worker-module ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-main-worker-module this::J2SProgram cnsttable
	   esexports esredirects esimports
	   scmheaders scmrecords records irecords
	   body ctx)
   (with-access::J2SProgram this (pcache-size call-size path
				    globals cnsts loc name)
      (let* ((jsmod (js-module/main this ctx))
	     (jsthis `(with-access::JsGlobalObject %this (js-object)
			 (js-new0 %this js-object)))
	     (thunk `(lambda (%this)
			(define-tls %this %t)
			(define-tls %module %m)
			(let* ((_ (set! __js_strings ,(j2s-jsstring-init this)))
			       (__ (set! __js_rxcaches ,(j2s-regexp-caches-init this)))
			       (this ,jsthis)
;* 				  ,@(j2s-let-globals globals)          */
			       )
			   ;; the sole purpose of the scmheaders split
			   ;; is not minimize letrec* nesting level
			   (define-tls %cnst-table ,cnsttable)
			   (define-tls %scope (nodejs-new-scope-object %this))
			   ,@(js-define-tls this ctx)
			   ,@(j2s-tls-headers scmheaders)
			   (letrec* ,(j2s-let-headers scmheaders)
			      ,@(j2s-expr-headers scmheaders)
			      ;; ,@globals
			      ,esexports
			      ,@esimports
			      ,@esredirects
			      ,@(exit-body ctx
				   scmrecords
				   (filter fundef? body)
				   (filter nofundef? body)))))))
	 `(,(append jsmod
	       (if (pair? irecords)
		   `((static ,@(map j2s-record-declaration irecords)))
		   '())
	       (if (pair? records)
		   `((export ,@(map j2s-record-declaration records)))
		   '()))
	   (define __js_strings #f)
	   (define __js_rxcaches #f)
	   (%define-cnst-table ,(length cnsts))
	   (%define-pcache ,pcache-size)
	   (hop-sofile-compile-policy-set! 'static)
	   (define %sourcepath ,(context-get ctx :filename))
	   (define %pcache
	      (js-make-pcache-table ,pcache-size
		 %sourcepath
		 ,@(if (or (context-get ctx :profile-cache #f)
			   (context-get ctx :profile-location #f))
		       (list `',(j2s-profile-cache this))
		       '())))
	   ,@(j2s-profile-headers ctx this)
	   (hopjs-standalone-set! #t)
	   (define %source ,path)
	   (define %resource (dirname %source))
	   ,@(if (context-get ctx :sofile-dir #f)
		 `((hop-sofile-directory-set! ,(context-get ctx :sofile-dir #f)))
		 '())
	   ,@(map j2s-record-predicate records)
	   ,@(js-declare-tls this ctx)
	   (define (main args)
	      (when (getenv "BIGLOOTRACE") (bigloo-debug-set! 1))
	      (bigloo-library-path-set! ',(bigloo-library-path))
	      (hop-port-set! -1)
	      (hop-ssl-port-set! -1)
	      (hopscript-install-expanders!)
	      (cond-expand
		 ((not bigloo-eval) (pragma "HOP_REWRITE_INIT($1)" ,pcache-size)))
	      (multiple-value-bind (%worker %t %m)
		 (js-main-worker! ,name ,(absolute path) #f
		    nodejs-new-global-object nodejs-new-module
		    :autostart #f)
		 (js-worker-push! %worker "nodejs-toplevel"
		    ,(if (context-get ctx :function-nice-name #f)
			 (let ((id (string->symbol "#main")))
			    `(let ((,id ,thunk))
				,id))
			 thunk))
		 (js-worker-start! %worker)
		 ,(profilers this ctx)
		 ,(js-wait-worker '%worker)))))))

;*---------------------------------------------------------------------*/
;*    j2s-main-sans-worker-module ...                                  */
;*---------------------------------------------------------------------*/
(define (j2s-main-sans-worker-module this::J2SProgram cnsttable toplevel
	   esexports esredirects esimports scmrecords records body ctx)
   (with-access::J2SProgram this (mode
				    pcache-size call-size
				    %this path globals
				    cnsts loc name)
      (let ((module (js-module/main this ctx)))
	 (epairify-deep loc
	    `(,(append module
	       (if (pair? records)
		   `((export ,@(map j2s-record-declaration records)))
		   '()))
		,@(filter fundef? globals)
		,@(filter fundef? body)
		(define __js_strings ,(j2s-jsstring-init this))
		(define __js_rxcaches ,(j2s-regexp-caches-init this))
		(define %sourcepath ,(context-get ctx :filename))
		(%define-cnst-table ,(length cnsts))
		(%define-pcache ,pcache-size)
		(define %pcache
		   (js-make-pcache-table ,pcache-size
		      %sourcepath
		      ,@(if (or (context-get ctx :profile-cache #f)
				(context-get ctx :profile-location #f))
			    (list `',(j2s-profile-cache this))
			    '())))
		,@(j2s-profile-headers ctx this)
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
		   (js-main-no-worker! ,name ,(absolute path) #f
		      nodejs-new-global-object nodejs-new-module))
		(define %module
		   (nodejs-new-module ,(basename path) ,(absolute path)
		      %worker %this))
		(define %cnst-table ,cnsttable)
		(define !process #f)
		(define %import-meta #unspecified)
		(define %filename #unspecified)
		,esexports
		,@esimports
		,@esredirects
		,@(filter nofundef? globals)
		,@toplevel
		,@(if (context-get ctx :sofile-dir #f)
		      `((hop-sofile-directory-set! ,(context-get ctx :sofile-dir #f)))
		      '())
		,@(map j2s-record-predicate records)
		,@(append-map (lambda (rec)
				 (j2s-record-prototype-method rec
				    mode (lambda (x) x) ctx))
		     records)
		(define (main args)
		   (when (getenv "BIGLOOTRACE") (bigloo-debug-set! 1))
		   ,(profilers this ctx)
		   (hopscript-install-expanders!)
		   (hop-port-set! -1)
		   (hop-ssl-port-set! -1)
		   (bigloo-library-path-set! ',(bigloo-library-path))
		   (cond-expand
		      ((not bigloo-eval) (pragma "HOP_REWRITE_INIT($1)" ,pcache-size)))
		   (set! !process (nodejs-process %worker %this))
		      ,@(exit-body ctx (filter nofundef? body))))))))

;*---------------------------------------------------------------------*/
;*    j2s-module-imports ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-module-imports this::J2SProgram ctx)
   
   (define (module-import-es6 im)
      (let ((impid (importpath-var im)))
	 (with-access::J2SImportPath im (abspath path protocol import loc)
	    (with-access::J2SImport import (iprgm)
	       (unless iprgm (tprint "pas iprgm abpath=" abspath " path=" path))
	       `(define ,impid
		   (nodejs-import-module %worker %this %module
		      ,(if (char=? (string-ref path 0) #\.)
			   `(make-file-name
			       (dirname (js-jsstring->string %filename))
			       ,path)
			   path)
		      ,(j2s-program-checksum! iprgm)
		      ,(unless (eq? protocol 'core)
			  (context-get ctx :commonjs-export))
		      ',loc))))))
   
   (define (module-evars-es6 im)
      (let ((impid (importpath-var im))
	    (evarid (importpath-evar im)))
	 (with-access::J2SImportPath im (abspath protocol checksum loc)
	    `(define ,evarid
		(with-access::JsModule ,impid (evars) evars)))))
   
   (with-access::J2SProgram this (imports)
      (let ((imps (map module-import-es6 imports))
	    (evars (map module-evars-es6 imports)))
	 (append imps evars
	    `((with-access::JsModule %module (imports)
		 (set! imports
		    (vector
		       ,@(map (lambda (im) (importpath-var im))
			    imports)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-module-redirects ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-module-redirects this::J2SProgram ctx)
   
   (define (module-redirects-es6 im)
      (let ((evarid (importpath-evar im)))
	 (with-access::J2SImportPath im (import)
	    (with-access::J2SImport import (iprgm ipath loc)
	       (with-access::J2SImportPath ipath (index)
		  (with-access::J2SProgram iprgm (exports)
		     (append-map (lambda (x)
				    (if (isa? x J2SRedirect)
					(let loop ((x x)
						   (rindexes '())
						   (expr evarid))
					   (with-access::J2SRedirect x ((rindex index) export import)
					      (if (isa? export J2SRedirect)
						  (loop export (cons rindex rindexes)
						     `(js-redirect-ref ,expr ,rindex ',loc))
						  (with-access::J2SImport import (ipath iprgm)
						     (let ((rvarid (importpath-rvar im (reverse (cons rindex rindexes)))))
							(cons
							   `(define ,rvarid
							       (js-redirect-ref ,expr ,rindex ',loc ))
							   (with-access::J2SProgram iprgm (exports)
							      (append-map (lambda (export)
									     (if (isa? export J2SRedirect)
										 (loop export (cons rindex rindexes)
										    rvarid)
										 '()))
								 exports))))))))
					'()))
			exports)))))))
   
   (with-access::J2SProgram this (imports exports)
      (append (append-map module-redirects-es6 imports)
	 (if (pair? exports)
	     (filter-map (lambda (x)
			    (when (isa? x J2SRedirect)
			       (with-access::J2SRedirect x (index import)
				  (with-access::J2SImport import (ipath)
				     `(with-access::JsModule %module (evars)
					 (vector-set! evars ,index
					     ,(importpath-evar ipath)))))))
		exports)
	     '()))))

;*---------------------------------------------------------------------*/
;*    j2s-module-exports ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-module-exports this::J2SProgram ctx)

   (define (export-from x)
      (with-access::J2SExport x (eprgm)
	 (with-access::J2SProgram eprgm (path)
	    path)))
   
   (define (j2s-export e::J2SExport)
      (with-access::J2SExport e (index from id alias loc decl)
	 (cond
	    ((isa? e J2SExportDefault)
	     `(js-evar-info ,(& alias this) '(,index) '()
		 #f))
	    ((not (isa? e J2SRedirect))
	     `(js-evar-info ,(& alias this) '(,index) '()
		 ,(decl-usage-has? decl '(assig))))
	    ((isa? e J2SRedirectNamespace)
	     (with-access::J2SRedirect e (export import)
		(with-access::J2SImport import (ipath)
		   (with-access::J2SImportPath ipath (index)
		      `(js-evar-info ,(& "*" this) '() '(,index) #f)))))
	    (else
	     (let loop ((x e)
			(rindexes '())
			(iindexes '()))
		(if (isa? x J2SRedirect)
		    (with-access::J2SRedirect x ((rindex index) export import ( id2 id))
		       (with-access::J2SImport import (ipath)
			  (with-access::J2SImportPath ipath ((iindex index))
			     (loop export
				(cons rindex rindexes)
				(cons iindex iindexes)))))
		    (with-access::J2SExport x ((rindex index) decl)
		       ;; when :ignore-unresolved-modules is #t,
		       ;; decl might be false
		       `(js-evar-info ,(& alias this)
			   ',(reverse (cons rindex rindexes))
			   ',(reverse iindexes)
			   ,(when decl (decl-usage-has? decl '(assig)))))))))))
   
   (with-access::J2SProgram this (exports imports path checksum)
      (let ((cs (j2s-program-checksum! this)))
	 (cond
	    ((pair? exports)
	     `(define %evars
		 (with-access::JsModule %module (evars exports checksum)
		    (set! checksum ,(j2s-program-checksum! this))
		    (set! exports (vector ,@(map j2s-export exports)))
		    ,@(if (pair? exports)
			  `((set! evars
			       (make-vector ,(length exports) (js-undefined))))
			  '())
		    evars)))
	    ((=fx cs 0)
	     #unspecified)
	    (else
	     `(with-access::JsModule %module (checksum)
		 (set! checksum ,cs)))))))

;*---------------------------------------------------------------------*/
;*    module-name ...                                                  */
;*---------------------------------------------------------------------*/
(define (module-name name)
   (let ((sym (string->symbol name)) )
      ;; rewrite Bigloo illegal module names
      (case sym
	 ((foreign) '__js_foreign)
	 ((t) '__js_t)
	 ((eval) '__js_eval)
	 (else sym))))

;*---------------------------------------------------------------------*/
;*    js-module/main ...                                               */
;*---------------------------------------------------------------------*/
(define (js-module/main this::J2SProgram ctx)
   (with-access::J2SProgram this (loc name)
      (epairify-deep loc
	 `(module ,(module-name name)
	     (eval (library hop) (library hopscript) (library nodejs))
	     (library hop hopscript nodejs)
	     (cond-expand (enable-libuv (library libuv)))
	     ,@(js-module-tls this ctx)
	     (main main)))))

;*---------------------------------------------------------------------*/
;*    js-program-tls ...                                               */
;*---------------------------------------------------------------------*/
(define (js-program-tls this::J2SProgram ctx)
   (with-access::J2SProgram this (globals decls headers)
      (append '(%this %module %scope %cnst-table)
	 (map (lambda (g)
		 (match-case g
		    ((define ?var ?-) var)
		    (else (error "js-program" "wrong global" g))))
	    globals)
	 (filter-map (lambda (g)
			(when (isa? g J2SDecl)
			   (with-access::J2SDecl g (id scope)
			      (when (eq? scope 'tls)
				 (j2s-decl-scm-id g ctx)))))
	    (append headers decls)))))

;*---------------------------------------------------------------------*/
;*    js-module-tls ...                                                */
;*---------------------------------------------------------------------*/
(define (js-module-tls this::J2SProgram ctx)
   (let ((tls (js-program-tls this ctx)))
      `((cond-expand
	   ((and enable-tls (not bigloo-eval))
	    (static ,@tls)))
	(cond-expand
	   ((and enable-tls (not bigloo-eval))
	    (pragma ,@(map (lambda (g) `(,g thread-local)) tls)))))))

;*---------------------------------------------------------------------*/
;*    js-declare-tls ...                                               */
;*---------------------------------------------------------------------*/
(define (js-declare-tls this::J2SProgram ctx)
   (let ((tls (js-program-tls this ctx)))
      (map (lambda (g) `(declare-tls ,g)) tls)))

;*---------------------------------------------------------------------*/
;*    js-define-tls ...                                                */
;*---------------------------------------------------------------------*/
(define (js-define-tls this::J2SProgram ctx)
   (with-access::J2SProgram this (globals)
      (map (lambda (g)
	      (match-case g
		 ((define ?var ?val) `(define-tls ,var ,val))
		 (else (error "js-program" "wrong global" g))))
	 globals)))

;*---------------------------------------------------------------------*/
;*    js-module ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-module this::J2SProgram ctx)
   (with-access::J2SProgram this (loc name)
      (epairify-deep loc
	 `(module ,(module-name name)
	     (library hop hopscript js2scheme nodejs)
	     (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject))
	     ,@(js-module-tls this ctx)))))

;*---------------------------------------------------------------------*/
;*    js-wait-worker ...                                               */
;*---------------------------------------------------------------------*/
(define (js-wait-worker worker)
   `(thread-join! ,worker))<
	   
;*---------------------------------------------------------------------*/
;*    profilers ...                                                    */
;*---------------------------------------------------------------------*/
(define (profilers this::J2SProgram ctx)
   (when (or (context-get ctx :profile-call #f)
	     (context-get ctx :profile-cmap #f)
	     (context-get ctx :profile-cache #f)
	     (context-get ctx :profile-hint #f)
	     (context-get ctx :profile-alloc #f))
      `(js-profile-init ',(cons* :hash (j2ssum this)
			     (filter-config (context-conf ctx)))
	  ,(cond
	      ((and (context-get ctx :profile-call #f)
		    (context-get ctx :profile-cmap #f))
	       '(vector %source %call-log %cmap-log %call-locations))
	      ((context-get ctx :profile-call #f)
	       '(vector %source %call-log #f %call-locations))
	      ((context-get ctx :profile-cmap #f)
	       '(vector %source #f %cmap-log %call-locations))
	      (else
	       #f))
	  ,(when (context-get ctx :profile-symbols #f)
	      `',(profile-symbols this))
	  ,(with-access::J2SProgram this (path)
	      (context-get ctx :filename path)))))

;*---------------------------------------------------------------------*/
;*    filter-config ...                                                */
;*---------------------------------------------------------------------*/
(define (filter-config conf)
   (let loop ((conf conf)
	      (res '()))
      (cond
	 ((null? conf)
	  (reverse! res))
	 ((or (number? (cadr conf))
	      (string? (cadr conf))
	      (boolean? (cadr conf)))
	  (loop (cddr conf) (cons* (cadr conf) (car conf) res)))
	 (else
	  (loop (cddr conf) res)))))
   
;*---------------------------------------------------------------------*/
;*    exit-body ...                                                    */
;*---------------------------------------------------------------------*/
(define (exit-body ctx . body)
   (cond
      ((not (pair? body))
       '())
      ((context-get ctx :module-main)
       `((define (%jsexit n) (exit n))
	 (let ()
	    ,@(apply append body))))
      ((context-get ctx :return-as-exit)
       `((bind-exit (%jsexit) #unspecified ,@(apply append body))))
      (else
       (let ((seq (apply append body)))
	  (if (null? seq)
	      '()
	      `((let () ,@seq)))))))

;*---------------------------------------------------------------------*/
;*    %cnst-table ...                                                  */
;*---------------------------------------------------------------------*/
(define (%cnst-table cnsts mode return ctx)
   
   (define (%cnst-table-debug cnsts)
      `(js-constant-init (js-cnst-table)
	  (vector
	     ,@(map (lambda (n)
		       (let ((s (j2s-scheme n mode return ctx)))
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
	 (cond
	    ((isa? val J2SString)
	     (with-access::J2SLiteralValue val (val)
		(if (eq? (string-minimal-charset val) 'ascii)
		    (vector 2 val)
		    (vector 3 val (utf8-codeunit-length val)))))
	    ((isa? val J2SLiteralValue)
	     (with-access::J2SLiteralValue val (val)
		(vector 1 val)))
	    (else
	     (with-access::J2SLiteralCnst val (index)
		(vector 0 index))))))
   
   (define (%cnst-table-intext cnsts)
      
      (define (j2s-constant this::J2SExpr)
	 (cond
	    ((isa? this J2SString)
	     (with-access::J2SString this (val)
		(if (eq? (string-minimal-charset val) 'ascii)
		    (vector 6 val)
		    (vector 9 val (utf8-codeunit-length val)))))
	    ((isa? this J2SRegExp)
	     (with-access::J2SRegExp this (loc val flags inline)
		(vector (if inline 5 4) val flags loc)))
	    ((isa? this J2SRegExp)
	     (with-access::J2SRegExp this (loc val flags inline)
		(vector (if inline 3 1) val flags)))
	    ((isa? this J2SCmap)
	     (with-access::J2SCmap this (val ctor)
		(vector (if ctor 2 10)
		   (vector-map (lambda (e) (cons (symbol->string (car e)) (cdr e)))
		      val))))
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
   (if (>fx (context-get ctx :debug 0) 0)
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
      (call-default-walker)
      (when (>=fx profid 0)
	 (match-case loc
	    ((at ?- ?point)
	     (vector-set! vec profid point)))))
   vec)

;*---------------------------------------------------------------------*/
;*    profile-symbols ...                                              */
;*---------------------------------------------------------------------*/
(define (profile-symbols this::J2SProgram)
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
	     (with-access::J2SBlock body (endloc)
		(cons (list (caddr loc) 'fun id (caddr endloc))
		   (collect-functions* body)))))))

;*---------------------------------------------------------------------*/
;*    collect-functions* ::J2SAssig ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-functions* this::J2SAssig)
   (with-access::J2SAssig this (lhs rhs loc)
      (let loop ((rhs rhs))
	 (cond
	    ((isa? rhs J2SFun)
	     (with-access::J2SFun rhs (body)
		(with-access::J2SBlock body (endloc)
		   (cons (list (+fx 2 (caddr loc)) 'fun (expr->id lhs) (caddr endloc))
		      (collect-functions* body)))))
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
		 (with-access::J2SDecl decl (id (target loc))
		    (cons (list (caddr loc) 'call id (caddr target))
		       (call-default-walker)))
		 (call-default-walker)))
	  (call-default-walker))))
	       
;*---------------------------------------------------------------------*/
;*    collect-fuctions* ::J2SFun ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-functions* this::J2SFun)
   (with-access::J2SFun this (loc body)
      (with-access::J2SBlock body (endloc)
	 (cons (list (caddr loc) 'fun '%%anonymous (caddr endloc))
	    (collect-functions* body)))))

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
(define (j2s-profile-cache this::J2SProgram)
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
   (with-access::J2SCall this (fun args thisargs cache loc)
      (if (isa? fun J2SAccess)
	  (begin
	     (profile-access fun table 'get)
	     (when cache
		(profile-access fun table 'call cache)))
	  (profile-cache-info-init fun table))
      (for-each (lambda (a) (profile-cache-info-init a table)) args)
      (for-each (lambda (a) (profile-cache-info-init a table)) thisargs)))

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
;*---------------------------------------------------------------------*/
;*    j2s-regexp-caches-init ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-regexp-caches-init this::J2SProgram)
   (with-access::J2SProgram this (rxcache-size)
      `(js-init-regexp-caches! %this ,rxcache-size)))
   
