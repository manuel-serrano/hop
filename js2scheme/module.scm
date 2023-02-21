;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/module.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 15 15:16:16 2018                          */
;*    Last change :  Tue Feb 21 08:03:40 2023 (serrano)                */
;*    Copyright   :  2018-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    ES6 Module handling                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_module

   (library web hop)
   
   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_hop-module)

   (export j2s-module-stage))

;*---------------------------------------------------------------------*/
;*    j2s-module-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-module-stage
   (instantiate::J2SStageProc
      (name "module")
      (comment "Handle es module export and import clauses")
      (proc j2s-esmodule)
      (optional #f)))

;*---------------------------------------------------------------------*/
;*    hop-node-modules-dir ...                                         */
;*---------------------------------------------------------------------*/
(define-macro (hop-node-modules-dir)
   (call-with-input-file "./hopc_config.sch"
      (lambda (p)
	 (let loop ((e (read p)))
	    (if (eof-object? e)
		(error "hop-node-modules-dir" "Cannot find node_modules dir" 
		   "./hopc_config.sch")
		(let ((v (eval e)))
		   (if (pair? v)
		       (let ((dir (assq '--node_modules v)))
			  (if (pair? dir)
			      (cdr dir)
			      (loop (read p))))
		       (loop (read p)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-esmodule ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-esmodule this args)
   (when (isa? this J2SProgram)
      (let ((env (config-get args :module-env (make-cell '()))))
	 (esimport this this env args)
	 (esexport this this env args)))
   this)

;*---------------------------------------------------------------------*/
;*    esimport ...                                                     */
;*---------------------------------------------------------------------*/
(define (esimport this::J2SProgram prgm::J2SProgram env args)
   (with-access::J2SProgram this (imports decls path loc path)
      ;; collect all the imported modules
      (let ((env (config-get args :module-env (make-cell '())))
	    (stack (config-get args :module-stack '()))
	    (ip (instantiate::J2SImportPath
		   (loc loc)
		   (name path)
		   (path path)
		   (abspath (prgm-abspath this))
		   (import #f)
		   (protocol 'file))))
	 (env-add! (prgm-abspath this) (cons this ip) env)
	 (set! imports (collect-imports* this (prgm-dirname this) env args))
	 ;; declare all the imported variables
	 (set! decls (append (collect-decls* this env args) decls)))))

;* {*---------------------------------------------------------------------*} */
;* {*    esimport ::J2SImportDynamic ...                                  *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (esimport this::J2SImportDynamic prgm::J2SProgram stack args) */
;*    (with-access::J2SProgram prgm (path)                             */
;*       (with-access::J2SImportDynamic this (base)                    */
;* 	 (set! base path)                                              */
;* 	 (call-default-walker))))                                      */

;*---------------------------------------------------------------------*/
;*    collect-imports* ...                                             */
;*    -------------------------------------------------------------    */
;*    Collect recursively all imported modules. Update J2SImport       */
;*    object so that RESPATH is the absolute imported module           */
;*    and IPRGM the J2SProgram object associated with the imported     */
;*    module.                                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-imports* this::J2SNode dirname env args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-imports* ::J2SProgram ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-imports* this::J2SProgram dirname env args)
   (with-access::J2SProgram this (imports exports decls nodes path)
      (let ((imports (delete-duplicates
			(append-map (lambda (n)
				       (collect-imports* n dirname env args))
			   (append decls nodes))
			eq?)))
	 (for-each (lambda (i n)
		      (with-access::J2SImportPath i (index)
			 (set! index n)))
	    imports (iota (length imports)))
	 imports)))

;*---------------------------------------------------------------------*/
;*    collect-imports* ::J2SImport ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-imports* this::J2SImport dirname env args)
   
   (define (server-side-import this dirname env args)
      (with-access::J2SImport this (ipath path lang loc iprgm)
	 (let* ((resv (resolve-module-file path dirname loc args))
		(abspath (car resv))
		(protocol (cdr resv)))
	    (let ((old (env-get abspath env)))
	       (cond
		  (old
		   (with-access::J2SImport this (iprgm ipath (rpath path))
		      (set! iprgm (car old))
		      (set! ipath (duplicate::J2SImportPath (cdr old)
				     (import this)
				     (path rpath)))
		      (list ipath)))
		  ((eq? protocol 'core)
		   (let ((prgm (open-string-hashtable-get core-modules path))
			 (ip (instantiate::J2SImportPath
				(loc loc)
				(name path)
				(path path)
				(abspath abspath)
				(protocol protocol)
				(import this))))
		      (set! iprgm prgm)
		      (set! lang 'javascript)
		      (set! ipath ip)
		      (env-add! abspath (cons prgm ip) env)
		      (list ip)))
		  (else
		   (let* ((prgm (instantiate::J2SProgram
				   (loc loc)
				   (endloc loc)
				   (path path)
				   (nodes '())))
			  (ip (instantiate::J2SImportPath
				 (loc loc)
				 (name path)
				 (path path)
				 (abspath abspath)
				 (protocol protocol)
				 (import this)))
			  (lg (or lang (path-lang abspath))))
		      (set! iprgm prgm)
		      (set! lang lg)
		      (set! ipath ip)
		      (env-add! abspath (cons prgm ip) env)
		      (import-module abspath prgm lang loc env args)
		      (list ip))))))))

   (define (client-side-import this dirname env args)
      '())
      
   (with-access::J2SImport this (ipath path dollarpath lang loc iprgm)
      (if (isa? dollarpath J2SUndefined)
	  (server-side-import this dirname env args)
	  (client-side-import this dirname env args))))

;*---------------------------------------------------------------------*/
;*    collect-imports* ::J2SRedirect ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-imports* this::J2SRedirect dirname env args)
   (with-access::J2SRedirect this (import)
      (collect-imports* import dirname env args)))

;*---------------------------------------------------------------------*/
;*    collect-decls* ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-decls* this::J2SNode env args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-decls* ::J2SImport ...                                   */
;*---------------------------------------------------------------------*/
(define-method (collect-decls* this::J2SImport env args)
   
   (define (find-redirect id::symbol export import::J2SImport iprgm::J2SProgram)
      (with-access::J2SRedirect export (loc import alias)
	 (with-access::J2SImport import (iprgm path ipath)
	    (let ((iname (instantiate::J2SImportName
			    (loc loc)
			    (id id)
			    (alias alias)))
		  (import (duplicate::J2SImport import
			     (path path)
			     (ipath ipath)
			     (names (list id)))))
	       (find-export id import iprgm loc)))))
   
   (define (find-export id::symbol import::J2SImport iprgm::J2SProgram loc)
      (with-access::J2SProgram iprgm (exports path)
	 (let loop ((exports exports))
	    (when (pair? exports)
	       (with-access::J2SExport (car exports) (alias (name id))
		  (cond
		     ((isa? (car exports) J2SRedirectNamespace)
		      (with-access::J2SRedirect (car exports) (import export)
			 (with-access::J2SImport import (iprgm)
			    (multiple-value-bind (x i)
			       (find-redirect id (car exports) import iprgm)
			       (if x
				   (duplicate::J2SRedirectNamespace (car exports)
				      (export x))
				   (loop (cdr exports)))))))
		     ((not (eq? id alias))
		      (loop (cdr exports)))
		     ((not (isa? (car exports) J2SRedirect))
		      (with-access::J2SImport import (respath)
			 (values (car exports) import)))
		     (else
		      (with-access::J2SRedirect (car exports) (import export)
			 (with-access::J2SImport import (iprgm)
			    (let ((x (find-redirect name (car exports) import iprgm)))
			       (set! export x)
			       (car exports)))))))))))
   
   (define (import-binding name import::J2SImport)
      (with-access::J2SImport import (iprgm loc ipath)
	 (with-access::J2SImportName name (id alias)
	    (multiple-value-bind (x i)
	       (find-export id this iprgm loc)
	       (if (not x)
		   (with-access::J2SImportPath ipath (abspath)
		      (raise
			 (instantiate::&io-parse-error
			    (proc "import")
			    (msg (format "imported binding \"~a\" not exported by module ~s"
				    id abspath))
			    (obj id)
			    (fname (cadr loc))
			    (location (caddr loc))))   )
		   (instantiate::J2SDeclImport
		      (loc loc)
		      (id alias)
		      (alias id)
		      (binder 'let)
		      (writable #f)
		      (vtype 'any)
		      (scope 'local)
		      (export x)
		      (import import)))))))
   
   (define (import-namespace name import::J2SImport)
      (with-access::J2SImport import (loc)
	 (with-access::J2SImportName name (alias)
	    (instantiate::J2SDeclInit
	       (loc loc)
	       (id alias)
	       (binder 'let-opt)
	       (scope 'global)
	       (writable #f)
	       (val (instantiate::J2SImportNamespace
		       (loc loc)
		       (id alias)
		       (type 'object)
		       (import this)))))))
   
   (with-access::J2SImport this (iprgm names loc ipath dollarpath)
      (if (isa? dollarpath J2SUndefined)
	  (map (lambda (name)
		  (with-access::J2SImportName name (id)
		     (if (eq? id '*)
			 (import-namespace name this)
			 (import-binding name this))))
	     names)
	  '())))

;*---------------------------------------------------------------------*/
;*    esexport ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SNode prgm env args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    esexport ::J2SProgram ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SProgram prgm env args)
   (with-access::J2SProgram this (nodes decls exports loc path exports)
      ;; a single index must be associated per variable, even for
      ;; variables exported under multiple names
      (let ((env (map (lambda (e n)
			 (with-access::J2SExport e (id)
			    (cons id n)))
		    exports (iota (length exports)))))
	 (for-each (lambda (e)
		      (with-access::J2SExport e (id index eprgm)
			 (set! eprgm prgm)
			 (set! index (cdr (assq id env)))))
	    exports))
      (for-each (lambda (o) (esexport o this env args)) nodes)
      (for-each (lambda (o) (esexport o this env args)) decls)
      (for-each (lambda (o) (esexport o this env args)) exports))
   this)

;*---------------------------------------------------------------------*/
;*    esexport ::J2SRedirect ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SRedirect prgm env args)
   
   (define (find-export name::symbol iprgm::J2SProgram loc)
      (with-access::J2SProgram iprgm (exports path)
	 (let loop ((exports exports))
	    (when (pair? exports)
		(with-access::J2SExport (car exports) (alias id loc)
		   (cond
		      ((isa? (car exports) J2SRedirectNamespace)
		       (with-access::J2SRedirect (car exports) (import export)
			  (with-access::J2SImport import (iprgm)
			     (let ((x (find-export name iprgm loc)))
				(if x
				    (duplicate::J2SRedirectNamespace (car exports)
				       (export x))
				    (loop (cdr exports)))))))
		      ((not (eq? name alias))
		       (loop (cdr exports)))
		      ((not (isa? (car exports) J2SRedirect))
		       (car exports))
		      (else
		       (with-access::J2SRedirect (car exports) (import export)
			  (with-access::J2SImport import (iprgm)
			     (let ((x (find-export id iprgm loc)))
				(set! export x)
				(car exports)))))))))))
   
   (with-access::J2SRedirect this (rindex ipath import loc id index export)
      (with-access::J2SImport import (iprgm ipath)
	 (let ((x (find-export id iprgm loc)))
	    (if x
		(set! export x)
		(with-access::J2SImportPath ipath (abspath)
		   (raise
		      (instantiate::&io-parse-error
			 (proc "export")
			 (msg (format "imported binding \"~a\" not exported by module ~s"
				 id abspath))
			 (obj id)
			 (fname (cadr loc))
			 (location (caddr loc)))))))
	 this)))

;*---------------------------------------------------------------------*/
;*    esexport ::J2SRedirectNamespace ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SRedirectNamespace prgm env args)
   this)

;*---------------------------------------------------------------------*/
;*    resolve-module-file ...                                          */
;*    -------------------------------------------------------------    */
;*    Almost similar to nodejs's resolve method (see                   */
;*    nodejs/require.scm).                                             */
;*---------------------------------------------------------------------*/
(define (resolve-module-file name::bstring dir::bstring loc args)

   (define (resolve-autoload-hz hz)
      (cond
	 ((hz-local-weblet-path hz (get-autoload-directories))
	  => resolve-directory)
	 ((hz-cache-path hz)
	  => resolve-directory)
	 ((and (null? (hop-hz-repositories))
	       (not (or (string-prefix? "http://" hz)
			(string-prefix? "https://" hz))))
	  (raise
	     (instantiate::&io-error
		(proc "import")
		(msg "No local repository provided (set HOP_HZ_REPOSITORY or hopcrc.js)")
		(obj hz)
		(fname (cadr loc))
		(location (caddr loc)))))
	 (else
	  (let ((dir (hz-download-to-cache hz (hop-hz-repositories))))
	     (if (directory? dir)
		 (resolve-directory dir)
		 #f)))))
   
   (define (resolve-hz hz)
      (let ((dir (hz-download-to-cache hz (hop-hz-repositories))))
	 (if (directory? dir)
	     (resolve-directory dir)
	     #f)))
   
   (define (resolve-file x)
      (cond
	 ((string-suffix? ".hz" x)
	  (cond
	     ((not (file-exists? x))
	      (resolve-autoload-hz x))
	     ((not (directory? x))
	      (resolve-hz x))
	     (else
	      #f)))
	 ((and (file-exists? x) (not (directory? x)))
	  (cons (file-name-canonicalize x) 'file))
	 (else
	  (let loop ((sufs '(".js" ".mjs" ".hop" ".ts" ".so" ".json" ".hss" ".css")))
	     (when (pair? sufs)
		(let* ((suffix (car sufs))
		       (src (string-append x suffix)))
		   (if (and (file-exists? src) (not (directory? src)))
		       (cons (file-name-canonicalize src) 'file)
		       (loop (cdr sufs)))))))))

   (define (resolve-package pkg dir)
      (with-handler
	 (lambda (e)
	    (with-access::&exception e (fname location)
	       (unless (and fname location)
		  (set! fname (cadr loc))
		  (set! location (caddr loc)))
	       (raise e)))
	 (call-with-input-file pkg
	    (lambda (ip)
	       (let* ((o (json-parse ip
			    :array-alloc (lambda ()
					    (make-cell '()))
			    :array-set (lambda (a i val)
					  (cell-set! a (cons val (cell-ref a))))
			    :array-return (lambda (a i)
					     (reverse! (cell-ref a)))
			    :object-alloc (lambda () (make-cell '()))
			    :object-set (lambda (o p val)
					   (cell-set! o
					      (cons (cons p val)
						 (cell-ref o))))
			    :object-return (lambda (o) (cell-ref o))
			    :parse-error (lambda (msg path loc)
					    (raise
					       (instantiate::&io-parse-error
						  (proc "resolve")
						  (msg msg)
						  (obj path)
						  (fname (cadr loc))
						  (location (caddr loc)))))))
		      (m (assoc "main" o)))
		  (if (pair? m)
		      (cdr m)
		      (let ((idx (make-file-name dir "index.js")))
			 (when (file-exists? idx)
			    idx))))))))
   
   (define (resolve-directory x)
      (let ((json (make-file-name x "package.json")))
	 (or (and (file-exists? json)
		  (let ((m (resolve-package json x)))
		     (when (string? m)
			(resolve-file-or-directory m x))))
	     (let ((p (make-file-name x "index.js")))
		(when (file-exists? p)
		   (cons (file-name-canonicalize p) 'file))))))

   (define (resolve-path-file-or-directory path)
      (or (resolve-file path)
	  (resolve-directory path)))
   
   (define (resolve-file-or-directory x dir)
      (let ((path (make-file-name dir x)))
	 (resolve-path-file-or-directory path)))
   
   (define (resolve-error x)
      (raise
	 (instantiate::&io-file-not-found-error
	    (proc "hopc:resolve")
	    (msg (format "Cannot find module in ~s" dir))
	    (obj name)
	    (fname (cadr loc))
	    (location (caddr loc)))))

   (define (core-module? name)
      (open-string-hashtable-get (get-core-modules) name))

   (define hop-modules-path
      (let* ((name (config-get args :source (config-get args :filename #f)))
	     (file (if (string-prefix? "/" name)
		       name
		       (file-name-canonicalize (make-file-name (pwd) name))))
	     (dir (if (string? file) (dirname file) (pwd))))
	 (cons* dir
	    (make-file-name dir "node_modules")
	    (config-get args :hop-library-path ".")
	    (list (config-get args :node-modules-directory ".")))))
   
   (define (resolve-modules name)
      (any (lambda (dir)
	      (resolve-file-or-directory name dir))
	 (filter string? hop-modules-path)))

   (let loop ((name name))
      (cond
	 ((core-module? name)
	  (cons name 'core))
	 ((or (string-prefix? "http://" name)
	      (string-prefix? "https://" name))
	  (cons name 'http))
	 ((string-prefix? "hop:" name)
	  (let ((dir (or (config-get args :node-modules-directory)
			 (hop-node-modules-dir))))
	     (loop (make-file-path dir (substring name 4)))))
	 ((or (string-prefix? "./" name) (string-prefix? "../" name))
	  (or (resolve-file-or-directory name dir)
	      (resolve-modules name)
	      (resolve-error name)))
	 ((string-prefix? "/" name)
	  (or (resolve-path-file-or-directory name)
	      (resolve-modules name)
	      (resolve-error name)))
	 (else
	  (or (resolve-modules name)
	      (resolve-error name))))))

;*---------------------------------------------------------------------*/
;*    module-cache                                                     */
;*---------------------------------------------------------------------*/
(define module-cache #f)

;*---------------------------------------------------------------------*/
;*    module-cache-get ...                                             */
;*---------------------------------------------------------------------*/
(define (module-cache-get path)
   (when module-cache
      (let ((ce (cache-get module-cache path)))
	 (when ce
	    (with-access::cache-entry ce (value)
	       value)))))

;*---------------------------------------------------------------------*/
;*    module-cache-put! ...                                            */
;*---------------------------------------------------------------------*/
(define (module-cache-put! path iprgm)
   (unless module-cache
      (set! module-cache
	 (instantiate::cache-memory
	    (max-file-size #e-1))))
   (cache-put! module-cache path iprgm)
   iprgm)

;*---------------------------------------------------------------------*/
;*    import-module ...                                                */
;*---------------------------------------------------------------------*/
(define (import-module path prgm lang loc env args)
   
   (define (assign! tgt src)
      (vector-for-each (lambda (f)
			  (if (class-field-mutator f)
			      (let ((v ((class-field-accessor f) src)))
				 ((class-field-mutator f) tgt v))))
	 (class-all-fields (object-class src)))
      tgt)

   (assign! prgm
      (or (module-cache-get path)
	  (with-handler
	     (lambda (e)
		(when (isa? e &exception)
		   (with-access::&exception e (fname location)
		      (unless (and fname location)
			 (set! fname (cadr loc))
			 (set! location (caddr loc)))))
		(raise e))
	     (call-with-input-file path
		(lambda (in)
		   (let ((margin (string-append
				    (config-get args :verbmargin "")
				    "     "))
			 (verb (config-get args :verbose 0)))
		      (when (>= verb 3)
			 (fprint (current-error-port) "\n" margin
			    path
			    (if (>= (config-get args :verbose 0) 4)
				(string-append " [" path "]")
				"")))
		      (let ()
			 (let ((iprgm (case lang
					 ((hop)
					  (hop-compile in
					     :source path
					     :verbose (if (<=fx verb 2) 0 verb)
					     :verbose 20
					     :verbmargin margin
					     :module-import #t
					     :module-env env
					     :import-program prgm
					     :import-loc loc))
					 (else
					  ;; Warning: ;; currently, typescript
					  ;; type is not imported. This should
					  ;; be supported in the future. For
					  ;; that, the imported modules must
					  ;; be compiled with something similar
					  ;; to compile-language@hopc_driver
					  (j2s-compile in
					     :source path
					     :driver (j2s-export-driver)
					     :warning 0
					     :module-import #t
					     :verbose (if (<=fx verb 2) 0 verb)
					     :verbmargin margin
					     :module-env env
					     :import-program prgm
					     :import-loc loc
					     :commonjs-export #t
					     :node-modules-directory (config-get args :node-modules-directory)
					     :language (symbol->string lang)
					     :plugins-loader (config-get args :plugins-loader #f))))))
			    (module-cache-put! path iprgm))))))))))

;*---------------------------------------------------------------------*/
;*    core-module-list ...                                             */
;*---------------------------------------------------------------------*/
(define core-module-list
   '("console" "constants" "util" "sys" "path" "_linklist" "events"
     "assert" "_stream_readable" "_stream_writable" "_stream_duplex"
     "_stream_transform" "_stream_passthrough" "stream" "fs"
     "punycode" "dgram" "vm" "timers" "net" "querystring" "string_decoder"
     "child_process" "cluster" "crypto" "dns" "domain" "freelist" "url"
     "tls" "tty" "http" "https" "zlib" "os" "hop" "hophz" "node_tick"
     "node_stdio" "node_proc" "node_timers" "node_cluster"))

;*---------------------------------------------------------------------*/
;*    core-modules ...                                                 */
;*---------------------------------------------------------------------*/
(define core-modules #f)

;*---------------------------------------------------------------------*/
;*    get-core-modules ...                                             */
;*---------------------------------------------------------------------*/
(define (get-core-modules)
   (unless core-modules
      (esimport-init-core-modules!))
   core-modules)
	   
;*---------------------------------------------------------------------*/
;*    esimport-init-core-modules! ...                                  */
;*---------------------------------------------------------------------*/
(define (esimport-init-core-modules!)
   
   (define (read-mo path)
      (call-with-input-file path
	 (lambda (ip)
	    (string->obj (read ip)))))
   
   (set! core-modules   
      (create-hashtable
	 :weak 'open-string
	 :size 64
	 :max-length 4096
	 :max-bucket-length 10))
   (for-each (lambda (cm)
		(let ((loc `(at ,(string-append cm ".js") 0))
		      (mo (make-file-path (hop-lib-directory)
			     "hop" (hop-version) "mo"
			     (string-append cm ".mod.mo"))))
		   (co-instantiate ((decl (instantiate::J2SDecl
					     (id 'default)
					     (loc loc)
					     (vtype 'any)
					     (export expo)
					     (binder 'let-opt)
					     (scope 'export)))
				    (expo (instantiate::J2SExport
					     (loc loc)
					     (id 'default)
					     (alias 'default)
					     (index 0)
					     (decl decl))))
		      (hashtable-put! core-modules cm
			 (if (file-exists? mo)
			     (read-mo mo)
			     (instantiate::J2SProgram
				(loc loc)
				(endloc loc)
				(path cm)
				(mode 'core)
				(nodes '())
				(exports (list expo))))))))
      core-module-list))

;*---------------------------------------------------------------------*/
;*    path-lang ...                                                    */
;*---------------------------------------------------------------------*/
(define (path-lang path)

   (define (string->lang suf)
      (cond
	 ((member suf '("js" "mjs")) 'js)
	 ((string=? suf "hop") 'hop)
	 ((string=? suf "json") 'json)
	 ((member suf '("html" "xml")) 'hopscript)
	 ((string=? suf "ts") 'typescript)
	 (else 'js)))

   (let ((suf (suffix path)))
	 (if (string-null? suf)
	     'js
	     (string->lang suf))))

;*---------------------------------------------------------------------*/
;*    resolve-lang ...                                                 */
;*---------------------------------------------------------------------*/
(define (resolve-lang respath::J2SImportPath)
   (with-access::J2SImportPath respath (abspath)
      (path-lang abspath)))

;*---------------------------------------------------------------------*/
;*    env-get ...                                                      */
;*---------------------------------------------------------------------*/
(define (env-get path env)
   (let ((old (assoc path (cell-ref env))))
      (when old (cdr old))))

;*---------------------------------------------------------------------*/
;*    env-add! ...                                                     */
;*---------------------------------------------------------------------*/
(define (env-add! path prgmip env)
   (cell-set! env (cons (cons path prgmip) (cell-ref env))))
   
;*---------------------------------------------------------------------*/
;*    prgm-dirname ...                                                 */
;*---------------------------------------------------------------------*/
(define (prgm-dirname prgm::J2SProgram)
   (with-access::J2SProgram prgm (path)
      (cond
	 ((string=? path "") (pwd))
	 ((char=? (string-ref path 0) (file-separator)) (dirname path))
	 (else (dirname (file-name-canonicalize (make-file-name (pwd) path)))))))

;*---------------------------------------------------------------------*/
;*    prgm-abspath ...                                                 */
;*---------------------------------------------------------------------*/
(define (prgm-abspath prgm::J2SProgram)
   (with-access::J2SProgram prgm (path)
      (cond
	 ((string=? path "") (pwd))
	 ((char=? (string-ref path 0) (file-separator)) (dirname path))
	 (else (file-name-canonicalize (make-file-name (pwd) path))))))

;*---------------------------------------------------------------------*/
;*    absolute-path ...                                                */
;*---------------------------------------------------------------------*/
(define (absolute-path path base)
   (cond
      ((string=? path "") base)
      ((char=? (string-ref path 0) (file-separator)) path)
      (else (file-name-canonicalize (make-file-name base path)))))
   
