;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/module.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 15 15:16:16 2018                          */
;*    Last change :  Mon May 10 11:33:20 2021 (serrano)                */
;*    Copyright   :  2018-21 Manuel Serrano                            */
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
;*    j2s-esmodule ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-esmodule this args)
   (when (isa? this J2SProgram)
      (esimport this this (config-get args :module-stack '()) args)
      (esexport this this))
   this)

;*---------------------------------------------------------------------*/
;*    core-module-list ...                                             */
;*---------------------------------------------------------------------*/
(define core-module-list
   '("console"
     "constants"
     "util"
     "sys"
     "path"
     "_linklist"
     "events"
     "assert"
     "_stream_readable"
     "_stream_writable"
     "_stream_duplex"
     "_stream_transform"
     "_stream_passthrough"
     "stream"
     "fs"
     "punycode"
     "dgram"
     "vm"
     "timers"
     "net"
     "querystring"
     "string_decoder"
     "child_process"
     "cluster"
     "crypto"
     "dns"
     "domain"
     "freelist"
     "url"
     "tls"
     "tty"
     "http"
     "https"
     "zlib"
     "os"
     "hop"
     "hophz"
     "node_tick"
     "node_stdio"
     "node_proc"
     "node_timers"
     "node_cluster"))

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
(define (module-cache-put! path export)
   (unless module-cache (set! module-cache (instantiate::cache-memory)))
   (cache-put! module-cache path export)
   export)

;*---------------------------------------------------------------------*/
;*    core-modules ...                                                 */
;*---------------------------------------------------------------------*/
(define core-modules #f)

;*---------------------------------------------------------------------*/
;*    esimport ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (esimport this::J2SNode prgm::J2SProgram stack args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    esimport ::J2SProgram ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (esimport this::J2SProgram prgm::J2SProgram stack args)
   (call-default-walker)
   (with-access::J2SProgram this (imports)
      (set! imports (reverse! imports))))

;*---------------------------------------------------------------------*/
;*    esimport ::J2SImport ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (esimport this::J2SImport prgm::J2SProgram stack args)
   
   (define (import-decl::pair-nil iprgm::J2SProgram name::J2SImportName)
      (with-access::J2SImportName name (id alias loc)
	 (with-access::J2SProgram iprgm (exports path mode)
	    (let ((expo (find (lambda (export)
				 (with-access::J2SExport export ((imp id))
				    (eq? id imp)))
			   exports)))
	       (cond
		  (expo
		   (with-access::J2SExport expo (decl from)
		      (with-access::J2SDecl decl (exports)
			 (map (lambda (export)
				 (with-access::J2SExport export (decl)
				    (with-access::J2SDecl decl (vtype)
				       (instantiate::J2SDeclImport
					  (loc loc)
					  (id alias)
					  (alias id)
					  (binder 'let)
					  (writable #f)
					  (vtype vtype)
					  (scope (if (eq? from 'hop) '%hop 'local))
					  (export export)
					  (import this)))))
			    exports))))
		  ((eq? mode 'core)
		   (co-instantiate ((decl (instantiate::J2SDecl
					     (id id)
					     (loc loc)
					     (vtype 'any)
					     (exports (list expo))))
				    (expo (instantiate::J2SExport
					     (id id)
					     (alias id)
					     (index 0)
					     (decl decl))))
		      (list (instantiate::J2SDeclImport
			       (loc loc)
			       (id alias)
			       (alias id)
			       (binder 'let)
			       (writable #f)
			       (vtype 'any)
			       (scope 'core)
			       (export expo)
			       (import this)))))
		  (else
		   (raise
		      (instantiate::&io-parse-error
			 (proc "import")
			 (msg (format "imported binding not exported by module ~s"
				 path))
			 (obj id)
			 (fname (cadr loc))
			 (location (caddr loc))))))))))
   
   (define (export-exports::J2SDecl prgm::J2SProgram id loc)
      (instantiate::J2SDeclInit
	 (loc loc)
	 (id id)
	 (binder 'let-opt)
	 (scope 'global)
	 (writable #f)
	 (val (instantiate::J2SImportExports
		 (loc loc)
		 (type 'object)
		 (import this)))))

   (define (redirect this::J2SImport prgm::J2SProgram iprgm::J2SProgram names)
      (with-access::J2SProgram prgm (exports imports path)
	 (with-access::J2SProgram iprgm ((iexports exports))
	    (set! exports
	       (append exports
		  (append-map (lambda (export)
				 (with-access::J2SExport export (id alias)
				    (filter-map (lambda (n)
						   (with-access::J2SImportRedirect n ((rid id) (ralias alias))
						      (when (eq? rid alias)
							 (duplicate::J2SExport export
							    (id id)
							    (alias ralias)
							    (from iprgm)))))
				       names)))
		     iexports)))))
      '())

   (define (reexport this::J2SImport prgm::J2SProgram iprgm::J2SProgram)
      (with-access::J2SProgram prgm (exports imports path)
	 (with-access::J2SProgram iprgm ((iexports exports))
	    (set! exports
	       (append exports
		  (filter-map (lambda (export)
				 (with-access::J2SExport export (id alias)
				    (unless (eq? id 'default)
				       (duplicate::J2SExport export
					  (from iprgm)))))
		     iexports)))))
      '())

   (define (import-module respath path loc)
      (or (open-string-hashtable-get core-modules respath)
	  (module-cache-get respath)
	  (with-handler
	     (lambda (e)
		(with-access::&exception e (fname location)
		   (unless (and fname location)
		      (set! fname (cadr loc))
		      (set! location (caddr loc))))
		(raise e))
	     (call-with-input-file respath
		(lambda (in)
		   (let ((margin (string-append
				    (config-get args :verbmargin "")
				    "     ")))
		      (when (>= (config-get args :verbose 0) 2)
			 (fprint (current-error-port) "\n" margin
			    path
			    (if (>= (config-get args :verbose 0) 3)
				(string-append " [" respath "]")
				"")))
		      (let ((iprgm (if (string-suffix? ".hop" respath)
				       (hop-compile in
					  :verbose (config-get args :verbose 0)
					  :verbmargin margin
					  :module-import #t
					  :module-stack (cons respath stack))
				       (j2s-compile in
					  :driver (j2s-export-driver)
					  :warning 0
					  :module-import #t
					  :verbose (config-get args :verbose 0)
					  :verbmargin margin
					  :module-stack (cons respath stack)))))
			 (module-cache-put! respath iprgm))))))))

   (define (import-module-decls this iprgm)
      (with-access::J2SImport this (names loc path)
	 (cond
	    ((null? names)
	     '())
	    ((isa? (car names) J2SImportNamespace)
	     (with-access::J2SImportNamespace (car names) (id)
		(list (export-exports iprgm id loc))))
	    ((isa? (car names) J2SImportRedirect)
	     (redirect this prgm iprgm names))
	    ((isa? (car names) J2SImportExport)
	     (reexport this prgm iprgm))
	    ((list? names)
	     (append-map (lambda (n) (import-decl iprgm n)) names))
	    (else
	     (raise
		(instantiate::&io-parse-error
		   (proc "import")
		   (msg "Illegal import")
		   (obj path)
		   (fname (cadr loc))
		   (location (caddr loc))))))))

   (unless core-modules
      (set! core-modules   
	 (create-hashtable
	    :weak 'open-string
	    :size 64
	    :max-length 4096
	    :max-bucket-length 10))
      (for-each (lambda (cm)
		   (let ((loc `(at ,(string-append cm ".js") 0)))
		      (co-instantiate ((decl (instantiate::J2SDecl
						(id 'default)
						(loc loc)
						(vtype 'any)
						(exports (list expo))))
				       (expo (instantiate::J2SExport
						(id 'default)
						(alias 'default)
						(index 0)
						(decl decl))))
			 (hashtable-put! core-modules cm
			    (instantiate::J2SProgram
			       (loc loc)
			       (endloc loc)
			       (path cm)
			       (mode 'core)
			       (nodes '())
			       (exports (list expo)))))))
	 core-module-list))
   
   (with-access::J2SProgram prgm ((src path) imports decls)
      (with-access::J2SImport this (path loc respath names iprgm)
	 (let ((base (cond
			((string=? src "")
			 (pwd))
			((char=? (string-ref src 0) #\/)
			 (dirname src))
			(else
			 (dirname
			    (file-name-canonicalize
			       (make-file-name (pwd) src)))))))
	    (set! respath (resolve-module-file path base loc))
	    (when (string=? respath src)
		(raise
		   (instantiate::&io-parse-error
		      (proc "import")
		      (msg "Illegal self-import")
		      (obj path)
		      (fname respath)
		      (location (caddr loc)))))
	    (unless (member respath stack)
	       (set! iprgm (import-module respath path loc))
	       (set! decls (append (import-module-decls this iprgm) decls)))
	    (set! imports (cons this imports))))))

;*---------------------------------------------------------------------*/
;*    esimport ::J2SImportDynamic ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (esimport this::J2SImportDynamic prgm::J2SProgram stack args)
   (with-access::J2SProgram prgm (path)
      (with-access::J2SImportDynamic this (base)
	 (set! base path)
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    esexport ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SNode prgm::J2SProgram)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    esexport ::J2SProgram ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SProgram prgm::J2SProgram)
   (with-access::J2SProgram this (nodes decls exports loc path)
      (for-each (lambda (o) (esexport o this)) nodes)
      (for-each (lambda (o) (esexport o this)) decls))
   this)

;*---------------------------------------------------------------------*/
;*    esexport ::J2SDecl ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SDecl prgm::J2SProgram)
   (with-access::J2SDecl this (exports program)
      (with-access::J2SProgram prgm ((allexports exports))
	 (set! allexports (append exports allexports))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    esexport ::J2SExportVars ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SExportVars prgm::J2SProgram)
   (with-access::J2SExportVars this (program aliases)
      (set! program prgm)))

;*---------------------------------------------------------------------*/
;*    resolve-module-file ...                                          */
;*    -------------------------------------------------------------    */
;*    Almost similar to nodejs's resolve method (see                   */
;*    nodejs/require.scm).                                             */
;*---------------------------------------------------------------------*/
(define (resolve-module-file name dir loc)
   
   (define (resolve-file x)
      (cond
	 ((and (file-exists? x) (not (directory? x)))
	  (file-name-canonicalize x))
	 (else
	  (let loop ((sufs '(".js" ".mjs" ".hop" ".so" ".json" ".hss" ".css")))
	     (when (pair? sufs)
		(let* ((suffix (car sufs))
		       (src (string-append x suffix)))
		   (if (and (file-exists? src) (not (directory? src)))
		       (file-name-canonicalize src)
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
		  (let* ((m (resolve-package json x)))
		     (cond
			((pair? m)
			 (cons name
			    (map (lambda (m)
				    (resolve-file-or-directory m x))
			       m)))
			((string? m)
			 (resolve-file-or-directory m x))
			(else
			 #f))))
	     (let ((p (make-file-name x "index.js")))
		(when (file-exists? p)
		   (file-name-canonicalize p))))))
   
   (define (resolve-file-or-directory x dir)
      (let ((file (make-file-name dir x)))
	 (or (resolve-file file)
	     (resolve-directory file))))
   
   (define (resolve-error x)
      (raise
	 (instantiate::&io-file-not-found-error
	    (proc "resolve")
	    (msg (format "Cannot find module in ~s" dir))
	    (obj name)
	    (fname (cadr loc))
	    (location (caddr loc)))))

   (define (core-module? name)
      (open-string-hashtable-get core-modules name))
   
   (cond
      ((core-module? name)
       name)
      ((or (string-prefix? "http://" name)
	   (string-prefix? "https://" name))
       name)
      ((or (string-prefix? "./" name) (string-prefix? "../" name))
       (or (resolve-file-or-directory name dir)
	   (resolve-error name)))
      ((string-prefix? "/" name)
       (or (resolve-file-or-directory name "/")
	   (resolve-error name)))
      (else
       (resolve-error name))))
