;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/module.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 15 15:16:16 2018                          */
;*    Last change :  Thu Oct 25 11:24:36 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
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
	   __js2scheme_utils)

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
	 (with-access::J2SProgram iprgm (exports path)
	    (let ((expo (find (lambda (export)
				 (with-access::J2SExport export ((imp id))
				    (eq? id imp)))
			   exports)))
	       (if expo
		   (with-access::J2SExport expo (decl)
		      (with-access::J2SDecl decl (exports)
			 (map (lambda (export)
				 (with-access::J2SExport export (index)
				    (instantiate::J2SDeclImport
				       (loc loc)
				       (id alias)
				       (alias id)
				       (binder 'let)
				       (writable #f)
				       (export export)
				       (import this))))
			    exports)))
		   (raise
		      (instantiate::&io-parse-error
			 (proc "import")
			 (msg (format "imported binding not exported by module ~s"
				 path))
			 (obj id)
			 (fname (cadr loc))
			 (location (caddr loc)))))))))
   
   (define (export-expr::J2SDecl prgm::J2SProgram id op loc)
      (instantiate::J2SDeclInit
	 (loc loc)
	 (id id)
	 (binder 'let-opt)
	 (scope 'global)
	 (writable #f)
	 (val (instantiate::J2SImportExpr
		 (loc loc)
		 (type (if (eq? op '*) 'object 'any))
		 (op op)
		 (import this)))))

   (define (redirect this::J2SImport prgm::J2SProgram iprgm::J2SProgram names)
      (with-access::J2SProgram prgm (exports imports path)
	 (with-access::J2SProgram iprgm ((iexports exports))
	    (set! exports
	       (append exports
		  (filter-map (lambda (export)
				 (with-access::J2SExport export (id alias)
				    (cond
				       ((null? names)
					(duplicate::J2SExport export
					   (from iprgm)))
				       ((assq alias names)
					=>
					(lambda (c)
					   (duplicate::J2SExport export
					      (id id)
					      (alias (cdr c))
					      (from iprgm))))
				       (else
					#f))))
		     iexports)))))
      '())

   (define (import-module respath path loc)
      (or (module-cache-get respath)
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
		      (let ((iprgm (j2s-compile in
				      :driver (j2s-export-driver)
				      :verbose (config-get args :verbose 0)
				      :verbmargin margin
				      :module-stack (cons respath stack))))
			 (module-cache-put! respath iprgm))))))))

   (define (import-module-decls this iprgm)
      (with-access::J2SImport this (names loc path)
	 (cond
	    ((and (pair? names) (memq (car names) '(* default)))
	     (list (export-expr iprgm (cdr names) (car names) loc)))
	    ((and (pair? names) (eq? (car names) 'redirect))
	     (redirect this prgm iprgm (cdr names)))
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
   
   (define (esexport-default-stmt loc)
      (J2SStmtExpr
	 (instantiate::J2SDefaultExport
	    (loc loc)
	    (expr (J2SAccess (J2SUnresolvedRef 'module)
		     (J2SString "exports"))))))
   
   (with-access::J2SProgram this (nodes decls exports loc defexport)
      (for-each (lambda (o) (esexport o this)) nodes)
      (for-each (lambda (o) (esexport o this)) decls)
      (unless defexport
	 ;; force a default export if non specified
	 (set! nodes (append nodes (list (esexport-default-stmt loc))))))
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
;*    esexport ::J2SDefaultExport ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SDefaultExport prgm::J2SProgram)
   (with-access::J2SDefaultExport this (loc expr)
      (with-access::J2SProgram prgm (defexport)
	 (if defexport
	     (raise
		(instantiate::&io-parse-error
		   (proc "export")
		   (msg "Duplicate export")
		   (obj #unspecified)
		   (fname (cadr loc))
		   (location (caddr loc))))
	     (begin
		(set! defexport this)
		(esexport expr prgm)
		this)))))

;*---------------------------------------------------------------------*/
;*    esexport ::J2SExportVars ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SExportVars prgm::J2SProgram)
   (with-access::J2SExportVars this (program aliases)
      (when (memq 'default aliases)
	 (with-access::J2SProgram prgm (%info)
	    (set! %info 'default)))
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
	  (let loop ((suffixes '(".js" ".hop" ".so" ".json" ".hss" ".css")))
	     (when (pair? suffixes)
		(let* ((suffix (car suffixes))
		       (src (string-append x suffix)))
		   (if (and (file-exists? src) (not (directory? src)))
		       (file-name-canonicalize src)
		       (loop (cdr suffixes)))))))))

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
   
   (cond
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
