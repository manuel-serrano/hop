;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/module.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 15 15:16:16 2018                          */
;*    Last change :  Sat Oct 20 08:43:14 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    ES6 Module handling                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_module

   (library web)
   
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
      (footer "")
      (comment "Handle es module export and import clauses")
      (proc j2s-esmodule)
      (optional #f)))

;*---------------------------------------------------------------------*/
;*    j2s-esmodule ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-esmodule this args)
   (when (isa? this J2SProgram)
      (esimport this this '() args)
      (esexport this this (make-cell 0) (make-cell #f))
      (with-access::J2SProgram this (imports path nodes loc)
	 (when (and (null? imports) (>= (config-get args :verbose 0) 2))
	    (newline (current-error-port)))))
   this)

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
   
   (define (export-decl::J2SDeclImport prgm::J2SProgram name::J2SImportName)
      (with-access::J2SImportName name (id alias loc)
	 (with-access::J2SProgram prgm (exports path)
	    (let ((decl (find (lambda (export)
				 (with-access::J2SDecl export ((imp id))
				    (eq? id imp)))
			   exports)))
	       (if decl
		   (with-access::J2SDecl decl (export)
		      (with-access::J2SExport export (index)
			 (instantiate::J2SDeclImport
			    (loc loc)
			    (id alias)
			    (alias id)
			    (binder 'let)
			    (writable #f)
			    (linkindex index)
			    (import this))))
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
   
   (with-access::J2SProgram prgm ((src path) imports decls)
      (with-access::J2SImport this (path loc respath names)
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
	    (set! imports (cons this imports))
	    (unless (member respath stack)
	       (call-with-input-file respath
		  (lambda (in)
		     (let ((margin (string-append
				      (config-get args :verbmargin "")
				      "     ")))
			(when (>= (config-get args :verbose 0) 2)
			   (fprint (current-error-port) "\n" margin
			      path " [" respath "]"))
			(let ((iprgm (j2s-compile in :driver (j2s-export-driver)
					:verbose (config-get args :verbose 0)
					:verbmargin margin)))
			   (with-access::J2SProgram iprgm (exports path)
			      (cond
				 ((list? names)
				  (set! decls
				     (append (map (lambda (n)
						     (export-decl iprgm n))
						names)
					decls)))
				 ((and (pair? names)
				       (memq (car names) '(* default)))
				  (set! decls
				     (cons (export-expr iprgm (cdr names)
					      (car names) loc)
					decls)))
				 (else
				  (raise
				     (instantiate::&io-parse-error
					(proc "import")
					(msg "Illegal import")
					(obj path)
					(fname (cadr loc))
					(location (caddr loc))))))))))))))))

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
(define-walk-method (esexport this::J2SNode prgm::J2SProgram
		       idx::cell defexport::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    esexport ::J2SProgram ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SProgram prgm::J2SProgram
		       idx::cell defexport::cell)

   (define (esexport-default loc)
      (instantiate::J2SDefaultExport
	 (loc loc)
	 (expr (J2SAccess (J2SUnresolvedRef 'module) (J2SString "exports")))))
   
   (with-access::J2SProgram this (nodes decls exports loc)
      (for-each (lambda (o) (esexport o this idx defexport)) nodes)
      (for-each (lambda (o) (esexport o this idx defexport)) decls)
      (unless (cell-ref defexport)
	 ;; force a default export if non specified
	 (set! nodes
	    (append nodes (list (J2SStmtExpr (esexport-default loc)))))))
   this)

;*---------------------------------------------------------------------*/
;*    esexport ::J2SDecl ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SDecl prgm::J2SProgram
		       idx::cell defexport::cell)
   (with-access::J2SDecl this (export)
      (when (isa? export J2SExport)
	 (with-access::J2SExport export (index)
	    (set! index (cell-ref idx))
	    (with-access::J2SProgram prgm (exports)
	       (set! exports (cons this exports)))
	    (cell-set! idx (+fx 1 (cell-ref idx)))))))

;*---------------------------------------------------------------------*/
;*    esexport ::J2SDefaultExport ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SDefaultExport prgm::J2SProgram
		       idx::cell defexport::cell)
   (with-access::J2SDefaultExport this (loc expr)
      (if (cell-ref defexport)
	  (raise
	     (instantiate::&io-parse-error
		(proc "export")
		(msg "Duplicate export")
		(obj #unspecified)
		(fname (cadr loc))
		(location (caddr loc))))
	  (begin
	     (esexport expr prgm idx defexport)
	     (cell-set! defexport this)
	     this))))

;*---------------------------------------------------------------------*/
;*    esexport ::J2SExportVars ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SExportVars prgm::J2SProgram
		       idx::cell defexport::cell)
   ;; J2SExportVars cannot be completed before the symbol resolution.
   ;; For this stage, only the "program" object is stored in the
   ;; export to help the export handling the symbol.scm
   (with-access::J2SExportVars this (program)
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
			 idx)))))))
   
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
