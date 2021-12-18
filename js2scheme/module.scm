;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/module.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 15 15:16:16 2018                          */
;*    Last change :  Fri Dec 17 17:44:49 2021 (serrano)                */
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
      (esimport this this (config-get args :module-stack '()) #unspecified args)
      (esexport this this args))
   this)

;*---------------------------------------------------------------------*/
;*    esimport ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (esimport this::J2SNode prgm::J2SProgram stack import args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    esimport ::J2SProgram ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (esimport this::J2SProgram prgm::J2SProgram stack import args)
   (with-access::J2SProgram this (imports decls path)
      (if (member path stack)
	  (with-access::J2SImport import (loc)
	     (raise
		(instantiate::&io-parse-error
		   (proc "import")
		   (msg "Illegal cyclic import")
		   (obj path)
		   (fname (cadr loc))
		   (location (caddr loc)))))
	  (begin
	     ;; collect all the imported modules (following re-exports)
	     (set! imports (collect-imports* this prgm stack import args))
	     ;; declare all the imported variables
	     (set! decls (append (collect-decls* this prgm args) decls))))))

;*---------------------------------------------------------------------*/
;*    collect-imports* ...                                             */
;*    -------------------------------------------------------------    */
;*    Collect recursively all imported modules. Update J2SImport       */
;*    object so that RESPATH is the absolute imported module           */
;*    and IPRGM the J2SProgram object associated with the imported     */
;*    module.                                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-imports* this::J2SNode prgm stack import args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-imports* ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-imports* this::J2SProgram prgm stack import args)
   (with-access::J2SProgram this (imports decls nodes path)
      (unless (pair? imports)
	 (let ((nstack (cons path stack)))
	    (set! imports
	       (delete-duplicates
		  (append-map (lambda (n)
				 (collect-imports* n this nstack import args))
		     (append decls nodes))
		  equal-respath?))
	    (for-each (lambda (i n)
			 (with-access::J2SImportPath i (index)
			    (set! index n)))
	       imports (iota (length imports)))))
      imports))

;*---------------------------------------------------------------------*/
;*    collect-imports* ::J2SImport ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-imports* this::J2SImport prgm stack import args)
   
   (define (import-resolve-path! this::J2SImport prgm args)
      (with-access::J2SImport this (respath path lang loc)
	 (or respath
	     (with-access::J2SProgram prgm ((src path))
		(set! respath (resolve-path! path src loc args))
		(unless lang (set! lang (resolve-lang respath)))
		respath))))
   
   (let ((respath (import-resolve-path! this prgm args)))
      (cons respath
	 (with-access::J2SImport this (iprgm lang loc)
	    (set! iprgm (import-module respath lang loc stack args))
	    (collect-imports* iprgm iprgm stack this args)))))

;*---------------------------------------------------------------------*/
;*    collect-decls* ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-decls* this::J2SNode prgm args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-decls* ::J2SImport ...                                   */
;*---------------------------------------------------------------------*/
(define-method (collect-decls* this::J2SImport prgm args)
   
   (define (prgm-respath prgm path loc)
      (with-access::J2SProgram prgm (imports)
	 (or (find (lambda (i) (equal-respath? i path)) imports)
	     (raise
		(instantiate::&io-parse-error
		   (proc "import")
		   (msg "internal error, cannot find module")
		   (obj path)
		   (fname (cadr loc))
		   (location (caddr loc)))))))
   
   (define (find-export name::symbol import::J2SImport iprgm::J2SProgram loc)
      (with-access::J2SProgram iprgm (exports path)
	 (let loop ((exports exports))
	    (if (null? exports)
		(raise
		   (instantiate::&io-parse-error
		      (proc "import")
		      (msg (format "imported binding \"~a\" not exported by module ~s"
			      name path))
		      (obj name)
		      (fname (cadr loc))
		      (location (caddr loc))))
		(with-access::J2SExport (car exports) (alias id from loc)
		   (cond
		      ((not (eq? name alias))
		       (loop (cdr exports)))
		      ((not from)
		       (with-access::J2SImport import (respath)
			  (set! respath (prgm-respath prgm respath loc))
			  (values (car exports) import)))
		      (else
		       (let* ((respath (resolve-path! from path loc args))
			      (iprgm (import-module respath #f loc '() args))
			      (iname (instantiate::J2SImportName
					(loc loc)
					(id id)
					(alias alias)))
			      (rpath (prgm-respath prgm respath loc))
			      (import (duplicate::J2SImport import
					 (path path)
					 (respath rpath)
					 (names (list iname)))))
			  (find-export id import iprgm loc)))))))))
   
   (define (import-binding name import::J2SImport)
      (with-access::J2SImport import (iprgm loc)
	 (with-access::J2SImportName name (id alias)
	    (multiple-value-bind (x i)
	       (find-export id this iprgm loc)
	       (instantiate::J2SDeclImport
		  (loc loc)
		  (id alias)
		  (alias id)
		  (binder 'let)
		  (writable #f)
		  (vtype 'any)
		  (scope 'local)
		  (export x)
		  (import i))))))

   (define (import-namespace name import::J2SImport)
      (with-access::J2SImport import (iprgm loc)
	 (with-access::J2SImportName name (alias)
	    (instantiate::J2SDeclInit
	       (loc loc)
	       (id alias)
	       (binder 'let-opt)
	       (scope 'global)
	       (writable #f)
	       (val (instantiate::J2SImportExports
		       (loc loc)
		       (type 'object)
		       (import this)))))))
   
   (with-access::J2SImport this (iprgm names loc)
      (map (lambda (name)
	      (with-access::J2SImportName name (id)
		 (if (eq? id '*)
		     (import-namespace name this)
		     (import-binding name this))))
	 names)))

;* {*---------------------------------------------------------------------*} */
;* {*    esimport ::J2SImport ...                                         *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (esimport this::J2SImport prgm::J2SProgram stack args) */
;*                                                                     */
;*    (define (import-decl::pair-nil iprgm::J2SProgram name::J2SImportName idx) */
;*       ;; IDX is the position in the import-list of name. It is only used */
;*       ;; when importing from a core module that lacks a true export stmt */
;*       (with-access::J2SImportName name (id alias loc)               */
;* 	 (with-access::J2SProgram iprgm (exports path mode)            */
;* 	    (let ((expo (find (lambda (export)                         */
;* 				 (with-access::J2SExport export ((imp alias)) */
;* 				    (eq? id imp)))                     */
;* 			   exports)))                                  */
;* 	       (cond                                                   */
;* 		  (expo                                                */
;* 		   (with-access::J2SExport expo (decl from id alias)   */
;* 		      (if (isa? from J2SProgram)                       */
;* 			  ;; a redirection                             */
;* 			  (with-access::J2SProgram from (path)         */
;* 			     (tprint "redirect id=" id " alias=" alias) */
;* 			     (esimport                                 */
;* 				(instantiate::J2SImport                */
;* 				   (loc loc)                           */
;* 				   (names (list (instantiate::J2SImportName */
;* 						   (loc loc)           */
;* 						   (id id)             */
;* 						   (alias alias))))    */
;* 				   (path path)                         */
;* 				   (dollarpath (instantiate::J2SUndefined (loc loc)))) */
;* 				prgm stack args))                      */
;* 			  ;; a direct import                           */
;* 			  (with-access::J2SDecl decl (exports)         */
;* 			     (map (lambda (export)                     */
;* 				     (with-access::J2SExport export (decl) */
;* 					(with-access::J2SDecl decl (vtype) */
;* 					   (instantiate::J2SDeclImport */
;* 					      (loc loc)                */
;* 					      (id alias)               */
;* 					      (alias id)               */
;* 					      (binder 'let)            */
;* 					      (writable #f)            */
;* 					      (vtype vtype)            */
;* 					      (scope (if (eq? from 'hop) '%hop 'local)) */
;* 					      (export export)          */
;* 					      (import this)))))        */
;* 				exports)))))                           */
;* 		  ((eq? mode 'core)                                    */
;* 		   (co-instantiate ((decl (instantiate::J2SDecl        */
;* 					     (id id)                   */
;* 					     (loc loc)                 */
;* 					     (vtype 'any)              */
;* 					     (exports (list expo))))   */
;* 				    (expo (instantiate::J2SExport      */
;* 					     (id id)                   */
;* 					     (alias id)                */
;* 					     (index idx)               */
;* 					     (decl decl))))            */
;* 		      (list (instantiate::J2SDeclImport                */
;* 			       (loc loc)                               */
;* 			       (id alias)                              */
;* 			       (alias id)                              */
;* 			       (binder 'let)                           */
;* 			       (writable #f)                           */
;* 			       (vtype 'any)                            */
;* 			       (scope 'core)                           */
;* 			       (export expo)                           */
;* 			       (import this)))))                       */
;* 		  (else                                                */
;* 		   (raise                                              */
;* 		      (instantiate::&io-parse-error                    */
;* 			 (proc "import")                               */
;* 			 (msg (format "imported binding \"~a\" not exported by module ~s" */
;* 				 id path))                             */
;* 			 (obj id)                                      */
;* 			 (fname (cadr loc))                            */
;* 			 (location (caddr loc))))))))))                */
;*                                                                     */
;*    (define (export-exports::J2SDecl prgm::J2SProgram id loc)        */
;*       (instantiate::J2SDeclInit                                     */
;* 	 (loc loc)                                                     */
;* 	 (id id)                                                       */
;* 	 (binder 'let-opt)                                             */
;* 	 (scope 'global)                                               */
;* 	 (writable #f)                                                 */
;* 	 (val (instantiate::J2SImportExports                           */
;* 		 (loc loc)                                             */
;* 		 (type 'object)                                        */
;* 		 (import this)))))                                     */
;*                                                                     */
;*    (define (redirect this::J2SImport prgm::J2SProgram iprgm::J2SProgram names) */
;*       (with-access::J2SProgram prgm (exports imports path)          */
;* 	 (with-access::J2SProgram iprgm ((iexports exports))           */
;* 	    (set! exports                                              */
;* 	       (append exports                                         */
;* 		  (append-map (lambda (export)                         */
;* 				 (with-access::J2SExport export (id alias) */
;* 				    (filter-map (lambda (n)            */
;* 						   (with-access::J2SImportRedirect n ((rid id) (ralias alias)) */
;* 						      (when (eq? rid alias) */
;* 							 (duplicate::J2SExport export */
;* 							    (id id)    */
;* 							    (alias ralias) */
;* 							    (from iprgm))))) */
;* 				       names)))                        */
;* 		     iexports)))))                                     */
;*       '())                                                          */
;*                                                                     */
;*    (define (reexport this::J2SImport prgm::J2SProgram iprgm::J2SProgram) */
;*       (with-access::J2SProgram prgm (exports imports path)          */
;* 	 (with-access::J2SProgram iprgm ((iexports exports))           */
;* 	    (set! exports                                              */
;* 	       (append exports                                         */
;* 		  (filter-map (lambda (export)                         */
;* 				 (with-access::J2SExport export (id alias) */
;* 				    (unless (eq? id 'default)          */
;* 				       (duplicate::J2SExport export    */
;* 					  (from iprgm)))))             */
;* 		     iexports)))))                                     */
;*       '())                                                          */
;*                                                                     */
;*                                                                     */
;*                                                                     */
;*    (define (import-module-decls this iprgm)                         */
;*       (with-access::J2SImport this (names loc path)                 */
;* 	 (cond                                                         */
;* 	    ((null? names)                                             */
;* 	     '())                                                      */
;* 	    ((isa? (car names) J2SImportNamespace)                     */
;* 	     (with-access::J2SImportNamespace (car names) (id)         */
;* 		(list (export-exports iprgm id loc))))                 */
;* 	    ((isa? (car names) J2SImportRedirect)                      */
;* 	     (redirect this prgm iprgm names))                         */
;* 	    ((isa? (car names) J2SImportExport)                        */
;* 	     (reexport this prgm iprgm))                               */
;* 	    ((list? names)                                             */
;* 	     (append-map (lambda (n idx) (import-decl iprgm n idx))    */
;* 		names (iota (length names))))                          */
;* 	    (else                                                      */
;* 	     (raise                                                    */
;* 		(instantiate::&io-parse-error                          */
;* 		   (proc "import")                                     */
;* 		   (msg "Illegal import")                              */
;* 		   (obj path)                                          */
;* 		   (fname (cadr loc))                                  */
;* 		   (location (caddr loc))))))))                        */
;*                                                                     */
;*    (esimport-init-core-modules!)                                    */
;*                                                                     */
;*    (with-access::J2SProgram prgm ((src path) imports decls)         */
;*       (with-access::J2SImport this (path loc respath iprgm)         */
;* 	 (let ((base (cond                                             */
;* 			((string=? src "")                             */
;* 			 (pwd))                                        */
;* 			((char=? (string-ref src 0) #\/)               */
;* 			 (dirname src))                                */
;* 			(else                                          */
;* 			 (dirname                                      */
;* 			    (file-name-canonicalize                    */
;* 			       (make-file-name (pwd) src)))))))        */
;* 	    (set! respath (resolve-module-file path base args loc))    */
;* 	    (when (string=? respath src)                               */
;* 		(raise                                                 */
;* 		   (instantiate::&io-parse-error                       */
;* 		      (proc "import")                                  */
;* 		      (msg "Illegal self-import")                      */
;* 		      (obj path)                                       */
;* 		      (fname respath)                                  */
;* 		      (location (caddr loc)))))                        */
;* 	    (unless (member respath stack)                             */
;* 	       (set! iprgm (import-module respath path loc))           */
;* 	       (set! decls (append (import-module-decls this iprgm) decls))) */
;* 	    (set! imports (cons this imports))))))                     */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    esimport ::J2SImportDynamic ...                                  *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (esimport this::J2SImportDynamic prgm::J2SProgram stack args) */
;*    (with-access::J2SProgram prgm (path)                             */
;*       (with-access::J2SImportDynamic this (base)                    */
;* 	 (set! base path)                                              */
;* 	 (call-default-walker))))                                      */

;*---------------------------------------------------------------------*/
;*    esexport ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SNode prgm::J2SProgram args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    esexport ::J2SProgram ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SProgram prgm::J2SProgram args)
   (with-access::J2SProgram this (nodes decls exports loc path exports)
      (for-each (lambda (o) (esexport o this args)) nodes)
      (for-each (lambda (o) (esexport o this args)) decls)
      (for-each (lambda (o) (esexport o this args)) exports))
   this)

;*---------------------------------------------------------------------*/
;*    esexport ::J2SRedirect ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (esexport this::J2SRedirect prgm::J2SProgram args)
   
   (define (find-import ip::J2SImportPath prgm::J2SProgram loc)
      (with-access::J2SProgram prgm (imports)
	 (let ((im (find (lambda (i) (equal-respath? ip i)) imports)))
	    (if im
		(with-access::J2SImportPath im (index)
		   index)
		(with-access::J2SImportPath ip (path)
		   (raise
		      (instantiate::&io-parse-error
			 (proc "export")
			 (msg "Cannot find module")
			 (obj path)
			 (fname (cadr loc))
			 (location (caddr loc)))))))))
   
   (define (find-export name::symbol iprgm::J2SProgram loc)
      (with-access::J2SProgram iprgm (exports path)
	 (let loop ((exports exports))
	    (if (null? exports)
		(raise
		   (instantiate::&io-parse-error
		      (proc "export")
		      (msg (format "imported binding \"~a\" not exported by module ~s"
			      name path))
		      (obj name)
		      (fname (cadr loc))
		      (location (caddr loc))))
		(with-access::J2SExport (car exports) (alias id from loc)
		   (cond
		      ((not (eq? name alias))
		       (loop (cdr exports)))
		      ((not from)
		       (car exports))
		      (else
		       (let* ((respath (resolve-path! from path loc args))
			      (iprgm (import-module respath #f loc '() args)))
			  (find-export id iprgm loc)))))))))
   
   (with-access::J2SRedirect this (rindex from loc id index)
      (with-access::J2SProgram prgm ((src path) imports)
	 (let* ((respath (resolve-path! from src loc args))
		(p (import-module respath (path-lang from) loc '() args))
		(x (find-export id p loc)))
	    (with-access::J2SExport x ((i index))
	       (set! index i)
	       (set! rindex (find-import respath prgm loc)))
	    this))))
   
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
	 (else 'js)))

   (let ((suf (suffix path)))
	 (if (string-null? suf)
	     'js
	     (string->lang suf))))

;*---------------------------------------------------------------------*/
;*    resolve-lang ...                                                 */
;*---------------------------------------------------------------------*/
(define (resolve-lang respath::J2SImportPath)
   (with-access::J2SImportPath respath (path)
      (path-lang path)))

;*---------------------------------------------------------------------*/
;*    resolve-module-file ...                                          */
;*    -------------------------------------------------------------    */
;*    Almost similar to nodejs's resolve method (see                   */
;*    nodejs/require.scm).                                             */
;*---------------------------------------------------------------------*/
(define (resolve-module-file::J2SImportPath name dir::bstring args loc)
   
   (define (resolve-file x)
      (cond
	 ((and (file-exists? x) (not (directory? x)))
	  (instantiate::J2SImportPath
	     (loc loc)
	     (name x)
	     (path (file-name-canonicalize x))
	     (protocol 'file)))
	 (else
	  (let loop ((sufs '(".js" ".mjs" ".hop" ".so" ".json" ".hss" ".css")))
	     (when (pair? sufs)
		(let* ((suffix (car sufs))
		       (src (string-append x suffix)))
		   (if (and (file-exists? src) (not (directory? src)))
		       (instantiate::J2SImportPath
			  (loc loc)
			  (name x)
			  (path (file-name-canonicalize src))
			  (protocol 'file))
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
      (open-string-hashtable-get (get-core-modules) name))

   (define hop-modules-path
      (cons (config-get args :hop-library-path ".")
	 (list (config-get args :node-modules-directory "."))))
   
   (define (resolve-modules name)
      (any (lambda (dir)
	      (resolve-file-or-directory name dir))
	 (filter string? hop-modules-path)))
   
   (cond
      ((core-module? name)
       (instantiate::J2SImportPath
	  (loc loc)
	  (name name)
	  (path name)
	  (protocol 'core)))
      ((or (string-prefix? "http://" name)
	   (string-prefix? "https://" name))
       (instantiate::J2SImportPath
	  (loc loc)
	  (name name)
	  (path name)
	  (protocol 'http)))
      ((or (string-prefix? "./" name) (string-prefix? "../" name))
       (or (resolve-file-or-directory name dir)
	   (resolve-modules name)
	   (resolve-error name)))
      ((string-prefix? "/" name)
       (or (resolve-file-or-directory name "/")
	   (resolve-modules name)
	   (resolve-error name)))
      (else
       (or (resolve-modules name)
	   (resolve-error name)))))

;*---------------------------------------------------------------------*/
;*    resolve-path! ...                                                */
;*---------------------------------------------------------------------*/
(define (resolve-path!::J2SImportPath path src loc args)
   (let* ((base (cond
		   ((string=? src "")
		    (pwd))
		   ((char=? (string-ref src 0) #\/)
		    (dirname src))
		   (else
		    (dirname
		       (file-name-canonicalize
			  (make-file-name (pwd) src))))))
	  (respath (resolve-module-file path base args loc)))
      (with-access::J2SImportPath respath (path)
	 (when (string=? path src)
	    (raise
	       (instantiate::&io-parse-error
		  (proc "import")
		  (msg "Illegal self-import")
		  (obj src)
		  (fname src)
		  (location (caddr loc))))))
      respath))

;*---------------------------------------------------------------------*/
;*    import-module ...                                                */
;*---------------------------------------------------------------------*/
(define (import-module impath::J2SImportPath lang loc stack args)
   (with-access::J2SImportPath impath (path)
      (or (open-string-hashtable-get core-modules path)
	  (module-cache-get path)
	  (with-handler
	     (lambda (e)
		(with-access::&exception e (fname location)
		   (unless (and fname location)
		      (set! fname (cadr loc))
		      (set! location (caddr loc))))
		(raise e))
	     (call-with-input-file path
		(lambda (in)
		   (let ((margin (string-append
				    (config-get args :verbmargin "")
				    "     ")))
		      (when (>= (config-get args :verbose 0) 2)
			 (fprint (current-error-port) "\n" margin
			    path
			    (if (>= (config-get args :verbose 0) 3)
				(string-append " [" path "]")
				"")))
		      (let* ((verb (config-get args :verbose 0))
			     (iprgm (case lang
				       ((hop)
					(hop-compile in
					   :verbose verb
					   :verbmargin margin
					   :module-import #t
					   :module-stack stack))
				       (else
					(j2s-compile in
					   :driver (j2s-export-driver)
					   :warning 0
					   :module-import #t
					   :verbose verb
					   :verbmargin margin
					   :module-stack stack)))))
			 (module-cache-put! path iprgm)))))))))

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
   (unless module-cache (set! module-cache (instantiate::cache-memory)))
   (cache-put! module-cache path iprgm)
   iprgm)

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
					     (export expo)
					     ))
				    (expo (instantiate::J2SExport
					     (loc loc)
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

;*---------------------------------------------------------------------*/
;*    equal-respath? ...                                               */
;*---------------------------------------------------------------------*/
(define (equal-respath? x y)
   (with-access::J2SImportPath x ((xpath path))
      (with-access::J2SImportPath y ((ypath path))
	 (string=? xpath ypath))))
