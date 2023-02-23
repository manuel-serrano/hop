;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-nodejs.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 22 13:12:15 2023                          */
;*    Last change :  Thu Feb 23 14:48:04 2023 (serrano)                */
;*    Copyright   :  2023 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Optimizing nodejs builtins                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-nodejs

   (include "ast.sch"
	    "usage.sch"
	    "context.sch")

   (library libuv)
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_classutils
	   __js2scheme_alpha
	   __js2scheme_js
	   __js2scheme_stage
	   __js2scheme_scheme)
   
   (export (stat-call? ::J2SNode)
	   (stat-require? ::J2SNode)
	   (stat-field ::J2SNode)
	   (stat-method ::J2SNode)
	   (j2s-stat-access ::J2SNode ::int mode return ctx)
	   (j2s-stat-call ::J2SNode ::symbol mode return ctx)
	   (hop-call? ::J2SNode)
	   (hop-require? ::J2SNode)
	   (hop-method ::J2SNode)
	   (j2s-hop-call ::J2SNode ::symbol mode return ctx)))
	   
;*---------------------------------------------------------------------*/
;*    is-require? ...                                                  */
;*    -------------------------------------------------------------    */
;*    Is obj the expression "require('XXX')"?                          */
;*---------------------------------------------------------------------*/
(define (is-require? obj::J2SNode module)
   (when (isa? obj J2SRef)
      (with-access::J2SRef obj (decl)
	 (when (and (isa? decl J2SDeclInit) (decl-ronly? decl))
	    (with-access::J2SDeclInit decl (val)
	       (when (isa? val J2SCall)
		  (with-access::J2SCall val (fun args)
		     (when (and (pair? args)
				(null? (cdr args))
				(isa? (car args) J2SString)
				(isa? fun J2SRef))
			(with-access::J2SString (car args) (val)
			   (when (string=? val module)
			      (with-access::J2SRef fun (decl)
				 (when (isa? decl J2SDeclExtern)
				    (with-access::J2SDecl decl (id)
				       (eq? id '%require))))))))))))))

;*---------------------------------------------------------------------*/
;*    stat-prop-table ...                                              */
;*---------------------------------------------------------------------*/
(define stat-prop-table
   (let* ((t (create-hashtable :weak 'open-string))
	  (v (uv-fs-stat-cb-vector-props))
	  (l (vector-length v)))
      (let loop ((i (-fx (vector-length v) 1)))
	 (when (>=fx i 0)
	    (let ((k (vector-ref v i)))
	       (cond
		  ((string=? k "ctime") #unspecified)
		  ((string=? k "atime") #unspecified)
		  ((string=? k "mtime") #unspecified)
		  ((string=? k "birthtime") #unspecified)
		  ((string=? k "ctime-ns") (hashtable-put! t "ctimeMs" i))
		  ((string=? k "mtime-ns") (hashtable-put! t "mtimeMs" i))
		  ((string=? k "atime-ns") (hashtable-put! t "atimeMs" i))
		  ((string=? k "birthtime-ns") (hashtable-put! t "birthtimeMs" i))
		  (else (hashtable-put! t k i)))
	       (loop (-fx i 1)))))
      t))

;*---------------------------------------------------------------------*/
;*    stat-fun ...                                                     */
;*---------------------------------------------------------------------*/
(define (stat-fun fun::J2SNode)
   (cond
      ((isa? fun J2SRef)
       (with-access::J2SRef fun (decl)
	  (when (isa? decl J2SDeclImport)
	     (with-access::J2SDeclImport decl (import)
		(with-access::J2SImport import (path names)
		   (and (string=? path "fs")
			(with-access::J2SImportName (car names) (id)
			   (case id
			      ((lstatSync) 'nodejs-lstat-sync-get)
			      ((statSync) 'nodejs-stat-sync-get)
			      (else #f)))))))))
      ((isa? fun J2SAccess)
       (with-access::J2SAccess fun (obj field)
	  (when (and (stat-require? obj) (isa? field J2SString))
	     (with-access::J2SString field (val)
		(cond
		   ((string=? val "lstatSync") 'nodejs-lstat-sync-get)
		   ((string=? val "statSync") 'nodejs-stat-sync-get)
		   (else #f))))))))

;*---------------------------------------------------------------------*/
;*    stat-call? ...                                                   */
;*---------------------------------------------------------------------*/
(define (stat-call? obj::J2SNode)
   (when (isa? obj J2SCall)
      (with-access::J2SCall obj (fun args)
	 (when (and (pair? args) (null? (cdr args)))
	    (symbol? (stat-fun fun))))))

;*---------------------------------------------------------------------*/
;*    stat-require? ...                                                */
;*    -------------------------------------------------------------    */
;*    Is obj the expression "require('fs')"?                           */
;*---------------------------------------------------------------------*/
(define (stat-require? obj::J2SNode)
   (is-require? obj "fs"))

;*---------------------------------------------------------------------*/
;*    stat-field ...                                                   */
;*---------------------------------------------------------------------*/
(define (stat-field field)
   (when (isa? field J2SString)
      (with-access::J2SString field (val)
	 (hashtable-get stat-prop-table val))))

;*---------------------------------------------------------------------*/
;*    stat-method ...                                                  */
;*---------------------------------------------------------------------*/
(define (stat-method field)
   (when (isa? field J2SString)
      (with-access::J2SString field (val)
	 (cond
	    ((string=? val "isDirectory") 'S_IFDIR)
	    ((string=? val "isFile") 'S_IFREG)
	    ((string=? val "isBlockDevice") 'S_IFBLK)
	    ((string=? val "isCharacterDevice") 'S_IFCHR)
	    ((string=? val "isSymbolicLink") 'S_IFLNK)
	    ((string=? val "isFIFO") 'S_IFIFO)
	    ((string=? val "isSocket") 'S_IFSOCK)))))

;*---------------------------------------------------------------------*/
;*    j2s-stat-access ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-stat-access call index mode return ctx)
   (with-access::J2SCall call (fun args)
      (let ((id (stat-fun fun))
	    (expr (j2s-scheme (car args) mode return ctx)))
	 (if (eq? (j2s-type (car args)) 'string)
	     `(,(symbol-append id '-string) ,expr ,index %this)
	     `(,(symbol-append id '-any) ,expr ,index %this)))))

;*---------------------------------------------------------------------*/
;*    j2s-stat-call ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-stat-call call method mode return ctx)
   (with-access::J2SCall call (fun args)
      (let ((id (stat-fun fun))
	    (expr (j2s-scheme (car args) mode return ctx)))
	 (if (eq? (j2s-type (car args)) 'string)
	     `(,(symbol-append id '-is-mode-string) ,expr ,method %this)
	     `(,(symbol-append id '-is-mode-string) ,expr ,method %this)))))

;*---------------------------------------------------------------------*/
;*    hop-fun ...                                                      */
;*---------------------------------------------------------------------*/
(define (hop-fun fun::J2SNode)
   (cond
      ((isa? fun J2SRef)
       (with-access::J2SRef fun (decl)
	  (when (isa? decl J2SDeclImport)
	     (with-access::J2SDeclImport decl (import)
		(with-access::J2SImport import (path names)
		   (and (string=? path "fs")
			(with-access::J2SImportName (car names) (id)
			   (case id
			      ((log) 'tprint)
			      (else #f)))))))))
      ((isa? fun J2SAccess)
       (with-access::J2SAccess fun (obj field)
	  (when (and (hop-require? obj) (isa? field J2SString))
	     (with-access::J2SString field (val)
		(cond
		   ((string=? val "log") 'tprint)
		   (else #f))))))))

;*---------------------------------------------------------------------*/
;*    hop-call? ...                                                    */
;*---------------------------------------------------------------------*/
(define (hop-call? obj::J2SNode)
   (when (isa? obj J2SCall)
      (with-access::J2SCall obj (fun args)
	 (when (and (pair? args) (null? (cdr args)))
	    (symbol? (hop-fun fun))))))

;*---------------------------------------------------------------------*/
;*    hop-require? ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop-require? obj::J2SNode)
   (is-require? obj "hop"))

;*---------------------------------------------------------------------*/
;*    hop-method ...                                                   */
;*---------------------------------------------------------------------*/
(define (hop-method field)
   (when (isa? field J2SString)
      (with-access::J2SString field (val)
	 (cond
	    ((string=? val "log") 'log)))))

;*---------------------------------------------------------------------*/
;*    j2s-hop-call ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-hop-call call method mode return ctx)
   (with-access::J2SCall call (fun args)
      (let ((id (hop-fun fun)))
	 (case method
	    ((log)
	     `(tprint ,@(map (lambda (a)
				(j2s-scheme a mode return ctx))
			   args)))
	    (else
	     (error "j2s-hop-call" "wrong call" method))))))




