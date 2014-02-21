;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/nodejs/require.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 16 15:47:40 2013                          */
;*    Last change :  Thu Feb 13 09:17:27 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo Nodejs module implementation                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_require

   (library hopscript js2scheme)

   (export (%nodejs-module::JsObject ::bstring ::bstring)
	   (%nodejs-module-get ::bstring)
	   (%nodejs-require ::bstring ::JsObject)
	   (nodejs-require ::bstring)
	   (nodejs-load ::bstring)))

;*---------------------------------------------------------------------*/
;*    %nodejs-module ...                                               */
;*---------------------------------------------------------------------*/
(define (%nodejs-module::JsObject id filename)
   (nodejs-init!)
   (let ((m (js-new js-object)))
      ;; id field
      (js-put! m 'id id #f)
      ;; exports
      (js-put! m 'exports (alist->jsobject `((prototype . ,(js-new js-object)))) #f)
      ;; filename
      (js-put! m 'filename filename #f)
      ;; loaded
      (js-put! m 'loaded #t #f)
      ;; children
      (js-put! m 'children '#() #f)
      ;; paths
      (js-put! m 'paths (nodejs-filename->paths filename) #f)
      (register-module! filename m)
      m))

;*---------------------------------------------------------------------*/
;*    %nodejs-module-get ...                                           */
;*---------------------------------------------------------------------*/
(define (%nodejs-module-get id)
   (synchronize module-mutex
      (hashtable-get jsnode-modules id)))

;*---------------------------------------------------------------------*/
;*    nodejs-filename->paths ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-filename->paths::vector file::bstring)
   (if (char=? (string-ref file 0) #\/)
       (let loop ((dir (dirname file))
		  (acc '()))
	  (if (string=? dir "/")
	      (list->vector
		 (reverse! (cons "/node_modules" acc)))
	      (loop (dirname dir)
		 (cons (make-file-name dir "node_modules") acc))))
       '#()))
   
;*---------------------------------------------------------------------*/
;*    %nodejs-require ...                                              */
;*---------------------------------------------------------------------*/
(define (%nodejs-require name module)
   (if (or (string-prefix? "./" name)
	   (string-prefix? "../" name)
	   (string-prefix? "/" name))
       (nodejs-load name)
       (let ((file (find (lambda (dir)
			    (let ((path (make-file-name dir name)))
			       (when (file-exists? path) path)))
		      (vector->list (js-get module 'paths)))))
	  (if (string? file)
	      (js-get (nodejs-load file) 'exports)
	      (let ((lib (nodejs-load name)))
		 (or lib (error "require" "cannot find module" name)))))))

;*---------------------------------------------------------------------*/
;*    module-mutex ...                                                 */
;*---------------------------------------------------------------------*/
(define module-mutex
   (make-mutex "jsnode-module"))

;*---------------------------------------------------------------------*/
;*    jsnode-modules ...                                               */
;*---------------------------------------------------------------------*/
(define jsnode-modules
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    register-module! ...                                             */
;*---------------------------------------------------------------------*/
(define (register-module! filename m)
   (synchronize module-mutex
      (hashtable-put! jsnode-modules filename m)))

;*---------------------------------------------------------------------*/
;*    nodejs-load ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-load filename)
   
   (define (load-module)
      (let ((exprs (call-with-input-file filename j2s-compile)))
	 (let ((m (eval-module))
	       (jsmodule #f))
	    (unwind-protect
	       (begin
		  ;; eval the compile module in the current environment
		  (for-each eval! exprs)
		  ;; grab the module value
		  (set! jsmodule (eval! 'module)))
	       ;; restore the previous module
	       (eval-module-set! m))
	    (register-module! filename jsmodule)
	    ;; the module
	    (js-get jsmodule 'exports))))

   (synchronize module-mutex
      (let ((old (hashtable-get jsnode-modules filename)))
	 (if old
	     ;; module already loaded
	     (js-get old 'exports)
	     ;; the module have to be loaded, compile it first
	     (load-module)))))

;*---------------------------------------------------------------------*/
;*    nodejs-require ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-require file)
   (%nodejs-require file (nodejs-hop-module)))

;*---------------------------------------------------------------------*/
;*    nodejs-hop-module ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-hop-module)
   ;; create a module for a fake file name that cannot exists
   (%nodejs-module (symbol->string (gensym 'hop)) "//hop.js"))

;*---------------------------------------------------------------------*/
;*    nodejs-initialized ...                                           */
;*---------------------------------------------------------------------*/
(define nodejs-initialized #f)

;*---------------------------------------------------------------------*/
;*    nodejs-init! ...                                                 */
;*    -------------------------------------------------------------    */
;*    bootstrap nodejs modules                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-init!)
   (unless nodejs-initialized
      (set! nodejs-initialized #t)
      (%nodejs-module "stream.js" "stream")))
