;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/process.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 15:02:45 2013                          */
;*    Last change :  Thu Jun  5 18:19:53 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    NodeJS process object                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_process
   
   (option (set! *warning-overriden-variables* #f))
   
   (library hopscript hop)

   (include "nodejs.sch")

   (import __nodejs__hop
	   __nodejs__timer
	   __nodejs__fs)
   
   (export (%nodejs-process %this::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    %nodejs-process ...                                              */
;*---------------------------------------------------------------------*/
(define (%nodejs-process %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-object)
      (let ((proc (js-new %this js-object)))
	 (js-put! proc 'title (hop-name) #f %this)
	 (js-put! proc 'version (hop-version) #f %this)
	 (js-put! proc 'exit
	    (js-make-function %this
	       (lambda (this status)
		  (exit status))
	       2 "exit")
	    #f %this)
	 (js-put! proc 'platform (os-name) #f %this)
	 (js-put! proc 'binding
	    (js-make-function %this
	       (lambda (this module)
		  (cond
		     ((string=? module "constants")
		      (process-constants %this))
		     ((string=? module "fs")
		      (process-fs %this))
		     ((string=? module "buffer")
		      (process-buffer %this))
		     ((string=? module "udp_wrap")
		      (process-udp-wrap %this))
		     ((string=? module "evals")
		      (process-evals %this))
		     ((string=? module "cares_wrap")
		      (process-cares-wrap %this))
		     ((string=? module "timer_wrap")
		      (hopjs-process-timer %this))
		     ((string=? module "process_wrap")
		      (process-process-wrap %this))
		     ((string=? module "crypto")
		      (process-crypto %this))
		     ((string=? module "http_parser")
		      (process-http-parser %this))
		     ((string=? module "zlib")
		      (process-zlib %this))
		     ((string=? module "hop")
		      (hopjs-process-hop %this))
		     (else
		      (warning "%nodejs-process"
			 "binding not implemented: " module)
		      (js-new %this js-object))))
	       2 "binding")
	    #f %this)
	 (js-put! proc 'stdout
	    (js-alist->jsobject
	       `((write . ,(js-make-function %this
			      (lambda (this o)
				 (display o))
			      1 "write")))
	       %this)
	    #f %this)
	 (js-put! proc 'stderr
	    (js-alist->jsobject
	       `((write . ,(js-make-function %this
			      (lambda (this o) (display o (current-error-port)))
			      1 "write")))
	       %this)
	    #f %this)
	 (js-put! proc 'env
	    (js-alist->jsobject
	       `((NODE_DEBUG . ,(getenv "NODE_DEBUG")))
	       %this)
	    #f %this)
	 (js-put! proc '_usingDomains
	    (js-make-function %this
	       (lambda (this) (js-undefined)) 0 "_usingDomains")
	    #f %this)
	 proc)))

;*---------------------------------------------------------------------*/
;*    process-constants ...                                            */
;*---------------------------------------------------------------------*/
(define (process-constants %this)
   (js-alist->jsobject
      `((O_RDONLY . ,O_RDONLY)
	(O_WRONLY . ,O_WRONLY)
	(O_RDWR . ,O_RDWR)
	(O_CREAT . ,O_CREAT)
	(O_EXCL . ,O_EXCL)
	(O_TRUNC . ,O_TRUNC)
	(O_NOCTTY . ,O_NOCTTY)
	(O_APPEND . ,O_APPEND)
	(O_DIRECTORY . ,O_DIRECTORY)
	(O_SYNC . ,O_SYNC)
	(O_NOFOLLOW . ,O_NOFOLLOW)
	
	(S_IFMT . ,S_IFMT)
	(S_IFDIR . ,S_IFDIR)
	(S_IFREG . ,S_IFREG))
      %this))

;*---------------------------------------------------------------------*/
;*    slowbuffer ...                                                   */
;*---------------------------------------------------------------------*/
(define (slowbuffer this len)
   (make-vector len))

;*---------------------------------------------------------------------*/
;*    process-buffer ...                                               */
;*---------------------------------------------------------------------*/
(define (process-buffer %this)
   (with-access::JsGlobalObject %this (js-object)
      (let ((SlowBuffer (js-make-function %this slowbuffer 1 "SlowBuffer"
			   :alloc (lambda (o) (js-object-alloc o %this))
			   :construct slowbuffer
			   :prototype (js-new %this js-object))))
	 (js-put! SlowBuffer 'byteLength
	    (js-make-function %this
	       (lambda (this) 0) 0 "ByteLength")
	    #t %this)
	 (js-put! SlowBuffer 'makeFastBuffer
	    (js-make-function %this
	       (lambda (this sbuf buf c d)
		  (js-put! sbuf 'asciiSlice
		     (js-make-function %this
			(lambda (this start end)
			   (js-get buf '%fast-buffer %this))
			3 "asciiSlice")
		     #f %this)
		  (js-put! buf '%fast-buffer (make-string d) #f %this))
	       4 "makeFastBuffer")
	    #t %this)
	 (js-alist->jsobject
	    `((SlowBuffer . ,SlowBuffer))
	    %this))))

;*---------------------------------------------------------------------*/
;*    process-udp-wrap ...                                             */
;*---------------------------------------------------------------------*/
(define (process-udp-wrap %this)
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 `((UDP . ,(js-new %this js-object)))
	 %this)))

;*---------------------------------------------------------------------*/
;*    process-evals ...                                                */
;*---------------------------------------------------------------------*/
(define (process-evals %this)
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 `((NodeScript . ,(js-new %this js-object)))
	 %this)))

;*---------------------------------------------------------------------*/
;*    process-cares-wrap ...                                           */
;*---------------------------------------------------------------------*/
(define (process-cares-wrap %this)
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 `((isIP . ,(js-new %this js-object)))
	 %this)))

;*---------------------------------------------------------------------*/
;*    process-process-wrap ...                                         */
;*---------------------------------------------------------------------*/
(define (process-process-wrap %this)
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 `((Process . ,(js-new %this js-object)))
	 %this)))

;*---------------------------------------------------------------------*/
;*    process-crypto ...                                               */
;*---------------------------------------------------------------------*/
(define (process-crypto %this)
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 `((SecureContext . ,(js-new %this js-object))
	   (randomBytes . ,(js-new %this js-object))
	   (pseudoRandomBytes . ,(js-new %this js-object))
	   (getCiphers . ,(js-new %this js-object))
	   (getHashes . ,(js-new %this js-object)))
	 %this)))

;*---------------------------------------------------------------------*/
;*    process-http-parser ...                                          */
;*---------------------------------------------------------------------*/
(define (process-http-parser %this)
   (js-alist->jsobject
      `((HTTPParser . ,(js-make-function %this
			  (lambda (this) (js-undefined)) 0
			  "HTTPParser")))
      %this))

;*---------------------------------------------------------------------*/
;*    process-zlib ...                                                 */
;*---------------------------------------------------------------------*/
(define (process-zlib %this)
   (js-alist->jsobject `() %this))

	   	   



   
