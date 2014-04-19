;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/process.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 15:02:45 2013                          */
;*    Last change :  Sat Apr 19 08:14:59 2014 (serrano)                */
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

   (import __nodejs__hop)
   
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
		      (process-timer-wrap %this))
		     ((string=? module "process_wrap")
		      (process-process-wrap %this))
		     ((string=? module "crypto")
		      (process-crypto %this))
		     ((string=? module "http_parser")
		      (process-http-parser %this))
		     ((string=? module "zlib")
		      (process-zlib %this))
		     ((string=? module "hop")
		      (process-hop %this))
		     (else
		      (warning "%nodejs-process"
			 "binding not implemented: " module)
		      (js-new %this js-object))))
	       2 "binding")
	    #f %this)
	 (js-put! proc 'stdout
	    (alist->jsobject
	       `((write . ,(lambda (this o) (display o)))))
	    #f %this)
	 (js-put! proc 'stderr
	    (alist->jsobject
	       `((write . ,(lambda (this o) (display o)))))
	    #f %this)
	 (js-put! proc 'env
	    (alist->jsobject
	       `((NODE_DEBUG . ,(getenv "NODE_DEBUG"))))
	    #f %this)
	 (js-put! proc '_usingDomains
	    (js-make-function %this
	       (lambda (this) (js-undefined)) 0 "_usingDomains")
	    #f %this)
	 proc)))

;*---------------------------------------------------------------------*/
;*    constants ...                                                    */
;*---------------------------------------------------------------------*/
(define (S_IFDIR) 4)
(define (S_IFREG) 8)

;*---------------------------------------------------------------------*/
;*    process-constants ...                                            */
;*---------------------------------------------------------------------*/
(define (process-constants %this)
   (alist->jsobject
      `((O_RDONLY . 0)
	(O_WRONLY . 1)
	(O_RDWR . 2)
	(S_IFMT . ,(bit-or (S_IFDIR) (S_IFREG)))
	(S_IFDIR . ,(S_IFDIR))
	(S_IFREG . ,(S_IFREG)))))

;*---------------------------------------------------------------------*/
;*    process-fs-stats ...                                             */
;*---------------------------------------------------------------------*/
(define process-fs-stats #f)

;*---------------------------------------------------------------------*/
;*    get-process-fs-stats ...                                         */
;*---------------------------------------------------------------------*/
(define (get-process-fs-stats %this)
   (with-access::JsGlobalObject %this (js-object)
      (unless process-fs-stats (set! process-fs-stats (js-new %this js-object)))
      process-fs-stats))

;*---------------------------------------------------------------------*/
;*    process-fs ...                                                   */
;*---------------------------------------------------------------------*/
(define (process-fs %this)
   
   (define (readdir this path)
      (js-vector->jsarray (list->vector (directory->path-list path))  %this))

   (define (lstat this path)
      (let ((obj (alist->jsobject
		    `((mode . ,(if (directory? path) (S_IFDIR) (S_IFREG)))))))
	 (with-access::JsObject obj (__proto__)
	    (set! __proto__ (get-process-fs-stats %this))
	    obj)))
   
   (alist->jsobject
      `((readdir . ,(js-make-function %this readdir 1 "readdir"))
	(Stats . ,(alist->jsobject `((prototype . ,(get-process-fs-stats %this)))))
	(lstat . ,(js-make-function %this lstat 1 "lstat")))))

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
	       (lambda (this a b c d) '#()) 4 "makeFastBuffer")
	    #t %this)
	 (alist->jsobject
	    `((SlowBuffer . ,SlowBuffer))))))

;*---------------------------------------------------------------------*/
;*    process-udp-wrap ...                                             */
;*---------------------------------------------------------------------*/
(define (process-udp-wrap %this)
   (with-access::JsGlobalObject %this (js-object)
      (alist->jsobject
	 `((UDP . ,(js-new %this js-object))))))

;*---------------------------------------------------------------------*/
;*    process-evals ...                                                */
;*---------------------------------------------------------------------*/
(define (process-evals %this)
   (with-access::JsGlobalObject %this (js-object)
      (alist->jsobject
	 `((NodeScript . ,(js-new %this js-object))))))

;*---------------------------------------------------------------------*/
;*    process-cares-wrap ...                                           */
;*---------------------------------------------------------------------*/
(define (process-cares-wrap %this)
   (with-access::JsGlobalObject %this (js-object)
      (alist->jsobject
	 `((isIP . ,(js-new %this js-object))))))

;*---------------------------------------------------------------------*/
;*    process-timer-wrap ...                                           */
;*---------------------------------------------------------------------*/
(define (process-timer-wrap %this)
   (with-access::JsGlobalObject %this (js-object)
      (alist->jsobject
	 `((Timer . ,(js-new %this js-object))))))

;*---------------------------------------------------------------------*/
;*    process-process-wrap ...                                         */
;*---------------------------------------------------------------------*/
(define (process-process-wrap %this)
   (with-access::JsGlobalObject %this (js-object)
      (alist->jsobject
	 `((Process . ,(js-new %this js-object))))))

;*---------------------------------------------------------------------*/
;*    process-crypto ...                                               */
;*---------------------------------------------------------------------*/
(define (process-crypto %this)
   (with-access::JsGlobalObject %this (js-object)
      (alist->jsobject
	 `((SecureContext . ,(js-new %this js-object))
	   (randomBytes . ,(js-new %this js-object))
	   (pseudoRandomBytes . ,(js-new %this js-object))
	   (getCiphers . ,(js-new %this js-object))
	   (getHashes . ,(js-new %this js-object))))))

;*---------------------------------------------------------------------*/
;*    process-http-parser ...                                          */
;*---------------------------------------------------------------------*/
(define (process-http-parser %this)
   (alist->jsobject
      `((HTTPParser . ,(js-make-function %this
			  (lambda (this) (js-undefined)) 0
			  "HTTPParser")))))

;*---------------------------------------------------------------------*/
;*    process-zlib ...                                                 */
;*---------------------------------------------------------------------*/
(define (process-zlib %this)
   (alist->jsobject
      `()))

;*---------------------------------------------------------------------*/
;*    process-hop ...                                                  */
;*---------------------------------------------------------------------*/
(define (process-hop %this)
   (with-access::JsGlobalObject %this (js-object)
      (alist->jsobject
	 `((currentRequest . ,(js-make-function %this
				 (lambda (this) (current-request))
				 0 "currentRequest"))
	   (withURL . ,(js-make-function %this
			  (lambda (this url success opt)
			     (nodejs-with-url url success opt %this))
			  3 "withURL"))
	   (withHOP . ,(js-make-function %this
			  (lambda (this svc success opt)
			     (nodejs-with-hop svc success opt %this))
			  3 "withHOP"))
	   (charsetConvert . ,(js-make-function %this
				 (lambda (this text from to)
				    (nodejs-charset-convert
				       this text from to %this))
				 3 "charsetConvert"))
	   (charset . ,(js-make-function %this
			  (lambda (this) (hop-charset))
			  0 "charset"))
	   (charsetSet . ,(js-make-function %this
			     (lambda (this v) (hop-charset-set! v))
			     1 "charset"))
	   (HTTPResponseFile . ,(js-make-function %this
				   (lambda (this v)
				      (instantiate::http-response-file
					 (request (current-request))
					 (file (js-tostring v %this))))
				   1 "HTTPResponseFile"))
	   (HTTPResponseAuthentication . ,(js-make-function %this
					     (lambda (this v)
						(user-access-denied (current-request)
						   (js-tostring v %this)))
					     1 "HTTPResponseAuthentication"))
	   (signal . ,(js-make-function %this
			 (lambda (this name v)
			    (hop-event-signal! (js-tostring name %this) v))
			 2 "signal"))
	   (broadcast . ,(js-make-function %this
			    (lambda (this name v)
			       (hop-event-broadcast! (js-tostring name %this) v))
			    2 "broadcast"))))))
	   	   



   
