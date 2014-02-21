;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/nodejs/process.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 15:02:45 2013                          */
;*    Last change :  Fri Feb 14 12:10:33 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    NodeJS process object                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_process
   
   (library hopscript)

   (include "nodejs.sch")
   
   (export (%nodejs-process)))

;*---------------------------------------------------------------------*/
;*    %nodejs-process ...                                              */
;*---------------------------------------------------------------------*/
(define (%nodejs-process)
   (let ((proc (js-new js-object)))
      (js-put! proc 'title (hop-name) #f)
      (js-put! proc 'version (hop-version) #f)
      (js-put! proc 'exit
	 (js-make-function
	    (lambda (this status)
	       (exit status))
	    2 "exit")
	 #f)
      (js-put! proc 'platform (os-name) #f)
      (js-put! proc 'binding
	 (js-make-function
	    (lambda (this module)
	       (cond
		  ((string=? module "constants")
		   (process-constants))
		  ((string=? module "fs")
		   (process-fs))
		  ((string=? module "buffer")
		   (process-buffer))
		  ((string=? module "udp_wrap")
		   (process-udp-wrap))
		  ((string=? module "evals")
		   (process-evals))
		  ((string=? module "cares_wrap")
		   (process-cares-wrap))
		  ((string=? module "timer_wrap")
		   (process-timer-wrap))
		  ((string=? module "process_wrap")
		   (process-process-wrap))
		  ((string=? module "crypto")
		   (process-crypto))
		  ((string=? module "http_parser")
		   (process-http-parser))
		  ((string=? module "zlib")
		   (process-zlib))
		  (else
		   (warning "%nodejs-process"
		      "binding not implemented: " module)
		   (js-new js-object))))
	    2 "binding")
	 #f)
      (js-put! proc 'stdout
	 (alist->jsobject
	    `((write . ,(lambda (this o) (display o)))))
	 #f)
      (js-put! proc 'stderr
	 (alist->jsobject
	    `((write . ,(lambda (this o) (display o)))))
	 #f)
      (js-put! proc 'env
	 (alist->jsobject
	    `((NODE_DEBUG . ,(getenv "NODE_DEBUG"))))
	 #f)
      (js-put! proc '_usingDomains
	 (js-make-function (lambda (this) (js-undefined)) 0 "_usingDomains")
	 #f)
      proc))

;*---------------------------------------------------------------------*/
;*    constants ...                                                    */
;*---------------------------------------------------------------------*/
(define (S_IFDIR) 4)
(define (S_IFREG) 8)

;*---------------------------------------------------------------------*/
;*    process-constants ...                                            */
;*---------------------------------------------------------------------*/
(define (process-constants)
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
(define (get-process-fs-stats)
   (unless process-fs-stats (set! process-fs-stats (js-new js-object)))
   process-fs-stats)

;*---------------------------------------------------------------------*/
;*    process-fs ...                                                   */
;*---------------------------------------------------------------------*/
(define (process-fs)
   (alist->jsobject
      `((readdir . ,(js-make-function readdir 1 "readdir"))
	(Stats . ,(alist->jsobject `((prototype . ,(get-process-fs-stats)))))
	(lstat . ,(js-make-function lstat 1 "lstat")))))

;*---------------------------------------------------------------------*/
;*    readdir ...                                                      */
;*---------------------------------------------------------------------*/
(define (readdir this path)
   (js-vector->jsarray (list->vector (directory->path-list path))))

;*---------------------------------------------------------------------*/
;*    lstat ...                                                        */
;*---------------------------------------------------------------------*/
(define (lstat this path)
   (let ((obj (alist->jsobject
		 `((mode . ,(if (directory? path) (S_IFDIR) (S_IFREG)))))))
      (with-access::JsObject obj (__proto__)
	 (set! __proto__ (get-process-fs-stats))
	 obj)))

;*---------------------------------------------------------------------*/
;*    slowbuffer ...                                                   */
;*---------------------------------------------------------------------*/
(define (slowbuffer this len)
   (make-vector len))

;*---------------------------------------------------------------------*/
;*    process-buffer ...                                               */
;*---------------------------------------------------------------------*/
(define (process-buffer)
   (let ((SlowBuffer (js-make-function slowbuffer 1 "SlowBuffer"
			:alloc js-object-alloc
			:construct slowbuffer
			:prototype (js-new js-object))))
      (js-put! SlowBuffer 'byteLength
	 (js-make-function (lambda (this) 0) 0 "ByteLength")
	 #t)
      (js-put! SlowBuffer 'makeFastBuffer
	 (js-make-function (lambda (this a b c d) '#()) 4 "makeFastBuffer")
	 #t)
      (alist->jsobject
	 `((SlowBuffer . ,SlowBuffer)))))

;*---------------------------------------------------------------------*/
;*    process-udp-wrap ...                                             */
;*---------------------------------------------------------------------*/
(define (process-udp-wrap)
   (alist->jsobject
      `((UDP . ,(js-new js-object)))))

;*---------------------------------------------------------------------*/
;*    process-evals ...                                                */
;*---------------------------------------------------------------------*/
(define (process-evals)
   (alist->jsobject
      `((NodeScript . ,(js-new js-object)))))

;*---------------------------------------------------------------------*/
;*    process-cares-wrap ...                                           */
;*---------------------------------------------------------------------*/
(define (process-cares-wrap)
   (alist->jsobject
      `((isIP . ,(js-new js-object)))))

;*---------------------------------------------------------------------*/
;*    process-timer-wrap ...                                           */
;*---------------------------------------------------------------------*/
(define (process-timer-wrap)
   (alist->jsobject
      `((Timer . ,(js-new js-object)))))

;*---------------------------------------------------------------------*/
;*    process-process-wrap ...                                         */
;*---------------------------------------------------------------------*/
(define (process-process-wrap)
   (alist->jsobject
      `((Process . ,(js-new js-object)))))

;*---------------------------------------------------------------------*/
;*    process-crypto ...                                               */
;*---------------------------------------------------------------------*/
(define (process-crypto)
   (alist->jsobject
      `((SecureContext . ,(js-new js-object))
	(randomBytes . ,(js-new js-object))
	(pseudoRandomBytes . ,(js-new js-object))
	(getCiphers . ,(js-new js-object))
	(getHashes . ,(js-new js-object)))))

;*---------------------------------------------------------------------*/
;*    process-http-parser ...                                          */
;*---------------------------------------------------------------------*/
(define (process-http-parser)
   (alist->jsobject
      `((HTTPParser . ,(js-make-function (lambda (this) (js-undefined)) 0
			  "HTTPParser")))))

;*---------------------------------------------------------------------*/
;*    process-zlib ...                                                 */
;*---------------------------------------------------------------------*/
(define (process-zlib)
   (alist->jsobject
      `()))
