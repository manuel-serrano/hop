;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/process.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 15:02:45 2013                          */
;*    Last change :  Mon Sep  1 07:20:57 2014 (serrano)                */
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

   (include "nodejs.sch"
	    "nodejs_debug.sch")

   (import __nodejs__hop
	   __nodejs__timer
	   __nodejs__fs
	   __nodejs__http
	   __nodejs__crypto
	   __nodejs__buffer
	   __nodejs_uv
	   __nodejs_require)

   (static (class JsProcess::JsObject
	      (tcp-proto (default #f))
	      (buffer-binding (default #f)))

	   (class JsHandle::JsObject
	      (handle (default #f))))

   (export (nodejs-process ::WorkerHopThread ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    nodejs-process ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-process %worker::WorkerHopThread %this::JsGlobalObject)
   (with-access::WorkerHopThread %worker (%process)
      (unless %process
	 (set! %process (new-process-object %worker %this))
	 ;; init tick machinery
	 (let* ((m (nodejs-require-core "node_tick" %worker %this))
		(tick (js-get m 'initNodeTick %this)))
	    (js-call1 %this tick (js-undefined) %process)))
      %process))

;*---------------------------------------------------------------------*/
;*    new-process-object ...                                           */
;*---------------------------------------------------------------------*/
(define (new-process-object %worker::WorkerHopThread %this)
   (with-access::JsGlobalObject %this (js-object)
      (let ((proc (instantiate::JsProcess
		     (__proto__ (js-new %this js-object)))))

	 (define (not-implemented name)
	    (js-put! proc name
	       (js-make-function %this
		  (lambda (this . l)
		     (error "process" "binding not implemented" name))
		  0 (symbol->string name))
	       #f %this))

	 (define slowbuffer
	    (make-slowbuffer %this))

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
	 (js-put! proc 'argv
	    (js-vector->jsarray (list->vector (command-line)) %this)
	    #f %this)
	 (js-put! proc 'execPath (car (command-line)) #f %this)
	 (js-put! proc 'execArgv
	    (js-vector->jsarray (list->vector (cdr (command-line))) %this)
	    #f %this)
	 (js-put! proc 'abort
	    (js-make-function %this
	       (lambda (this)
		  (exit 134))
	       0 "abort")
	    #f %this)
	 
	 (js-put! proc 'title (hop-name) #f %this)
	 (js-put! proc 'version (hop-version) #f %this)
	 (js-put! proc 'exit
	    (js-make-function %this
	       (lambda (this status)
		  (exit status))
	       1 "exit")
	    #f %this)
	 (js-put! proc 'reallyExit
	    (js-make-function %this
	       (lambda (this status)
		  (exit status))
	       1 "exit")
	    #f %this)
	 (js-put! proc 'arch (os-arch) #f %this)
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
		      (process-buffer %this slowbuffer))
		     ((string=? module "udp_wrap")
		      (process-udp-wrap %this))
		     ((string=? module "tcp_wrap")
		      (process-tcp-wrap %this proc slowbuffer))
		     ((string=? module "pipe_wrap")
		      (process-pipe-wrap %this))
		     ((string=? module "evals")
		      (process-evals %this))
		     ((string=? module "cares_wrap")
		      (process-cares-wrap %this proc))
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
		     ((string=? module "os")
		      (process-os %this))
		     ((string=? module "tty_wrap")
		      (process-tty-wrap %this))
		     ((string=? module "fs_event_wrap")
		      (process-fs-event-wrap %this))
		     ((string=? module "hop")
		      (hopjs-process-hop %this %worker))
		     (else
		      (warning "%nodejs-process"
			 "binding not implemented: " module)
		      (js-new %this js-object))))
	       2 "binding")
	    #f %this)
	 (js-put! proc 'env
	    (js-alist->jsobject (getenv) %this)
	    #f %this)
	 (js-put! proc 'pid (getpid)
	    #f %this)
	 (js-put! proc 'features
	    (js-alist->jsobject
	       `((debug . ,(>fx (bigloo-debug) 0))
		 (uv . #t)
		 (ipv6 . #t)
		 (tls_npm . #f)
		 (tls_sni . #f)
		 (tls . #t))
	       %this)
	    #f %this)
	 (js-put! proc 'cwd
	    (js-make-function %this
	       (lambda (this) (pwd))
	       0 "cwd")
	    #f %this)
	 (js-put! proc 'getuid
	    (js-make-function %this
	       (lambda (this) (getuid))
	       0 "getuid")
	    #f %this)
	 (js-put! proc 'setuid
	    (js-make-function %this
	       (lambda (this val) (setuid (js-tointeger val %this)))
	       1 "setuid")
	    #f %this)
	 
	 (js-put! proc '_usingDomains
	    (js-make-function %this
	       (lambda (this) (js-undefined)) 0 "_usingDomains")
	    #f %this)

	 ;; tick
	 (js-put! proc '_tickInfoBox
	    (js-vector->jsarray (make-vector 3 0) %this)
	    #f %this)
	 (js-put! proc '_needTickCallback
	    (js-make-function %this
	       (lambda (this)
		  (nodejs-need-tick-callback %worker %this proc))
	       0 "needTickCallback")
	    #f %this)
		  
	 (for-each not-implemented
	    '(_getActiveRequest
	      _getActiveHandles
	      chdir
	      setgid
	      getgid
	      getgroups
	      setgroups
	      initgroups
	      _kill
	      _debugProcess
	      _debugPause
	      _debugEnd
	      hrtime
	      dlopen
	      uptime
	      memoryUsage))
	 
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
;*    process-udp-wrap ...                                             */
;*---------------------------------------------------------------------*/
(define (process-udp-wrap %this)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "udp_wrap" "binding not implemented" name))
	 0 (symbol->string name)))
   
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 (map (lambda (id)
		 (cons id (not-implemented id)))
	    `(bind
		send
		bind6
		send6
		close
		recvStart
		recvStop
		getsockname
		addMembership
		dropMembership
		setMulticastTTL
		setMulticastLoopback
		setBroadcast
		setTTL
		ref
		unref))
	 %this)))

;*---------------------------------------------------------------------*/
;*    stream-write-string ...                                          */
;*---------------------------------------------------------------------*/
(define (stream-write-string %this this::JsHandle
	   string::bstring offset::long len::long
	   encoding callback)
   (with-access::JsGlobalObject %this (js-object)
      (let ((ipc #f))
	 (if ipc
	     (error "stream-write-string" "IPC Not implemented yet" this)
	     (let ((req (js-new %this js-object)))
		(js-put! req 'bytes len #f %this)
		(with-access::JsHandle this (handle)
		   (nodejs-stream-write %this handle
		      string offset len
		      (lambda (status)
			 (let ((oncomp (js-get req 'oncomplete %this)))
			    (js-call3 %this oncomp req status this req)
			    (js-put! this 'writeQueueSize
			       (nodejs-stream-write-queue-size handle) #f %this)
			    (js-undefined)))))
		req)))))

;*---------------------------------------------------------------------*/
;*    ucs2-string->buffer ...                                          */
;*---------------------------------------------------------------------*/
(define (ucs2-string->buffer ucs2string)
   (let* ((len (ucs2-string-length ucs2string))
	  (buf (make-string (*fx len 2))))
      (let loop ((i 0))
	 (if (=fx i len)
	     buf
	     (let* ((u (ucs2-string-ref ucs2string i))
		    (n (ucs2->integer u))
		    (i2 (*fx i 2)))
		(string-set! buf i2 (integer->char (bit-rsh n 8)))
		(string-set! buf (+fx 1 i2) (integer->char (bit-and n #x255)))
		(loop (+fx i 1)))))))
		
;*---------------------------------------------------------------------*/
;*    process-tcp-wrap ...                                             */
;*---------------------------------------------------------------------*/
(define (process-tcp-wrap %this process::JsProcess slowbuffer::JsObject)
   
   (define (->fixnum n)
      (cond
	 ((fixnum? n) n)
	 ((flonum? n) (flonum->fixnum n))
	 (else 0)))

   (define (connect family)
      (with-access::JsGlobalObject %this (js-object)
	 (lambda (this host port callback)
	    (with-access::JsHandle this (handle)
	       (let ((req (js-new %this js-object)))
		  (nodejs-tcp-connect %this handle host
		     (->fixnum (js-tointeger port %this)) family
		     (lambda (status handle)
			(when (<fx status 0)
			   (js-put! process '_errno
			      (uv-err-name status) #f %this))
			(let ((oncomp (js-get req 'oncomplete %this)))
			   (js-call5 %this oncomp req status
			      this req #t #t)
			   (js-undefined))))
		  req)))))

   (define (create-tcp-proto)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new %this js-object)))
	    
	    (js-put! obj 'close
	       (js-make-function %this
		  (lambda (this cb)
		     (with-access::JsHandle this (handle)
			(nodejs-close %this handle cb)))
		  1 "close")
	       #f %this)
	    
	    (js-put! obj 'ref
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-ref handle)))
		  0 "ref")
	       #f %this)
	    
	    (js-put! obj 'unref
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-unref handle)))
		  0 "unref")
	       #f %this)
	    
	    (js-put! obj 'connect
	       (js-make-function %this
		  (connect 4)
		  3 "connect")
	       #f %this)
	    
	    (js-put! obj 'connect6
	       (js-make-function %this
		  (connect 6)
		  3 "connect")
	       #f %this)
	    
	    (js-put! obj 'writeBuffer
	       (js-make-function %this
		  (lambda (this buffer)
		     (with-access::JsTypedArray buffer (%vec byteoffset length)
			(stream-write-string %this this
			   %vec
			   (uint32->fixnum byteoffset)
			   (uint32->fixnum length)
			   "ascii" #f)))
		  1 "writeBuffer")
	       #f %this)
	    
	    (js-put! obj 'writeAsciiString
	       (js-make-function %this
		  (lambda (this string)
		     (stream-write-string %this this
			string 0 (string-length string)
			"ascii" #f))
		  1 "writeAsciiString")
	       #f %this)
	    
	    (js-put! obj 'writeUtf8String
	       (js-make-function %this
		  (lambda (this string)
		     (stream-write-string %this this
			string 0 (string-length string)
			"utf8" #f))
		  1 "writeUtf8String")
	       #f %this)
	    
	    (js-put! obj 'writeUcs2String
	       (js-make-function %this
		  (lambda (this string)
		     (let* ((ucs2string (utf8-string->ucs2-string string))
			    (buffer (ucs2-string->buffer ucs2string)))
			(stream-write-string %this this
			   string 0 (string-length string)
			   "ascii" #f)))
		  1 "writeUcs2String")
	       #f %this)
	    
	    (js-put! obj 'readStart
	       (js-make-function %this
		  (lambda (this)
		     (let ((slab (make-slab-allocator %this slowbuffer)))
		     (with-access::JsHandle this (handle)
			(nodejs-stream-read-start %this handle
			   (lambda (obj size)
			      (slab-allocate slab obj size))
			   (lambda (buf offset len)
			      (if (integer? buf)
				  (begin
				     (slab-shrink! slab offset 0)
				     (js-put! process '_errno
					(uv-err-name buf) #f %this))
				  (slab-shrink! slab offset len))
			      (let ((onread (js-get this 'onread %this)))
				 (js-call3 %this onread this buf offset len)
				 (js-undefined))))))
		  0 "readStart")
	       #f %this)
	    
	    (js-put! obj 'readStop
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHandle this (handle)
			(nodejs-stream-read-stop %this handle)))
		  0 "readStop")
	       #f %this)
	    
	    (js-put! obj 'setNoDelay
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-nodelay handle (js-totest val))))
		  1 "setNoDelay")
	       #f %this)
	    
	    (js-put! obj 'setKeepAlive
	       (js-make-function %this
		  (lambda (this val tmt)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-keepalive handle (js-totest val)
			   (->fixnum (js-tointeger tmt %this)))))
		  2 "setKeepAlive")
	       #f %this)
	    
	    (js-put! obj 'setSimultaneousAccepts
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-simultaneous-accepts
			   handle (js-totest val))))
		  1 "setSimultaneousAccepts")
	       #f %this)
	    
	    (js-put! obj 'getsockname
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-getsockname %this handle)))
		  1 "getsockname")
	       #f %this)
	    
	    (js-put! obj 'getpeername
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-getpeername %this handle)))
		  1 "getpeername")
	       #f %this)
	    
	    (js-put! obj 'shutdown
	       (js-make-function %this
		  (lambda (this val)
		     (with-access::JsHandle this (handle)
			(let* ((req (js-new %this js-object))
			       (res (nodejs-stream-shutdown %this handle
				       (lambda (status handle)
					  (when (<fx status 0)
					     (js-put! process '_errno
						(uv-err-name status) #f %this))))))
			   (=fx res 0))))
		  1 "shutdown")
	       #f %this)
	    
	    (js-put! obj 'open
	       (js-make-function %this
		  (lambda (this handle fd)
		     (tprint "UNTESTED, example needed")
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-open %this handle fd)))
		  2 "open")
	       #f %this)

	    (js-put! obj 'bind
	       (js-make-function %this
		  (lambda (this addr port)
		     (tprint "UNTESTED, example needed")
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-bind %this handle addr (or port 0) 4)))
		  2 "bind")
	       #f %this)
	    
	    (js-put! obj 'bind6
	       (js-make-function %this
		  (lambda (this addr port)
		     (tprint "UNTESTED, example needed")
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-bind %this handle addr (or port 0) 6)))
		  2 "bind6")
	       #f %this)

	    (js-put! obj 'listen
	       (js-make-function %this
		  (lambda (this backlog)
		     (tprint "UNTESTED, example needed")
		     (with-access::JsHandle this (handle)
			(nodejs-tcp-listen %this process this handle
			   (->fixnum (js-tointeger backlog %this)))))
		  1 "listen")
	       #f %this)
	    
	    obj)))
   
   (define (get-tcp-proto)
      (with-access::JsProcess process (tcp-proto)
	 (unless tcp-proto
	    (set! tcp-proto (create-tcp-proto)))
	 tcp-proto))
   
   (define (TCP this)
      (with-access::JsGlobalObject %this (js-object)
	 (let* ((hdl (nodejs-tcp-handle))
		(obj (instantiate::JsHandle
			(handle hdl)
			(__proto__ (get-tcp-proto)))))
	    (js-bind! %this obj 'fd
	       :get (js-make-function %this
		       (lambda (this)
			  (nodejs-stream-fd hdl))
		       0 'getGD)
	       :writable #f :configurable #f)
	    (js-put! obj 'writeQueueSize
	       (nodejs-stream-write-queue-size hdl) #f %this)
	    obj)))
   
   (with-access::JsGlobalObject %this (js-object)
      (let ((obj (js-new %this js-object)))
	 (js-put! obj 'TCP
	    (js-make-function %this TCP 0 "TCP"
	       :alloc (lambda (o) #unspecified)
	       :construct TCP)
	    #f %this)
	 obj)))

;*---------------------------------------------------------------------*/
;*    process-pipe-wrap ...                                            */
;*---------------------------------------------------------------------*/
(define (process-pipe-wrap %this)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "pipe_wrap" "binding not implemented" name))
	 0 (symbol->string name)))
   
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 (map (lambda (id)
		 (cons id (not-implemented id)))
	    `(Pipe))
	 %this)))

;*---------------------------------------------------------------------*/
;*    process-fs-event-wrap ...                                        */
;*---------------------------------------------------------------------*/
(define (process-fs-event-wrap %this)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "fs_event_wrap" "binding not implemented" name))
	 0 (symbol->string name)))
   
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 (map (lambda (id)
		 (cons id (not-implemented id)))
	    `(start close))
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
(define (process-cares-wrap %this process)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "care_wrap"
	       (format "binding not implemented ~a" name)
	       l))
	 0 name))
   
   (define (getaddrinfo this domain family)
      (nodejs-getaddrinfo %this process domain family))
   
   (define (query this domain family callback)
      (nodejs-query %this process domain family callback))
   
   (define (query4 this domain callback)
      (query this domain 4 callback))
   
   (define (query6 this domain callback)
      (query this domain 6 callback))
   
   (define (gethostbyaddr this addr callback)
      (let ((res (hostname addr)))
	 (if (string=? res addr)
	     (begin
		(js-call2 %this callback (js-undefined) -1 #f)
		(js-put! process '_errno -1 #f %this)
		#f)
	     (begin
		(js-call2 %this callback (js-undefined) #f res)
		#t))))
   
   (define (gethostbyname this name callback)
      (let ((res (hostname name)))
	 (if (pair? res)
	     (let ((addr (assq 'addresses res)))
		(js-call2 %this callback (js-undefined) #f (car (cdr addr)))
		#t)
	     (begin
		(js-call2 %this callback (js-undefined) -1 #f)
		(js-put! process '_errno -1 #f %this)
		#f))))

   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 `((isIP . ,(js-make-function %this
		       (lambda (this domain)
			  (nodejs-isip domain))
		       1 'isIP))
	   (getaddrinfo . ,(js-make-function %this getaddrinfo 2 "getaddrinfo"))
	   (queryA . ,(js-make-function %this query4 2 "queryA"))
	   (queryAaaa . ,(js-make-function %this query6 2 "queryAaaa"))
	   (queryCname . ,(not-implemented "queryCname"))
	   (queryMx . ,(not-implemented "queryMx"))
	   (queryNs . ,(not-implemented "queryNs"))
	   (queryTxt . ,(not-implemented "queryTxt"))
	   (querySrv . ,(not-implemented "querySrv"))
	   (queryNaptr . ,(not-implemented "queryNaptr"))
	   (getHostByAddr . ,(js-make-function %this gethostbyaddr 2 "gethostbyaddr"))
	   (getHostByName . ,(js-make-function %this gethostbyname 2 "gethostbyname")))
	 %this)))

;*---------------------------------------------------------------------*/
;*    process-tty-wrap ...                                             */
;*---------------------------------------------------------------------*/
(define (process-tty-wrap %this)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "tty_wrap" "binding not implemented" name))
	 0 (symbol->string name)))
   
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 (append
	    `((isTTY . ,(js-make-function %this
			   (lambda (this fd)
			      (nodejs-istty fd))
			   1 'isTTY))
	      (guessHandleType . (js-make-function %this
				    (lambda (this fd)
				       (nodejs-guess-handle-type fd))
				    1 'guessHandleType)))
	    (map (lambda (id)
		    (cons id (not-implemented id)))
	       `(guessHandleType
		   close
		   unref
		   readStart
		   readStop
		   writeBuffer
		   writeAsciiString
		   writeUtf8String
		   writeUcs2String
		   getWindowSize
		   setRawMode)))
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
;*    process-zlib ...                                                 */
;*---------------------------------------------------------------------*/
(define (process-zlib %this)
   (js-alist->jsobject `() %this))

;*---------------------------------------------------------------------*/
;*    process-os ...                                                   */
;*---------------------------------------------------------------------*/
(define (process-os %this)
   
   (define (interfaces->js)
      (let ((t '()))
	 (for-each (lambda (i)
		      (match-case i
			 ((?name ?addr ?family . ?-)
			  (let* ((id (string->symbol (car i)))
				 (desc (js-alist->jsobject
					  `((address . ,addr)
					    (family . ,family))
					  %this))
				 (en (assq id t)))
			     (if (not en)
				 (set! t (cons (cons id (list desc)) t))
				 (set-cdr! en (cons desc (cdr en))))))))
	    (get-interfaces))
	 (js-alist->jsobject
	    (map (lambda (i)
		    (cons (car i)
		       (js-vector->jsarray (list->vector (cdr i)) %this)))
	       t)
	    %this)))
   
   (js-alist->jsobject
      `((getEndianness . ,(js-make-function %this
			     (lambda (this)
				(if (eq? (bigloo-config 'endianess) 'little-endian)
				    "LE"
				    "BE"))
			     0 "endianess"))
	(getHostname . ,(js-make-function %this
			   (lambda (this)
			      (hostname))
			   0 "getHostname"))
	(getOSType . ,(js-make-function %this
			 (lambda (this)
			    (os-name))
			 0 "getOSType"))
	(getOSRelease . ,(js-make-function %this
			    (lambda (this)
			       (os-version))
			    0 "getOSRelease"))
	(getInterfaceAddresses . ,(js-make-function %this
				     (lambda (this)
					(interfaces->js))
				     0 "getInterfaceAddresses"))
	(getLoadAvg . ,(js-make-function %this
			  (lambda (this)
			     (let* ((f64 (js-get %this 'Float64Array %this))
				    (obj (js-new %this f64 3)))
				(with-access::JsTypedArray obj (buffer)
				   (with-access::JsArrayBuffer buffer (vec)
				      (nodejs-loadavg vec))) obj))
			  0 "getLoadAvg"))
	(getFreeMem . ,(js-make-function %this
			  (lambda (this)
			     (nodejs-getfreemem))
			  0 "getFreeMem"))
	(getTotalMem . ,(js-make-function %this
			   (lambda (this)
			      (nodejs-gettotalmem))
			   0 "getTotalMem"))
	(getCPUs . ,(js-make-function %this
		       (lambda (this)
			  (js-vector->jsarray
			     (vector-map! (lambda (cpu)
					     (js-alist->jsobject cpu %this))
				(nodejs-getcpus))
			     %this))
		       0 "getCPUs")))
      %this))
