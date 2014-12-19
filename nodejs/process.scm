;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/process.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 15:02:45 2013                          */
;*    Last change :  Sat Dec 13 08:56:13 2014 (serrano)                */
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
	   __nodejs__evals
	   __nodejs__http
	   __nodejs__crypto
	   __nodejs__buffer
	   __nodejs__process-wrap
	   __nodejs__tcp-wrap
	   __nodejs__pipe-wrap
	   __nodejs_uv
	   __nodejs_require)

   (export (class JsProcess::JsObject
	      (tcp-proto (default #f))
	      (fs-event-proto (default #f))
	      (buffer-binding (default #f))
	      (exiting::bool (default #f)))

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
	    (js-call1 %this tick (js-undefined) %process))
	 ;; stdios
	 (let* ((m (nodejs-require-core "node_stdio" %worker %this))
		(stdio (js-get m 'initNodeStdio %this)))
	    (js-call1 %this stdio (js-undefined) %process))
	 ;; process and exit
	 (let* ((m (nodejs-require-core "node_proc" %worker %this))
		(prockillexit (js-get m 'initProcessKillAndExit %this))
		(procchannel (js-get m 'initProcessChannel %this)))
	    (js-call1 %this prockillexit (js-undefined) %process)
	    (js-call1 %this procchannel (js-undefined) %process))
	 ;; timers
	 (let* ((m (nodejs-require-core "node_timers" %worker %this))
		(timers (js-get m 'initNodeTimers %this)))
	    (js-call0 %this timers (js-undefined)))
	 ;; events
	 (with-access::JsObject %process (__proto__)
	    (let* ((e (nodejs-require-core "events" %worker %this))
		   (em (js-get e 'EventEmitter %this))
		   (proto (js-get em 'prototype %this))
		   (add (js-get proto 'addListener %this))
		   (rem (js-get proto 'removeListener %this))
		   (exitarmed #f))
	       
	       (set! __proto__ proto)
	       
	       (define (on this signal proc)
		  (let ((sig (js-tostring signal %this)))
		     (cond
			((string=? sig "uncaughtException")
			 (js-worker-add-handler! %worker proc))
			((and (string=? sig "exit") (not exitarmed))
			 (with-access::WorkerHopThread %worker (onexit)
			    (set! onexit proc)))
;* 			;; MS 25 Nov 2014: commented out below...      */
;* 			 (set! exitarmed #t)                           */
;* 			       (js-make-function %this                 */
;* 				  (lambda (this retval)                */
;* 				     (js-call1                         */
;* 			 (register-exit-function!                      */
;* 			    (lambda (status)                           */
;* 			       (with-access::JsProcess %process (exiting) */
;* 				  (unless exiting                      */
;* 				     (set! exiting #t)                 */
;* 				     (let ((emit (js-get %process 'emit %this))) */
;* 					(js-call2 %this emit %process  */
;* 					   (string->js-string "exit")  */
;* 					   status))))                  */
;* 			       status)))                               */
			(else
			 (js-call2 %this add this signal proc)))))
	       
	       (define (remove this signal proc)
		  (let ((sig (js-tostring signal %this)))
		     (cond
			((string=? sig "uncaughtException")
			 (js-worker-remove-handler! %worker proc))
			((string=? sig "exit")
			 (with-access::WorkerHopThread %worker (onexit)
			    (set! onexit #f)))
			(else
			 (js-call2 %this rem this signal proc)))))
	       
	       ;; on
	       (let ((add (js-make-function %this on 2 "")))
		  (js-put! %process 'on add #f %this)
		  (js-put! %process 'addListener add #f %this))
	       ;; remove
	       (let ((rem (js-make-function %this remove 2 "")))
		  (js-put! %process 'removeListener rem #f %this)))))
      ;; bind process into %this
      (js-put! %this 'process %process #t %this)
      ;; return the process object
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

	 (define slab
	    (make-slab-allocator %this slowbuffer))

	 (define (display-value o port)
	    (if (isa? o JsTypedArray)
		(with-access::JsTypedArray o (%data byteoffset length)
		   (display (substring %data
			       (uint32->fixnum byteoffset)
			       (uint32->fixnum length))
		      port))
		(display o port))
	    (flush-output-port port))

	 (js-put! proc 'stdout
	    (js-alist->jsobject
	       `((write . ,(js-make-function %this
			      (lambda (this o)
				 (display-value o (current-output-port)))
			      1 "write"))
		 (writable . #t)
		 (_isStdio . #t)
		 (fd . 1))
	       %this)
	    #f %this)
	 (js-put! proc 'stderr
	    (js-alist->jsobject
	       `((write . ,(js-make-function %this
			      (lambda (this o)
				 (display-value o (current-error-port)))
			      1 "write"))
		 (writable . #t)
		 (_isStdio . #t)
		 (fd . 2))
	       %this)
	    #f %this)
	 (js-put! proc 'stdin
	    (js-alist->jsobject
	       `((read . ,(js-make-function %this
			      (lambda (this o)
				 (tprint "stdin read not implemented"))
			      1 "read"))
		 (writable . #f)
		 (_isStdio . #t)
		 (fd . 0))
	       %this)
	    #f %this)
	 (js-put! proc 'argv
	    (let ((jsargs (member "--" (command-line))))
	       (if jsargs
		   (let ((cmdline (cons (string->js-string (car (command-line)))
				     (map string->js-string (cdr jsargs)))))
		      (js-vector->jsarray (list->vector cmdline) %this))
		   (js-vector->jsarray
		      (list->vector (map string->js-string (command-line)))
		      %this)))
	    #f %this)
	 (js-put! proc 'execPath
	    (string->js-string (car (command-line))) #f %this)
	 (js-put! proc 'execArgv
	    (js-vector->jsarray
	       (list->vector (map string->js-string (cdr (command-line))))
	       %this)
	    #f %this)
	 (js-put! proc 'abort
	    (js-make-function %this
	       (lambda (this)
		  (exit 134))
	       0 "abort")
	    #f %this)
	 
	 (js-put! proc 'title (string->js-string (hop-name)) #f %this)
	 (js-put! proc 'version (string->js-string (hop-version)) #f %this)
	 
	 (js-put! proc 'exit
	    (js-make-function %this
	       (lambda (this status)
		  (let ((r (if (eq? status (js-undefined))
			       0
			       (js-tointeger status %this))))
		     (with-access::JsProcess proc (exiting)
			(unless exiting
			   (set! exiting #t)
			   (let ((emit (js-get proc 'emit %this)))
			      (js-call2 %this emit proc "exit" r)))
			(exit r))))
	       1 "exit")
	    #f %this)
	 (js-put! proc 'reallyExit
	    (js-make-function %this
	       (lambda (this status)
		  (exit (js-tointeger status %this)))
	       1 "exit")
	    #f %this)
	 (js-put! proc 'arch (string->js-string (os-arch)) #f %this)
	 (js-put! proc 'platform (string->js-string (os-name)) #f %this)
	 (js-put! proc 'binding
	    (js-make-function %this
	       (lambda (this module)
		  (let ((mod (js-string->string module)))
		     (cond
			((string=? mod "constants")
			 (process-constants %this))
			((string=? mod "fs")
			 (process-fs %worker %this))
			((string=? mod "buffer")
			 (process-buffer %this slowbuffer))
			((string=? mod "udp_wrap")
			 (process-udp-wrap %this))
			((string=? mod "tcp_wrap")
			 (process-tcp-wrap %worker %this proc slab slowbuffer))
			((string=? mod "pipe_wrap")
			 (process-pipe-wrap %worker %this proc slab))
			((string=? mod "evals")
			 (process-evals %worker %this))
			((string=? mod "cares_wrap")
			 (process-cares-wrap %worker %this proc))
			((string=? mod "timer_wrap")
			 (hopjs-process-timer %worker %this))
			((string=? mod "process_wrap")
			 (process-process-wrap %worker %this proc))
			((string=? mod "crypto")
			 (process-crypto %this))
			((string=? mod "http_parser")
			 (process-http-parser %this))
			((string=? mod "zlib")
			 (process-zlib %this))
			((string=? mod "os")
			 (process-os %this))
			((string=? mod "tty_wrap")
			 (process-tty-wrap %worker %this))
			((string=? mod "fs_event_wrap")
			 (process-fs-event-wrap %worker %this proc))
			((string=? mod "hop")
			 (hopjs-process-hop %worker %this))
			(else
			 (warning "%nodejs-process"
			    "binding not implemented: " mod)
			 (js-new %this js-object)))))
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
	 (let ((check #f)
	       (idle #f))
	    (js-bind! %this proc '_needImmediateCallback
	       :get (js-make-function %this
		       (lambda (this)
			  (isa? check UvCheck))
		       0 '_needImmediateCallback)
	       :set (js-make-function %this
		       (lambda (this val)
			  (let ((v (js-totest val)))
			     (cond
				((and v (not (isa? check UvCheck)))
				 (set! idle
				    (nodejs-make-idle
				       %worker %this
				       (lambda (_) #t)))
				 (set! check
				    (nodejs-make-check
				       %worker %this proc)))
				((and (not v) (isa? check UvCheck))
				 (nodejs-idle-stop %worker %this idle)
				 (set! idle #f)
				 (nodejs-check-stop %worker %this check)
				 (set! check #f)))))
		       1 '_needImmediateCallback)
	       :configurable #f))
	    
	 (js-put! proc 'cwd
	    (js-make-function %this
	       (lambda (this)
		  (string->js-string (pwd)))
	       0 "cwd")
	    #f %this)
	 (js-put! proc 'chdir
	    (js-make-function %this
	       (lambda (this path)
		  (chdir (js-string->string path)))
	       1 "chdir")
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
	 (js-put! proc 'getgid
	    (js-make-function %this
	       (lambda (this) (getgid))
	       0 "getgid")
	    #f %this)
	 (js-put! proc 'setgid
	    (js-make-function %this
	       (lambda (this val) (setgid (js-tointeger val %this)))
	       1 "setgid")
	    #f %this)
	 (js-put! proc 'umask
	    (js-make-function %this
	       (lambda (this val)
		  (cond
		     ((eq? val (js-undefined))
		      (umask))
		     ((js-string? val)
		      (umask (string->integer (js-string->string val) 8)))
		     (else
		      (umask (js-tointeger val %this)))))
	       1 "umask")
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

	 ;; hrtime
	 (js-put! proc 'hrtime
	    (js-make-function %this
	       (lambda (this diff)
		  (let* ((t (current-nanoseconds))
			 (t0 (llong->flonum (/llong t #l1000000000)))
			 (t1 (llong->flonum (remainderllong t #l1000000000))))
		     (if (eq? diff (js-undefined))
			 (js-vector->jsarray (vector t0 t1) %this)
			 (let ((dt0 (js-get diff 0 %this))
			       (dt1 (js-get diff 1 %this)))
			 (js-vector->jsarray
			    (vector (- t0 dt0) (- t1 dt1)) %this)))))
			 
	       1 "hrtime")
	    #t %this)

	 ;; kill
	 (js-put! proc '_kill
	    (js-make-function %this
	       (lambda (this pid sig)
		  (nodejs-kill %worker %this proc pid sig))
	       2 "_kill")
	    #t %this)
	 
	 (for-each not-implemented
	    '(_getActiveRequest
	      _getActiveHandles
	      getgroups
	      setgroups
	      initgroups
	      _debugProcess
	      _debugPause
	      _debugEnd
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
	(S_IFREG . ,S_IFREG)
	(S_IFBLK . ,S_IFBLK)
	(S_IFCHR . ,S_IFCHR)
	(S_IFLNK . ,S_IFLNK)
	(S_IFIFO . ,S_IFIFO)
	(S_IFSOCK . ,S_IFSOCK)

	(SIGINT . ,sigint)
	(SIGKILL . ,sigkill)
	(SIGFPE . ,sigfpe)
	(SIGILL . ,sigill)
	(SIGBUS . ,sigbus)
	(SIGSEGV . ,sigsegv)
	(SIGPIPE . ,sigpipe)
	(SIGTERM . ,sigterm))
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
	    `(UDP
		bind
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
;*    process-fs-event-wrap ...                                        */
;*---------------------------------------------------------------------*/
(define (process-fs-event-wrap %worker %this process)
   
   (define (create-fs-event-proto)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new %this js-object)))
	    
	    (js-put! obj 'start
	       (js-make-function %this
		  (lambda (this::JsHandle path options listener)
		     (with-access::JsHandle this (handle)
			(js-put! this 'initialized_ #t #f %this)
			(nodejs-fs-event-start handle
			   (lambda (_ path events status)
			      ;; see fs_event_wrap.cc
			      (let ((eventstr "")
				    (onchange (js-get this 'onchange %this)))
				 (cond
				    ((not (=fx status 0))
				     (js-put! process '_errno
					(nodejs-err-name status)
					#f %this))
				    ((=fx (uv-fs-event-change)
					(bit-and events (uv-fs-event-change)))
				     (set! eventstr
					(string->js-string "change")))
				    ((=fx (uv-fs-event-rename)
					(bit-and events (uv-fs-event-rename)))
				     (set! eventstr
					(string->js-string "rename")))
				    (else
				     (error "process-fs-event-wrap"
					"bad event" eventstr)))
				 (js-call3 %this onchange this
				    status eventstr
				    (string->js-string path))))
			   (js-string->string path)))
		     (unless (js-totest options)
			(with-access::JsHandle this (handle)
			   (nodejs-unref handle %worker))))
		  3 "start")
	       #f %this)
	    
	    (js-put! obj 'close
	       (js-make-function %this
		  (lambda (this)
		     (js-put! this 'initialized_ #f #f %this)
		     (with-access::JsHandle this (handle)
			(nodejs-fs-event-stop handle)))
		  1 "close")
	       #f %this)
	    
	    obj)))
   
   (define (get-fs-event-proto process)
      (with-access::JsProcess process (fs-event-proto)
	 (unless fs-event-proto
	    (set! fs-event-proto (create-fs-event-proto)))
	 fs-event-proto))
   
   (define (fs-event this)
      (instantiate::JsHandle
	 (handle (nodejs-make-fs-event %worker))
	 (__proto__ (get-fs-event-proto process))))
   
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 `((FSEvent . ,(js-make-function %this fs-event 0 "FSEvent"
			  :alloc (lambda (o) #unspecified)
			  :construct fs-event)))
	 %this)))

;*---------------------------------------------------------------------*/
;*    process-cares-wrap ...                                           */
;*---------------------------------------------------------------------*/
(define (process-cares-wrap %worker %this process)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "care_wrap"
	       (format "binding not implemented ~a" name)
	       l))
	 0 name))
   
   (define (getaddrinfo this domain family)
      (nodejs-getaddrinfo %worker %this process domain family))
   
   (define (query this domain family callback)
      (nodejs-query %worker %this process domain family callback))
   
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
			  (nodejs-isip (js-tojsstring domain %this)))
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
(define (process-tty-wrap %worker %this)
   
   (define (not-implemented name::symbol)
      (js-make-function %this
	 (lambda (this . l)
	    (error "tty_wrap" "binding not implemented" name))
	 0 (symbol->string name)))
   
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 (append
	    `((isTTY . ,(js-make-function %this
			   (lambda (this fd)
			      (nodejs-istty %worker %this fd))
			   1 'isTTY))
	      (guessHandleType . ,(js-make-function %this
				     (lambda (this fd)
					(nodejs-guess-handle-type
					   %worker %this fd))
				     1 'guessHandleType)))
	    (map (lambda (id)
		    (cons id (not-implemented id)))
	       `(close
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
			 ((?name ?addr ?family ?- ?internal . ?-)
			  (let* ((id (string->symbol (car i)))
				 (desc (js-alist->jsobject
					  `((address . ,addr)
					    (family . ,family)
					    (internal . ,internal))
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
				    (string->js-string "LE")
				    (string->js-string "BE")))
			     0 "endianess"))
	(getHostname . ,(js-make-function %this
			   (lambda (this)
			      (string->js-string (hostname)))
			   0 "getHostname"))
	(getOSType . ,(js-make-function %this
			 (lambda (this)
			    (string->js-string (os-name)))
			 0 "getOSType"))
	(getOSRelease . ,(js-make-function %this
			    (lambda (this)
			       (string->js-string (os-version)))
			    0 "getOSRelease"))
	(getInterfaceAddresses . ,(js-make-function %this
				     (lambda (this)
					(interfaces->js))
				     0 "getInterfaceAddresses"))
	(getUptime . ,(js-make-function %this
			 (lambda (this)
			    (nodejs-getuptime))
			 0 "getUptime"))
	(getLoadAvg . ,(js-make-function %this
			  (lambda (this)
			     (let* ((f64 (js-get %this 'Float64Array %this))
				    (obj (js-new %this f64 3)))
				(with-access::JsTypedArray obj (buffer)
				   (with-access::JsArrayBuffer buffer (data)
				      (nodejs-loadavg data))) obj))
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
