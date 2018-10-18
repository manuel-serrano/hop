;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/nodejs/process.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 15:02:45 2013                          */
;*    Last change :  Wed May 17 11:16:55 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    NodeJS process object                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_process
   
   (option (set! *warning-overriden-variables* #f))
   
   (library hopscript hop js2scheme)

   (cond-expand
      (enable-ssl (library ssl)))

   (include "nodejs.sch"
	    "nodejs_debug.sch"
	    "nodejs_async.sch")

   (import __nodejs__hop
	   __nodejs__fs
	   __nodejs__evals
	   __nodejs__http
	   __nodejs__crypto
	   __nodejs__buffer
	   __nodejs__timer-wrap
	   __nodejs__process-wrap
	   __nodejs__tcp-wrap
	   __nodejs__udp-wrap
	   __nodejs__pipe-wrap
	   __nodejs__tty-wrap
	   __nodejs__zlib
	   __nodejs_uv
	   __nodejs_require)

   (export (class JsProcess::JsObject
	      (tcp-proto (default #f))
	      (udp-proto (default #f))
	      (tty-proto (default #f))
	      (js-udp (default #f))
	      (js-tcp (default #f))
	      (js-pipe (default #f))
	      (js-tty (default #f))
	      (fs-event-proto (default #f))
	      (fs-watcher-proto (default #f))
	      (buffer-binding (default #f))
	      (using-domains::bool (default #f))
	      (exiting::bool (default #f))
	      (tick-callback (default #f)))

	   (class JsHandle::JsObject
	      (handle (default #f))
	      (reqs::pair-nil (default '()))
	      (flags::int (default 0))))

   (export (nodejs-compiler-options-add! ::keyword ::obj)
	   (nodejs-process ::WorkerHopThread ::JsGlobalObject)
	   (process-ares-fail ::JsGlobalObject ::JsProcess ::int)))

;*---------------------------------------------------------------------*/
;*    nodejs-version ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-version) "0.10.32")

;*---------------------------------------------------------------------*/
;*    nodejs-compiler-options-add! ...                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-compiler-options-add! k v)
   (j2s-compile-options-set! (cons* k v (j2s-compile-options))))

;*---------------------------------------------------------------------*/
;*    nodejs-process ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-process %worker::WorkerHopThread %this::JsGlobalObject)
   (with-access::WorkerHopThread %worker (%process)
      (unless %process
	 ;; create the process object
	 (set! %process (new-process-object %worker %this))
	 ;; bind process into %this
	 (js-put! %this 'process %process #t %this)
	 ;; bind the process fatal error handler
	 (js-worker-add-handler! %worker
	    (js-make-function %this
	       (lambda (this exn)
		  (let ((fatal (js-get %process '_fatalException %this)))
		     (if (isa? fatal JsFunction)
			 (js-call1 %this fatal %process exn)
			 (raise exn))))
	       1 "fatalException"))
	 ;; init tick machinery
	 (let* ((m (nodejs-require-core "node_tick" %worker %this))
                (tick (js-get m 'initNodeTick %this)))
            (js-call1 %this tick (js-undefined) %process))
	 ;; events
	 (with-access::JsObject %process (__proto__)
	    (let* ((e (nodejs-require-core "events" %worker %this))
		   (em (js-get e 'EventEmitter %this))
		   (proto (js-get em 'prototype %this))
		   (add (js-get proto 'addListener %this))
		   (rem (js-get proto 'removeListener %this))
		   (remall (js-get proto 'removeAllListeners %this))
		   (exitarmed #f)
		   (sighdls '()))
	       
	       (set! __proto__ proto)
	       
	       (define (on this signame proc)
		  (let ((sig (js-tostring signame %this)))
		     (cond
			((and (string=? sig "exit") (not exitarmed))
			 (with-access::WorkerHopThread %worker (onexit)
			    (set! onexit proc))
			 (js-call2 %this add this signame proc))
			((assq (string->symbol sig) signals)
			 =>
			 (lambda (c)
			    (set! sighdls (cons c sighdls))
			    (if (eq? (car c) 'SIGTERM)
				(hop-sigterm-handler-set!
				   (lambda (n)
				      (js-call0 %this proc this)
				      '(js-worker-tick %worker)))
				(signal (cdr c)
				   (lambda (s)
				      (!js-callback0 "signal" %worker %this
					 proc this))))))
;* 				   (lambda (s)                         */
;* 				      (js-worker-push-thunk! %worker sig */
;* 					 (lambda ()                    */
;* 					    (js-call0 %this proc this)))))))) */
			(else
			 (js-call2 %this add this signame proc)))))
	       
	       (define (remove this signame proc)
		  (let ((sig (js-tostring signame %this)))
		     (cond
			((string=? sig "exit")
			 (with-access::WorkerHopThread %worker (onexit)
			  (set! onexit #f))
			 (js-call2 %this rem this signame proc))
			((assq (string->symbol sig) signals)
			 =>
			 (lambda (c)
			    (set! sighdls (remq! c sighdls))
			    (if (eq? (car c) 'SIGTERM)
				(hop-sigterm-handler-set!
				   hop-sigterm-default-handler)
				(signal (cdr c) 'default))))
			(else
			 (js-call2 %this rem this signame proc)))))
	       
	       (define (removeall this signame)
		  (let ((sig (js-tostring signame %this)))
		     (cond
			((string=? sig "exit")
			 (with-access::WorkerHopThread %worker (onexit)
			    (set! onexit #f)))
			((assq (string->symbol sig) signals)
			 =>
			 (lambda (c)
			    (set! sighdls (remq! c sighdls))
			    (if (eq? (car c) 'SIGTERM)
				(hop-sigterm-handler-set!
				   hop-sigterm-default-handler)
				(signal (cdr c) 'default))))
			(else
			 (js-call1 %this remall this signame)))))

	       ;; on
	       (let ((add (js-make-function %this on 2 "addListener")))
		  (js-put! %process 'on add #f %this)
		  (js-put! %process 'addListener add #f %this))
	       ;; remove
	       (let ((rem (js-make-function %this remove 2 "removeListener")))
		  (js-put! %process 'removeListener rem #f %this))
	       ;; removeALl
	       (let ((remall (js-make-function %this removeall 1 "removeAllListeners")))
		  (js-put! %process 'removeAllListeners remall #f %this))))
	 ;; stdios
	 (let* ((oldstdout (js-get %process 'stdout %this))
		(oldstderr (js-get %process 'stderr %this))
		(m (nodejs-require-core "node_stdio" %worker %this))
		(stdio (js-get m 'initNodeStdio %this)))
	    (js-call1 %this stdio (js-undefined) %process))
	 ;; console finalization
	 ;; for this a new console object is create and the core module
	 ;; console.exports value is updated
	 (let* ((stdout (js-get %process 'stdout %this))
		(stderr (js-get %process 'stderr %this))
		(mcon (nodejs-core-module "console" %worker %this))
		(exports (js-get mcon 'exports %this))
		(ctor (js-get exports 'Console %this))
		(con (js-new2 %this ctor stdout stderr)))
	    ;; update console.exports
	    (js-put! con 'Console ctor #f %this)
	    (js-put! mcon 'exports con #f %this))
	 ;; timers
	 (let* ((m (nodejs-require-core "node_timers" %worker %this))
		(timers (js-get m 'initNodeTimers %this)))
	    (js-call0 %this timers (js-undefined)))
	 ;; process and exit
	 (let* ((m (nodejs-require-core "node_proc" %worker %this))
		(fatal (js-get m 'initFatal %this))
		(assert (js-get m 'initAssert %this))
		(prockillexit (js-get m 'initProcessKillAndExit %this))
		(procchannel (js-get m 'initProcessChannel %this)))
	    (js-call1 %this fatal (js-undefined) %process)
	    (js-call1 %this assert (js-undefined) %process)
	    (js-call1 %this prockillexit (js-undefined) %process)
	    (js-call1 %this procchannel (js-undefined) %process))
	 ;; cluster
	 (let* ((m (nodejs-require-core "node_cluster" %worker %this))
		(cluster (js-get m 'initNodeCluster %this)))
	    (js-call0 %this cluster (js-undefined))))

      ;; return the process object
      %process))

;*---------------------------------------------------------------------*/
;*    new-process-object ...                                           */
;*---------------------------------------------------------------------*/
(define (new-process-object %worker::WorkerHopThread %this)
   (with-access::JsGlobalObject %this (js-object)
      (let ((proc (instantiate::JsProcess
		     (cmap (instantiate::JsConstructMap))
		     (__proto__ (js-new %this js-object)))))

	 (define (not-implemented name)
	    (js-put! proc name
	       (js-make-function %this
		  (lambda (this . l)
		     (error "process" "binding not implemented" name))
		  0 (symbol->string name))
	       #f %this))

	 (define prog-start-time::uint64 (nodejs-uptime %worker))

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

	 (define (domain-call this)
	    (lambda (callback)
	       ;; this is a transcription of the C++ nodejs MakeDomainCall
	       ;; function (see node.cc)
	       (let ((domainv (js-get this 'domain %this)))
		  (if (isa? domainv JsObject)
		      (begin
			 (unless (js-get domainv '_disposed %this)
			    (let ((enter (js-get domainv 'enter %this)))
			       (js-call0 %this enter domainv)))
			 (let ((ret (callback)))
			    (let ((exit (js-get domainv 'exit %this)))
			       (js-call0 %this exit domainv)
			       ret)))
		      (callback)))))

	 (define need-tick-cb #f)
	 
	 (define tick-from-spinner #f)

	 (define (spinner status)
	    ;; see Spin, node.cc:184
	    (when need-tick-cb
	       (set! need-tick-cb #f)
	       (nodejs-idle-stop %worker %this tick-spinner)
	       (unless tick-from-spinner
		  (set! tick-from-spinner
		     (js-get proc '_tickFromSpinner %this)))
	       (with-access::WorkerHopThread %worker (call state)
		  (if (eq? state 'error)
		      (js-worker-push-thunk! %worker "spin"
			 (lambda ()
			    (js-call0 %this tick-from-spinner (js-undefined))))
		      (js-call0 %this tick-from-spinner (js-undefined))))))

	 (define tick-spinner
	    (nodejs-make-idle %worker %this spinner))

	 (define (need-tick-callback this)
	    ;; see NeedTickCallback, node.cc:215
	    (set! need-tick-cb #t)
	    (nodejs-idle-start %worker %this tick-spinner))
	 
	 ;; these stdio definitions are used during the bootstrap only
	 ;; they will be overriden by node_stdio.js
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
		   (let ((cmdline (cons (js-string->jsstring (car (command-line)))
				     (map js-string->jsstring (cdr jsargs)))))
		      (js-vector->jsarray (list->vector cmdline) %this))
		   (js-vector->jsarray
		      (list->vector (map js-string->jsstring (command-line)))
		      %this)))
	    #f %this)
	 (js-put! proc 'execPath
	    (js-string->jsstring (nodejs-exepath)) #f %this)
	 (js-put! proc 'execArgv
	    (js-vector->jsarray '#() %this)
	    #f %this)
	 (js-put! proc 'abort
	    (js-make-function %this
	       (lambda (this)
		  (exit 134))
	       0 "abort")
	    #f %this)

	 (js-bind! %this proc 'compilerOptions
	    :get (js-make-function %this
		    (lambda (this)
		       (js-plist->jsobject (j2s-compile-options) %this))
		    0 'compilerOptions)
	    :set (js-make-function %this
		    (lambda (this o)
		       (j2s-compile-options-set!
			  (js-jsobject->plist o %this)))
		    1 'compilerOptions)
	    :configurable #f)
	 
	 ;; first process name
	 (nodejs-process-title-init!)
	 
	 (js-bind! %this proc 'title
	    :get (js-make-function %this
		    (lambda (this)
		       (js-string->jsstring (nodejs-get-process-title)))
		    0 'title)
	    :set (js-make-function %this
		    (lambda (this str)
		       (nodejs-set-process-title! (js-tostring str %this)))
		    1 'title)
	    :configurable #f)
	 
	 (js-put! proc 'version
	    (js-stringlist->jsstring `("v" ,(nodejs-version))) #f %this)
	 
	 (js-put! proc 'versions
	    (js-alist->jsobject
	       `((http_parser: . "1.0")
		 (hop: . ,(hop-version))
		 (bigloo: . ,(bigloo-config 'release-number))
		 (uv: . ,(nodejs-uv-version))
		 (modules: . "11")
		 (openssl: . ,(cond-expand
				 (enable-ssl (ssl-version))
				 (else "-")))
		 (v8: . "-")
		 (node: . ,(nodejs-version))
		 (ares: . "-")
		 (zlib: . "-"))
	       %this)
	    #f %this)
	 
	 (js-put! proc 'exit
	    (js-make-function %this
	       (lambda (this status)
		  (let ((r (if (eq? status (js-undefined))
			       0
			       (js-tointeger status %this))))
		     (unless (js-totest (js-get proc '_exiting %this))
			(js-put! proc '_exiting #t #f %this)
			(let ((emit (js-get proc 'emit %this)))
			   (js-call2 %this emit proc "exit" r))
			(exit r))))
	       1 "exit")
	    #f %this)
	 (js-put! proc 'reallyExit
	    (js-make-function %this
	       (lambda (this status)
		  (exit (js-tointeger status %this)))
	       1 "exit")
	    #f %this)
	 (js-put! proc 'arch (js-string->jsstring (os-arch)) #f %this)
	 (js-put! proc 'platform (js-string->jsstring (os-name)) #f %this)
	 (js-put! proc 'binding
	    (js-make-function %this
	       (lambda (this module)
		  (let ((mod (js-jsstring->string module)))
		     (cond
			((string=? mod "constants")
			 (process-constants %this))
			((string=? mod "fs")
			 (process-fs %worker %this proc))
			((string=? mod "buffer")
			 (process-buffer %this slowbuffer))
			((string=? mod "tcp_wrap")
			 (process-tcp-wrap %worker %this proc slab slowbuffer))
			((string=? mod "udp_wrap")
			 (process-udp-wrap %worker %this proc slab slowbuffer))
			((string=? mod "pipe_wrap")
			 (process-pipe-wrap %worker %this proc slab))
			((string=? mod "evals")
			 (process-evals %worker %this))
			((string=? mod "cares_wrap")
			 (process-cares-wrap %worker %this proc))
			((string=? mod "timer_wrap")
			 (hopjs-process-timer %worker %this proc))
			((string=? mod "process_wrap")
			 (process-process-wrap %worker %this proc))
			((string=? mod "crypto")
			 (process-crypto %worker %this))
			((string=? mod "http_parser")
			 (process-http-parser %this))
			((string=? mod "zlib")
			 (process-zlib %worker %this proc))
			((string=? mod "os")
			 (process-os %this))
			((string=? mod "tty_wrap")
			 (process-tty-wrap %worker %this proc slab slowbuffer))
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
		 (tls_npn . #t)
		 (tls_sni . #t)
		 (tls . #t))
	       %this)
	    #f %this)
	 (let ((check #f)
	       (idle #f))
	    (js-bind! %this proc '_needImmediateCallback
	       :get (js-make-function %this
		       (lambda (this)
			  (nodejs-check? check))
		       0 '_needImmediateCallback)
	       :set (js-make-function %this
		       (lambda (this val)
			  (let ((v (js-totest val)))
			     (cond
				((and v (not (nodejs-check? check)))
				 (set! idle
				    (nodejs-make-idle
				       %worker %this
				       (lambda (_) #t)))
				 (nodejs-idle-start %worker %this idle)
				 (set! check
				    (nodejs-make-check
				       %worker %this proc)))
				((and (not v) (nodejs-check? check))
				 (nodejs-idle-stop %worker %this idle)
				 (set! idle #f)
				 (nodejs-check-stop %worker %this check)
				 (set! check #f)))))
		       1 '_needImmediateCallback)
	       :configurable #f))
	    
	 (js-put! proc 'cwd
	    (js-make-function %this
	       (lambda (this)
		  (js-string->jsstring (pwd)))
	       0 "cwd")
	    #f %this)
	 (js-put! proc 'chdir
	    (js-make-function %this
	       (lambda (this path)
		  (chdir (js-jsstring->string path)))
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
		     ((js-jsstring? val)
		      (umask (string->integer (js-jsstring->string val) 8)))
		     (else
		      (umask (js-tointeger val %this)))))
	       1 "umask")
	    #f %this)
	 
	 (js-put! proc '_usingDomains
	    (js-make-function %this
	       (lambda (this)
		  (with-access::JsProcess proc (using-domains tick-callback)
		     (unless using-domains
			(set! using-domains #t)
			(with-access::WorkerHopThread %worker (call async)
			   (set! async #t)
			   (set! call (domain-call this)))
			(let ((tdc (js-get this '_tickDomainCallback %this))
			      (ndt (js-get this '_nextDomainTick %this)))
			   (unless (isa? tdc JsFunction)
			      (error "_usingDomains"
				 "process._tickDomainCallback assigned to non-function"
				 tdc))
			   (unless (isa? ndt JsFunction)
			      (error "_usingDomains"
				 "process._nextDomainTick assigned to non-function"
				 ndt))
			   (set! tick-callback #f)
			   (js-put! this '_tickCallback tdc #f %this)
			   (js-put! this '_currentTickHandler ndt #f %this)))))
	       0 "_usingDomains")
	    #f %this)

	 ;; tick
	 (js-put! proc '_tickInfoBox
	    (js-vector->jsarray (make-vector 3 0) %this)
	    #f %this)
	 (js-put! proc '_needTickCallback
	    (js-make-function %this need-tick-callback
	       0 "needTickCallback")
	    #f %this)

	 ;; hrtime
	 (js-put! proc 'hrtime
	    (js-make-function %this
	       (lambda (this diff)
		  (let* ((t (nodejs-hrtime))
			 (d #u64:1000000000))
		     (unless (eq? diff (js-undefined))
			(unless (isa? diff JsArray)
			   (js-raise-type-error %this "Illegal diff time" diff))
			(let* ((dt0 (js-touint32 (js-get diff 0 %this) %this))
			       (dt1 (js-touint32 (js-get diff 1 %this) %this))
			       (seconds (uint32->uint64 dt0))
			       (nanos (uint32->uint64 dt1)))
			   (set! t (-u64 t (+u64 (*u64 seconds d) nanos)))))
		     (let ((t0 (uint64->flonum (/u64 t d)))
			   (t1 (uint64->flonum (remainderu64 t d))))
			(js-vector->jsarray (vector t0 t1) %this))))
	       1 "hrtime")
	    #t %this)

	 ;; uptime
	 (js-put! proc 'uptime
	    (js-make-function %this
	       (lambda (this)
		  (let* ((uptime (-u64 (nodejs-uptime %worker) prog-start-time))
			 (t (uint64->flonum uptime)))
		     (/fl t 1000.)))
	       0 "uptime")
	    #t %this)

	 ;; kill
	 (js-put! proc '_kill
	    (js-make-function %this
	       (lambda (this pid sig)
		  (nodejs-kill %worker %this proc pid sig))
	       2 "_kill")
	    #t %this)

;* 	 ;; _getActiveRequests                                         */
;* 	 (js-put! proc '_getActiveRequests                             */
;* 	    (js-make-function %this                                    */
;* 	       (lambda (this)                                          */
;* 		  (js-vector->jsarray (make-vector 0) %this))          */
;* 	       0 "_getActiveRequests")                                 */
;* 	    #t %this)                                                  */
;*                                                                     */
;* 	 ;; _getActiveHandles                                          */
;* 	 (js-put! proc '_getActiveHandles                              */
;* 	    (js-make-function %this                                    */
;* 	       (lambda (this)                                          */
;* 		  (js-vector->jsarray (make-vector 0) %this))          */
;* 	       0 "_getActiveHandles")                                  */
;* 	    #t %this)                                                  */

	 ;; memoryUsage
	 (js-put! proc 'memoryUsage
	    (js-make-function %this
	       (lambda (this)
		  (js-alist->jsobject
		     `((rss . ,(nodejs-getresidentmem))
		       (heapTotal . 0)
		       (heapUsed . 0))
		     %this))
	       0 "memoryUsage")
	    #t %this)

	 ;; getgroups
	 (js-put! proc 'getgroups
	    (js-make-function %this
	       (lambda (this)
		  (js-vector->jsarray (getgroups) %this))
	       0 "getgroups")
	    #t %this)

	 ;; ioctl (hop extension)
	 (js-put! proc 'ioctl
	    (js-make-function %this
	       (lambda (this fd request val)
		  (apply ioctl (inexact->exact (js-tointeger fd %this))
		     (if (number? request) request (js-tostring request %this))
		     (js-tonumber val %this)))
	       3 "ioctl")
	    #t %this)

	 ;; mainModule
	 (with-access::JsGlobalObject %this (js-main) 
	    (js-bind! %this proc 'mainModule
	       :get (js-make-function %this (lambda (this) js-main) 0 'main)
	       :configurable #f
	       :writable #f))
	 
	 (for-each not-implemented
	    '(_getActiveRequests
	      _getActiveHandles
	      setgroups
	      initgroups
	      _debugProcess
	      _debugPause
	      _debugEnd
	      dlopen))
	 
	 proc)))

;*---------------------------------------------------------------------*/
;*    signals ...                                                      */
;*---------------------------------------------------------------------*/
(define signals
   `((SIGHUP . ,sighup)
     (SIGINT . ,sigint)
     (SIGQUIT . ,sigquit)
     (SIGILL . ,sigill)
     (SIGABRT . ,sigabrt)
     (SIGFPE . ,sigfpe)
     (SIGKILL . ,sigkill)
     (SIGBUS . ,sigbus)
     (SIGSEGV . ,sigsegv)
     (SIGPIPE . ,sigpipe)
     (SIGALRM . ,sigalrm)
     (SIGTERM . ,sigterm)
     (SIGUSR1 . ,sigusr1)
     (SIGUSR2 . ,sigusr2)
     (SIGWINCH . ,sigwinch)))

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

	,@signals

	,@(crypto-constants))

      %this))

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
				    ((=fx (nodejs-fs-event-change)
					(bit-and events (nodejs-fs-event-change)))
				     (set! eventstr
					(js-ascii->jsstring "change")))
				    ((=fx (nodejs-fs-event-rename)
					(bit-and events (nodejs-fs-event-rename)))
				     (set! eventstr
					(js-ascii->jsstring "rename")))
				    (else
				     (error "process-fs-event-wrap"
					"bad event" eventstr)))
				 (!js-callback3 'fs-event %worker %this
				    onchange this status eventstr
				    (js-string->jsstring path))))
			   (js-jsstring->string path)))
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
;*    process-aeres-fail ...                                           */
;*---------------------------------------------------------------------*/
(define (process-ares-fail %this process errno)
   
   (define errnames
      '#("SUCCESS" "ENODATA" "EFORMERR" "ESERVFAIL" "ENOTFOUND" "ENOTIMP"
	 "EREFUSED" "EBADQUERY" "EBADNAME" "EBADFAMILY" "EBADRESP"
	 "ECONNREFUSED" "ETIMEOUT" "EOF" "EFILE" "ENOMEM" "EDESTRUCTION"
	 "EBADSTR" "EBADFLAGS" "ENONAME" "EBADHINTS" "ENOTINITIALIZED"))
   
   (define (ares-err-name errno)
      (let ((n (negfx errno)))
	 (if (or (<fx n 0) (>=fx n (vector-length errnames)))
	     "ENOTFOUND"
	     (vector-ref errnames n))))

   (js-put! process 'errno errno #f %this)
   (js-put! process '_errno (js-string->jsstring (ares-err-name errno)) #f %this)
   #f)

;*---------------------------------------------------------------------*/
;*    process-cares-wrap ...                                           */
;*---------------------------------------------------------------------*/
(define (process-cares-wrap %worker %this process)
   
   (define ENOTIMP -5)
   
   (define (getaddrinfo this domain family)
      (nodejs-getaddrinfo %worker %this process domain
	 (if (eq? family (js-undefined)) 4 family)))
   
   (define (query this domain family callback)
      (nodejs-query %worker %this process domain family callback))

   (define (dns-resolv this nstype domain callback)
      
      (define (fmt-mx e)
	 (with-access::JsGlobalObject %this (js-object)
	    (let ((obj (js-new %this js-object)))
	       (js-put! obj 'exchange
		  (js-string->jsstring (car e)) #f %this)
	       (js-put! obj 'priority
		  (cdr e) #f %this)
	       obj)))

      (define (fmt-srv e)
	 (with-access::JsGlobalObject %this (js-object)
	    (let ((obj (js-new %this js-object)))
	       (js-put! obj 'name
		  (js-string->jsstring (car e)) #f %this)
	       (js-put! obj 'priority
		  (cadr e) #f %this)
	       (js-put! obj 'weight
		  (caddr e) #f %this)
	       (js-put! obj 'port
		  (cadddr e) #f %this)
	       obj)))
      
      (define (fmt-naptr e)
	 (with-access::JsGlobalObject %this (js-object)
	    (let ((obj (js-new %this js-object)))
	       (js-put! obj 'replacement
		  (js-string->jsstring (car e)) #f %this)
	       (js-put! obj 'regexp
		  (js-string->jsstring (cadr e)) #f %this)
	       (js-put! obj 'service
		  (js-string->jsstring (caddr e)) #f %this)
	       (js-put! obj 'flags
		  (js-string->jsstring (cadddr e)) #f %this)
	       (js-put! obj 'order
		  (cadddr (cdr e)) #f %this)
	       (js-put! obj 'preference
		  (cadddr (cddr e)) #f %this)
	       obj)))
      
      (with-handler
	 (lambda (e)
	    (process-ares-fail %this process ENOTIMP)
	    #f)
	 (with-access::JsGlobalObject %this (js-object)
	    (let* ((str (js-tostring domain %this))
		   (res (resolv str nstype))
		   (fmt (case nstype
			   ((ns_t_mx) fmt-mx)
			   ((ns_t_srv) fmt-srv)
			   ((ns_t_naptr) fmt-naptr)
			   (else js-string->jsstring))))
	       (js-call2 %this callback (js-undefined) #f
		  (js-vector->jsarray
		     (vector-map! fmt res)
		     %this))
	       (js-new %this js-object)))))
   
   (define (query4 this domain callback)
      (query this domain 4 callback))
   
   (define (query6 this domain callback)
      (query this domain 6 callback))
   
   (define (gethostbyaddr this addr callback)
      (let* ((str (js-tostring addr %this))
	     (res (hostname str)))
	 (if (and (string=? res str) (=fx 0 (nodejs-isip addr)))
	     (begin
		(process-ares-fail %this process ENOTIMP)
		;;(js-call2 %this callback (js-undefined) ENOTIMP #f)
		#f)
	     (begin
		(js-call2 %this callback (js-undefined) #f
		   (js-vector->jsarray (vector (js-string->jsstring res)) %this))
		(with-access::JsGlobalObject %this (js-object)
		   (js-new %this js-object))))))

   (define (query-cname this domain callback)
      (dns-resolv this 'ns_t_cname domain callback))

   (define (query-mx this domain callback)
      (dns-resolv this 'ns_t_mx domain callback))

   (define (query-ns this domain callback)
      (dns-resolv this 'ns_t_ns domain callback))
   
   (define (query-txt this domain callback)
      (dns-resolv this 'ns_t_txt domain callback))

   (define (query-srv this domain callback)
      (dns-resolv this 'ns_t_srv domain callback))

   (define (query-naptr this domain callback)
      (dns-resolv this 'ns_t_naptr domain callback))

   (define (gethostbyname this name callback)
      (let ((res (hostinfo (js-tostring name %this))))
	 (if (pair? res)
	     (let ((addr (assq 'addresses res)))
		(js-call2 %this callback (js-undefined) #f
		   (js-string->jsstring (car (cdr addr))))
		#t)
	     (begin
		(js-put! process '_errno -1 #f %this)
		(js-call2 %this callback (js-undefined) -1 #f)
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
	   (queryCname . ,(js-make-function %this query-cname 2 "queryCname"))
	   (queryMx . ,(js-make-function %this query-mx 2 "queryMx"))
	   (queryNs . ,(js-make-function %this query-ns 2 "queryNs"))
	   (queryTxt . ,(js-make-function %this query-txt 2 "queryTxt"))
	   (querySrv . ,(js-make-function %this query-srv 2 "querySrv"))
	   (queryNaptr . ,(js-make-function %this query-naptr 2 "querySrv"))
	   (getHostByAddr . ,(js-make-function %this gethostbyaddr 2 "gethostbyaddr"))
	   (getHostByName . ,(js-make-function %this gethostbyname 2 "gethostbyname")))
	 %this)))

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
				    (js-ascii->jsstring "LE")
				    (js-ascii->jsstring "BE")))
			     0 "endianness"))
	(getHostname . ,(js-make-function %this
			   (lambda (this)
			      (js-string->jsstring (hostname)))
			   0 "getHostname"))
	(getOSType . ,(js-make-function %this
			 (lambda (this)
			    (js-string->jsstring (os-name)))
			 0 "getOSType"))
	(getOSRelease . ,(js-make-function %this
			    (lambda (this)
			       (js-string->jsstring (os-version)))
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
