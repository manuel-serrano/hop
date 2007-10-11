;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/param.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Thu Oct 11 07:51:20 2007 (serrano)                */
;*    Copyright   :  2004-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP global parameters                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_param
   
   (include "param.sch")

   (import  __hop_configure
	    __hop_mime)

   (export  (hop-uptime::date)
	    
	    (hop-rc-directory::bstring)
	    (hop-rc-directory-set! ::bstring)
	    
	    (hop-rc-file::bstring)
	    (hop-rc-file-set! ::bstring)
	    
	    (hop-verbose::int)
	    (hop-verbose-set! ::int)
	    
	    (hop-session::int)
	    (hop-session-set! ::int)
	    
	    (hop-login-cookie-id::bstring)
	    (hop-login-cookie-time::int)
	    (hop-login-cookie-crypt-key::int)
	    
	    (hop-port::int)
	    (hop-port-set! ::int)
	    
	    (hop-proxy::obj)
	    (hop-proxy-set! ::obj)
	    
	    (hop-proxy-port::obj)
	    (hop-proxy-port-set! ::obj)
	    
	    (hop-proxy-host::obj)
	    (hop-proxy-host-set! ::obj)

	    (hop-log::int)
	    (hop-log-set! ::int)
	    
	    (hop-http-request-error::obj)
	    (hop-http-request-error-set! ::obj)
	    
	    (hop-http-response-error::obj)
	    (hop-http-response-error-set! ::obj)

	    (hop-filters-close!)
	    (hop-filters::pair-nil)
	    (hop-filters-set! ::pair-nil)
	    (hop-filter-add! ::procedure)
	    (hop-filter-remove! ::procedure)
	    (hop-filter-add-always-first! ::procedure)
	    (hop-filter-add-always-last! ::procedure)
	    
	    (hop-http-response-local-hooks::pair-nil)
	    (hop-http-response-local-hooks-set! ::pair-nil)
	    (hop-http-response-local-hook-add! ::procedure)
	    (hop-http-response-local-hook-remove! ::procedure)
	    
	    (hop-http-response-remote-hooks::pair-nil)
	    (hop-http-response-remote-hooks-set! ::pair-nil)
	    (hop-http-response-remote-hook-add! ::procedure)
	    (hop-http-response-remote-hook-remove! ::procedure)
	    
	    (hop-password::pair-nil)
	    (hop-password-set! ::pair-nil)
	    
	    (hop-path::pair-nil)
	    (hop-path-set! ::pair-nil)
	    
	    (hop-server-hostname::bstring)
	    (hop-server-hostip::bstring)

	    (hop-service-base::bstring)
	    
	    (hop-service-weblet-name::bstring)
	    (hop-service-weblet-wid::symbol)

	    (hop-initial-weblet::bstring)
	    (hop-initial-weblet-set! ::bstring)

	    (hop-enable-proxing::bool)
	    (hop-enable-proxing-set! ::bool)
	    
	    (hop-server-aliases::pair-nil)
	    (hop-server-aliases-set! ::pair-nil)
	    (hop-server-aliases-add! ::bstring)

	    (hop-mime-types::pair-nil)
	    (hop-mime-types-set! ::pair-nil)
	    
	    (hop-authorize-service-hook::procedure)
	    (hop-authorize-service-hook-set! ::procedure)
	    
	    (hop-authorize-request-hook::procedure)
	    (hop-authorize-request-hook-set! ::procedure)

	    (hop-hopaccess::bstring)
	    (hop-hopaccess-set! ::bstring)
	    
	    (hop-charset::symbol)
	    (hop-charset-set! ::symbol)
	    
	    (hop-locale::symbol)
	    (hop-locale-set! ::symbol)

	    (hop-default-mime-type::bstring)
	    (hop-default-mime-type-set! ::bstring)

	    (hop-upload-directory::bstring)
	    (hop-upload-directory-set! ::bstring)
	    
	    (hop-job-file::bstring)
	    (hop-job-file-set! ::bstring)
	    
	    (hop-job-restore::bool)
	    (hop-job-restore-set! ::bool)

	    (hop-server-name::bstring)
	    (hop-server-name-set! ::bstring)
	    
	    hop-icons-directory
	    hop-demos-directory

	    (hop-connection-ttl::int) 
	    (hop-connection-ttl-set! ::int)
	    
	    (hop-connection-timeout::int) 
	    (hop-connection-timeout-set! ::int)

	    (hop-read-timeout::int) 
	    (hop-read-timeout-set! ::int)

	    (hop-enable-remote-keep-alive::bool) 
	    (hop-enable-remote-keep-alive-set! ::bool)

	    (hop-enable-keep-alive::bool) 
	    (hop-enable-keep-alive-set! ::bool)

	    (hop-keep-alive-timeout::int) 
	    (hop-keep-alive-timeout-set! ::int)

	    (hop-remote-keep-alive-timeout::int) 
	    (hop-remote-keep-alive-timeout-set! ::int)

	    (hop-max-remote-keep-alive-connection::int)
	    (hop-max-remote-keep-alive-connection-set! ::int)

	    (hop-weblets::pair-nil)
	    (hop-weblets-set! ::pair-nil)

	    (hop-make-escape::procedure)
	    (hop-make-escape-set! ::procedure)
	    
	    (hop-read-pre-hook::procedure)
	    (hop-read-pre-hook-set! ::procedure)
	    
	    (hop-read-post-hook::procedure)
	    (hop-read-post-hook-set! ::procedure)
	    
	    (hop-path-access-control::procedure)
	    (hop-path-access-control-set! ::procedure)

	    (hop-service-access-control::procedure)
	    (hop-service-access-control-set! ::procedure)

	    (hop-service-default-timeout::long)
	    (hop-service-default-timeout-set! ::long)
	    
	    (hop-service-flush-pace::long)
	    (hop-service-flush-pace-set! ::long)

	    (hop-enable-dashboard::bool)
	    (hop-enable-dashboard-set! ::bool)

	    (hop-dashboard-weblet-applets::pair-nil)
	    (hop-dashboard-weblet-applets-set! ::pair-nil)

	    (hop-event-buffer-size::int)
	    (hop-event-buffer-size-set! ::int)

	    (hop-event-timeout::elong)
	    (hop-event-timeout-set! ::elong)
	    
	    (hop-rc-loaded!)))

;*---------------------------------------------------------------------*/
;*    hop-uptime ...                                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-uptime
   (current-date))

;*---------------------------------------------------------------------*/
;*    hop-rc-directory ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hop-rc-directory
   (let ((home (or (getenv "HOME") "/"))
	 (host (hostname)))
      (let loop ((host (if (not (string? host)) (getenv "HOST") host)))
	 (if (string? host)
	     (let ((home/host (string-append home "/.config/hop." host)))
		(if (and (file-exists? home/host) (directory? home/host))
		    home/host
		    (if (string=? (suffix host) "")
			(let ((home/def (make-file-name home ".config/hop")))
			   (cond
			      ((and (file-exists? home/def)
				    (directory? home/def))
			       home/def)
			      (else
			       home)))
			(loop (prefix host)))))))))

;*---------------------------------------------------------------------*/
;*    hop-rc-file ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-rc-file
   "hoprc.hop")

;*---------------------------------------------------------------------*/
;*    hop-verbose ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-verbose
   0)

;*---------------------------------------------------------------------*/
;*    hop-session ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-session
   (elong->fixnum (date->seconds (current-date))))

;*---------------------------------------------------------------------*/
;*    hop-login-cookie-id ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-login-cookie-id
   (format "hop@~a:~a" (hostname) hop-port))

;*---------------------------------------------------------------------*/
;*    hop-login-cookie-crypt-key ...                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-login-cookie-crypt-key
   (elong->fixnum (date->seconds (current-date))))

;*---------------------------------------------------------------------*/
;*    hop-login-cookie-time ...                                        */
;*---------------------------------------------------------------------*/
(define-parameter hop-login-cookie-time
   (* 60 60 24))

;*---------------------------------------------------------------------*/
;*    hop-port ...                                                     */
;*---------------------------------------------------------------------*/
(define-parameter hop-port
   8080
   (lambda (v)
      (if (integer? v)
	  (begin
	     (hop-login-cookie-id-set! (format "hop@~a:~a" (hostname) v))
	     v)
	  (error 'hop-port "Illegal hop port" v))))

;*---------------------------------------------------------------------*/
;*    hop-proxy ...                                                    */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy
   #f
   (lambda (v)
      (cond
	 ((string? v)
	  (let ((p (pregexp-match "([^:]+):([0-9]+)" v)))
	     (if (pair? p)
		 (begin
		    (hop-proxy-host-set! (cadr p))
		    (hop-proxy-port-set! (string->integer (caddr p))))
		 (begin
		    (hop-proxy-host-set! v)
		    (hop-proxy-port-set! 80)))
	     v))
	 ((not v)
	  (hop-proxy-host-set! #f)
	  (hop-proxy-port-set! #f)
	  v)
	 (else
	  (error 'hop-proxy-set! "Illegal proxy" v)))))
	  
(define-parameter hop-proxy-host
   #f)

(define-parameter hop-proxy-port
   #f)

;*---------------------------------------------------------------------*/
;*    hop-log ...                                                      */
;*---------------------------------------------------------------------*/
(define-parameter hop-log
   0)

;*---------------------------------------------------------------------*/
;*    hop-http-request-error ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-http-request-error
   #f
   (lambda (v)
      (if (and v (or (not (procedure? v)) (not (correct-arity? v 2))))
	  (error 'hop-http-request-error "Illegal value" v)
	  v)))
      

;*---------------------------------------------------------------------*/
;*    hop-http-response-error ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter hop-http-response-error
   #f)

;*---------------------------------------------------------------------*/
;*    *filter-mutex* ...                                               */
;*---------------------------------------------------------------------*/
(define *filter-mutex* (make-mutex "hop-filter-mutex*"))

;*---------------------------------------------------------------------*/
;*    hop-filter-mutex ...                                             */
;*---------------------------------------------------------------------*/
(define (hop-filter-mutex)
   *filter-mutex*)

;*---------------------------------------------------------------------*/
;*    *hop-filters-open* ...                                           */
;*---------------------------------------------------------------------*/
(define *hop-filters-open*
   #t)

;*---------------------------------------------------------------------*/
;*    hop-filters-close! ...                                           */
;*---------------------------------------------------------------------*/
(define (hop-filters-close!)
   (with-lock (hop-filter-mutex)
      (lambda ()
	 (set! *hop-filters-open* #f))))

;*---------------------------------------------------------------------*/
;*    hop-filter ...                                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-filters
   '()
   (lambda (v)
      (if (not *hop-filters-open*)
	  (error 'hop-filters-set! "Filters closed" #f)
	  v)))

;*---------------------------------------------------------------------*/
;*    %hop-filter-add! ...                                             */
;*---------------------------------------------------------------------*/
(define (%hop-filter-add! proc kind)
   (define (add! p n)
      (if p
	  (set-cdr! p n)
	  (hop-filters-set! n)))
   (with-lock (hop-filter-mutex)
      (lambda ()
	 (if (eq? kind 'last)
	     (let loop ((fs (hop-filters))
			(p #f))
		(cond
		   ((null? fs)
		    (add! p (list (cons kind proc))))
		   ((eq? (caar fs) 'last)
		    (add! p (cons (cons kind proc) fs)))
		   (else
		    (loop (cdr fs) fs))))
	     (let loop ((fs (hop-filters))
			(p #f))
		(cond
		   ((null? fs)
		    (add! p (list (cons kind proc))))
		   ((eq? (caar fs) 'first)
		    (loop (cdr fs) fs))
		   (else
		    (add! p (cons (cons kind proc) fs)))))))))

;*---------------------------------------------------------------------*/
;*    hop-filter-remove! ...                                           */
;*---------------------------------------------------------------------*/
(define (hop-filter-remove! proc)
   (with-lock (hop-filter-mutex)
      (lambda ()
	 (let loop ((fs (hop-filters))
		    (p #f))
	    (when (pair? fs)
	       (if (eq? (cdar fs) proc)
		   (if p
		       (set-cdr! p (cdr fs))
		       (hop-filters-set! (cdr fs)))
		   (loop (cdr fs) fs)))))))

;*---------------------------------------------------------------------*/
;*    hop-filter-add! ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-filter-add! proc)
   (%hop-filter-add! proc 'float))

;*---------------------------------------------------------------------*/
;*    hop-filter-add-always-first! ...                                 */
;*---------------------------------------------------------------------*/
(define (hop-filter-add-always-first! proc)
   (%hop-filter-add! proc 'first))

;*---------------------------------------------------------------------*/
;*    hop-filter-add-always-last! ...                                  */
;*---------------------------------------------------------------------*/
(define (hop-filter-add-always-last! proc)
   (%hop-filter-add! proc 'last))

;*---------------------------------------------------------------------*/
;*    hop-http-response-local-hooks ...                                */
;*---------------------------------------------------------------------*/
(define-parameter hop-http-response-local-hooks
   '()
   (lambda (v)
      (if (not *hop-filters-open*)
	  (error 'hop-http-response-local-hook-set! "Hooks closed" #f)
	  v)))

;*---------------------------------------------------------------------*/
;*    hop-http-response-local-hook-add! ...                            */
;*---------------------------------------------------------------------*/
(define (hop-http-response-local-hook-add! proc)
   (with-lock (hop-filter-mutex)
      (lambda ()
	 (hop-http-response-local-hooks-set!
	  (cons proc (hop-http-response-local-hooks))))))

;*---------------------------------------------------------------------*/
;*    hop-http-response-local-hook-remove! ...                         */
;*---------------------------------------------------------------------*/
(define (hop-http-response-local-hook-remove! proc)
   (with-lock (hop-filter-mutex)
      (lambda ()
	 (hop-http-response-local-hooks-set!
	  (remq! proc (hop-http-response-local-hooks))))))

;*---------------------------------------------------------------------*/
;*    hop-http-response-remote-hooks ...                               */
;*---------------------------------------------------------------------*/
(define-parameter hop-http-response-remote-hooks
   '()
   (lambda (v)
      (if (not *hop-filters-open*)
	  (error 'hop-http-response-remote-hook-set! "Hooks closed" #f)
	  v)))

;*---------------------------------------------------------------------*/
;*    hop-http-response-remote-hook-add! ...                           */
;*---------------------------------------------------------------------*/
(define (hop-http-response-remote-hook-add! proc)
   (with-lock (hop-filter-mutex)
      (lambda ()
	 (hop-http-response-remote-hooks-set!
	  (cons proc (hop-http-response-remote-hooks))))))

;*---------------------------------------------------------------------*/
;*    hop-http-response-remote-hook-remove! ...                        */
;*---------------------------------------------------------------------*/
(define (hop-http-response-remote-hook-remove! proc)
   (with-lock (hop-filter-mutex)
      (lambda ()
	 (hop-http-response-remote-hooks-set!
	  (remq! proc (hop-http-response-remote-hooks))))))

;*---------------------------------------------------------------------*/
;*    hop-password ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter hop-password
   (list (base64-encode "Hop:hop")))

;*---------------------------------------------------------------------*/
;*    hop-path ...                                                     */
;*---------------------------------------------------------------------*/
(define-parameter hop-path
   (list "."
	 (hop-rc-directory)
	 (make-file-name (hop-rc-directory) "weblets")
	 (hop-share-directory)
	 (hop-weblets-directory)
	 (hop-contribs-directory)))

;*---------------------------------------------------------------------*/
;*    hop-server-hostname ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-server-hostname
   (hostname))

;*---------------------------------------------------------------------*/
;*    hop-server-hostip ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-server-hostip
   (host (hop-server-hostname)))

;*---------------------------------------------------------------------*/
;*    hop-service-base ...                                             */
;*    -------------------------------------------------------------    */
;*    The prefix of all HOP weblets.                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-service-base
   "/hop")
   
;*---------------------------------------------------------------------*/
;*    hop-initial-weblet ...                                           */
;*    -------------------------------------------------------------    */
;*    This is the name of the weblet to execute when the URL           */
;*      http://localhost:8080/(hop-service-base)                       */
;*    has been intercepted. This is not to be confused with            */
;*    HOP-SERVICE-BASE.                                                */
;*---------------------------------------------------------------------*/
(define-parameter hop-initial-weblet
   "hop")

;*---------------------------------------------------------------------*/
;*    hop-service-weblet-weblet-name ...                               */
;*---------------------------------------------------------------------*/
(define-parameter hop-service-weblet-name
   (format "svc-~a" (hop-session)))

(define-parameter hop-service-weblet-wid
   (string->symbol (hop-service-weblet-name)))

;*---------------------------------------------------------------------*/
;*    hop-enable-proxing ...                                           */
;*    -------------------------------------------------------------    */
;*    Enable (or disable) the proxy facility. If set to #f HOP no      */
;*    longer acts as proxy.                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-enable-proxing
   #t)

;*---------------------------------------------------------------------*/
;*    hop-server-aliases ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter hop-server-aliases
   '("hop" "localhost" "127.0.0.1"))

;*---------------------------------------------------------------------*/
;*    hop-server-aliases-add! ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-server-aliases-add! a)
   (hop-server-aliases-set! (cons a (hop-server-aliases))))

;*---------------------------------------------------------------------*/
;*    hop-mime-types ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter hop-mime-types
   '(;; web
     ("text/html" "html" "htm" "shtml")
     ("text/css" "css" "hss")
     ("application/x-javascript" "js")
     ("application/xml" "xml" "rss")
     ;; audio
     ("audio/audible" "aa")
     ("audio/aac" "aac")
     ("audio/ac3" "ac3")
     ("audio/mpeg" "mp3")
     ("audio/x-ogg" "ogg")
     ("audio/flac" "flac")
     ;; video
     ("video/mpeg" "avi")
     ("video/mpeg" "mpg"))
   (lambda (v)
      (mime-type-add-list! v)
      v))

;*---------------------------------------------------------------------*/
;*    hop-icons-directory ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-icons-directory
   (make-file-name (hop-share-directory) "icons"))

;*---------------------------------------------------------------------*/
;*    hop-demos-directory ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-demos-directory
   (make-file-name (hop-share-directory) "demos"))

;*---------------------------------------------------------------------*/
;*    hop-authorize-service-hook ...                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-authorize-service-hook
   (lambda (u s) #f)
   (lambda (v)
      (if (or (not (procedure? v)) (not (correct-arity? v 2)))
	  (error 'hop-authorized-service "Illegal value" v)
	  v)))
      
;*---------------------------------------------------------------------*/
;*    hop-authorize-request-hook ...                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-authorize-request-hook
   (lambda (u r) #f)
   (lambda (v)
      (if (or (not (procedure? v)) (not (correct-arity? v 2)))
	  (error 'hop-authorized-request "Illegal value" v)
	  v)))

;*---------------------------------------------------------------------*/
;*    hop-hopaccess ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter hop-hopaccess
   ".hopaccess")

;*---------------------------------------------------------------------*/
;*    hop-default-mime-type ...                                        */
;*---------------------------------------------------------------------*/
(define-parameter hop-default-mime-type
   "text/html")

;*---------------------------------------------------------------------*/
;*    hop-charset ...                                                  */
;*    -------------------------------------------------------------    */
;*    This parameter specifies the charset used by HOP for             */
;*    representing texts inside XML trees (i.e., http-response-hop)    */
;*    If one wish to change the value of this parameter, he should     */
;*    make the change before any HOP tree has been produced, i.e., at  */
;*    the very beginning of the HOP session.                           */
;*    It should be noted that other responses (e.g.,                   */
;*    http-response-string) can use a different charset.               */
;*---------------------------------------------------------------------*/
(define-parameter hop-charset
   'UTF-8
   (lambda (v)
      (case v
	 ((UTF-8 utf-8) 'UTF-8)
	 ((ISO-8859-1 iso-8859-1) 'ISO-8859-1)
	 ((ISO-8859-2 iso-8859-2) 'ISO-8859-2)
	 (else (error 'hop-charset-set! "Illegal charset" v)))))

;*---------------------------------------------------------------------*/
;*    hop-locale ...                                                   */
;*    -------------------------------------------------------------    */
;*    This parameter specifies the default charset of files hosted on  */
;*    the server disc and the default charset of string responses.     */
;*    HOP-LOCALE is mainly used to convert the files read from the     */
;*    disc into the charset specified by HOP-CHARSET.                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-locale
   'ISO-8859-1
   (lambda (v)
      (case v
	 ((UTF-8 utf-8) 'UTF-8)
	 ((ISO-8859-1 iso-8859-1) 'ISO-8859-1)
	 ((ISO-8859-2 iso-8859-2) 'ISO-8859-2)
	 (else (error 'hop-locale-set! "Illegal charset" v)))))

;*---------------------------------------------------------------------*/
;*    hop-upload-directory ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-upload-directory
   (make-file-name (hop-rc-directory) "upload"))

;*---------------------------------------------------------------------*/
;*    hop-job-file ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter hop-job-file
   "JOBS.scm")

;*---------------------------------------------------------------------*/
;*    hop-job-restore ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-job-restore
   #t)

;*---------------------------------------------------------------------*/
;*    hop-server-name ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-server-name
   (hop-name))

;*---------------------------------------------------------------------*/
;*    Connection delays and timeouts                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-connection-ttl
   ;; the number of retry when a connection cannot be established
   5)

(define-parameter hop-connection-timeout
   ;; a number of milli-seconds before a connection fails
   1500)

(define-parameter hop-read-timeout
   ;; the number of milli-seconds to wait for parsing http headers
   20000)

(define-parameter hop-enable-remote-keep-alive
   ;; does hop support keep-alive remote connection (when proxying)
   #t)
   
(define-parameter hop-enable-keep-alive
   ;; does hop support keep-alive connection
   #t
   (lambda (v)
      (unless v (hop-enable-remote-keep-alive-set! v))
      v))
   
(define-parameter hop-keep-alive-timeout
   ;; the number of milli-seconds to wait for keep-alive connections
   300)

(define-parameter hop-remote-keep-alive-timeout
   ;; the number of milli-seconds to keep alive remote connections
   2000)

(define-parameter hop-max-remote-keep-alive-connection
   ;; the max number of keep-alive remote (proxing) connections
   50)

;*---------------------------------------------------------------------*/
;*    hop-weblets ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-weblets
   '())

;*---------------------------------------------------------------------*/
;*    hop-make-escape ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-make-escape
   (lambda (x p)
      (error 'hop-make-escape "No escape hooked." x))
   (lambda (v)
      (if (or (not (procedure? v)) (not (correct-arity? v 2)))
	  (error 'hop-make-escape-set! "Illegal value" v)
	  v)))

;*---------------------------------------------------------------------*/
;*    hop-load-pre-hook ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-read-pre-hook
   (lambda (p)
      #unspecified)
   (lambda (v)
      (if (or (not (procedure? v)) (not (correct-arity? v 1)))
	  (error 'hop-read-pre-hook-set! "Illegal value" v)
	  v)))

;*---------------------------------------------------------------------*/
;*    hop-load-post-hook ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter hop-read-post-hook
   (lambda (p)
      #unspecified)
   (lambda (v)
      (if (or (not (procedure? v)) (not (correct-arity? v 1)))
	  (error 'hop-read-post-hook-set! "Illegal value" v)
	  v)))

;*---------------------------------------------------------------------*/
;*    *hop-rc-loaded* ...                                              */
;*---------------------------------------------------------------------*/
(define *hop-rc-loaded* #f)

;*---------------------------------------------------------------------*/
;*    hop-path-access-control ...                                      */
;*    -------------------------------------------------------------    */
;*    This parameter enables user customization of path access         */
;*    control.                                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-path-access-control
   (lambda (req path)
      (any? (lambda (p)
	       (substring-at? path p 0))
	    (hop-path)))
   (lambda (v)
      (cond
	 (*hop-rc-loaded*
	  (error 'define-parameter
		 "Parameter can only be set in rc file"
		 'hop-path-access-control-set!))
	 ((not (procedure? v))
	  (error 'hop-path-access-control-set!
		 (bigloo-type-error-msg "Type"
					'procedure
					(find-runtime-type v))
		 v))
	 ((not (correct-arity? v 2))
	  (error 'hop-path-access-control-set!
		 "arity two procedure expected"
		 v))
	 (else
	  v))))

;*---------------------------------------------------------------------*/
;*    hop-service-access-control ...                                   */
;*    -------------------------------------------------------------    */
;*    This parameter enables user customization of path access         */
;*    control.                                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-service-access-control
   (lambda (req svc)
      #f)
   (lambda (v)
      (cond
	 (*hop-rc-loaded*
	  (error 'define-parameter
		 "Parameter can only be set in rc file"
		 'hop-service-access-control-set!))
	 ((not (procedure? v))
	  (error 'hop-path-access-control-set!
		 (bigloo-type-error-msg "Type"
					'procedure
					(find-runtime-type v))
		 v))
	 ((not (correct-arity? v 2))
	  (error 'hop-service-access-control-set!
		 "arity two procedure expected"
		 v))
	 (else
	  v))))

;*---------------------------------------------------------------------*/
;*    hop-service-default-timeout ...                                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-service-default-timeout
   (* 2 24 60 60))

;*---------------------------------------------------------------------*/
;*    hop-service-flush-pace ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-service-flush-pace
   100)

;*---------------------------------------------------------------------*/
;*    hop-enable-dashboard ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-enable-dashboard
   #t)

;*---------------------------------------------------------------------*/
;*    hop-dashboard-weblet-applets ...                                 */
;*---------------------------------------------------------------------*/
(define-parameter hop-dashboard-weblet-applets
   '())

;*---------------------------------------------------------------------*/
;*    hop-event-buffer-size ...                                        */
;*    -------------------------------------------------------------    */
;*    The maximum number of entries an event server buffer may hold.   */
;*---------------------------------------------------------------------*/
(define-parameter hop-event-buffer-size
   100)

;*---------------------------------------------------------------------*/
;*    hop-event-timeout ...                                            */
;*    -------------------------------------------------------------    */
;*    A number of seconds a server event buffer is kept alive          */
;*---------------------------------------------------------------------*/
(define-parameter hop-event-timeout
   #e30)

;*---------------------------------------------------------------------*/
;*    hop-rc-loaded! ...                                               */
;*---------------------------------------------------------------------*/
(define (hop-rc-loaded!)
   (set! *hop-rc-loaded* #t))

