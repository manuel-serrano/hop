;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/param.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Tue Aug 19 15:29:19 2008 (serrano)                */
;*    Copyright   :  2004-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP global parameters                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_param
   
   (include "param.sch")

   (import  __hop_configure
	    __hop_mime
	    __hop_charset)

   (export  (hop-uptime::date)
	    
	    (hop-rc-directory::bstring)
	    (hop-rc-directory-set! ::bstring)
	    
	    (hop-rc-file::bstring)
	    (hop-rc-file-set! ::bstring)
	    
	    (hop-load-preferences::bool)
	    (hop-load-preferences-set! ::bool)
	    (hop-store-preferences::bool)
	    (hop-store-preferences-set! ::bool)
	    
	    (hop-verbose::int)
	    (hop-verbose-set! ::int)
	    
	    (hop-session::int)
	    (hop-session-set! ::int)
	    
	    (hop-login-cookie-id::bstring)
	    (hop-login-cookie-time::int)
	    (hop-login-cookie-crypt-key::int)
	    
	    (hop-port::int)
	    (hop-port-set! ::int)
	    
	    (hop-use-proxy::obj)
	    (hop-use-proxy-set! ::obj)
	    
	    (hop-use-proxy-port::obj)
	    (hop-use-proxy-port-set! ::obj)
	    
	    (hop-use-proxy-host::obj)
	    (hop-use-proxy-host-set! ::obj)

	    (hop-log::int)
	    (hop-log-set! ::int)

	    (hop-max-file-size-cache::elong)
	    (hop-max-file-size-cache-set! ::elong)

	    (hop-max-file-entry-cache::int)
	    (hop-max-file-entry-cache-set! ::int)
	    
	    (hop-restore-disk-cache::bool)
	    (hop-restore-disk-cache-set! ::bool)
	    
	    (hop-http-request-error::obj)
	    (hop-http-request-error-set! ::obj)
	    
	    (hop-http-response-error::obj)
	    (hop-http-response-error-set! ::obj)

	    (hop-filters-close!)
	    (hop-filters-open?::bool)
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

	    (hop-scm-compile-suffix::bstring)
	    (hop-hss-compile-suffix::bstring)
	    
	    (hop-client-script-suffixes::pair-nil)
	    
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
	    
	    (hop-default-mime-type::bstring)
	    (hop-default-mime-type-set! ::bstring)

	    (hop-json-mime-type::bstring)
	    (hop-bigloo-mime-type::bstring)

	    (hop-authorize-service-hook::procedure)
	    (hop-authorize-service-hook-set! ::procedure)
	    
	    (hop-authorize-request-hook::procedure)
	    (hop-authorize-request-hook-set! ::procedure)

	    (hop-hopaccess::bstring)
	    (hop-hopaccess-set! ::bstring)
	    
	    (hop-charset::symbol)
	    (hop-charset-set! ::symbol)
	    
	    (hop-charset->locale::procedure)
	    (hop-locale->charset::procedure)

	    (hop-locale::symbol)
	    (hop-locale-set! ::symbol)

	    (hop-upload-directory::bstring)
	    (hop-upload-directory-set! ::bstring)
	    
	    (hop-job-file::bstring)
	    (hop-job-file-set! ::bstring)
	    
	    (hop-job-restore::bool)
	    (hop-job-restore-set! ::bool)

	    (hop-server-name::bstring)
	    (hop-server-name-set! ::bstring)
	    
	    (hop-icons-directory)

	    (hop-connection-ttl::int) 
	    (hop-connection-ttl-set! ::int)
	    
	    (hop-connection-timeout::int) 
	    (hop-connection-timeout-set! ::int)

	    (hop-read-timeout::int) 
	    (hop-read-timeout-set! ::int)

	    (hop-enable-keep-alive::bool) 
	    (hop-enable-keep-alive-set! ::bool)

	    (hop-enable-remote-keep-alive::bool) 
	    (hop-enable-remote-keep-alive-set! ::bool)

	    (hop-keep-alive-timeout::int) 
	    (hop-keep-alive-timeout-set! ::int)

	    (hop-remote-keep-alive-timeout::int) 
	    (hop-remote-keep-alive-timeout-set! ::int)

	    (hop-keep-alive-threshold::int)
	    (hop-keep-alive-threshold-set! ::int)

	    (hop-max-remote-keep-alive-connection::int)
	    (hop-max-remote-keep-alive-connection-set! ::int)

	    (hop-remanent-timeout::int) 
	    (hop-remanent-timeout-set! ::int)

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

	    (hop-force-content-length::bool)
	    (hop-force-content-length-set! ::bool)
	    
	    (hop-service-access-control::procedure)
	    (hop-service-access-control-set! ::procedure)

	    (hop-service-default-timeout::long)
	    (hop-service-default-timeout-set! ::long)
	    
	    (hop-service-flush-pace::long)
	    (hop-service-flush-pace-set! ::long)

	    (hop-allow-service-override::bool)
	    (hop-allow-service-override-set! ::bool)

	    (hop-enable-dashboard::bool)
	    (hop-enable-dashboard-set! ::bool)

	    (hop-dashboard-weblet-applets::pair-nil)
	    (hop-dashboard-weblet-applets-set! ::pair-nil)

	    (hop-dashboard-weblet-disabled-applets::pair-nil)
	    (hop-dashboard-weblet-disabled-applets-set! ::pair-nil)

	    (hop-event-buffer-size::int)
	    (hop-event-buffer-size-set! ::int)

	    (hop-event-max-clients::int)
	    (hop-event-max-clients-set! ::int)

	    (hop-event-keep-alive::elong)
	    (hop-event-keep-alive-set! ::elong)

	    (hop-accept-kill::bool)
	    (hop-accept-kill-set! ::bool)
	    
	    (hop-enable-proxy-sniffer::bool)
	    (hop-enable-proxy-sniffer-set! ::bool)
	    
	    (hop-proxy-sniffer::procedure)
	    (hop-proxy-sniffer-set! ::procedure)
	    (hop-proxy-sniffer-add! ::procedure)

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
			(loop (prefix host))))))))
   (lambda (v)
      (hop-path-set! (cons (make-file-name v "cache") (hop-path)))
      v))

;*---------------------------------------------------------------------*/
;*    hop-rc-file ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-rc-file
   "hoprc.hop")

;*---------------------------------------------------------------------*/
;*    hop-load-preferences ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-load-preferences
   #t)

(define-parameter hop-store-preferences
   #t)

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
(define-parameter hop-use-proxy
   #f
   (lambda (v)
      (cond
	 ((string? v)
	  (let ((p (pregexp-match "([^:]+):([0-9]+)" v)))
	     (if (pair? p)
		 (begin
		    (hop-use-proxy-host-set! (cadr p))
		    (hop-use-proxy-port-set! (string->integer (caddr p))))
		 (begin
		    (hop-use-proxy-host-set! v)
		    (hop-use-proxy-port-set! 80)))
	     v))
	 ((not v)
	  (hop-use-proxy-host-set! #f)
	  (hop-use-proxy-port-set! #f)
	  v)
	 (else
	  (error 'hop-proxy-set! "Illegal proxy" v)))))
	  
(define-parameter hop-use-proxy-host
   #f)

(define-parameter hop-use-proxy-port
   #f)

;*---------------------------------------------------------------------*/
;*    hop-log ...                                                      */
;*---------------------------------------------------------------------*/
(define-parameter hop-log
   0)

;*---------------------------------------------------------------------*/
;*    hop-restore-disk-cache ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-restore-disk-cache
   #f)

;*---------------------------------------------------------------------*/
;*    hop-max-file-size-cache ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter hop-max-file-size-cache
   #e16384)

;*---------------------------------------------------------------------*/
;*    hop-max-file-entry-cache ...                                     */
;*---------------------------------------------------------------------*/
(define-parameter hop-max-file-entry-cache
   16)

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
;*    hop-filters-open? ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-filters-open?)
   (with-lock (hop-filter-mutex)
      (lambda ()
	 *hop-filters-open*)))

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
	 (make-file-name (hop-rc-directory) "cache")
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
;*    hop-scm-compile-suffix ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-scm-compile-suffix
   "?scm")

;*---------------------------------------------------------------------*/
;*    hop-hss-compile-suffix ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-hss-compile-suffix
   "?hss")

;*---------------------------------------------------------------------*/
;*    hop-client-script-suffixes ...                                   */
;*    -------------------------------------------------------------    */
;*    The suffixes of the client compilation urls                      */
;*---------------------------------------------------------------------*/
(define-parameter hop-client-script-suffixes
   '("hop" "scm"))
   
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
     ("application/xhtml+xml" "xhtml")
     ("application/x-javascript" "js")
     ("application/xml" "xml" "rss")
     ;; audio
     ("audio/audible" "aa")
     ("audio/aac" "aac")
     ("audio/ac3" "ac3")
     ("audio/mpeg" "mp3")
     ("audio/x-ogg" "ogg")
     ("audio/flac" "flac")
     ;; images
     ("image/png" "png")
     ("image/jpeg" "jpeg" "jpg")
     ("image/gif" "gif")
     ;; video
     ("video/mpeg" "avi")
     ("video/mpeg" "mpg")
     ("video/x-flv" "flv"))
   (lambda (v)
      (mime-type-add-list! v)
      v))

;*---------------------------------------------------------------------*/
;*    hop-default-mime-type ...                                        */
;*---------------------------------------------------------------------*/
(define-parameter hop-default-mime-type
   "text/plain")

;*---------------------------------------------------------------------*/
;*    hop-json-mime-type ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter hop-json-mime-type
   "application/json")

;*---------------------------------------------------------------------*/
;*    hop-bigloo-mime-type ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-bigloo-mime-type
   "application/bigloo")

;*---------------------------------------------------------------------*/
;*    hop-icons-directory ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-icons-directory
   (make-file-name (hop-share-directory) "icons"))

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
;*    hop-charset->locale ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-charset->locale
   (lambda (x) x))

(define-parameter hop-locale->charset
   (lambda (x) x))

;*---------------------------------------------------------------------*/
;*    hop-charset ...                                                  */
;*    -------------------------------------------------------------    */
;*    This parameter specifies the charset used by HOP for             */
;*    representing texts inside XML trees (i.e., http-response-hop)    */
;*    If one wishes to change the value of this parameter, he should   */
;*    make the change before any HOP tree has been produced, i.e., at  */
;*    the very beginning of the HOP session.                           */
;*    It should be noted that other responses (e.g.,                   */
;*    http-response-string) can use a different charset.               */
;*---------------------------------------------------------------------*/
(define-parameter hop-charset
   'UTF-8
   (lambda (v)
      (when (and (symbol? v) (symbol? *hop-locale*))
	 (hop-locale->charset-set! (charset-converter v (hop-locale)))
	 (hop-charset->locale-set! (charset-converter (hop-locale) v)))
      (case v
	 ((UTF-8 utf-8) 'UTF-8)
	 ((UCS-2 ucs-2) 'UCS-2)
	 ((ISO-LATIN-1 iso-latin-1) 'ISO-LATIN-1)
	 ((ISO-8859-1 iso-8859-1) 'ISO-8859-1)
	 ((ISO-8859-2 iso-8859-2) 'ISO-8859-2)
	 ((ISO-8859-15 iso-8859-15) 'ISO-8859-15)
	 ((WINDOW-1252 window-1252) 'WINDOW-1252)
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
      (when (and (symbol? v) (symbol? (hop-charset)))
	 (hop-locale->charset-set! (charset-converter v (hop-charset)))
	 (hop-charset->locale-set! (charset-converter (hop-charset) v)))
      (case v
	 ((UTF-8 utf-8) 'UTF-8)
	 ((UCS-2 ucs-2) 'UCS-2)
	 ((ISO-LATIN-1 iso-latin-1) 'ISO-LATIN-1)
	 ((ISO-8859-1 iso-8859-1) 'ISO-8859-1)
	 ((ISO-8859-2 iso-8859-2) 'ISO-8859-2)
	 ((ISO-8859-15 iso-8859-15) 'ISO-8859-15)
	 ((WINDOW-1252 window-1252) 'WINDOW-1252)
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
   20000)

(define-parameter hop-read-timeout
   ;; the number of milli-seconds to wait for parsing http headers
   60000)

(define-parameter hop-enable-keep-alive
   ;; does hop support keep-alive connection
   (cond-expand
      ;; in a multi-threaded config, by default, it does
      (enable-threads #t)
      ;; in a single env, it does not
      (else #f))
   (lambda (v)
      (unless v (hop-enable-remote-keep-alive-set! v))
      v))

(define-parameter hop-enable-remote-keep-alive
   ;; does hop support keep-alive remote connection (when proxying)
   (cond-expand
      (enable-threads #t)
      (else #f)))
   
(define-parameter hop-keep-alive-timeout
   ;; the number of milli-seconds to wait for keep-alive connections
   ;; Don't change this value, it is an optimal value that it in phase
   ;; with browsers value. There is a tradeoff here, higher value raises
   ;; the number of re-use connections but it also increases the cost
   ;; of abandonned keep-alive connections.
   30)

(define-parameter hop-remote-keep-alive-timeout
   ;; the number of milli-seconds to keep alive remote connections
   30)

(define-parameter hop-keep-alive-threshold
   ;; the max number of connections above which keep-alive are closed
   256)
   
(define-parameter hop-max-remote-keep-alive-connection
   ;; the max number of keep-alive remote (proxing) connections
   8
   (lambda (v)
      (if (<fx v 4)
	  (error 'hop-max-remote-keep-alive-connection-set!
		 "value should be greater or equal to 4"
		 v)
	  v)))

(define-parameter hop-remanent-timeout
   (* 1000 30))

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
      #f)
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
;*    hop-force-content-length ...                                     */
;*---------------------------------------------------------------------*/
(define-parameter hop-force-content-length
   #f)

;*---------------------------------------------------------------------*/
;*    hop-service-access-control ...                                   */
;*    -------------------------------------------------------------    */
;*    This parameter enables user customization of path access         */
;*    control.                                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-service-access-control
   (lambda (req svc) #f)
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
;*    hop-allow-service-override ...                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-allow-service-override
   #f)

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
;*    hop-dashboard-weblet-disabled-applets ...                        */
;*    -------------------------------------------------------------    */
;*    A list of dashboard applets that are disabled.                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-dashboard-weblet-disabled-applets
   '())

;*---------------------------------------------------------------------*/
;*    hop-event-buffer-size ...                                        */
;*    -------------------------------------------------------------    */
;*    The maximum number of entries an event server buffer may hold.   */
;*---------------------------------------------------------------------*/
(define-parameter hop-event-buffer-size
   5)

;*---------------------------------------------------------------------*/
;*    hop-event-max-clients ...                                        */
;*    -------------------------------------------------------------    */
;*    The maximum number of simultaneous clients waiting for events.   */
;*---------------------------------------------------------------------*/
(define-parameter hop-event-max-clients 200)

;*---------------------------------------------------------------------*/
;*    hop-event-keep-alive ...                                         */
;*    -------------------------------------------------------------    */
;*    The number of seconds to keep an ajax connection alive.          */
;*---------------------------------------------------------------------*/
(define-parameter hop-event-keep-alive #e121)

;*---------------------------------------------------------------------*/
;*    hop-accept-kill ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-accept-kill
   #f)

;*---------------------------------------------------------------------*/
;*    hop-enable-proxy-sniffer ...                                     */
;*---------------------------------------------------------------------*/
(define-parameter hop-enable-proxy-sniffer
   #f
   (lambda (v)
      (if (not *hop-filters-open*)
	  (error 'hop-enable-proxy-sniffer-set!
		 "Sniffer can only be enabled/disabled to HOP startup time"
		 #f)
	  v)))

;*---------------------------------------------------------------------*/
;*    hop-proxy-sniffer ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy-sniffer
   (lambda (req) #f)
   (lambda (v)
      (if (not (and (procedure? v) (correct-arity? v 1)))
	  (error 'hop-proxy-sniffer
		 "arity one procedure expected"
		 v)
	  v)))

(define (hop-proxy-sniffer-add! proc)
   (let ((old (hop-proxy-sniffer)))
      (hop-proxy-sniffer-set!
       (lambda (req)
	  (let ((old (old req))
		(new (proc req)))
	     (if (output-port? old)
		 (let ((p (open-output-procedure
			   (lambda (s)
			      (display s old)
			      (display s new)))))
		    (output-port-close-hook-set!
		     p
		     (lambda (p)
			(close-output-port old)
			(close-output-port new))))
		 new))))))

;*---------------------------------------------------------------------*/
;*    hop-rc-loaded! ...                                               */
;*---------------------------------------------------------------------*/
(define (hop-rc-loaded!)
   (set! *hop-rc-loaded* #t))

