;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/param.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Fri Dec 18 08:14:04 2015 (serrano)                */
;*    Copyright   :  2004-15 Manuel Serrano                            */
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

	    (hop-var-directory::bstring)
	    (hop-var-directory-set! ::bstring)

	    (hop-cache-directory::bstring)
	    (hop-cache-directory-set! ::bstring)

	    (hop-cache-enable::bool)
	    (hop-cache-enable-set! ::bool)

	    (hop-sofile-enable::bool)
	    (hop-sofile-enable-set! ::bool)

	    (hop-sofile-directory::bstring)
	    (hop-sofile-directory-set! ::bstring)

	    (hop-load-preferences::bool)
	    (hop-load-preferences-set! ::bool)
	    (hop-store-preferences::bool)
	    (hop-store-preferences-set! ::bool)

	    %%*hop-verbose*
	    (inline hop-verbose::int)
	    (hop-verbose-set! ::int)
	    
	    (hop-security::int)
	    (hop-security-set! ::int)

	    (hop-http-authentication::symbol)
	    (hop-http-authentication-set! ::symbol)

	    (hop-session::int)
	    (hop-session-set! ::int)

	    (hop-realm::bstring)
	    (hop-realm-set! ::bstring)
	    
	    (hop-https-protocol::symbol)
	    (hop-https-protocol-set! ::symbol)
	    
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

            (hop-verbose-file::obj)
            (hop-verbose-file-set! ::obj)

	    (hop-capture-port::obj)
	    (hop-capture-port-set! ::obj)

	    (hop-loaders::pair-nil)
	    (hop-loaders-set! ::pair-nil)
	    (hop-loader-add! ::bstring ::procedure)
	    
	    (hop-max-file-size-cache::elong)
	    (hop-max-file-size-cache-set! ::elong)

	    (hop-max-file-entry-cache::int)
	    (hop-max-file-entry-cache-set! ::int)
	    
	    (hop-svg-img-cache-size::int)
	    (hop-svg-img-cache-size-set! ::int)
	    
	    (hop-svg-img-max-file-size-cache::elong)
	    (hop-svg-img-max-file-size-cache-set! ::elong)
	    
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
	    
	    (hop-http-response-server-hooks::pair-nil)
	    (hop-http-response-server-hooks-set! ::pair-nil)
	    (hop-http-response-server-hook-add! ::procedure)
	    (hop-http-response-server-hook-remove! ::procedure)
	    
	    (hop-http-response-proxy-hooks::pair-nil)
	    (hop-http-response-proxy-hooks-set! ::pair-nil)
	    (hop-http-response-proxy-hook-add! ::procedure)
	    (hop-http-response-proxy-hook-remove! ::procedure)
	    
	    (hop-password::pair-nil)
	    (hop-password-set! ::pair-nil)
	    
	    (hop-path::pair-nil)
	    (hop-path-set! ::pair-nil)
	    
	    (hop-server-hostname::bstring)
	    (hop-server-hostname-set! ::bstring)
	    (hop-server-hostip::bstring)

	    (hop-scm-compile-suffix::bstring)
	    (hop-hss-compile-suffix::bstring)
	    
	    (hop-client-script-suffixes::pair-nil)
	    (hop-module-suffixes::pair-nil)
	    
	    (hop-service-weblet-name::bstring)
	    (hop-service-weblet-id::symbol)

	    (hop-initial-weblet::bstring)
	    (hop-initial-weblet-set! ::bstring)

	    (hop-force-reload-service::bool)
	    (hop-force-reload-service-set! ::bool)

	    (hop-allow-redefine-service::bool)
	    (hop-allow-redefine-service-set! ::bool)

	    (hop-hss-theme::bstring)
	    (hop-hss-theme-set! ::bstring)

	    (hop-hss-foreign-eval::procedure)
	    (hop-hss-foreign-eval-set! ::procedure)

	    (hop-hss-clear-cache::obj)
	    (hop-hss-clear-cache-set! ::obj)

	    (hop-enable-proxying::bool)
	    (hop-enable-proxying-set! ::bool)
	    (hop-enable-proxing-set! ::bool)
	    
	    (hop-enable-websocket-proxying::bool)
	    (hop-enable-websocket-proxying-set! ::bool)
	    
	    (hop-max-websocket-proxy-tunnel::int)
	    (hop-max-websocket-proxy-tunnel-set! ::int)
	    
	    (hop-server-aliases::pair-nil)
	    (hop-server-aliases-set! ::pair-nil)
	    (hop-server-aliases-add! ::bstring)

	    (hop-mime-types::pair-nil)
	    (hop-mime-types-set! ::pair-nil)

	    (hop-mime-type::bstring)
	    (hop-mime-type-set! ::bstring)

	    (hop-javascript-version::bstring)
	    (hop-javascript-version-set! ::bstring)

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

	    (hop-charset->locale::procedure)
	    (hop-charset->locale!::procedure)
	    (hop-locale->charset::procedure)
	    (hop-locale->charset!::procedure)

	    (hop-upload-directory::bstring)
	    (hop-upload-directory-set! ::bstring)
	    
	    (hop-job-file::bstring)
	    (hop-job-file-set! ::bstring)
	    
	    (hop-job-restore::bool)
	    (hop-job-restore-set! ::bool)

	    (hop-server-name::bstring)
	    (hop-server-name-set! ::bstring)

	    (hop-server-addresses::pair-nil)
	    (hop-server-addresses-set! ::pair-nil)
	    
	    (hop-icons-directory::bstring)
	    (hop-icons-directory-set! ::bstring)

	    (hop-connection-ttl::int) 
	    (hop-connection-ttl-set! ::int)
	    
	    (hop-connection-timeout::int) 
	    (hop-connection-timeout-set! ::int)

	    (hop-read-timeout::int) 
	    (hop-read-timeout-set! ::int)

	    (hop-enable-keep-alive::bool) 
	    (hop-enable-keep-alive-set! ::bool)

	    (hop-enable-proxy-keep-alive::bool) 
	    (hop-enable-proxy-keep-alive-set! ::bool)

	    (hop-keep-alive-timeout::int) 
	    (hop-keep-alive-timeout-set! ::int)

	    (hop-proxy-keep-alive-timeout::int) 
	    (hop-proxy-keep-alive-timeout-set! ::int)

	    (hop-keep-alive-threshold::int)
	    (hop-keep-alive-threshold-set! ::int)

	    (hop-max-proxy-keep-alive-connection::int)
	    (hop-max-proxy-keep-alive-connection-set! ::int)

	    (hop-remanent-timeout::int) 
	    (hop-remanent-timeout-set! ::int)

	    (hop-weblets::pair-nil)
	    (hop-weblets-set! ::pair-nil)

	    (hop-clientc::obj)
	    (hop-clientc-set! ::obj)

	    (hop-clientc-clear-cache::obj)
	    (hop-clientc-clear-cache-set! ::obj)

	    (hop-clientc-debug-unbound::int)
	    (hop-clientc-debug-unbound-set! ::int)

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

	    (hop-hz-resolver::procedure)
	    (hop-hz-resolver-set! ::procedure)
	    
	    (hop-hz-server::bstring)
	    (hop-hz-server-set! ::bstring)
	    
	    (hop-hz-package-suffix::bstring)
	    (hop-hz-package-suffix-set! ::bstring)

	    (hop-hz-repositories::pair-nil)
	    (hop-hz-repositories-set! ::pair-nil)
	    (hop-hz-repositories-add! ::bstring)
	    
	    (hop-runtime-extra::pair-nil)
	    (hop-runtime-extra-set! ::pair-nil)
	    (hop-runtime-extra-add! ::bstring)

	    (hop-exepath)
	    (hop-exepath-set! ::obj)

	    (hop-user-agent::bstring)
	    (hop-user-agent-set! ::bstring)

	    (hop-preferred-language::bstring)
	    (hop-preferred-language-set! ::bstring)

	    (hop-rc-loaded)
	    (hop-rc-loaded?)
	    (hop-rc-loaded! ::obj)))

;*---------------------------------------------------------------------*/
;*    hop-uptime ...                                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-uptime
   (current-date))

;*---------------------------------------------------------------------*/
;*    hop-rc-directory ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hop-rc-directory
   (hop-configure-rc-directory))

;*---------------------------------------------------------------------*/
;*    hop-path ...                                                     */
;*---------------------------------------------------------------------*/
(define-parameter hop-path
   (list "."
      (hop-rc-directory)
      (hop-share-directory)
      (hop-weblets-directory)
      (hop-contribs-directory)))

;*---------------------------------------------------------------------*/
;*    hop-var-directory ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-var-directory
   (or (hop-configure-var-directory) (hop-rc-directory)))

;*---------------------------------------------------------------------*/
;*    hop-cache-directory ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-cache-directory
   (or (hop-configure-cache-directory)
       (let ((h (getenv "XDG_CACHE_HOME")))
	  (when (string? h)
	     (make-file-name h "hop")))
       (make-file-name (hop-rc-directory) "cache"))
   (lambda (v)
      (hop-path-set! (cons v (hop-path)))
      v))

;*---------------------------------------------------------------------*/
;*    hop-cache-enable ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hop-cache-enable
   #t)

;*---------------------------------------------------------------------*/
;*    hop-sofile-enable                                                */
;*---------------------------------------------------------------------*/
(define-parameter hop-sofile-enable
   (bigloo-config 'have-dlopen))

;*---------------------------------------------------------------------*/
;*    hop-sofile-directory ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-sofile-directory
   (make-file-path (hop-rc-directory) "libs"))

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
;*    -------------------------------------------------------------    */
;*    HOP-VERBOSE is used everywhere so it's performance matters.      */
;*    For that reason it is implemented as an inline function and      */
;*    the global register *HOP-VERBOSE* is exported.                   */
;*---------------------------------------------------------------------*/
(define %%*hop-verbose* 0)
(define-inline (hop-verbose) %%*hop-verbose*)
(define (hop-verbose-set! v) (set! %%*hop-verbose* v))

;*---------------------------------------------------------------------*/
;*    hop-security ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter hop-security 1)

;*---------------------------------------------------------------------*/
;*    hop-http-authentication ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter hop-http-authentication 'basic)

;*---------------------------------------------------------------------*/
;*    hop-session ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-session
   (bit-rsh (absfx (elong->fixnum (date->seconds (current-date)))) 2))

;*---------------------------------------------------------------------*/
;*    hop-realm ...                                                    */
;*---------------------------------------------------------------------*/
(define-parameter hop-realm
   "hop")

;*---------------------------------------------------------------------*/
;*    hop-https-protocol ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter hop-https-protocol
   (cond-expand
      (bigloo4.2a 'tlsv1)
      (else 'tlsv1_2)))

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
	  (error "hop-proxy-set!" "Illegal proxy" v)))))
	  
(define-parameter hop-use-proxy-host
   #f)

(define-parameter hop-use-proxy-port
   #f)

;*---------------------------------------------------------------------*/
;*    hop-log ...                                                      */
;*---------------------------------------------------------------------*/
(define-parameter hop-log
   1)

;*---------------------------------------------------------------------*/
;     hop-verbose-file ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hop-verbose-file
   #f
   (lambda (path)
      (if path
          (open-output-file path)
          #f)))

;*---------------------------------------------------------------------*/
;*    hop-capture-port ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hop-capture-port
   #f)

;*---------------------------------------------------------------------*/
;*    hop-loaders ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-loaders
   '())

(define (hop-loader-add! suffix loader)
   (hop-loaders-set! (cons (cons suffix loader) (hop-loaders))))

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
;*    hop-svg-img-cache-size ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-svg-img-cache-size
   32)

;*---------------------------------------------------------------------*/
;*    hop-svg-img-max-file-size-cache ...                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-svg-img-max-file-size-cache
   #e8192)

;*---------------------------------------------------------------------*/
;*    hop-http-request-error ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-http-request-error
   #f
   (lambda (v)
      (if (and v (or (not (procedure? v)) (not (correct-arity? v 2))))
	  (error "hop-http-request-error" "Illegal value" v)
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
   (synchronize (hop-filter-mutex)
      (set! *hop-filters-open* #f)))

;*---------------------------------------------------------------------*/
;*    hop-filters-open? ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-filters-open?)
   (synchronize (hop-filter-mutex)
      *hop-filters-open*))

;*---------------------------------------------------------------------*/
;*    hop-filter ...                                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-filters
   '()
   (lambda (v)
      (if (not *hop-filters-open*)
	  (error "hop-filters-set!" "Filters closed" #f)
	  v)))

;*---------------------------------------------------------------------*/
;*    %hop-filter-add! ...                                             */
;*---------------------------------------------------------------------*/
(define (%hop-filter-add! proc kind)
   
   (define (add! p n)
      (if p
	  (set-cdr! p n)
	  (hop-filters-set! n)))
   
   (synchronize (hop-filter-mutex)
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
		 (add! p (cons (cons kind proc) fs))))))))

;*---------------------------------------------------------------------*/
;*    hop-filter-remove! ...                                           */
;*---------------------------------------------------------------------*/
(define (hop-filter-remove! proc)
   (synchronize (hop-filter-mutex)
      (let loop ((fs (hop-filters))
		 (p #f))
	 (when (pair? fs)
	    (if (eq? (cdar fs) proc)
		(if p
		    (set-cdr! p (cdr fs))
		    (hop-filters-set! (cdr fs)))
		(loop (cdr fs) fs))))))

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
;*    hop-http-response-server-hooks ...                               */
;*---------------------------------------------------------------------*/
(define-parameter hop-http-response-server-hooks
   '()
   (lambda (v)
      (if (not *hop-filters-open*)
	  (error "hop-http-response-server-hook-set!" "Hooks closed" #f)
	  v)))

;*---------------------------------------------------------------------*/
;*    hop-http-response-server-hook-add! ...                           */
;*---------------------------------------------------------------------*/
(define (hop-http-response-server-hook-add! proc)
   (synchronize (hop-filter-mutex)
      (hop-http-response-server-hooks-set!
	 (cons proc (hop-http-response-server-hooks)))))

;*---------------------------------------------------------------------*/
;*    hop-http-response-server-hook-remove! ...                        */
;*---------------------------------------------------------------------*/
(define (hop-http-response-server-hook-remove! proc)
   (synchronize (hop-filter-mutex)
      (hop-http-response-server-hooks-set!
	 (remq! proc (hop-http-response-server-hooks)))))

;*---------------------------------------------------------------------*/
;*    hop-http-response-proxy-hooks ...                                */
;*---------------------------------------------------------------------*/
(define-parameter hop-http-response-proxy-hooks
   '()
   (lambda (v)
      (if (not *hop-filters-open*)
	  (error "hop-http-response-proxy-hook-set!" "Hooks closed" #f)
	  v)))

;*---------------------------------------------------------------------*/
;*    hop-http-response-proxy-hook-add! ...                            */
;*---------------------------------------------------------------------*/
(define (hop-http-response-proxy-hook-add! proc)
   (synchronize (hop-filter-mutex)
      (hop-http-response-proxy-hooks-set!
	 (cons proc (hop-http-response-proxy-hooks)))))

;*---------------------------------------------------------------------*/
;*    hop-http-response-proxy-hook-remove! ...                         */
;*---------------------------------------------------------------------*/
(define (hop-http-response-proxy-hook-remove! proc)
   (synchronize (hop-filter-mutex)
      (hop-http-response-proxy-hooks-set!
	 (remq! proc (hop-http-response-proxy-hooks)))))

;*---------------------------------------------------------------------*/
;*    hop-password ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter hop-password
   (list (base64-encode "Hop:hop")))

;*---------------------------------------------------------------------*/
;*    hop-server-hostip ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-server-hostip
   (with-handler
      (lambda (e)
	 "127.0.0.1")
      (let ((addr (assq 'addresses (hostinfo (hostname)))))
	 (if (pair? addr)
	     (cadr addr)
	     "127.0.0.1"))))

;*---------------------------------------------------------------------*/
;*    hop-server-hostname ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-server-hostname
   (let ((h (hostname)))
      (if (string=? h "localhost")
	  ;; try to find something better
	  (if (string=? (hop-server-hostip) "127.0.0.1")
	      (begin
		 (flush-output-port (current-error-port))
		 (let ((intf (find (lambda (e)
				      (flush-output-port (current-error-port))
				      (and (not (string=? (car e) "lo"))
					   (string=? (caddr e) "ipv4")))
				(get-interfaces))))
		    (if (pair? intf)
			;; we got an ipv4 configured interface
			(let ((ip (cadr intf)))
			   (hop-server-hostip-set! ip)
			   (hostname ip))
			"localhost")))
	      (let ((h (hostname (hop-server-hostip))))
		 (if (string? h) h (hop-server-hostip))))
	  h)))

;*---------------------------------------------------------------------*/
;*    hop-scm-compile-suffix ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-scm-compile-suffix
   "scm")

;*---------------------------------------------------------------------*/
;*    hop-hss-compile-suffix ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-hss-compile-suffix
   "hss")

;*---------------------------------------------------------------------*/
;*    hop-client-script-suffixes ...                                   */
;*    -------------------------------------------------------------    */
;*    The suffixes of the client compilation urls                      */
;*---------------------------------------------------------------------*/
(define-parameter hop-client-script-suffixes
   '("scm" "hop"))

;*---------------------------------------------------------------------*/
;*    hop-module-suffixes ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-module-suffixes
   '("hop" "scm" "sch" "hss"))

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
;*    hop-force-reload-service ...                                     */
;*    -------------------------------------------------------------    */
;*    When set to #t each time a service is invoked, it's source       */
;*    file is checked. When modified, it is automatically reloaded     */
;*    before the service is executed. See --devel mode.                */
;*---------------------------------------------------------------------*/
(define-parameter hop-force-reload-service
   #f)

;*---------------------------------------------------------------------*/
;*    hop-allow-redefine-service ...                                   */
;*    -------------------------------------------------------------    */
;*    This facilitie should only be used in "devel" mode.              */
;*---------------------------------------------------------------------*/
(define-parameter hop-allow-redefine-service
   #f)

;*---------------------------------------------------------------------*/
;*    hop-hss-theme ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter hop-hss-theme
   "hss")

;*---------------------------------------------------------------------*/
;*    hop-hss-foreign-eval ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-hss-foreign-eval
   (lambda (ip)
      (raise
	 (instantiate::&io-read-error
	    (fname (input-port-name ip))
	    (location (input-port-position ip))
	    (proc "read")
	    (msg "No foreign evaluator given")
	    (obj "{")))))

;*---------------------------------------------------------------------*/
;*    hop-hss-clear-cache ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-hss-clear-cache
   #t)

;*---------------------------------------------------------------------*/
;*    hop-service-weblet-weblet-name ...                               */
;*---------------------------------------------------------------------*/
(define-parameter hop-service-weblet-name
   (format "svc-~a" (hop-session)))

(define-parameter hop-service-weblet-id
   (string->symbol (hop-service-weblet-name)))

;*---------------------------------------------------------------------*/
;*    hop-enable-proxying ...                                          */
;*    -------------------------------------------------------------    */
;*    Enable (or disable) the proxy facility. If set to #f HOP no      */
;*    longer acts as proxy.                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-enable-proxying
   #t)

(define-parameter hop-enable-websocket-proxying
   #t)

(define-parameter hop-max-websocket-proxy-tunnel
   10)

(define (hop-enable-proxing-set! v)
   (fprint (current-error-port) "\"hop-enable-proxing-set!\" is deprecated\n"
      "It has been replaced with \"hop-enable-proxying-set!\". "
      (format "Edit the file \"~a/wizard.hop\" or \"~a/hoprc.hop\" and fix it."
	 (hop-rc-directory)))
   (hop-enable-proxying-set! v))

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
     ("text/javascript" "scm")
     ("text/html" "html" "htm" "shtml")
     ("text/css" "css" "hss")
     ("application/xhtml+xml" "xhtml")
     ("application/x-javascript" "js")
     ("application/xml" "xml" "rss")
     ("application/x-hop" "hop" "scm")
     ;; audio
     ("audio/audible" "aa")
     ("audio/aac" "aac")
     ("audio/ac3" "ac3")
     ("audio/mpeg" "mp3")
     ("audio/ogg" "ogg")
     ("audio/flac" "flac")
     ;; images
     ("image/png" "png")
     ("image/jpeg" "jpeg" "jpg")
     ("image/gif" "gif")
     ;; video
     ("video/mpeg" "avi")
     ("video/ogg" "ogv")
     ("video/mpeg" "mpg")
     ("video/x-flv" "flv")
     ;; fonts
     ("application/x-font-ttf" "ttf")
     ("application/octet-stream" "otf")
     ("application/vnd.ms-fontobject" ".eot"))
   (lambda (v)
      (mime-type-add-list! v)
      v))

;*---------------------------------------------------------------------*/
;*    hop-mime-type ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter hop-mime-type
   "application/x-javascript")

;*---------------------------------------------------------------------*/
;*    hop-javascript-version ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-javascript-version
   "1.5"
   (lambda (v)
      (if (>= (string-natural-compare3 v "1.7") 0)
	  (hop-mime-type-set! (format "application/x-javascript;version=~a" v))
	  (hop-mime-type-set! "application/x-javascript"))
      v))

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
	  (error "hop-authorized-service" "Illegal value" v)
	  v)))
      
;*---------------------------------------------------------------------*/
;*    hop-authorize-request-hook ...                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-authorize-request-hook
   (lambda (u r) #f)
   (lambda (v)
      (if (or (not (procedure? v)) (not (correct-arity? v 2)))
	  (error "hop-authorized-request" "Illegal value" v)
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

(define-parameter hop-charset->locale!
   (lambda (x) x))

(define-parameter hop-locale->charset
   (lambda (x) x))

(define-parameter hop-locale->charset!
   (lambda (x) x))

;*---------------------------------------------------------------------*/
;*    hop-locale ...                                                   */
;*    -------------------------------------------------------------    */
;*    This parameter specifies the default charset of files hosted on  */
;*    the server disc and the default charset of string responses.     */
;*    HOP-LOCALE is mainly used to convert the files read from the     */
;*    disc into the charset specified by HOP-CHARSET.                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-locale
   (case (string->symbol (suffix (os-charset)))
      ((UTF-8) 'UTF-8)
      (else 'ISO-8859-1))
   (lambda (v)
      (when (and (symbol? v) (symbol? *hop-charset*))
	 (hop-locale->charset-set! (charset-converter v (hop-charset)))
	 (hop-locale->charset!-set! (charset-converter! v (hop-charset)))
	 (hop-charset->locale-set! (charset-converter (hop-charset) v))
	 (hop-charset->locale!-set! (charset-converter! (hop-charset) v)))
      (case v
	 ((UTF-8 utf-8) 'UTF-8)
	 ((UCS-2 ucs-2) 'UCS-2)
	 ((ISO-LATIN-1 iso-latin-1) 'ISO-LATIN-1)
	 ((ISO-8859-1 iso-8859-1) 'ISO-8859-1)
	 ((ISO-8859-2 iso-8859-2) 'ISO-8859-2)
	 ((ISO-8859-15 iso-8859-15) 'ISO-8859-15)
	 ((WINDOW-1252 window-1252) 'WINDOW-1252)
	 (else (error "hop-locale-set!" "Illegal charset" v)))))

;*---------------------------------------------------------------------*/
;*    hop-charset ...                                                  */
;*    -------------------------------------------------------------    */
;*    It is highly recommended to use 'UTF-8 as value.                 */
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
      (when (symbol? v)
	 (when (symbol? *hop-locale*)
	    (hop-locale->charset-set! (charset-converter (hop-locale) v))
	    (hop-locale->charset!-set! (charset-converter! (hop-locale) v))
	    (hop-charset->locale-set! (charset-converter v (hop-locale)))
	    (hop-charset->locale!-set! (charset-converter! v (hop-locale)))))
      (case v
	 ((UTF-8 utf-8) 'UTF-8)
	 ((UCS-2 ucs-2) 'UCS-2)
	 ((ISO-LATIN-1 iso-latin-1) 'ISO-LATIN-1)
	 ((ISO-8859-1 iso-8859-1) 'ISO-8859-1)
	 ((ISO-8859-2 iso-8859-2) 'ISO-8859-2)
	 ((ISO-8859-15 iso-8859-15) 'ISO-8859-15)
	 ((WINDOW-1252 window-1252) 'WINDOW-1252)
	 (else (error "hop-charset-set!" "Illegal charset" v)))))

;*---------------------------------------------------------------------*/
;*    hop-upload-directory ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-upload-directory
   "upload")

;*---------------------------------------------------------------------*/
;*    hop-job-file ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter hop-job-file
   "jobs.hop")

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
;*    hop-server-addresses ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-server-addresses
   (with-handler
      (lambda (e)
	 '())
      (let ((hi (hostinfo (hostname))))
	 (let ((c (assq 'addresses hi)))
	    (if (pair? c) (cdr c) '())))))

;*---------------------------------------------------------------------*/
;*    Connection delays and timeouts                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-connection-ttl
   ;; the number of retry when a connection cannot be established
   5)

(define-parameter hop-connection-timeout
   ;; a number of milli-seconds before a connection fails
   (*fx 20 1000))

(define-parameter hop-read-timeout
   ;; the number of milli-seconds to wait for parsing http headers
   (*fx 10 1000))

(define-parameter hop-enable-keep-alive
   ;; does hop support keep-alive connection
   (cond-expand
      ;; in a multi-threaded config, by default, it does
      (enable-threads #t)
      ;; in a single env, it does not
      (else #f))
   (lambda (v)
      (unless v (hop-enable-proxy-keep-alive-set! v))
      v))

(define-parameter hop-enable-proxy-keep-alive
   ;; does hop support keep-alive remote connection (when proxying)
   (cond-expand
      (enable-threads #t)
      (else #f)))
   
(define-parameter hop-keep-alive-timeout
   ;; the number of milli-seconds to wait for keep-alive connections
   ;; Don't change this value, it is an optimal value that is in phase
   ;; with browsers value. There is a tradeoff here, higher value raises
   ;; the number of re-use connections but it also increases the cost
   ;; of abandonned keep-alive connections.
   (*fx 5 1000))

(define-parameter hop-proxy-keep-alive-timeout
   ;; the number of milli-seconds to keep alive remote connections
   (*fx 3 1000))

(define-parameter hop-keep-alive-threshold
   ;; the max number of connections above which keep-alive are closed
   256)
   
(define-parameter hop-max-proxy-keep-alive-connection
   ;; the max number of keep-alive remote (proxying) connections
   8
   (lambda (v)
      (if (<fx v 4)
	  (error "hop-max-proxy-keep-alive-connection-set!"
		 "value should be greater or equal to 4"
		 v)
	  v)))

(define-parameter hop-remanent-timeout
   (*fx 1000 30))

;*---------------------------------------------------------------------*/
;*    hop-weblets ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-weblets
   '())

;*---------------------------------------------------------------------*/
;*    hop-clientc ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-clientc
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hop-clientc-clear-cache ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter hop-clientc-clear-cache
   #t)

;*---------------------------------------------------------------------*/
;*    hop-clientc-debug-unbound ...                                    */
;*---------------------------------------------------------------------*/
(define-parameter hop-clientc-debug-unbound
   0)

;*---------------------------------------------------------------------*/
;*    hop-load-pre-hook ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-read-pre-hook
   (lambda (p)
      #unspecified)
   (lambda (v)
      (if (or (not (procedure? v)) (not (correct-arity? v 1)))
	  (error "hop-read-pre-hook-set!" "Illegal value" v)
	  v)))

;*---------------------------------------------------------------------*/
;*    hop-load-post-hook ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter hop-read-post-hook
   (lambda (p)
      #unspecified)
   (lambda (v)
      (if (or (not (procedure? v)) (not (correct-arity? v 1)))
	  (error "hop-read-post-hook-set!" "Illegal value" v)
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
	  (error "define-parameter"
		 "Parameter can only be set in rc file"
		 'hop-path-access-control-set!))
	 ((not (procedure? v))
	  (error "hop-path-access-control-set!"
		 (bigloo-type-error-msg "Type" "procedure" (typeof v))
		 v))
	 ((not (correct-arity? v 2))
	  (error "hop-path-access-control-set!"
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
   (lambda (req svc) #f)
   (lambda (v)
      (cond
	 (*hop-rc-loaded*
	  (error "define-parameter"
		 "Parameter can only be set in rc file"
		 'hop-service-access-control-set!))
	 ((not (procedure? v))
	  (error "hop-path-access-control-set!"
		 (bigloo-type-error-msg "Type" "procedure" (typeof v))
		 v))
	 ((not (correct-arity? v 2))
	  (error "hop-service-access-control-set!"
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
	  (error "hop-enable-proxy-sniffer-set!"
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
	  (error "hop-proxy-sniffer"
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
		      (output-port-close-hook-set! p
			 (lambda (p)
			    (close-output-port old)
			    (close-output-port new))))
		   new))))))

;*---------------------------------------------------------------------*/
;*    hop-hz-resolver ...                                              */
;*    -------------------------------------------------------------    */
;*    A resolver is a function that accepts a HZ package name and      */
;*    either returns the name of a local file containing that package  */
;*    or #f. In that case, the regular HZ resolution takes place.      */
;*---------------------------------------------------------------------*/
(define-parameter hop-hz-resolver
   (lambda (hz) #f))

;*---------------------------------------------------------------------*/
;*    hop-hz-server ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter hop-hz-server
   (hop-url))

;*---------------------------------------------------------------------*/
;*    hop-hz-package-suffix ...                                        */
;*---------------------------------------------------------------------*/
(define-parameter hop-hz-package-suffix
   "hz")

;*---------------------------------------------------------------------*/
;*    hop-hz-repositories ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-hz-repositories
   '())

(define (hop-hz-repositories-add! v)
   (hop-hz-repositories-set! (cons v (hop-hz-repositories))))

;*---------------------------------------------------------------------*/
;*    hop-runtime-extra ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-runtime-extra
   '()
   (lambda (v)
      (if *hop-rc-loaded*
	  (error "hop-runtime-extra-set!" "runtime extra closed" #f)
	  v)))

(define (hop-runtime-extra-add! v)
   (unless (member v (hop-runtime-extra))
      (hop-runtime-extra-set! (cons v (hop-runtime-extra)))))

;*---------------------------------------------------------------------*/
;*    hop-rc-loaded! ...                                               */
;*---------------------------------------------------------------------*/
(define (hop-rc-loaded! path)
   (set! *hop-rc-loaded* path))

;*---------------------------------------------------------------------*/
;*    hop-rc-loaded ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-rc-loaded)
   *hop-rc-loaded*)

;*---------------------------------------------------------------------*/
;*    hop-rc-loaded? ...                                               */
;*---------------------------------------------------------------------*/
(define (hop-rc-loaded?)
   (string? *hop-rc-loaded*))

;*---------------------------------------------------------------------*/
;*    hop-login-cookie-id ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-login-cookie-id
   (format "hop@~a:~a" (hop-server-hostname) 8080))

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
	     (hop-login-cookie-id-set!
		(format "hop@~a:~a" (hop-server-hostname) v))
	     v)
	  (error "hop-port" "Illegal hop port" v))))

;*---------------------------------------------------------------------*/
;*    hop-exepath ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hop-exepath
   #f)

;*---------------------------------------------------------------------*/
;*    hop-user-agent ...                                               */
;*    -------------------------------------------------------------    */
;*    User-Agent string when emitting http requests                    */
;*---------------------------------------------------------------------*/
(define-parameter hop-user-agent
   "Mozilla/5.0 (X11; Linux i686; rv:31.0) Gecko/20100101 Firefox/31.0 Iceweasel/31.5.3")

;*---------------------------------------------------------------------*/
;*    hop-preferred-language ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-preferred-language
   "hop")
