;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/src/hop_param.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Wed Jul 18 05:36:11 2012 (serrano)                */
;*    Copyright   :  2004-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP global parameters                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_param

   (library hop)

   (export  (hop-scheduler::obj)
	    (hop-scheduler-set! ::obj)

	    (hop-max-threads::int)
	    (hop-max-threads-set! ::int)

	    (hop-autoload-directories::pair-nil)
	    (hop-autoload-directories-set! ::pair-nil)
	    (hop-autoload-directory-add! ::bstring)

	    (hop-preload-libraries::pair-nil)
	    (hop-preload-libraries-set! ::pair-nil)

	    (hop-proxy-authentication::bool)
	    (hop-proxy-authentication-set! ::bool)

	    (hop-proxy-allow-remote-client::bool)
	    (hop-proxy-allow-remote-client-set! ::bool)

	    (hop-proxy-remote-authentication::bool)
	    (hop-proxy-remote-authentication-set! ::bool)

	    (hop-proxy-ip-mask::bstring)
	    (hop-proxy-ip-mask-set! ::bstring)

	    (hop-proxy-ip-mask-word::elong)

	    (hop-ip-blacklist::obj)
	    (hop-ip-blacklist-set! ::obj)
	    (hop-ip-blacklist-table::obj)

	    (hop-log-file::obj)
	    (hop-log-file-set! ::obj)

	    (hop-scheduling::symbol)
	    (hop-scheduling-set! ::symbol)

	    (hop-somaxconn::int)
	    (hop-somaxconn-set! ::int)

	    (hop-sndbuf::int)
	    (hop-sndbuf-set! ::int)

	    (hop-enable-https::bool)
	    (hop-enable-https-set! ::bool)

	    (hop-fast-server-event-port::int)
	    (hop-fast-server-event-port-set! ::int)

	    (hop-enable-fast-server-event::bool)
	    (hop-enable-fast-server-event-set! ::bool)

	    (hop-enable-repl::bool)
	    (hop-enable-repl-set! ::bool)

	    (hop-enable-discovery::bool)
	    (hop-enable-discovery-set! ::bool)

	    (hop-https-protocol::symbol)
	    (hop-https-protocol-set! ::symbol)

	    (hop-enable-webdav::bool)
	    (hop-enable-webdav-set! ::bool)

	    (hop-gzipped-directories::pair-nil)
	    (hop-gzipped-directories-set! ::pair-nil)

	    (hop-process-key::bstring)
	    (hop-process-key-set! ::bstring)

	    (hop-report-execution-time::bool)
	    (hop-report-execution-time-set! ::bool)

	    (hop-script-file::obj)
	    (hop-script-file-set! ::obj)

	    (hop-get-cache-size::int)
	    (hop-get-cache-size-set! ::int)

	    (hop-user::obj)
	    (hop-user-set! ::obj)

	    (hop-preload-services::pair-nil)
	    (hop-preload-services-set! ::pair-nil)

	    (hop-enable-zeroconf::bool)
	    (hop-enable-zeroconf-set! ::bool))

   (eval    (export-exports)))

;*---------------------------------------------------------------------*/
;*    Thread management                                                */
;*---------------------------------------------------------------------*/
(define-parameter hop-scheduler
   #unspecified)

(define-parameter hop-max-threads
   12
   (lambda (v)
      (cond-expand
	 (enable-threads v)
	 (else 1))))

;*---------------------------------------------------------------------*/
;*    Autoload                                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-autoload-directories
   (list (hop-weblets-directory)))

(define (hop-autoload-directory-add! d)
   (hop-autoload-directories-set! (cons d (hop-autoload-directories))))

;*---------------------------------------------------------------------*/
;*    hop-preload-libraries ...                                        */
;*---------------------------------------------------------------------*/
(define-parameter hop-preload-libraries
   (cond-expand
      (enable-threads
       '(pthread hop hopwidget web hopscheme scheme2js multimedia))
      (else
       '(hop hopwidget web hopscheme scheme2js multimedia))))

;*---------------------------------------------------------------------*/
;*    hop-proxy-authentication ...                                     */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy-authentication
   #f)

;*---------------------------------------------------------------------*/
;*    hop-proxy-allow-remote-client ...                                */
;*    -------------------------------------------------------------    */
;*    Accept or not to relay request from distant client?              */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy-allow-remote-client
   #f)

;*---------------------------------------------------------------------*/
;*    hop-proxy-remote-authentication ...                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy-remote-authentication
   #t)

;*---------------------------------------------------------------------*/
;*    hop-proxy-ip-mask ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy-ip-mask
   "255.255.255.255"
   (lambda (v)
      (if (string? v)
	  (let ((l (pregexp-match "([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})" v)))
	     (if (not l)
		 (error "hop-proxy-ip-mask-set!"
			"Illegal IPv4 mask"
			v)
		 (begin
		    (hop-proxy-ip-mask-word-set! (ipv4->elong v))
		    v)))
	  (ipv4->elong "255.255.255.255"))))

;*---------------------------------------------------------------------*/
;*    hop-proxy-ip-mask-word ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy-ip-mask-word
   (ipv4->elong "255.255.255.255"))

;*---------------------------------------------------------------------*/
;*    hop-ip-blacklist ...                                             */
;*    -------------------------------------------------------------    */
;*    A list of IP addresses not allowed to get connected              */
;*---------------------------------------------------------------------*/
(define-parameter hop-ip-blacklist
   '()
   (lambda (v)
      (cond
	 ((hashtable? v)
	  v)
	 ((list? v)
	  (let ((t (create-hashtable :size 32)))
	     (for-each (lambda (e)
			  (when (string? e)
			     (hashtable-put! t e #t)))
		       v)
	     (hop-ip-blacklist-table-set! t)
	     v))
	 (else
	  (error "hop-ip-blacklist" "Illegal blacklist" v)))))

;*---------------------------------------------------------------------*/
;*    hop-ip-blacklist-table ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-ip-blacklist-table
   (create-hashtable :size 1)
   (lambda (v)
      (if (hashtable? v)
	  v
	  (error "hop-ip-blacklist-table" "Illegal hashtable" v))))

;*---------------------------------------------------------------------*/
;*    hop-log-file ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter hop-log-file
   #f)

;*---------------------------------------------------------------------*/
;*    hop-scheduling ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter hop-scheduling
   'pool)

;*---------------------------------------------------------------------*/
;*    hop-somaxconn ...                                                */
;*    -------------------------------------------------------------    */
;*    On Linux 2.6.x see /proc/sys/net/core/somaxconn for the          */
;*    actual maximal limit of SOMAXCONN.                               */
;*---------------------------------------------------------------------*/
(define-parameter hop-somaxconn
   16)

;*---------------------------------------------------------------------*/
;*    hop-sndbuf ...                                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-sndbuf
   (*fx 1024 12))

;*---------------------------------------------------------------------*/
;*    hop-enable-https ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hop-enable-https
   #f
   (lambda (v)
      (cond-expand
	 (enable-ssl
	  v)
	 (else
	  (when v (warning "SSL not supporting, disabling https support."))
	  v))))

(define-parameter hop-https-protocol
   'tlsv1)

;*---------------------------------------------------------------------*/
;*    hop-fast-server-event-port ...                                   */
;*---------------------------------------------------------------------*/
(define-parameter hop-fast-server-event-port
   (hop-port)
   (lambda (v)
      (if (<fx v 1024)
	  (error "hop-fast-server-event-port-set!"
		 "Server event ports must be greater than 1023"
		 v)
	  v)))

(define-parameter hop-enable-fast-server-event
   #t)

;*---------------------------------------------------------------------*/
;*    hop-enable-repl ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-enable-repl
   #f)

;*---------------------------------------------------------------------*/
;*    hop-enable-discovery ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-enable-discovery
   #t)

;*---------------------------------------------------------------------*/
;*    hop-enable-webdav ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-enable-webdav
   #f)

;*---------------------------------------------------------------------*/
;*    hop-gzipped-directories ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter hop-gzipped-directories
   (list (hop-share-directory))
   (lambda (v)
      (if (every? string? v)
	  v
	  (error "hop-gzipped-directories" "Illegal gzipped directory list" v))))

;*---------------------------------------------------------------------*/
;*    hop-process-key ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-process-key
   (md5sum-string (elong->string (current-seconds))))

;*---------------------------------------------------------------------*/
;*    hop-report-execution-time ...                                    */
;*---------------------------------------------------------------------*/
(define-parameter hop-report-execution-time
   #f)

;*---------------------------------------------------------------------*/
;*    hop-script-file ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-script-file
   #f)

;*---------------------------------------------------------------------*/
;*    hop-get-cache-size ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter hop-get-cache-size
   64)

;*---------------------------------------------------------------------*/
;*    hop-user ...                                                     */
;*---------------------------------------------------------------------*/
(define-parameter hop-user
   "hop")

;*---------------------------------------------------------------------*/
;*    hop-preload-services ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-preload-services
   '())

;*---------------------------------------------------------------------*/
;*    hop-enable-zeroconf ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hop-enable-zeroconf
   (hop-zeroconf-default))
