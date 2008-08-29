;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/user.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 19 14:13:15 2005                          */
;*    Last change :  Thu Aug 28 20:49:37 2008 (serrano)                */
;*    Copyright   :  2005-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    User support                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_user
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_http-lib
	    __hop_misc
	    __hop_service
	    __hop_cache)
   
   (export  (users-close!)
	    (add-user! ::bstring . opt)
	    (user-exists? ::bstring)
	    (encrypt-authentication ::symbol ::bstring ::bstring)
	    (decrypt-authentication ::bstring ::bstring)
	    (anonymous-user::user)
	    (find-authenticated-user ::bstring ::bstring)
	    (find-user ::bstring ::bstring)
	    (find-user/encrypt ::bstring ::bstring ::procedure)
	    (user-authorized-request?::bool ::user ::http-request)
	    (user-authorized-path?::bool ::user ::bstring)
	    (authorized-path?::bool ::http-request ::bstring)
	    (user-authorized-service?::bool ::user ::symbol)
	    (authorized-service?::bool ::http-request ::symbol)
	    (user-access-denied ::http-request #!optional message)
	    (user-service-denied ::http-request ::user ::symbol)
	    (proxy-denied ::http-request ::user ::bstring)))
	    
;*---------------------------------------------------------------------*/
;*    *user-mutex* ...                                                 */
;*---------------------------------------------------------------------*/
(define *user-mutex* (make-mutex "hop-user"))

;*---------------------------------------------------------------------*/
;*    *users* ...                                                      */
;*---------------------------------------------------------------------*/
(define *users* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    make-user-key ...                                                */
;*---------------------------------------------------------------------*/
(define (make-user-key name passwd)
   (base64-encode
    (format "~a:~a:~a" name passwd (hop-login-cookie-crypt-key))))

;*---------------------------------------------------------------------*/
;*    user-key ...                                                     */
;*---------------------------------------------------------------------*/
(define (user-key u)
   (make-user-key (user-name u) (user-password u)))

;*---------------------------------------------------------------------*/
;*    *users-open* ...                                                 */
;*---------------------------------------------------------------------*/
(define *users-open* #t)

;*---------------------------------------------------------------------*/
;*    users-close! ...                                                 */
;*---------------------------------------------------------------------*/
(define (users-close!)
   (set! *users-open* #f))
   
;*---------------------------------------------------------------------*/
;*    add-user! ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-user! name . args)
   (if *users-open*
       (%add-user! name args)
       (error 'add-user! "User registration closed" #f)))

;*---------------------------------------------------------------------*/
;*    %add-user! ...                                                   */
;*---------------------------------------------------------------------*/
(define (%add-user! name args)
   (let ((g '())
	 (p #f)
	 (s '())
	 (cname (make-file-path
		 (hop-rc-directory) "users" (string-append name ".prefs")))
	 (c '())
	 (d (list (hop-share-directory)
		  (make-file-name (hop-rc-directory) "cache"))))
      (let loop ((a args))
	 (cond
	    ((null? a)
	     (with-lock *user-mutex*
		(lambda ()
		   (when (string? cname)
		      (unless (directory? (dirname cname))
			 (make-directories (dirname cname))))
		   (let* ((prefs (if (and (hop-load-preferences)
					  (string? cname)
					  (file-exists? cname))
				     (with-input-from-file cname read)
				     '()))
			  (u (instantiate::user
				(name name)
				(groups g)
				(password (or p (symbol->string (gensym))))
				(services s)
				(preferences (append c prefs))
				(preferences-filename cname)
				(directories d)))
			  (k (make-user-key name p)))
		      (if (hashtable-get *users* name)
			  (begin
			     (hashtable-remove! *users* name)
			     (hashtable-put! *users* name u))
			  (hashtable-put! *users* name u))
		      u))))
	    ((or (not (keyword? (car a))) (null? (cdr a)))
	     (error 'add-user! "Illegal arguments" args))
	    (else
	     (case (car a)
		((:groups)
		 (if (not (and (list? (cadr a)) (every? symbol? (cadr a))))
		     (error 'add-user! "Illegal group" (cadr a))
		     (set! g (cadr a))))
		((:password)
		 (if (not (string? (cadr a)))
		     (error 'add-user! "Illegal password" (cadr a))
		     (set! p (cadr a))))
		((:services)
		 (if (not (or (eq? (cadr a) '*)
			      (and (list? (cadr a)) (every? symbol? (cadr a)))))
		     (error 'add-user! "Illegal services" (cadr a))
		     (unless (eq? s '*)
			(if (eq? (cadr a) '*)
			    (set! s '*)
			    (set! s (cons (hop-service-weblet-wid) (cadr a)))))))
		((:directories)
		 (unless (eq? d '*)
		    (cond
		       ((eq? (cadr a) '*)
			(set! d '*))
		       ((and (list? (cadr a)) (every? string? (cadr a)))
			(set! d (append (map file-name-unix-canonicalize (cadr a)) d)))
		       (else
			(error 'add-user! "Illegal directories" (cadr a))))))
		((:preferences)
		 (set! c (append c (cadr a))))
		((:preferences-filename)
		 (set! cname (cadr a)))
		(else
		 (error 'add-user! "Illegal argument" args)))
	     (loop (cddr a)))))))

;*---------------------------------------------------------------------*/
;*    user-exists? ...                                                 */
;*---------------------------------------------------------------------*/
(define (user-exists? name)
   (with-lock *user-mutex*
      (lambda ()
	 (user? (hashtable-get *users* name)))))

;*---------------------------------------------------------------------*/
;*    encrypt-authentication ...                                       */
;*---------------------------------------------------------------------*/
(define (encrypt-authentication algo auth path)
   
   (define (encrypt-ho0-authentication i auth path)
      (let ((n (substring auth 0 i))
	    (p (substring auth (+fx i 1) (string-length auth))))
	 (string-append "HO0" n ":" (md5sum (format "~a ~a" n p)))))

   (define (encrypt-ho1-authentication i auth path)
      (let* ((n (substring auth 0 i))
	     (p (substring auth (+fx i 1) (string-length auth)))
	     (k (md5sum (string-append n " " p))))
	 (string-append "HO1" n ":" (md5sum (string-append k path)))))
   
   (let* ((i (string-index auth #\:)))
      (if (not (fixnum? i))
	  (error 'encrypt-authentication "Illegal authentication" auth)
	  (case algo
	     ((none)
	      auth)
	     ((ho0)
	      (encrypt-ho0-authentication i auth path))
	     ((ho1)
	      (encrypt-ho1-authentication i auth path))
	     (else
	      (error 'encrypt-authentication "Illegal algorithm" algo))))))

;*---------------------------------------------------------------------*/
;*    decrypt-authentication ...                                       */
;*    -------------------------------------------------------------    */
;*    This function never raises an error. It returns #f if something  */
;*    goes wrong.                                                      */
;*---------------------------------------------------------------------*/
(define (decrypt-authentication auth path)
   
   (define (find-none-authentication auth n p path)
      (let ((u (find-user n (md5sum (format "~a ~a" n p)))))
	 (if (user? u)
	     (add-cached-user! auth u)
	     (hop-verb 2 "Can't authentify user: " n "\n"))
	 u))
   
   (define (find-ho0-authentication auth n md5p path)
      (let ((u (find-user n md5p)))
	 (if (user? u)
	     (add-cached-user! auth u)
	     (hop-verb 2 "HO0: Can't authentify user: " n "\n"))
	 u))
   
   (define (find-ho1-authentication auth n md5p path)
      (let ((u (hashtable-get *users* n)))
	 (when (user? u)
	    (if (string=? (md5sum (string-append (user-password u) path)) md5p)
		u
		(begin
		   (hop-verb 2 "HO1: Can't authentify user: " n "\n")
		   #f)))))
   
   (let ((i (string-index auth #\:)))
      (when (and (fixnum? i) (>fx i 0))
	 (let ((s (substring auth 0 i))
	       (p (substring auth (+fx i 1) (string-length auth))))
	    (cond
	       ((substring-at? s "HO0" 0)
		(let ((n (substring s 3 (string-length s))))
		   (find-ho0-authentication auth n p path)))
	       ((substring-at? s "HO1" 0)
		(let ((n (substring s 3 (string-length s))))
		   (find-ho1-authentication auth n p path)))
	       (else
		(find-none-authentication auth s p path)))))))
   
;*---------------------------------------------------------------------*/
;*    *authenticated-users* ...                                        */
;*    -------------------------------------------------------------    */
;*    We use a two level cache. A pair of global variables for         */
;*    level 1 and a hashtable for level 2.                             */
;*---------------------------------------------------------------------*/
(define *authenticated-users* (make-hashtable 5))
(define *last-authentication* #f)
(define *last-user* #f)
(define *anonymous-user* #unspecified)

;*---------------------------------------------------------------------*/
;*    anonymous-user ...                                               */
;*---------------------------------------------------------------------*/
(define (anonymous-user)
   (if (user? *anonymous-user*)
       *anonymous-user*
       (let ((a (hashtable-get *users* "anonymous")))
	  (if (user? a)
	      (set! *anonymous-user* a)
	      (error 'anonymous-user "No anonymous user declared" a))
	  *anonymous-user*)))

;*---------------------------------------------------------------------*/
;*    find-cached-user ...                                             */
;*---------------------------------------------------------------------*/
(define (find-cached-user auth)
   (with-lock *user-mutex*
      (lambda ()
	 (if (and (string? *last-authentication*)
		  (string=? *last-authentication* auth))
	     *last-user*
	     (hashtable-get *authenticated-users* auth)))))

;*---------------------------------------------------------------------*/
;*    add-cached-user! ...                                             */
;*---------------------------------------------------------------------*/
(define (add-cached-user! auth u)
   (with-lock *user-mutex*
      (lambda ()
	 (when (string? *last-authentication*)
	    (hashtable-put! *authenticated-users* *last-authentication* *last-user*))
	 (set! *last-authentication* auth)
	 (set! *last-user* u))))

;*---------------------------------------------------------------------*/
;*    find-user ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-user user-name encoded-passwd)
   (let ((u (hashtable-get *users* user-name)))
      (and (user? u)
	   (string=? encoded-passwd (user-password u))
	   u)))

;*---------------------------------------------------------------------*/
;*    find-user/encrypt ...                                            */
;*---------------------------------------------------------------------*/
(define (find-user/encrypt user-name encoded-passwd encrypt)
   (let ((u (hashtable-get *users* user-name)))
      (and (user? u)
	   (string=? encoded-passwd (encrypt (user-password u)))
	   u)))

;*---------------------------------------------------------------------*/
;*    find-authenticated-user ...                                      */
;*    -------------------------------------------------------------    */
;*    The authorization is of the form "Basic <base64string>".         */
;*---------------------------------------------------------------------*/
(define (find-authenticated-user auth path)
   (and (string? auth)
	(or (find-cached-user auth)
	    (decrypt-authentication (http-decode-authentication auth) path))))

;*---------------------------------------------------------------------*/
;*    hopaccess-cache ...                                              */
;*---------------------------------------------------------------------*/
(define hopaccess-cache
   (instantiate::cache-memory
      (max-entries 256)))

;*---------------------------------------------------------------------*/
;*    find-hopaccess ...                                               */
;*---------------------------------------------------------------------*/
(define (find-hopaccess path)
   (let loop ((p path))
      (let ((cache (cache-get hopaccess-cache p)))
	 (cond
	    (cache
	     (when (string? cache) cache))
	    ((string=? p "/")
	     (cache-put! hopaccess-cache path #t)
	     #f)
	    ((string=? p ".")
	     #f)
	    (else
	     (let ((hopaccess (make-file-name p (hop-hopaccess))))
		(if (file-exists? hopaccess)
		    (begin
		       (cache-put! hopaccess-cache path hopaccess)
		       hopaccess)
		    (loop (dirname p)))))))))

;*---------------------------------------------------------------------*/
;*    user-authorized-path? ...                                        */
;*---------------------------------------------------------------------*/
(define (user-authorized-path? user path)
   (define (path-member path dirs)
      (any? (lambda (d) (substring-at? path d 0)) dirs))
   (and (with-access::user user (directories services)
	   (or (eq? directories '*)
	       (path-member path directories)
	       (let ((cpath (file-name-unix-canonicalize path)))
		  (or (path-member cpath directories)
		      (let ((service-path (etc-path->service cpath)))
			 (and (symbol? service-path)
			      (user-authorized-service? user service-path)))))))
	(let ((hopaccess (find-hopaccess path)))
	   (or (not hopaccess)
	       (let ((access (with-input-from-file hopaccess read)))
		  (cond
		     ((eq? access '*)
		      #t)
		     ((list? access)
		      (member (user-name user) access))
		     (else
		      #f)))))))

;*---------------------------------------------------------------------*/
;*    authorized-path? ...                                             */
;*---------------------------------------------------------------------*/
(define (authorized-path? req path)
   (or ((hop-path-access-control) req path)
       (user-authorized-path? (http-request-user req) path)))

;*---------------------------------------------------------------------*/
;*    user-authorized-service? ...                                     */
;*---------------------------------------------------------------------*/
(define (user-authorized-service? user service)
   (or (with-access::user user (services)
	  (or (eq? services '*) (memq service services)))
       ((hop-authorize-service-hook) user service)
       ;; flash sends anonymous requests so we have to access server-event/init
       ;; requests for all users
       (eq? service 'server-event/init)
       (eq? service 'server-event/policy-file)))

;*---------------------------------------------------------------------*/
;*    authorized-service? ...                                          */
;*---------------------------------------------------------------------*/
(define (authorized-service? req service)
   (or ((hop-service-access-control) req service)
       (user-authorized-service? (http-request-user req) service)))

;*---------------------------------------------------------------------*/
;*    user-authorized-request? ...                                     */
;*---------------------------------------------------------------------*/
(define (user-authorized-request? user req)
   (or (with-access::http-request req (path)
	  (user-authorized-path? user path))
       ((hop-authorize-request-hook) user req)))

;*---------------------------------------------------------------------*/
;*    realm ...                                                        */
;*---------------------------------------------------------------------*/
(define (realm req)
   (format "Basic realm=\"hop@~a:~a\""
	   (http-request-host req)
	   (http-request-port req)))

;*---------------------------------------------------------------------*/
;*    user-access-denied ...                                           */
;*---------------------------------------------------------------------*/
(define (user-access-denied req #!optional message)
   (instantiate::http-response-authentication
      (header `((WWW-Authenticate: . ,(realm req))))
      (start-line "HTTP/1.0 401 Unauthorized")
      (request req)
      (body (cond
	       (message
		message)
	       ((http-request? req)
		(format "Protected Area! Authentication required: ~a:~a:/~a"
			(http-request-host req)
			(http-request-port req)
			(http-request-path req)))
	       (else
		"Protected Area! Authentication required.")))))

;*---------------------------------------------------------------------*/
;*    user-service-denied ...                                          */
;*---------------------------------------------------------------------*/
(define (user-service-denied req user svc)
   (instantiate::http-response-authentication
      (header `((WWW-Authenticate: . ,(realm req))))
      (start-line "HTTP/1.0 401 Unauthorized")
      (request req)
      (body (format "User `~a' is not allowed to execute service `~a'."
		    (user-name user) svc))))

;*---------------------------------------------------------------------*/
;*    proxy-denied ...                                                 */
;*---------------------------------------------------------------------*/
(define (proxy-denied req user host)
   (instantiate::http-response-authentication
      (request req)
      (start-line "HTTP/1.0 407 Proxy Authentication Required")
      (header `((Proxy-Authenticate:
		 .
		 ,(format "Basic realm=\"Hop proxy (~a) authentication\""
			  host))))
      (body (format "Protected Area! Authentication required for user `~a'."
		    (user-name user)))))
