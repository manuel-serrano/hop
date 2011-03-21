;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/user.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 19 14:13:15 2005                          */
;*    Last change :  Mon Mar 21 16:45:25 2011 (serrano)                */
;*    Copyright   :  2005-11 Manuel Serrano                            */
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
	    __hop_cache
	    __hop_password)

   (export  (users-close!)
	    (add-user! ::bstring . opt)
	    (user-exists? ::bstring)
	    (user-authentication-encrypt::bstring ::symbol ::bstring ::bstring)
	    (anonymous-user::user)
	    (find-authenticated-user ::bstring ::bstring ::symbol ::bstring)
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
(define *basic-realm* #f)

;*---------------------------------------------------------------------*/
;*    users-close! ...                                                 */
;*---------------------------------------------------------------------*/
(define (users-close!)
   (set! *users-open* #f))
   (set! *basic-realm* (format "Basic realm=\"Basic ~a authentication\"" (hop-realm)))

;*---------------------------------------------------------------------*/
;*    add-user! ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-user! name . args)
   (if *users-open*
       (%add-user! name args)
       (error "add-user!" "User registration closed" #f)))

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
	 (u #f)
	 (d (list (hop-share-directory)
		  (hop-var-directory))))
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
			  (auth 'basic)
			  (pass (cond
				   ((or (not (string? p))
					(=fx (string-length p) 0))
				    (set! auth 'digest)
				    (symbol->string (gensym)))
				   ((char=? (string-ref p 0) #\+)
				    (set! auth 'digest)
				    (substring p 1))
				   (else
				    p)))
			  (u (instantiate::user
				(name name)
				(groups g)
				(password pass)
				(authentication auth)
				(services (if (eq? s '*) s (cons (hop-service-weblet-wid) s)))
				(preferences (append c prefs))
				(preferences-filename cname)
				(directories d)
				(uuid u))))
		      (if (hashtable-get *users* name)
			  (begin
			     (hashtable-remove! *users* name)
			     (hashtable-put! *users* name u))
			  (hashtable-put! *users* name u))
		      u))))
	    ((or (not (keyword? (car a))) (null? (cdr a)))
	     (error "add-user!" "Illegal arguments" args))
	    (else
	     (case (car a)
		((:groups)
		 (if (not (and (list? (cadr a)) (every? symbol? (cadr a))))
		     (error "add-user!" "Illegal group" (cadr a))
		     (set! g (cadr a))))
		((:password)
		 (if (not (string? (cadr a)))
		     (error "add-user!" "Illegal password" (cadr a))
		     (set! p (cadr a))))
		((:services)
		 (if (not (or (eq? (cadr a) '*)
			      (and (list? (cadr a)) (every? symbol? (cadr a)))))
		     (error "add-user!" "Illegal services" (cadr a))
		     (unless (eq? s '*)
			(if (eq? (cadr a) '*)
			    (set! s '*)
			    (set! s (cadr a))))))
		((:directories)
		 (unless (eq? d '*)
		    (cond
		       ((eq? (cadr a) '*)
			(set! d '*))
		       ((and (list? (cadr a)) (every? string? (cadr a)))
			(set! d (append (map file-name-unix-canonicalize (cadr a)) d)))
		       (else
			(error "add-user!" "Illegal directories" (cadr a))))))
		((:preferences)
		 (set! c (append c (cadr a))))
		((:preferences-filename)
		 (set! cname (cadr a)))
		((:uuid)
		 (set! u (cadr a)))
		(else
		 (error "add-user!" "Illegal argument" args)))
	     (loop (cddr a)))))))

;*---------------------------------------------------------------------*/
;*    user-exists? ...                                                 */
;*---------------------------------------------------------------------*/
(define (user-exists? name)
   (with-lock *user-mutex*
      (lambda ()
	 (user? (hashtable-get *users* name)))))

;*---------------------------------------------------------------------*/
;*    user-authentication-encrypt ...                                  */
;*---------------------------------------------------------------------*/
(define (user-authentication-encrypt algo auth path)
   (let ((a (http-parse-authentication auth)))
      (case (car a)
	 ((basic url)
	  (let* ((auth (cdr a))
		 (i (string-index auth #\:))
		 (n (substring auth 0 i))
		 (p (substring auth (+fx i 1) (string-length auth)))
		 (s (let ((u (hashtable-get *users* n)))
		       (if (user? u)
			   (user-authentication u)
			   'basic))))
	     (authentication-encrypt s algo (hop-session) n p path)))
	 (else
	  (error "user-authentication-encrypt" "Illegal authentication" auth)))))

;*---------------------------------------------------------------------*/
;*    find-authorized-user ...                                         */
;*    -------------------------------------------------------------    */
;*    This function never raises an error. It returns #f if something  */
;*    goes wrong.                                                      */
;*---------------------------------------------------------------------*/
(define (find-authorized-user auth l::pair path::bstring method::symbol ip::bstring)

   (define (cannot-authenticate m n)
      (hop-verb 2 m " Can't authentify user: " n "\n")
      #f)
      
   (define (find-none-authentication n p)
      (let ((u (hashtable-get *users* n)))
	 (if (user? u)
	     (with-access::user u (password authentication)
		(if (string=? password (password-encrypt n p authentication))
		    (add-cached-user! auth u)
		    (cannot-authenticate "basic:" n)))
	     (cannot-authenticate "basic:" n))))
   
   (define (find-ho0-authentication n md5p)
      (let ((u (hashtable-get *users* n)))
	 (if (user? u)
	     (with-access::user u (password)
		(let ((p (h0password password path)))
		   (if (string=? p md5p)
		       (add-cached-user! auth u)
		       (cannot-authenticate "HO0" n))))
	     (cannot-authenticate "HO0:" n))))
   
   (define (find-ho1-authentication n md5p path)
      (let ((u (hashtable-get *users* n)))
	 (if (user? u)
	     (with-access::user u (password)
		(let ((p (h1password password path (hop-session))))
		   (if (string=? p md5p)
		       (add-cached-user! auth u)
		       (cannot-authenticate  "H01:" n))))
	     (cannot-authenticate  "H01:" n))))

   (define (find-ho2-authentication n md5p path)
      (let ((u (hashtable-get *users* n)))
	 (if (user? u)
	     (with-access::user u (password)
		(let ((p (h2password password path (hop-session) ip)))
		   (if (string=? p md5p)
		       (add-cached-user! auth u)
		       (cannot-authenticate  "H02:" n))))
	     (cannot-authenticate  "H02:" n))))

   (define (find-digest-authentication l)
      
      (define (get k l)
	 (let ((c (assq k l)))
	    (when (pair? c)
	       (cdr c))))
      
      (define (H str)
	 (md5sum-string str))
      
      (define (KD secret data)
	 (H (string-append secret ":" data)))
      
      (define (request-digest A1 n)
	 (let ((nc (get 'nc l))
	       (nonce (get 'nonce l))
	       (qop (get 'qop l))
	       (uri (get 'uri l)))
	    (if (and (string? nonce) (string? nc) (string? uri))
		;; Hop does not support auth-int so A2 is
		;; only defined as follows
		(let ((A2 (string-append
			   (symbol->string method) ":" uri)))
		   (if (equal? qop "auth")
		       (let ((cnonce (get 'cnonce l)))
			  (if (string? cnonce)
			      (KD A1
				  (string-append
				   nonce
				   ":" nc
				   ":" cnonce
				   ":" qop
				   ":" (H A2)))
			      (cannot-authenticate "digest:" n)))
		       (KD (H A1) (string-append nonce ":" (H A2)))))
		(cannot-authenticate "digest:" n))))
      
      (let* ((n (get 'username l))
	     (u (hashtable-get *users* n)))
	 (if (user? u)
	     (with-access::user u (password authentication)
		(if (eq? authentication 'digest)
		    (let ((opaque (get 'opaque l))
			  (realm (get 'realm l))
			  (response (get 'response l)))
		       (if (and (string? opaque)
				(string=? opaque digest-opaque)
				(string? realm)
				(string=? realm (hop-realm))
				(string? response))
			   (let ((request (request-digest password n)))
			      (if (and (string? request)
				       (string=? request response))
				  (add-cached-user! auth u)
				  (cannot-authenticate "digest:" n)))
			   (cannot-authenticate "digest:" n)))
		    (cannot-authenticate "digest:" n))))))

   (case (car l)
      ((basic url)
       (let* ((auth (cdr l))
	      (i (string-index auth #\:)))
	  (when (and (fixnum? i) (>fx i 0))
	     (let ((s (substring auth 0 i))
		   (p (substring auth (+fx i 1) (string-length auth))))
		(cond
		   ((substring-at? s "HO0" 0)
		    (let ((n (substring s 3 (string-length s))))
		       (find-ho0-authentication n p)))
		   ((substring-at? s "HO1" 0)
		    (let ((n (substring s 3 (string-length s))))
		       (find-ho1-authentication n p path)))
		   ((substring-at? s "HO2" 0)
		    (let ((n (substring s 3 (string-length s))))
		       (find-ho2-authentication n p path)))
		   (else
		    (find-none-authentication s p)))))))
      ((digest)
       (find-digest-authentication (cdr l)))))
   
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
	      (error "anonymous-user" "No anonymous user declared" a))
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
	     (let ((u (hashtable-get *authenticated-users* auth)))
		(when (user? u)
		   (set! *last-authentication* auth)
		   (set! *last-user* u))
		u)))))

;*---------------------------------------------------------------------*/
;*    add-cached-user! ...                                             */
;*---------------------------------------------------------------------*/
(define (add-cached-user! auth u)
   (with-lock *user-mutex*
      (lambda ()
	 (when (string? *last-authentication*)
	    (hashtable-put! *authenticated-users* *last-authentication* *last-user*))
	 (set! *last-authentication* auth)
	 (set! *last-user* u)
	 u)))

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
(define (find-authenticated-user auth path method ip)
   (and (string? auth)
	(or (find-cached-user auth)
	    (find-authorized-user auth (http-parse-authentication auth) path method ip))))

;*---------------------------------------------------------------------*/
;*    find-unauthenticated-user ...                                    */
;*---------------------------------------------------------------------*/
(define (find-unauthenticated-user req)
   
   (define (find-request-user-name req)
      (with-access::http-request req (userinfo abspath authorization)
	 (cond
	    ((string? userinfo)
	     (let ((i (string-index userinfo #\:)))
		(when i (substring userinfo 0 (-fx i 1)))))
	    (authorization
	     (let ((auth (http-parse-authentication authorization)))
		(case (car auth)
		   ((basic)
		    (let* ((auth (cdr auth))
			   (i (string-index auth #\:)))
		       (when (and (fixnum? i) (>fx i 0))
			  (substring auth 0 i))))
		   ((digest)
		    (let ((c (assq 'username (cdr auth))))
		       (when (pair? c) (cdr c))))))))))
   
   (let ((name (find-request-user-name req)))
      (when (string? name)
	 (hashtable-get *users* name))))

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
      (any? (lambda (d)
	       (and (substring-at? path d 0)
		    (or (=fx (string-length d) (string-length path))
			(and (>fx (string-length path) (string-length d))
			     (char=? (string-ref path (string-length d))
				     (file-separator))))))
	    dirs))
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
	  (or (eq? services '*) (and (pair? services) (memq service services))))
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
;*    basic-authenticate ...                                           */
;*    -------------------------------------------------------------    */
;*    The basic realm is computed once for all when users are closed   */
;*    (see users-close!).                                              */
;*---------------------------------------------------------------------*/
(define (basic-authenticate req)
   *basic-realm*)

;*---------------------------------------------------------------------*/
;*    digest-private-key ...                                           */
;*---------------------------------------------------------------------*/
(define digest-private-key
   (number->string
    (bit-xor (random 10000000) (elong->fixnum (current-seconds)))))

;*---------------------------------------------------------------------*/
;*    digest-opaque ...                                                */
;*---------------------------------------------------------------------*/
(define digest-opaque
   (base64-encode (format "~a:~a:~a" (hostname) (hop-port) (hop-session))))

;*---------------------------------------------------------------------*/
;*    digest-authenticate ...                                          */
;*---------------------------------------------------------------------*/
(define (digest-authenticate req)
   (let ((nonce (base64-encode
		 (string-append
		  (number->string (current-seconds)) digest-private-key))))
      (format "Digest realm=\"~a\", qop=\"auth\", nonce=\"~a\", opaque=\"~a\""
	      (hop-realm)
	      nonce
	      digest-opaque)))

;*---------------------------------------------------------------------*/
;*    authenticate-header ...                                          */
;*---------------------------------------------------------------------*/
(define (authenticate-header req)
   (with-access::http-request req (http)
      (if (or (eq? (hop-http-authentication) 'basic)
	      (eq? http 'HTTP/1.0)
	      (let ((user (find-unauthenticated-user req)))
		 (and (user? user) (eq? (user-authentication user) 'basic))))
	  `((WWW-Authenticate: . ,(basic-authenticate req))
	    (hop-session: . ,(hop-session)))
	  `((WWW-Authenticate: . ,(digest-authenticate req))
	    (hop-session: . ,(hop-session)))))) ;

;*---------------------------------------------------------------------*/
;*    user-access-denied ...                                           */
;*---------------------------------------------------------------------*/
(define (user-access-denied req #!optional message)
   (instantiate::http-response-authentication
      (header (authenticate-header req))
      (start-line "HTTP/1.0 401 Unauthorized")
      (request req)
      (body (cond
	       (message
		message)
	       ((http-request? req)
		(format "Protected Area! Authentication required: ~a:~a:~a"
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
      (header (authenticate-header req))
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
