;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/user.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 19 14:13:15 2005                          */
;*    Last change :  Sat Dec 30 10:30:37 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    User support                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_user
   
   (import  __hop_param
	    __hop_types
	    __hop_http-lib
	    __hop_misc)
   
   (export  (users-close!)
	    (add-user! ::bstring . opt)
	    (user-exists? ::bstring)
	    (anonymous-user::user)
	    (find-authenticated-user ::bstring)
	    (find-user ::bstring ::bstring)
	    (find-user/encrypt ::bstring ::bstring ::procedure)
	    (user-authorized-request?::bool ::user ::http-request)
	    (user-authorized-path?::bool ::user ::bstring)
	    (authorized-path?::bool ::http-request ::bstring)
	    (user-authorized-service?::bool ::user ::symbol)
	    (authorized-service?::bool ::http-request ::symbol)
	    (user-access-denied ::http-request)
	    (user-service-denied ::http-request ::user ::symbol)))

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
   (let loop ((a args)
	      (g '())
	      (p #f)
	      (s '())
	      (d '()))
      (cond
	 ((null? a)
	  (let ((u (instantiate::user
		      (name name)
		      (groups g)
		      (password (or p (symbol->string (gensym))))
		      (services s)
		      (directories d)))
		(k (make-user-key name p)))
	     (with-lock *user-mutex*
		(lambda ()
		   (if (hashtable-get *users* name)
		       (begin
			  (hashtable-remove! *users* name)
			  (hashtable-put! *users* name u))
		       (hashtable-put! *users* name u))))
	     u))
	 ((or (not (keyword? (car a))) (null? (cdr a)))
	  (error 'add-user! "Illegal arguments" args))
	 (else
	  (case (car a)
	     ((:groups)
	      (if (not (and (list? (cadr a)) (every? symbol? (cadr a))))
		  (error 'add-user! "Illegal group" (cadr a))
		  (loop (cddr a) (cadr a) p s d)))
	     ((:password)
	      (if (not (string? (cadr a)))
		  (error 'add-user! "Illegal password" (cadr a))
		  (loop (cddr a) g (cadr a) s d)))
	     ((:services)
	      (if (not (or (eq? (cadr a) '*)
			   (and (list? (cadr a)) (every? symbol? (cadr a)))))
		  (error 'add-user! "Illegal services" (cadr a))
		  (loop (cddr a) g p
			(if (eq? s '*)
			    s
			    (if (eq? (cadr a) '*)
				'*
				(cons (hop-service-weblet-wid) (cadr a))))
			d)))
	     ((:directories)
	      (cond
		 ((eq? (cadr a) '*)
		  (loop (cddr a) g p s (if (eq? d '*) d (cadr a))))
		 ((and (list? (cadr a)) (every? string? (cadr a)))
		  (loop (cddr a) g p s (map file-name-unix-canonicalize (cadr a))))
		 (else
		  (error 'add-user! "Illegal directories" (cadr a)))))
	     (else
	      (error 'add-user! "Illegal argument" args)))))))

;*---------------------------------------------------------------------*/
;*    user-exists? ...                                                 */
;*---------------------------------------------------------------------*/
(define (user-exists? name)
   (with-lock *user-mutex*
      (lambda ()
	 (user? (hashtable-get *users* name)))))

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
;*    find-authenticated-user ...                                      */
;*---------------------------------------------------------------------*/
(define (find-authenticated-user auth)
   (and (string? auth)
	(or (find-cached-user auth)
	    (let* ((dauth (http-decode-authentication auth))
		   (len (string-length dauth))
		   (i (string-index dauth #\:)))
	       (and (>fx i 0)
		    (let* ((n (substring dauth 0 i))
			   (p (substring dauth (+fx i 1) len))
			   (u (find-user n (md5sum (format "~a ~a" n p)))))
		       (if (user? u)
			   (add-cached-user! auth u)
			   (hop-verb 2 "Can't authentify user: " n))
		       u))))))

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
;*    find-hopaccess ...                                               */
;*---------------------------------------------------------------------*/
(define (find-hopaccess path)
   (let loop ((path path))
      (cond
	 ((string=? path "/")
	  #f)
	 ((string=? path ".")
	  #f)
	 (else
	  (let ((hopaccess (make-file-name path (hop-hopaccess))))
	     (if (file-exists? hopaccess)
		 hopaccess
		 (loop (dirname path))))))))

;*---------------------------------------------------------------------*/
;*    user-authorized-path? ...                                        */
;*---------------------------------------------------------------------*/
(define (user-authorized-path? user path)
   (define (path-member path dirs)
      (any? (lambda (d) (substring-at? path d 0)) dirs))
   (and (with-access::user user (directories)
	   (or (eq? directories '*)
	       (path-member (file-name-unix-canonicalize path)
			    directories)))
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
       ((hop-authorize-service-hook) user service)))

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
;*    user-access-denied ...                                           */
;*---------------------------------------------------------------------*/
(define (user-access-denied req)
   (instantiate::http-response-authentication
      (header '((WWW-Authenticate: . "Basic realm=\"Hop authentication\"")))
      (request req)
      (body (if (http-request? req)
		(format "Protected Area! Authentication required.~a:~a:/~a"
			(http-request-host req)
			(http-request-port req)
			(http-request-path req))
		"Protected Area! Authentication required."))))

;*---------------------------------------------------------------------*/
;*    user-service-denied ...                                          */
;*---------------------------------------------------------------------*/
(define (user-service-denied req user svc)
   (instantiate::http-response-authentication
      (header '((WWW-Authenticate: . "Basic realm=\"Hop authentication\"")))
      (request req)
      (body (format "User `~a' is not allowed to execute service `~a'."
		    (user-name user) svc))))
