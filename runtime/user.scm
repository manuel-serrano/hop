;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/user.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 19 14:13:15 2005                          */
;*    Last change :  Fri Feb 24 13:04:43 2006 (serrano)                */
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
	    __hop_http-lib)
   
   (export  (add-user! ::bstring . opt)
	    (user-exists? ::bstring)
	    (users-added?::bool)
	    (find-authenticated-user ::bstring)
	    (user-authorized-request?::bool ::obj ::http-request)
	    (user-authorized-path?::bool ::obj ::bstring)
	    (user-authorized-service?::bool ::obj ::symbol)
	    (user-access-denied ::http-request)))

;*---------------------------------------------------------------------*/
;*    *user-mutex* ...                                                 */
;*---------------------------------------------------------------------*/
(define *user-mutex* (make-mutex "hop-user"))

;*---------------------------------------------------------------------*/
;*    *users* ...                                                      */
;*---------------------------------------------------------------------*/
(define *users* #f)

;*---------------------------------------------------------------------*/
;*    users-added? ...                                                 */
;*---------------------------------------------------------------------*/
(define (users-added?)
   (hashtable? *users*))

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
;*    add-user! ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-user! name . args)
   (let loop ((a args)
	      (g '())
	      (p #f)
	      (s '())
	      (d #f))
      (cond
	 ((null? a)
	  (if (string? p)
	      (let ((u (instantiate::user
			  (name name)
			  (groups g)
			  (password p)
			  (services s)
			  (directories d)))
		    (k (make-user-key name p)))
		 (with-lock *user-mutex*
		    (lambda ()
		       (unless (hashtable? *users*)
			  (set! *users* (make-hashtable)))
		       (if (hashtable-get *users* name)
			   (error 'add-user! "User already added" name)
			   (hashtable-put! *users* name u))))
		 u)
	      (error 'add-user! "Password missing" p)))
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
		  (loop (cddr a) g p (if (eq? s '*) s (cadr a)) d)))
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
	 (and (hashtable? *users*)
	      (user? (hashtable-get *users* name))))))

;*---------------------------------------------------------------------*/
;*    *authenticated-users* ...                                        */
;*    -------------------------------------------------------------    */
;*    We use a two level cache. A pair of globla variables for         */
;*    level 1 and a hashtable for level 2.                             */
;*---------------------------------------------------------------------*/
(define *authenticated-users* (make-hashtable 5))
(define *last-authentication* #f)
(define *last-user* #f)

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
	(users-added?)
	(or (find-cached-user auth)
	    (let* ((dauth (http-decode-authentication auth))
		   (len (string-length dauth))
		   (i (string-index dauth #\:))
		   (u (and (>=fx i 0)
			   (let* ((nm (substring dauth 0 i))
				  (pd (substring dauth (+fx i 1) len))
				  (u (hashtable-get *users* nm)))
			      (and (user? u)
				   (string=? (md5sum (format "~a ~a" nm pd))
					     (user-password u))
				   u)))))
	       (when (user? u)
		  (add-cached-user! auth u))
	       u))))

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
	  (let ((hopaccess (make-file-name path ".hopaccess")))
	     (if (file-exists? hopaccess)
		 hopaccess
		 (loop (dirname path))))))))

;*---------------------------------------------------------------------*/
;*    user-authorized-path? ...                                        */
;*---------------------------------------------------------------------*/
(define (user-authorized-path? user path)
   (define (path-member path dirs)
      (any? (lambda (d) (substring-at? path d 0)) dirs))
   (or (not (users-added?))
       (and (user? user)
	    (with-access::user user (directories)
	       (or (eq? directories '*)
		   (path-member (file-name-unix-canonicalize path)
				directories)))
	    (let ((hopaccess (find-hopaccess path)))
	       (or (not hopaccess)
		   (member (user-name user)
			   (with-input-from-file hopaccess read)))))))

;*---------------------------------------------------------------------*/
;*    user-authorized-service? ...                                     */
;*---------------------------------------------------------------------*/
(define (user-authorized-service? user service)
   (or (not (users-added?))
       (and (user? user)
	    (with-access::user user (services)
	       (or (eq? services '*) (memq service services))))
       ((hop-authorize-service-hook) user service)))

;*---------------------------------------------------------------------*/
;*    user-authorized-request? ...                                     */
;*---------------------------------------------------------------------*/
(define (user-authorized-request? user req)
   (or (and (not (users-added?)) (http-request-localclientp req))
       (and (user? user)
	    (with-access::http-request req (path)
	       (user-authorized-path? user path)))
       ((hop-authorize-request-hook) user req)))

;*---------------------------------------------------------------------*/
;*    user-access-denied ...                                           */
;*---------------------------------------------------------------------*/
(define (user-access-denied req)
   (instantiate::http-response-authentication
      (header '((WWW-Authenticate: . "Basic realm=\"Hop authentication\"")))
      (body (if (http-request? req)
		(format "Protected Area! Authentication required.~a:~a:/~a"
			(http-request-host req)
			(http-request-port req)
			(http-request-path req))
		"Protected Area! Authentication required."))))

