;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/service.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:29:08 2006                          */
;*    Last change :  Thu Jun  7 10:07:30 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP services                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_service
   
   (include "service.sch")
   
   (library web)
   
   (import  __hop_param
	    __hop_types
	    __hop_misc
	    __hop_read
	    __hop_thread
	    __hop_http-error
	    __hop_http-response
	    __hop_cgi
	    __hop_xml
	    __hop_hop-extra
	    __hop_js-lib
	    __hop_user)
   
   (export  (get-service-url::bstring)
	    (hop-service-path? ::bstring)
	    (make-hop-service-url::bstring ::hop-service . o)
	    (make-service-url::bstring ::hop-service . o)
	    (hop-request-service-name::bstring ::http-request)
	    (procedure->service::hop-service ::procedure)
            (%eval::%http-response ::obj ::http-request ::procedure)
	    (service-filter ::http-request)
	    (register-service!::hop-service ::hop-service)
	    (expired-service-path?::bool ::bstring)
	    (service-resource::bstring ::hop-service #!optional file)
	    (service-base-url::bstring ::hop-service ::http-request)))

;*---------------------------------------------------------------------*/
;*    mutexes ...                                                      */
;*---------------------------------------------------------------------*/
(define *service-table-mutex* (make-mutex "hop-service-table"))
(define *service-serialize-mutex* (make-mutex "hop-service-serialize"))

;*---------------------------------------------------------------------*/
;*    *service-table-count* ...                                        */
;*---------------------------------------------------------------------*/
(define *service-table-count* 1)

;*---------------------------------------------------------------------*/
;*    get-service-url ...                                              */
;*---------------------------------------------------------------------*/
(define (get-service-url)
   (with-lock *service-table-mutex*
      (lambda ()
	 (set! *service-table-count* (+fx 1 *service-table-count*))
	 (format "~a/~a" (hop-service-weblet-name) *service-table-count*))))

;*---------------------------------------------------------------------*/
;*    hop-service-path? ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-service-path? path)
   (let ((l1 (string-length (hop-service-base)))
	 (lp (string-length path)))
      (and (substring-at? path (hop-service-base) 0)
	   (or (=fx lp l1)
	       (and (>fx lp l1)
		    (char=? (string-ref path l1) #\/))))))
   
;*---------------------------------------------------------------------*/
;*    hop-request-service-name ...                                     */
;*---------------------------------------------------------------------*/
(define (hop-request-service-name req)
   (let* ((path (http-request-path req))
	  (len (string-length path)))
      (let loop ((i 1))
	 (cond
	    ((=fx i len)
	     path)
	    ((char=? (string-ref path i) #\?)
	     (substring path 0 i))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    make-hop-service-url ...                                         */
;*---------------------------------------------------------------------*/
(define (make-hop-service-url svc . vals)
   (if (not (hop-service? svc))
       (bigloo-type-error 'make-hop-service-url 'service svc)
       (with-access::hop-service svc (path args)
	  (if (and (null? args) (null? vals))
	      path
	      (apply string-append
		     path
		     "?hop-encoding=hop"
		     (map (lambda (f v)
			     (format "&~a=~a" f (url-encode (obj->string v))))
			  args vals))))))

;*---------------------------------------------------------------------*/
;*    make-service-url ...                                             */
;*---------------------------------------------------------------------*/
(define (make-service-url svc . vals)
   (if (not (hop-service? svc))
       (bigloo-type-error 'make-hop-service-url 'service svc)
       (with-access::hop-service svc (id path args)
	  (cond
	     ((null? args)
	      (if (null? vals)
		  path
		  (error 'make-service-url id "too many arguments provided")))
	     ((=fx (length args) (length vals))
	      (apply string-append
		     path
		     "?hop-encoding=none"
		     (map (lambda (f v)
			     (let ((a (if (string? v) (url-encode v) v)))
				(format "&~a=~a" f a)))
			  args vals)))
	     ((<fx (length args) (length vals))
	      (error 'make-service-url id "too many arguments provided"))
	     (else
	      (error 'make-service-url id "missing arguments"))))))

;*---------------------------------------------------------------------*/
;*    procedure->service ...                                           */
;*---------------------------------------------------------------------*/
(define (procedure->service::hop-service proc::procedure)
   (let ((arity (procedure-arity proc)))
      (case arity
	 ((0)
	  (service () (proc)))
	 ((1)
	  (service (a0) (proc a0)))
	 ((2)
	  (service (a0 a1) (proc a0 a1)))
	 ((3)
	  (service (a0 a1 a2) (proc a0 a1 a2)))
	 ((4)
	  (service (a0 a1 a2 a3) (proc a0 a1 a2 a3)))
	 ((5)
	  (service (a0 a1 a2 a3 a4) (proc a0 a1 a2 a3 a4)))
	 ((6)
	  (service (a0 a1 a2 a3 a4 a5) (proc a0 a1 a2 a3 a4 a5)))
	 ((7)
	  (service (a0 a1 a2 a3 a4 a5 a6) (proc a0 a1 a2 a3 a4 a5 a6)))
	 ((8)
	  (service (a0 a1 a2 a3 a4 a5 a6 a7) (proc a0 a1 a2 a3 a4 a5 a6 a7)))
	 (else
	  (error 'procedure->service "arity not supported" proc)))))

;*---------------------------------------------------------------------*/
;*    exp->eval-string ...                                             */
;*    -------------------------------------------------------------    */
;*    JS eval does not accept \n inside quotes!                        */
;*---------------------------------------------------------------------*/
(define (exp->eval-string exp)
   (cond
      ((string? exp)
       (string-replace exp #\Newline #\space))
      ((xml-tilde? exp)
       (with-access::xml-tilde exp (body)
	  (if (string? body)
	      (let ((l (string-length body)))
		 (if (substring-at? body ";\n" (-fx l 2))
		     (string-replace (substring body 0 (-fx l 2))
				     #\Newline #\space)
		     (string-replace body #\Newline #\space)))
	      body)))
      ((list? exp)
       (apply string-append (map exp->eval-string exp)))
      (else
       exp)))

;*---------------------------------------------------------------------*/
;*    exp-list->eval-string ...                                        */
;*---------------------------------------------------------------------*/
(define (exp-list->eval-string exp)
   (if (null? exp)
       "[]"
       (let loop ((exp exp)
		  (res '()))
	  (cond
	     ((null? (cdr exp))
	      (apply string-append "["
		     (reverse! (cons* "]" (exp->eval-string (car exp)) res))))
	     (else
	      (loop (cdr exp)
		    (cons* "," (exp->eval-string (car exp)) res)))))))

;*---------------------------------------------------------------------*/
;*    %eval ...                                                        */
;*---------------------------------------------------------------------*/
(define (%eval exp req cont)
   (let ((s (hop->json (procedure->service (lambda (res) (cont res))))))
      (instantiate::http-response-hop
	 (backend (hop-xml-backend))
	 (content-type (xml-backend-mime-type (hop-xml-backend)))
	 (request req)
	 (xml (<HTML>
		 (<HEAD>)
		 (<BODY>
		    (<SCRIPT>
		       (format "hop( ~a( eval( '~a' ) ), true )"
			       s
			       (exp-list->eval-string exp)))))))))

;*---------------------------------------------------------------------*/
;*    *service-mutex* ...                                              */
;*---------------------------------------------------------------------*/
(define *service-mutex* (make-mutex))

;*---------------------------------------------------------------------*/
;*    *service-table*                                                  */
;*---------------------------------------------------------------------*/
(define *service-table*
   (make-hashtable #unspecified #unspecified equal-path? hash-path))

;*---------------------------------------------------------------------*/
;*    equal-path? ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes that p1 and p2 shares (hop-service-base)   */
;*    as prefix.                                                       */
;*---------------------------------------------------------------------*/
(define (equal-path? p1 p2)
   (let ((l1 (string-length p1))
	 (l2 (string-length p2)))
      (and (>=fx l2 l1)
	   (let loop ((i (+fx 1 (string-length (hop-service-base)))))
	      (cond
		 ((=fx i l1)
		  (or (=fx i l2) (char=? (string-ref p2 i) #\?)))
		 ((char=? (string-ref p1 i) (string-ref p2 i))
		  (loop (+fx i 1)))
		 (else
		  #f))))))

;*---------------------------------------------------------------------*/
;*    hash-path ...                                                    */
;*---------------------------------------------------------------------*/
(define (hash-path::long p)
   (let ((l (string-length p)))
      (let loop ((i (+fx 1 (string-length (hop-service-base))))
		 (r::long 0))
	 (if (>=fx i l)
	     (bit-and r (-fx (bit-lsh 1 29) 1))
	     (let ((c (string-ref p i)))
		(if (char=? c #\?)
		    (bit-and r (-fx (bit-lsh 1 29) 1))
		    (loop (+fx i 1)
			  (+fx r (+fx (bit-lsh r 3) (char->integer c))))))))))

;*---------------------------------------------------------------------*/
;*    service-filter ...                                               */
;*    -------------------------------------------------------------    */
;*    This filter is executed after the AUTOLOAD-FILTER. Hence         */
;*    when the default service is selected, the entire HOP loop        */
;*    has to be re-executed in order to properly autoload the          */
;*    initial weblet.                                                  */
;*---------------------------------------------------------------------*/
(define (service-filter req)
   (when (http-request-localhostp req)
      (with-access::http-request req (path user service)
	 (when (hop-service-path? path)
	    (mutex-lock! *service-mutex*)
	    (let ((svc (hashtable-get *service-table* path)))
	       (mutex-unlock! *service-mutex*)
	       (cond
		  ((hop-service? svc)
		   (set! service svc)
		   (with-access::hop-service svc (%exec ttl path id wid)
		      (cond
			 ((=fx ttl 1)
			  (unregister-service! svc))
			 ((>fx ttl 1)
			  (set! ttl (-fx ttl 1))))
		      (cond
			 ((service-expired? svc)
			  (mark-service-path-expired! path)
			  #f)
			 ((or (user-authorized-service? user wid)
			      (user-authorized-service? user id))
			  (scheme->response (%exec req) req))
			 (else
			  (user-service-denied req user id)))))
		  (else
		   (let ((init (hop-initial-weblet)))
		      (cond
			 ((and (string? init)
			       (substring-at? path (hop-service-base) 0)
			       (let ((l1 (string-length path))
				     (l2 (string-length
					  (hop-service-base))))
				  (or (=fx l1 l2)
				      (and (=fx l1 (+fx l2 1))
					   (char=? (string-ref path l2)
						   #\/)))))
			  (set! path
				(string-append (hop-service-base) "/" init))
			  ;; resume the hop loop in order to autoload
			  ;; the initial weblet
			  'hop-resume)
			 (else
			  #f))))))))))

;*---------------------------------------------------------------------*/
;*    register-service! ...                                            */
;*---------------------------------------------------------------------*/
(define (register-service! svc)
   (with-access::hop-service svc (path)
      (let ((sz (hashtable-size *service-table*)))
	 (hop-verb 2 (hop-color 1 "" "REG. SERVICE ")
		   "(" (/fx sz 2) "): "
		   svc " " path "\n")
	 (mutex-lock! *service-mutex*)
	 (hashtable-put! *service-table* path svc)
	 (hashtable-put! *service-table* (string-append path "/") svc)
	 (let ((l (string-length path)))
	    (let loop ((i (+fx (string-length (hop-service-base)) 1)))
	       (cond
		  ((>=fx i l)
		   (hop-weblets-set! (cons svc (hop-weblets))))
		  ((char=? (string-ref path i) #\/)
		   #unspecified)
		  (else
		   (loop (+fx i 1))))))
	 (when (=fx (remainder sz *service-table-count*) 0)
	    (flush-expired-services!))
	 (mutex-unlock! *service-mutex*)
	 svc)))

;*---------------------------------------------------------------------*/
;*    service-expired? ...                                             */
;*---------------------------------------------------------------------*/
(define (service-expired? svc)
   (with-access::hop-service svc (creation timeout path)
      (and (>fx timeout 0)
	   (>elong (date->seconds (current-date))
		   (+elong creation timeout)))))

;*---------------------------------------------------------------------*/
;*    flush-expired-services! ...                                      */
;*---------------------------------------------------------------------*/
(define (flush-expired-services!)
   (hashtable-filter! *service-table*
		      (lambda (key svc)
			 (if (service-expired? svc)
			     (with-access::hop-service svc (path)
				(mark-service-path-expired! path)
				#f)
			     #t))))
			 
;*---------------------------------------------------------------------*/
;*    unregister-service! ...                                          */
;*---------------------------------------------------------------------*/
(define (unregister-service! svc)
   (with-access::hop-service svc (path)
      (mutex-lock! *service-mutex*)
      (hashtable-remove! *service-table* path)
      (hashtable-remove! *service-table* (string-append path "/"))
      (mutex-unlock! *service-mutex*)))

;*---------------------------------------------------------------------*/
;*    *expiration-table*                                               */
;*---------------------------------------------------------------------*/
(define *expiration-table*
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    *expiration-mutex* ...                                           */
;*---------------------------------------------------------------------*/
(define *expiration-mutex* (make-mutex))

;*---------------------------------------------------------------------*/
;*    expired-service-path? ...                                        */
;*---------------------------------------------------------------------*/
(define (expired-service-path? path)
   (mutex-lock! *expiration-mutex*)
   (let ((r (hashtable-get *expiration-table* path)))
      (mutex-unlock! *expiration-mutex*)
      r))

;*---------------------------------------------------------------------*/
;*    mark-service-path-expired! ...                                   */
;*---------------------------------------------------------------------*/
(define (mark-service-path-expired! path)
   (mutex-lock! *expiration-mutex*)
   (hashtable-put! *expiration-table* path #t)
   (mutex-unlock! *expiration-mutex*)
   #f)

;*---------------------------------------------------------------------*/
;*    service-resource ...                                             */
;*---------------------------------------------------------------------*/
(define (service-resource svc #!optional file)
   (with-access::hop-service svc (resource)
      (if (string? file)
	  (string-append resource "/" file)
	  resource)))
   
;*---------------------------------------------------------------------*/
;*    service-base-url ...                                             */
;*---------------------------------------------------------------------*/
(define (service-base-url svc req)
   (with-access::http-request req (scheme host port)
      (format "~a://~a:~a~a/"
	      (if (eq? scheme '*) "http" scheme) host port
	      (service-resource svc))))
