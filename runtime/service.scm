;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/service.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:29:08 2006                          */
;*    Last change :  Sat Jun 19 06:26:58 2010 (serrano)                */
;*    Copyright   :  2006-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP services                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_service
   
   (include "service.sch")
   
   (library web)
   
   (import  __hop_configure
	    __hop_param
	    __hop_types
	    __hop_misc
	    __hop_read
	    __hop_http-error
	    __hop_http-response
	    __hop_cgi
	    __hop_xml
	    __hop_html
	    __hop_hop-extra
	    __hop_prefs
	    __hop_js-lib
	    __hop_user
	    __hop_weblets)
   
   (export  (init-hop-services!)
	    (inline service?::bool ::obj)
	    (get-all-services ::http-request)
	    (gen-service-url::bstring #!optional (prefix ""))
	    (hop-service-path? ::bstring)
	    (hop-apply-nice-url::bstring ::bstring ::pair-nil)
	    (hop-apply-url::bstring ::bstring ::pair-nil)
	    (service-funcall-url::bstring ::hop-service . o)
	    (hop-request-service-name::bstring ::http-request)
	    (procedure->service::procedure ::procedure)
            (%eval::%http-response ::obj ::http-request ::procedure)
	    (service-filter ::http-request)
	    (register-service!::hop-service ::hop-service)
	    (expired-service-path?::bool ::bstring)
	    (service-resource::bstring ::procedure #!optional file)
	    (service-path::bstring ::procedure)
	    (service-base-url::bstring ::procedure ::http-request)
	    (service-etc-path-table-fill! ::bstring)
	    (etc-path->service ::bstring)))

;*---------------------------------------------------------------------*/
;*    service? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (service? obj)
   (and (procedure? obj) (hop-service? (procedure-attr obj))))

;*---------------------------------------------------------------------*/
;*    mutexes ...                                                      */
;*---------------------------------------------------------------------*/
(define *service-mutex* (make-mutex "service"))
(define *expiration-mutex* (make-mutex "expiration"))
(define *service-table-mutex* (make-mutex "hop-service-table"))
(define *service-etc-table-mutex* (make-mutex "hop-service-etc-table"))
(define *service-serialize-mutex* (make-mutex "hop-service-serialize"))

;*---------------------------------------------------------------------*/
;*    *service-table-count* ...                                        */
;*---------------------------------------------------------------------*/
(define *service-table-count* 1)

;*---------------------------------------------------------------------*/
;*    *service-table*                                                  */
;*---------------------------------------------------------------------*/
(define *service-table*
   (make-hashtable #unspecified #unspecified equal-path? hash-path))

;*---------------------------------------------------------------------*/
;*    get-all-services ...                                             */
;*---------------------------------------------------------------------*/
(define (get-all-services req)
   (with-lock *service-table-mutex*
      (lambda ()
	 (delete-duplicates!
	  (filter (lambda (svc)
		     (with-access::hop-service svc (id wid)
			(or (authorized-service? req wid)
			    (authorized-service? req id))))
		  (hashtable->list *service-table*))))))

;*---------------------------------------------------------------------*/
;*    init-hop-services! ...                                           */
;*    -------------------------------------------------------------    */
;*    Create the first HOP services.                                   */
;*---------------------------------------------------------------------*/
(define (init-hop-services!)
   (init-hop-prefs-services!))

;*---------------------------------------------------------------------*/
;*    gen-service-url ...                                              */
;*---------------------------------------------------------------------*/
(define (gen-service-url #!optional (prefix ""))
   (with-lock *service-table-mutex*
      (lambda ()
	 (set! *service-table-count* (+fx 1 *service-table-count*))
	 (format "~a/~a~a"
		 (hop-service-weblet-name)
		 prefix
		 *service-table-count*))))
 
;*---------------------------------------------------------------------*/
;*    hop-service-path? ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-service-path? path)
   (when (substring-at? path (hop-service-base) 0)
      (let ((l1 (string-length (hop-service-base)))
	    (lp (string-length path)))
	 (or (=fx lp l1)
	     (and (>fx lp l1) (char=? (string-ref path l1) #\/))))))
   
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
;*    hop-apply-nice-url ...                                           */
;*    -------------------------------------------------------------    */
;*    This is used for fix arity function in order to build nice URLS. */
;*    When at least an argument is not a string, it falls back to      */
;*    hop-apply-url.                                                   */
;*---------------------------------------------------------------------*/
(define (hop-apply-nice-url base vals)
   
   (define (all-keyword-string? vals)
      (cond
	 ((null? vals)
	  #t)
	 ((null? (cdr vals))
	  #f)
	 ((and (keyword? (car vals)) (string? (cadr vals)))
	  (all-keyword-string? (cddr vals)))
	 (else
	  #f)))
   
   (cond
      ((null? vals)
       base)
      ((all-keyword-string? vals)
       (let loop ((vals vals)
		  (sep "?")
		  (strs '()))
	  (if (null? vals)
	      (apply string-append base strs)
	      (loop (cddr vals)
		    "&"
		    (let ((str (string-append
				sep
				(keyword->string (car vals))
				"="
				(url-path-encode (cadr vals)))))
		       (cons str strs))))))
      (else
       (hop-apply-url base vals))))
       
;*---------------------------------------------------------------------*/
;*    hop-apply-url ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-apply-url base vals)
   (let ((o (if (vector? vals) (vector->list vals) vals)))
      (string-append base
		     "?hop-encoding=hop"
		     "&vals=" (url-path-encode (obj->string o)))))

;*---------------------------------------------------------------------*/
;*    service-funcall-url ...                                          */
;*---------------------------------------------------------------------*/
(define (service-funcall-url svc . vals)
   (if (not (hop-service? svc))
       (bigloo-type-error 'service-funcall-url 'service svc)
       (with-access::hop-service svc (path)
	  (hop-apply-url path vals))))

;*---------------------------------------------------------------------*/
;*    service-handler ...                                              */
;*---------------------------------------------------------------------*/
(define (service-handler svc req)
   
   (define (invoke proc vals)
      (hop-verb 2 (hop-color req req " INVOKE.svc")
		" "
		(with-output-to-string
		   (lambda () (write (cons (hop-service-id svc) vals))))
		"\n")
      (cond
	 ((not vals)
	  (error (hop-service-id svc)
		 "Illegal service arguments encoding"
		 `(,(hop-service-id svc) ,vals)))
	 ((correct-arity? proc (length vals))
	  (apply proc vals))
	 (else
	  (error (hop-service-id svc)
		 "Wrong number of arguments"
		 `(,(hop-service-id svc) ,@vals)))))
   
   (let* ((ca (http-request-cgi-args req)))
      (cond
	 ((null? (cdr ca))
	  (invoke (hop-service-proc svc) '()))
	 ((equal? (cgi-arg "hop-encoding" ca) "hop")
	  (http-request-charset-set! req 'UTF-8)
	  (invoke (hop-service-proc svc) (serialized-cgi-arg "vals" ca)))
	 (else
	  (invoke (hop-service-proc svc)
		  (append-map (lambda (p)
				 (list (string->keyword (car p)) (cdr p)))
			      (cdr ca)))))))

;*---------------------------------------------------------------------*/
;*    procedure->service ...                                           */
;*---------------------------------------------------------------------*/
(define (procedure->service::procedure proc::procedure)
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
	  (error "procedure->service" "arity not supported" proc)))))

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
       (let ((body (xml-tilde->expression exp)))
	  (let ((l (string-length body)))
	     (if (substring-at? body ";\n" (-fx l 2))
		 (string-replace (substring body 0 (-fx l 2)) #\Newline #\space)
		 (string-replace body #\Newline #\space)))))
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
   (let ((s (call-with-output-string
	     (lambda (op)
		(obj->javascript (procedure->service (lambda (res) (cont res))) op #f)))))
      (instantiate::http-response-hop
	 (backend (hop-xml-backend))
	 (content-type (xml-backend-mime-type (hop-xml-backend)))
	 (request req)
	 (header '((Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	 (xml (<HTML>
		 (<HEAD>)
		 (<BODY>
		    (<SCRIPT>
		       (format "hop( ~a( eval( '~a' ) ), true )"
			       s
			       (exp-list->eval-string exp)))))))))

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
   (when (http-server-request? req)
      (with-access::http-server-request req (abspath user service method)
	 (when (hop-service-path? abspath)
	    (mutex-lock! *service-mutex*)
	    (let loop ((svc (hashtable-get *service-table* abspath))
		       (armed #f))
	       (mutex-unlock! *service-mutex*)
	       (cond
		  ((hop-service? svc)
		   (set! service svc)
		   (with-access::hop-service svc (ttl path id wid)
		      (cond
			 ((=fx ttl 1)
			  (unregister-service! svc))
			 ((>fx ttl 1)
			  (set! ttl (-fx ttl 1))))
		      (cond
			 ((service-expired? svc)
			  (mark-service-path-expired! path)
			  (http-invalidated-service-error req))
			 ((or (authorized-service? req wid)
			      (authorized-service? req id))
			  (scheme->response (service-handler svc req) req))
			 (else
			  (user-service-denied req user id)))))
		  (else
		   (let ((ini (hop-initial-weblet)))
		      (cond
			 ((and (string? ini)
			       (substring-at? abspath (hop-service-base) 0)
			       (let ((l1 (string-length abspath))
				     (l2 (string-length (hop-service-base))))
				  (or (=fx l1 l2)
				      (and (=fx l1 (+fx l2 1))
					   (char=? (string-ref abspath l2)
						   #\/)))))
			  (set! abspath
				(string-append (hop-service-base) "/" ini))
			  ;; resume the hop loop in order to autoload
			  ;; the initial weblet
			  'hop-resume)
			 (armed
			  (http-service-not-found abspath))
			 ((autoload-filter req)
			  (mutex-lock! *service-mutex*)
			  (loop (hashtable-get *service-table* abspath) #t))
			 (else
			  (http-service-not-found abspath)))))))))))

;*---------------------------------------------------------------------*/
;*    register-service! ...                                            */
;*---------------------------------------------------------------------*/
(define (register-service! svc)
   (with-access::hop-service svc (path id)
      (let ((sz (hashtable-size *service-table*)))
	 (hop-verb 4 (hop-color 1 "" "REG. SERVICE ")
		   "(" (/fx sz 2) "): "
		   svc " " path "\n")
	 (mutex-lock! *service-mutex*)
	 ;; CARE: for security matters, service re-definition should probably
	 ;; be for
	 (when (hashtable-get *service-table* path)
	    (cond
	       ((>fx (bigloo-debug) 0)
		(warning 'register-service! "Service re-defined -- " id))
	       ((>fx (hop-security) 0)
		(error id
		       "Service re-definition not permitted"
		       "use `-g' or `-s0' options to enable re-definitions"))))
	 (hashtable-put! *service-table* path svc)
	 (unless (char=? #\/ (string-ref path (-fx (string-length path) 1)))
	    (hashtable-put! *service-table* (string-append path "/") svc))
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
   (with-access::hop-service (procedure-attr svc) (resource)
      (if (string? file)
	  (string-append resource "/" file)
	  resource)))

;*---------------------------------------------------------------------*/
;*    service-path ...                                                 */
;*---------------------------------------------------------------------*/
(define (service-path svc)
   (with-access::hop-service (procedure-attr svc) (path)
      path))
   
;*---------------------------------------------------------------------*/
;*    service-base-url ...                                             */
;*---------------------------------------------------------------------*/
(define (service-base-url svc req)
   (with-access::http-request req (scheme host port)
      (let ((path (service-resource svc)))
	 (format (if (and (>fx (string-length path) 0)
			  (not (char=? (string-ref path 0) #\/)))
		     "~a://~a:~a/~a/"
		     "~a://~a:~a~a/")
		 (if (eq? scheme '*) "http" scheme) host port
		 path))))

;*---------------------------------------------------------------------*/
;*    *etc-table*                                                      */
;*---------------------------------------------------------------------*/
(define *etc-table*
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    service-etc-path-table-fill! ...                                 */
;*---------------------------------------------------------------------*/
(define (service-etc-path-table-fill! file)
   (with-lock *service-etc-table-mutex*
      (lambda ()
	 (let ((etc (make-file-name (dirname file) "etc"))
	       (svc (string->symbol (prefix (basename file)))))
	    (when (directory? etc)
	       (let loop ((dir (file-name-unix-canonicalize etc)))
		  (for-each (lambda (f)
			       (let ((path (make-file-name dir f)))
				  (hashtable-put! *etc-table* path svc)
				  (when (directory? path) (loop path))))
			    (directory->list dir))))))))

;*---------------------------------------------------------------------*/
;*    etc-path->service ...                                            */
;*---------------------------------------------------------------------*/
(define (etc-path->service path)
   (hashtable-get *etc-table* path))

