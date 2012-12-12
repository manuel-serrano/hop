;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/service.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:29:08 2006                          */
;*    Last change :  Sun Dec  9 00:57:59 2012 (serrano)                */
;*    Copyright   :  2006-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP services                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_service
   
   (include "service.sch"
            "verbose.sch")
   
   (library web)
   
   (import  __hop_configure
	    __hop_param
	    __hop_types
	    __hop_misc
	    __hop_read
	    __hop_http-error
	    __hop_http-response
	    __hop_cgi
	    __hop_xml-types
	    __hop_xml
	    __hop_html-base
	    __hop_html-head
	    __hop_preferences
	    __hop_xdomain
	    __hop_js-comp
	    __hop_user
	    __hop_weblets
	    __hop_hop)
   
   (export  (init-hop-services!)
	    (inline service?::bool ::obj)
	    (service-exists? ::bstring)
	    (get-all-services ::http-request)
	    (gen-service-url::bstring #!key (prefix "") (public #f))
	    (hop-service-path? ::bstring)
	    (hop-apply-nice-url::bstring ::bstring ::pair-nil)
	    (hop-apply-url::bstring ::bstring ::pair-nil)
	    (service-funcall-url::bstring ::hop-service . o)
	    (hop-request-service-name::bstring ::http-request)
	    (procedure->service::procedure ::procedure)
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
   (and (procedure? obj) (isa? (procedure-attr obj) hop-service)))

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
;*    *service-source-table* ...                                       */
;*---------------------------------------------------------------------*/
(define *service-source-table*
   (make-hashtable 4))

;*---------------------------------------------------------------------*/
;*    get-all-services ...                                             */
;*---------------------------------------------------------------------*/
(define (get-all-services req)
   (synchronize *service-table-mutex*
      (delete-duplicates!
	 (filter (lambda (svc)
		    (with-access::hop-service svc (id wid)
		       (or (authorized-service? req wid)
			   (authorized-service? req id))))
	    (hashtable->list *service-table*)))))

;*---------------------------------------------------------------------*/
;*    init-hop-services! ...                                           */
;*    -------------------------------------------------------------    */
;*    Create the first HOP services.                                   */
;*---------------------------------------------------------------------*/
(define (init-hop-services!)
   (init-hop-prefs-services!)
   (init-hop-xdomain-service!))

;*---------------------------------------------------------------------*/
;*    gen-service-url ...                                              */
;*---------------------------------------------------------------------*/
(define (gen-service-url #!key (prefix "") (public #f))
   (synchronize *service-table-mutex*
      (set! *service-table-count* (+fx 1 *service-table-count*))
      (format (if public "public/~a/~a~a" "~a/~a~a")
	 (hop-service-weblet-name)
	 prefix
	 *service-table-count*)))

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
   (with-access::http-request req (path)
      (let ((len (string-length path)))
	 (let loop ((i 1))
	    (cond
	       ((=fx i len)
		path)
	       ((char=? (string-ref path i) #\?)
		(substring path 0 i))
	       (else
		(loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    hop-apply-nice-url ...                                           */
;*    -------------------------------------------------------------    */
;*    This is used for fix arity function in order to build nice URLS. */
;*    When at least one argument is not a string, it falls back to     */
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
		  (strs '()))
	  (if (null? vals)
	      (apply string-append base strs)
	      (loop (cddr vals)
		    (let ((str (string-append
				(if (pair? (cddr vals)) "&" "?")
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
   (if (not (isa? svc hop-service))
       (bigloo-type-error 'service-funcall-url 'service svc)
       (with-access::hop-service svc (path)
	  (hop-apply-url path vals))))

;*---------------------------------------------------------------------*/
;*    service-handler ...                                              */
;*---------------------------------------------------------------------*/
(define (service-handler svc req)
   
   (define (invoke proc vals)
      (with-access::hop-service svc (id)
	 (hop-verb 2 (hop-color req req " INVOKE.svc")
	    " "
	    (with-output-to-string
	       (lambda ()
		  (write-circle (cons id vals))))
	    "\n")
	 (cond
	    ((not vals)
	     (error id
		"Illegal service arguments encoding"
		`(,id ,vals)))
	    ((correct-arity? proc (length vals))
	     (apply proc vals))
	    (else
	     (error id
		"Wrong number of arguments"
		`(,id ,@vals))))))
   
   (let ((ca (http-request-cgi-args req)))
      (with-access::hop-service svc (proc id)
	 (cond
	    ((null? (cdr ca))
	     (invoke proc '()))
	    ((equal? (cgi-arg "hop-encoding" ca) "hop")
	     (with-access::http-request req (charset)
		(set! charset 'UTF-8))
	     (let ((vals (serialized-cgi-arg "vals" ca)))
		(if (or (null? vals) (pair? vals))
		    (invoke proc vals)
		    (error id "Illegal arguments" vals))))
	    (else
	     (invoke proc
		(append-map (lambda (p)
			       (list (string->keyword (car p)) (cdr p)))
		   (cdr ca))))))))

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
      ((isa? exp xml-tilde)
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
;*    service-exists? ...                                              */
;*---------------------------------------------------------------------*/
(define (service-exists? svc)
   (let* ((abspath (string-append (hop-service-base) "/" svc))
	  (loaded (synchronize *service-mutex*
		     (hashtable-get *service-table* abspath))))
      (or loaded
	  (let ((creq (current-request))
		(th (current-thread)))
	     (unwind-protect
		(let ((req (instantiate::http-server-request
			      (user (anonymous-user))
			      (localclientp #t)
			      (lanclientp #t)
			      (abspath abspath)
			      (method 'GET))))
		   (current-request-set! th req)
		   (autoload-filter req))
		(current-request-set! th creq))))))
   
;*---------------------------------------------------------------------*/
;*    service-filter ...                                               */
;*---------------------------------------------------------------------*/
(define (service-filter req)
   (when (isa? req http-server-request)
      (with-access::http-server-request req (abspath user service method)
	 (when (hop-service-path? abspath)
	    (let loop ((svc (synchronize *service-mutex*
			       (hashtable-get *service-table* abspath))))
	       (cond
		  ((isa? svc hop-service)
		   (when (hop-force-reload-service)
		      (set! svc (force-reload-service svc)))
		   (set! service svc)
		   (with-access::hop-service svc (ttl path id wid)
		      (cond
			 ((service-expired? svc)
			  (mark-service-path-expired! path)
			  (http-invalidated-service-error req))
			 ((or (authorized-service? req wid)
			      (authorized-service? req id))
			  (if (>fx ttl 0)
			      (unwind-protect
				 (scheme->response (service-handler svc req) req)
				 (if (=fx ttl 1)
				     (unregister-service! svc)
				     (set! ttl (-fx ttl 1))))
			      (scheme->response (service-handler svc req) req)))
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
			 ((autoload-filter req)
			  =>
			  (lambda (o)
			     (if (eq? o #t)
				 (let ((s (synchronize *service-mutex*
					     (hashtable-get *service-table* abspath))))
				    (if (not s)
					(http-service-not-found abspath)
					(loop s)))
				 (http-error o))))
			 (else
			  (http-service-not-found abspath)))))))))))

;*---------------------------------------------------------------------*/
;*    force-reload-service ...                                         */
;*---------------------------------------------------------------------*/
(define (force-reload-service svc)
   (with-access::hop-service svc (resource source path)
      (if (and (string? resource) (string? source))
	  (let ((file (make-file-path resource source)))
	     (if (file-exists? file)
		 (let ((ct (file-modification-time file)))
		    (or (synchronize *service-mutex*
			   (let ((ot (hashtable-get *service-source-table* file)))
			      (cond
				 ((not ot)
				  (hashtable-put! *service-source-table* file ct)
				  svc)
				 ((=elong ct ot)
				  svc)
				 (else
				  (hashtable-put! *service-source-table* file ct)
				  #f))))
			(begin
			   (hop-load file)
			   (synchronize *service-mutex*
			      (hashtable-get *service-table* path)))))
		 svc))
	  svc)))

;*---------------------------------------------------------------------*/
;*    register-service! ...                                            */
;*---------------------------------------------------------------------*/
(define (register-service! svc)
   (with-access::hop-service svc (path id)
      (let ((sz (hashtable-size *service-table*)))
	 (hop-verb 4 (hop-color 1 "" "REG. SERVICE ")
	    "(" (/fx sz 2) "): "
	    svc " " path "\n")
	 (synchronize *service-mutex*
	    ;; CARE: for security matters, service re-definition should probably
	    ;; be for
	    (when (hashtable-get *service-table* path)
	       (cond
		  ((not (hop-allow-redefine-service))
		   (mutex-unlock! *service-mutex*)
		   (error id
		      "Service re-definition not permitted"
		      "use `--devel' or `-s0' options to enable re-definitions"))
		  ((>fx (bigloo-debug) 0)
		   (warning 'register-service! "Service re-defined -- " id))))
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
	    svc))))

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
      (synchronize *service-mutex*
	 (hashtable-remove! *service-table* path)
	 (hashtable-remove! *service-table* (string-append path "/")))))

;*---------------------------------------------------------------------*/
;*    *expiration-table*                                               */
;*---------------------------------------------------------------------*/
(define *expiration-table*
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    expired-service-path? ...                                        */
;*---------------------------------------------------------------------*/
(define (expired-service-path? path)
   (synchronize *expiration-mutex*
      (hashtable-get *expiration-table* path)))

;*---------------------------------------------------------------------*/
;*    mark-service-path-expired! ...                                   */
;*---------------------------------------------------------------------*/
(define (mark-service-path-expired! path)
   (synchronize *expiration-mutex*
      (hashtable-put! *expiration-table* path #t)
      #f))

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
   (synchronize *service-etc-table-mutex*
      (let ((etc (make-file-name (dirname file) "etc"))
	    (svc (string->symbol (prefix (basename file)))))
	 (when (directory? etc)
	    (let loop ((dir (file-name-unix-canonicalize etc)))
	       (for-each (lambda (f)
			    (let ((path (make-file-name dir f)))
			       (hashtable-put! *etc-table* path svc)
			       (when (directory? path) (loop path))))
		  (directory->list dir)))))))

;*---------------------------------------------------------------------*/
;*    etc-path->service ...                                            */
;*---------------------------------------------------------------------*/
(define (etc-path->service path)
   (hashtable-get *etc-table* path))

