;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/service.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:29:08 2006                          */
;*    Last change :  Thu Mar 16 09:24:15 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HOP services                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_service

   (include "eval-macro.sch"
	    "service.sch")

   (library web)
   
   (import  __hop_param
	    __hop_types
	    __hop_misc
	    __hop_thread
	    __hop_http-error
	    __hop_http-response
	    __hop_cgi
	    __hop_xml
	    __hop_html-extra
	    __hop_js-lib)
   
   (static  (class %autoload
	       (path::bstring read-only)
	       (pred::procedure read-only)
	       (hooks::pair-nil read-only (default '()))
	       (mutex::mutex (default (make-mutex)))
	       (loaded::bool (default #f))))
   
   (export  (get-service-url::bstring)
	    (make-hop-service-url::bstring ::hop-service . o)
	    (make-service-url::bstring ::hop-service . o)
	    (hop-request-service-name::bstring ::http-request)
	    (procedure->service::hop-service ::procedure)
            (%eval::%http-response ::bstring ::procedure)
	    (autoload ::bstring ::procedure . hooks)
	    (autoload-filter ::http-request)
	    (service-filter ::http-request)
	    (register-service!::hop-service ::hop-service)))

;*---------------------------------------------------------------------*/
;*    mutexes ...                                                      */
;*---------------------------------------------------------------------*/
(define *service-table-mutex* (make-mutex "hop-service-table"))

;*---------------------------------------------------------------------*/
;*    *service-table-count* ...                                        */
;*---------------------------------------------------------------------*/
(define *service-table-count* 0)

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
   (let ((l1 (string-length (hop-service-base))))
      (and (substring-at? path (hop-service-base) 0)
	   (>fx (string-length path) l1)
	   (char=? (string-ref path l1) #\/))))
   
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
   (with-access::hop-service svc (path args)
      (if (null? args)
	  path
	  (apply string-append
		 path
		 "?hop-encoding=hop"
		 (map (lambda (f v)
			 (format "&~a=~a" f (cgi-url-encode (obj->string v))))
		      args vals)))))

;*---------------------------------------------------------------------*/
;*    make-service-url ...                                             */
;*---------------------------------------------------------------------*/
(define (make-service-url svc . vals)
   (with-access::hop-service svc (path args)
      (if (null? args)
	  path
	  (apply string-append
		 path
		 "?hop-encoding=none"
		 (map (lambda (f v)
			 (let ((a (if (string? v)
				      (cgi-url-encode v)
				      v)))
			    (format "&~a=~a" f a)))
		      args vals)))))

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
	  (service a (apply proc a))))))

;*---------------------------------------------------------------------*/
;*    %eval ...                                                        */
;*---------------------------------------------------------------------*/
(define (%eval exp cont)
   (let ((s (scheme->javascript
	     (procedure->service (lambda (res) (cont res))))))
      (instantiate::http-response-hop
	 (xml (<HTML>
		 (<HOP-HEAD>)
		 (<BODY>
		    (<SCRIPT>
		       (format "hop( ~a( eval( '~a' ) ), true )" s exp))))))))

;*---------------------------------------------------------------------*/
;*    *autoload-mutex* ...                                             */
;*---------------------------------------------------------------------*/
(define *autoload-mutex* (make-mutex))

;*---------------------------------------------------------------------*/
;*    *autoloads* ...                                                  */
;*---------------------------------------------------------------------*/
(define *autoloads* '())

;*---------------------------------------------------------------------*/
;*    autoload ...                                                     */
;*---------------------------------------------------------------------*/
(define (autoload file pred . hooks)
   (let ((qfile (find-file/path file (hop-path))))
      (if (not (and (string? qfile) (file-exists? qfile)))
	  (error 'autoload-add! "Can't find autoload file" file)
	  (let ((al (instantiate::%autoload
		       (path qfile)
		       (pred pred)
		       (hooks hooks))))
	     (mutex-lock! *autoload-mutex*)
	     (set! *autoloads* (cons al *autoloads*))
	     (mutex-unlock! *autoload-mutex*)))))

;*---------------------------------------------------------------------*/
;*    autoload-filter ...                                              */
;*    -------------------------------------------------------------    */
;*    Autoload has to be the first filter. When the autoload matches,  */
;*    it simply returns the request that is handled by the autoloaded  */
;*    file.                                                            */
;*---------------------------------------------------------------------*/
(define (autoload-filter req)
   (let loop ((al *autoloads*))
      (if (null? al)
	  req
	  (with-access::%autoload (car al) (pred path hooks loaded mutex)
	     (if (pred req)
		 (begin
		    (mutex-lock! mutex)
		    (unwind-protect
		       (unless loaded
			  (hop-verb 1 (hop-color req req " AUTOLOADING")
				    ": " path "\n")
			  ;; load the autoloaded file
			  (load-once path)
			  ;; execute the hooks
			  (for-each (lambda (h) (h req)) hooks)
			  ;; remove the autoaload (once loaded)
			  (mutex-lock! *autoload-mutex*)
			  (set! *autoloads* (remq! (car al) *autoloads*))
			  (mutex-unlock! *autoload-mutex*)
			  (set! loaded #t))
		       (mutex-unlock! mutex))
		    ;; re-scan the filter list
		    req)
		 (begin
		    (mutex-lock! *autoload-mutex*)
		    (let ((tail (cdr al)))
		       (mutex-unlock! *autoload-mutex*)
		       (loop tail))))))))

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
	 (if (=fx i l)
	     (bit-and r (-fx (bit-lsh 1 29) 1))
	     (let ((c (string-ref p i)))
		(if (char=? c #\?)
		    (bit-and r (-fx (bit-lsh 1 29) 1))
		    (loop (+fx i 1)
			  (+fx r (+fx (bit-lsh r 3) (char->integer c))))))))))

;*---------------------------------------------------------------------*/
;*    service-filter ...                                               */
;*---------------------------------------------------------------------*/
(define (service-filter req)
   (when (http-request-localhostp req)
      (with-access::http-request req (path)
	 (when (hop-service-path? path)
	    (mutex-lock! *service-mutex*)
	    (let ((svc (hashtable-get *service-table* path)))
	       (mutex-unlock! *service-mutex*)
	       (when (hop-service? svc)
		  (with-access::hop-service svc (id %exec)
		     (scheme->response (%exec req) req))))))))

;*---------------------------------------------------------------------*/
;*    register-service! ...                                            */
;*---------------------------------------------------------------------*/
(define (register-service! svc)
   (with-access::hop-service svc (path)
      (hop-verb 2 (hop-color 1 1 " REG. SERVICE: ") svc " " path "\n")
      (mutex-lock! *service-mutex*)
      (hashtable-put! *service-table* path svc)
      (let ((l (string-length path)))
	 (let loop ((i (+fx (string-length (hop-service-base)) 1)))
	    (cond
	       ((>=fx i l)
		(hop-weblets-set! (cons svc (hop-weblets))))
	       ((char=? (string-ref path i) #\/)
		#unspecified)
	       (else
		(loop (+fx i 1))))))
      (mutex-unlock! *service-mutex*)
      svc))
			       
