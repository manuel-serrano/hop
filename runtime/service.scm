;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/service.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:29:08 2006                          */
;*    Last change :  Sun Jan 29 08:14:17 2006 (serrano)                */
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
	    __hop_builtin
	    __hop_cgi)
   
   (export  (get-service-url::bstring)
	    (hop-service ::hop-request-filter ::http-request)
	    (hop-filter-path?::bool ::bstring ::bstring)
	    (make-hop-service-url::bstring ::hop-request-service . o)
	    (make-service-url::bstring ::hop-request-service . o)
	    (hop-request-service-name::bstring ::http-request)
	    (procedure->service::hop-request-service ::procedure))
   
   (eval    (export-exports)))

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
	 (format "~a-~a" *service-table-count* (hop-session)))))

;*---------------------------------------------------------------------*/
;*    hop-filter-path? ...                                             */
;*---------------------------------------------------------------------*/
(define (hop-filter-path? path filter)
   (let ((l1 (string-length (hop-filter-base)))
	 (l2 (string-length path))
	 (l3 (string-length filter)))
      (and (or (=fx l2 (+fx 1 (+fx l1 l3)))
	       (and (>fx l2 (+fx 1 (+fx l1 l3)))
		    (char=? (string-ref path (+fx (+fx 1 l1) l3)) #\/)))
	   (and (substring-at? path (hop-filter-base) 0)
		(char=? (string-ref path l1) #\/)
		(substring-at? path filter (+fx 1 l1))))))

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
   (with-access::hop-request-service svc (path args)
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
   (with-access::hop-request-service svc (path args)
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
;*    hop-service ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop-service filter req)
   (with-access::http-request req (method path)
      (with-access::hop-request-filter filter (table)
	 (let* ((k (hop-request-service-name req))
		(e (hashtable-get table k)))
	    (when (hop-request-service? e)
	       (with-access::hop-request-service e (id %exec)
		  (%exec req)))))))

;*---------------------------------------------------------------------*/
;*    procedure->service ...                                           */
;*---------------------------------------------------------------------*/
(define (procedure->service::hop-request-service proc::procedure)
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

