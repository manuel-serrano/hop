;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/cgi.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 16 11:17:40 2003                          */
;*    Last change :  Fri Dec 17 07:35:45 2010 (serrano)                */
;*    Copyright   :  2003-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    CGI scripts handling                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_cgi

   (library web)

   (import  __hop_param
	    __hop_types
	    __hop_http-lib)
   
   (export  (http-request-cgi-args::pair-nil ::http-request)
	    (cgi-arg::obj ::bstring ::pair-nil)
	    (serialized-cgi-arg name args)))

;*---------------------------------------------------------------------*/
;*    http-request-cgi-args ...                                        */
;*---------------------------------------------------------------------*/
(define (http-request-cgi-args req::http-request)
   
   (define (cgi-args req)
      (with-access::http-request req (socket method path abspath query
					     header content-length)
	 (case method
	    ((POST)
	     (let* ((pi (socket-input socket))
		    (ctype (http-header-field header content-type:)))
		(if (and (string? ctype)
			 (substring-ci-at? ctype
					   "multipart/form-data; boundary="
					   0))
		    (let ((boundary (substring
				     ctype
				     (string-length
				      "multipart/form-data; boundary=")
				     (string-length ctype))))
		       (cons path
			     (with-handler
				(lambda (e)
				   (if (&io-parse-error? e)
				       '()
				       (raise e)))
				(cgi-multipart->list (hop-upload-directory)
						     pi
						     content-length
						     boundary))))
		    (let ((body (read-chars (elong->fixnum content-length) pi)))
		       (cons path (cgi-args->list body))))))
	    ((GET PUT)
	     (if (string? query)
		 (cons abspath (cgi-args->list query))
		 (cons abspath '())))
	    (else
	     (error "http-request-cgi-args" "Illegal HTTP method" method)))))
   
   (define (normalize l)
      (let loop ((l l)
		 (res '()))
	 (if (null? l)
	     res
	     (let ((a (assoc (caar l) res)))
		(if (pair? a)
		    ;; Already present in res. Add the current item in list.
		    (begin
		       (if (pair? (cdr a))
			   (append! (cdr a) (list (cdar l)))
			   (set-cdr! a (list (cdr a) (cdar l))))
		       (loop (cdr l) res))
		    ;; Not yet added.
		    (loop (cdr l) (cons (car l) res)))))))

   (with-trace 2 'http-request-cgi-args
      (trace-item "path=" (http-request-path req))
      (trace-item "abspath=" (string-for-read (http-request-abspath req)))
      (trace-item "query=" (if (string? (http-request-query req))
			       (string-for-read (http-request-query req))
			       "#f"))
      (let ((args (cgi-args req)))
	 (trace-item "args="
		     (map (lambda (a)
			     (cond
				((string? a)
				 (string-for-read a))
				((pair? a)
				 (cons (car a) (string-for-read (cdr a))))
				(else
				 a)))
			  args))
	 (cons (car args) (normalize (cdr args))))))

;*---------------------------------------------------------------------*/
;*    cgi-arg ...                                                      */
;*---------------------------------------------------------------------*/
(define (cgi-arg name args)
   (let ((c (assoc name (cdr args))))
      (if (pair? c)
	  (cdr c)
	  #f)))

;*---------------------------------------------------------------------*/
;*    serialized-cgi-arg ...                                           */
;*---------------------------------------------------------------------*/
(define (serialized-cgi-arg name args)
   (let ((c (assoc name (cdr args))))
      (if (pair? c)
	  (string->obj (cdr c))
	  #f)))
