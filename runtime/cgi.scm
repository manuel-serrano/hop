;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/cgi.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 16 11:17:40 2003                          */
;*    Last change :  Tue Jan  6 09:30:24 2015 (serrano)                */
;*    Copyright   :  2003-15 Manuel Serrano                            */
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
   
   (export  (http-request-cgi-args ::http-request ::procedure)
	    (cgi-arg::obj ::bstring ::pair-nil)
	    (serialized-cgi-arg ::bstring ::pair-nil ::obj)))

;*---------------------------------------------------------------------*/
;*    http-request-cgi-args ...                                        */
;*---------------------------------------------------------------------*/
(define (http-request-cgi-args req::http-request unjson::procedure)
 
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
		       (values path
			  (with-handler
			     (lambda (e)
				(if (isa? e &io-parse-error)
				    '()
				    (raise e)))
			     (map! (lambda (v)
				      (set-cdr! v (cadr (memq :data v)))
				      v)
				(cgi-multipart->list (hop-upload-directory)
				   pi
				   content-length
				   boundary)))))
		    (values path
		       (list (cons "hop-encoding" "json")
			  (cons "json" (list (unjson pi))))))))
	    ((GET PUT)
	     (if (string? query)
		 (values abspath (normalize (cgi-args->list query)))
		 (values abspath '())))
	    (else
	     (error "http-request-cgi-args" "Illegal HTTP method" method)))))
   
   (with-trace 2 'http-request-cgi-args
      (with-access::http-request req (path abspath query method)
	 (trace-item "path=" path)
	 (trace-item "abspath=" (string-for-read abspath))
	 (trace-item "method=" method)
	 (trace-item "query=" (if (string? query)
				  (string-for-read query)
				  "#f")))
      (cgi-args req)))

;*---------------------------------------------------------------------*/
;*    cgi-arg ...                                                      */
;*---------------------------------------------------------------------*/
(define (cgi-arg name args)
   (let ((c (assoc name args)))
      (if (pair? c)
	  (cdr c)
	  #f)))

;*---------------------------------------------------------------------*/
;*    serialized-cgi-arg ...                                           */
;*---------------------------------------------------------------------*/
(define (serialized-cgi-arg name args extension)
   (let ((c (assoc name args)))
      (if (pair? c)
	  (string->obj (cdr c) extension)
	  #f)))
