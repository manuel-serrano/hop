;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/cgi.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 16 11:17:40 2003                          */
;*    Last change :  Thu Jun  8 10:03:22 2006 (serrano)                */
;*    Copyright   :  2003-06 Manuel Serrano                            */
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
   
   (export  (http-request-url-cgi-args::pair-nil ::bstring)
	    (http-request-cgi-args::pair-nil ::http-request)
	    (cgi-arg::obj ::bstring ::pair-nil)
	    (serialized-cgi-arg name args)))

;*---------------------------------------------------------------------*/
;*    http-request-url-cgi-args ...                                    */
;*---------------------------------------------------------------------*/
(define (http-request-url-cgi-args path)
   (let ((i (string-index path #\?)))
      (if (=fx i -1)
	  (list path)
	  ;; CARE: Je ne sais pas s'il faut toujours faire la conversion
	  ;; utf8->iso-latin!
	  (let ((cmd (utf8->iso-latin!
		      (xml-string-decode!
		       (substring path 0 i))))
		(args (substring path (+fx i 1) (string-length path))))
	     (cons cmd
		   (map! (lambda (p)
			    (set-cdr! p (utf8->iso-latin!
					 (xml-string-decode! (cdr p))))
			    p)
			 (cgi-args->list args)))))))

;*---------------------------------------------------------------------*/
;*    http-request-cgi-args ...                                        */
;*---------------------------------------------------------------------*/
(define (http-request-cgi-args req::http-request)
   (define (cgi-args req)
      (with-access::http-request req (socket method path encoded-path
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
			     (cgi-multipart->list
			      (hop-upload-directory)
			      pi
			      content-length
			      boundary)))
		    (let ((body (read-chars (elong->fixnum content-length) pi)))
		       (cons path
			     (cgi-args->list body))))))
	    ((GET PUT)
	     (http-request-url-cgi-args encoded-path))
	    (else
	     (error 'http-request-cgi-args "Not a cgi request" method)))))
   (define (normalize l)
      (let loop ((l l)
		 (res '()))
	 (if (null? l)
	     res
	     (let ((a (assoc (caar l) res)))
		(if (pair? a)
		    ;; Already present in res. Add the current item in list
		    (begin
		       (if (pair? (cdr a))
			   (append! (cdr a) (list (cdar l)))
			   (set-cdr! a (list (cdr a) (cdar l))))
		       (loop (cdr l) res))
		    ;; Not yet added.
		    (loop (cdr l)
			  (cons (car l) res)))))))
   
   (let ((args (cgi-args req)))
      (cons (car args) (normalize (cdr args)))))

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
