;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/cgi.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 16 11:17:40 2003                          */
;*    Last change :  Sat Jan 10 19:18:40 2015 (serrano)                */
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
   
   (export  (http-request-cgi-args ::http-request ::procedure ::procedure ::obj)
	    (cgi-arg::obj ::bstring ::pair-nil)
	    (serialized-cgi-arg ::bstring ::pair-nil ::obj)))

;*---------------------------------------------------------------------*/
;*    http-request-cgi-args ...                                        */
;*---------------------------------------------------------------------*/
(define (http-request-cgi-args req::http-request unjson::procedure stringify::procedure extension)
 
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

   (define (cgi-arg-multipart-value! v)
      (let ((header (memq :header v))
	    (val (cadr (memq :data v))))
	 (set-cdr! v
	    (if (not header)
		val
		(let ((enc (cadr (memq :hop-encoding (cadr header)))))
		   (cond
		      ((string=? enc "string") (stringify val))
		      ((string=? enc "integer") (string->integer val))
		      ((string=? enc "keyword") (string->keyword val))
		      (else (string->obj val extension))))))
	 v))
   
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
			  (cons '("hop-encoding" . "hop-multipart")
			     (map! cgi-arg-multipart-value!
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
