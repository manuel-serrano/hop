;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/http-lib.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  6 09:04:30 2004                          */
;*    Last change :  Mon May 26 05:42:03 2008 (serrano)                */
;*    Copyright   :  2004-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple HTTP lib                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-lib

   (library web)
   
   (include "http-lib.sch")
   
   (import  __hop_param
	    __hop_types)
   
   (export  (http-parse-error-message ::obj ::input-port)
	    (http-header-field ::pair-nil ::keyword)
	    (http-header-field-values::pair-nil ::bstring)
	    (http-cookie-get ::http-request ::bstring #!optional path domain)
	    (http-basic-authentication? ::http-request ::bstring ::bstring)
	    (http-basic-base64-authentication? ::http-request ::pair-nil)
	    (http-basic-authorization::bstring ::bstring ::bstring)
	    (http-htaccess-authentication? ::http-request ::bstring)
	    (http-decode-authentication::bstring ::bstring)
	    (http-write-header ::output-port ::pair-nil)
	    (http-filter-proxy-header::pair-nil ::pair-nil)))
	   
;*---------------------------------------------------------------------*/
;*    parse-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-error proc msg obj)
   (raise (instantiate::&io-parse-error
	     (obj obj)
	     (proc proc)
	     (msg msg))))

;*---------------------------------------------------------------------*/
;*    http-parse-error-message ...                                     */
;*---------------------------------------------------------------------*/
(define (http-parse-error-message c port)
   (if (char? c)
       (let ((line (http-read-line port)))
	  (string-for-read
	   (string-append "{" (string c) "}" (if (string? line) line ""))))
       c))

;*---------------------------------------------------------------------*/
;*    http-header-field ...                                            */
;*---------------------------------------------------------------------*/
(define (http-header-field header kwd)
   (let loop ((hs header))
      (cond
	 ((null? hs)
	  #f)
	 ((not (pair? (car hs)))
	  (loop (cdr hs)))
	 ((eq? (caar hs) kwd)
	  (cdar hs))
	 (else
	  (loop (cdr hs))))))

;*---------------------------------------------------------------------*/
;*    http-header-field-values ...                                     */
;*---------------------------------------------------------------------*/
(define (http-header-field-values str)
   (define key-gram
      (regular-grammar ()
	 ((: (out ",; \t") (+ (out #\= #\")) #\=)
	  (let* ((key (the-substring 0 (-fx (the-length) 1)))
		 (val (read/rp val-gram (the-port))))
	     (cons (cons key val) (ignore))))
	 ((: #\" (+ (out #\=)) #\" #\=)
	  (let* ((key (the-substring 1 (-fx (the-length) 2)))
		 (val (read/rp val-gram (the-port))))
	     (cons (cons key val) (ignore))))
	 ((+ (in ",; \t"))
	  (ignore))
	 (else
	  (let ((c (the-failure)))
	     (if (eof-object? c)
		 '()
		 (parse-error 'http-header-field-values
			      "Illegal field value"
			      (http-parse-error-message c (the-port))))))))
   (define val-gram
      (regular-grammar ()
	 ((: #\" (* (or (out #\") (: #\\ #\"))) #\")
	  (the-substring 1 (-fx (the-length) 1)))
	 ((: (+ (out " ,;\t")) (in ",;"))
	  (the-substring 0 (-fx (the-length) 1)))
	 ((: (+ (out " ,;\t")))
	  (the-string))))
   (let* ((p (open-input-string str))
	  (r (read/rp key-gram p)))
      (close-input-port p)
      r))

;*---------------------------------------------------------------------*/
;*    http-cookie-get ...                                              */
;*---------------------------------------------------------------------*/
(define (http-cookie-get req name #!optional path domain)
   (with-access::http-request req (header)
      (let ((cookie (http-header-field header cookie:)))
	 ;; konqueror uses the string "undefined" for no cookie!
	 (and (and (string? cookie) (not (substring-at? cookie "undefined" 0)))
	      (let* ((vals (http-header-field-values cookie))
		     (user (assoc name vals)))
		 (if (and (pair? user)
			  (or (not path)
			      (let ((dom (assoc '$Path vals)))
				 (and (pair? dom)
				      (string=? (cdr dom) path))))
			  (or (not domain)
			      (let ((dom (assoc '$Domain vals)))
				 (and (pair? dom)
				      (string=? (cdr dom) domain)))))
		     (cdr user)
		     #f))))))

;*---------------------------------------------------------------------*/
;*    password-grammar ...                                             */
;*---------------------------------------------------------------------*/
(define password-grammar
   (regular-grammar ()
      ((+ (out ": \t\r\n"))
       (the-string))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    basic-authentication-grammar ...                                 */
;*---------------------------------------------------------------------*/
(define basic-authentication-grammar
   (regular-grammar ((SP #\Space))
      ((: (* SP) "Basic" (* SP))
       (read/rp password-grammar (the-port)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    http-basic-base64-authentication? ...                            */
;*---------------------------------------------------------------------*/
(define (http-basic-base64-authentication? req b64-lst)
   (with-access::http-request req (header)
      (let ((c (http-header-field header authorization:)))
	 (and (string? c)
	      (let* ((p (open-input-string c))
		     (s (read/rp basic-authentication-grammar p)))
		 (close-input-port p)
		 (member s b64-lst))))))
   
;*---------------------------------------------------------------------*/
;*    http-basic-authentication? ...                                   */
;*---------------------------------------------------------------------*/
(define (http-basic-authentication? req user password)
   (http-basic-base64-authentication?
    req
    (list (base64-encode (string-append user ":" password)))))

;*---------------------------------------------------------------------*/
;*    http-basic-authorization ...                                     */
;*---------------------------------------------------------------------*/
(define (http-basic-authorization name passwd)
   (string-append "Basic " (base64-encode (string-append name ":" passwd))))

;*---------------------------------------------------------------------*/
;*    http-decode-authentication ...                                   */
;*---------------------------------------------------------------------*/
(define (http-decode-authentication auth)
   (define authentication-grammar
      (regular-grammar ((SP #\Space))
	 ((: (* SP) "Basic" (+ SP))
	  (base64-decode (read/rp password-grammar (the-port))))
	 (else
	  auth)))
   (let* ((p (open-input-string auth))
	  (s (read/rp authentication-grammar p)))
      (close-input-port p)
      s))
   
;*---------------------------------------------------------------------*/
;*    http-htaccess-authentication? ...                                */
;*---------------------------------------------------------------------*/
(define (http-htaccess-authentication? req file)
   (with-access::http-request req (header)
      (let ((c (http-header-field header authorization:)))
	 (and (string? c)
	      (let* ((p (open-input-string c))
		     (s (read/rp basic-authentication-grammar p)))
		 (close-input-port p)
		 (let ((pf (open-input-file file)))
		    (if (not (input-port? pf))
			#f
			(unwind-protect
			   (let loop ((user (read pf)))
			      (if (eof-object? user)
				  #f
				  (let ((ps (format "~s~a" user
						    (base64-decode
						     (symbol->string
						      (read pf))))))
				     (or (string=? (base64-encode ps) s)
					 (loop (read pf))))))
			   (close-input-port pf)))))))))

;*---------------------------------------------------------------------*/
;*    http-debug-read-line ...                                         */
;*---------------------------------------------------------------------*/
(define (http-debug-read-line p)
   (read/rp (regular-grammar ()
	       ((or (: (+ all) "\r\n") (: (+ all) "\n") (+ all))
		(the-string))
	       (else
		(let ((c (the-failure)))
		   (if (eof-object? c)
		       c
		       (the-string)))))
	    p))

;*---------------------------------------------------------------------*/
;*    http-write-header ...                                            */
;*---------------------------------------------------------------------*/
(define (http-write-header p header)
   (with-trace 4 'http-write-header
      (for-each (lambda (h)
		   (if (pair? h)
		       (let ((k (car h))
			     (v (cdr h)))
			  (if (keyword? k)
			      (begin
				 (trace-item (keyword->string! k) ": " v)
				 (http-write-line p (keyword->string! k) ": " v))
			      (error 'http-write-header "Illegal header" h)))
		       (http-write-line p h)))
		header)
      (flush-output-port p)))

;*---------------------------------------------------------------------*/
;*    http-filter-proxy-header ...                                     */
;*    -------------------------------------------------------------    */
;*    According to RFC 2616, Section 14.10 Connection, proxies must    */
;*    filter out the "connection:" header fields. This is the purpose  */
;*    of this function.                                                */
;*---------------------------------------------------------------------*/
(define (http-filter-proxy-header header)
   (with-trace 5 'http-filter-proxy-header
      (let loop ((lst header)
		 (fil '()))
	 (cond
	    ((null? lst)
	     (if (null? fil)
		 header
		 (filter (lambda (c)
			    (or (not (pair? c))
				(not (memq (car c) fil))))
			 header)))
	    ((not (pair? (car lst)))
	     (loop (cdr lst) fil))
	    ((eq? (caar lst) proxy-connection:)
	     (loop (cdr lst) (cons (caar lst) fil)))
	    ((eq? (caar lst) connection:)
	     (loop (cdr lst) (cons (caar lst) fil)))
	    ((not (eq? (caar lst) content:))
	     (loop (cdr lst) fil))
	    (else
	     (if (string-ci=? (cdar lst) "close")
		 (loop (cdr lst) fil)
		 (loop (cdr lst) (cons (cdar lst) fil))))))))
		 
