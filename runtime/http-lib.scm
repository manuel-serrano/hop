;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-lib.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  6 09:04:30 2004                          */
;*    Last change :  Sat Mar 18 10:09:47 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple HTTP lib                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_http-lib

   (include "http-lib.sch")
   
   (import  __hop_param
	    __hop_types)
   
   (export  (http-parse-error-message ::obj ::input-port)
	    (http-read-crlf ::input-port)
	    (http-read-line ::input-port)
	    (http-read-header::pair-nil ::input-port)
	    (http-header-field ::pair-nil ::keyword)
	    (http-header-field-values::pair-nil ::bstring)
	    (http-cookie-get ::http-request ::bstring #!optional path domain)
	    (http-basic-authentication? ::http-request ::bstring ::bstring)
	    (http-basic-base64-authentication? ::http-request ::pair-nil)
	    (http-htaccess-authentication? ::http-request ::bstring)
	    (http-parse-status-line ::input-port)
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
       (string-for-read
	(string-append "{" (string c) "}" (http-read-line port)))
       c))

;*---------------------------------------------------------------------*/
;*    http-read-crlf ...                                               */
;*---------------------------------------------------------------------*/
(define (http-read-crlf p)
   (define http-crlf-grammar
      (regular-grammar ((CRLF "\r\n"))
	 (CRLF
	  "\r\n")
	 (else
	  (parse-error 'crlf-grammar
		       "Illegal character"
		       (http-parse-error-message (the-failure) (the-port))))))
   (read/rp http-crlf-grammar p))

;*---------------------------------------------------------------------*/
;*    http-read-line ...                                               */
;*---------------------------------------------------------------------*/
(define (http-read-line p)
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
;*    http-read-header ...                                             */
;*---------------------------------------------------------------------*/
(define (http-read-header p)
   (define value-grammar
      (regular-grammar ()
	 ((+ (in " \t"))
	  (ignore))
	 ((: (out " \t\r\n") (* (out "\r\n")) "\r\n")
	  (the-substring 0 (-fx (the-length) 2)))
	 ((: (out " \t\r\n") (* (out "\r\n")) "\n")
	  (the-substring 0 (-fx (the-length) 1)))
	 ((: (? #\Return) #\Newline)
	  "")
	 (else
	  (let ((c (the-failure)))
	     (if (eof-object? c)
		 '()
		 c)))))
   (define blank-grammar
      (regular-grammar ()
	 ((+ (in " \t")) (ignore))))
   (define hostname-grammar
      (regular-grammar ()
	 ((: (+ (out ":\n\r\t ")) #\:)
	  (let* ((h (the-substring 0 -1))
		 (p (read/rp fixnum-grammar (the-port))))
	     (values h p)))
	 ((+ (out ":\n\r\t "))
	  (values (the-string) #f))
	 ((+ (in " \t"))
	  (ignore))))
   (define name-grammar
      (regular-grammar ()
	 ((+ (out "\n\r\t ")) (the-string))
	 ((+ (in " \t")) (ignore))))
   (define fixnum-grammar
      (regular-grammar ((DIGIT (in ("09"))))
	 ((+ DIGIT) (the-fixnum))
	 ((+ (in " \t")) (ignore))))
   (define elong-grammar
      (regular-grammar ((DIGIT (in ("09"))))
	 ((+ DIGIT) (fixnum->elong (the-fixnum)))
	 ((+ (in " \t")) (ignore))))
   (define symbol-grammar
      (regular-grammar ()
	 ((+ alpha) (the-downcase-symbol))
	 ((+ (in " \t")) (ignore))))
   (define auth-grammar
      (regular-grammar ()
	 ((: (+ (in #\Space #\Tab)))
	  (ignore))
	 ((: (out #\Space #\Tab) (* (out "\n\r")))
	  (the-string))
	 (else
	  #f)))
   (define crlf-grammar
      (regular-grammar ()
	 ((: (* (in #\space #\tab)) (? #\Return) #\Newline)
	  #unspecified)
	 (else
	  #f)))
   (define header-grammar
      (regular-grammar (header
			hostname port content-length transfer-encoding
			authorization proxy-authorization)
	 ((: (+ (or (out " :\r\n\t") (: #\space (out #\:)))) #\:)
	  (let* ((k (the-downcase-keyword)))
	     (case k
		((host:)
		 (multiple-value-bind (h p)
		    (read/rp hostname-grammar (the-port))
		    (set! hostname h)
		    (set! port p)
		    (trace-item "##1 " k " [" hostname ":" port "]")
		    (read/rp crlf-grammar (the-port))
		    (set! header (cons (cons k hostname) header))
		    (ignore)))
		((content-length:)
		 (set! content-length (read/rp elong-grammar (the-port)))
		 (trace-item "##2 " k " [" content-length "]")
		 (read/rp crlf-grammar (the-port))
		 (set! header (cons (cons k content-length) header))
		 (ignore))
		((transfer-encoding:)
		 (set! transfer-encoding (read/rp symbol-grammar (the-port)))
		 (trace-item "##3 " k " [" transfer-encoding "]")
		 (read/rp crlf-grammar (the-port))
		 (set! header (cons (cons k transfer-encoding) header))
		 (ignore))
		((authorization:)
		 (set! authorization (read/rp auth-grammar (the-port)))
		 (trace-item "##4 " k " [" authorization "]")
		 (read/rp crlf-grammar (the-port))
		 (set! header (cons (cons k authorization) header))
		 (ignore))
		((proxy-authorization:)
		 (set! proxy-authorization (read/rp auth-grammar (the-port)))
		 (trace-item "##5 " k " [" proxy-authorization "]")
		 (read/rp crlf-grammar (the-port))
		 ;; don't store the proxy-authorization in the header
		 (ignore))
		(else
		 (let ((v (read/rp value-grammar (the-port))))
		    (trace-item "##6 " k " [" v "]")
		    (set! header (cons (cons k v) header))
		    (ignore))))))
	 ((: (* (in #\space #\tab)) (? #\Return) #\Newline)
	  (values (reverse! header)
		  hostname
		  port
		  content-length
		  transfer-encoding
		  authorization
		  proxy-authorization))
	 (else
	  (let ((c (the-failure)))
	     (if (eof-object? c)
		 ;; some (bugous?) HTTP server don't send the appropriate
		 ;; CRLF when the body is empty
		 (values (reverse! header)
			 hostname
			 port
			 content-length
			 transfer-encoding
			 authorization
			 proxy-authorization)
		 (parse-error 'http-read-header
				 "Illegal characters"
				 (http-parse-error-message
				  (the-failure)
				  (the-port))))))))
   (with-trace 5 'http-read-header
      (read/rp header-grammar p
	       '()   ;; header
	       #f    ;; hostname
	       #f    ;; port
	       #e-1  ;; content-length
	       #f    ;; transfer-encoding
	       #f    ;; authorization
	       #f))) ;; proxy-authorization
	       
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
;*    http-parse-status-line ...                                       */
;*    -------------------------------------------------------------    */
;*    The syntax of the status (section 6.1 of http/1.1) is defined    */
;*    as follow:                                                       */
;*    Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF  */
;*---------------------------------------------------------------------*/
(define (http-parse-status-line ip)
   (if (>=fx (bigloo-debug) 3)
       (let ((line (http-read-line ip)))
	  (if (string? line)
	      (with-trace 5 'http-parse-status-line
		 (trace-item "status-line: " (string-for-read line))
		 (let* ((ip (open-input-string line)))
		    (let ((r (read/rp status-line-grammar ip)))
		       (close-input-port ip)
		       r)))
	      (with-trace 5 'http-parse-status-line
		 (trace-item "error in status-line: " line)
		 (let* ((ip (open-input-string "")))
		    (let ((r (read/rp status-line-grammar ip)))
		       (close-input-port ip)
		       r)))))
       (read/rp status-line-grammar ip)))

;*---------------------------------------------------------------------*/
;*    status-line-grammar ...                                          */
;*---------------------------------------------------------------------*/
(define status-line-grammar
   (regular-grammar ((SP #\Space)
		     (HTTP (: (+ (in "httpsHTTPS"))
			      #\/ (+ digit) #\. (+ digit)))
		     (CRLF "\r\n"))
      ((: HTTP SP)
       (let* ((http (the-substring 0 (-fx (the-length) 1)))
	      (code (read/rp status-code-grammar (the-port)))
	      (phrase (http-read-line (the-port))))
	  (values http code phrase)))
      (else
       (let ((c (the-failure)))
	  (raise 
	   (if (eof-object? c)
	       (instantiate::&io-parse-error
		  (obj (the-port))
		  (proc 'status-line)
		  (msg "Illegal status line, premature end of input"))
	       (instantiate::&io-parse-error
		  (obj (http-parse-error-message c (the-port)))
		  (proc 'status-line)
		  (msg "Illegal status line"))))))))

;*---------------------------------------------------------------------*/
;*    status-code-grammar ...                                          */
;*---------------------------------------------------------------------*/
(define status-code-grammar
   (regular-grammar ((CODE (+ (in digit))))
      ((: CODE)
       (the-fixnum))
      (else
       (raise (instantiate::&io-parse-error
		 (obj (http-parse-error-message (the-failure) (the-port)))
		 (proc 'status-line)
		 (msg "Illegal Status-code"))))))

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
			      (http-write-line p (keyword->string! k) ": " v)
			      (error 'http-write-header
				     "Illegal header"
				     h)))
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
	    ((not (eq? (caar lst) content:))
	     (loop (cdr lst) fil))
	    (else
	     (if (string-ci=? (cdar lst) "close")
		 (loop (cdr lst) fil)
		 (loop (cdr lst) (cons (cdar lst) fil))))))))
		 
