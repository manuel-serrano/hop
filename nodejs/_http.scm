;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_http.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug  7 06:23:37 2014                          */
;*    Last change :  Wed Jan 21 08:35:40 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HTTP bindings                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__http

   (include "nodejs_debug.sch")
   
   (library hopscript)

   (import  __nodejs_uv)

   (static (class JsHttpParser::JsObject
	      (buffer (default #f))
	      (offset (default #f))
	      (chunk (default 0))
	      (clen (default 0))
	      (length (default #f))
	      (ip (default #f))
	      (obuf (default #f))
	      (oend::int (default 0))
	      (ooff::int (default 0))
	      (status-code::int (default 0))
	      (http-major::int (default 0))
	      (http-minor::int (default 0))
	      (headers::pair-nil (default '()))
	      (flags::pair-nil (default '()))
	      (content-length::int32 (default #s32:-1))
	      (method::obj (default #f))
	      (url::obj (default #f))
	      (upgrade::bool (default #f))
	      (errno::long (default 0))
	      (errname (default #f))
	      state::procedure))

   (export (process-http-parser ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    process-http-parser ...                                          */
;*---------------------------------------------------------------------*/
(define (process-http-parser %this)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "http-parse" "binding not implemented" name))
	 0 name))
   
   (define (->fixnum n)
      (cond
	 ((fixnum? n) n)
	 ((flonum? n) (flonum->fixnum n))
	 (else 0)))

   (define http-parser-proto
      (with-access::JsGlobalObject %this (js-object)
	 (let ((proto (js-new0 %this js-object)))
	    (js-put! proto 'reinitialize
	       (js-make-function %this
		  (lambda (this kind)
		     (if (not (or (eq? kind 0) (eq? kind 1)))
			 (exn %this
			    "Argument must be HTTPParser.REQUEST or HTTPParser.RESPONSE")
			 (with-access::JsHttpParser this
			       (buffer state
				  status-code
				  http-major http-minor
				  flags content-length clen
				  method upgrade)
			    (set! state http-line-state)
			    (set! buffer #f)
			    (set! status-code 0)
			    (set! http-major 0)
			    (set! http-minor 0)
			    (set! flags '())
			    (set! content-length #s32:-1)
			    (set! clen -1)
			    (set! method #f)
			    (set! upgrade #f)
			    (js-undefined))))
		  1 'reinitialize)
	       #f %this)
	    (js-put! proto 'execute
	       (js-make-function %this
		  (lambda (this buf offset length)
		     (let ((off (->fixnum (js-tointeger offset %this)))
			   (len (->fixnum (js-tointeger length %this))))
			(http-parser-execute %this this buf off len)))
		  1 'execute)
	       #f %this)
	    (js-put! proto 'finish
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHttpParser this (buffer ip)
			(set! buffer #f)
			(when (input-port? ip)
			   (close-input-port ip)
			   (set! ip #f))))
		  0 'finish)
	       #f %this)
	    (js-put! proto 'pause
	       (js-make-function %this
		  (lambda (this)
		     (tprint "parser pause not implemented...")
		     (js-undefined))
		  0 'pause)
	       #f %this)
	    (js-put! proto 'resume
	       (js-make-function %this
		  (lambda (this)
		     (tprint "parser resume not implemented...")
		     (js-undefined))
		  0 'resume)
	       #f %this)
	    proto)))
   
   (define http-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (js-put! proto 'REQUEST 0 #f %this)
	 (js-put! proto 'RESPONSE 1  #f %this)
	 proto))
   
   (define (http-parser this kind)
      (with-access::JsGlobalObject %this (js-object)
	 (instantiate::JsHttpParser
	    (__proto__ http-parser-proto)
	    (state http-line-state))))
   
   (let ((http (js-make-function %this http-parser 1 "HTTPParser"
		  :construct http-parser
		  :prototype http-parser-proto)))
      (with-access::JsObject http (__proto__)
	 (set! __proto__ http-proto))
      (js-alist->jsobject
	 `((HTTPParser . ,http))
	 %this)))

;*---------------------------------------------------------------------*/
;*    exn ...                                                          */
;*---------------------------------------------------------------------*/
(define (exn %this msg . args)
   (with-access::JsGlobalObject %this (js-object js-error)
      (raise (js-new %this js-error (apply format msg args)))))

;*---------------------------------------------------------------------*/
;*    http-parser-execute ...                                          */
;*---------------------------------------------------------------------*/
(define debug-parser 0)

(define (http-parser-execute %this parser::JsHttpParser buf off len)

   (define (execute vec boff length)
      (when (>=fx debug-parser 1)
	 (tprint ">>> execute len="
	    (string-length vec) " off=" off " len=" len))
      (when (>=fx debug-parser 2)
	 (tprint ">>>>> execute [" (substring vec off (+fx off len) ) "]"))
      (with-access::JsHttpParser parser (buffer state obuf oend ooff)
	 (cond
	    ((>= off length)
	     (exn %this
		(format "Offset is out of bounds, offset=~d length=~d" off length)))
	    ((or (> off length) (< (- length off) len))
	     (exn %this
		(format "off + len > buffer.length, offset=~d len=~d buffer.length=~d" off len length)))
	    (else
	     (with-access::JsHttpParser parser (offset length)
		(set! buffer buf)
		(set! offset off)
		(set! length len))
	     (let* ((off (+fx off boff))
		    (end (+fx len off))
		    str offset length)
		(when (>=fx debug-parser 2)
		   (tprint "### http-parser-execut off=" off " end=" end " ["
		      (string-for-read (substring vec off end)) "]"
		      " obuf={"
		      (when (string? obuf)
			 (string-for-read (substring obuf ooff oend)))
		      "} ooff=" ooff " oend=" oend))
		(cond
		   ((not obuf)
		    (set! str vec))
		   ((>fx off (-fx oend ooff))
		    (when (>=fx debug-parser 2)
		       (tprint "blit"))
		    (let ((blen (-fx oend ooff)))
		       (blit-string! obuf ooff vec (-fx off blen) blen)
		       (set! str vec)
		       (set! off (-fx off blen))
		       (set! len (+fx len blen))))
		   (else
		    (when (>=fx debug-parser 2)
		       (tprint "concat"))
		    (set! str
		       (string-append
			  (substring obuf ooff oend)
			  (substring vec off end)))
		    (set! off 0)
		    (set! len (+fx (-fx oend ooff) len))))
		(when (>=fx debug-parser 2)
		   (tprint "--- execute len=" len " off=" off
		      " [" (string-for-read (substring str off end)) "]"))
		(when (>=fx debug-parser 1)
		   (tprint "--- execute len="
		      (string-length str) " off=" off " end=" end))
		(let ((ip (open-input-string! str off end)))
		   (multiple-value-bind (nstate nread)
		      (state ip %this parser 0 (-fx end off))
		      (when (>=fx debug-parser 1) 
			 (tprint "<<< execute " nread))
		      (when (procedure? nstate)
			 (set! state nstate))
		      (set! buffer #f)
		      (with-access::JsHttpParser parser (upgrade errname)
			 (if (or (=fx nread len) upgrade)
			     ;; everyting read
			     (begin
				(set! obuf #f)
				len)
			     ;; less bytes read
			     (with-access::JsGlobalObject %this (js-error)
				(let ((err (js-new %this js-error "Parse Error")))
				   (js-put! err 'bytesParsed nread #f %this)
				   (js-put! err 'code errname #f %this)
				   err)))))))))))

   (with-access::JsHttpParser parser (buffer state obuf oend ooff)
      (cond
	 (buffer
	  (exn %this "Already parsing a buffer"))
	 ((isa? buf JsArrayBuffer)
	  (with-access::JsArrayBuffer buf (data)
	     (execute data 0 (string-length data))))
	 ((isa? buf JsTypedArray)
	  (with-access::JsTypedArray buf (%data byteoffset length)
	     (execute %data (uint32->fixnum byteoffset) length)))
	 (else
	  (exn %this "Argument should be a buffer ~a"
	     (typeof buf))))))

;*---------------------------------------------------------------------*/
;*    http-parse-error ...                                             */
;*---------------------------------------------------------------------*/
(define (http-parse-error parser::JsHttpParser err::int msg::bstring nread)
   (with-access::JsHttpParser parser (errno errname)
      (set! errno err)
      (set! errname msg)
      (values #f nread)))

;*---------------------------------------------------------------------*/
;*    http-line-state ...                                              */
;*---------------------------------------------------------------------*/
(define (http-line-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::int avail::int)

   (define (set-http-version! parser version)
      (with-access::JsHttpParser parser (http-major http-minor)
	 (let ((i (string-index version #\.)))
	    (set! http-major (string->integer version))
	    (set! http-minor (string->integer (substring version (+fx i 1)))))))

   (define grammar
      (regular-grammar ((SP #\Space)
			(CRLF "\r\n")
			%this parser)
	 ((: (+ (in ("AZ"))) SP)
	  ;; HTTP request
	  (with-access::JsHttpParser parser (method)
	     (set! method (the-substring 0 -1)))
	  (tprint "HTTP-PARSE-REQUEST not implemented...")
	  (values #f nread))
	 ((: "HTTP/" (: (+ (in ("09"))) #\. (+ (in ("09")))) SP)
	  ;; HTTP response
	  (let* ((len (the-length))
		 (http-protocol "http"))
	     (http-on-message-begin %this parser)
	     (set-http-version! parser (the-substring 5 (-fx len 1)))
	     (http-status-state (the-port) %this parser
		(+fx nread len) (-fx avail len))))
	 ((: "HTTPS/" (: (+ (in ("09"))) #\. (+ (in ("09")))) SP)
	  ;; HTTPS response
	  (let* ((len (the-length))
		 (http-protocol "https"))
	     (set-http-version! parser (the-substring 6 (-fx len 1)))
	     (http-status-state (the-port) %this parser
		(+fx nread len) (-fx avail len))))
	 ((or (: (+ (out SP)) SP)
	      (: "HTTP" (out #\/ #\S))
	      (: "HTTPS" (out #\/)))
	  (http-parse-error parser 13
	     "invalid HTTP method" nread))
	 (else
	  (values http-line-state nread))))

   (read/rp grammar ip %this parser))

;*---------------------------------------------------------------------*/
;*    http-status-state ...                                            */
;*---------------------------------------------------------------------*/
(define (http-status-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::int avail::int)
   
   (define grammar
      (regular-grammar ((SP #\Space)
			%this parser)
	 ((: (+ (in ("09"))) SP)
	  (with-access::JsHttpParser parser (status-code)
	     (let ((len (the-length)))
		(set! status-code (string->integer (the-substring 0 -1)))
		(http-message-state (the-port) %this parser
		   (+fx nread len) (-fx avail len)))))
	 ((+ (out ("09")))
	  (http-parse-error parser 11
	     "invalid HTTP version" nread))
	 (else
	  (values http-status-state nread))))
   
   (read/rp grammar ip %this parser))

;*---------------------------------------------------------------------*/
;*    http-message-state ...                                           */
;*---------------------------------------------------------------------*/
(define (http-message-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::int avail::int)
   
   (define grammar
      (regular-grammar ((SP #\Space)
			(crlf (: (? #\Return) #\Newline))
			%this parser)
	 ((: (+ (out "\r\n")) crlf)
	  (let ((message (the-substring 0 -2))
		(len (the-length)))
	     (http-header-state (the-port) %this parser
		(+fx nread len) (-fx avail len))))
	 ((: "\r" (out "\n"))
	  (http-parse-error parser 12
	     "invalid HTTP status code" nread))
	 ("\n"
	  (http-parse-error parser 12
	     "invalid HTTP status code" nread))
	 (else
	  (values http-message-state nread))))
   
   (read/rp grammar ip %this parser))

;*---------------------------------------------------------------------*/
;*    http-header-state ...                                            */
;*---------------------------------------------------------------------*/
(define (http-header-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::int avail::int)

   (define grammar
      (regular-grammar ((crlf (: (? #\Return) #\Newline))
			%this parser)
	 ((: (+ (or (out " :\r\n\t") (: #\space (out #\:)))) #\:)
	  (let ((key (the-substring 0 -1))
		(len (the-length)))
	     (with-access::JsHttpParser parser (headers)
		(set! headers (cons key headers)))
	     (http-header-value-state (the-port) %this parser
		(+fx nread len) (-fx avail len))))
	 ((: (+ (in " \t"))
	     (+ (or (out #\Return) (: #\Return (out #\Newline))))
	     crlf)
	  ;; multiple line header
	  (with-access::JsHttpParser parser (headers)
	     (if (null? headers)
		 (http-parse-error parser 21
		    "invalid character in header" nread)
		 (let* ((len (the-length))
			(str (the-substring 0 -2))
			(i (string-skip str " \t")))
		    (set-car! headers
		       (string-append (car headers)
			  (if i (substring str i) str)))
		    (http-header-state (the-port) %this parser
		       (+fx nread len) (-fx avail len))))))
	 (crlf
	  (let ((len (the-length)))
	     (http-on-header-complete %this parser)
	     (http-body-state (the-port) %this parser
		(+fx nread len) (-fx avail len))))
	 (#\:
	  (http-parse-error parser 21
	     "invalid character in header" nread))
	 ((: (+ (or (out " :\r\n\t") (: #\space (out #\:)))) (in "\r\n"))
	  (http-parse-error parser 21
	     "invalid character in header" nread))
	 (else
	  (values http-header-state nread))))

   (read/rp grammar ip %this parser))

;*---------------------------------------------------------------------*/
;*    http-header-value-state ...                                      */
;*---------------------------------------------------------------------*/
(define (http-header-value-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::int avail::int)

   (define (add-header! parser value)
      (with-access::JsHttpParser parser (headers flags content-length clen upgrade)
	 (case (string->symbol (string-downcase (car headers)))
	    ((connection)
	     (cond
		((string-ci=? value "close")
		 (set! flags (cons 'connection-close flags)))
		((string-ci=? value "keep-alive")
		 (set! flags (cons 'connection-keep-alive flags)))))
	    ((transfer-encoding)
	     (when (string-ci=? value "chunked"))
	     (set! flags (cons 'transfer-encoding-chunked flags)))
	    ((content-length)
	     (set! clen (string->integer value))
	     (set! content-length (fixnum->int32 clen)))
	    ((upgrade)
	     (set! upgrade #t)))
	 (set! headers (cons value headers))))

   (define grammar
      (regular-grammar ((crlf (: (? #\Return) #\Newline))
			%this parser)
	 ((+ (in " \t"))
	  (let ((len (the-length)))
	     (set! nread (+fx nread len))
	     (set! avail (-fx avail len)))
	  (ignore))
	 ((: (out " \t\r\n") (* (or (out "\r\n") (: "\r" (out "\n")))) crlf)
	  (let ((value (the-substring 0 -2))
		(len (the-length)))
	     (add-header! parser value)
	     (http-header-state ip %this parser
		(+fx nread len) (-fx avail len))))
	 ((: (out " \t\r\n") (* (or (out "\r\n") (: "\r" (out "\n")))) "\n")
	  (let ((value (the-substring 0 -1))
		(len (the-length)))
	     (add-header! parser value)
	     (http-header-state ip %this parser
		(+fx nread len) (-fx avail len))))
	 (crlf
	  (let ((value "")
		(len (the-length)))
	     (add-header! parser value)
	     (http-header-state ip %this parser
		(+fx nread len) (-fx avail len))))
	 ((: #\Return (out #\Newline))
	  (http-parse-error parser 21
	     "invalid character in header" nread))
	 (else
	  (values http-header-value-state nread))))

   (read/rp grammar ip %this parser))

;*---------------------------------------------------------------------*/
;*    http-body-state ...                                              */
;*---------------------------------------------------------------------*/
(define (http-body-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::int avail::int)
   (with-access::JsHttpParser parser (flags)
      (if (memq 'transfer-encoding-chunked flags)
	  (http-chunk-state ip %this parser nread avail)
	  (http-content-state ip %this parser nread avail))))

;*---------------------------------------------------------------------*/
;*    http-content-state ...                                           */
;*---------------------------------------------------------------------*/
(define (http-content-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::int avail::int)
   
   (define grammar
      (regular-grammar (%this parser)
	 ((or all #\Newline)
	  (with-access::JsHttpParser parser (offset clen)
	     (cond
		((= clen -1)
		 (when (>fx avail 0)
		    (http-on-body %this parser (+fx offset nread) avail))
		 (http-on-message-complete %this parser)
		 (values #f (+fx nread avail)))
		((>=fx avail clen)
		 (http-on-body %this parser (+fx offset nread) clen)
		 (http-on-message-complete %this parser)
		 (values #f (+fx nread avail)))
		(else
		 (http-on-body %this parser (+fx offset nread) avail)
		 (set! clen (-fx clen avail))
		 (values http-body-state (+fx nread avail))))))
	 (else
	  (values http-body-state nread))))
   
   (read/rp grammar ip %this parser))

;*---------------------------------------------------------------------*/
;*    http-chunk-state ...                                             */
;*---------------------------------------------------------------------*/
(define (http-chunk-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::int avail::int)
   
   (define grammar
      (regular-grammar ((crlf (: (? #\Return) #\Newline))
			%this parser)
	 ((: (+ (in ("09afAF"))) crlf)
	  (let* ((chunk (string->integer (the-substring 0 -2) 16))
		 (len (the-length))
		 (avail (-fx avail len))
		 (nread (+fx nread len)))
	     (cond
		((=fx chunk 0)
		 (http-on-message-complete %this parser)
		 (values #f (+fx nread avail)))
		((>=fx avail chunk)
		 (with-access::JsHttpParser parser (offset)
		    (http-on-body %this parser (+fx offset nread) chunk))
		 (set-input-port-position! (the-port) (+fx nread chunk))
		 (ignore))
		((=fx avail 0)
		 (values http-chunk-state nread))
		(else
		 (with-access::JsHttpParser parser (offset (chunksz chunk))
		    (http-on-body %this parser (+fx offset nread) avail)
		    (set! chunksz (-fx chunk avail))
		 (values http-chunk-value-state (+fx nread avail)))))))
	 ((out ("09afAF"))
	  (http-parse-error parser 23
	     "invalid character in chunk size header" nread))
	 (else
	  (values http-chunk-state nread))))

   (read/rp grammar ip %this parser))

;*---------------------------------------------------------------------*/
;*    http-next-chunk-state ...                                        */
;*---------------------------------------------------------------------*/
(define (http-next-chunk-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::int avail::int)
   
   (define grammar
      (regular-grammar ((crlf (: (? #\Return) #\Newline))
			%this parser)
	 (crlf
	  (let ((len (the-length)))
	     (http-chunk-state ip %this parser
		(+fx nread len) (-fx avail len))))
	 (#\Return
	  (values http-next-chunk-state nread))
	 (else
	  (http-parse-error parser 23
	     "invalid character in chunk size header" nread))))
   
   (read/rp grammar ip %this parser))

;*---------------------------------------------------------------------*/
;*    http-chunk-value-state ...                                       */
;*---------------------------------------------------------------------*/
(define (http-chunk-value-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::int avail::int)
   (with-access::JsHttpParser parser (chunk)
      (cond
	 ((>=fx avail chunk)
	  (with-access::JsHttpParser parser (offset)
	     (http-on-body %this parser (+fx offset nread) chunk))
	  (set-input-port-position! ip (+fx nread chunk))
	  (http-next-chunk-state ip %this parser
	     (+fx nread chunk) (-fx avail chunk)))
	 ((=fx avail 0)
	  (values http-chunk-value-state nread))
	 (else
	  (with-access::JsHttpParser parser (offset (chunksz chunk))
	     (http-on-body %this parser (+fx offset nread) avail)
	     (set! chunksz (-fx chunk avail))
	     (values http-chunk-value-state (+fx nread avail)))))))

;*---------------------------------------------------------------------*/
;*    http-on-message-begin ...                                        */
;*---------------------------------------------------------------------*/
(define (http-on-message-begin %this parser)
   (with-access::JsHttpParser parser (url headers)
      (set! url #f)
      (set! headers '())))

;*---------------------------------------------------------------------*/
;*    http-on-body ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-on-body %this parser offset length)
   (with-access::JsHttpParser parser (buffer)
      (let ((cb (js-get parser 'onBody %this)))
	 (when (isa? cb JsFunction)
	    (js-call3 %this cb parser buffer offset length)))))

;*---------------------------------------------------------------------*/
;*    http-on-message-complete ...                                     */
;*---------------------------------------------------------------------*/
(define (http-on-message-complete %this parser)
   (let ((cb (js-get parser 'onMessageComplete %this)))
      (when (isa? cb JsFunction)
	 (js-call0 %this cb parser))))

;*---------------------------------------------------------------------*/
;*    http-on-header-complete ...                                      */
;*---------------------------------------------------------------------*/
(define (http-on-header-complete %this parser::JsHttpParser)

   (define (needs-eof? parser)
      (with-access::JsHttpParser parser (status-code flags content-length)
	 (or (<fx status-code 200)
	     (=fx status-code 204)
	     (=fx status-code 304)
	     (memq 'connection-keep-alive flags)
	     (<u32 content-length #u32:0))))
      
   (define (should-keep-alive? parser)
      (with-access::JsHttpParser parser (http-major http-minor flags)
	 (and (if (and (>fx http-major 0) (>fx http-minor 0))
		  ;; http 1.1
		  (not (memq 'connection-close flags))
		  ;; http 1.0
		  (memq 'connection-keep-alive flags))
	      (not (needs-eof? parser)))))

   (define (headers parser)
      (with-access::JsHttpParser parser (headers)
	 (js-vector->jsarray
	    (list->vector (map! js-string->jsstring (reverse! headers)))
	    %this)))
      
   (let ((cb (js-get parser 'onHeadersComplete %this)))
      (when (isa? cb JsFunction)
	 (with-access::JsHttpParser parser (status-code
					      http-major http-minor
					      upgrade method url)
	    (with-access::JsGlobalObject %this (js-object)
	       (let ((info (js-new0 %this js-object)))
		  ;; headers
		  (js-put! info 'headers (headers parser) #f %this)
		  ;; request or resposne
		  (if (string? method)
		      ;; request
		      (begin
			 (js-put! info 'method
			    (js-string->jsstring method) #f %this)
			 (js-put! info 'url
			    (if url (js-string->jsstring url) url) #t %this))
		      ;; response
		      (js-put! info 'statusCode status-code #f %this))
		  ;; http-version
		  (js-put! info 'versionMajor http-major #f %this)
		  (js-put! info 'versionMinor http-minor #f %this)
		  ;; keep-alive
		  (let ((kalive (should-keep-alive? parser)))
		     (js-put! info 'shouldKeepAlive kalive #f %this))
		  ;; upgrade
		  (js-put! info 'upgrade upgrade #f %this)
		  ;; invoke the callback
		  (js-call2 %this cb parser info (js-undefined))))))))

   
