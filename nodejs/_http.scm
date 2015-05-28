;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_http.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug  7 06:23:37 2014                          */
;*    Last change :  Wed May 20 19:06:36 2015 (serrano)                */
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
	      (offset::long (default 0))
	      (shift::long (default 0))
	      (prevec (default #f))
	      (prematch (default #f))
	      (errno::long (default 0))
	      (chunk (default 0))
	      (clen (default -1))
	      (ip (default #f))
	      (status-code::long (default 0))
	      (http-major::long (default 0))
	      (http-minor::long (default 0))
	      (headers::pair-nil (default '()))
	      (flags::pair-nil (default '()))
	      (content-length::int32 (default #s32:-1))
	      (method::obj (default #f))
	      (url::bstring (default ""))
	      (mode::symbol (default 'default))
	      (errname (default #f))
	      (type::symbol (default 'response))
	      state::procedure))

   (export (process-http-parser ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    debug-parser ...                                                 */
;*---------------------------------------------------------------------*/
(define debug-parser
   (let ((env (getenv "NODE_DEBUG")))
      (cond
	 ((not (string? env)) 0)
	 ((string-contains env "_http") 2)
	 (else 0))))

;*---------------------------------------------------------------------*/
;*    ->fixnum ...                                                     */
;*---------------------------------------------------------------------*/
(define (->fixnum n)
   (cond
      ((fixnum? n) n)
      ((flonum? n) (flonum->fixnum n))
      (else 0)))

;*---------------------------------------------------------------------*/
;*    reset-parsing! ...                                               */
;*    -------------------------------------------------------------    */
;*    Reset the parsing state, i.e., prepare for a new parsing.        */
;*---------------------------------------------------------------------*/
(define (reset-parsing! p::JsHttpParser)
   (with-access::JsHttpParser p (state errno prematch clen content-length
				   status-code http-major http-minor headers
				   flags method url mode type)
      (set! prematch #f)
      (set! state http-line-state)
      (set! errno 0)
      (set! status-code 0)
      (set! http-major 0)
      (set! http-minor 0)
      (set! headers '())
      (set! flags '())
      (set! content-length #s32:-1)
      (set! clen -1)
      (set! method #f)
      (set! url "")
      (set! mode 'default)
      (set! type 'response)))

;*---------------------------------------------------------------------*/
;*    reset-parser! ...                                                */
;*    -------------------------------------------------------------    */
;*    Full parser reset.                                               */
;*---------------------------------------------------------------------*/
(define (reset-parser! p::JsHttpParser)
   (when (>fx debug-parser 0)
      (tprint "RESET-PARSER!"))
   (with-access::JsHttpParser p (state buffer shift prevec)
      (set! buffer #f)
      (set! shift 0)
      (set! prevec #f)
      (reset-parsing! p)))

;*---------------------------------------------------------------------*/
;*    process-http-parser ...                                          */
;*---------------------------------------------------------------------*/
(define (process-http-parser %this)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "http-parse" "binding not implemented" name))
	 0 name))
   
   (define http-parser-proto
      (with-access::JsGlobalObject %this (js-object)
	 (let ((proto (js-new0 %this js-object)))
	    (js-put! proto 'reinitialize
	       (js-make-function %this
		  (lambda (this kind)
		     (if (not (or (eq? kind 0) (eq? kind 1)))
			 (exn %this
			    "Argument must be HTTPParser.REQUEST or HTTPParser.RESPONSE")
			 (begin
			    (reset-parser! this)
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
		     (with-access::JsHttpParser this (errno)
			(set! errno 1)) 
		     (js-undefined))
		  0 'pause)
	       #f %this)
	    (js-put! proto 'resume
	       (js-make-function %this
		  (lambda (this)
		     (with-access::JsHttpParser this (errno)
			(set! errno 0) )
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
(define (http-parser-execute %this parser::JsHttpParser buf off len)

   (define (execute vec::bstring offfx::long bufoff::long buflen::long)
      (when (>=fx debug-parser 2)
	 (tprint ">>> http-parser-execute vlen="
	    (string-length vec) " off=" off " len=" len " -----------------"))
      (let (str stroff strend)
	 (with-access::JsHttpParser parser (buffer shift prevec offset type)
	    (set! buffer buf)
	    (set! offset offfx)
	    (let ((bufstart (+fx bufoff offfx)))
	       (if (string? prevec)
		   (let ((subvec (substring vec bufstart
				    (+fx bufstart buflen))))
		      (set! shift (string-length prevec))
		      (set! str (string-append prevec subvec))
		      (set! stroff 0)
		      (set! strend (string-length str))
		      (set! prevec #f))
		   (begin
		      (set! shift 0)
		      (set! str vec)
		      (set! stroff bufstart)
		      (set! strend (+fx bufstart buflen)))))
	    (when (>=fx debug-parser 1)
	       (tprint ">-- execute " type
		  " stroff=" stroff " strend=" strend
		  " len=" (-fx strend stroff)
		  " shift=" shift
		  (if (>=fx debug-parser 2)
		      (string-append " {"
			 (string-for-read
			    (substring str stroff
			       (minfx (+fx stroff 200) strend)))
			 (if (< (+fx stroff 200) strend)
			     "..."
			     "")
			 "}")
		      ""))))
	 (let ((ip (open-input-string! str stroff strend)))
	    (let loop ((count 0)
		       (avail (-fx strend stroff)))
	       (with-access::JsHttpParser parser (state mode errno buffer)
		  (multiple-value-bind (nstate nread)
		     (state ip %this parser 0 avail)
		     (when (>=fx debug-parser 2) 
			(tprint "<-- execute count=" count
			   " nread=" nread " nstate=" nstate " errno=" errno))
		     (cond
			((not (=fx errno 0))
			 ;; parse error
			 (let ((byteparsed (+fx count nread)))
			    (when (>fx debug-parser 0)
			       (tprint "<<< execute byteparsed=" byteparsed
				  " errno=" errno))
			    byteparsed))
			((not nread)
			 ;; partial chunk
			 (set! buffer #f)
			 (set! state nstate)
			 (when (>fx debug-parser 0)
			    (tprint "<<< execute byteparsed=" avail
			       " partial-chunk-parse"))
			 avail)
			((<=fx nread 0)
			 ;; premature eof
			 (let ((byteparsed (-fx count nread)))
			    (with-access::JsHttpParser parser (prevec)
			       (set! state nstate)
			       ;; -fx because nread is negative
			       (set! prevec
				  (substring str (+fx stroff (-fx count nread))
				     strend))
			       (set! buffer #f)
			       (when (>fx debug-parser 0)
				  (tprint "<<< execute byteparsed=" byteparsed
				     " partial-parse=" nread " prevec="
				     (string-length prevec)))
			       byteparsed)))
			((and (=fx nread avail) (eq? mode 'flush-eof))
			 ;; keep reading he message body
			 (let ((byteparsed (+fx count nread)))
			    (set! buffer #f)
			    (set! state http-identity-eof)
			    (when (>fx debug-parser 0)
			       (tprint "<<< execute byteparsed=" byteparsed
				  " read up to eof"))
			    byteparsed))
			((or (=fx nread avail) (not nstate) (eq? mode 'upgrade))
			 (let ((byteparsed (+fx count nread)))
			    ;; everything has been parsed
			    (reset-parser! parser)
			    (when (>fx debug-parser 0)
			       (tprint "<<< execute byteparsed=" byteparsed
				  " parsing done"))
			    byteparsed))
			(else
			 ;; keep parsing
			 (reset-parsing! parser)
			 (with-access::JsHttpParser parser (state)
			    (set! state nstate))
			 (let ((byteparsed (+fx count nread)))
			    (when (>fx debug-parser 0)
			       (tprint "!-- execute byteparsed=" byteparsed
				  " loop"
				  (string-append " {"
				     (string-for-read
					(substring str (+fx nread stroff)
					   (minfx (+fx (+fx nread stroff) 200)
					      strend)))
				     (if (< (+fx stroff 200) strend)
					 "..."
					 "")
				     "}")))
			    (loop byteparsed (-fx avail nread)))))))))))

   (define (execute-safe vec::bstring bufoff::long buflen::long)
      (let ((offfx (->fixnum off))
	    (lenfx (->fixnum len)))
	 (cond
;* 	 ((or (<fx bufoff 0) (>=fx bufoff buflen))                     */
;* 	  (exn %this                                                   */
;* 	     (format "Offset is out of bounds, offset=~d length=~d"    */
;* 		off length)))                                          */
;* 	 ((< (- buflen off) len)                                       */
;* 	  (exn %this                                                   */
;* 	     (format "off + len > buffer.length, offset=~d len=~d buffer.length=~d" */
;* 		off len length)))                                      */
	    (else
	     (let ((nparsed (execute vec offfx bufoff (minfx buflen lenfx))))
		(with-access::JsHttpParser parser (errno errname)
		   (if (>fx errno 0)
		       (with-access::JsGlobalObject %this (js-type-error)
			  (let ((e (js-new %this js-type-error
				      (js-string->jsstring "Parse Error"))))
			     (js-put! e 'bytesParsed nparsed #f %this)
			     (js-put! e 'code errname #f %this)
			     e))
		       nparsed)))))))

   (with-access::JsHttpParser parser (buffer state)
      (cond
	 (buffer
	  (exn %this "Already parsing a buffer"))
	 ((isa? buf JsArrayBuffer)
	  (with-access::JsArrayBuffer buf (data)
	     (execute-safe data 0 (string-length data))))
	 ((isa? buf JsTypedArray)
	  (with-access::JsTypedArray buf (%data byteoffset length)
	     (execute-safe %data (uint32->fixnum byteoffset)
		(uint32->fixnum length))))
	 (else
	  (exn %this "Argument should be a buffer ~a" (typeof buf))))))

;*---------------------------------------------------------------------*/
;*    http-parse-error ...                                             */
;*---------------------------------------------------------------------*/
(define (http-parse-error parser::JsHttpParser err::long msg::bstring nread state)
   (when (>fx debug-parser 0)
      (tprint "!!! HTTP-PARSE-ERROR nread=" nread " msg=" msg " err=" err
	 " state=" state))
   (with-access::JsHttpParser parser (errno errname)
      (set! errno err)
      (set! errname (js-string->jsstring msg))
      (values #f nread)))

;*---------------------------------------------------------------------*/
;*    http-error-excerpt ...                                           */
;*---------------------------------------------------------------------*/
(define (http-error-excerpt msg c ip)
   (let ((rest (read-chars 40 ip)))
      (string-append msg " \"{" (string-for-read (string c)) "}"
	 (if (string? rest) rest "") "\"")))

;*---------------------------------------------------------------------*/
;*    http-eof-or-parse-error ...                                      */
;*---------------------------------------------------------------------*/
(define (http-eof-or-parse-error parser::JsHttpParser state::procedure
	   failure nread::long)
   (if (eof-object? failure)
       ;; premature eof
       (values state (negfx nread))
       ;; parse error
       (http-parse-error parser 13 "illegal char" nread "eof-or-parse")))

;*---------------------------------------------------------------------*/
;*    set-http-version ...                                             */
;*---------------------------------------------------------------------*/
(define (set-http-version! parser version)
   (with-access::JsHttpParser parser (http-major http-minor)
      (let ((i (string-index version #\.)))
	 (set! http-major (string->integer version))
	 (set! http-minor (string->integer (substring version (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    http-line-state ...                                              */
;*---------------------------------------------------------------------*/
(define (http-line-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)

   (define (http-state port version len)
      (with-access::JsHttpParser parser (method)
	 (set! method #f))
      (http-on-message-begin %this parser)
      (set-http-version! parser version)
      (http-status-state port %this parser (+fx nread len) (-fx avail len)))

   (define grammar
      (regular-grammar ((SP #\Space)
			(CR #\Return)
			(LF #\Newline)
			(CRLF (: CR LF))
			%this parser
			nread)
	 ((+ (or CR LF))
	  ;; ignore extra crlf heading
	  (set! nread (the-length))
	  (ignore))
	 ((: (+ (in ("AZ"))) SP)
	  ;; HTTP request
	  (with-access::JsHttpParser parser (method type flags prematch)
	     (set! type 'request)
	     (set! method (the-substring 0 -1))
	     (when (string? prematch)
		(set! method (string-append prematch method))
		(set! prematch #f))
	     (when (string=? method "HEAD")
		(set! flags (cons 'skipbody flags))))
	  (let ((len (the-length)))
	     (http-path-state (the-port) %this parser
		(+fx nread len) (-fx avail len))))
	 (SP
	  ;; HTTP request
	  (with-access::JsHttpParser parser (method type flags prematch)
	     (cond
		((not (string? prematch))
		 (http-parse-error parser 16
		    (http-error-excerpt "unexpected char" (the-failure) ip)
		    nread "line0"))
		((string-prefix? "HTTP/" prematch)
		 (http-state (the-port)
		    (substring prematch 5 (string-length prematch)) 1))
		((string-prefix? "HTTPS/" prematch)
		 (http-state (the-port)
		    (substring prematch 6 (string-length prematch)) 1))
		(else
		 (set! type 'request)
		 (set! method prematch)
		 (set! prematch #f)
		 (when (string=? method "HEAD")
		    (set! flags (cons 'skipbody flags)))
		 (http-path-state (the-port) %this parser
		    (+fx nread 1) (-fx avail 1))))))
	 ((: "HTTP/" (: (+ (in ("09"))) #\. (+ (in ("09")))) SP)
	  ;; HTTP response
	  (let ((len (the-length)))
	     (http-state (the-port) (the-substring 5 (-fx len 1)) len)))
;* 	  (with-access::JsHttpParser parser (method)                   */
;* 	     (set! method #f))                                         */
;* 	  (let* ((len (the-length))                                    */
;* 		 (http-protocol "http"))                               */
;* 	     (http-on-message-begin %this parser)                      */
;* 	     (set-http-version! parser (the-substring 5 (-fx len 1)))  */
;* 	     (http-status-state (the-port) %this parser                */
;* 		(+fx nread len) (-fx avail len))))                     */
	 ((: "HTTPS/" (: (+ (in ("09"))) #\. (+ (in ("09")))) SP)
	  ;; HTTPS response
	  (let ((len (the-length)))
	     (http-state (the-port) (the-substring 6 (-fx len 1)) len)))
;* 	  (with-access::JsHttpParser parser (method)                   */
;* 	     (set! method #f))                                         */
;* 	  (let* ((len (the-length))                                    */
;* 		 (http-protocol "https"))                              */
;* 	     (set-http-version! parser (the-substring 6 (-fx len 1)))  */
;* 	     (http-status-state (the-port) %this parser                */
;* 		(+fx nread len) (-fx avail len))))                     */
	 ((or (: (+ (out SP CR LF)) SP)
	      (: "HTTP" (out #\/ #\S))
	      (: "HTTPS" (out #\/)))
	  (http-parse-error parser 13
	     "HPE_INVALID_CONSTANT" nread "line1"))
	 ((: (+ (or (in ("AZ09")) (in "/."))))
	  ;; partial state
	  (with-access::JsHttpParser parser (prematch)
	     (if (string? prematch)
		 (set! prematch (string-append prematch (the-string)))
		 (set! prematch (the-string)))
	     (set! nread (+fx nread (the-length)))
	     (ignore)))
	 (else
	  (if (eof-object? (the-failure))
	      (values http-line-state #f)
	      (http-parse-error parser 16
		 (http-error-excerpt "unexpected char" (the-failure) ip)
		 nread "line2")))))

   (when (>fx debug-parser 0)
      (tprint "HTTP-LINE-STATE nread=" nread " avail=" avail))
   (read/rp grammar ip %this parser nread))

;*---------------------------------------------------------------------*/
;*    http-path-state ...                                              */
;*---------------------------------------------------------------------*/
(define (http-path-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)

   (define grammar
      (regular-grammar (%this parser)
	 ((: (+ (out #\space #\tab #\Newline #\Return)) #\space)
	  (with-access::JsHttpParser parser (url)
	     (set! url (the-substring 0 -1)))
	  (let ((len (the-length)))
	     (http-version-state (the-port) %this parser
		(+fx nread len) (-fx avail len))))
	 ((: (+ (out #\space #\tab #\Newline #\Return)))
	  ;; partial state
	  (values http-path-state (negfx nread)))
	 (else
	  (http-eof-or-parse-error parser http-path-state (the-failure) nread))))

   (when (>fx debug-parser 0)
      (tprint "HTTP-PATH-STATE nread=" nread " avail=" avail))
   (read/rp grammar ip %this parser))

;*---------------------------------------------------------------------*/
;*    http-version-state ...                                           */
;*---------------------------------------------------------------------*/
(define (http-version-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)
   
   (define grammar
      (regular-grammar (%this parser nread)
	 ((+ #\space)
	  (set! nread (the-length))
	  (ignore))
	 ((: "HTTP/" (+ digit) #\. (+ digit) "\r\n")
	  (set-http-version! parser (the-substring 5 -2))
	  (let ((len (the-length)))
	     (http-header-state (the-port) %this parser
		(+fx nread len) (-fx avail len))))
	 ((+ (out #\Return #\Newline #\Space))
	  ;; partial state
	  (values http-version-state (negfx nread)))
	 (else
	  (http-eof-or-parse-error parser http-version-state (the-failure) nread))))
   
   (when (>fx debug-parser 0)
      (tprint "HTTP-VERSION-STATE nread=" nread " avail=" avail))
   (read/rp grammar ip %this parser nread))

;*---------------------------------------------------------------------*/
;*    http-status-state ...                                            */
;*---------------------------------------------------------------------*/
(define (http-status-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)
   
   (define grammar
      (regular-grammar ((SP #\Space)
			%this parser nread)
	 ((: (+ (in ("09"))) SP)
	  (with-access::JsHttpParser parser (status-code)
	     (let ((len (the-length)))
		(set! status-code (string->integer (the-substring 0 -1)))
		(http-message-state (the-port) %this parser
		   (+fx nread len) (-fx avail len)))))
	 ((: (+ (in ("09"))) "\r\n")
	  (with-access::JsHttpParser parser (status-code)
	     (let ((len (the-length)))
		(set! status-code (string->integer (the-substring 0 -3)))
		(http-header-state (the-port) %this parser
		   (+fx nread len) (-fx avail len)))))
	 ((: (+ (in ("09"))))
	  ;; partial state
	  (values http-status-state (negfx nread)))
	 (else
	  (http-eof-or-parse-error parser http-status-state (the-failure) nread))))
   
   (when (>fx debug-parser 0)
      (tprint "HTTP-STATUS-STATE nread=" nread " avail=" avail))
   (read/rp grammar ip %this parser nread))

;*---------------------------------------------------------------------*/
;*    http-message-state ...                                           */
;*---------------------------------------------------------------------*/
(define (http-message-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)
   
   (define grammar
      (regular-grammar ((SP #\Space)
			(crlf (: (? #\Return) #\Newline))
			%this parser nread)
	 ((: (+ (out "\r\n")) crlf)
	  (let ((message (the-substring 0 -2))
		(len (the-length)))
	     (http-header-state (the-port) %this parser
		(+fx nread len) (-fx avail len))))
	 ((: "\r" (out "\n"))
	  (http-parse-error parser 12
	     "invalid HTTP status code" nread "message"))
	 ("\n"
	  (http-parse-error parser 12
	     "invalid HTTP status code" nread "message2"))
	 ((+ (out "\r\n"))
	  ;; partial state
	  (values http-message-state (negfx nread)))
	 (else
          (http-eof-or-parse-error parser http-message-state (the-failure) nread))))
   
   (when (>fx debug-parser 0)
      (tprint "HTTP-MESSAGE-STATE nread=" nread " avail=" avail))
   (read/rp grammar ip %this parser nread))

;*---------------------------------------------------------------------*/
;*    http-header-state ...                                            */
;*---------------------------------------------------------------------*/
(define (http-header-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)

   (define grammar
      (regular-grammar ((crlf (: (? #\Return) #\Newline))
			%this parser nread)
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
		    "invalid character in header" nread "header")
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
	     (with-access::JsHttpParser parser (flags url headers)
		(if (memq 'trailer flags)
		    (begin
		       (when (>fx debug-parser 0)
			  (tprint "*** MESSAGE_COMPLETE.2.." nread
			     " avail=" avail))
		       (http-on-message-complete %this parser)
		       ;; skip \r\n sequences
		       (let loop ((nread (+fx nread len))
				  (avail (-fx avail len)))
			  (cond
			     ((=fx avail 0)
			      (values #f nread))
			     ((>=fx avail 2)
			      (let* ((c1 (read ip))
				     (c2 (read ip)))
				 (if (and (char=? c1 #\Return)
					  (char=? c2 #\Newline))
				     (loop (+fx nread 2) (-fx avail 2))
				     (http-parse-error parser 22
					"bad character" nread "header2"))))
			     (else
			      (http-parse-error parser 22
				 "bad character" nread "header3")))))
		    (http-body-state (the-port) %this parser
		       (+fx nread len) (-fx avail len))))))
	 (#\:
	  (http-parse-error parser 21
	     "invalid character in header" nread "header4"))
	 ((: (+ (or (out " :\r\n\t") (: #\space (out #\:)))) (in "\r\n"))
	  (http-parse-error parser 21
	     "invalid character in header" nread "header5"))
	 (else
	  ;; partial state
	  (values http-header-state (negfx nread)))))

   (when (>fx debug-parser 0)
      (tprint "HTTP-HEADER-STATE nread=" nread " avail=" avail))
   (read/rp grammar ip %this parser nread))

;*---------------------------------------------------------------------*/
;*    http-header-value-state ...                                      */
;*---------------------------------------------------------------------*/
(define (http-header-value-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)

   (define (add-header! parser value)
      (with-access::JsHttpParser parser (headers flags content-length clen mode)
	 (case (string->symbol (string-downcase (car headers)))
	    ((connection)
	     (cond
		((string-ci=? value "close")
		 (set! flags (cons 'connection-close flags)))
		((string-ci=? value "keep-alive")
		 (set! flags (cons 'connection-keep-alive flags)))))
	    ((transfer-encoding)
	     (when (string-ci=? value "chunked")
		(set! flags (cons 'transfer-encoding-chunked flags))))
	    ((content-length)
	     (set! clen (string->integer value))
	     (set! content-length (fixnum->int32 clen)))
	    ((upgrade)
	     (set! mode 'upgrade)))
	 (set! headers (cons value headers))))

   (define grammar
      (regular-grammar ((crlf (: (? #\Return) #\Newline))
			%this parser nread)
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
	     "invalid character in header" nread "header-value"))
	 (else
	  (values http-header-value-state (negfx nread)))))

   (when (>fx debug-parser 0)
      (tprint "HTTP-HEADER-VALUE-STATE nread=" nread " avail=" avail))
   (read/rp grammar ip %this parser nread))

;*---------------------------------------------------------------------*/
;*    http-body-state ...                                              */
;*---------------------------------------------------------------------*/
(define (http-body-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)
   (with-access::JsHttpParser parser (flags mode method content-length type
					status-code)
      (when (>fx debug-parser 0)
	 (tprint "*** HTTP-BODY-STATE nread=" nread " avail=" avail))

      (cond
	 ((eq? mode 'upgrade)
	  (when (>fx debug-parser 0)
	     (tprint "*** MESSAGE_COMPLETE.3.."))
	  (http-on-message-complete %this parser)
	  (values #f nread))
	 ((memq 'skipbody flags)
	  (when (>fx debug-parser 0)
	     (tprint "*** MESSAGE_COMPLETE.4.."))
	  (http-on-message-complete %this parser)
	  (values #f nread))
	 ((memq 'transfer-encoding-chunked flags)
	  (http-chunk-state ip %this parser nread avail))
	 ((=s32 content-length #s32:0)
	  (when (>fx debug-parser 0)
	     (tprint "*** MESSAGE_COMPLETE.5.. status-code=" status-code))
	  (http-on-message-complete %this parser)
	  (values http-line-state nread))
	 ((>s32 content-length #s32:0)
	  (http-body-identity ip %this parser nread avail))
	 ((or (eq? type 'request) (not (http-needs-eof? parser)))
	  (when (>fx debug-parser 0)
	     (tprint "*** MESSAGE_COMPLETE.6.. (" type ")"))
	  (http-on-message-complete %this parser)
	  (values http-line-state nread))
	 (else
	  ;; MS CARE
	  (when (>fx avail 0)
	     (http-on-body %this parser nread avail))
	  (set! mode 'flush-eof)
	  (values 'http-identity-eof (+fx nread avail))
	  ;;(http-content-state ip %this parser nread avail)
	  ))))

;*---------------------------------------------------------------------*/
;*    http-identity-eof ...                                            */
;*---------------------------------------------------------------------*/
(define (http-identity-eof ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)
   (when (>=fx debug-parser 0)
      (tprint "HTTP-IDENTITY-EOF avail=" avail " nread=" nread))
   (if (=fx avail 0)
       (begin
	  (when (>fx debug-parser 0)
	     (tprint "*** MESSAGE_COMPLETE.7.."))
	  (http-on-message-complete %this parser)
	  (values #f (+fx nread avail)))
       (begin
	  (http-on-body %this parser nread avail)
	  (values 'http-identity-eof (+fx nread avail)))))

;*---------------------------------------------------------------------*/
;*    http-body-identity ...                                           */
;*---------------------------------------------------------------------*/
(define (http-body-identity ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)
   (with-access::JsHttpParser parser (content-length offset)
      (let* ((avail32 (fixnum->int32 avail))
	     (to-read (if (<s32 avail32 content-length) avail32 content-length)))
	 (set! content-length (-s32 content-length to-read))
	 (when (>fx debug-parser 0)
	    (tprint "*** HTTP-BODY-IDENTITY nread=" nread " avail=" avail
	       " clen=" content-length " to-read=" to-read))
	 (let ((to-readfx (int32->fixnum to-read)))
	    (if (=s32 content-length #s32:0)
		(begin
		   (when (>fx debug-parser 0)
		      (tprint "*** HTTP-DATA_CB on_body.1 to-read="
			 to-read " (nread=" nread ")"))
		   (http-on-body %this parser nread to-readfx)
		   (when (>fx debug-parser 0)
		      (tprint ">>> MESSAGE_COMPLETE.7..."))
		   (http-on-message-complete %this parser)
		   (when (>fx debug-parser 0)
		      (tprint "<<< MESSAGE_COMPLETE.7..."))
		   (set-input-port-position! ip (+fx nread to-readfx))
		   (values http-line-state (+fx nread to-readfx)))
		(begin
		   (when (>fx debug-parser 0)
		      (tprint "*** HTTP_DATA_CB on_body off="
			 nread " len=" to-readfx))
		   (http-on-body %this parser nread to-readfx)
		   (values http-body-identity #f)))))))

;*---------------------------------------------------------------------*/
;*    http-content-state ...                                           */
;*---------------------------------------------------------------------*/
(define (http-content-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)
   (with-access::JsHttpParser parser (offset clen)
      (when (>fx debug-parser 0)
	 (tprint "*** HTTP-CONTENT-STATE nread=" nread " avail=" avail
	    " clen=" clen))
      (cond
	 ((=fx avail 0)
	  (values http-body-state (negfx nread)))
	 ((= clen -1)
	  (http-on-body %this parser nread avail)
	  (tprint "MS: 20 may 2015 CARE, I'M NOT SURE HERE")
	  ;(http-on-message-complete %this parser)
	  (values #f (+fx nread avail)))
	 ((>=fx avail clen)
	  (http-on-body %this parser nread clen)
	  (http-on-message-complete %this parser)
	  (values #f (+fx nread avail)))
	 (else
	  (http-on-body %this parser nread avail)
	  (set! clen (-fx clen avail))
	  (values http-body-state (+fx nread avail))))))

;*---------------------------------------------------------------------*/
;*    http-chunk-state ...                                             */
;*---------------------------------------------------------------------*/
(define (http-chunk-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)
   
   (define grammar
      (regular-grammar ((crlf (: (? #\Return) #\Newline))
			%this parser nread)
	 ((: (+ (in ("09afAF"))) crlf)
	  (let* ((chunk (string->integer (the-substring 0 -2) 16))
		 (len (the-length))
		 (availc (-fx avail len))
		 (nreadc (+fx nread len)))
	     (when (>fx debug-parser 0)
		(tprint "--- HTTP-CHUNK-STATE nread=" nread
		   " size=" chunk " availc=" availc))
	     (cond
		((=fx chunk 0)
;* 		 (http-on-message-complete %this parser)               */
		 (with-access::JsHttpParser parser (flags headers)
		    (set! flags (cons 'trailer flags))
		    (set! headers '())
		    (http-header-state ip %this parser nreadc availc)))
;* 		 (values #f nreadc))                                   */
		((>=fx availc (+fx 2 chunk))
		 (when (>fx debug-parser 1)
		    (tprint "--- HTTP-CHUNK-STATE OK nread=" nread
		       " availc=" availc
		       " chunk=" chunk))
		 (http-on-body %this parser nreadc chunk)
		 (set-input-port-position! (the-port) (+fx nreadc chunk))
		 (let ((c1 (read-char ip))
		       (c2 (read-char ip)))
		    (if (and (char=? c1 #\Return) (char=? c2 #\Newline))
			(begin
			   (set! nread (+fx nreadc (+fx chunk 2)))
			   (set! avail (-fx availc (+fx chunk 2)))
			   (ignore))
			(http-parse-error parser 15 "illegal char" nread
			   "chunk"))))
		(else
		 (http-partial-chunk ip %this parser nreadc availc chunk)))))
	 ((: (+ (in ("09afAF"))) (? #\Return))
	  (values http-chunk-state (negfx nread)))
	 (else
	  (if (eof-object? (the-failure))
	      (values http-chunk-state #f)
	      (http-eof-or-parse-error parser http-chunk-state (the-failure) nread)))))

   (when (>fx debug-parser 0)
      (tprint ">>> HTTP-CHUNK-STATE nread=" nread " avail=" avail))
   (read/rp grammar ip %this parser nread))

;*---------------------------------------------------------------------*/
;*    http-partial-chunk ...                                           */
;*---------------------------------------------------------------------*/
(define (http-partial-chunk ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long size::long)
   (when (>fx debug-parser 1)
      (tprint "--- HTTP-PARTIAL-CHUNK nread=" nread " avail=" avail
	 " chunk-size=" size))
   (cond
      ((and (>fx avail 0) (=fx size 0))
       (let ((c (read-char ip)))
	  (if (char=? c #\Return)
	      (http-partial-chunk ip %this parser
		 (+fx nread 1) (-fx avail 1) (-fx size 1))
	      (http-parse-error parser 14 "illegal char" nread
		 "partial-chunk"))))
      ((and (>fx avail 0) (=fx size -1))
       (let ((c (read-char ip)))
	  (if (char=? c #\Newline)
	      (http-chunk-state ip %this parser (+fx nread 1) (-fx avail 1))
	      (http-parse-error parser 14 "illegal char" nread
		 "partial-chunk.2"))))
      ((>=fx avail (+fx 2 size))
       (when (>fx size 0)
	  (http-on-body %this parser nread size)
	  (set-input-port-position! ip (+fx nread size)))
       (let* ((c1 (read-char ip))
	      (c2 (read-char ip)))
	  (if (and (char=? c1 #\Return) (char=? c2 #\Newline))
	      (http-chunk-state ip %this parser
		 (+fx nread (+fx size 2)) (-fx avail (+fx size 2)))
	      (http-parse-error parser 15 "illegal char" nread
		 "partial-chunk.3"))))
      (else
       (when (>fx avail 0)
	  (let ((nreadc (+fx nread avail)))
	     (http-on-body %this parser nread (minfx size avail))
	     (set-input-port-position! ip nreadc)))
       (let* ((rest (-fx size avail))
	      (state (lambda (ip %this parser nread avail)
			(when (>fx debug-parser 0)
			   (tprint ">>> HTTP-PARTIAL-CHUNK-STATE nread="
			      nread " avail=" avail))
			(http-partial-chunk ip %this parser nread avail rest))))
	  (values state #f)))))

;*---------------------------------------------------------------------*/
;*    http-next-chunk-state ...                                        */
;*---------------------------------------------------------------------*/
(define (http-next-chunk-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)
   
   (define grammar
      (regular-grammar ((crlf (: (? #\Return) #\Newline))
			%this parser nread)
	 (crlf
	  (let ((len (the-length)))
	     (http-chunk-state ip %this parser
		(+fx nread len) (-fx avail len))))
	 (#\Return
	  (values http-next-chunk-state nread))
	 (else
	  (http-parse-error parser 23
	     "invalid character in chunk size header" nread "next-chunk"))))
   
   (when (>fx debug-parser 0)
      (tprint ">>> HTTP-NEXT-CHUNK-STATE nread=" nread " avail=" avail))
   (read/rp grammar ip %this parser nread))

;*---------------------------------------------------------------------*/
;*    http-chunk-value-state ...                                       */
;*---------------------------------------------------------------------*/
(define (http-chunk-value-state ip::input-port %this::JsGlobalObject parser::JsHttpParser nread::long avail::long)
   (with-access::JsHttpParser parser (chunk)
      (when (>fx debug-parser 0)
	 (tprint "*** HTTP-CHUNK-VALUE-STATE nread=" nread " avail=" avail))
      (cond
	 ((>=fx avail chunk)
	  (with-access::JsHttpParser parser (offset)
	     (http-on-body %this parser nread chunk))
	  (set-input-port-position! ip (+fx nread chunk))
	  (http-next-chunk-state ip %this parser
	     (+fx nread chunk) (-fx avail chunk)))
	 ((=fx avail 0)
	  (values http-chunk-value-state nread))
	 (else
	  (with-access::JsHttpParser parser (offset (chunksz chunk))
	     (http-on-body %this parser nread avail)
	     (set! chunksz (-fx chunk avail))
	     (values http-chunk-value-state (+fx nread avail)))))))

;*---------------------------------------------------------------------*/
;*    http-on-message-begin ...                                        */
;*---------------------------------------------------------------------*/
(define (http-on-message-begin %this parser)
   (with-access::JsHttpParser parser (url headers)
      (set! url "")
      (set! headers '())))

;*---------------------------------------------------------------------*/
;*    http-on-body ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-on-body %this parser off length)
   (with-access::JsHttpParser parser (buffer shift offset)
      (let ((cb (js-get parser 'onBody %this)))
	 (when (>fx debug-parser 0)
	    (tprint "http-on-body offset=" offset
	       " off=" off " (" (+fx offset (-fx off shift))
	       ") length=" length " shift=" shift))
	 (when (isa? cb JsFunction)
	    (js-call3 %this cb parser buffer (+fx offset (-fx off shift))
	       length)))))

;*---------------------------------------------------------------------*/
;*    http-on-message-complete ...                                     */
;*---------------------------------------------------------------------*/
(define (http-on-message-complete %this parser)
   (when (>fx debug-parser 0)
      (tprint "http-on-message-complete"))
   (let ((cb (js-get parser 'onMessageComplete %this)))
      (when (isa? cb JsFunction)
	 (js-call0 %this cb parser))))

;*---------------------------------------------------------------------*/
;*    http-needs-eof? ...                                              */
;*---------------------------------------------------------------------*/
(define (http-needs-eof? parser)
   (with-access::JsHttpParser parser
	 (status-code flags content-length type method)
      (cond
	 ((eq? type 'request) #f)
	 ((and (<fx status-code 200) (>fx status-code 0)) #f)
	 ((or (=fx status-code 204) (=fx status-code 304)) #f)
	 ((memq 'skipbody flags) #f)
	 ((or (memq 'transfer-encoding-chunked flags)
	      (>=s32 content-length #s32:0))
	  #f)
	 (else
	  #t))))

;*---------------------------------------------------------------------*/
;*    headers->jsheaders ...                                           */
;*---------------------------------------------------------------------*/
(define (headers->jsheaders %this parser)
   (with-access::JsHttpParser parser (headers)
      (js-vector->jsarray
	 (list->vector (map! js-string->jsstring (reverse headers)))
	 %this)))

;*---------------------------------------------------------------------*/
;*    http-on-header-complete ...                                      */
;*---------------------------------------------------------------------*/
(define (http-on-header-complete %this parser::JsHttpParser)
   
   (define (should-keep-alive? parser)
      (with-access::JsHttpParser parser (http-major http-minor flags)
	 (and (if (and (>fx http-major 0) (>fx http-minor 0))
		  ;; http 1.1
		  (not (memq 'connection-close flags))
		  ;; http 1.0
		  (memq 'connection-keep-alive flags))
	      (not (http-needs-eof? parser)))))
   
   (when (>fx debug-parser 0)
      (tprint "*** HTTP-ON-HEADER-COMPLETE"))
   (with-access::JsHttpParser parser (headers method
					status-code
					http-major http-minor
					url mode flags)
      (let ((cb (js-get parser 'onHeaders %this))
	    (jsheaders (headers->jsheaders %this parser))
	    (jsurl (js-string->jsstring url)))
	 (when (and (pair? headers) (isa? cb JsFunction))
	    (js-call2 %this cb parser jsheaders jsurl))
	 (unless (memq 'trailer flags)
	    (let ((cb (js-get parser 'onHeadersComplete %this)))
	       (when (isa? cb JsFunction)
		  (with-access::JsGlobalObject %this (js-object)
		     (let ((info (js-new0 %this js-object)))
			;; upgrade
			(unless (eq? mode 'upgrade)
			   (when (and (string? method)
				      (string=? method "CONNECT"))
			      (set! mode 'upgrade)))
			;; headers
			(js-put! info 'headers jsheaders #f %this)
			;; request or resposne
			(if (string? method)
			    ;; request
			    (begin
			       (js-put! info 'method
				  (js-string->jsstring method) #f %this)
			       (js-put! info 'url jsurl #t %this))
			    ;; response
			    (js-put! info 'statusCode status-code #f %this))
			;; http-version
			(js-put! info 'versionMajor http-major #f %this)
			(js-put! info 'versionMinor http-minor #f %this)
			;; keep-alive
			(let ((kalive (should-keep-alive? parser)))
			   (js-put! info 'shouldKeepAlive kalive #f %this))
			;; upgrade
			(js-put! info 'upgrade (eq? mode 'upgrade) #f %this)
			;; invoke the callback
			(let ((r (js-call2 %this cb parser info (js-undefined))))
			   (when (js-totest r)
			      (set! flags (cons 'skipbody flags)))
			   r)))))))))
