;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/runtime/read.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  6 11:55:38 2005                          */
;*    Last change :  Sun Nov 13 19:51:02 2016 (serrano)                */
;*    Copyright   :  2005-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    An ad-hoc reader that supports blending s-expressions and        */
;*    js-expressions. Js-expressions starts with { and ends with }.    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_read

   (include "thread.sch")
   
   (library web)

   (import  __hop_param
	    __hop_read-js
	    __hop_css
	    __hop_charset
	    __hop_clientc
	    __hop_hz
	    __hop_weblets
	    __hop_types
	    __hop_xml-types
	    __hop_xml
	    __hop_misc
	    __hop_thread
	    __hop_configure)

   (export  (loading-file-set! ::obj)
	    (the-loading-file)
	    (the-loading-dir)
	    (with-loading-file ::obj ::procedure)
	    (with-input-from-loading-file ::obj ::procedure)
	    
	    (hop-load-afile ::bstring)
	    
	    (hop-read #!optional
		      (iport::input-port (current-input-port))
		      (charset (hop-locale))
		      (menv #f)
		      location)
	    (hop-load ::bstring #!key
		      (env (default-environment))
		      (menv #f)
		      (mode 'load)
		      (charset (hop-locale))
		      (abase #t)
		      (afile #t))

	    (hop-load-once ::bstring
			   #!key
			   (env (default-environment))
			   (menv #f)
			   (mode 'load)
			   (charset (hop-locale))
			   (abase #t)
			   (afile #t))
	    (hop-load-modified ::bstring
			       #!key
			       (env (default-environment))
			       (menv #f)
			       (mode 'load)
			       (charset (hop-locale))
			       (abase #t)
			       (afile #t)
			       (module #f))
	    (hop-unload! ::bstring)

	    (hop-find-sofile::obj ::bstring)
	    (hop-sofile-path::bstring ::bstring)
	    
	    (read-error msg obj port)
	    (read-error/location msg obj fname loc)))
   
;*---------------------------------------------------------------------*/
;*    Control marks ...                                                */
;*---------------------------------------------------------------------*/
(define *end-of-list* (cons 0 0))
(define *dotted-mark* (cons 1 1))

;*---------------------------------------------------------------------*/
;*    read-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-error msg obj port)
   (let ((loc (if (epair? obj)
		  (match-case (cer obj)
		     ((at ?fname ?pos)
		      pos)
		     (else
		      (-fx (input-port-position port) 1)))
		  (-fx (input-port-position port) 1))))
      (raise (instantiate::&io-read-error
		(fname (input-port-name port))
		(location loc)
		(proc "read")
		(msg msg)
		(obj obj)))))

;*---------------------------------------------------------------------*/
;*    read-error/location ...                                          */
;*---------------------------------------------------------------------*/
(define (read-error/location msg obj fname loc)
   (raise (instantiate::&io-read-error
	     (fname fname)
	     (location (if (fixnum? loc) loc 0))
	     (proc "read")
	     (msg msg)
	     (obj obj))))

;*---------------------------------------------------------------------*/
;*    unreference! ...                                                 */
;*---------------------------------------------------------------------*/
(define (unreference! obj port cycles)
   (let loop ((obj obj))
      (cond
	 ((procedure? obj)
	  (let* ((no   (obj))
		 (cell (assq no cycles)))
	     (if (not (pair? cell))
		 (read-error/location "no target for graph reference"
				      no
				      (input-port-name port)
				      (input-port-position port))
		 (cdr cell))))
	 ((pair? obj)
	  (set-car! obj (loop (car obj)))
	  (set-cdr! obj (loop (cdr obj)))
	  obj)
	 ((vector? obj)
	  (let ((len (vector-length obj)))
	     (let laap ((i 0))
		(if (<fx i len)
		    (begin
		       (vector-set! obj i (loop (vector-ref obj i)))
		       (laap (+fx i 1)))
		    obj))))
	 ((struct? obj)
	  (let ((len (struct-length obj)))
	     (let laap ((i 0))
		(if (<fx i len)
		    (begin
		       (struct-set! obj i (loop (struct-ref obj i)))
		       (laap (+fx i 1)))
		    obj))))
	 (else
	  obj))))
   
;*---------------------------------------------------------------------*/
;*    make-list! ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-list! l port)
   
   (define (reverse-proper-list! l)
      (let nr ((l l)
	       (r '()))
	 (cond
	    ((eq? (car l) *dotted-mark*)
	     (read-error "Illegal pair" r port))
	    ((null? (cdr l))
	     (set-cdr! l r)
	     l)
	    (else
	     (let ((cdrl (cdr l)))
		(nr cdrl
		    (begin (set-cdr! l r)
			   l)))))))
   (define (reverse-improper-list! l)
      (let nr ((l (cddr l))
	       (r (car l)))
	 (cond
	    ((eq? (car l) *dotted-mark*)
	     (read-error "Illegal pair" r port))
	    ((null? (cdr l))
	     (set-cdr! l r)
	     l)
	    (else
	     (let ((cdrl (cdr l)))
		(nr cdrl
		    (begin (set-cdr! l r)
			   l)))))))
   (cond
      ((null? l)
       l)
      ((and (pair? l) (pair? (cdr l)) (eq? (cadr l) *dotted-mark*))
       (if (null? (cddr l))
	   (car l)
	   (reverse-improper-list! l)))
      (else
       (reverse-proper-list! l)))) 

;*---------------------------------------------------------------------*/
;*    collect-upto ...                                                 */
;*---------------------------------------------------------------------*/
(define (collect-upto ignore kind port location)
   
   (define (collect-upto-debug ignore kind port)
      ;; move one character backward for the open-parenthesis
      (let* ((name (input-port-name port))
	     (po (-fx (input-port-position port) 1))
	     (item (ignore)))
	 (if (eq? item *end-of-list*)
	     '()
	     (let loop ((acc (econs item '() (list 'at name po))))
		(let ((item (ignore)))
		   (if (eq? item *end-of-list*)
		       acc
		       (loop (let ((po (input-port-last-token-position port)))
				(econs item acc (list 'at name po))))))))))
   
   (define (collect-upto-optim ignore kind port)
      (let ((item (ignore)))
	 (if (eq? item *end-of-list*)
	     '()
	     (let loop ((acc (cons item '())))
		(let ((item (ignore)))
		   (if (eq? item *end-of-list*)
		       acc
		       (loop (cons item acc))))))))
   
   (if (or (>fx (bigloo-debug) 0) location)
       (collect-upto-debug ignore kind port)
       (collect-upto-optim ignore kind port)))

;*---------------------------------------------------------------------*/
;*    read-quote ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-quote kwote port ignore)
   (let* ((pos (input-port-position port))
	  (obj (ignore)))
      (if (or (eof-object? obj) (eq? obj *end-of-list*))
	  (read-error/location "Illegal quotation"
			       kwote
			       (input-port-name port)
			       pos))
      (econs kwote
	     (cons obj '())
	     (list 'at (input-port-name port) pos))))

;*---------------------------------------------------------------------*/
;*    read-multi-line-comment ...                                      */
;*---------------------------------------------------------------------*/
(define (read-multi-line-comment port)
   (let ((g (regular-grammar ()
	       ("#|"
		(read-multi-line-comment (the-port))
		(ignore))
	       ((+ (or (out #\# #\|) (: #\# (out #\|)) (: #\| (out #\#))))
		(ignore))
	       ("|#"
		#unspecified)
	       (else
		(let ((c (the-failure)))
		   (if (eof-object? c)
		       (read-error/location 
			"EOF inside block comment -- #| missing a closing |#"
			c
			(input-port-name (the-port))
			(input-port-position (the-port)))))))))
      (read/rp g port)))

;*---------------------------------------------------------------------*/
;*    *sharp-grammar* ...                                              */
;*---------------------------------------------------------------------*/
(define *sharp-grammar*
   (regular-grammar ()
      
      ;; characters
      ((: (uncase "a") (= 3 digit))
       (let ((string (the-string)))
	  (if (not (=fx (the-length) 4))
	      (read-error "Illegal ascii character" string (the-port))
	      (integer->char (string->integer (the-substring 1 4))))))
      
      ;; ucs-2 characters
      ((: "u" (= 4 xdigit))
       (integer->ucs2 (string->integer (the-substring 1 5) 16)))
      
      ;; ucs2 strings
      ((: "u\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (let ((str (the-escape-substring 2 (-fx (the-length) 1) #f)))
  	  (utf8-string->ucs2-string str)))
      
      ;; fixnums
      ((: "b" (? (in "-+")) (+ (in ("01"))))
       (string->integer (the-substring 1 (the-length)) 2))
      ((: "o" (? (in "-+")) (+ (in ("07"))))
       (string->integer (the-substring 1 (the-length)) 8))
      ((: "d" (? (in "-+")) (+ (in ("09"))))
       (string->integer (the-substring 1 (the-length)) 10))
      ((: "e" (? (in "-+")) (+ digit))
       (string->elong (the-substring 1 (the-length)) 10))
      ((: "ex" (+ xdigit))
       ($strtoeul (the-substring 2 (the-length)) 0 16))
      ((: "l" (? (in "-+")) (+ digit))
       (string->llong (the-substring 1 (the-length)) 10))
      ((: "lx" (+ xdigit))
       ($strtoull (the-substring 2 (the-length)) 0 16))
      ((: "x" (? (in "-+")) (+ (in (uncase (in ("09af"))))))
       (string->integer (the-substring 1 (the-length)) 16))
      
      ;; unspecified and eof-object
      ((: (in "ue") (+ (in "nspecified-objt")))
       (let ((symbol (string->symbol (string-upcase! (the-string)))))
	  (cond
	     ((eq? symbol 'UNSPECIFIED)
	      unspec)
	     ((eq? symbol 'EOF-OBJECT)
	      beof)
	     (else
	      (read-error "Illegal identifier"
			  (string-append "#" (symbol->string symbol))
			  (the-port))))))
      
      ;; constants
      ((: "<" (+ (or digit (uncase (in "afAF")))) ">")
       (if (not (=fx (the-length) 6))
	   (read-error "Illegal constant" (the-string) (the-port))
	   (make-cnst (string->integer (the-substring 1 5) 16))))
      
      (else
       (let ((c (the-failure)))
	  (if (char? c)
	      (read-error "Illegal char" c (the-port))
	      (read-error "Illegal token" (string #\# c) (the-port)))))))

;*---------------------------------------------------------------------*/
;*    *hop-grammar* ...                                                */
;*---------------------------------------------------------------------*/
(define *hop-grammar*
   (regular-grammar ((float       (or (: (* digit) "." (+ digit))
				      (: (+ digit) "." (* digit))))
		     (letter      (in ("azAZ") (#a128 #a255)))
		     (kspecial    (in "!@$%^&*></-_+\\=?"))
		     (specialsans (or kspecial #\:))
		     (special     (or specialsans #\.))
		     (quote       (in "\",'`"))
		     (paren       (in "()[]{}"))
		     (id          (: (* digit)
				     (or letter special)
				     (* (or letter special digit (in "'`")))))
		     (idsans      (: (* digit)
				     (or letter specialsans)
				     (* (or letter specialsans digit (in ",'`")))))
		     (field       (: idsans (+ (: "." idsans))))
		     (letterid    (: (or letter special)
				     (* (or letter special digit (in "'`")))))
		     (kid         (or digit letter kspecial "."))
		     (blank       (in #\Space #\Tab #a012 #a013))
		     
		     cycles par-open bra-open par-poses bra-poses cset menv location)
      ;; utf-8 bom
      ((bof (: #a239 #a187 #a191))
       (set! cset (charset-converter! 'UTF-8 (hop-charset)))
       (ignore))

      ;; utf-16 big endian
      ((bof (: #a254 #a255))
       ;; MS 23nov2011: CARE I don't know if ucs-2 is big or little endian
       (set! cset (charset-converter! 'UCS-2 (hop-charset)))
       (ignore))

      ;; utf-16 little endian
      ((bof (: #a255 #a254))
       ;; MS 23nov2011: CARE I don't know if ucs-2 is big or little endian
       (set! cset (charset-converter! 'UCS-2 (hop-charset)))
       (ignore))

      ;; newlines
      ((+ #\Newline)
       (ignore))
      
      ;; blank lines
      ((+ blank)
       (ignore))
      
      ;; comments
      ((: ";" (* all))
       (ignore))
      
      ;; multi-line comment (SRFI-30)
      ("#|"
       (read-multi-line-comment (the-port))
       (ignore))
      
      ;; #; expression comments
      ("#;"
	 (begin
	    (ignore)
	    (ignore)))
      
      ;; srfi-22 support
      ((bol (: "#!" #\space (or digit letter special (in "|,'`")) (* all)))
       (ignore))
      
      ;; the interpreter header or the dsssl named constants
      ((: "#!" (+ (or digit letter special (in "|,'`"))))
       (let* ((str (the-string)))
	  (cond
	     ((string=? str "#!optional")
	      boptional)
	     ((string=? str "#!rest")
	      brest)
	     ((string=? str "#!key")
	      bkey)
	     (else
	      (set! *bigloo-interpreter* #t)
	      (ignore)))))
      
      ((: (uncase "#a") (= 3 digit))
       (let ((string (the-string)))
	  (if (not (=fx (the-length) 5))
	      (read-error/location
	       "Illegal ascii character"
	       string
	       (input-port-name (the-port))
	       (input-port-position (the-port)))
	      (integer->char (string->integer (the-substring 2 5))))))
      ((: "#\\" (>= 3 digit))
       (integer->char (string->integer (the-substring 2 (the-length)) 8)))
      ((: "#\\" (or letter digit special (in "~|#; " quote paren)))
       (string-ref (the-string) 2))
      ((: "#\\" (>= 2 letter))
       (let ((char-name (string->symbol
			 (string-upcase!
			  (the-substring 2 (the-length))))))
	  (cond
	     ((eq? char-name 'NEWLINE)
	      #\Newline)
	     ((eq? char-name 'TAB)
	      #\tab)
	     ((eq? char-name 'SPACE)
	      #\space)
	     ((eq? char-name 'RETURN)
	      (integer->char 13))
	     ((eq? char-name 'NULL)
	      (integer->char 0))
	     (else
	      (read-error/location
	       "Illegal character"
	       (the-string)
	       (input-port-name (the-port))
	       (input-port-position (the-port)))))))
      
      ;; strings with newline in them in addition to compute
      ;; the string, we have to count the number of newline
      ;; in order to increment the line-num variable strings
      ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (the-escape-substring 1 (-fx (the-length) 1) #f))
      
      ((: "#\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (the-escape-substring 2 (-fx (the-length) 1) #f))
      
      ;; fixnums
      ((: (? "+") (+ digit))
       (the-integer))
      ((: "-" (+ digit))
       (the-integer))
      
      ;; flonum
      ((: (? (in "-+"))
	  (or float
	      (: (or float (+ digit)) (in "eE") (? (in "+-")) (+ digit))))
       (the-flonum))
      
      ;; dotted pairs
      ("."
       (if (<=fx par-open 0)
	   (read-error/location 
	    "Illegal token"
	    #\.
	    (input-port-name (the-port))
	    (input-port-position (the-port)))
	   *dotted-mark*))
      
      ;; booleans
      ((: "#" (uncase #\t))
       #t)
      ((: "#" (uncase #\f))
       #f)
      
      ;; keywords
      ((or (: ":" (+ kid) (* (or kid special)))
	   (: (? (: kid (* (or kid special)))) (+ kid) ":"))
       ;; since the keyword expression is also matched by the id
       ;; rule, keyword rule has to be placed before the id rule.
       (the-keyword))
      
      ;; identifiers and fields
      (field
       ;;;
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (str (the-string))
	      (char (string-ref str 0))
	      (loc (list 'at name pos)))
	  (if (char=? char #\$)
	      `(begin
		  '$
		  ,(econs '->
		      (map! string->symbol (string-split str "."))
		      loc))
	      (econs '->
		 (map! string->symbol (string-split str "."))
		 loc))))
      ((or id "$")
       ;; this rule has to be placed after the rule matching the `.' char
       (the-symbol))
      ((: "|" (+ (or (out #a000 #\\ #\|) (: #\\ all))) "|")
       (if (=fx (the-length) 2)
	   (the-symbol)
	   (string->symbol (the-escape-substring 1 (-fx (the-length) 1) #f))))
      
      ;; quotations 
      ("'"
       (read-quote 'quote (the-port) ignore))
      ("`"
       (read-quote 'quasiquote (the-port) ignore))
      (","
       (read-quote 'unquote (the-port) ignore))
      (",@"
       (read-quote 'unquote-splicing (the-port) ignore))
      
      ;; lists
      ("("
       ;; we increment the number of open parenthesis
       (set! par-open (+fx 1 par-open))
       (set! par-poses (cons (-fx (input-port-position (the-port)) 1) par-poses))
       ;; and then, we compute the result list...
       (make-list! (collect-upto ignore "list" (the-port) location) (the-port)))
      (")"
       ;; we decrement the number of open parenthesis
       (set! par-open (-fx par-open 1))
       (if (<fx par-open 0)
	   (begin
	      (set! par-open 0)
	      (ignore))
	   (begin
	      (set! par-poses (cdr par-poses))
	      *end-of-list*)))
       
      ;; list of strings
      (#\[
       (let ((exp (read/rp *text-grammar* (the-port)
			   cycles par-open bra-open par-poses bra-poses
			   cset menv)))
	  (if (and (pair? exp) (null? (cdr exp)))
	      (car exp)
	      (list 'quasiquote exp))))
      
      ;; vectors
      ("#("
       ;; we increment the number of open parenthesis
       (set! par-open (+fx 1 par-open))
       (set! par-poses (cons (-fx (input-port-position (the-port)) 1)
			     par-poses))
       (list->vector
	(reverse! (collect-upto ignore "vector" (the-port) location))))

      ;; typed vectors
      ((: "#" letterid "(")
       (set! par-open (+fx 1 par-open))
       (set! par-poses (cons (-fx (input-port-position (the-port)) 1)
			     par-poses))
       (let ((s (the-substring 1 -1)))
	  (cond
	     ((string=? s "s8")
	      (list->s8vector
	       (reverse! (collect-upto ignore "s8vector" (the-port) location))))
	     ((string=? s "u8")
	      (list->u8vector
	       (reverse! (collect-upto ignore "u8vector" (the-port) location))))
	     ((string=? s "s16")
	      (list->s16vector
	       (reverse! (collect-upto ignore "s16vector" (the-port) location))))
	     ((string=? s "u16")
	      (list->u16vector
	       (reverse! (collect-upto ignore "u16vector" (the-port) location))))
	     ((string=? s "s32")
	      (list->s32vector
	       (reverse! (collect-upto ignore "s32vector" (the-port) location))))
	     ((string=? s "u32")
	      (list->u32vector
	       (reverse! (collect-upto ignore "u32vector" (the-port) location))))
	     ((string=? s "s64")
	      (list->s64vector
	       (reverse! (collect-upto ignore "s64vector" (the-port) location))))
	     ((string=? s "u64")
	      (list->u64vector
	       (reverse! (collect-upto ignore "u64vector" (the-port) location))))
	     ((string=? s "f32")
	      (list->f32vector
	       (reverse! (collect-upto ignore "f32vector" (the-port) location))))
	     ((string=? s "f64")
	      (list->f64vector
	       (reverse! (collect-upto ignore "f64vector" (the-port) location))))
	     (else
	      (let* ((id (string->symbol s))
		     (l (reverse! (collect-upto ignore "vector" (the-port) location))))
		 (list->tvector id l))))))
      
      ;; javascript (this reads up to the closing bracket).
      ("{"
       (let ((v (hop-read-javascript (the-port))))
	  (read-char (the-port))
	  v))
      
      ("~"
       ;;; client side expression
       (with-access::clientc (hop-clientc) (expressionc)
	  (let* ((loc (list 'at
			 (input-port-name (the-port))
			 (input-port-position (the-port))))
		 (expr (ignore))
		 (src (tree-copy expr))
		 (env (current-module-clientc-import))
		 (js (expressionc expr env menv hop-read-javascript-string)))
	     (econs '<TILDE> (list js :src `',src :loc `',loc :env `',env) loc))))
      
      ;; structures
      ("#{"
       ;; then, we compute the structure
       ;; we increment the number of open parenthesis
       (set! bra-open (+fx 1 bra-open))
       (set! bra-poses (cons (-fx (input-port-position (the-port)) 1)
			     bra-poses))
       (let ((l (reverse! (collect-upto ignore "structure" (the-port) location))))
	  (cons '_structure_ l)))
      ("}"
       (set! bra-open (-fx bra-open 1))
       (if (<fx bra-open 0)
	   (begin
	      (set! bra-open 0)
	      (ignore))
	   (begin
	      (set! bra-poses (cdr bra-poses))
	      *end-of-list*)))
      
      ;; cyclic target mark
      ((: "#" (+ digit) "=")
       (let* ((no (string->integer (the-substring 1 (-fx (the-length) 1))))
	      (pos (input-port-position (the-port)))
	      (the-object (ignore)))
	  (if (eof-object? the-object)
	      (read-error/location "Illegal cyclic reference"
				   no
				   (input-port-name (the-port))
				   pos))
	  (set! cycles (cons (cons no the-object) cycles))
	  (unreference! the-object (the-port) cycles)))
      
      ;; cyclic target reference
      ((: "#" (+ digit) "#")
       (let* ((no (string->integer (the-substring 1 (-fx (the-length) 1))))
	      (cell (assq no cycles)))
	  (if (not (pair? cell))
	      (lambda () no)
	      (cdr cell))))
      
      ;; special tokens
      ("#"
	(read/rp *sharp-grammar* (the-port)))
      
      ;; error or eof
      (else
       (let ((char (the-failure)))
	  (if (eof-object? char)
	      (cond
		 ((>fx par-open 0)
		  (read-error/location "Unexpected end-of-file"
				       "Unclosed list"
				       (input-port-name (the-port))
				       (car par-poses)))
		 ((>fx bra-open 0)
		  (read-error/location "Unexpected end-of-file"
				       "Unclosed vector or structure"
				       (input-port-name (the-port))
				       (car par-poses)))
		 (else
		  (reset-eof (the-port))
		  char))
	      (read-error/location "Illegal char"
				   (illegal-char-rep char)
				   (input-port-name (the-port))
				   (input-port-position (the-port))))))))

;*---------------------------------------------------------------------*/
;*    text->html-string ...                                            */
;*---------------------------------------------------------------------*/
(define (text->html-string str)
   (if (string-index str "<>&")
       (html-string-encode str)
       str))

;*---------------------------------------------------------------------*/
;*    *text-grammar* ...                                               */
;*    -------------------------------------------------------------    */
;*    The grammar that parses texts (the [...] forms).                 */
;*---------------------------------------------------------------------*/
(define *text-grammar*
   (regular-grammar (cycles par-open bra-open par-poses bra-poses cset menv)

      ((: (* (out ",[]\\")) #\])
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (list 'at name pos))
	      (item (cset (the-substring 0 (-fx (the-length) 1)))))
	  (econs (text->html-string item) '() loc)))
      ((: (* (out ",[]\\")) ",]")
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (list 'at name pos))
	      (item (cset (the-substring 0 (-fx (the-length) 1)))))
	  (econs (text->html-string item) '() loc)))
      ((: (* (out ",[]\\")) #\, (out #\( #\] #\,))
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (list 'at name pos))
	      (item (cset (the-string)))
	      (rest (ignore)))
	  (econs (text->html-string item) rest loc)))
      ((: (* (out ",[]\\")) #\, #\,)
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (list 'at name pos))
	      (item (cset (the-substring 0 -1))))
	  (unread-char! #\, (the-port))
	  (econs (text->html-string item) (ignore) loc)))
      ((: (* (out ",[]\\")) #\,)
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (list 'at name pos))
	      (item (cset (the-substring 0 (-fx (the-length) 1))))
	      (sexp (read/rp *hop-grammar* (the-port)
			     cycles par-open bra-open
			     par-poses bra-poses cset menv
			     #f))
	      (rest (ignore)))
	  (if (string=? item "")
	      (cons (list 'unquote sexp) rest)
	      (econs (text->html-string item)
		     (cons (list 'unquote sexp) rest) loc))))
      ((or (+ (out ",[]\\"))
	   (+ #\Newline)
	   (: (* (out ",[]\\")) #\, (out "([]\\")))
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (list 'at name pos))
	      (item (cset (the-string)))
	      (rest (ignore)))
	  (econs (text->html-string item) rest loc)))
      ("\\]"
       (cons "]" (ignore)))
      ("\\["
       (cons "[" (ignore)))
      (#\\
       (cons "\\" (ignore)))
      (else
       (let ((c (the-failure))
	     (port (the-port)))
	  (define (err msg)
	     (read-error/location
	      msg c (input-port-name port) (input-port-position port)))
	  (cond
	     ((eof-object? c)
	      (err "Illegal `end of file'"))
	     ((char=? c #\[)
	      (err "Illegal nested `[...]' form"))
	     (else
	      (err "Illegal string character")))))))

;*---------------------------------------------------------------------*/
;*    hop-read ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-read #!optional
	   (iport::input-port (current-input-port))
	   (charset (hop-locale))
	   (menv #f)
	   location)
   (if (closed-input-port? iport)
       (error "hop-read" "Illegal closed input port" iport)
       (begin
	  ((hop-read-pre-hook) iport)
	  (with-access::clientc (hop-clientc) (macroe)
	     (let* ((cset (charset-converter! charset (hop-charset)))
		    (menv (or menv (macroe)))
		    (e (read/rp *hop-grammar* iport '() 0 0 '() '() cset menv location)))
		((hop-read-post-hook) iport)
		e)))))

;*---------------------------------------------------------------------*/
;*    *the-loading-file* ...                                           */
;*---------------------------------------------------------------------*/
(define *the-loading-file* #f)

;*---------------------------------------------------------------------*/
;*    the-loading-file ...                                             */
;*---------------------------------------------------------------------*/
(define (the-loading-file)
   (let ((t (current-thread)))
      (if (isa? t hopthread)
	  (with-access::hopthread t (%loading-file)
	     %loading-file)
	  *the-loading-file*)))

;*---------------------------------------------------------------------*/
;*    loading-file-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (loading-file-set! file-name)
   (let ((t (current-thread)))
      (if (isa? t hopthread)
	  (with-access::hopthread t (%loading-file)
	     (set! %loading-file file-name))
	  (set! *the-loading-file* file-name))))

;*---------------------------------------------------------------------*/
;*    the-loading-dir ...                                              */
;*---------------------------------------------------------------------*/
(define (the-loading-dir)
   (let ((path (the-loading-file)))
      (and (string? path) (dirname path))))

;*---------------------------------------------------------------------*/
;*    with-loading-file ...                                            */
;*---------------------------------------------------------------------*/
(define (with-loading-file file proc)
   (let ((old (the-loading-file)))
      (loading-file-set! file)
      (unwind-protect
	 (proc)
	 (loading-file-set! old))))
      
;*---------------------------------------------------------------------*/
;*    with-input-from-loading-file ...                                 */
;*---------------------------------------------------------------------*/
(define (with-input-from-loading-file file proc)
   (let ((old (the-loading-file)))
      (loading-file-set! file)
      (unwind-protect
	 (with-input-from-file file proc)
	 (loading-file-set! old))))
      
;*---------------------------------------------------------------------*/
;*    *afile-dirs* ...                                                 */
;*---------------------------------------------------------------------*/
(define *afile-dirs* '())
(define *afile-mutex* (make-mutex "hop-afile"))
(define *afile-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    hop-load-afile ...                                               */
;*    -------------------------------------------------------------    */
;*    Load the .afile if it exists and complement the module           */
;*    access table.                                                    */
;*---------------------------------------------------------------------*/
(define (hop-load-afile dir)
   (or (module-load-access-file dir)
       (get-directory-module-access dir)))

;*---------------------------------------------------------------------*/
;*    get-directory-module-access ...                                  */
;*---------------------------------------------------------------------*/
(define (get-directory-module-access dir)
   
   (define (get-file-module-access f abase)
      (synchronize *afile-mutex*
	 (unless (hashtable-get *afile-table* f)
	    (hashtable-put! *afile-table* f #t)
	    (with-handler
	       (lambda (e)
		  #unspecified)
	       (call-with-input-file f
		  (lambda (p)
		     (match-case (hop-read p)
			((module ?module-name . ?-)
			 (module-add-access! module-name (list f) abase)))))))))
   
   (for-each (lambda (f)
		(when (member (suffix f) (hop-module-suffixes))
		   ;; look like a source file, try it
		   (get-file-module-access f dir)))
	     (directory->path-list dir)))

;*---------------------------------------------------------------------*/
;*    hop-load ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-load file-name
		  #!key
		  (env (default-environment))
		  (menv #f)
		  (mode 'load)
		  (charset (hop-locale))
		  (abase #t)
		  (afile #t))
   (if (hz-package-filename? file-name)
       (hop-load-from-hz file-name env menv mode charset abase)
       (hop-load-file file-name env menv mode charset abase 'eval afile)))

;*---------------------------------------------------------------------*/
;*    hz-dir ...                                                       */
;*---------------------------------------------------------------------*/
(define (hz-dir fname)
   (or (hz-local-weblet-path fname (get-autoload-directories))
       (hz-cache-path fname)
       (hz-download-to-cache fname (hop-hz-repositories))))
       
;*---------------------------------------------------------------------*/
;*    hop-load-from-hz ...                                             */
;*---------------------------------------------------------------------*/
(define (hop-load-from-hz fname env menv mode charset abase)
   (with-trace 'read "hop-load-from-hz"
      (trace-item "fname=" fname)
      (trace-item "abase=" abase)
      ;; feed the cache
      (let ((dir (hz-dir fname)))
	 ;; load the afile
	 (hop-load-afile dir)
	 ;; load the .hop source file
	 (let ((base (basename dir)))
	    (let ((fname (string-append (make-file-name dir base) ".hop")))
	       (hop-load-file fname env menv mode charset abase 'eval #f))))))

;*---------------------------------------------------------------------*/
;*    so-arch-directory ...                                            */
;*---------------------------------------------------------------------*/
(define-macro (so-arch-directory)
   (string-append (os-name) "-" (os-arch)))

;*---------------------------------------------------------------------*/
;*    so-suffix ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (so-suffix)
   (string-append "." (shared-library-suffix)))

;*---------------------------------------------------------------------*/
;*    hop-find-sofile ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-find-sofile path)
   
   (define (more-recent? sopath)
      (when (file-exists? sopath)
	 (>elong (file-modification-time sopath)
	    (file-modification-time path))))
   
   (define (errfile file)
      (string-append file ".err"))
   
   ;; check the path directory first
   (when (hop-sofile-enable)
      (let* ((dir (dirname path))
	     (base (prefix (basename path)))
	     (file (string-append base
		      (cond-expand
			 (bigloo-unsafe "_u")
			 (else "_s"))
		      "-" (hop-version) (so-suffix)))
	     (sopath (make-file-path dir ".libs"
			(hop-version) (hop-build-id) (so-arch-directory)
			file)))
	 (cond
	    ((more-recent? sopath)
	     sopath)
	    ((more-recent? (errfile sopath))
	     'error)
	    (else
	     ;; if not found check the user global libs repository
	     (let ((sopath (hop-sofile-path path)))
		(cond
		   ((more-recent? sopath)
		    sopath)
		   ((more-recent? (errfile sopath))
		    'error)
		   (else
		    (let ((env (getenv "HOP_LIBS_PATH")))
		       (when (string? env)
			  (let loop ((paths (unix-path->list env)))
			     (when (pair? paths)
				(let ((path (make-file-path (car paths) file)))
				   (cond
				      ((more-recent? path)
				       path)
				      ((more-recent? (errfile path))
				       'error)
				      (else
				       (loop (cdr paths)))))))))))))))))

;*---------------------------------------------------------------------*/
;*    hop-sofile-path ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-sofile-path path)
   (let* ((base (prefix (basename path)))
	  (soname (string-append base "-" (md5sum-string path) (so-suffix))))
      (make-file-path
	 (hop-sofile-directory)
	 (hop-version) (hop-build-id) (so-arch-directory)
	 soname)))
   
;*---------------------------------------------------------------------*/
;*    hop-load-file ...                                                */
;*    -------------------------------------------------------------    */
;*    The C code generation imposes the variable traceid not to        */
;*    be inlined.                                                      */
;*---------------------------------------------------------------------*/
(define (hop-load-file fname env menv mode charset abase traceid::symbol afile)

   (define (hop-eval-path path)
      (let ((apath (cond
		      ((string? abase) abase)
		      (abase (dirname fname))
		      (else ".")))
	    (menv (or menv (with-access::clientc (hop-clientc) (macroe)
			      (macroe)))))
	 (with-trace 'read "hop-eval-path"
	    (trace-item "fname=" fname)
	    (trace-item "path=" path)
	    (trace-item "abase=" abase)
	    (trace-item "afile=" afile)
	    (let ((port (open-input-file path)))
	       (if (input-port? port)
		   (let ((f (the-loading-file))
			 (denv (current-dynamic-env))
			 (m (eval-module)))
		      (unwind-protect
			 (let ()
			    ($env-push-trace denv traceid #f)
			    (when afile (hop-load-afile apath))
			    (loading-file-set! path)
			    (when (evmodule? env) (eval-module-set! env))
			    (case mode
			       ((load)
				(let loop ((last #unspecified)
					   (loc #t))
				   ;; always read the first expression
				   ;; in debug mod to enforce location inside
				   ;; the module clause
				   (let ((e (hop-read port charset menv loc)))
				      (when (epair? e)
					 ($env-set-trace-location denv (cer e)))
				      (if (eof-object? e)
					  (let ((nm (eval-module)))
					     (when (and (not (eq? m nm))
							(evmodule? nm))
						(evmodule-check-unbound nm #f))
					     ($env-pop-trace denv)
					     last)
					  (let ((val (eval! e (eval-module))))
					     (when (isa? val xml-tilde)
						(evwarning
						   (when (pair? e) (cer e))
						   "hop-load"
						   "Useless ~ expression"))
					     (loop val #f))))))
			       ((include)
				(let loop ((res '())
					   (loc #t))
				   (let ((e (hop-read port charset menv loc)))
				      (when (epair? e)
					 ($env-set-trace-location denv (cer e)))
				      (if (eof-object? e)
					  (let ((nm (eval-module)))
					     (unless (eq? m nm)
						(evmodule-check-unbound nm #f))
					     ($env-pop-trace denv)
					     (reverse! res))
					  (let ((val (eval! e (eval-module))))
					     (loop (cons val res) #f))))))
			       ((module force-module)
				(let loop ((loc #t))
				   (let ((e (hop-read port charset menv loc)))
				      (when (epair? e)
					 ($env-set-trace-location denv (cer e)))
				      (if (eof-object? e)
					  (let ((nm (eval-module)))
					     (unless (eq? m nm)
						(evmodule-check-unbound nm #f))
					     ($env-pop-trace denv)
					     (unless (eq? m nm) nm))
					  (let ((val (eval! e (eval-module))))
					     (loop #f))))))
			       (else
				(error "hop-load"
				   (format "Illegal mode \"~a\"" mode)
				   fname))))
			 (begin
			    (close-input-port port)
			    (eval-module-set! m)
			    (loading-file-set! f))))
		   (raise (instantiate::&io-port-error
			     (proc "hop-load")
			     (msg "Can't open file")
			     (obj fname))))))))
   
   (define (hop-load-path path)
      (with-trace 'read "hop-load-path"
	 (trace-item "fname=" fname)
	 (trace-item "path=" path)
	 (trace-item "abase=" abase)
	 (trace-item "afile=" afile)
	 (if (or (eq? mode 'load) (eq? mode 'module))
	     (let ((sopath (hop-find-sofile path)))
		(if sopath
		    ;; a compiled file has been found, try to load that one
		    (with-trace 'read "hop-load-path-compiled"
		       (trace-item "sopath=" sopath)
		       (let ((loadval (dynamic-load sopath)))
			  (trace-item "sovalue=" loadval)
			  (if (eq? mode 'load)
			      loadval
			      (or loadval
				  ;; the dynamic load didn't produced the expected
				  ;; value, load with eval
				  (hop-eval-path path)))))
		    (hop-eval-path path)))
	     (hop-eval-path path))))
   
   (let ((path (find-file/path fname (hop-path))))
      (cond
	 ((not (string? path))
	  (raise (instantiate::&io-file-not-found-error
		    (proc "hop-load")
		    (msg "file not found")
		    (obj fname))))
	 (else
	  (let ((loader (hop-find-loader path)))
	     (if (procedure? loader)
		 (loader path env menv mode charset abase traceid afile)
		 (hop-load-path path)))))))

;*---------------------------------------------------------------------*/
;*    hop-find-loader ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-find-loader path)
   (let ((c (assoc (suffix path) (hop-loaders))))
      (when (pair? c)
	 (cdr c))))
	  
;*---------------------------------------------------------------------*/
;*    *load-table* ...                                                 */
;*---------------------------------------------------------------------*/
(define *load-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    *load-mutex* ...                                                 */
;*---------------------------------------------------------------------*/
(define *load-mutex* (make-mutex "load-once"))

;*---------------------------------------------------------------------*/
;*    *dependency-table* ...                                           */
;*---------------------------------------------------------------------*/
(define *dependency-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    %hop-load ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function loads a file in ENV. If the parameter MODIFIEDP    */
;*    is #t and if the file has changed since the last load, it is     */
;*    reloaded.                                                        */
;*---------------------------------------------------------------------*/
(define (%hop-load file env menv charset modifiedp abase mode afile module)
   
   (define (load-file f::bstring cv::condvar)
      ;; the file has to be loaded
      (trace-item "load")
      ;; save the module dependency
      (when (evmodule? module)
	 (synchronize *load-mutex*
	    (let ((mpath (evmodule-path module)))
	       (when (string? mpath)
		  (let ((c (hashtable-get *dependency-table* mpath)))
		     (cond
			((not (pair? c))
			 (let* ((ftime (file-modification-time file))
				(entry (cons file ftime)))
			    (hashtable-put! *dependency-table* mpath (list entry))))
			((not (assoc file c))
			 (let* ((ftime (file-modification-time file))
				(entry (cons file ftime)))
			    (set-cdr! (last-pair c) (list entry))))))))))
      (with-handler
	 (lambda (e)
	    (synchronize *load-mutex*
	       (hashtable-put! *load-table* f
		  (cons 'error (file-modification-time f)))
	       (condition-variable-signal! cv))
	    (raise e))
	 (begin
	    (synchronize *load-mutex*
	       (hashtable-put! *load-table* f
		  (cons 'loaded (file-modification-time f))))
	    (when modifiedp
	       (reload-dependencies file '()))
	    (hop-load f :mode mode :env env :menv menv
	       :charset charset :abase abase :afile afile)
	    (synchronize *load-mutex*
	       (condition-variable-signal! cv)))))

   (define (reload-dependencies file stack)
      (unless (member file stack)
	 (let ((deps (synchronize *load-mutex*
			(hashtable-get *dependency-table* file))))
	    (when (pair? deps)
	       (for-each (lambda (entry)
			    (hop-load-modified (car entry) :env env :menv menv
			       :mode mode :charset charset :abase abase
			       :afile afile)
			    (reload-dependencies (car entry) (cons file stack)))
		  deps)))))
   
   (define (dependencies-modified file stack)
      (unless (member file stack)
	 (let ((deps (synchronize *load-mutex*
		       (hashtable-get *dependency-table* file))))
	    (when (pair? deps)
	       (any (lambda (entry)
		       (or (>elong (file-modification-time (car entry))
			      (cdr entry))
			   (dependencies-modified (car entry)
			      (cons (car entry) stack))))
		  deps)))))

   (define (get-file-info::pair f::bstring)
      (synchronize *load-mutex*
	 (let ((info (hashtable-get *load-table* f))
	       (t (file-modification-time f)))
	    (cond
	       ((not (pair? info))
		;; the file has to be loaded
		(let* ((cv (make-condition-variable "load"))
		       (ninfo (cons 'loading cv)))
		   (hashtable-put! *load-table* f ninfo)
		   ninfo))
	       ((eq? (car info) 'error)
		;; the file failed to be loaded
		(trace-item "error")
		(if (and modifiedp
			 (or (not (=elong t (cdr info)))
			     (dependencies-modified f '())))
		    (begin
		       (hashtable-remove! *load-table* f)
		       (cons 'force-reload t))
		    info))
	       ((eq? (car info) 'loaded)
		;; the file is already loaded
		(trace-item "already loaded")
		;; re-load the dependency recursively
		;; re-load if modified
		(if (and modifiedp
			 (or (not (=elong t (cdr info)))
			     (dependencies-modified f '())))
		    (begin
		       (hashtable-remove! *load-table* f)
		       (cons 'force-reload t))
		    info))
	       ((eq? (car info) 'loading)
		(trace-item "loading")
		(condition-variable-wait! (cdr info) *load-mutex*)
		(cons 'loop t))
	       (else
		(cons 'error t))))))

   (with-trace 'read "%hop-load"
      (trace-item "file=" file)
      (trace-item "env=" (if (evmodule? env) (evmodule-name env) ""))
      (trace-item "modifiedp=" modifiedp)
      (let ((f (file-name-unix-canonicalize file)))
	 (let loop ()
	    (let ((info (get-file-info f)))
	       (case (car info)
		  ((loading)
		   (load-file f (cdr info)))
		  ((force-reload loop)
		   (loop))
		  ((error)
		   (hop-unload! f)
		   #f)
		  ((loaded)
		   #unspecified)
		  (else
		   (error "hop-load-once" "Illegal state" info))))))))

;*---------------------------------------------------------------------*/
;*    hop-load-once ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-load-once file
		       #!key
		       (env (default-environment))
		       (menv #f)
		       (charset (hop-locale))
		       (mode 'load)
		       (abase #t)
		       (afile #t))
   (%hop-load file env menv charset #f abase mode afile #f))

;*---------------------------------------------------------------------*/
;*    hop-load-modified ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-load-modified file
			   #!key
			   (env (default-environment))
			   (menv #f)
			   (charset (hop-locale))
			   (mode 'load)
			   (abase #t)
			   (afile #t)
			   (module #f))
   (%hop-load file env menv charset #t abase mode afile module))

;*---------------------------------------------------------------------*/
;*    hop-unload! ...                                                  */
;*    -------------------------------------------------------------    */
;*    Remove a file name from the load table, makes it as unload       */
;*---------------------------------------------------------------------*/
(define (hop-unload! file)
   (synchronize *load-mutex*
      (when (hashtable? *load-table*)
	 (hashtable-remove! *load-table*
	    (file-name-unix-canonicalize file)))))
   
