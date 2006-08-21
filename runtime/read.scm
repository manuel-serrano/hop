;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/read.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  6 11:55:38 2005                          */
;*    Last change :  Tue Aug 15 15:31:24 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    An ad-hoc reader that supports blending s-expressions and        */
;*    js-expressions. Js-expressions starts with { and ends with }.    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_read
   
   (library pthread)

   (import  __hop_param
	    __hop_read-js
	    __hop_css)
   
   (export  (the-loading-file)
	    
	    (hop-load-afile ::bstring)
	    
	    (hop-read #!optional (iport::input-port (current-input-port)))
	    (hop-load ::bstring #!key (env (interaction-environment)) (mode 'load))

	    (load-once ::bstring #!key (mode 'load))
	    (load-once-unmark! ::bstring)
	    
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
		      0))
		  0)))
      (raise (instantiate::&io-read-error
		(fname (input-port-name port))
		(location loc)
		(proc 'read)
		(msg msg)
		(obj obj)))))

;*---------------------------------------------------------------------*/
;*    read-error/location ...                                          */
;*---------------------------------------------------------------------*/
(define (read-error/location msg obj fname loc)
   (raise (instantiate::&io-read-error
	     (fname fname)
	     (location (if (fixnum? loc) loc 0))
	     (proc 'read)
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
	  (let ((len (vector-length obj)))
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
;*    collect-up-to ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (collect-up-to ignore kind port)
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
		(read-multi-line-comment input-port)
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
			(input-port-name input-port)
			(input-port-position input-port))))))))
      (read/rp g port)))

;*---------------------------------------------------------------------*/
;*    *hop-grammar* ...                                                */
;*---------------------------------------------------------------------*/
(define *hop-grammar*
   (regular-grammar ((float    (or (: (* digit) "." (+ digit))
				   (: (+ digit) "." (* digit))))
		     (letter   (in ("azAZ") (#a128 #a255)))
		     (special  (in "!@~$%^&*></-_+\\=?.:"))
		     (kspecial (in "!@~$%^&*></-_+\\=?."))
		     (quote    (in "\",'`"))
		     (paren    (in "()[]{}"))
		     (id       (: (* digit)
				  (or letter special)
				  (* (or letter special digit (in "'`")))))
		     (kid      (: (* digit)
				  (or letter kspecial)
				  (* (or letter kspecial digit (in ",'`")))))
		     (blank    (in #\Space #\Tab #a012 #a013))
		     
		     cycles par-open bra-open par-poses bra-poses)
      
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
      ((bol (: "#!" #\space (in digit letter special "|,'`") (* all)))
       (ignore))
      
      ;; the interpreter header or the dsssl named constants
      ((: "#!" (+ (in digit letter special "|,'`")))
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
      
      ;; characters
      ((: (uncase "#a") (= 3 digit))
       (let ((string (the-string)))
	  (if (not (=fx (the-length) 5))
	      (read-error/location
	       "Illegal ascii character"
	       string
	       (input-port-name     (the-port))
	       (input-port-position (the-port)))
	      (integer->char (string->integer (the-substring 2 5))))))
      ((: "#\\" (>= 3 digit))
       (integer->char (string->integer (the-substring 2 0) 8)))
      ((: "#\\" (or letter digit special (in "|#; " quote paren)))
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
	       (input-port-name     (the-port))
	       (input-port-position (the-port)))))))
      
      ;; ucs-2 characters
      ((: "#u" (= 4 xdigit))
       (integer->ucs2 (string->integer (the-substring 2 6) 16)))
      
      ;; strings with newline in them in addition to compute
      ;; the string, we have to count the number of newline
      ;; in order to increment the line-num variable strings
      ((: (? #\#) "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (let ((str (the-substring 0 (-fx (the-length) 1))))
	  (escape-C-string str)))
      ;; ucs2 strings
      ((: "#u\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (let ((str (the-substring 3 (-fx (the-length) 1))))
  	  (utf8-string->ucs2-string str)))
      
      ;; fixnums
      ((: (? "+") (+ digit))
       (the-integer))
      ((: "-" (+ digit))
       (the-integer))
      ((: "#b" (? (in "-+")) (+ (in ("02"))))
       (string->integer (the-substring 2 (the-length)) 2))
      ((: "#o" (? (in "-+")) (+ (in ("07"))))
       (string->integer (the-substring 2 (the-length)) 8))
      ((: "#d" (? (in "-+")) (+ (in ("09"))))
       (string->integer (the-substring 2 (the-length)) 10))
      ((: "#x" (? (in "-+")) (+ (in (uncase (in ("09af"))))))
       (string->integer (the-substring 2 (the-length)) 16))
      ((: "#e" (? (in "-+")) (+ digit))
       (string->elong (the-substring 2 (the-length)) 10))
      ((: "#l" (? (in "-+")) (+ digit))
       (string->llong (the-substring 2 (the-length)) 10))
      
      ;; flonum
      ((: (? (in "-+"))
	  (or float
	      (: (or float (+ digit)) (in "eE") (? (in "+-")) (+ digit))))
       (the-flonum))
      
      ;; doted pairs
      ("."
       (if (<=fx par-open 0)
	   (read-error/location 
	    "Illegal token"
	    #\.
	    (input-port-name     (the-port))
	    (input-port-position (the-port)))
	   *dotted-mark*))
      
      ;; unspecified and eof-object
      ((: "#" (in "ue") (+ (in "nspecified-objt")))
       (let ((symbol (string->symbol
		      (string-upcase!
		       (the-substring 1 (the-length))))))
	  (cond
	     ((eq? symbol 'UNSPECIFIED)
	      unspec)
	     ((eq? symbol 'EOF-OBJECT)
	      beof)
	     (else
	      (read-error/location
	       "Illegal identifier"
	       symbol
	       (input-port-name     (the-port))
	       (input-port-position (the-port)))))))
      
      ;; booleans
      ((: "#" (uncase #\t))
       #t)
      ((: "#" (uncase #\f))
       #f)
      
      ;; constants
      ((: "#<" (+ (or digit (uncase (in "afAF")))) ">")
       (if (not (=fx (the-length) 7))
	   (read-error/location
	    "Illegal constant"
	    (the-string)
	    (input-port-name     (the-port))
	    (input-port-position (the-port)))
	   (make-cnst (string->integer (the-substring 2 6) 16))))
      
      ;; keywords
      ((or (: ":" kid) (: kid ":"))
       ;; since the keyword expression is also matched by the id
       ;; rule, keyword rule has to be placed before the id rule.
       (the-keyword))
      
      ;; identifiers
      (id
       ;; this rule has to be placed after the rule matching the `.' char
       (the-symbol))
      ((: "|" (+ (or (out #a000 #\\ #\|) (: #\\ all))) "|")
       (if (=fx (the-length) 2)
	   (the-symbol)
	   (let ((str (the-substring 0 (-fx (the-length) 1))))
	      (string->symbol (escape-C-string str)))))
      
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
      ((in "(")
       ;; we increment the number of open parenthesis
       (set! par-open (+fx 1 par-open))
       (set! par-poses (cons (-fx (input-port-position (the-port)) 1)
			     par-poses))
       ;; and then, we compute the result list...
       (make-list! (collect-up-to ignore "list" (the-port)) (the-port)))
      ((in ")")
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
			   cycles par-open bra-open par-poses bra-poses)))
	  (list 'quasiquote exp)))
      
      ;; vectors
      ("#("
       ;; we increment the number of open parenthesis
       (set! par-open (+fx 1 par-open))
       (set! par-poses (cons (-fx (input-port-position (the-port)) 1)
			     par-poses))
       (list->vector (reverse! (collect-up-to ignore "vector" (the-port)))))
      ((: "#" (: letter (* id)) "(")
       ;; we increment the number of open parenthesis
       (set! par-open (+fx 1 par-open))
       (set! par-poses (cons (-fx (input-port-position (the-port)) 1)
			     par-poses))
       (let* ((id  (let ((str (the-substring 1 (-fx (the-length) 1))))
		      (string->symbol str)))
	      (l   (reverse! (collect-up-to ignore "vector" (the-port)))))
	  (list->tvector id l)))
      
      ;; javascript (this reads up to the closing bracket).
      ("{"
       (hop-read-javascript (the-port)))
      
      ;; hss (this reads up to the closing bracket).
      ("@{"
       (hop-read-hss (the-port)))
      
      ;; escape sequences
      ("~("
       (set! par-open (+fx 1 par-open))
       (set! par-poses (cons (-fx (input-port-position (the-port)) 1)
			     par-poses))
       ;; and then, we compute the result list...
       (list '<TILDE>
	     ((hop-make-escape)
	      (the-port)
	      (make-list!
	       (collect-up-to ignore "list" (the-port)) (the-port)))))
      
      ;; structures
      ("#{"
       ;; then, we compute the structure
       ;; we increment the number of open parenthesis
       (set! bra-open (+fx 1 bra-open))
       (set! bra-poses (cons (-fx (input-port-position (the-port)) 1)
			     bra-poses))
       (let ((l (reverse! (collect-up-to ignore "structure" (the-port)))))
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
;*    *text-grammar* ...                                               */
;*    -------------------------------------------------------------    */
;*    The grammar that parses texts (the [...] forms).                 */
;*---------------------------------------------------------------------*/
(define *text-grammar*
   (regular-grammar (cycles par-open bra-open par-poses bra-poses)

      ((: (* (out ",[]\\")) #\])
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (list 'at name pos))
	      (item (the-substring 0 (-fx (the-length) 1))))
	  (econs item '() loc)))
      ((: (* (out ",[\\")) ",]")
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (list 'at name pos))
	      (item (the-substring 0 (-fx (the-length) 1))))
	  (econs item '() loc)))
      ((: (* (out ",[]\\")) #\, (out #\( #\] #\,))
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (list 'at name pos))
	      (item (the-string))
	      (rest (ignore)))
	  (econs item rest loc)))
      ((: (* (out ",[]\\")) #\,)
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (list 'at name pos))
	      (item (the-substring 0 (-fx (the-length) 1)))
	      (sexp (read/rp *hop-grammar* (the-port)
			     cycles par-open bra-open
			     par-poses bra-poses))
	      (rest (ignore)))
	  (if (string=? item "")
	      (cons (list 'unquote sexp) rest)
	      (econs item (cons (list 'unquote sexp) rest) loc))))
      ((or (+ (out ",[]\\"))
	   (+ #\Newline)
	   (: (* (out ",[]\\")) #\, (out "([]\\")))
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (list 'at name pos))
	      (item (the-string))
	      (rest (ignore)))
	  (econs item rest loc)))
      ("\\\\"
       (cons "\\" (ignore)))
      ("\\n"
       (cons "\n" (ignore)))
      ("\\t"
       (cons "\t" (ignore)))
      ("\\]"
       (cons "]" (ignore)))
      ("\\["
       (cons "[" (ignore)))
      ("\\,"
       (cons "," (ignore)))
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
(define (hop-read #!optional (iport::input-port (current-input-port)))
   (if (closed-input-port? iport)
       (error 'hop-read "Illegal closed input port" iport)
       (begin
	  ((hop-read-pre-hook) iport)
	  (let ((e (read/rp *hop-grammar* iport '() 0 0 '() '())))
	     ((hop-read-post-hook) iport)
	     e))))

;*---------------------------------------------------------------------*/
;*    *the-loading-file* ...                                           */
;*---------------------------------------------------------------------*/
(define *the-loading-file* #f)

;*---------------------------------------------------------------------*/
;*    the-loading-file ...                                             */
;*---------------------------------------------------------------------*/
(define (the-loading-file)
   (let ((t (current-thread)))
      (if (thread? t)
	  (thread-specific t)
	  *the-loading-file*)))

;*---------------------------------------------------------------------*/
;*    *afile-dirs* ...                                                 */
;*---------------------------------------------------------------------*/
(define *afile-dirs* '())
(define *afile-mutex* (make-mutex "hop-afile"))

;*---------------------------------------------------------------------*/
;*    hop-load-afile ...                                               */
;*    -------------------------------------------------------------    */
;*    Load the .afile if it exists and complement the module           */
;*    accesses.                                                        */
;*---------------------------------------------------------------------*/
(define (hop-load-afile dir)
   (define (add-dir f)
      (if (or (string=? f "") (char=? (string-ref f 0) #\/))
	  f
	  (make-file-name dir f)))
   (mutex-lock! *afile-mutex*)
   (if (memq dir *afile-dirs*)
       (mutex-unlock! *afile-mutex*)
       (begin
	  (set! *afile-dirs* (cons dir *afile-dirs*))
	  (mutex-unlock! *afile-mutex*)
	  (let ((path (make-file-name dir ".afile")))
	     (when (file-exists? path)
		(let ((exp (with-input-from-file path read)))
		   (when (list? exp)
		      (for-each (lambda (a)
				   (if (and (list? a)
					    (symbol? (car a))
					    (every? string? (cdr a)))
				       (let ((fs (map! add-dir (cdr a))))
					  (evmodule-add-access! (car a) fs))))
				exp))))))))
   
;*---------------------------------------------------------------------*/
;*    hop-load ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-load file-name #!key (env (interaction-environment)) (mode 'load))
   (let ((path (find-file/path file-name (hop-path))))
      (if (not (string? path))
	  (raise (instantiate::&io-file-not-found-error
		    (proc 'hop-load)
		    (msg "Can't find file")
		    (obj file-name)))
	  (let ((port (open-input-file path)))
	     (if (input-port? port)
		 (let ((t (current-thread))
		       (m (eval-module))
		       (f *the-loading-file*))
		    (unwind-protect
		       (begin
			  (hop-load-afile (dirname path))
			  (if (thread? t)
			      (thread-specific-set! t file-name)
			      (set! *the-loading-file* file-name))
			  (case mode
			     ((load)
			      (let loop ((last #unspecified))
				 (let ((sexp (hop-read port)))
				    (if (eof-object? sexp)
					last
					(loop (eval sexp env))))))
			     ((include)
			      (let loop ((res '()))
				 (let ((sexp (hop-read port)))
				    (if (eof-object? sexp)
					(reverse! res)
					(loop (cons (eval sexp env) res))))))
			     (else
			      (error 'hop-load "Illegal mode" mode))))
		       (begin
			  (close-input-port port)
			  (eval-module-set! m)
			  (set! *the-loading-file* f))))
		 (raise (instantiate::&io-port-error
			   (proc 'hop-load)
			   (msg "Can't open file")
			   (obj file-name))))))))

;*---------------------------------------------------------------------*/
;*    *load-once-table* ...                                            */
;*---------------------------------------------------------------------*/
(define *load-once-table* #unspecified)

;*---------------------------------------------------------------------*/
;*    *load-once-mutex* ...                                            */
;*---------------------------------------------------------------------*/
(define *load-once-mutex* (make-mutex "load-once"))
(define *load-once-condvar* (make-condition-variable "load-once"))

;*---------------------------------------------------------------------*/
;*    load-once ...                                                    */
;*---------------------------------------------------------------------*/
(define (load-once file #!key (mode 'load))
   (mutex-lock! *load-once-mutex*)
   (unless (hashtable? *load-once-table*)
      (set! *load-once-table* (make-hashtable)))
   (let loop ()
      (let ((f (file-name-unix-canonicalize file)))
	 (case (hashtable-get *load-once-table* f)
	    ((error)
	     ;; the file failed to be loaded
	     #f)
	    ((loaded)
	     ;; the file is already loaded
	     (mutex-unlock! *load-once-mutex*))
	    ((loading)
	     ;; the file is currently being loaded
	     (condition-variable-wait! *load-once-condvar* *load-once-mutex*)
	     (loop))
	    (else
	     ;; the file has to be loaded
	     (hashtable-put! *load-once-table* f 'loading)
	     (mutex-unlock! *load-once-mutex*)
	     (with-handler
		(lambda (e)
		   (mutex-lock! *load-once-mutex*)
		   (hashtable-put! *load-once-table* f 'error)
		   (condition-variable-signal! *load-once-condvar*)
		   (mutex-unlock! *load-once-mutex*)
		   (raise e))
		(hop-load f :mode mode))
	     (mutex-lock! *load-once-mutex*)
	     (hashtable-put! *load-once-table* f 'loaded)
	     (condition-variable-signal! *load-once-condvar*)
	     (mutex-unlock! *load-once-mutex*))))))

;*---------------------------------------------------------------------*/
;*    load-once-unmark! ...                                            */
;*    -------------------------------------------------------------    */
;*    Remove a file name from the load-once table                      */
;*---------------------------------------------------------------------*/
(define (load-once-unmark! file)
   (mutex-lock! *load-once-mutex*)
   (when (hashtable? *load-once-table*)
      (hashtable-remove! *load-once-table* (file-name-unix-canonicalize file)))
   (mutex-unlock! *load-once-mutex*))
   
