;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/share/hop-exception.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  4 15:51:42 2009                          */
;*    Last change :  Tue Dec 21 12:10:27 2010 (serrano)                */
;*    Copyright   :  2009-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Client-side debugging facility (includes when Hop launched in    */
;*    debug mode).                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop-exception
   (export (hop-get-stack offset . depth)
	   (hop-report-exception exc)
	   (bigloo-mangled? str)
	   (bigloo-demangle str)
	   (<EXCEPTION-STACK> stack)
	   (<EXCEPTION-FRAME> . args))
   (JS (properties->string hop_properties_to_string)
       "Error"
       "window"
       (hop-config hop_config))
   (scheme2js-pragma (hop-get-stack (JS "hop_get_stack"))
		     (hop-report-exception (JS "hop_report_exception"))
		     (bigloo-mangled? (JS "hop_mangledp"))
		     (bigloo-demangle (JS "hop_demangle"))
		     (<EXCEPTION-STACK> (JS "hop_make_exception_stack"))
		     (<EXCEPTION-FRAME> (JS "hop_make_exception_frame"))))

;*---------------------------------------------------------------------*/
;*    bigloo-mangled? ...                                              */
;*---------------------------------------------------------------------*/
(define (bigloo-mangled? string)
   (let ((len (string-length string)))
      (and (>fx len 7)
	   (or (substring=? string "BgL_" 4)
	       (substring=? string "BGl_" 4))
	   (char=? (string-ref string (-fx len 3)) #\z)
	   (or (char-alphabetic? (string-ref string (-fx len 2)))
	       (char-numeric? (string-ref string (-fx len 2))))
	   (or (char-alphabetic? (string-ref string (-fx len 1)))
	       (char-numeric? (string-ref string (-fx len 1)))))))

;*---------------------------------------------------------------------*/
;*    bigloo-demangle ...                                              */
;*---------------------------------------------------------------------*/
(define (bigloo-demangle string)
   (let* ((len (string-length string))
	  (clen (-fx len 3)))
      (define (char->digit c)
	 (if (char-numeric? c)
	     (-fx (char->integer c) (char->integer #\0))
	     (+fx 10 (-fx (char->integer c) (char->integer #\a)))))
      (define (get-8bits-integer r)
	 (let* ((c1 (string-ref string (+fx r 1)))
		(c2 (string-ref string (+fx r 2)))
		(i1 (char->digit c1))
		(i2 (char->digit c2)))
	    (+fx i1 (bit-lsh i2 4))))
      (define (subvector vec len)
	 (let loop ((i (- len 1))
		    (l '()))
	    (if (= i -1)
		(list->string l)
		(loop (- i 1) (cons (vector-ref vec i) l)))))
      (define (bigloo-demangle-at offset)
	 (let ((new (make-vector clen)))
	    (let loop ((r offset)
		       (w 0)
		       (checksum 0))
	       (if (=fx r clen)
		   ;; we still have to check the checksum
		   (if (=fx checksum (get-8bits-integer r))
		       (values (subvector new w) (+fx r 3))
		       (values string (+fx r 3)))
		   (let ((c (string-ref string r)))
		      (if (char=? c #\z)
			  (if (char=? (string-ref string (+fx r 1)) #\z)
			      (values (subvector new (-fx w 1)) (+fx r 2))
			      (let* ((i (get-8bits-integer r))
				     (nc (integer->char i)))
				 (vector-set! new w nc)
				 (loop (+fx r 3)
				       (+fx w 1)
				       (bit-xor checksum i))))
			  (begin
			     (vector-set! new w c)
			     (loop (+fx r 1)
				   (+fx w 1)
				   checksum))))))))
      (define (bigloo-demangle-simple)
	 (multiple-value-bind (str offset)
	    (bigloo-demangle-at 4)
	    str))
      (define (bigloo-demangle-module)
	 (multiple-value-bind (id offset)
	    (bigloo-demangle-at 4)
	    (multiple-value-bind (module offset)
	       (bigloo-demangle-at offset)
	       (string-append id "@" module))))
      (cond
	 ((not (bigloo-mangled? string))
	  string)
	 ((substring=? string "BgL_" 4)
	  (bigloo-demangle-simple))
	 ((substring=? string "BGl_" 4)
	  (bigloo-demangle-module))
	 (else
	  string))))

;*---------------------------------------------------------------------*/
;*    hop-get-stack ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-get-stack offset . depth)
   ;; skip offset frames of the stack
   (let loop ((proc (@ arguments _).callee)
	      (offset offset))
      (cond
	 ((= offset 0)
	  ;; grab depth frame of the stack
	  (let loop ((caller proc)
		     (n (if (pair? depth) (car depth) 10))
		     (stack '()))
	     (if (and caller (> n 0))
		 (let ((frame (cons caller (vector->list caller.arguments))))
		    (loop caller.caller (- n 1) (cons frame stack)))
		 (reverse! stack))))
	 (proc
	  (loop proc.caller (- offset 1)))
	 (else
	  '()))))

;*---------------------------------------------------------------------*/
;*    in-exception-report ...                                          */
;*    -------------------------------------------------------------    */
;*    Use a symbol instead of a boolean to avoid confusion when        */
;*    uninitialized.                                                   */
;*---------------------------------------------------------------------*/
(define in-exception-report 'no)

;*---------------------------------------------------------------------*/
;*    <EXCEPTION-FRAME> ...                                            */
;*---------------------------------------------------------------------*/
(define (<EXCEPTION-FRAME> . args)
   (<DIV> :onclick (begin
		      (set! in-exception-report 'no)
		      (dom-remove-child! (dom-parent-node this) this))
      (list
       (<DIV> :hssclass "hop-error-frame")
       args)))

;*---------------------------------------------------------------------*/
;*    obj->string ...                                                  */
;*---------------------------------------------------------------------*/
(define (obj->string o longp)
   (cond
      ((procedure? o)
       (let ((name (cond
		      ((not (string? o.name))
		       (with-output-to-string (lambda () (write o))))
		      ((> (string-length o.name) 0)
		       o.name)
		      (else
		       (<I> "anonymous")))))
	  (if (and longp (string? o.location))
	      (let ((m (pregexp-match "[(]at ([^ ]+) ([^ ]+)[)]" o.location)))
		 (if m
		     (list
		      (<SPAN> :style "color: #777" 
			 (<A> :style "color: inherit"
			    :href (cadr m) (cadr m) "@" (caddr m))
			 ", ")
		      "(" name " ...)")
		     (list "(" name " ...)")))
	      (if longp
		  (list "(" name " ...)")
		  name))))
      ((string? o)
       o)
      (else
       (with-output-to-string (lambda () (write o))))))

;*---------------------------------------------------------------------*/
;*    <EXCEPTION-STACK> ...                                            */
;*---------------------------------------------------------------------*/
(define (<EXCEPTION-STACK> stack)
   (<DIV> :hssclass "hop-error-trace"
      (<DIV> "Hop client stack:")
      (<PRE> :onclick (stop-event-propagation event)
	 (map (lambda (frame)
		 (list (obj->string (car frame) #t) "\n"))
	      stack))))

;*---------------------------------------------------------------------*/
;*    <EXCEPTION-JSSTACK> ...                                          */
;*---------------------------------------------------------------------*/
(define (<EXCEPTION-JSSTACK> stack skip)
   
   (define (pp-js-stack stack)
      (map (lambda (f)
	      (let ((i (string-index f #\@))
		    (l (string-length f)))
		 (if i
		     (let ((href (substring f (+ i 1) l)))
			(list
			 (<SPAN> :style "color: #777"
			    (<A> :style "color: inherit" :href href href)
			    ", ")
			 (substring f 0 i)
			 "\n"))
		     (let ((m (pregexp-match "at ([^ ]+) [(]([^ ]+)(:[0-9]+:[0-9]+)[)]" f)))
			(if m
			    (list
			     (<SPAN> :style "color: #777" 
				(<A> :style "color: inherit"
				   :href (caddr m) (cadr m) "@" (caddr m)
				   (cadddr m))
				", ")
			     "(" (cadr m)  " ...)\n")
			    (list f "\n"))))))
	   stack))

   (let loop ((l (string-split stack "\n"))
	      (s skip))
      (cond
	 ((null? l)
	  "")
	 ((= s 0)
	  (<DIV> :hssclass "hop-error-trace"
	     (<DIV> :style "font-weight: bold" "JavaScript stack:")
	     (<PRE> :style "font-size: 9pt; padding-left: 1em"
		:onclick (stop-event-propagation event)
		(pp-js-stack l))))
	 (else
	  (loop (cdr l) (- s 1))))))

;*---------------------------------------------------------------------*/
;*    <EXCEPTION> ...                                                  */
;*---------------------------------------------------------------------*/
(define (<EXCEPTION> exc)
   
   (define (unhtml string)
      (set! string (pregexp-replace* "<" "&lt;" string))
      (set! string (pregexp-replace* ">" "&gt;" string))
      (set! string (pregexp-replace* "&quot;" "\"" string))
      string)
   
   (define (exception-name exc)
      (cond
	 ((string? exc.name) exc.name)
	 ((eq? exc.name #unspecified) "HopClientSideError")
	 (else (obj->string exc.name #f))))
   
   (define (exception-message exc)
      (cond
	 ((not (js-in? "message" exc))
	  "unknwown error")     
	 ((string? exc.message)
	  (apply string-append
		 (map (lambda (s)
			 (string-append (bigloo-demangle s) " "))
		      (string-split exc.message "\n "))))
	 ((symbol? exc.message)
	  (symbol->string exc.message))
	 ((keyword? exc.message)
	  (keyword->string exc.message))
	 ((number? exc.message)
	  exc.message)
	 ((not (eq? exc.message #unspecified))
	  (obj->string exc.message #f))
	 ((string? exc.description)
	  (apply string-append
		 (map (lambda (s)
			 (string-append (bigloo-demangle s) " "))
		      (string-split exc.description "\n "))))
	 (else
	  "unknwown error")))
   
   (let* ((message (exception-message exc))
	  (msg (if (and exc (js-in? "scObject" exc))
		   (list message " -- " (<TT> (obj->string exc.scObject #f)))
		   message))
	  (name (exception-name exc))
	  (url (if (string? exc.fileName) exc.fileName document.location.href))
	  (errtitle (if (string? exc.hopLocation)
			(string-append "Client Error: " exc.hopLocation)
			"Client Error"))
	  (src (cond
		  ((and exc.lineNumber (not (eq? exc.lineNumber #unspecified)))
		   (list (<A> :href url url) ", line " exc.lineNumber))
		  ((and exc.line (not (eq? exc.line #unspecified)))
		   (list (<A> :href url url) ", line " exc.line))
		  (else
		   (<A> :href url)))))
      
      (<EXCEPTION-FRAME>
	 (<DIV> :hssclass "hop-error" :class "client"
	    (<SPAN> :hssclass "hop-error-img")
	    (<DIV>
	       (<DIV> :hssclass "hop-error-title" errtitle)
	       (<DIV> :hssclass "hop-error-msg"
		  (<TABLE> :style "font-weight: normal"
		     (<TR>
			(<TD>
			   (<SPAN> :style "color: #777; font-weight: bold" name)
			   ": "
			   (<TT> msg)))
		     (<TR>
			(<TD>
			   src))
		     (<TR>
			(<TD> :style "color: #777"
			   (properties->string exc)))))
	       (when (and exc.hopService (not (eq? exc.hopService #unspecified)))
		  (<DIV> :hssclass "hop-error-trace"
		     (<DIV> :style "font-weight: bold" "Service:")
		     (<TABLE> :style "padding-left: 1em"
			(<TR> (<TD> :style "font-size: 110%; font-family: monospace; font-weight: normal"
				 (obj->string exc.hopService #f))))))
	       (when (pair? exc.hopStack)
		  (<EXCEPTION-STACK> exc.hopStack))
	       (when (string? exc.stack)
		  (<EXCEPTION-JSSTACK> exc.stack 2)))))))

;*---------------------------------------------------------------------*/
;*    hop-report-exception ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-report-exception exc)
   (cond
      ((eq? in-exception-report 'yes)
       ;; we are already reporting an error
       (raise exc))
      ((and document.body (not (null? document.body)))
       ;; regular report
       (set! in-exception-report 'yes)
       (let ((e (cond
		   ((or (not exc) (eq? exc #unspecified))
		    (let ((e (new Error)))
		       (set! e.message "unknown error")
		       e))
		   ((string? exc)
		    (let ((e (new Error)))
		       (set! e.message exc)
		       e))
		   (else
		    exc))))
	  (dom-append-child! document.body (<EXCEPTION> e))))
      (else
       ;; the error might be raised even before document.body is bound
       (add-event-listener! window "load"
	  (lambda (e) (hop-report-exception exc))))))

;*---------------------------------------------------------------------*/
;*    hop-last-exception ...                                           */
;*---------------------------------------------------------------------*/
(define hop-last-exception #f)

;*---------------------------------------------------------------------*/
;*    hop-get-exception ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-get-exception msg url line)
   (if (and hop-last-exception (string=? hop-last-exception.message msg))
       hop-last-exception
       (let ((exc (new Error)))
	  (set! exc.message msg)
	  (set! exc.fileName url)
	  (set! exc.lineNumber line)
	  (set! exc.hopStack (hop-get-stack 2))
	  exc)))

;*---------------------------------------------------------------------*/
;*    hop-onerror-handler ...                                          */
;*---------------------------------------------------------------------*/
(define (hop-onerror-handler msg url line)
   (or (let loop ((i (- (vector-length hop-config.filtered_errors) 1)))
	  (when (>= i 0)
	     (or (eq? url (vector-ref hop-config.filtered_errors i))
		 (loop (- i 1)))))
       ;; build a dummy exception for reporting
       (let ((exc (hop-get-exception msg url line)))
	  ;; report the error
	  (hop-report-exception exc)
	  ;; don't propagate the error
	  (< (hop-debug) 2))))

;*---------------------------------------------------------------------*/
;*    install the default error handler ...                            */
;*---------------------------------------------------------------------*/
(when (> (hop-debug) 0)
   ;; on debug install the Hop error handler
   (error-hook-set!
    (lambda (exc _)
       (set! hop-last-exception exc)
       (set! exc.hopStack (hop-get-stack 3))
       exc))
   (set! window.onerror hop-onerror-handler))
