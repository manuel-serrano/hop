;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/share/hop-exception.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  4 15:51:42 2009                          */
;*    Last change :  Mon Jul  6 08:21:04 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Client-side debugging facility (includes when Hop launched in    */
;*    debug mode).                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop-exception
   (export (hop-get-stack offset . depth)
	   (hop-report-exception exc)
	   (<EXCEPTION-STACK> stack)
	   (<EXCEPTION-FRAME> . args))
   (JS (properties->string hop_properties_to_string)
       "Error"
       "window"
       (hop-config hop_config))
   (scheme2js-pragma (hop-get-stack (JS "hop_get_stack"))
		     (hop-report-exception (JS "hop_report_exception"))
		     (<EXCEPTION-STACK> (JS "hop_make_exception_stack"))
		     (<EXCEPTION-FRAME> (JS "hop_make_exception_frame"))))

;*---------------------------------------------------------------------*/
;*    hop-error-icon ...                                               */
;*---------------------------------------------------------------------*/
(define (hop-error-icon)
   "data:image/png;base64,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")

;*---------------------------------------------------------------------*/
;*    Exception style                                                  */
;*---------------------------------------------------------------------*/
(define (hop-exception-mask-style)
   "position: fixed;
    top: 0; left: 0; right: 0; bottom: 0;
    opacity: 0.8;
    background: #141111;
    z-index: 1")

(define (hop-exception-frame-style)
   "position: fixed;
    top: 60px; left: 150px; right: 150px; bottom: 60px;
    opacity: 0.97;
    background: white;
    z-index: 2;
    border: 3px dashed red; padding: 4px;
    overflow: hidden")

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
;*    <EXCEPTION-FRAME> ...                                            */
;*---------------------------------------------------------------------*/
(define (<EXCEPTION-FRAME> . args)
   (let ((mask (<DIV> :style (hop-exception-mask-style) ""))
	 (frame (<DIV> :style (hop-exception-frame-style)
		   (<DIV> :style "overflow: auto" args))))
      (<DIV> :onclick (dom-remove-child! (dom-parent-node this) this)
	 mask frame)))

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
			    :href (cadr m) (cadr m) "!" (caddr m))
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
   (<DIV> :style "font-family: arial; font-size: 10pt; padding: 5px"
      (<DIV> :style "font-weight: bold; margin-bttom: 1ex;" "Hop client stack:")
      (<PRE> :style "font-size: 9pt; padding-left: 1em"
	 :onclick (stop-event-propagation event)
	 (map (lambda (frame)
		 (list (obj->string (car frame) #t) "\n"))
	      stack))))

;*---------------------------------------------------------------------*/
;*    <EXCEPTION-JSSTACK> ...                                          */
;*---------------------------------------------------------------------*/
(define (<EXCEPTION-JSSTACK> stack)
   
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
		     f)))
	   (string-split stack "\n")))

   (<DIV> :style "font-family: arial; font-size: 10pt; padding: 5px"
      (<DIV> :style "font-weight: bold" "JavaScript stack:")
      (<PRE> :style "font-size: 9pt; padding-left: 1em"
	 :onclick (stop-event-propagation event)
	 (pp-js-stack stack))))

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
	  (msg (if (and exc.scObject (not (eq? exc.scObject #unspecified)))
		   (list message " -- " (obj->string exc.scObject #f))
		   message))
	  (name (exception-name exc))
	  (url (or exc.fileName document.location))
	  (location (if (string? exc.hopLocation) exc.hopLocation "Client Error"))
	  (src (cond
		  ((and exc.lineNumber (not (eq? exc.lineNumber #unspecified)))
		   (list url ", line " exc.lineNumber))
		  ((and exc.line (not (eq? exc.line #unspecified)))
		   (list url ", line " exc.line))
		  (else
		   url))))

      (<EXCEPTION-FRAME>
	 (<TABLE> :style "width: 100%; font-family: arial; font-size: 10pt; background: #FFFFF7; border-bottom: 1px solid #ccc; overflow: visible"
	    (<COLGROUP>
	       (<COL> :width "64px"))
	    (<TR>
	       (<TD> :style "height: 64px; vertical-align: top; padding-top: 10px; text-align: center"
		  (<IMG> :src (hop-error-icon) :alt "Error"))
	       (<TD> :style "margin:0; padding: 0"
		  (<TABLE> :style "width: 100%"
		     (<TR> (<TD> :style "font-size: 20pt; padding-bottom: 4px"
			      (<SPAN> :style "color: red; font-weight: bold" location)))
		     (<TR> (<TD> :style "font-size: 14pt" (<SPAN> :style "color: #777; font-weight: bold" name) ": " msg))
		     (<TR> (<TD> :style "font-family: monospace; font-size: 11pt" src))
		     (<TR> (<TD> :style "font-family: monospace; color: #777" (properties->string exc)))))))
	 (<DIV> :style "font-family: arial; font-size: 10pt; overflow: visible"
	    (when (and exc.hopService (not (eq? exc.hopService #unspecified)))
	       (<DIV> :style "font-family: arial; font-size: 10pt; padding: 5px"
		  (<DIV> :style "font-weight: bold" "Service:")
		  (<TABLE> :style "width: 100%; font-size: 9pt; overflow: visible; padding-left: 1em"
		     (<TR> (<TD> :style "font-size: 11pt"
			      (obj->string exc.hopService #f))))))
	    (when (pair? exc.hopStack)
	       (<DIV> :style (if (and exc.hopService (not (eq? exc.hopService #unspecified)))
				 "border-top: 1px dashed #ccc; margin-top: 2ex"
				 "margin-top: 2ex")
		  (<EXCEPTION-STACK> exc.hopStack)))
	    (when (string? exc.stack)
	       (<DIV> :style (if (or (and exc.hopService (not (eq? exc.hopService #unspecified)))
				     (pair? exc.hopStack))
				 "border-top: 1px dashed #ccc; margin-top: 2ex"
				 "margin-top: 2ex")
		  (<EXCEPTION-JSSTACK> exc.stack)))))))

;*---------------------------------------------------------------------*/
;*    hop-report-exception ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-report-exception exc)
   ;; the error might be raised before document.body is bound
   (if (and document.body (not (null? document.body)))
       (let ((e (<EXCEPTION> exc)))
	  (dom-append-child! document.body (<EXCEPTION> exc)))
       (add-window-onload! (lambda (e) (hop-report-exception exc)))))

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
	  (< (debug) 2))))

;*---------------------------------------------------------------------*/
;*    install the default error handler ...                            */
;*---------------------------------------------------------------------*/
(when (> (debug) 0)
   ;; on debug install the Hop error handler
   (error-hook-set!
    (lambda (exc _)
       (set! hop-last-exception exc)
       (set! exc.hopStack (hop-get-stack 3))
       exc))
   (set! window.onerror hop-onerror-handler))
