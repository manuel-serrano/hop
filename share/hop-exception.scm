;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/share/hop-exception.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  4 15:51:42 2009                          */
;*    Last change :  Wed Dec 16 21:16:29 2015 (serrano)                */
;*    Copyright   :  2009-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Client-side debugging facility (includes when Hop launched in    */
;*    debug mode).                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop-exception

   (include
      "../runtime/client-exception.sch")
   
   (export
      hop-name-aliases
      hop_current_stack_context
      hop_initial_stack_context
      (hop-callback-handler exc ctx)
      (hop-callback-html-context el file line)
      (hop-callback-listener-context msg)
      (hop-get-exception-stack e)
      (hop-report-exception e stk)
      (bigloo-mangled? str)
      (hop-demangle str)
      (hop-source-map-register! proc))
   
   (js (properties->string hop_properties_to_string)
      (js-arguments arguments)
      "Error"
      "window"
      (hop-config hop_config))
   
   (scheme2js-pragma
      (hop-name-aliases (JS "hop_name_aliases"))
      (hop_initial_stack_context (JS "hop_initial_stack_context"))
      (hop_current_stack_context (JS "hop_current_stack_context"))
      (hop-callback-handler (JS "hop_callback_handler"))
      (hop-callback-html-context (JS "hop_callback_html_context"))
      (hop-callback-listener-context (JS "hop_callback_listener_context"))
      (hop-report-exception (JS "hop_report_exception"))
      (hop-get-exception-stack (JS "hop_get_exception_stack"))
      (hop-mangled? (JS "hop_mangledp"))
      (hop-demangle (JS "hop_demangle"))))

;*---------------------------------------------------------------------*/
;*    hop_current_stack_context ...                                    */
;*---------------------------------------------------------------------*/
(define hop_current_stack_context '())
(define hop_initial_stack_context '())

;*---------------------------------------------------------------------*/
;*    hop-default-source-map ...                                       */
;*---------------------------------------------------------------------*/
(define (hop-default-source-map file line col)
   file)

;*---------------------------------------------------------------------*/
;*    hop-source-map ...                                               */
;*---------------------------------------------------------------------*/
(define hop-source-map hop-default-source-map)

;*---------------------------------------------------------------------*/
;*    hop-source-map-register! ...                                     */
;*---------------------------------------------------------------------*/
(define (hop-source-map-register! proc)
   (set! hop-source-map proc))
   
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
	       (let ((i (string-index id #\.)))
		  (if i
		      (string-append (substring id 0 i) "@" module)
		      (string-append id "@" module))))))
      
      (when (bigloo-mangled? string)
	 (cond
	    ((substring=? string "BgL_" 4)
	     (bigloo-demangle-simple))
	    ((substring=? string "BGl_" 4)
	     (bigloo-demangle-module))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    hop-get-stack ...                                                */
;*    -------------------------------------------------------------    */
;*    This function is called when an exception lacks a "stack"        */
;*    field. This function returns an empty list if "callee"           */
;*    special JavaScript variable is deprecated.                       */
;*---------------------------------------------------------------------*/
(define (hop-get-stack offset . depth)
   (cond
      ((not this)
       ;; nothing we can do in strict mode
       '())
      ((not (js-in? "callee" js-arguments))
       ;; no callee, cannot do much
       '())
      (else
       ;; skip offset frames of the stack
       (with-handler
	  (lambda (e) '())
	  (let loop ((proc js-arguments.callee)
		     (offset offset))
	     (cond
		((= offset -1)
		 ;; grab depth frame of the stack
		 (let loop ((caller proc)
			    (n (if (pair? depth) (car depth) 10))
			    (stack '()))
		    (if (and caller (> n 0))
			(let ((frame `(,caller (type . client))))
			   (loop caller.caller (- n 1) (cons frame stack)))
			(reverse! stack))))
		((and proc (not (eq? proc #unspecified)))
		 (loop proc.caller (- offset 1)))
		(else
		 '())))))))

;*---------------------------------------------------------------------*/
;*    obj->name ...                                                    */
;*---------------------------------------------------------------------*/
(define (obj->name o longp)
   (cond
      ((procedure? o)
       (let ((name (cond
		      ((js-in? "displayName" o)
		       o.displayName)
		      ((and (string? o.name) (> (string-length o.name) 0))
		       (bigloo-demangle o.name))
		      (else
		       "anonymous"))))
	  (if longp
	      (list "(" name " ...)")
	      name)))
      ((string? o)
       o)
      (else
       (with-output-to-string (lambda () (write o))))))

;*---------------------------------------------------------------------*/
;*    <EXCEPTION-STACK> ...                                            */
;*---------------------------------------------------------------------*/
(define (<EXCEPTION-STACK> stack skip)
   
   (define (frame-type rest)
      (with-handler
	 (lambda (e)
	    'client)
	 (let ((c (assq 'type rest)))
	    (if (pair? c)
		(symbol->string (cdr c))
		"client"))))

   (define (frame-klass rest)
      (string-append "hop-exception-frame-"
	 (frame-type rest)))

   (define (js-name s)
      (if (symbol? s)
	  (symbol->string s)
	  s))

   (define (js-file f)
      (pregexp-replace "[?]js=.*$" f ""))

   (define (<TR:LINE> klass name . src)
      (<TR> :class klass
	 (<TD> :class "hop-exception-frame-id"
	    name)
	 (<TD> :class "hop-exception-frame-line"
	    src)))

   (define (pp-stack stack)
      (map (lambda (f)
	      (match-case f
		 ((?name (line-col ?file ?line ?col) . ?rest)
		  (let ((jsline (assq 'js-line-col rest))
			(klass (frame-klass rest))
			(file (js-file file)))
		     (if (and (pair? jsline)
			      (string=? ((@ hop_idiom js)) "scheme"))
			 (list
			    (<TR:LINE> klass (js-name name)
			       (<SPAN> :class "hop-exception-js"
				  file ":" line ":" col))
			    (match-case jsline
			       ((js-line-col ?jsfile ?jsline ?jscol)
				(<TR:LINE> "hop-exception-frame-js" ""
				   jsfile ":" jsline ":" jscol))))
			 (<TR:LINE> klass (js-name name)
			    file ":" line ":" col))))
		 ((?name (line ?file ?line) . ?rest)
		  (let ((jsline (assq 'js-line rest))
			(klass (frame-klass rest))
			(file (js-file file)))
		     (if (and (pair? jsline)
			      (string=? ((@ hop_idiom js)) "scheme"))
			 (list
			    (<TR:LINE> klass (js-name name)
			       (<SPAN> :class "hop-exception-js"
				  file ":" line))
			    (match-case jsline
			       ((js-line ?jsfile ?jsline)
				(<TR:LINE> "hop-exception-frame-js" ""
				   jsfile ":" jsline))))
			 (<TR:LINE> klass (js-name name) file ":" line))))
		 ((?name (at ?file ?point) . ?rest)
		  (let ((klass (frame-klass rest))
			(file (js-file file)))
		     (<TR:LINE> klass (js-name name)
			(<SPAN> :class "hop-exception-js"
			   file "@" point))))
		 ((?name ?loc . ?rest)
		  (<TR:LINE> (frame-klass rest) (js-name name) loc))
		 ((?name . ?rest)
		  (<TR:LINE> (frame-klass rest) (js-name name)))
		 ((? string?)
		  (<TR> (<TH> f)))))
	 stack))

  (let loop ((l stack)
	     (s skip))
     (cond
	((null? l)
	 "")
	((= s 0)
	 (<DIV> :data-hss-class "hop-exception-stack"
	    :data-idiom ((@ hop_idiom js))
	    :data-debug-mode (if (string=? ((@ hop_idiom js)) "scheme")
				 "hop" "all")
	    (if (string=? ((@ hop_idiom js)) "scheme")
		(<BUTTON> "Show JavaScript frames"
		   :onclick ~(let* ((p this.parentNode)
				    (c (p.getAttribute "data-debug-mode")))
				(if (equal? c "all")
				    (begin
				       (innerHTML-set! this "Show JavaScript frames")
				       (p.setAttribute "data-debug-mode" "hop"))
				    (begin
				       (innerHTML-set! this "Hide JavaScript frames")
				       (p.setAttribute "data-debug-mode" "all")))
				(stop-event-propagation event))))
	    (<TABLE> :data-hss-class "hop-exception-stack"
	       :onclick ~(stop-event-propagation event)
	       (<TR> (<TH> "Execution stack:"))
	       (pp-stack l))))
	(else
	 (loop (cdr l) (- s 1))))))

;*---------------------------------------------------------------------*/
;*    <EXCEPTION> ...                                                  */
;*---------------------------------------------------------------------*/
(define (<EXCEPTION> exc stack)
   (<DIV> :data-hss-class "hop-exception-frame"
      :onclick ~(dom-remove-child! (dom-parent-node this) this)
      (<DIV> :data-hss-class "hop-exception-background")
      (<DIV> :data-hss-class "hop-exception" :class "client"
	 (<SPAN> :data-hss-class "hop-exception-img")
	 (<DIV> :data-hss-class "hop-exception-body"
	    ;; error title
	    (<DIV> :data-hss-class "hop-exception-title" "Client Error")
	    ;; error message
	    (<TABLE> :data-hss-class "hop-exception-msg"
	       (<TR> (<TH> exc.name))
	       (<TR> (<TD> (get-exception-message exc)))
	       (let ((name (if (and (js-instanceof? exc (@ Object js))
				    (js-in? "scObject" exc))
			       (obj->name exc.scObject #f)
			       (typeof exc))))
		  (<TR> (<TD> (<TT> name)))))
	    ;; call stack
	    (<EXCEPTION-STACK> stack 0)))))

;*---------------------------------------------------------------------*/
;*    hop-report-exception ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-report-exception exc stack)
   (cond
      ((and document.body (not (null? document.body)))
       ;; regular report
       (dom-append-child! document.body (<EXCEPTION> exc stack)))
      (else
       ;; the error might be raised even before document.body is bound
       (add-event-listener! window "load"
	  (lambda (e)
	     (hop-report-exception exc stack))))))

;*---------------------------------------------------------------------*/
;*    hop-current-exception ...                                        */
;*---------------------------------------------------------------------*/
(define hop-current-exception #f)
(define hop-current-exception-stack #f)

;*---------------------------------------------------------------------*/
;*    hop-onerror-handler ...                                          */
;*---------------------------------------------------------------------*/
(define (hop-onerror-handler msg url line)
   (or (let loop ((i (- (vector-length hop-config.filtered_errors) 1)))
	  (when (>= i 0)
	     (or (eq? url (vector-ref hop-config.filtered_errors i))
		 (loop (- i 1)))))
       ;; build a dummy exception for reporting
       (if hop-current-exception
	   (let ((exc hop-current-exception))
	      (set! hop-current-exception #f)
	      (hop-report-exception exc
		 (append hop-current-exception-stack
		    (list document.location.href))))
	   (hop-report-exception (js-new (@ Error js) msg url line)
	      (list document.location.href)))))

;*---------------------------------------------------------------------*/
;*    hop-get-exception-stack ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-get-exception-stack e)
   (cond
      ((and (js-instanceof? e (@ Object JS)) (js-in? "stack" e))
       (let ((offset (if (js-in? "scOffset" e) e.scOffset 0))
	     (host (string-append document.location.protocol "//"
		      document.location.host)))
	  (list
	     `(,e.stack
		 #f
		 (type . exception)
		 (offset . ,offset)
		 (host . ,host)))))
      (else
       (hop-get-stack 0))))

;*---------------------------------------------------------------------*/
;*    get-exception-message ...                                        */
;*---------------------------------------------------------------------*/
(define (get-exception-message e)
   
   (define (reference-error msg)
      ;; demangle the id part of the error message
      (let ((i (string-index msg " ")))
	 (if (>fx i 0)
	     (let ((id (substring msg 0 i)))
		(string-append (hop-demangle id) (substring msg i)))
	     msg)))

   (define (demangle-string msg)
      (if (not (string? msg))
	  msg
	  (let ((len (string-length msg)))
	     (let loop ((i 0))
		(if (>=fx i len)
		    ""
		    (let ((j (string-index msg " \t\n\"';" i)))
		       (if (not j)
			   (hop-demangle (substring msg i len))
			   (let ((k (string-skip msg " \t\n\"';" j)))
			      (if k
				  (string-append
				     (hop-demangle (substring msg i j))
				     (substring msg j k)
				     (loop k))
				  (string-append
				     (hop-demangle (substring msg i j))
				     (substring msg j len)))))))))))

   (cond
      ((not (isa? e (@ Object js))) (string-append e ""))
      ((isa? e (@ ReferenceError js)) (reference-error e.message))
      ((js-in? "message" e) (demangle-string e.message))
      ((js-in? "description" e) (demangle-string e.description))
      (else (string-append e ""))))

;*---------------------------------------------------------------------*/
;*    hop-callback-handler ...                                         */
;*    -------------------------------------------------------------    */
;*    See HOP_CALLBACK, hop-lib.js.                                    */
;*---------------------------------------------------------------------*/
(define (hop-callback-handler e ctx)
   ;; store the exception for the default handler to display it, don't
   ;; display it now, otherwise we would have to implement a complex
   ;; machinery to prevent hop-onerror-handler to also display it
   (let ((stk (append (hop-get-exception-stack e) ctx)))
      (set! hop-current-exception e)
      (set! hop-current-exception-stack (hop-debug-exception-stack stk)))
   ;; notify the server of the exception and re-throw it
   (when (js-instanceof? e (@ Object js))
      (unless (and (js-in? "scClientOnly" e) e.scClientOnly)
	 (with-hop ($(service :name "public/server-debug/exception"
			(#!key exc proc msg obj stack))
		      :exc e
		      :url (if (js-in? "name" e)
			       (string-append document.location.href ", " e.name)
			       document.location.href)
		      :msg (get-exception-message e)
		      :obj (if (js-in? "scObject" e) e.scObject #unspecified)
		      :stack hop-current-exception-stack))))
   (raise e))

;*---------------------------------------------------------------------*/
;*    hop-callback-html-context ...                                    */
;*---------------------------------------------------------------------*/
(define (hop-callback-html-context site file line)
   (let ((frame (list site `(at ,file ,line) '(format . "~~~a") '(type . html))))
      (cons frame hop_initial_stack_context)))

;*---------------------------------------------------------------------*/
;*    hop-callback-listener-context ...                                */
;*---------------------------------------------------------------------*/
(define (hop-callback-listener-context msg)
   (let ((frame (list msg #f '(format . "~~~a") '(type . html))))
      (cons frame hop_initial_stack_context)))

;*---------------------------------------------------------------------*/
;*    install the default error handler ...                            */
;*---------------------------------------------------------------------*/
(when (>= (hop-debug) 1)
   (set! hop_current_stack_context (list document.location.href))
   (set! window.onerror hop-onerror-handler))
