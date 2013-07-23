;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/share/hop-exception.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  4 15:51:42 2009                          */
;*    Last change :  Tue Jul 23 11:04:53 2013 (serrano)                */
;*    Copyright   :  2009-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Client-side debugging facility (includes when Hop launched in    */
;*    debug mode).                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop-exception
   
   (export
      hop-name-aliases
      (hop-callback-handler exc ctx)
      (hop-callback-handler/location exc site file line)
      (bigloo-mangled? str)
      (hop-demangle str)
      (<EXCEPTION-FRAME> . args))
   
   (js (properties->string hop_properties_to_string)
      (js-arguments arguments)
      "Error"
      "window"
      (hop-config hop_config))
   
   (scheme2js-pragma
      (hop-name-aliases (JS "hop_name_aliases"))
      (hop-callback-handler (JS "hop_callback_handler"))
      (hop-callback-handler/location (JS "hop_callback_handler_location"))
      (hop-mangled? (JS "hop_mangledp"))
      (hop-demangle (JS "hop_demangle"))
      (<EXCEPTION-FRAME> (JS "hop_make_exception_frame"))))

;*---------------------------------------------------------------------*/
;*    hop-name-aliases ...                                             */
;*---------------------------------------------------------------------*/
(define hop-name-aliases
   '(("hop_request_onready" . "changed dynamically in hop_request_onready")
     ("hop_add_event_listener" . "add-event-listener!")
     ("hop_innerHTML_set" . "innerHTML-set!")))

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
      
      (when (bigloo-mangled? string)
	 (cond
	    ((substring=? string "BgL_" 4)
	     (bigloo-demangle-simple))
	    ((substring=? string "BGl_" 4)
	     (bigloo-demangle-module))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    scheme2js-mangled? ...                                           */
;*---------------------------------------------------------------------*/
(define (scheme2js-mangled? str)
   (string-prefix? "sc_" str))

;*---------------------------------------------------------------------*/
;*    scheme2js-demangle ...                                           */
;*---------------------------------------------------------------------*/
(define (scheme2js-demangle str)
   
   (define (is-has-bang str)
      (cond
	 ((string-prefix? "is-" str)
	  (is-has-bang
	     (string-append (substring str 3 (string-length str)) "?")))
	 ((string-prefix? "has-" str)
	  (is-has-bang (string-append str "?")))
	 ((string-suffix? "-bang" str)
	  (is-has-bang
	     (string-append (substring str 0 (- (string-length str) 5)) "!")))
	 (else
	  str)))

   (when (scheme2js-mangled? str)
      (let loop ((chars (list-tail (string->list str) 3))
		 (rev-res '()))
	 (if (null? chars)
	     (let ((res (list->string (reverse! rev-res))))
		(is-has-bang res))
	     (cond
		((and (char-upper-case? (car chars)))
		 (loop (cdr chars)
		    (cons (char-downcase (car chars)) (cons #\- rev-res))))
		((eq? (car chars) #\_)
		 (loop (cdr chars)
		    (cons #\- rev-res)))
		((eq? (car chars) #\2)
		 (loop (cdr chars)
		    (cons #\> (cons #\- rev-res))))
		(else
		 (loop (cdr chars)
		    (cons (car chars) rev-res))))))))

;*---------------------------------------------------------------------*/
;*    hop-demangle ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop-demangle str)
   (or (bigloo-demangle str)
       (scheme2js-demangle str)
       (let ((a (assoc str hop-name-aliases)))
	  (if (pair? a)
	      (cdr a)
	      str))))

;*---------------------------------------------------------------------*/
;*    client-demangle ...                                              */
;*---------------------------------------------------------------------*/
(define (client-demangle str)
   (let ((dm (hop-demangle str)))
      (if (eq? dm str)
	  (<SPAN> :class "hop-error-frame-js" str)
	  (<SPAN> :class "hop-error-frame-hop" dm))))

;*---------------------------------------------------------------------*/
;*    hop-get-stack ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-get-stack offset . depth)
   (if (not this)
       ;; nothing we can do in strict mode
       '()
       ;; skip offset frames of the stack
       (let loop ((proc js-arguments.callee)
		  (offset offset))
	  (cond
	     ((= offset -1)
	      ;; grab depth frame of the stack
	      (let loop ((caller proc)
			 (n (if (pair? depth) (car depth) 10))
			 (stack '()))
		 (if (and caller (> n 0))
		     (let ((frame (cons caller (vector->list caller.arguments))))
			(loop caller.caller (- n 1) (cons frame stack)))
		     (reverse! stack))))
	     ((and proc (not (eq? proc #unspecified)))
	      (loop proc.caller (- offset 1)))
	     (else
	      '())))))

;*---------------------------------------------------------------------*/
;*    <EXCEPTION-FRAME> ...                                            */
;*---------------------------------------------------------------------*/
(define (<EXCEPTION-FRAME> . args)
   (<DIV> :onclick (dom-remove-child! (dom-parent-node this) this)
      (<DIV> :hssclass "hop-error-frame")
      args))

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
	  (if (and longp (string? o.sc_location))
	      (let ((m (pregexp-match "[(]at ([^ ]+) ([^ ]+)[)]" o.sc_location)))
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
(define (<EXCEPTION-STACK> stack skip)
   
   (define (pp-stack stack)
      (map (lambda (f)
	      (<SPAN> (car f) ": "
		 (when (pair? (cdr f))
		    (let ((loc (cdr f)))
		       (<SPAN> :style "color: #777"
			  (cadr loc) " " (caddr loc))))
		 "\n"))
	 stack))

   (let loop ((l stack)
	      (s skip))
      (cond
	 ((null? l)
	  "")
	 ((= s 0)
	  (<DIV> :hssclass "hop-error-trace"
	     (<DIV> "JavaScript stack:")
	     (<PRE> 
		:onclick (stop-event-propagation event)
		(pp-stack l))))
	 (else
	  (loop (cdr l) (- s 1))))))

;*---------------------------------------------------------------------*/
;*    <EXCEPTION> ...                                                  */
;*---------------------------------------------------------------------*/
(define (<EXCEPTION> exc ctx)
   (let* ((url (if (js-in? "fileName" exc)
		   exc.fileName
		   document.location.href))
	  (src (cond
		  ((js-in? "charNumber" exc)
		   (list (<A> :href url url) ", char " exc.charNumber))
		  ((js-in? "lineNumber" exc)
		   (list (<A> :href url url) ", line " exc.lineNumber))
		  ((js-in? "line" exc)
		   (list (<A> :href url url) ", line " exc.line))
		  (else
		   (<A> :href url)))))
      (<EXCEPTION-FRAME>
	 (<DIV> :hssclass "hop-error" :class "client"
	    (<SPAN> :hssclass "hop-error-img")
	    (<DIV>
	       ;; error title
	       (<DIV> :hssclass "hop-error-title" "Client Error")
	       ;; error message
	       (<DIV> :hssclass "hop-error-msg"
		  (<TABLE> :style "font-weight: normal"
		     (<TR>
			(<TD>
			   (<SPAN> :style "color: #777; font-weight: bold"
			      exc.name)
			   ": "
			   exc.message))
		     (when (js-in? "scObject" exc)
			(<TR> (<TD> (<TT> (obj->name exc.scObject #f)))))
		     (<TR> (<TD> src))))
	       ;; exception service
	       (when (js-in? "hopService" exc)
		  (<DIV> :hssclass "hop-error-trace"
		     (<DIV> "Service:")
		     (<PRE> (obj->name exc.hopService #f))))
	       ;; call stack
	       (<EXCEPTION-STACK>
		  (append-stack-context
		     (get-exception-stack exc client-demangle)
		     ctx)
		  0))))))

;*---------------------------------------------------------------------*/
;*    hop-report-exception ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-report-exception exc ctx)
   (cond
      ((and document.body (not (null? document.body)))
       ;; regular report
       (dom-append-child! document.body (<EXCEPTION> exc ctx)))
      (else
       ;; the error might be raised even before document.body is bound
       (add-event-listener! window "load"
	  (lambda (e)
	     (hop-report-exception exc ctx))))))

;*---------------------------------------------------------------------*/
;*    hop-last-exception ...                                           */
;*---------------------------------------------------------------------*/
(define hop-last-exception #f)
(define hop-last-context #f)

;*---------------------------------------------------------------------*/
;*    hop-onerror-handler ...                                          */
;*---------------------------------------------------------------------*/
(define (hop-onerror-handler msg url line)
   (or (let loop ((i (- (vector-length hop-config.filtered_errors) 1)))
	  (when (>= i 0)
	     (or (eq? url (vector-ref hop-config.filtered_errors i))
		 (loop (- i 1)))))
       ;; build a dummy exception for reporting
       (if hop-last-exception
	   (let ((exc hop-last-exception))
	      (set! hop-last-exception #f)
	      (hop-report-exception exc hop-last-context))
	   (hop-report-exception (js-new (@ Error js) msg url line) #f))))

;*---------------------------------------------------------------------*/
;*    *firefox-stack-frame-regexp* ...                                 */
;*---------------------------------------------------------------------*/
(define *firefox-stack-frame-regexp*
   (pregexp "^([^ ]+)@([^ ]+)$"))

(define *firefox-frame-location-regexp*
   (pregexp "(.+):([0-9]+)$"))

;*---------------------------------------------------------------------*/
;*    *chrome-stack-regexp* ...                                        */
;*---------------------------------------------------------------------*/
(define *chrome-stack-regexp*
   (pregexp "^.+\n[ \t]+at "))

(define *chrome-stack-frame-regexp*
   (pregexp "^[ \t]+at ([^ ]+) [(](.+)[)]"))

(define *chrome-frame-location-regexp*
   (pregexp "(.+):([0-9]+:[0-9]+)$"))

;*---------------------------------------------------------------------*/
;*    get-exception-stack ...                                          */
;*---------------------------------------------------------------------*/
(define (get-exception-stack e demangle)

   (define host
      (string-append document.location.protocol "//"
	 document.location.host))
   
   (define (filename f)
      (let ((i (string-index f "?")))
	 (if (integer? i)
	     (substring f 0 i)
	     f)))
   
   (define (abspath host f)
      (if (string-prefix? host f)
	  (substring f (string-length host))
	  f))

   (define (firefox-frame f)
      
      (define (firefox-demangle id)
	 (let ((i (string-index id "/")))
	    (if (integer? i)
		(let* ((pref (substring id 0 i))
		       (dm (demangle pref)))
		   (if (string-suffix? "/sc_let" id)
		       (string-append dm " (let)")
		       (string-append dm (substring id i))))
		(demangle id))))
      
      (let ((m (pregexp-match *firefox-stack-frame-regexp* f)))
	 (when (pair? m)
	    (let* ((id (cadr m))
		   (loc (caddr m))
		   (dm (firefox-demangle id))
		   (l (pregexp-match *firefox-frame-location-regexp* loc)))
	       (if (pair? l)
		   (cons dm `(line
				,(filename (abspath host (cadr l)))
				,(string->integer (caddr l))))
		   (list dm))))))

   (define (chrome-frame f)
      
      (let ((m (pregexp-match *chrome-stack-frame-regexp* f)))
	 (when (pair? m)
	    (let* ((id (cadr m))
		   (loc (caddr m))
		   (dm (demangle id))
		   (l (pregexp-match *chrome-frame-location-regexp* loc)))
	       (if (pair? l)
		   (cons dm `(line-col
				,(filename (abspath host (cadr l)))
				,(caddr l)))
		   (list dm))))))
	       
   (cond
      ((and (js-instanceof? e (@ Object JS)) (js-in? "stack" e))
       (let ((lines (string-split e.stack "\n"))
	     (offset (if (js-in? "scOffset" e) e.scOffset 0)))
	  (cond
	     ((null? lines)
	      '())
	     ((pregexp-match *firefox-stack-frame-regexp* (car lines))
	      (filter-map firefox-frame (list-tail lines offset)))
	     ((pregexp-match *chrome-stack-regexp* e.stack)
	      (filter-map chrome-frame (list-tail (cdr lines) offset)))
	     (else
	      (map (lambda (l)
		      (list l #f))
		 (list-tail lines offset))))))
      (else
       (hop-get-stack 0))))

;*---------------------------------------------------------------------*/
;*    append-stack-context ...                                         */
;*---------------------------------------------------------------------*/
(define (append-stack-context stack context)
   (cond
      ((not context)
       stack)
      ((string? context)
       (append stack (list (list context))))
      (else
       (append stack (list context)))))

;*---------------------------------------------------------------------*/
;*    hop-get-exception-message ...                                    */
;*---------------------------------------------------------------------*/
(define (hop-get-exception-message e)
   (cond
      ((js-in? "message" e) e.message)
      ((js-in? "description" e) e.description)
      (else e)))

;*---------------------------------------------------------------------*/
;*    hop-callback-handler ...                                         */
;*    -------------------------------------------------------------    */
;*    See HOP_CALLBACK, hop-lib.js.                                    */
;*---------------------------------------------------------------------*/
(define (hop-callback-handler e ctx)
   ;; store the exception for the default handler to display it, don't
   ;; dispplay it now, otherwise we would have to implement a complex
   ;; machinery to prevent hop-onerror-handler to also display it
   (set! hop-last-exception e)
   (set! hop-last-context ctx)
   ;; notify the server of the exception and re-throw it
   (let ((stk (append-stack-context (get-exception-stack e hop-demangle) ctx)))
      (with-hop ($(service :name "public/server-debug/exception"
		     (#!key exc proc msg obj stack))
		   :exc e
		   :url (if (js-in? "name" e)
			    (string-append document.location.href ", " e.name)
			    document.location.href)
		   :msg (hop-get-exception-message e)
		   :obj (if (js-in? "scObject" e) e.scObject #unspecified)
		   :stack stk))
      (raise e)))

;*---------------------------------------------------------------------*/
;*    hop-callback-handler/location ...                                */
;*---------------------------------------------------------------------*/
(define (hop-callback-handler/location e site file line)
   (hop-callback-handler e (cons site `(at ,file ,line))))
   
;*---------------------------------------------------------------------*/
;*    install the default error handler ...                            */
;*---------------------------------------------------------------------*/
(when (>= (hop-debug) 1)
   (set! window.onerror hop-onerror-handler))
