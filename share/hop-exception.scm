;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/share/hop-exception.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  4 15:51:42 2009                          */
;*    Last change :  Fri Aug  2 17:34:35 2013 (serrano)                */
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
      hop-current-stack-context
      (hop-callback-handler exc ctx)
      (hop-callback-html-context el file line)
      (hop-append-stack-context stk ctx)
      (hop-get-exception-stack e)
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
      (hop-current-stack-context (JS "hop_current_stack_context"))
      (hop-callback-handler (JS "hop_callback_handler"))
      (hop-callback-html-context (JS "hop_callback_html_context"))
      (hop-get-exception-stack (JS "hop_get_exception_stack"))
      (hop-append-stack-context (JS "hop_append_stack_context"))
      (hop-mangled? (JS "hop_mangledp"))
      (hop-demangle (JS "hop_demangle"))))

;*---------------------------------------------------------------------*/
;*    hop-name-aliases ...                                             */
;*---------------------------------------------------------------------*/
(define hop-name-aliases
   '(("hop_trigger_servevt" . "changed dynamically in hop_trigger_servevt")
     ("hop_add_event_listener" . "add-event-listener!")
     ("hop_innerHTML_set" . "innerHTML-set!")))

;*---------------------------------------------------------------------*/
;*    hop-current-stack-context ...                                    */
;*---------------------------------------------------------------------*/
(define hop-current-stack-context #f)

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
;*    hop-get-stack ...                                                */
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
	  (lambda (e)
	     '())
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
			(klass (frame-klass rest)))
		     (if (pair? jsline)
			 (list
			    (<TR:LINE> klass (js-name name)
			       file ":" line ":" col)
			    (match-case jsline
			       ((js-line-col ?jsfile ?jsline ?jscol)
				(<TR:LINE> "hop-exception-frame-js" ""
				   jsfile ":" jsline ":" jscol))))
			 (<TR:LINE> klass (js-name name)
			    file ":" line ":" col))))
		 ((?name (line ?file ?line) . ?rest)
		  (let ((jsline (assq 'js-line rest))
			(klass (frame-klass rest)))
		     (if (pair? jsline)
			 (list
			    (<TR:LINE> klass (js-name name) file ":" line)
			    (match-case jsline
			       ((js-line ?jsfile ?jsline)
				(<TR:LINE> "hop-exception-frame-js" ""
				   jsfile ":" jsline))))
			 (<TR:LINE> klass (js-name name) file ":" line))))
		 ((?name (at ?file ?point) . ?rest)
		  (let ((klass (frame-klass rest)))
		     (<TR:LINE> klass (js-name name) file "@" point)))
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
	     (<BUTTON> "Show JavaScript frames"
		:onclick (let* ((p this.parentNode)
				(c (p.getAttribute "data-debug-mode")))
			    (if (equal? c "all")
				(begin
				   (innerHTML-set! this "Show JavaScript frames")
				   (p.setAttribute "data-debug-mode" "hop"))
				(begin
				   (innerHTML-set! this "Hide JavaScript frames")
				   (p.setAttribute "data-debug-mode" "all")))
			    (stop-event-propagation event)))
	     (<TABLE> :data-hss-class "hop-exception-stack"
		:onclick (stop-event-propagation event)
		(<TR> (<TH> "Execution Stack:"))
		(pp-stack l))))
	 (else
	  (loop (cdr l) (- s 1))))))

;*---------------------------------------------------------------------*/
;*    <EXCEPTION> ...                                                  */
;*---------------------------------------------------------------------*/
(define (<EXCEPTION> exc ctx)
   (<DIV> :data-hss-class "hop-exception-frame"
      :onclick (dom-remove-child! (dom-parent-node this) this)
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
	       (when (js-in? "scObject" exc)
		  (<TR>
		     (<TD> (<TT> (obj->name exc.scObject #f))))))
	    ;; call stack
	    (<EXCEPTION-STACK>
	       (hop-append-stack-context (hop-get-exception-stack exc) ctx)
	       0)))))

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
   (pregexp "(.+):([0-9]+):([0-9]+)$"))

;*---------------------------------------------------------------------*/
;*    hop-get-exception-stack ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-get-exception-stack e)

   (define host
      (string-append document.location.protocol "//"
	 document.location.host))
   
   (define (abspath host f)
      (if (string-prefix? host f)
	  (substring f (string-length host))
	  f))

   (define (firefox-frame f)
      
      (define (firefox-demangle id)
	 (let ((i (string-index id "/")))
	    (if (integer? i)
		(let* ((pref (substring id 0 i))
		       (dm (hop-demangle pref)))
		   (if (string-suffix? "/sc_let" id)
		       (string-append dm " (let)")
		       (string-append dm (hop-demangle (substring id i)))))
		(hop-demangle id))))
      
      (let ((m (pregexp-match *firefox-stack-frame-regexp* f)))
	 (when (pair? m)
	    (let* ((id (cadr m))
		   (loc (caddr m))
		   (dm (firefox-demangle id))
		   (l (pregexp-match *firefox-frame-location-regexp* loc)))
	       (if (pair? l)
		   (let ((file (abspath host (cadr l)))
			 (line (string->integer (caddr l))))
		      (multiple-value-bind (srcfile srcline _)
			 (hop-source-map file line 0)
			 (if (string? srcfile)
			     (list dm
				`(line ,srcfile ,srcline)
				`(js-line ,file ,(-fx line 1))
				`(type . ,(if (string=? dm id) 'js 'client))
				'(format . "~~~a"))
			     (list dm
				`(line ,file ,(-fx line 1))
				`(type . ,(if (string=? dm id) 'js 'client))
				'(format . "~~~a")))))
		   (list dm))))))

   (define (chrome-frame f)
      (let ((m (pregexp-match *chrome-stack-frame-regexp* f)))
	 (when (pair? m)
	    (let* ((id (cadr m))
		   (loc (caddr m))
		   (dm (hop-demangle id))
		   (l (pregexp-match *chrome-frame-location-regexp* loc)))
	       (if (pair? l)
		   ;; source map the file position
		   (let ((file (abspath host (cadr l)))
			 (line (string->integer (caddr l)))
			 (col (string->integer (cadddr l))))
		      (multiple-value-bind (srcfile srcline srccol)
			 (hop-source-map file line col)
			 (if (string? srcfile)
			     (list dm
				`(line-col ,srcfile ,srcline ,srccol)
				`(js-line-col ,file ,(-fx line 1), col)
				`(type . ,(if (string=? dm id) 'js 'client))
				'(format . "~~~a"))
			     (list dm
				`(line-col ,file ,(-fx line 1), col)
				`(type . ,(if (string=? dm id) 'js 'client))
				'(format . "~~~a")))))
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
		      (list l #f '(format . "~~~a") '(type . js)))
		 (list-tail lines offset))))))
      (else
       (hop-get-stack 0))))

;*---------------------------------------------------------------------*/
;*    hop-append-stack-context ...                                     */
;*---------------------------------------------------------------------*/
(define (hop-append-stack-context stack context)
   (cond
      ((or (not context) (eq? context #unspecified))
       stack)
      ((string? context)
       (append stack (list (list context))))
      ((isa? context (@ Error js))
       (let* ((estack (hop-get-exception-stack context))
	      (nstack (append stack (cons "Client Trace:" estack))))
	  (hop-append-stack-context nstack context.precontext)))
      (else
       (append stack context))))

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
   (cond
      ((isa? e (@ ReferenceError js)) (reference-error e.message))
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
   (let ((stk (hop-append-stack-context (hop-get-exception-stack e) ctx)))
      (with-hop ($(service :name "public/server-debug/exception"
		     (#!key exc proc msg obj stack))
		   :exc e
		   :url (if (js-in? "name" e)
			    (string-append document.location.href ", " e.name)
			    document.location.href)
		   :msg (get-exception-message e)
		   :obj (if (js-in? "scObject" e) e.scObject #unspecified)
		   :stack stk))
      (raise e)))

;*---------------------------------------------------------------------*/
;*    hop-callback-html-context ...                                    */
;*---------------------------------------------------------------------*/
(define (hop-callback-html-context site file line)
   (let ((frame (list site `(at ,file ,line) '(format . "~~~a") '(type . html))))
      (list frame)))
   
;*---------------------------------------------------------------------*/
;*    install the default error handler ...                            */
;*---------------------------------------------------------------------*/
(when (>= (hop-debug) 1)
   (set! window.onerror hop-onerror-handler))
