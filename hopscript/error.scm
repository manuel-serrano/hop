;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/error.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Wed Apr  8 08:27:37 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript errors                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.11        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_error

   (library hop)
   
   (include "types.sch" "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_property
	   __hopscript_array
	   __hopscript_function
	   __hopscript_private
	   __hopscript_lib
	   __hopscript_public
	   __hopscript_worker)

   (static (class JsFrame::JsObject
	      (file::bstring read-only)
	      (line::int read-only)
	      (column::int read-only)
	      (iseval::bool read-only (default #f))
	      (fun::bstring read-only)))
   
   (export (js-init-error! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-stacktracelimit ...                                           */
;*    -------------------------------------------------------------    */
;*    The stackTraceLimit value (how many frames to store when         */
;*    capturing the stack) is shared by all JS workers.                */
;*---------------------------------------------------------------------*/
(define js-stacktracelimit 10)

;*---------------------------------------------------------------------*/
;*    constructor                                                      */
;*---------------------------------------------------------------------*/
(define-instantiate JsFrame)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsError ...                                  */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsError
   (lambda (o ctx)
      (with-access::JsError o (name msg stack fname location %this)
	 (let ((%this (if (isa? ctx JsGlobalObject) ctx %this)))
	    (if (isa? %this JsGlobalObject)
		(vector (string-append "Server" (js-tostring name %this))
		   (js-tostring msg %this)
		   (obj->string stack %this)
		   (js-tostring fname %this)
		   (js-tonumber location %this))
		(error "obj->string ::JsError" "Not a JavaScript context" ctx)))))
   (lambda (o ctx)
      (if (and (vector? o) (=fx (vector-length o) 5))
	  (if (isa? ctx JsGlobalObject)
	      (with-access::JsGlobalObject ctx (js-error)
		 (instantiateJsError
		    (cmap (instantiate::JsConstructMap))
		    (%this ctx)
		    (__proto__ (js-get js-error (& "prototype") ctx))
		    (name (js-string->jsstring (vector-ref o 0)))
		    (msg (js-string->jsstring (vector-ref o 1)))
		    (stack (string->obj (url-decode (vector-ref o 2)) ctx))
		    (fname (js-string->jsstring (vector-ref o 3)))
		    (location (vector-ref o 4))))
	      (instantiate::&error
		 (obj (js-string->jsstring (vector-ref o 0)))
		 (msg (js-string->jsstring (vector-ref o 1)))
		 (stack (string->obj (url-decode (vector-ref o 2)) ctx))
		 (proc (js-string->jsstring (vector-ref o 3)))
		 (location (vector-ref o 4)))))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsError ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsError worker::WorkerHopThread %_this)
   (js-undefined))

;*---------------------------------------------------------------------*/
;*    object-display ::JsError ...                                     */
;*---------------------------------------------------------------------*/
(define-method (object-display o::JsError . port)
   (with-output-to-port (if (null? port) (current-output-port) (car port))
      (lambda ()
	 (with-access::JsError o (name msg fname location)
	    (display* "#<"
	       (class-name (object-class o)) ": " name ", " msg
	       (if (and location fname) (format " ~s:~s" fname location) "")
	       ">")))))

(define-method (object-print o::JsError port pslot)
   (object-display o port))
   
;*---------------------------------------------------------------------*/
;*    exception-notify ::JsError ...                                   */
;*---------------------------------------------------------------------*/
(define-method (exception-notify exc::JsError)
   (with-access::JsError exc (name msg stack fname location %this)
      (if (isa? msg &exception)
	  (exception-notify msg)
	  (with-handler
	     (lambda (e)
		(let ((port (current-error-port)))
		   (display "*** INTERNAL-ERROR: " port)
		   (display "an error occurred while signaling an error!" port)
		   (newline port)
		   (display "The initial error was:" port)
		   (newline port)
		   (display "  " port)
		   (display (js-toname name %this) port)
		   (display " -- " port)
		   (display (js-tostring msg %this) port)
		   (newline port)
		   (display "  " port)
		   (display (js-tostring fname %this) port)
		   (display ":" port)
		   (display location port)
		   (newline port)
		   (display "The reraise error is:" port)
		   (newline port)
		   (exception-notify e)))
	     (let ((notify (lambda ()
			      (let* ((name (js-jsstring->string name))
				     (stk (js-get exc (& "stack") %this))
				     (port (current-error-port)))
				 (cond
				    ((js-jsstring? fname)
				     (display-trace-stack-source
					(list `(,name (at ,(js-jsstring->string fname) ,location)))
					port))
				    ((string? fname)
				     (display-trace-stack-source
					(list `(,name (at ,fname ,location)))
					port)))
				 (if (js-jsstring? stk)
				     (display (js-jsstring->string stk) port)
				     (let ((stack (cond
						     ((string=? name "ReferenceError")
						      stack)
						     ((string=? name "TypeError")
						      stack)
						     (else
						      stack))))
					(fprint port name ": " msg "\n")
					(display-trace-stack stack port)))))))
		(with-access::JsGlobalObject %this (worker)
		   (if worker
		       (js-worker-exec worker "error" #t notify)
		       (notify))))))))

;*---------------------------------------------------------------------*/
;*    exception-notify ::obj ...                                       */
;*---------------------------------------------------------------------*/
(define-method (exception-notify exc::JsObject)
   (with-access::JsObject exc (%this)
      (let* ((%this (js-new-global-object))
	     (msg (js-get exc (& "message") %this))
	     (stack (js-get exc (& "stack") %this))
	     (name (js-get exc (& "name") %this))
	     (port (current-error-port)))
	 (unless (or (eq? name (js-undefined)) (eq? msg (js-undefined)))
	    (fprint port name ": " msg "\n"))
	 (display stack port))))

;*---------------------------------------------------------------------*/
;*    js-init-error! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-init-error! %this::JsGlobalObject)
   ;; local constant strings
   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   
   ;; bind the errors into the global object
   (with-access::JsGlobalObject %this (js-function js-error 
					 js-syntax-error js-type-error
					 js-uri-error js-eval-error
					 js-range-error js-reference-error
					 name)
      
      (define js-function-prototype
	 (js-object-proto js-function))
      
      (define js-error-prototype
	 (instantiateJsError
	    (cmap (instantiate::JsConstructMap))
	    (%this %this)
	    (__proto__ (js-object-proto %this))
	    (name (& "Error"))
	    (msg (& ""))))

      (define js-frame-proto
	 (js-init-frame-proto! %this))
      
      (define (js-error-alloc %this constructor::JsFunction)
	 (with-access::JsGlobalObject %this (js-new-target)
	    (set! js-new-target constructor)
	    (instantiateJsError
	       (cmap (instantiate::JsConstructMap))
	       (%this %this)
	       (name (js-get constructor (& "name") %this))
	       (msg (& ""))
	       (__proto__ (js-get constructor (& "prototype") %this))
	       (stack '()))))

      (define (%js-error this message fname loc)
	 (with-access::JsGlobalObject %this (js-new-target js-error)
	    (if (eq? js-new-target (js-undefined))
		(js-new3 %this js-error message fname loc)
		(js-error-construct/stack this message fname loc))))

      (define (%js-syntax-error this message fname loc)
	 (with-access::JsGlobalObject %this (js-new-target js-syntax-error)
	    (if (eq? js-new-target (js-undefined))
		(js-new3 %this js-syntax-error message fname loc)
		(js-error-construct/stack this message fname loc))))

      (define (%js-type-error this message fname loc)
	 (with-access::JsGlobalObject %this (js-new-target js-type-error)
	    (if (eq? js-new-target (js-undefined))
		(js-new3 %this js-type-error message fname loc)
		(js-error-construct/stack this message fname loc))))

      (define (%js-uri-error this message fname loc)
	 (with-access::JsGlobalObject %this (js-new-target js-uri-error)
	    (if (eq? js-new-target (js-undefined))
		(js-new3 %this js-uri-error message fname loc)
		(js-error-construct/stack this message fname loc))))

      (define (%js-eval-error this message fname loc)
	 (with-access::JsGlobalObject %this (js-new-target js-eval-error)
	    (if (eq? js-new-target (js-undefined))
		(js-new3 %this js-eval-error message fname loc)
		(js-error-construct/stack this message fname loc))))

      (define (%js-range-error this message fname loc)
	 (with-access::JsGlobalObject %this (js-new-target js-range-error)
	    (if (eq? js-new-target (js-undefined))
		(js-new3 %this js-range-error message fname loc)
		(js-error-construct/stack this message fname loc))))

      (define (%js-reference-error this message fname loc)
	 (with-access::JsGlobalObject %this (js-new-target js-reference-error)
	    (if (eq? js-new-target (js-undefined))
		(js-new3 %this js-reference-error message fname loc)
		(js-error-construct/stack this message fname loc))))

      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.11
      (define (js-error-construct this::JsError m f l)
	 (with-access::JsGlobalObject %this (js-new-target)
	    (set! js-new-target (js-undefined)))
	 (with-access::JsError this (msg fname location name)
	    (unless (eq? m (js-undefined))
	       (js-bind! %this this (& "message") :value m :enumerable #f
		  :hidden-class #t)
	       (set! msg m))
	    (when (js-jsstring? f)
	       (set! fname f)
	       (set! location l))
	    (when (string? f)
	       (set! fname (js-string->jsstring f))
	       (set! location l))
	    (when (js-object? f)
	       (set! fname (js-get f (& "filename") %this))
	       (set! location (js-get f (& "pos") %this)))
	    (js-bind! %this this (& "name")
	       :value name
	       :enumerable #f
	       :hidden-class #t))
	 this)
      
      (define (js-error-construct/stack this::JsError message fname loc)
	 (js-error-construct this message fname loc)
	 (capture-stack-trace this (js-undefined))
	 this)
      
      (define (capture-stack-trace err start-fun)
	 
	 (define (make-frame fun loc file line column)
	    (with-access::JsGlobalObject %this (js-object)
	       (let ((obj (instantiateJsFrame
			     (__proto__ js-frame-proto)
			     (file file)
			     (line line)
			     (column column)
			     (fun fun))))
		  (js-put! obj (& "receiver") (js-undefined) #f %this)
		  (js-put! obj (& "fun") (js-string->jsstring fun) #f %this)
		  (js-put! obj (& "pos") (js-string->jsstring loc) #f %this)
		  obj)))
	 
	 (define (hop-frame->js-frame frame)
	    (let ((name (pregexp-match "\\@@([^ ]*) hopscript"
			   (symbol->string! (car frame)))))
	       (cond
		  ((pair? name)
		   (let ((fun (cadr name))
			 (loc (cadr frame)))
		      (make-frame fun (apply format "~a:~a" (cdr loc))
			 (cadr loc) (caddr loc) 0)))
		  ((eq? (car frame) 'hopscript)
		   (make-frame "hopscript" "" "" 0 0))
		  (else
		   #f))))
	 
	 (define (hop-stack->jsstring err stack)
	    (js-string->jsstring
	       (call-with-output-string
		  (lambda (op)
		     (when (js-object? err)
			;; needed for nodejs compatibility
			(let ((head (js-get err (& "name") %this)))
			   (unless (eq? head (js-undefined))
			      (display (js-tostring head %this) op)
			      (display ": " op)))
			(display (js-get err (& "message") %this) op)
			(newline op))
		     (display-trace-stack stack op 1)))))
	 
	 (when (fixnum? js-stacktracelimit)
	    (let ((stack (get-trace-stack js-stacktracelimit)))
	       (js-bind! %this err (& "stack")
		  :get (js-make-function %this
			  (lambda (o)
			     (tprint "ICI2")
			     (let ((prepare (js-get js-error
					       (& "prepareStackTrace") %this)))
				(if (js-procedure? prepare)
				    (let ((frames (js-vector->jsarray
						     (list->vector
							(filter-map hop-frame->js-frame
							   stack))
						     %this)))
				       (js-call2 %this prepare js-error
					  err frames))
				    (hop-stack->jsstring err stack))))
			  (js-function-arity 0 0)
			  (js-function-info :name "stack" :len 0))
		  :set (js-make-function %this
			  (lambda (o v)
			     (js-undefined))
			  (js-function-arity 1 0)
			  (js-function-info :name "stack" :len 2))
		  :enumerable #f
		  :configurable #t
		  :hidden-class #t))))
      
      ;; bind the properties of the prototype
      (js-bind! %this js-error-prototype (& "notify")
	 :value (js-make-function %this
		   (lambda (exn)
		      (exception-notify exn))
		   (js-function-arity 0 0)
		   (js-function-info :name "notify" :len 1))
	 :enumerable #f)
      (js-bind! %this js-error-prototype (& "message")
	 :set (js-make-function %this
		 (lambda (o v)
		    (js-bind! %this o (& "message") :value v :enumerable #f))
		 (js-function-arity 1 0)
		 (js-function-info :name "message" :len 1))
	 :get (js-make-function %this
		 (lambda (o)
		    (if (isa? o JsError)
			(with-access::JsError o (msg) msg)
			(js-undefined)))
		 (js-function-arity 0 0)
		 (js-function-info :name "message" :len 0))
	 :enumerable #f
	 :configurable #t
	 :hidden-class #t)
      (js-bind! %this js-error-prototype (& "name")
	 :set (js-make-function %this
		 (lambda (o v)
		    (js-bind! %this o (& "name") :value v))
		 (js-function-arity 1 0)
		 (js-function-info :name "name" :len 1))
	 :get (js-make-function %this
		 (lambda (o)
		    (if (isa? o JsError)
			(with-access::JsError o (name) name)
			(js-undefined)))
		 (js-function-arity 0 0)
		 (js-function-info :name "name" :len 0))
	 :enumerable #f
	 :hidden-class #t)
      
      ;; then, create a HopScript object
      (set! js-error
	 (js-make-function %this %js-error
	    (js-function-arity 3 0)
	    (js-function-info :name "Error" :len 1)
	    :__proto__ js-function-prototype
	    :prototype js-error-prototype
	    :size 5
	    :alloc js-error-alloc))
      
      (init-builtin-error-prototype! %this js-error js-error-prototype)
      (set! js-syntax-error
	 (js-make-function %this %js-syntax-error
	    (js-function-arity 3 0)
	    (js-function-info :name "SyntaxError" :len 1)
	    :__proto__ js-function-prototype
	    :prototype (instantiateJsError
			  (cmap (instantiate::JsConstructMap))
			  (%this %this)
			  (__proto__ js-error-prototype)
			  (name (& "error"))
			  (msg (& "")))
	    :alloc js-error-alloc))

      (set! js-type-error
	 (js-make-function %this %js-type-error
	    (js-function-arity 3 0)
	    (js-function-info :name "TypeError" :len 1)
	    :__proto__ js-function-prototype
	    :prototype (instantiateJsError
			  (cmap (instantiate::JsConstructMap))
			  (%this %this)
			  (__proto__ js-error-prototype)
			  (name (& "error"))
			  (msg (& "")))
	    :alloc js-error-alloc))

      (set! js-uri-error
	 (js-make-function %this %js-uri-error
	    (js-function-arity 3 0)
	    (js-function-info :name "URIError" :len 1)
	    :__proto__ js-function-prototype
	    :prototype (instantiateJsError
			  (cmap (instantiate::JsConstructMap))
			  (%this %this)
			  (__proto__ js-error-prototype)
			  (name (& "error"))
			  (msg (& "")))
	    :alloc js-error-alloc))

      (set! js-eval-error
	 (js-make-function %this %js-eval-error
	    (js-function-arity 3 0)
	    (js-function-info :name "EvalError" :len 1)
	    :__proto__ js-function-prototype
	    :prototype (instantiateJsError 
			  (cmap (instantiate::JsConstructMap))
			  (%this %this)
			  (__proto__ js-error-prototype)
			  (name (& "error"))
			  (msg (& "")))
	    :alloc js-error-alloc))

      (set! js-range-error
	 (js-make-function %this %js-range-error
	    (js-function-arity 3 0)
	    (js-function-info :name "RangeError" :len 1)
	    :__proto__ js-function-prototype
	    :prototype (instantiateJsError 
			  (cmap (instantiate::JsConstructMap))
			  (%this %this)
			  (__proto__ js-error-prototype)
			  (name (& "error"))
			  (msg (& "")))
	    :alloc js-error-alloc))

      (set! js-reference-error
	 (js-make-function %this %js-reference-error
	    (js-function-arity 3 0)
	    (js-function-info :name "ReferenceError" :len 1)
	    :__proto__ js-function-prototype
	    :prototype (instantiateJsError 
			  (cmap (instantiate::JsConstructMap))
			  (%this %this)
			  (__proto__ js-error-prototype)
			  (name (& "error"))
			  (msg (& "")))
	    :alloc js-error-alloc))

      ;; bind Error in the global object
      (js-bind! %this %this (& "Error") :configurable #f :enumerable #f
	 :value js-error
	 :hidden-class #t)
      (js-bind! %this %this (& "SyntaxError") :configurable #f :enumerable #f
	 :value js-syntax-error
	 :hidden-class #t)
      (js-bind! %this %this (& "TypeError") :configurable #f :enumerable #f
	 :value js-type-error
	 :hidden-class #t)
      (js-bind! %this %this (& "URIError") :configurable #f :enumerable #f
	 :value js-uri-error
	 :hidden-class #t)
      (js-bind! %this %this (& "EvalError") :configurable #f :enumerable #f
	 :value js-eval-error
	 :hidden-class #t)
      (js-bind! %this %this (& "RangeError") :configurable #f :enumerable #f
	 :value js-range-error
	 :hidden-class #t)
      (js-bind! %this %this (& "ReferenceError") :configurable #f :enumerable #f
	 :value js-reference-error
	 :hidden-class #t)
      ;; nodejs addon
      (js-bind! %this js-error (& "stackTraceLimit")
	 :get (js-make-function %this
		 (lambda (o)
		    js-stacktracelimit)
		 (js-function-arity 0 0)
		 (js-function-info :name "stackTraceLimit" :len 0))
	 :set (js-make-function %this
		 (lambda (o v)
		    (set! js-stacktracelimit v))
		 (js-function-arity 1 0)
		 (js-function-info :name "stackTraceLimit" :len 1))
	 :enumerable #t
	 :hidden-class #t)
      (js-bind! %this js-error (& "captureStackTrace")
	 :value (js-make-function %this
		   (lambda (o this start-func)
		      (capture-stack-trace this start-func))
		   (js-function-arity 2 0)
		   (js-function-info :name "captureStackTrace" :len 2))
	 :enumerable #f
	 :hidden-class #t)
      js-error))

;*---------------------------------------------------------------------*/
;*    js-init-frame-proto! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-init-frame-proto! %this::JsGlobalObject)
   (let ((frame-proto (instantiateJsObject
			 (__proto__ (js-object-proto %this))
			 (elements ($create-vector 5)))))
      ;; initialize the frame proto object
      (js-bind! %this frame-proto (& "getFileName")
	 :value (js-make-function %this
		   (lambda (o)
		      (with-access::JsFrame o (file)
			 (js-string->jsstring file)))
		   (js-function-arity 0 0)
		   (js-function-info :name "getFileName" :len 0))
	 :enumerable #t
	 :hidden-class #t)
      (js-bind! %this frame-proto (& "getLineNumber")
	 :value (js-make-function %this
		   (lambda (o)
		      (with-access::JsFrame o (line)
			 line))
		   (js-function-arity 0 0)
		   (js-function-info :name "getLineNumber" :len 0))
	 :enumerable #t
	 :hidden-class #t)
      (js-bind! %this frame-proto (& "getColumnNumber")
	 :value (js-make-function %this
		   (lambda (o)
		      (with-access::JsFrame o (column)
			 column))
		   (js-function-arity 0 0)
		   (js-function-info :name "getColumnNumber" :len 0))
	 :enumerable #t
	 :hidden-class #t)
      (js-bind! %this frame-proto (& "getFunctionName")
	 :value (js-make-function %this
		   (lambda (o)
		      (with-access::JsFrame o (fun)
			 (js-string->jsstring fun)))
		   (js-function-arity 0 0)
		   (js-function-info :name "getFunctionName" :len 0))
	 :enumerable #t
	 :hidden-class #t)
      (js-bind! %this frame-proto (& "isEval")
	 :value (js-make-function %this
		   (lambda (o)
		      (with-access::JsFrame o (iseval)
			 iseval))
		   (js-function-arity 0 0)
		   (js-function-info :name "isEval" :len 0))
	 :enumerable #t
	 :hidden-class #t)
      frame-proto))

;*---------------------------------------------------------------------*/
;*    %js-type-error ...                                               */
;*---------------------------------------------------------------------*/
(define (%js-type-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-type-error name)
	 (js-new %this js-type-error message fname loc))))

;*---------------------------------------------------------------------*/
;*    %js-uri-error ...                                                */
;*---------------------------------------------------------------------*/
(define (%js-uri-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-uri-error name)
	 (js-new %this js-uri-error message fname loc))))

;*---------------------------------------------------------------------*/
;*    %js-eval-error ...                                               */
;*---------------------------------------------------------------------*/
(define (%js-eval-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-eval-error name)
	 (js-new %this js-eval-error message fname loc))))

;*---------------------------------------------------------------------*/
;*    %js-range-error ...                                              */
;*---------------------------------------------------------------------*/
(define (%js-range-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-range-error name)
	 (js-new %this js-range-error message fname loc))))

;*---------------------------------------------------------------------*/
;*    %js-reference-error ...                                          */
;*---------------------------------------------------------------------*/
(define (%js-reference-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-reference-error name)
	 (js-new %this js-reference-error message fname loc))))

;*---------------------------------------------------------------------*/
;*    *js-builtin-error-prototype* ...                                 */
;*---------------------------------------------------------------------*/
(define *js-builtin-error-prototype* #f)

;*---------------------------------------------------------------------*/
;*    js-builtin-error-prototype ...                                   */
;*---------------------------------------------------------------------*/
(define (js-builtin-error-prototype)
   *js-builtin-error-prototype*)

;*---------------------------------------------------------------------*/
;*    init-builtin-error-prototype! ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.11.4      */
;*---------------------------------------------------------------------*/
(define (init-builtin-error-prototype! %this js-error obj)
   ;; prototype fields
   (js-bind! %this obj (& "constructor")
      :value js-error
      :enumerable #f
      :hidden-class #t)
   
   ;; toString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.11.4.4
   (define (error-prototype-tostring this)
      (if (not (js-object? this))
          (bigloo-type-error "toString" "JsObject" this)
          (let* ((name3 (js-get this (& "name") %this))
                 (name4 (if (eq? name3 (js-undefined))
                            (js-ascii->jsstring "Error")
                            (js-tojsstring name3 %this)))
                 (msg5 (if (js-object? this)
                           (js-get this (& "message") %this)
                           (js-undefined)))
                 (msg6 (if (eq? msg5 (js-undefined))
                           ""
                           (js-tostring msg5 %this))))
             (cond
                ((js-jsstring-null? name4) (js-string->jsstring msg6))
                ((string=? msg6 "") name4)
                (else
                 (js-stringlist->jsstring
                    (list (js-jsstring->string name4) ": " msg6)))))))
      
   (js-bind! %this obj (& "toString")
      :value (js-make-function %this error-prototype-tostring
		(js-function-arity error-prototype-tostring)
		(js-function-info :name "toString" :len 1))
      :enumerable #f
      :hidden-class #t)
   
   (set! *js-builtin-error-prototype* obj))

;*---------------------------------------------------------------------*/
;*    js-get ::&error ...                                              */
;*    -------------------------------------------------------------    */
;*    Accessing Bigloo errors from hopscript                           */
;*---------------------------------------------------------------------*/
(define-method (js-get o::&error prop %this)
   (case (js-toname prop %this)
      ((message)
       (with-access::&error o (msg)
	  (js-string->jsstring
	     (if (string? msg)
		 msg
		 (call-with-output-string
		    (lambda (op)
		       (display msg op)))))))
      (else
       (call-next-method))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
