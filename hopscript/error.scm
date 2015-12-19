;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/error.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Wed Dec 16 21:11:56 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
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
   
   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_property
	   __hopscript_array
	   __hopscript_function
	   __hopscript_private
	   __hopscript_public)

   (static (class JsFrame::JsObject
	      (file::bstring read-only)
	      (line::int read-only)
	      (column::int read-only)
	      (iseval::bool read-only (default #f))
	      (fun::bstring read-only)))
   
   (export (js-init-error! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsError ...                                  */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsError
   (lambda (o ctx)
      (let ((%this (js-initial-global-object)))
	 (with-access::JsError o (name msg stack fname location)
	    (vector (string-append "Server" (js-tostring name %this))
	       (js-tostring msg %this)
	       (obj->string stack %this)
	       (js-tostring fname %this)
	       (js-tonumber location %this)))))
   (lambda (o ctx)
      (if (and (vector? o) (=fx (vector-length o) 5))
	  (if (eq? ctx 'hop)
	      (instantiate::&error
		 (obj (js-string->jsstring (vector-ref o 0)))
		 (msg (js-string->jsstring (vector-ref o 1)))
		 (stack (string->obj (url-decode (vector-ref o 2)) ctx))
		 (proc (js-string->jsstring (vector-ref o 3)))
		 (location (vector-ref o 4)))
	      (with-access::JsGlobalObject ctx (js-error)
		 (instantiate::JsError
		    (__proto__ (js-get js-error 'prototype ctx))
		    (name (js-string->jsstring (vector-ref o 0)))
		    (msg (js-string->jsstring (vector-ref o 1)))
		    (stack (string->obj (url-decode (vector-ref o 2)) ctx))
		    (fname (js-string->jsstring (vector-ref o 3)))
		    (location (vector-ref o 4)))))
	  (error "Error" "wrong error" o))))

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
	 (with-access::JsError o (name msg fname)
	    (display* "#<"
	       (class-name (object-class o)) ": " name ", " msg ">")))))

(define-method (object-print o::JsError port pslot)
   (object-display o port))
   
;*---------------------------------------------------------------------*/
;*    exception-notify ::obj ...                                       */
;*---------------------------------------------------------------------*/
(define-method (exception-notify exc::JsError)
   (with-access::JsError exc (name msg stack fname location)
      (if (isa? msg &exception)
	  (exception-notify msg)
	  (let* ((name (js-jsstring->string name))
		 (%this (js-new-global-object))
		 (stk (js-get exc 'stack %this))
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
	     (if (isa? stk JsStringLiteral)
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

;*---------------------------------------------------------------------*/
;*    exception-notify ::obj ...                                       */
;*---------------------------------------------------------------------*/
(define-method (exception-notify exc::JsObject)
   (let* ((%this (js-new-global-object))
	  (msg (js-get exc 'message %this))
	  (stack (js-get exc 'stack %this))
	  (name (js-get exc 'name %this))
	  (port (current-error-port)))
      (unless (or (eq? name (js-undefined)) (eq? msg (js-undefined)))
	  (fprint port name ": " msg "\n"))
      (display stack port)))

;*---------------------------------------------------------------------*/
;*    js-init-error! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-init-error! %this::JsGlobalObject)
   ;; bind the errors into the global object
   (with-access::JsGlobalObject %this (__proto__ js-error js-function
					 js-syntax-error js-type-error
					 js-uri-error js-eval-error
					 js-range-error js-reference-error)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 (define js-error-prototype
	    (instantiate::JsError
	       (__proto__ __proto__)
	       (msg (js-string->jsstring ""))
	       (extensible #t)))
	 
	 (define (js-error-alloc constructor::JsFunction)
	    (with-access::JsFunction constructor (name)
	       (instantiate::JsError
		  (name (js-string->jsstring name))
		  (msg (js-string->jsstring ""))
		  (__proto__ (js-get constructor 'prototype %this))
		  (stack '()))))
	 
	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.11
	 (define (js-error-construct this::JsError . args)
	    (with-access::JsError this (msg fname location name)
	       (match-case args
		  ((?m)
		   (unless (eq? m (js-undefined))
		      (js-bind! %this this 'message :value m :enumerable #f)
		      (set! msg m)))
		  ((?m (and (? string?) ?f) ?l)
		   (unless (eq? m (js-undefined))
		      (js-bind! %this this 'message :value m :enumerable #f)
		      (set! msg m))
		   (set! fname (js-string->jsstring f))
		   (set! location l))
		  ((?m (and (? js-jsstring?) ?f) ?l)
		   (unless (eq? m (js-undefined))
		      (js-bind! %this this 'message :value m :enumerable #f)
		      (set! msg m))
		   (set! fname f)
		   (set! location l))
		  ((?m . ?-)
		   (unless (eq? m (js-undefined))
		      (js-bind! %this this 'message :value m :enumerable #f)
		      (set! msg m))))

	       (js-bind! %this this 'name
		  :value name
		  :enumerable #f))
	    this)

	 (define (js-error-construct/stack this::JsError . args)
	    (let ((err (apply js-error-construct this args)))
	       (capture-stack-trace err (js-undefined))
	       err))

	 (define (capture-stack-trace err start-fun)
	    
	    (define frame-proto
	       (with-access::JsGlobalObject %this (js-object)
		  (js-new0 %this js-object)))
	    
	    (define (make-frame fun loc file line column)
	       (with-access::JsGlobalObject %this (js-object)
		  (let ((obj (instantiate::JsFrame
				(__proto__ frame-proto)
				(file file)
				(line line)
				(column column)
				(fun fun))))
		     (js-put! obj 'receiver (js-undefined) #f %this)
		     (js-put! obj 'fun (js-string->jsstring fun) #f %this)
		     (js-put! obj 'pos (js-string->jsstring loc) #f %this)
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
		      (js-undefined)))))
	    
	    (define (hop-stack->jsstring err stack)
	       (js-string->jsstring
		  (call-with-output-string
		     (lambda (op)
			(when (isa? err JsObject)
			   (let ((head (js-get err 'name %this)))
			      (unless (eq? head (js-undefined))
				 (display (js-tostring head %this) op)
				 (display ": " op)))
			   (display (js-get err 'message %this) op)
			   (newline op))
			(display-trace-stack stack op 1)))))
	    
	    ;; initialize the frame proto object
	    (js-bind! %this frame-proto 'getFileName
	       :value (js-make-function %this
			 (lambda (o)
			    (with-access::JsFrame o (file)
			       (js-string->jsstring file)))
			 0 'getFileName)
	       :enumerable #t)
	    (js-bind! %this frame-proto 'getLineNumber
	       :value (js-make-function %this
			 (lambda (o)
			    (with-access::JsFrame o (line)
			       line))
			 0 'getLineNumber)
	       :enumerable #t)
	    (js-bind! %this frame-proto 'getColumnNumber
	       :value (js-make-function %this
			 (lambda (o)
			    (with-access::JsFrame o (column)
			       column))
			 0 'getColumnNumber)
	       :enumerable #t)
	    (js-bind! %this frame-proto 'getFunctionName
	       :value (js-make-function %this
			 (lambda (o)
			    (with-access::JsFrame o (fun)
			       (js-string->jsstring fun)))
			 0 'getFunctionName)
	       :enumerable #t)
	    (js-bind! %this frame-proto 'isEval
	       :value (js-make-function %this
			 (lambda (o)
			    (with-access::JsFrame o (iseval)
			       iseval))
			 0 'isEval)
	       :enumerable #t)
	    
	    
	    (let ((limit (js-get js-error 'stackTraceLimit %this)))
	       (cond
		  ((eq? limit (js-undefined))
		   (set! limit 10))
		  ((not (integer? limit))
		   (set! limit 10))
		  ((or (< limit 0) (> limit 10000))
		   (set! limit 10)))
	       (let ((stack (get-trace-stack limit)))
		  (js-bind! %this err 'stack
		     :get (js-make-function %this
			     (lambda (o)
				(let ((prepare (js-get js-error
						  'prepareStackTrace %this)))
				   (if (isa? prepare JsFunction)
				       (let ((frames (js-vector->jsarray
							(list->vector
							   (filter-map hop-frame->js-frame
							      stack))
							%this)))
					  (js-call2 %this prepare js-error
					     err frames))
				       (hop-stack->jsstring err stack))))
			     0 'get)
		     :set (js-make-function %this
			     (lambda (o v)
				(js-undefined))
			     2 'set)
		     :enumerable #f
		     :configurable #f))))
	 
	 ;; bind the properties of the prototype
	 (js-bind! %this js-error-prototype 'message
	    :set (js-make-function %this
		    (lambda (o v)
		       (js-bind! %this o 'message :value v :enumerable #f))
		    1 'message)
	    :get (js-make-function %this
		    (lambda (o)
		       (if (isa? o JsError)
			   (with-access::JsError o (msg) msg)
			   (js-undefined)))
		    0 'message)
	    :enumerable #f
	    :configurable #t)
	 (js-bind! %this js-error-prototype 'name
	    :set (js-make-function %this
		    (lambda (o v)
		       (js-bind! %this o 'name :value v))
		    1 'name)
	    :get (js-make-function %this
		    (lambda (o)
		       (if (isa? o JsError)
			   (with-access::JsError o (name) name)
			   (js-undefined)))
		    0 'name)
	    :enumerable #f)
	 
	 ;; then, create a HopScript object
	 (set! js-error
	    (js-make-function %this (%js-error %this) 1
	       'Error
	       :__proto__ js-function-prototype
	       :prototype js-error-prototype
	       :alloc js-error-alloc
	       :construct js-error-construct/stack))
	 (init-builtin-error-prototype! %this js-error js-error-prototype)
	 (set! js-syntax-error
	    (js-make-function %this (%js-syntax-error %this) 1
	       'SyntaxError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (msg (js-string->jsstring ""))
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct/stack))
	 (set! js-type-error
	    (js-make-function %this (%js-type-error %this) 1
	       'TypeError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (msg (js-string->jsstring ""))
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct/stack))
	 (set! js-uri-error
	    (js-make-function %this (%js-uri-error %this) 1
	       'URIError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (msg (js-string->jsstring ""))
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct/stack))
	 (set! js-eval-error
	    (js-make-function %this (%js-eval-error %this) 1
	       'EvalError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (msg (js-string->jsstring ""))
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct/stack))
	 (set! js-range-error
	    (js-make-function %this (%js-range-error %this) 1
	       'RangeError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (msg (js-string->jsstring ""))
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct))
	 (set! js-reference-error
	    (js-make-function %this (%js-reference-error %this) 1
	       'ReferenceError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (msg (js-string->jsstring ""))
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct/stack))
	 ;; bind Error in the global object
	 (js-bind! %this %this 'Error :configurable #f :enumerable #f
	    :value js-error)
	 (js-bind! %this %this 'SyntaxError :configurable #f :enumerable #f
	    :value js-syntax-error)
	 (js-bind! %this %this 'TypeError :configurable #f :enumerable #f
	    :value js-type-error)
	 (js-bind! %this %this 'URIError :configurable #f :enumerable #f
	    :value js-uri-error)
	 (js-bind! %this %this 'EvalError :configurable #f :enumerable #f
	    :value js-eval-error)
	 (js-bind! %this %this 'RangeError :configurable #f :enumerable #f
	    :value js-range-error)
	 (js-bind! %this %this 'ReferenceError :configurable #f :enumerable #f
	    :value js-reference-error)
	 ;; nodejs addon
	 (js-bind! %this js-error 'stackTraceLimit
	    :value 10
	    :enumerable #f)
	 (js-bind! %this js-error 'captureStackTrace
	    :value (js-make-function %this
		      (lambda (o this start-func)
			 (capture-stack-trace this start-func))
		      2 'captureStackTrace)
	    :enumerable #f)
	 js-error)))

;*---------------------------------------------------------------------*/
;*    %js-error ...                                                    */
;*---------------------------------------------------------------------*/
(define (%js-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-error)
	 (js-new %this js-error message fname loc))))

;*---------------------------------------------------------------------*/
;*    %js-syntax-error ...                                             */
;*---------------------------------------------------------------------*/
(define (%js-syntax-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-syntax-error)
	 (tprint "js-syntax-error fname=" fname " loc=" loc)
	 (js-new %this js-syntax-error message fname loc))))

;*---------------------------------------------------------------------*/
;*    %js-type-error ...                                               */
;*---------------------------------------------------------------------*/
(define (%js-type-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-type-error)
	 (js-new %this js-type-error message fname loc))))

;*---------------------------------------------------------------------*/
;*    %js-uri-error ...                                                */
;*---------------------------------------------------------------------*/
(define (%js-uri-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-uri-error)
	 (js-new %this js-uri-error message fname loc))))

;*---------------------------------------------------------------------*/
;*    %js-eval-error ...                                               */
;*---------------------------------------------------------------------*/
(define (%js-eval-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-eval-error)
	 (js-new %this js-eval-error message fname loc))))

;*---------------------------------------------------------------------*/
;*    %js-range-error ...                                              */
;*---------------------------------------------------------------------*/
(define (%js-range-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-range-error)
	 (js-new %this js-range-error message fname loc))))

;*---------------------------------------------------------------------*/
;*    %js-reference-error ...                                          */
;*---------------------------------------------------------------------*/
(define (%js-reference-error %this)
   (lambda (this message fname loc)
      (with-access::JsGlobalObject %this (js-reference-error)
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
   (js-bind! %this obj 'constructor
      :value js-error
      :enumerable #f)
   
   ;; toString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.11.4.4
   (define (error-prototype-tostring this)
      (if (not (isa? this JsObject))
	  (bigloo-type-error "toString" "JsObject" this)
	  (let* ((name3 (js-get this 'name %this))
		 (name4 (if (eq? name3 (js-undefined))
			    (js-string->jsstring "Error")
			    (js-tojsstring name3 %this)))
		 (msg5 (if (isa? this JsObject)
			   (js-get this 'message %this)
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
      
   (js-bind! %this obj 'toString
      :value (js-make-function %this error-prototype-tostring 1 'toString)
      :enumerable #f)
   
   (set! *js-builtin-error-prototype* obj))
   
;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
