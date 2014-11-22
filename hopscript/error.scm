;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/error.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Sat Nov 22 09:45:10 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
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
	   __hopscript_function
	   __hopscript_private
	   __hopscript_public)

   (export (js-init-error! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-string-literal-begin!)

;*---------------------------------------------------------------------*/
;*    object-display ::JsError ...                                     */
;*---------------------------------------------------------------------*/
(define-method (object-display o::JsError . port)
   (with-output-to-port (if (null? port) (current-output-port) (car port))
      (lambda ()
	 (with-access::JsError o (name msg fname)
	    (display* "#<" (class-name (object-class o)) ": " name ", " msg
	       ">")))))

(define-method (object-print o::JsError port pslot)
   (object-display o port))
   
;*---------------------------------------------------------------------*/
;*    exception-notify ::obj ...                                       */
;*---------------------------------------------------------------------*/
(define-method (exception-notify exc::JsError)
   (with-access::JsError exc (name msg stack fname location)
      (if (isa? msg &exception)
	  (exception-notify msg)
	  (let* ((%this (js-new-global-object))
		 (stk (js-get exc 'stack %this))
		 (port (current-error-port))
		 (tstack `(,name (at ,fname ,location))))
	     (display-trace-stack-source (list tstack) port)
	     (fprint port name ": "msg "\n")
	     (if (string? stk)
		 (display stk port)
		 (let ((stack (cond
				 ((eq? name 'ReferenceError)
				  stack)
				 ((eq? name 'TypeError)
				  stack)
				 (else
				  stack))))
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
	       (extensible #t)))
	 
	 (define (js-error-alloc constructor::JsFunction)
	    (with-access::JsFunction constructor (name)
	       (instantiate::JsError
		  (name name)
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
		      (js-bind! %this this 'message :value m :enumerable #f))
		   (set! msg m)
		   (set! fname (if (string? f) (string->js-string f) f))
		   (set! location l))
		  ((?m . ?-)
		   (unless (eq? m (js-undefined))
		      (js-bind! %this this 'message :value m :enumerable #f)
		      (set! msg m))))

	       (js-bind! %this this 'name
		  :value (string->js-string name)
		  :enumerable #f)
	       (js-bind! %this this 'stack
		  :value (string->js-string "")
		  :enumerable #f))
	    this)

	 (define (js-error-construct/stack this::JsError . args)
	    (let ((err (apply js-error-construct this args)))
	       (capture-stack-trace #f err %this)
	       err))

	 (define (capture-stack-trace head this %this)
	    (let ((stk (call-with-output-string
			  (lambda (op)
			     (when (string? head)
				(display head op)
				(when (isa? this JsObject)
				   (display (js-get this 'message %this) op)
				   (newline op)))
			     (display-trace-stack (get-trace-stack 10)
				op 1)))))
	       (js-put! this 'stack stk #f %this)))
	 
	 ;; bind the properties of the prototype
	 (js-bind! %this js-error-prototype 'message
	    :set (js-make-function %this
		    (lambda (o v)
		       (js-bind! %this o 'message :value v :enumerable #f))
		    2 'message)
	    :get (js-make-function %this
		    (lambda (o)
		       (if (isa? o JsError)
			   (with-access::JsError o (msg) msg)
			   (js-undefined)))
		    1 'message)
	    :enumerable #f
	    :configurable #t)
	 (js-bind! %this js-error-prototype 'name
	    :set (js-make-function %this
		    (lambda (o v)
		       (js-bind! %this o 'name :value v))
		    2 'name)
	    :get (js-make-function %this
		    (lambda (o)
		       (if (isa? o JsError)
			   (with-access::JsError o (name) name)
			   (js-undefined)))
		    1 'name)
	    :enumerable #f)
	 
	 ;; then, create a HopScript object
	 (set! js-error
	    (js-make-function %this (%js-error %this) 1 'Error
	       :__proto__ js-function-prototype
	       :prototype js-error-prototype
	       :alloc js-error-alloc
	       :construct js-error-construct/stack))
	 (init-builtin-error-prototype! %this js-error js-error-prototype)
	 (set! js-syntax-error
	    (js-make-function %this (%js-syntax-error %this) 1 'SyntaxError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct/stack))
	 (set! js-type-error
	    (js-make-function %this (%js-type-error %this) 1 'TypeError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct/stack))
	 (set! js-uri-error
	    (js-make-function %this (%js-uri-error %this) 1 'URIError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct/stack))
	 (set! js-eval-error
	    (js-make-function %this (%js-eval-error %this) 1 'EvalError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct/stack))
	 (set! js-range-error
	    (js-make-function %this (%js-range-error %this) 1 'RangeError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct))
	 (set! js-reference-error
	    (js-make-function %this (%js-reference-error %this) 1 'ReferenceError
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
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
	 (js-bind! %this js-error 'captureStackTrace
	    :value (js-make-function %this
		      (lambda (o this start-func)
			 (capture-stack-trace "Trace: " this %this))
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
			    "Error"
			    (js-tostring name3 %this)))
		 (msg5 (if (isa? this JsObject)
			   (js-get this 'message %this)
			   (js-undefined)))
		 (msg6 (if (eq? msg5 (js-undefined))
			   ""
			   (js-tostring msg5 %this))))
	     (cond
		((string=? name4 "") (string->js-string msg6))
		((string=? msg6 "") (string->js-string name4))
		(else (string->js-string (string-append name4 ": " msg6)))))))
      
   (js-bind! %this obj 'toString
      :value (js-make-function %this error-prototype-tostring 1 'toString)
      :enumerable #f)
   
   (set! *js-builtin-error-prototype* obj))
   
;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-string-literal-end!)
