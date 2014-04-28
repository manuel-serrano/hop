;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/error.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Tue Apr 22 07:58:02 2014 (serrano)                */
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
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_property
	   __hopscript_function
	   __hopscript_private
	   __hopscript_public)

   (export (js-init-error! ::JsGlobalObject)))

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
	  (let ((port (current-error-port)))
	     (let ((tstack `(,name (at ,fname ,location)))
		   (stack (cond
			     ((string=? name "ReferenceError")
			      stack)
			     ((string=? name "TypeError")
			      stack)
			     (else
			      stack))))
		(display-trace-stack-source (list tstack) port)
		(fprint port "*** " name ": "msg "\n")
		(display-trace-stack stack port))))))

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
		      (js-bind! %this this 'message :value m)
		      (set! msg m)))
		  ((?m ?f ?l)
		   (js-bind! %this this 'message :value m)
		   (set! msg m)
		   (set! fname f)
		   (set! location l)))
	       (js-bind! %this this 'name :value name))
	    this)
   
	 ;; bind the properties of the prototype
	 (js-bind! %this js-error-prototype 'message
	    :set (js-make-function %this
		    (lambda (o v) #f) 2 "message")
	    :get (js-make-function %this
		    (lambda (o) (with-access::JsError o (msg) msg)) 1 "message")
	    :enumerable #f
	    :configurable #t)
	 (js-bind! %this js-error-prototype 'name
	    :set (js-make-function %this
		    (lambda (o v) #f) 2 "name")
	    :get (js-make-function %this
		    (lambda (o) (with-access::JsError o (name) name)) 1 "name")
	    :enumerable #f)
	 
	 ;; then, create a HopScript object
	 (set! js-error
	    (js-make-function %this (%js-error %this) 1 "Error"
	       :__proto__ js-function-prototype
	       :prototype js-error-prototype
	       :alloc js-error-alloc
	       :construct js-error-construct))
	 (init-builtin-error-prototype! %this js-error js-error-prototype)
	 (set! js-syntax-error
	    (js-make-function %this (%js-syntax-error %this) 1 "SyntaxError"
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct))
	 (set! js-type-error
	    (js-make-function %this (%js-type-error %this) 1 "TypeError"
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct))
	 (set! js-uri-error
	    (js-make-function %this (%js-uri-error %this) 1 "URIError"
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct))
	 (set! js-eval-error
	    (js-make-function %this (%js-eval-error %this) 1 "EvalError"
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct))
	 (set! js-range-error
	    (js-make-function %this (%js-range-error %this) 1 "RangeError"
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct))
	 (set! js-reference-error
	    (js-make-function %this (%js-reference-error %this) 1 "ReferenceError"
	       :__proto__ js-function-prototype
	       :prototype (instantiate::JsError 
			     (__proto__ js-error-prototype)
			     (extensible #t))
	       :alloc js-error-alloc
	       :construct js-error-construct))
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
	 js-error)))

;*---------------------------------------------------------------------*/
;*    %js-error ...                                                    */
;*---------------------------------------------------------------------*/
(define (%js-error %this)
   (lambda (this message)
      (with-access::JsGlobalObject %this (js-error)
	 (js-new %this js-error message))))

;*---------------------------------------------------------------------*/
;*    %js-syntax-error ...                                             */
;*---------------------------------------------------------------------*/
(define (%js-syntax-error %this)
   (lambda (this message)
      (with-access::JsGlobalObject %this (js-syntax-error)
	 (js-new %this js-syntax-error message))))

;*---------------------------------------------------------------------*/
;*    %js-type-error ...                                               */
;*---------------------------------------------------------------------*/
(define (%js-type-error %this)
   (lambda (this message)
      (with-access::JsGlobalObject %this (js-type-error)
	 (js-new %this js-type-error message))))

;*---------------------------------------------------------------------*/
;*    %js-uri-error ...                                                */
;*---------------------------------------------------------------------*/
(define (%js-uri-error %this)
   (lambda (this message)
      (with-access::JsGlobalObject %this (js-uri-error)
	 (js-new %this js-uri-error message))))

;*---------------------------------------------------------------------*/
;*    %js-eval-error ...                                               */
;*---------------------------------------------------------------------*/
(define (%js-eval-error %this)
   (lambda (this message)
      (with-access::JsGlobalObject %this (js-eval-error)
	 (js-new %this js-eval-error message))))

;*---------------------------------------------------------------------*/
;*    %js-range-error ...                                              */
;*---------------------------------------------------------------------*/
(define (%js-range-error %this)
   (lambda (this message)
      (with-access::JsGlobalObject %this (js-range-error)
	 (js-new %this js-range-error message))))

;*---------------------------------------------------------------------*/
;*    %js-reference-error ...                                          */
;*---------------------------------------------------------------------*/
(define (%js-reference-error %this)
   (lambda (this message)
      (with-access::JsGlobalObject %this (js-reference-error)
	 (js-new %this js-reference-error message))))

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
		 (msg5 (js-get this 'message %this))
		 (msg6 (if (eq? msg5 (js-undefined))
			   ""
			   (js-tostring msg5 %this))))
	     (cond
		((string=? name4 "") msg6)
		((string=? msg6 "") name4)
		(else (string-append name4 ": " msg6))))))
      
   (js-bind! %this obj 'toString
      :value (js-make-function %this error-prototype-tostring 1 "toString")
      :enumerable #f)
   
   (set! *js-builtin-error-prototype* obj))
   
