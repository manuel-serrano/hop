;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/hopscript/error.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Fri Feb 14 11:06:06 2014 (serrano)                */
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

   (import __hopscript_types
	   __hopscript_object
	   __hopscript_property
	   __hopscript_function
	   __hopscript_private
	   __hopscript_public)

   (export js-error
	   js-error-prototype
	   js-syntax-error
	   js-type-error
	   js-uri-error
	   js-eval-error
	   js-range-error
	   js-reference-error
	   (js-init-error! ::JsObject)
	   (js-builtin-error-prototype)))

;*---------------------------------------------------------------------*/
;*    error objects ...                                                */
;*---------------------------------------------------------------------*/
(define js-error #f)
(define js-error-prototype #f)
(define js-syntax-error #f)
(define js-type-error #f)
(define js-uri-error #f)
(define js-eval-error #f)
(define js-range-error #f)
(define js-reference-error #f)

;*---------------------------------------------------------------------*/
;*    exception-notify ::obj ...                                       */
;*---------------------------------------------------------------------*/
(define-method (exception-notify exc::JsError)
   (with-access::JsError exc (name msg stack fname location)
      (if (isa? msg &exception)
	  (exception-notify msg)
	  (let ((port (current-error-port)))
	     (fprint port "*** " name ": " fname "," location ":" msg "\n")
	     (display-trace-stack stack port)))))

;*---------------------------------------------------------------------*/
;*    js-init-error! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-init-error! %this)
   ;; bind the errors into the global object
   (set! js-error-prototype
      (instantiate::JsError
	 (__proto__ js-object-prototype)
	 (properties (list
			(instantiate::JsAccessorDescriptor
			   (name 'message)
			   (enumerable #f)
			   (configurable #t)
			   (set (js-make-function
				   (lambda (o v) #f)
				   2 "message"))
			   (get (js-make-function
				   (lambda (o)
				      (with-access::JsError o (msg)
					 msg))
				   1 "message")))
			(instantiate::JsAccessorDescriptor
			   (name 'name)
			   (enumerable #f)
			   (configurable #t)
			   (set (js-make-function
				   (lambda (o v) #f)
				   2 "name"))
			   (get (js-make-function
				   (lambda (o)
				      (with-access::JsError o (name)
					 name))
				   1 "name")))))
	 (extensible #t)))
   ;; then, create a HopScript object
   (let ((obj (js-make-function %js-error 1 "Error"
		 :__proto__ js-function-prototype
		 :prototype js-error-prototype
		 :alloc js-error-alloc
		 :construct js-error-construct)))
      (set! js-error obj)
      (init-builtin-error-prototype! js-error-prototype)
      (set! js-syntax-error
	 (js-make-function %js-syntax-error 1 "SyntaxError"
		 :__proto__ js-function-prototype
		 :prototype (instantiate::JsError 
			       (__proto__ js-error-prototype)
			       (extensible #t))
		 :alloc js-error-alloc
		 :construct js-error-construct))
      (set! js-type-error
	 (js-make-function %js-type-error 1 "TypeError"
	    :__proto__ js-function-prototype
	    :prototype (instantiate::JsError 
			  (__proto__ js-error-prototype)
			  (extensible #t))
	    :alloc js-error-alloc
	    :construct js-error-construct))
      (set! js-uri-error
	 (js-make-function %js-uri-error 1 "URIError"
	    :__proto__ js-function-prototype
	    :prototype (instantiate::JsError 
			  (__proto__ js-error-prototype)
			  (extensible #t))
	    :alloc js-error-alloc
	    :construct js-error-construct))
      (set! js-eval-error
	 (js-make-function %js-eval-error 1 "EvalError"
	    :__proto__ js-function-prototype
	    :prototype (instantiate::JsError 
			  (__proto__ js-error-prototype)
			  (extensible #t))
	    :alloc js-error-alloc
	    :construct js-error-construct))
      (set! js-range-error
	 (js-make-function %js-range-error 1 "RangeError"
	    :__proto__ js-function-prototype
	    :prototype (instantiate::JsError 
			  (__proto__ js-error-prototype)
			  (extensible #t))
	    :alloc js-error-alloc
	    :construct js-error-construct))
      (set! js-reference-error
	 (js-make-function %js-reference-error 1 "ReferenceError"
	    :__proto__ js-function-prototype
	    :prototype (instantiate::JsError 
			  (__proto__ js-error-prototype)
			  (extensible #t))
	    :alloc js-error-alloc
	    :construct js-error-construct))
      ;; bind Error in the global object
      (js-bind! %this 'Error :configurable #f :enumerable #f
	 :value js-error)
      (js-bind! %this 'SyntaxError :configurable #f :enumerable #f
	 :value js-syntax-error)
      (js-bind! %this 'TypeError :configurable #f :enumerable #f
	 :value js-type-error)
      (js-bind! %this 'URIError :configurable #f :enumerable #f
	 :value js-uri-error)
      (js-bind! %this 'EvalError :configurable #f :enumerable #f
	 :value js-eval-error)
      (js-bind! %this 'RangeError :configurable #f :enumerable #f
	 :value js-range-error)
      (js-bind! %this 'ReferenceError :configurable #f :enumerable #f
	 :value js-reference-error)
      js-error))

;*---------------------------------------------------------------------*/
;*    %js-error ...                                                    */
;*---------------------------------------------------------------------*/
(define (%js-error this message)
   (js-new js-error message))

;*---------------------------------------------------------------------*/
;*    js-error-alloc ...                                               */
;*---------------------------------------------------------------------*/
(define (js-error-alloc constructor::JsFunction)
   (with-access::JsFunction constructor (name)
      (instantiate::JsError
	 (name name)
	 (__proto__ (js-get constructor 'prototype))
	 (stack '()))))

;*---------------------------------------------------------------------*/
;*    js-error-construct ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.11        */
;*---------------------------------------------------------------------*/
(define (js-error-construct this::JsError . args)
   (with-access::JsError this (msg fname location name)
      (match-case args
	 ((?m)
	  (unless (eq? m (js-undefined))
	     (js-bind! this 'message :value m)
	     (set! msg m)))
	 ((?m ?f ?l)
	  (js-bind! this 'message :value m)
	  (set! msg m)
	  (set! fname f)
	  (set! location l)))
      (js-bind! this 'name :value name))
   this)
   
;*---------------------------------------------------------------------*/
;*    %js-syntax-error ...                                             */
;*---------------------------------------------------------------------*/
(define (%js-syntax-error this message)
   (js-new js-syntax-error message))

;*---------------------------------------------------------------------*/
;*    %js-type-error ...                                               */
;*---------------------------------------------------------------------*/
(define (%js-type-error this message)
   (js-new js-type-error message))

;*---------------------------------------------------------------------*/
;*    %js-uri-error ...                                                */
;*---------------------------------------------------------------------*/
(define (%js-uri-error this message)
   (js-new js-uri-error message))

;*---------------------------------------------------------------------*/
;*    %js-eval-error ...                                               */
;*---------------------------------------------------------------------*/
(define (%js-eval-error this message)
   (js-new js-eval-error message))

;*---------------------------------------------------------------------*/
;*    %js-range-error ...                                              */
;*---------------------------------------------------------------------*/
(define (%js-range-error this message)
   (js-new js-range-error message))

;*---------------------------------------------------------------------*/
;*    %js-reference-error ...                                          */
;*---------------------------------------------------------------------*/
(define (%js-reference-error this message)
   (js-new js-reference-error message))

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
(define (init-builtin-error-prototype! obj)
   ;; prototype fields
   (js-bind! obj 'constructor
      :value js-error
      :enumerable #f)
   ;; toString
   (js-bind! obj 'toString
      :value (js-make-function error-prototype-tostring 1 "toString")
      :enumerable #f)
   (set! *js-builtin-error-prototype* obj))
      
;*---------------------------------------------------------------------*/
;*    error-prototype-tostring ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.11.4.4    */
;*---------------------------------------------------------------------*/
(define (error-prototype-tostring this)
   (if (not (isa? this JsObject))
       (bigloo-type-error "toString" "JsObject" this)
       (let* ((name3 (js-get this 'name))
	      (name4 (if (eq? name3 (js-undefined))
			 "Error"
			 (js-tostring name3)))
	      (msg5 (js-get this 'message))
	      (msg6 (if (eq? msg5 (js-undefined))
			""
			(js-tostring msg5))))
	  (cond
	     ((string=? name4 "") msg6)
	     ((string=? msg6 "") name4)
	     (else (string-append name4 ": " msg6))))))
   
