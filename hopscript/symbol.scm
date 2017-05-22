;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/symbol.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Sun May 21 09:34:10 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript symbols                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/6.0/#19.4             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_symbol

   (include "../nodejs/nodejs_debug.sch")
   
   (library hop)
   
   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_error
	   __hopscript_worker)

   (export (js-symbol-table)
	   (js-init-symbol! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    Jsstringliteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsSymbol ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsSymbol
   (lambda (o)
      (with-access::JsSymbol o (val)
	 val))
   (lambda (o %this)
      (let ((this (or %this (js-initial-global-object))))
	 (with-access::JsGlobalObject this (js-symbol)
	    (instantiate::JsSymbol
	       (val o)
	       (__proto__ (js-get js-symbol 'prototype this)))))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsSymbol ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsSymbol worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-symbol)
	 (let ((nobj (call-next-method)))
	    (with-access::JsSymbol nobj (__proto__ val)
	       (with-access::JsSymbol obj ((_val val))
		  (set! __proto__ (js-get js-symbol 'prototype %this))
		  (set! val (js-donate _val worker %_this))))
	    nobj))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsSymbol ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsSymbol op compile isexpr)
   (with-access::JsSymbol o (val)
      (display "Symbol(\"" op)
      (display (string-for-read val) op)
      (display "\")" op)))

;*---------------------------------------------------------------------*/
;*    js-symbol-table ...                                              */
;*---------------------------------------------------------------------*/
(define (js-symbol-table)
   (create-hashtable :eqtest string=? :hash string-hash))   

;*---------------------------------------------------------------------*/
;*    js-init-symbol! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-symbol! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-symbol js-function
					 js-symbol-table
					 js-symbol-iterator
					 js-symbol-species)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; builtin prototype
	 (define js-symbol-prototype
	    (instantiate::JsSymbol
	       (val "")
	       (__proto__ __proto__)))

	 ;; symbol allocation
	 (define (js-symbol-alloc::JsSymbol constructor::JsFunction)
	    (js-raise-type-error %this "Not a constructor" constructor))

	 ;; then, create a HopScript object
	 (set! js-symbol
	    (js-make-function %this
	       (%js-symbol %this) 1 'Symbol
	       :__proto__ js-function-prototype
	       :prototype js-symbol-prototype
	       :alloc js-symbol-alloc))
	 
	 (define (%js-symbol %this)
	    (lambda::JsSymbol (this . args)
	       (instantiate::JsSymbol
		  (__proto__ js-symbol-prototype)
		  (val (if (null? args) "" (js-tostring (car args) %this))))))

	 (define (bind-sym! s)
	    (let ((sym (instantiate::JsSymbol
			  (__proto__ js-symbol-prototype)
			  (val (string-append "Symbol."
				  (symbol->string! s))))))
	       (js-bind! %this js-symbol s
		  :value sym
		  :writable #f
		  :enumerable #f
		  :configurable #f
		  :hidden-class #t)
	       sym))
	 
	 ;; for
	 ;; http://www.ecma-international.org/ecma-262/6.0/#sec-symbol.for
	 (define (js-symbol-for::JsSymbol this key)
	    (let* ((stringkey (js-tostring key %this))
		   (old (hashtable-get js-symbol-table stringkey)))
	       (or old
		   (let ((new (instantiate::JsSymbol
				 (__proto__ js-symbol-prototype)
				 (val stringkey))))
		      (hashtable-put! js-symbol-table stringkey new)
		      new))))

	 (js-bind! %this js-symbol 'for
	    :value (js-make-function %this js-symbol-for 1 'for)
	    :writable #t
	    :enumerable #f
	    :configurable #t
	    :hidden-class #t)
	 
	 ;; keyFor
	 ;; http://www.ecma-international.org/ecma-262/6.0/#sec-symbol.keyfor
	 (define (js-symbol-keyfor::obj this sym)
	    (if (isa? sym JsSymbol)
		(with-access::JsSymbol sym (val)
		   (if (hashtable-get js-symbol-table val) #t (js-undefined)))
		(js-raise-type-error %this "not a symbol" sym)))

	 (js-bind! %this js-symbol 'keyFor
	    :value (js-make-function %this js-symbol-keyfor 1 'keyFor)
	    :writable #t
	    :enumerable #f
	    :configurable #t
	    :hidden-class #t)

	 ;; global symbols
	 (for-each bind-sym!
	    '(hasInstance isConcatSpreadable match prototype
	      replace search split toPrimitive toStringTag
	      unscopables))

	 (set! js-symbol-iterator (bind-sym! 'iterator))
	 (set! js-symbol-species (bind-sym! 'species))
	 
	 ;; bind Symbol in the global object
	 (js-bind! %this %this 'Symbol
	    :configurable #f :enumerable #f :value js-symbol
	    :hidden-class #t)

	 ;; prototype properties
	 (init-builtin-symbol-prototype! %this js-symbol js-symbol-prototype)

	 js-symbol)))

;*---------------------------------------------------------------------*/
;*    js-valueof ::JsSymbol ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsSymbol %this)
   (with-access::JsSymbol this (val)
      val))

;*---------------------------------------------------------------------*/
;*    init-builtin-symbol-prototype! ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/6.0/#sec-19.4.3       */
;*---------------------------------------------------------------------*/
(define (init-builtin-symbol-prototype! %this::JsGlobalObject js-symbol obj)
   
   ;; length
   (js-bind! %this obj 'length
      :value 0
      :enumerable #f
      :hidden-class #t)
   
   ;; toString
   ;; http://www.ecma-international.org/ecma-262/6.0/#19.4.3.2
   (define (tostring::JsStringLiteral this)
      (cond
	 ((isa? this JsSymbol)
	  (with-access::JsSymbol this (val)
	     (js-stringlist->jsstring `("Symbol(" ,val ")"))))
	 ((isa? this JsObject)
	  (js-raise-type-error %this "no internal slow" this))
	 (else
	  (js-raise-type-error %this "not a symbol" this))))
   
   (js-bind! %this obj 'toString
      :value (js-make-function %this tostring 0 'toString)
      :enumerable #f
      :hidden-class #t)
   
   (define (valueof::JsSymbol this)
      (cond
	 ((isa? this JsSymbol)
	  this)
	 ((isa? this JsObject)
	  (js-raise-type-error %this "no internal slow" this))
	 (else
	  (js-raise-type-error %this "not a symbol" this))))
   
   (js-bind! %this obj 'valueOf
      :value (js-make-function %this valueof 0 'valueOf)
      :enumerable #f
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    Jsstringliteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
