;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/symbol.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Wed Feb 12 10:07:15 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
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
   
   (include "types.sch" "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_error
	   __hopscript_worker
	   __hopscript_names)

   (export (js-symbol-table)
	   (js-init-symbol! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsSymbol ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsSymbol
   (lambda (o ctx)
      (with-access::JsSymbol o (val)
	 val))
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-toobject ctx o)
	  (error "string->obj ::JsSymbol" "Not a JavaScript context" ctx))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsSymbol ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsObject worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((nobj (duplicate::JsSymbol obj
			(elements '#())
			(__proto__ (js-get js-object (& "prototype") %this)))))
	    (js-for-in obj
	       (lambda (k %this)
		  (js-put! nobj k
		     (js-donate (js-get obj k %_this) worker %_this)
		     #f %this))
	       %this)
	    nobj))))

;*---------------------------------------------------------------------*/
;*    js-tostring ::JsSymbolLiteral ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-tostring obj::JsSymbolLiteral %this)
   (with-access::JsSymbolLiteral obj (val)
      (string-append "Symbol(" (js-jsstring->string val) ")")))

;*---------------------------------------------------------------------*/
;*    js-inspect ::JsSymbolLiteral ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-inspect obj::JsSymbolLiteral cnt)
   (with-access::JsSymbolLiteral obj (val)
      (string-append "Symbol(" (js-jsstring->string val) ")")))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsSymbol ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsSymbol op compile isexpr ctx)
   (with-access::JsSymbol o (val)
      (display "Symbol(\"" op)
      (display (string-for-read (js-jsstring->string val)) op)
      (display "\")" op)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsSymbol ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsSymbolLiteral op compile isexpr ctx)
   (with-access::JsSymbolLiteral o (val)
      (display "Symbol(\"" op)
      (display (string-for-read (js-jsstring->string val)) op)
      (display "\")" op)))

;*---------------------------------------------------------------------*/
;*    js-toprimitive ::JsSymbolLiteral ...                             */
;*---------------------------------------------------------------------*/
(define-method (js-toprimitive o::JsSymbolLiteral preferredtype %this::JsGlobalObject)
   (if (eq? preferredtype 'any)
       (with-access::JsSymbolLiteral o (val)
	  (js-raise-type-error %this
	     (format "Cannot convert symbol value \"~a\" to a string"
		(js-jsstring->string val)) o))
       (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-toprimitive ::JsSymbol ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-toprimitive o::JsSymbol preferredtype %this::JsGlobalObject)
   (with-access::JsSymbol o (val)
      val))

;*---------------------------------------------------------------------*/
;*    js-properties-name ::JsSymbolLiteral ...                         */
;*---------------------------------------------------------------------*/
(define-method (js-properties-names o::JsSymbolLiteral enump::bool %this)
   '())

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsSymbolLiteral ...                            */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsSymbolLiteral name %this)
   #f)

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::JsSymbolLiteral ...                        */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsSymbolLiteral p %this)
   (js-undefined))

;*---------------------------------------------------------------------*/
;*    js-get ::JsSymbolLiteral ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsSymbolLiteral prop %this)
   (with-access::JsGlobalObject %this (js-symbol)
      (js-get (js-get js-symbol (& "prototype") %this) prop %this)))

;*---------------------------------------------------------------------*/
;*    js-symbol-table ...                                              */
;*---------------------------------------------------------------------*/
(define (js-symbol-table)
   (create-hashtable :eqtest string=? :hash string-hash))   

;*---------------------------------------------------------------------*/
;*    symbols ...                                                      */
;*---------------------------------------------------------------------*/
(define symbols '())

;*---------------------------------------------------------------------*/
;*    js-init-symbol! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-symbol! %this::JsGlobalObject)
   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   (with-access::JsGlobalObject %this (__proto__
					 js-function
					 js-symbol
					 js-symbol-ctor
					 js-symbol-table
					 js-symbol-iterator
					 js-symbol-species
					 js-symbol-hasinstance
					 js-symbol-tostringtag
					 js-symbol-unscopables)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; builtin prototype
	 (define js-symbol-prototype
	    (instantiateJsSymbol
	       (val (instantiate::JsSymbolLiteral
		       (val (js-ascii->jsstring ""))))
	       (__proto__ __proto__)))

	 (set! js-symbol-ctor
	    ;; this constructor is invoked when Object( symbol ) is called
	    (lambda (this arg)
	       (instantiateJsSymbol
		  (__proto__ js-symbol-prototype)
		  (val arg))))
	 
	 ;; Symbol constructor function
	 (set! js-symbol
	    (js-make-function %this
	       (lambda (this . args)
		  (instantiate::JsSymbolLiteral
		     (val (cond
			     ((null? args)
			      (js-ascii->jsstring ""))
			     ((js-jsstring? (car args))
			      (car args))
			     (else
			      (js-string->jsstring (js-tostring (car args) %this)))))))
	       1 "Symbol"
	       :__proto__ js-function-prototype
	       :prototype js-symbol-prototype
	       :size 17
	       :shared-cmap #f))
	 
	 (js-bind! %this %this (& "Symbol")
	    :configurable #f :enumerable #f :value js-symbol
	    :hidden-class #t)

	 (define (bind-symbol! s::bstring)
	    (let ((o (assoc s symbols)))
	       (let ((v (if (pair? o)
			    (cdr o)
			    (let ((sym (instantiate::JsSymbolLiteral
					  (val (js-ascii->jsstring
						  (string-append "Symbol." s))))))
			       (set! symbols (cons (cons s sym) symbols))
			       sym))))
	       (js-bind! %this js-symbol (js-ascii-name->jsstring s)
		  :value v
		  :writable #f
		  :enumerable #f
		  :configurable #f
		  :hidden-class #f)
	       v)))
	 
	 ;; for
	 ;; http://www.ecma-international.org/ecma-262/6.0/#sec-symbol.for
	 (define (js-symbol-for this key)
	    (let* ((str (js-tojsstring key %this))
		   (stringkey (js-jsstring->string str))
		   (old (hashtable-get js-symbol-table stringkey)))
	       (or old
		   (let ((new (instantiate::JsSymbolLiteral
				 (val str))))
		      (hashtable-put! js-symbol-table stringkey new)
		      new))))

	 (js-bind! %this js-symbol (& "for")
	    :value (js-make-function %this js-symbol-for 1 "for")
	    :writable #t
	    :enumerable #f
	    :configurable #t
	    :hidden-class #t)

	 ;; keyFor
	 ;; http://www.ecma-international.org/ecma-262/6.0/#sec-symbol.keyfor
	 (define (js-symbol-keyfor::obj this sym)
	    (if (isa? sym JsSymbolLiteral)
		(with-access::JsSymbolLiteral sym (val)
		   (if (hashtable-get js-symbol-table (js-jsstring->string val))
		       val
		       (js-undefined)))
		(js-raise-type-error %this "not a symbol ~a" sym)))

	 (js-bind! %this js-symbol (& "keyFor")
	    :value (js-make-function %this js-symbol-keyfor 1 "keyFor")
	    :writable #t
	    :enumerable #f
	    :configurable #t
	    :hidden-class #f)

	 ;; global symbols
	 (for-each bind-symbol!
	    '("isConcatSpreadable" "match"
	      "replace" "search" "split" "toPrimitive"))

	 ;; hop symbol
	 (bind-symbol! "compiler")

	 ;; bind the known symbols
	 (set! js-symbol-iterator (bind-symbol! "iterator"))
	 (set! js-symbol-tostringtag (bind-symbol! "toStringTag"))
	 (set! js-symbol-species (bind-symbol! "species"))
	 (set! js-symbol-unscopables (bind-symbol! "unscopables"))

	 ;; hasinstance has already been allocated
	 (js-bind! %this js-symbol (& "hasInstance")
	    :value js-symbol-hasinstance
	    :writable #f
	    :enumerable #f
	    :configurable #f
	    :hidden-class #f)

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
   (js-bind! %this obj (& "length")
      :value 0
      :enumerable #f
      :hidden-class #t)
   
   ;; toString
   ;; http://www.ecma-international.org/ecma-262/6.0/#19.4.3.2
   (define (tostring this)
      (cond
	 ((isa? this JsSymbolLiteral)
	  (js-tojsstring this %this))
	 ((isa? this JsSymbol)
	  (with-access::JsSymbol this (val)
	     (js-tojsstring val %this)))
	 ((js-object? this)
	  (js-raise-type-error %this "no internal slot ~a" this))
	 (else
	  (js-raise-type-error %this "not a symbol ~a" this))))
   
   (js-bind! %this obj (& "toString")
      :value (js-make-function %this tostring 0 "toString")
      :enumerable #f
      :hidden-class #t)
   
   (define (valueof this)
      (cond
	 ((isa? this JsSymbolLiteral)
	  this)
	 ((isa? this JsSymbol)
	  (with-access::JsSymbol this (val)
	     val))
	 ((js-object? this)
	  (js-raise-type-error %this "no internal slot ~a" this))
	 (else
	  (js-raise-type-error %this "not a symbol ~a" this))))
   
   (js-bind! %this obj (& "valueOf")
      :value (js-make-function %this valueof 0 "valueOf")
      :enumerable #f
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
