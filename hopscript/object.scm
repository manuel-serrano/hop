;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/object.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 17 08:43:24 2013                          */
;*    Last change :  Mon Feb 15 09:13:19 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo implementation of JavaScript objects               */
;*                                                                     */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2         */
;*    Complete: 27 sep 2013, untested.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_object

   (library hop hopwidget)
   
   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_string
	   __hopscript_stringliteral
	   __hopscript_symbol
	   __hopscript_promise
	   __hopscript_generator
	   __hopscript_function
	   __hopscript_number
	   __hopscript_math
	   __hopscript_boolean
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_arraybuffer
	   __hopscript_arraybufferview
	   __hopscript_date
	   __hopscript_error
	   __hopscript_json
	   __hopscript_service
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_worker
	   __hopscript_websocket
	   __hopscript_lib)

   (with   __hopscript_dom)

   (export (js-initial-global-object)
	   (inline js-object?::bool ::obj)
	   (js-new-global-object::JsGlobalObject)
	   
	   (js-object-prototype-hasownproperty this v ::JsGlobalObject)

	   (js-get-global-object-name
	      ::JsObject ::symbol ::obj ::JsGlobalObject)
	   (inline js-get-global-object-name/cache
	      ::JsObject ::symbol ::JsPropertyCache ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsObject ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsObject
   (lambda (o ctx)
      (js-jsobject->plist o (js-initial-global-object)))
   (lambda (o %this)
      (if (eq? %this 'hop)
	  o
	  (js-plist->jsobject o (or %this (js-initial-global-object))))))

;*---------------------------------------------------------------------*/
;*    js-tostring ::JsObject ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-tostring obj::JsObject %this::JsGlobalObject)
   (js-tostring (js-toprimitive obj 'string %this) %this))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsObject ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsObject worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-object)
	 (with-access::JsObject obj (extensible)
	    (let ((nobj (duplicate::JsObject obj
			   (__proto__ (js-get js-object 'prototype %this))
			   (properties '()))))
	       (js-for-in obj
		  (lambda (k)
		     (js-put! nobj k
			(js-donate (js-get obj k %_this) worker %_this)
			#f %this))
		  %this)
	       nobj)))))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsObject ...                               */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value obj::JsObject)
   (js-jsobject->plist obj (js-initial-global-object)))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsGlobalObject ...                                   */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsGlobalObject worker %_this)
   (js-new-global-object))

;*---------------------------------------------------------------------*/
;*    scheme->response ::JsObject ...                                  */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::JsObject req)
   (let* ((this (js-initial-global-object))
	  (proc (js-get obj 'toResponse this)))
      (if (isa? proc JsFunction)
	  (js-call1 this proc obj req)
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsObject ...                                        */
;*    -------------------------------------------------------------    */
;*    Used when an JS object is to pack the arguments sent to          */
;*    an XML constructor.                                              */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack o::JsObject)
   (js-jsobject->keyword-plist o (js-initial-global-object)))

;*---------------------------------------------------------------------*/
;*    xml-to-errstring ::JsObject ...                                  */
;*    -------------------------------------------------------------    */
;*    This method is invoked when an native error occurs on a XML      */
;*    object. This method is then invoked from Scheme code.            */
;*---------------------------------------------------------------------*/
(define-method (xml-to-errstring o::JsObject)
   (js-tostring o (js-initial-global-object)))

;*---------------------------------------------------------------------*/
;*    jsobject-fields ...                                              */
;*---------------------------------------------------------------------*/
(define jsobject-fields (vector (find-class-field JsObject 'elements)))

;*---------------------------------------------------------------------*/
;*    javascript-class-all-fields ::JsObject ...                       */
;*---------------------------------------------------------------------*/
(define-method (javascript-class-all-fields obj::JsObject)
   jsobject-fields)
   
;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsObject ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsObject op compile isexpr)
   (with-access::WorkerHopThread (js-current-worker) (%this)
      ;; force the literal to be treated as an expression
      (display "({" op)
      (let ((sep ""))
	 (js-for-in o
	    (lambda (p)
	       (display sep op)
	       (display "\"" op)
	       (display p op)
	       (display "\":" op)
	       (hop->javascript
		  (js-get o (string->symbol (js-jsstring->string p)) %this)
		  op compile isexpr)
	       (set! sep ","))
	    %this))
      (display "})" op)))

;*---------------------------------------------------------------------*/
;*    js-bind-tag! ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (js-bind-tag! %this obj tag . tagjs)
   `(begin
       (js-bind! ,%this ,obj ',(if (pair? tagjs) (car tagjs) tag)
          :value (js-make-function ,%this
                    (lambda (this attrs . nodes)
                       (if (isa? attrs JsObject)
                           (if (null? nodes)
                               (apply ,(symbol-append '< tag '>)
                                  (js-jsobject->keyword-plist attrs ,%this))
                               (apply ,(symbol-append '< tag '>)
                                  (append
                                     (js-jsobject->keyword-plist attrs ,%this)
                                     nodes)))
                           (apply ,(symbol-append '< tag '>)
                              nodes)))
                    2 ',tag)
          :writable #f :configurable #f :enumerable #f)))

;*---------------------------------------------------------------------*/
;*    js-bind-tags! ...                                                */
;*---------------------------------------------------------------------*/
(define-macro (js-bind-tags! %this obj . tags)
   `(begin
       ,@(map (lambda (tag)
		 `(js-bind-tag! ,%this ,obj ,tag))
	    tags)))
   
;*---------------------------------------------------------------------*/
;*    %this ...                                                        */
;*---------------------------------------------------------------------*/
(define %this #f)

;*---------------------------------------------------------------------*/
;*    js-initial-global-object ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1         */
;*---------------------------------------------------------------------*/
(define (js-initial-global-object)
   (unless %this
      (set! %this (js-new-global-object)))
   %this)

;*---------------------------------------------------------------------*/
;*    js-object? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-object? obj)
   (isa? obj JsObject))

;*---------------------------------------------------------------------*/
;*    js-new-global-object ...                                         */
;*---------------------------------------------------------------------*/
(define (js-new-global-object)
   ;; before all, initialize the builtin object prototype
   ;; and then create the global object
   (let* ((%prototype (instantiate::JsObject
			 (__proto__ (js-null))
			 (elements (make-vector 11))
			 (cmap (instantiate::JsConstructMap))
			 (extensible #t)))
	  (%this (instantiate::JsGlobalObject
		    (__proto__ %prototype)
		    (cmap (instantiate::JsConstructMap)))))
      ;; init the builtin function class
      (js-init-function! %this)
      ;; the object constructor
      (with-access::JsGlobalObject %this (js-function js-object)
	 (with-access::JsFunction js-function ((js-function-prototype __proto__))
	    ;; the prototypes and other builtin classes
	    (js-init-symbol! %this)
	    (js-init-array! %this)
	    (js-init-arraybuffer! %this)
	    (js-init-arraybufferview! %this)
	    (js-init-string! %this)
	    (js-init-boolean! %this)
	    (js-init-number! %this)
	    (js-init-math! %this)
	    (js-init-regexp! %this)
	    (js-init-date! %this)
	    (js-init-error! %this)
	    (js-init-json! %this)
	    (js-init-service! %this)
	    (js-init-worker! %this)
	    (js-init-websocket! %this)
	    (js-init-promise! %this)
	    (js-init-generator! %this)
	    (js-init-object! %this)
	    (js-init-object-prototype! %this)

	    ;; bind the global object properties
	    (js-bind! %this %this 'Object
	       :value js-object
	       :writable #t :enumerable #f :configurable #f)
	    (js-bind! %this %this 'NaN
	       :value +nan.0
	       :writable #f :enumerable #f :configurable #f)
	    (js-bind! %this %this 'Infinity
	       :value +inf.0
	       :writable #f :enumerable #f :configurable #f)
	    (js-bind! %this %this 'undefined
	       :value (js-undefined)
	       :writable #f :enumerable #f :configurable #f)

	    ;; eval
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.1
	    ;; not used, see nodejs/require.scm
	    (define (js-eval _ string)
	       (if (not (js-jsstring? string))
		   string
		   (call-with-input-string (js-jsstring->string string)
		      (lambda (ip)
			 (%js-eval ip 'eval %this %this %this)))))
	    
	    (js-bind! %this %this 'eval
	       :value (js-make-function %this js-eval 1 'eval
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t)
	    
	    ;; parseInt
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.2
	    (define (parseint this string radix)
	       (js-parseint
		  (trim-whitespaces+ (js-tostring string %this) :plus #t)
		  radix #f %this))

	    (js-bind! %this %this 'parseInt
	       :value (js-make-function %this parseint 2 'parseInt
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t)

	    ;; parseFloat
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.3
	    (define (parsefloat this string)
	       (js-parsefloat
		  (trim-whitespaces+ (js-tostring string %this) :plus #t)
		  #f
		  %this))
	    
	    (js-bind! %this %this 'parseFloat
	       :value (js-make-function %this parsefloat 1 'parseFloat
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t)

	    ;; isNaN
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.4
	    (define (isnan this number)
	       (let ((n (js-tonumber number %this)))
		  (and (flonum? n) (not (=fl n n)))))

	    (js-bind! %this %this 'isNaN
	       :value (js-make-function %this isnan 1 'isNaN
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t)

	    ;; isFinite
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.5
	    (define (isfinite this number)
	       (let ((n (js-tonumber number %this)))
		  (cond
		     ((not (flonum? n)) #t)
		     ((or (not (=fl n n)) (=fl n +inf.0) (=fl n -inf.0)) #f)
		     (else #t))))

	    (js-bind! %this %this 'isFinite
	       :value (js-make-function %this isfinite 1 'isFinite
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t)

	    ;; check-utfi-validity
	    (define (check-utf8-validity str)
	       (if (utf8-string? str)
		   (js-string->jsstring str)
		   (js-raise-uri-error %this "Not a utf8 string ~s" string)))
	    
	    ;; decodeURI
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.1
	    (define (decodeuri this string)
	       (let ((str (js-tostring string %this)))
		  (if (url? str)
		      (check-utf8-validity (uri-decode str))
		      (js-raise-uri-error %this "Badly formed url ~s" string))))
   
	    (js-bind! %this %this 'decodeURI
	       :value (js-make-function %this decodeuri 1 'decodeURI
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t)

	    ;; decodeURIComponent
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.2
	    (define (decodeuricomponent this string)
	       (let ((str (js-tostring string %this)))
		  (if (url? str)
		      (check-utf8-validity (uri-decode-component str))
		      (js-raise-uri-error %this "Badly formed url ~s" string))))
		
	    (js-bind! %this %this 'decodeURIComponent
	       :value (js-make-function %this decodeuricomponent
			 1 'decodeURIComponent
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t)

	    ;; encodeURI
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.3
	    (define (encodeuri this string)
	       (let ((str (js-tostring string %this)))
		  (if (utf8-string? str #t)
		      (js-string->jsstring (uri-encode str))
		      (js-raise-uri-error %this "Badly formed url ~s" string))))

	    (js-bind! %this %this 'encodeURI
	       :value (js-make-function %this encodeuri 1 'encodeURI
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t)

	    ;; encodeURIComponent
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.3.4
	    (define (encodeuricomponent this string)
	       (let ((str (js-tostring string %this)))
		  (if (utf8-string? str #t)
		      (js-string->jsstring (uri-encode-component str))
		      (js-raise-uri-error %this "Badly formed url ~s" string))))

	    (js-bind! %this %this 'encodeURIComponent
	       :value (js-make-function %this encodeuricomponent
			 1 'encodeURIComponent
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t)

	    ;; escape
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.1
	    (define (escape this string)
	       (js-string->jsstring (url-path-encode (js-tostring string %this))))
	    
	    (js-bind! %this %this 'escape
	       :value (js-make-function %this escape 1 'escape
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t)

	    ;; unescape
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.1
	    (define (unescape this string)
	       (js-string->jsstring (url-decode! (js-tostring string %this))))

	    (js-bind! %this %this 'unescape
	       :value (js-make-function %this unescape 1 'unescape
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t)

	    ;; html_base
	    (js-bind-tags! %this %this
	       A ABBR ACRONYM ADDRESS APPLET AREA B BASE
	       BASEFONT BDO BIG BLOCKQUOTE BODY BR BUTTON
	       CANVAS CAPTION CENTER CITE CODE COL COLGROUP
	       DATALIST DD DEL DFN DIR DIV DL DT EM EMBED FIELDSET FIGURE
	       FIGCAPTION FONT FOOTER FORM FRAME FRAMESET H1 H2 H3 H4 H5 H6
	       HR HEADER HGROUP I IFRAME INPUT INS ISINDEX KBD LABEL LEGEND
	       LI MAP MARQUEE MENU META NAV NOFRAMES NOSCRIPT
	       OBJECT OL OPTGROUP OPTION P PARAM PRE PROGRESS
	       Q S SAMP SECTION SELECT SMALL SOURCE SPAN STRIKE
	       STRONG SUB SUP TABLE TBODY TD TEXTAREA TFOOT TH
	       THEAD TITLE TR TT U UL VAR)

	    ;; html5
	    (js-bind-tags! %this %this
	       AUDIO VIDEO)

	    ;; html
	    (define (js-html-html %this)
	       (js-make-function %this
		  (lambda (this attrs . nodes)
		     (if (isa? attrs JsObject)
			 (apply <HTML> :idiom "javascript" :context %this
			    (append
			       (js-jsobject->keyword-plist attrs %this)
			       nodes))
			 (apply <HTML> :idiom "javascript" nodes)))
		  1 'HTML))
	    
	    (js-bind! %this %this 'HTML
	       :value (js-html-html %this) :enumerable #f)

	    ;; only used with the global object, see nodejs/require.scm
	    (js-bind! %this %this 'HEAD
	       :value (js-html-head %this) :enumerable #f)

	    ;; html_head
	    (js-bind-tags! %this %this
	       LINK SCRIPT STYLE)

	    (js-bind-tags! %this %this IMG)

	    ;; svg
	    (js-bind-tags! %this %this
	       SVG SVG:DEFS SVG:RECT SVG:CIRCLE SVG:ELLIPSE SVG:FILTER
	       SVG:FEGAUSSIANBLUR SVG:FECOLORMATRIX SVG:FOREIGNOBJECT SVG:G
	       SVG:LINE SVG:PATH SVG:POLYLINE SVG:POLYGON SVG:TEXT
	       SVG:TEXTPATH SVG:TREF SVG:TSPAN
	       SVG:RADIALGRADIENT SVG:LINEARGRADIENT SVG:IMG)

	    ;; mathml
	    (js-bind-tags! %this %this
	       MATH MATH:MSTYLE MATH:MI MATH:MN MATH:MO
	       MATH:MROW MATH:MUNDER MATH:MOVER MATH:MUNDEROVER
	       MATH:MSUP MATH:MSUB MATH:MSUBSUP MATH:MFRAC
	       MATH:MROOT MATH:MSQRT MATH:MTEXT MATH:MTABLE
	       MATH:MTR MATH:MTD MATH:MPADDED MATH:TEX)

	    (js-bind! %this %this '!--
	       :value (js-make-function %this
			 (lambda (this data)
			    (instantiate::xml-comment
			       (data data)))
			 1 '<!--)
	       :enumerable #f :writable #f :configurable #f)

	    (define (string->xml-tilde body)
	       (let ((expr (js-tostring body %this)))
		  (instantiate::JsWrapper
		     (__proto__ %prototype)
		     (data body)
		     (obj (instantiate::xml-tilde
			     (lang 'javascript)
			     (%js-expression expr)
			     (body (vector body '() '() '() expr #f)))))))
	    
	    ;; tilde object
	    (js-bind! %this %this 'Tilde
	       :value (js-make-function %this
			 (lambda (this body)
			    (string->xml-tilde body))
			 1 'Tilde
			 :__proto__ js-function-prototype
			 :construct (lambda (this body)
				       (string->xml-tilde body)))
	       :enumerable #f :writable #f :configurable #f)
	    
	    ;; return the newly created object
	    %this))))

;*---------------------------------------------------------------------*/
;*    js-init-object! ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3       */
;*---------------------------------------------------------------------*/
(define (js-init-object! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this ((%prototype __proto__)
				       js-object js-function)
      
      ;; Object.prototype
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.1
      (define (%js-object this . value)
	 (cond
	    ((null? value)
	     (js-new %this js-object (js-undefined)))
	    ((or (eq? (car value) (js-null)) (eq? (car value) (js-undefined)))
	     (js-new %this js-object (car value)))
	    (else
	     (js-toobject %this (car value)))))

      ;; Object.constructor
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.2.1
      (define (js-object-constructor f value)
	 (with-access::JsGlobalObject %this (js-string js-boolean js-number)
	    (cond
	       ((or (eq? value (js-null)) (eq? value (js-undefined)))
		;; 2
		(with-access::JsFunction f (constrmap)
		   (instantiate::JsObject
		      (cmap constrmap)
		      (elements ($create-vector 4))
		      (__proto__ %prototype)
		      (extensible #t))))
	       ((isa? value JsObject)
		;; 1.a
		value)
	       ((js-jsstring? value)
		;; 1.b
		(js-new %this js-string value))
	       ((boolean? value)
		;; 1.c
		(js-new %this js-boolean value))
	       ((number? value)
		;; 1.c
		(js-new %this js-number value))
	       (else
		(js-raise-type-error %this "illegal value ~s" value)))))

      (define (js-object-construct f . arg)
	 (tprint "DEPRECATED, SHOULD NOT BE HERE...f=" f " arg=" arg)
	 (with-access::JsGlobalObject %this (js-object)
	    (js-object-constructor js-object
	       (if (pair? arg) (car arg) (js-undefined)))))
      
      (with-access::JsObject js-function ((js-function-prototype __proto__))
	 (set! js-object 
	    (js-make-function %this %js-object 1 'Object 
	       :__proto__ js-function-prototype
	       :prototype %prototype
	       :construct js-object-construct
	       :constructor js-object-constructor)))
      
      ;; getprototypeof
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.2
      (define (getprototypeof this o)
	 (let ((o (js-cast-object o %this "getPrototypeOf")))
	    (with-access::JsObject o (__proto__)
	       __proto__)))
      
      (js-bind! %this js-object 'getPrototypeOf
	 :value (js-make-function %this getprototypeof 1 'getPrototypeOf)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; getOwnPropertyDescriptor
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.3
      (define (getownpropertydescriptor this o p)
	 (let* ((o (js-cast-object o %this "getOwnPropertyDescriptor"))
		(desc (js-get-own-property o p %this)))
	    (js-from-property-descriptor %this desc o)))
      
      (js-bind! %this js-object 'getOwnPropertyDescriptor
	 :value (js-make-function %this
		   getownpropertydescriptor 2 'getOwnPropertyDescriptor)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; getOwnPropertyNames
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.4
      (define (getownpropertynames this o p)
	 (let ((o (js-cast-object o %this "getOwnPropertyNames")))
	    (js-vector->jsarray (js-properties-name o #f %this) %this)))
      
      (js-bind! %this js-object 'getOwnPropertyNames
	 :value (js-make-function %this
		   getownpropertynames 1 'getOwnPropertyNames)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; getOwnPropertySymbols
      ;; http://www.ecma-international.org/ecma-262/6.0/#sec-19.1.2.8
      (define (getownpropertysymbols this o p)
	 (let ((o (js-cast-object o %this "getOwnPropertySymbols")))
	    (js-vector->jsarray (js-properties-symbol o %this) %this)))
      
      (js-bind! %this js-object 'getOwnPropertySymbols
	 :value (js-make-function %this
		   getownpropertysymbols 1 'getOwnPropertySymbols)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; create
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.5
      (define (create this o properties)
	 (if (not (or (eq? o (js-null)) (isa? o JsObject)))
	     (js-raise-type-error %this "create: bad object ~s" o)
	     (let ((obj (js-new0 %this js-object)))
		(with-access::JsObject obj (__proto__)
		   (set! __proto__ o)
		   (unless (eq? properties (js-undefined))
		      (object-defineproperties %this this obj properties)))
		obj)))
      
      (js-bind! %this js-object 'create
	 :value (js-make-function %this create 2 'create)
	 :writable #t
	 :configurable #t
	 :enumerable #f)

      ;; defineProperty
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.6
      (define (defineproperty this obj p attributes)
	 (let* ((o (js-cast-object obj %this "defineProperty"))
		(name (js-toname p %this))
		(desc (js-to-property-descriptor %this attributes name)))
	    (js-define-own-property o name desc #t %this)
	    obj))
      
      (js-bind! %this js-object 'defineProperty
	 :value (js-make-function %this defineproperty 3 'defineProperty)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; defineProperties
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.7
      (js-bind! %this js-object 'defineProperties
	 :value (js-make-function %this
		   (lambda (this obj properties)
		      (object-defineproperties %this this obj properties))
		   2 'defineProperties)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; seal
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.8
      (define (seal this obj)
	 (js-seal (js-cast-object obj %this "seal") obj))
      
      (js-bind! %this js-object 'seal
	 :value (js-make-function %this seal 1 'seal)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; freeze
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.9
      (define (freeze this obj)
	 (js-freeze (js-cast-object obj %this "freeze") obj))
      
      (js-bind! %this js-object 'freeze
	 :value (js-make-function %this freeze 1 'freeze)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; preventExtensions
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.10
      (define (preventextensions this obj)
	 (let ((o (js-cast-object obj %this "preventExtensions")))
	    (with-access::JsObject o (extensible)
	       (set! extensible #f))
	    obj))
      
      (js-bind! %this js-object 'preventExtensions
	 :value (js-make-function %this preventextensions 1 'preventExtensions)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; isSealed
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.11
      (define (issealed this o)
	 ;; 1
	 (let ((o (js-cast-object o %this "isSealed")))
	    (with-access::JsObject o (properties cmap)
	       (and
		(not cmap)
		;; 2
		(every (lambda (desc::JsPropertyDescriptor)
			  (with-access::JsPropertyDescriptor desc (configurable)
			     (not (eq? configurable #t))))
		   properties)
		;; 3
		(with-access::JsObject o (extensible)
		   (not extensible))))))
      
      (js-bind! %this js-object 'isSealed
	 :value (js-make-function %this issealed 1 'isSealed)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; isFrozen
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.12
      (define (isfrozen this o)
	 ;; 1
	 (let ((o (js-cast-object o %this "isFrozen")))
	    (with-access::JsObject o (properties cmap)
	       (and
		(or (not cmap)
		    (with-access::JsConstructMap cmap (names)
		       (=fx (vector-length names) 0)))
		;; 2
		(every (lambda (desc::JsPropertyDescriptor)
			  (with-access::JsPropertyDescriptor desc (configurable)
			     (and (not (eq? configurable #t))
				  (or (not (isa? desc JsValueDescriptor))
				      (with-access::JsValueDescriptor desc (writable)
					 (not (eq? writable #t)))))))
		   properties)
		;; 3
		(with-access::JsObject o (extensible)
		   (not extensible))))))
      
      (js-bind! %this js-object 'isFrozen
	 :value (js-make-function %this isfrozen 1 'isFrozen)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; isExtensible
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.13
      (define (isextensible this obj)
	 (let ((o (js-cast-object obj %this "Object.isExtensible")))
	    (with-access::JsObject o (extensible)
	       extensible)))
      
      (js-bind! %this js-object 'isExtensible
	 :value (js-make-function %this isextensible 1 'isExtensible)
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      
      ;; keys
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.14
      (define (keys this obj)
	 (let ((o (js-cast-object obj %this "Object.keys")))
	    (js-vector->jsarray (js-properties-name o #t %this) %this)))
      
      (js-bind! %this js-object 'keys
	 :value (js-make-function %this keys 1 'keys)
	 :writable #t
	 :configurable #t
	 :enumerable #f)))

;*---------------------------------------------------------------------*/
;*    js-init-object-prototype! ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4       */
;*---------------------------------------------------------------------*/
(define (js-init-object-prototype! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this ((obj __proto__) js-object)
      
      ;; __proto__
      (js-bind! %this obj '__proto__
	 :enumerable #f
	 :configurable #f
	 :get (js-make-function %this
		 (lambda (o)
		    (let ((o (js-cast-object o %this "__proto__")))
		       (with-access::JsObject o (__proto__)
			  __proto__)))
		 1 'get)
	 :set (js-make-function %this
		 (lambda (o v)
		    (let ((o (js-cast-object o %this "__proto__"))
			  (v (js-cast-object v %this "__proto__")))
		       (with-access::JsObject o (extensible)
			  (if (not extensible)
			      (js-raise-type-error %this 
				 "Prototype of non-extensible object mutated" v)
			      (with-access::JsObject o (__proto__)
				 (set! __proto__ v))))))
		 2 'set))
      
      ;; constructor
      (js-bind! %this obj 'constructor
	 :value js-object
	 :enumerable #f)
      
      ;; toString
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.2
      (define (js-object-prototype-tostring this)
	 (cond
	    ((eq? this (js-undefined))
	     (js-string->jsstring "[object Undefined]"))
	    ((eq? this (js-null))
	     (js-string->jsstring "[object Null]"))
	    ((isa? this JsWrapper)
	     (with-access::JsWrapper this (obj)
		(js-string->jsstring (js-tostring obj %this))))
	    (else
	     (let* ((obj (js-toobject %this this))
		    (name (symbol->string! (class-name (object-class obj)))))
		(js-string->jsstring
		   (format "[object ~a]"
		      (cond
			 ((not (string-prefix? "Js" name))
			  name)
			 ((string=? name "JsGlobalObject")
			  "Object")
			 ((isa? obj JsArrayBufferView)
			  (let ((ctor (js-get obj 'constructor %this)))
			     (with-access::JsFunction ctor (name)
				name)))
			 (else
			  (substring name 2)))))))))
      
      (js-bind! %this obj 'toString
	 :value (js-make-function %this
		   js-object-prototype-tostring 0 'toString
		   :prototype (js-undefined))
	 :enumerable #f)
      
      ;; toLocaleString
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.3
      (define (js-object-prototype-tolocalestring this)
	 (js-call0 %this (js-get this 'toString %this) this))
      
      (js-bind! %this obj 'toLocaleString
	 :value (js-make-function %this
		   js-object-prototype-tolocalestring 0 'toLocaleString
		   :prototype (js-undefined))
	 :enumerable #f)
      
      ;; valueOf
      (js-bind! %this obj 'valueOf
	 :value (js-make-function %this (lambda (o) (js-valueof o %this))
		   0 'valueOf
		   :prototype (js-undefined))
	 :enumerable #f)
      
      ;; hasOwnProperty
      (js-bind! %this obj 'hasOwnProperty
	 :value (js-make-function %this
		   (lambda (this v)
		      (js-object-prototype-hasownproperty this v %this))
		   1 'hasOwnProperty
		   :prototype (js-undefined))
	 :enumerable #f)
      
      ;; isPrototypeOf
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.6
      (define (js-object-prototype-isprototypeof this v)
	 (when (isa? v JsObject)
	    (let ((o (js-toobject %this this)))
	       (let loop ((v v))
		  (with-access::JsObject v ((v __proto__))
		     (unless (eq? v (js-null))
			(or (eq? v o) (loop v))))))))
      
      (js-bind! %this obj 'isPrototypeOf
	 :value (js-make-function %this
		   js-object-prototype-isprototypeof 1 'isPrototypeOf
		   :prototype (js-undefined))
	 :enumerable #f)
      
      ;; propertyIsEnumerable
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.7
      (define (js-object-prototype-propertyisenumerable this v)
	 (let* ((p (js-tostring v %this))
		(o (js-toobject %this this))
		(desc (js-get-own-property o p %this)))
	    (if (eq? desc (js-undefined))
		#f
		(with-access::JsPropertyDescriptor desc (enumerable) enumerable))))
      
      (js-bind! %this obj 'propertyIsEnumerable
	 :value (js-make-function %this
		   js-object-prototype-propertyisenumerable 1 'propertyIsEnumerable
		   :prototype (js-undefined))
	 :enumerable #f)))


;*---------------------------------------------------------------------*/
;*    js-object-construct ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.2.1     */
;*---------------------------------------------------------------------*/


;*---------------------------------------------------------------------*/
;*    js-object-prototype-hasownproperty ...                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.5 */  */
;*---------------------------------------------------------------------*/
(define (js-object-prototype-hasownproperty this v %this)
   (let* ((p (js-tostring v %this))
	  (o (js-toobject %this this))
	  (desc (js-get-own-property o p %this)))
      (not (eq? desc (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    js-tonumber ::JsObject ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tonumber obj::JsObject %this)
   (let ((v (js-toprimitive obj 'number %this)))
      (js-tonumber (js-toprimitive obj 'number %this) %this)))
   
;*---------------------------------------------------------------------*/
;*    js-tointeger ::JsObject ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-tointeger obj::JsObject %this)
   (js-tointeger (js-tonumber obj %this) %this))
   
;*---------------------------------------------------------------------*/
;*    js-valueof ::JsGlobalObject ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsGlobalObject %this)
   (js-toobject this (js-null)))

;*---------------------------------------------------------------------*/
;*    js-valueof ::JsObject ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.4     */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsObject %this)
   this)

;*---------------------------------------------------------------------*/
;*    object-defineproperties ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.7     */
;*---------------------------------------------------------------------*/
(define (object-defineproperties %this::JsGlobalObject this obj properties)
   
   (define (vfor-each proc vec)
      (let ((len (vector-length vec)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (proc (vector-ref-ur vec i) i)
	       (loop (+fx i 1))))))

   (define (defineproperties/cmap cmap o props)
      (with-access::JsConstructMap cmap (names descriptors)
	 (vfor-each (lambda (name::symbol i)
		       (let ((prop (vector-ref-ur descriptors i)))
			  (with-access::JsPropertyDescriptor prop (enumerable)
			     (when (eq? enumerable #t)
				(let* ((descobj (if (isa? prop JsAccessorDescriptor)
						    ;; CARE 5 JUL 2014, not sure about props below
						    (js-property-value properties props prop %this)
						    (js-object-element-ref props i)))
				       (desc (js-to-property-descriptor %this
						descobj name)))
				   (js-define-own-property o name desc #t %this))))))
	    names)))
   
   (define (defineproperties/properties oprops o props)
      (for-each (lambda (prop)
		   (with-access::JsPropertyDescriptor prop ((p name) enumerable)
		      (when (eq? enumerable #t)
			 (let* ((descobj (js-property-value properties o prop %this))
				(desc (js-to-property-descriptor %this
					 descobj p)))
			    (js-define-own-property o p desc #t %this)))))
	 oprops))
   
   (let* ((o (js-cast-object obj %this "defineProperties"))
	  (props (js-cast-object (js-toobject %this properties) %this
		    "defineProperties")))
      (with-access::JsObject props (cmap (oprops properties))
	 (if cmap
	     (defineproperties/cmap cmap o props)
	     (defineproperties/properties oprops o props)))
      obj))

;*---------------------------------------------------------------------*/
;*    js-seal ::JsObject ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.8     */
;*---------------------------------------------------------------------*/
(define-method (js-seal o::JsObject obj)
   (when (>=fx (bigloo-debug) 3)
      (tprint "TODO, why js-seal need unmap?"))
   (js-object-unmap! obj)
   (with-access::JsObject o (properties)
      ;; 2
      (for-each (lambda (desc::JsPropertyDescriptor)
		   (with-access::JsPropertyDescriptor desc (name configurable)
		      (set! configurable #f)))
	 properties)
      ;; 3
      (with-access::JsObject o (extensible)
	 (set! extensible #f))
      ;; 4
      obj))

;*---------------------------------------------------------------------*/
;*    js-freeze ::JsObject ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.9     */
;*---------------------------------------------------------------------*/
(define-method (js-freeze o::JsObject obj)
   (when (>=fx (bigloo-debug) 3)
      (tprint "TODO, why js-freeze need unmap?"))
   (js-object-unmap! obj)
   (with-access::JsObject o (properties)
      (for-each js-freeze-property! properties)
      (with-access::JsObject o (extensible)
	 (set! extensible #f))
      obj))

;*---------------------------------------------------------------------*/
;*    js-get-global-object-name ...                                    */
;*    -------------------------------------------------------------    */
;*    This is an inlined version of js-get-own-property.               */
;*---------------------------------------------------------------------*/
(define (js-get-global-object-name o::JsObject name throw %this)
   (let ((pval (js-get-property-value o o name %this)))
      (if (eq? pval (js-absent))
	  (js-get-notfound name throw %this)
	  pval)))

;*---------------------------------------------------------------------*/
;*    js-get-global-object-name/cache ...                              */
;*---------------------------------------------------------------------*/
(define-inline (js-get-global-object-name/cache o::JsObject name::symbol cache::JsPropertyCache throw %this)
   (with-access::JsObject o ((omap cmap))
      (with-access::JsPropertyCache cache (cmap index)
	 (if (eq? cmap omap)
	     (js-object-element-ref o index)
	     (js-get-name/cache-miss o name cache throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-toprimitive ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.8       */
;*---------------------------------------------------------------------*/
(define-method (js-toprimitive o::JsObject preferredtype %this::JsGlobalObject)
   
   (define (err)
      (js-raise-type-error %this
	 "toPrimitive: illegal default value \"~a\""
	 (class-name (object-class o))))
   
   (define (get-field-value . fields)
      (let loop ((fields fields))
	 (if (null? fields)
	     (err)
	     (let ((proc (js-get o (car fields) %this)))
		(if (isa? proc JsFunction)
		    (let ((r (js-call0 %this proc o)))
		       (if (not (isa? r JsObject))
			   r
			   (loop (cdr fields))))
		    (loop (cdr fields)))))))
   
   (define (primitive-as-string)
      (get-field-value 'toString 'valueOf))
   
   (define (primitive-as-number)
      (get-field-value 'valueOf 'toString))

   (cond
      ((eq? preferredtype 'string)
       (primitive-as-string))
      ((eq? preferredtype 'number)
       (primitive-as-number))
      ((isa? o JsDate)
       (primitive-as-string))
      ((isa? o JsGlobalObject)
       ;; according to http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.8
       ;; the GlobalObject might be considered as a host object, which gives the
       ;; freedom to default to string instead of number
       (primitive-as-string))
      (else
       (primitive-as-number))))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
