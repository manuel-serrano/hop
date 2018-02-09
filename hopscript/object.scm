;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/object.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 17 08:43:24 2013                          */
;*    Last change :  Fri Feb  9 09:54:19 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
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
   
   (library hop hopwidget js2scheme)
   
   (include "types.sch" "stringliteral.sch" "property.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
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
	   (js-new-global-object::JsGlobalObject)))

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
	 (let ((nobj (duplicate::JsObject obj
			(__proto__ (js-get js-object 'prototype %this)))))
	    (js-object-properties-set! nobj '())
	    (js-object-mode-set! nobj (js-object-mode obj))
	    (js-for-in obj
	       (lambda (k)
		  (js-put! nobj k
		     (js-donate (js-get obj k %_this) worker %_this)
		     #f %this))
	       %this)
	    nobj))))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsObject ...                               */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value obj::JsObject)
   (js-jsobject->plist obj (js-initial-global-object)))

;*---------------------------------------------------------------------*/
;*    obj->json ::JsObject ...                                         */
;*---------------------------------------------------------------------*/
(define-method (obj->json obj::JsObject op::output-port)
   (let ((stringify (js-json-stringify (js-initial-global-object))))
      (display (stringify (js-undefined) obj (js-undefined) 1) op)))

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
;*    j2s-js-literal ::JsObject ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-js-literal o::JsObject)
   (with-access::JsService o (svc)
      (with-access::hop-service svc (path)
	 (call-with-output-string
	    (lambda (op)
	       (obj->json o op))))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsObject ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsObject op compile isexpr)
   (with-access::WorkerHopThread (js-current-worker) (%this)
      (display "{" op)
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
      (display "}" op)))

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
          :writable #f :configurable #f :enumerable #f :hidden-class #f)))

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
   (unless %this (set! %this (js-new-global-object)))
   %this)

;*---------------------------------------------------------------------*/
;*    make-cmap ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-cmap props)
   (instantiate::JsConstructMap
      (methods (make-vector (vector-length props)))
      (props props)))

;*---------------------------------------------------------------------*/
;*    js-new-global-object ...                                         */
;*---------------------------------------------------------------------*/
(define (js-new-global-object)
   ;; before all, initialize the builtin object prototype
   ;; and then create the global object
   (let* ((%prototype (instantiateJsObject
			 (__proto__ (js-null))
			 (elements (make-vector 20))
			 (cmap (instantiate::JsConstructMap))))
	  (%this (instantiateJsGlobalObject
		    (__proto__ %prototype)
		    (cmap (make-cmap '#())))))
      ;; for bootstrap, first allocat hasInstance symbol
      (with-access::JsGlobalObject %this (js-symbol-hasinstance)
	 (set! js-symbol-hasinstance
	    (instantiate::JsSymbolLiteral
	       (val "hasInstance"))))
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
	       :writable #t :enumerable #f :configurable #f :hidden-class #f)
	    (js-bind! %this %this 'NaN
	       :value +nan.0
	       :writable #f :enumerable #f :configurable #f :hidden-class #f)
	    (js-bind! %this %this 'Infinity
	       :value +inf.0
	       :writable #f :enumerable #f :configurable #f :hidden-class #f)
	    (js-bind! %this %this 'undefined
	       :value (js-undefined)
	       :writable #f :enumerable #f :configurable #f :hidden-class #f)

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
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)
	    
	    ;; parseInt
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.2
	    (define (parseint this string radix)
	       (js-parseint
		  (trim-whitespaces+ (js-tostring string %this) :plus #t)
		  radix #f %this))

	    (js-bind! %this %this 'parseInt
	       :value (js-make-function %this parseint 2 'parseInt
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

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
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; isNaN
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.4
	    (define (isnan this number)
	       (let ((n (js-tonumber number %this)))
		  (and (flonum? n) (not (=fl n n)))))

	    (js-bind! %this %this 'isNaN
	       :value (js-make-function %this isnan 1 'isNaN
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

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
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

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
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

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
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

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
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

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
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; escape
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.1
	    (define (escape this string)
	       (js-jsstring-escape (js-tojsstring string %this)))
	    
	    (js-bind! %this %this 'escape
	       :value (js-make-function %this escape 1 'escape
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; unescape
	    ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.1
	    (define (unescape this string)
	       (js-jsstring-unescape (js-tojsstring string %this) %this))

	    (js-bind! %this %this 'unescape
	       :value (js-make-function %this unescape 1 'unescape
			 :prototype (js-undefined))
	       :enumerable #f :configurable #t :writable #t :hidden-class #f)

	    ;; html_base
	    (js-bind-tags! %this %this
	       A ABBR ACRONYM ADDRESS APPLET AREA ARTICLE B BASE
	       BASEFONT BDI BDO BIG BLOCKQUOTE BODY BR BUTTON
	       CANVAS CAPTION CENTER CITE CODE COL COLGROUP
	       DATALIST DD DEL DFN DIR DIV DL DT EM EMBED FIELDSET FIGURE
	       FIGCAPTION FONT FOOTER FORM FRAME FRAMESET H1 H2 H3 H4 H5 H6
	       HR HEADER HGROUP I IFRAME INPUT INS ISINDEX KBD LABEL LEGEND
	       LI MAIN MAP MARQUEE MENU MENUITEM META METER NAV NOFRAMES NOSCRIPT
	       OBJECT OL OPTGROUP OPTION P PARAM PRE PROGRESS
	       Q S SAMP SECTION SELECT SMALL SOURCE SPAN STRIKE
	       STRONG SUB SUP TABLE TBODY TD TEXTAREA TFOOT TH
	       THEAD TIME TITLE TR TT U UL VAR REACT)

	    ;; html5
	    (js-bind-tags! %this %this
	       AUDIO VIDEO TRACK)

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
	       :value (js-html-html %this) :enumerable #f :hidden-class #f)

	    ;; only used with the global object, see nodejs/require.scm
	    ;; rebound in generated code
	    (js-bind! %this %this 'HEAD
	       :value (js-html-head %this) :enumerable #f :hidden-class #f)
	    (js-bind! %this %this 'SCRIPT
	       :value (js-html-script %this) :enumerable #f :hidden-class #f)

	    ;; html_head
	    (js-bind-tags! %this %this
	       LINK STYLE)

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
	       :enumerable #f :writable #f :configurable #f :hidden-class #f)

	    (define (string->xml-tilde body)
	       (let ((expr (js-tostring body %this)))
		  (instantiateJsWrapper
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
	       :enumerable #f :writable #f :configurable #f :hidden-class #f)

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
		(with-access::JsFunction f (constrmap constrsize)
		   (unless constrmap
		      (set! constrmap (instantiate::JsConstructMap (ctor f))))
		   (js-make-jsobject constrsize constrmap %prototype)))
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
	 (with-access::JsGlobalObject %this (js-object)
	    (js-object-constructor js-object
	       (if (pair? arg) (car arg) (js-undefined)))))
      
      (with-access::JsObject js-function ((js-function-prototype __proto__))
	 (set! js-object 
	    (js-make-function %this %js-object 1 'Object 
	       :__proto__ js-function-prototype
	       :constrsize 3 :maxconstrsize 4
	       :prototype %prototype
	       :construct js-object-construct
	       :constructor js-object-constructor
	       :shared-cmap #f)))
      
      ;; getPrototypeOf
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.2
      (define (getprototypeof this o)
	 (js-getprototypeof o %this "getPrototypeOf"))
      
      (js-bind! %this js-object 'getPrototypeOf
	 :value (js-make-function %this getprototypeof 1 'getPrototypeOf)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; setPrototypeOf
      ;; http://www.ecma-international.org/ecma-262/6.0/#sec-object.setprototypeof
      (define (setprototypeof this o v)
	 (js-setprototypeof o v %this "setPrototypeOf"))
      
      (js-bind! %this js-object 'setPrototypeOf
	 :value (js-make-function %this setprototypeof 1 'setPrototypeOf)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; getOwnPropertyDescriptor
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.3
      (define (getownpropertydescriptor this o p)
	 (let* ((o (if (isa? o object)
		       o
		       (js-cast-object o %this "getOwnPropertyDescriptor")))
		(desc (js-get-own-property o p %this)))
	    (js-from-property-descriptor %this desc o)))
      
      (js-bind! %this js-object 'getOwnPropertyDescriptor
	 :value (js-make-function %this
		   getownpropertydescriptor 2 'getOwnPropertyDescriptor)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
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
	 :enumerable #f
	 :hidden-class #f)
      
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
	 :enumerable #f
	 :hidden-class #f)
      
      ;; create
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.5
      (define (create this o properties)
	 (if (not (or (eq? o (js-null)) (isa? o JsObject)))
	     (js-raise-type-error %this "create: bad object ~s" o)
	     (let ((obj (js-new0 %this js-object)))
		(with-access::JsObject obj (__proto__ elements)
		   (set! __proto__ o)
		   (unless (eq? properties (js-undefined))
		      (object-defineproperties %this this obj properties)))
		obj)))

      (js-bind! %this js-object 'create
	 :value (js-make-function %this create 2 'create)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)

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
	 :enumerable #f
	 :hidden-class #f)

      ;; assign
      ;; http://www.ecma-international.org/ecma-262/6.0/#sec-object.assign
      ;; http://www.ecma-international.org/ecma-262/6.0/#sec-9.1.12
      (define (assign this target . sources)
	 
	 (define (idx-cmp a b)
	    (<=fx (string-natural-compare3
		     (symbol->string! a)
		     (symbol->string! b))
	       0))

	 (let ((to (js-cast-object target %this "assign")))
	    (for-each (lambda (src)
			 (unless (or (null? src) (eq? src (js-undefined)))
			    (let* ((fro (js-toobject %this src))
				   (names (js-properties-names fro #t %this))
				   (keys '())
				   (idx '()))
			       ;; keys have to be sorted as specified in
			       ;; section 9.1.12
			       (for-each (lambda (name)
					    (if (js-isindex? (js-toindex name))
						(set! idx (cons name idx))
						(set! keys (cons name keys))))
				  names)
			       (for-each (lambda (k)
					    (let ((val (js-get fro k %this)))
					       (js-put! to k val #t %this)))
				  (sort idx-cmp idx))
			       (for-each (lambda (k)
					    (let ((val (js-get fro k %this)))
					       (js-put! to k val #t %this)))
				  (reverse! keys)))))
	       sources)
	    to))

      (js-bind! %this js-object 'assign
	 :value (js-make-function %this assign 2 'assign)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; defineProperties
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.7
      (js-bind! %this js-object 'defineProperties
	 :value (js-make-function %this
		   (lambda (this obj properties)
		      (object-defineproperties %this this obj properties))
		   2 'defineProperties)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; seal
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.8
      (define (seal this obj)
	 (js-seal (js-cast-object obj %this "seal") obj))
      
      (js-bind! %this js-object 'seal
	 :value (js-make-function %this seal 1 'seal)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; freeze
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.9
      (define (freeze this obj)
	 (js-freeze (js-cast-object obj %this "freeze") obj))
      
      (js-bind! %this js-object 'freeze
	 :value (js-make-function %this freeze 1 'freeze)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; preventExtensions
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.10
      (define (preventextensions this obj)
	 (let ((o (js-cast-object obj %this "preventExtensions")))
	    (js-prevent-extensions o)
	    obj))
      
      (js-bind! %this js-object 'preventExtensions
	 :value (js-make-function %this preventextensions 1 'preventExtensions)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)

      (define (vector-every proc v)
	 (let loop ((i (-fx (vector-length v) 1)))
	    (cond
	       ((=fx i -1) #t)
	       ((proc (vector-ref v i)) (loop (-fx i 1)))
	       (else #f))))
      
      ;; isSealed
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.11
      (define (issealed this o)
	 ;; 1
	 (let ((o (js-cast-object o %this "isSealed")))
	    (with-access::JsObject o (cmap)
	       (and
		;; 2
		(with-access::JsConstructMap cmap (props)
		   (vector-every (lambda (p)
				    (let ((flags (prop-flags p)))
				       (not (flags-configurable? flags))))
		      props))
		(every (lambda (desc::JsPropertyDescriptor)
			  (with-access::JsPropertyDescriptor desc (configurable)
			     (not (eq? configurable #t))))
		   (js-object-properties o))
		;; 3
		(not (js-object-mode-extensible? o))))))
      
      (js-bind! %this js-object 'isSealed
	 :value (js-make-function %this issealed 1 'isSealed)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; isFrozen
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.12
      (define (isfrozen this o)
	 ;; 1
	 (let ((o (js-cast-object o %this "isFrozen")))
	    (with-access::JsObject o (cmap)
	       (and
		;; 2
		(with-access::JsConstructMap cmap (props)
		   (vector-every (lambda (p)
				    (let ((flags (prop-flags p)))
				       (and (not (flags-writable? flags))
					    (not (flags-configurable? flags)))))
			props))	     
		(every (lambda (desc::JsPropertyDescriptor)
			  (with-access::JsPropertyDescriptor desc (configurable)
			     (and (not (eq? configurable #t))
				  (or (not (isa? desc JsValueDescriptor))
				      (with-access::JsValueDescriptor desc (writable)
					 (not (eq? writable #t)))))))
		   (js-object-properties o))
		;; 3
		(not (js-object-mode-extensible? o))))))
      
      (js-bind! %this js-object 'isFrozen
	 :value (js-make-function %this isfrozen 1 'isFrozen)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; isExtensible
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.13
      (define (isextensible this obj)
	 (let ((o (js-cast-object obj %this "Object.isExtensible")))
	    (js-object-mode-extensible? o)))
      
      (js-bind! %this js-object 'isExtensible
	 :value (js-make-function %this isextensible 1 'isExtensible)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; keys
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.14
      (define (keys this obj)
	 (let ((o (if (isa? obj object)
		      obj
		      (js-cast-object obj %this "Object.keys"))))
	    (js-vector->jsarray (js-properties-name o #t %this) %this)))
      
      (js-bind! %this js-object 'keys
	 :value (js-make-function %this keys 1 'keys)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)))

;*---------------------------------------------------------------------*/
;*    js-getprototypeof ...                                            */
;*---------------------------------------------------------------------*/
(define (js-getprototypeof o %this msg)
   (let ((o (js-cast-object o %this msg)))
      (with-access::JsObject o (__proto__)
	 __proto__)))

;*---------------------------------------------------------------------*/
;*    js-setprototypeof ...                                            */
;*---------------------------------------------------------------------*/
(define (js-setprototypeof o v %this msg)
   (let ((o (js-cast-object o %this msg))
	 (v (js-cast-object v %this msg)))
      (if (not (js-object-mode-extensible? o))
	  (js-raise-type-error %this 
	     "Prototype of non-extensible object mutated" v)
	  (with-access::JsObject o (__proto__ cmap)
	     (js-invalidate-pcaches-pmap! %this)
	     (unless (eq? cmap (js-not-a-cmap))
		(set! cmap (duplicate::JsConstructMap cmap (%id (gencmapid)))))
	     (set! __proto__ v)))))

;*---------------------------------------------------------------------*/
;*    js-init-object-prototype! ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4       */
;*---------------------------------------------------------------------*/
(define (js-init-object-prototype! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this ((obj __proto__)
				       js-object
				       js-symbol-tostringtag)
      
      ;; __proto__
      (js-bind! %this obj '__proto__
	 :enumerable #f
	 :configurable #f
	 :get (js-make-function %this
		 (lambda (o)
		    (js-getprototypeof o %this "__proto__"))
		 1 'get)
	 :set (js-make-function %this
		 (lambda (o v)
		    (js-setprototypeof o v %this "__proto__"))
		 2 'set)
	 :hidden-class #f)
      
      ;; constructor
      (js-bind! %this obj 'constructor
	 :value js-object
	 :enumerable #f
	 :hidden-class #f)
      
      ;; toString
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.2
      (define (js-object-prototype-tostring this)
	 (cond
	    ((eq? this (js-undefined))
	     (js-ascii->jsstring "[object Undefined]"))
	    ((eq? this (js-null))
	     (js-ascii->jsstring "[object Null]"))
	    ((isa? this JsWrapper)
	     (with-access::JsWrapper this (obj)
		(js-string->jsstring (js-tostring obj %this))))
	    (else
	     (let* ((obj (js-toobject %this this))
		    (tag (js-get obj js-symbol-tostringtag %this)))
		(if (js-jsstring? tag)
		    (js-jsstring-append
		       (js-string->jsstring "[object")
		       (js-jsstring-append tag
			  (js-string->jsstring "]")))
		    (let* ((clazz (if (isa? obj JsFunction)
				      JsFunction
				      (object-class obj)))
			   (name (symbol->string! (class-name clazz))))
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
				 (substring name 2)))))))))))
      
      (js-bind! %this obj 'toString
	 :value (js-make-function %this
		   js-object-prototype-tostring 0 'toString
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #f)
      
      ;; toLocaleString
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.4.3
      (define (js-object-prototype-tolocalestring this)
	 (js-call0 %this (js-get this 'toString %this) this))
      
      (js-bind! %this obj 'toLocaleString
	 :value (js-make-function %this
		   js-object-prototype-tolocalestring 0 'toLocaleString
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #f)
      
      ;; valueOf
      (js-bind! %this obj 'valueOf
	 :value (js-make-function %this (lambda (o) (js-valueof o %this))
		   0 'valueOf
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #f)
      
      ;; hasOwnProperty
      (js-bind! %this obj 'hasOwnProperty
	 :value (js-make-function %this
		   (lambda (this v)
		      (js-object-prototype-hasownproperty this v %this))
		   1 'hasOwnProperty
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #f)
      
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
	 :enumerable #f
	 :hidden-class #f)
      
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
	 :enumerable #f
	 :hidden-class #f)))

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
	       (proc (vector-ref vec i) i)
	       (loop (+fx i 1))))))
	       
   (define (enumerable-mapped-property? obj i)
      (with-access::JsObject obj (elements)
	 (let ((el (vector-ref elements i)))
	    (if (isa? el JsPropertyDescriptor)
		(with-access::JsPropertyDescriptor el (enumerable) enumerable)
		#t))))

   (define (define-own-property o name prop)
      (let* ((descobj (if (isa? prop JsPropertyDescriptor)
			  (js-property-value properties prop %this)
			  prop))
	     (desc (js-to-property-descriptor %this descobj name)))
	 (js-define-own-property o name desc #t %this)))
   
   (define (defineproperties/cmap cmap o props)
      (with-access::JsObject props (elements)
	 (with-access::JsConstructMap cmap (props)
	    (vfor-each (lambda (d::struct i)
			  (when (flags-enumerable? (prop-flags d))
			     (let ((prop (vector-ref elements i)))
				(define-own-property o (prop-name d) prop))))
	       props))))
   
   (define (defineproperties/properties oprops o props)
      (for-each (lambda (prop)
		   (with-access::JsPropertyDescriptor prop (name enumerable)
		      (when (eq? enumerable #t)
			 (define-own-property o name prop))))
	 oprops))
   
   (let* ((o (js-cast-object obj %this "defineProperties"))
	  (props (js-cast-object (js-toobject %this properties) %this
		    "defineProperties")))
      (with-access::JsObject props (cmap)
	 (let ((oprops (js-object-properties props)))
	    (if (not (eq? cmap (js-not-a-cmap)))
		(defineproperties/cmap cmap o props)
		(defineproperties/properties oprops o props))))
      obj))

;*---------------------------------------------------------------------*/
;*    js-seal ::JsObject ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.8     */
;*---------------------------------------------------------------------*/
(define-method (js-seal o::JsObject obj)
   
   (define (prop-seal p)
      (let ((flags (prop-flags p)))
	 (prop (prop-name p)
	    (property-flags
	       (flags-writable? flags)
	       (flags-enumerable? flags)
	       #f
	       (flags-accessor? flags)))))
   
   (for-each (lambda (desc::JsPropertyDescriptor)
		(with-access::JsPropertyDescriptor desc (configurable)
		   (set! configurable #f)))
      (js-object-properties o))
   (js-object-mode-extensible-set! o #f)
   (with-access::JsObject o (cmap)
      (unless (eq? cmap (js-not-a-cmap))
	 (with-access::JsConstructMap cmap (props)
	    (let ((ncmap (duplicate::JsConstructMap cmap
			    (props (vector-map prop-seal props)))))
	       (set! cmap ncmap)))))
   obj)

;*---------------------------------------------------------------------*/
;*    js-freeze ::JsObject ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.2.3.9     */
;*---------------------------------------------------------------------*/
(define-method (js-freeze o::JsObject obj)
   
   (define (prop-freeze p)
      (let ((flags (prop-flags p)))
	 (prop (prop-name p)
	    (property-flags
	       #f
	       (flags-enumerable? flags)
	       #f
	       (flags-accessor? flags)))))

   (for-each js-freeze-property! (js-object-properties o))
   (js-object-mode-extensible-set! o #f)
   (with-access::JsObject o (cmap)
      (unless (eq? cmap (js-not-a-cmap))
	 (with-access::JsConstructMap cmap (props)
	    (let ((ncmap (duplicate::JsConstructMap cmap
			    (props (vector-map prop-freeze props)))))
	       (set! cmap ncmap)))))
   obj)

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
