;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/string.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Sun May 21 09:34:05 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript strings                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_string

   (include "../nodejs/nodejs_debug.sch")
   
   (library hop js2scheme)
   
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
	   __hopscript_symbol
	   __hopscript_generator
	   __hopscript_worker)

   (export (js-init-string! ::JsGlobalObject)
	   (js-template-raw ::JsArray ::JsArray ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsString ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsString
   (lambda (o)
      (with-access::JsString o (val)
	 (js-jsstring->string val)))
   (lambda (o %this)
      (let ((this (or %this (js-initial-global-object))))
	 (with-access::JsGlobalObject this (js-string)
	    (instantiate::JsString
	       (val (js-string->jsstring o))
	       (__proto__ (js-get js-string 'prototype this)))))))

;*---------------------------------------------------------------------*/
;*    js-tostring ::JsString ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-tostring obj::JsString %this::JsGlobalObject)
   (with-access::JsString obj (val)
      (js-jsstring->string val)))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsString ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsString worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-string)
	 (let ((nobj (call-next-method)))
	    (with-access::JsString nobj (__proto__ val)
	       (set! __proto__ (js-get js-string 'prototype %this))
	       (set! val (js-donate val worker %_this)))
	    nobj))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsString ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsString op compile isexpr)
   (with-access::JsString o (val)
      (display "new String(\"" op)
      (display (string-for-read (js-jsstring->string val)) op)
      (display "\")" op)))

;*---------------------------------------------------------------------*/
;*    j2s-js-literal ::JsString ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-js-literal o::JsString)
   (with-access::JsString o (val)
      (j2s-js-literal (js-jsstring->string val))))

;*---------------------------------------------------------------------*/
;*    j2s-js-literal ::JsStringLiteral ...                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js-literal o::JsStringLiteral)
   (j2s-js-literal (js-jsstring->string o)))

;*---------------------------------------------------------------------*/
;*    js-init-string! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-string! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-string js-function)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; builtin prototype
	 (define js-string-prototype
	    (instantiate::JsString
	       (val (js-ascii->jsstring ""))
	       (__proto__ __proto__)))

	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5
	 (define (js-string-construct o::JsString . arg)
	    
	    (define (set-ascii-string! str)
	       (let ((len (instantiate::JsValueDescriptor
			     (name 'length)
			     (writable #f)
			     (configurable #f)
			     (enumerable #f)
			     (value (string-length str)))))
		  (with-access::JsString o (val properties)
		     (set! val (js-ascii->jsstring str))
		     (set! properties (list len)))))
	    
	    (define (set-string! str)
	       (let ((len (instantiate::JsValueDescriptor
			     (name 'length)
			     (writable #f)
			     (configurable #f)
			     (enumerable #f)
			     (value (uint32->fixnum
				       (js-jsstring-codeunit-length str))))))
		  (with-access::JsString o (val properties)
		     (set! val str)
		     (set! properties (list len)))))
	    
	    (if (null? arg)
		;; 2
		(set-ascii-string! "")
		(let ((value (car arg)))
		   (cond
		      ((string? value)
		       (set-ascii-string! value))
		      ((isa? value JsStringLiteral)
		       (set-string! value))
		      ((isa? value JsObject)
		       (set-string! (js-cast-string %this value)))
		      (else
		       (let ((str (js-tostring value %this)))
			  (set-string! (js-string->jsstring str))))))))

	 ;; string allocation
	 (define (js-string-alloc::JsString constructor::JsFunction)
	    (instantiate::JsString
	       (val (js-ascii->jsstring ""))
	       (__proto__ (js-get constructor 'prototype %this))))

	 ;; then, create a HopScript object
	 (set! js-string
	    (js-make-function %this
	       (%js-string %this) 1 'String
	       :__proto__ js-function-prototype
	       :prototype js-string-prototype
	       :construct js-string-construct
	       :alloc js-string-alloc))
	 
	 ;; fromCharCode
	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.3.2
	 (define (js-string-fromcharcode this . l)
	    (js-string->jsstring
	       (ucs2-string->utf8-string
		  (apply ucs2-string
		     (map (lambda (c)
			     (integer->ucs2
				(uint16->fixnum (js-touint16 c %this))))
			l)))))
	 
	 (js-bind! %this js-string 'fromCharCode
	    :value (js-make-function %this
		      js-string-fromcharcode 1 'fromCharCode)
	    :writable #t
	    :enumerable #f
	    :configurable #t
	    :hidden-class #t)

	 ;; raw
	 ;; http://www.ecma-international.org/ecma-262/6.0/#21.1.2.4
	 (define (js-string-raw this . a)
	    (let ((template (car a))
		  (substitutions (cdr a)))
	       (if (isa? template JsObject)
		   (let ((raw (js-get template 'raw %this)))
		      (if (isa? raw JsObject)
			  ;; step 9
			  (let ((literalsegments (js-get raw 'length %this)))
			     (let loop ((strs '())
					(nextindex 0)
					(subs substitutions))
				(if (=fx nextindex literalsegments)
				    (js-stringlist->jsstring (reverse! strs))
				    (let ((nextseg (js-tostring
						      (js-get raw nextindex %this)
						      %this)))
				       (if (pair? subs)
					   (loop (cons* (js-tostring (car subs) %this)
						    nextseg
						    strs)
					      (+fx nextindex 1)
					      (cdr subs))
					   (loop (cons nextseg strs)
					      (+fx nextindex 1)
					      subs))))))
			  (js-undefined)))
		   (js-undefined))))
		    
	 (js-bind! %this js-string 'raw
	    :value (js-make-function %this
		      js-string-raw 1 'raw)
	    :writable #t
	    :enumerable #f
	    :configurable #t
	    :hidden-class #t)
	 
	 ;; prototype properties
	 (init-builtin-string-prototype! %this js-string js-string-prototype)
	 
	 ;; bind String in the global object
	 (js-bind! %this %this 'String
	    :configurable #f :enumerable #f :value js-string
	    :hidden-class #t)

	 js-string)))

;*---------------------------------------------------------------------*/
;*    js-cast-string ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-cast-string %this::JsGlobalObject obj)
   (cond
      ((js-jsstring? obj)
       obj)
      ((isa? obj JsString)
       (with-access::JsString obj (val)
	  val))
      (else
       (js-tojsstring (js-toobject %this obj) %this))))

;*---------------------------------------------------------------------*/
;*    js-cast-string-normalize! ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-cast-string-normalize!::obj %this::JsGlobalObject obj)
   (cond
      ((string? obj)
       obj)
      ((isa? obj JsStringLiteral)
       (js-jsstring-normalize! obj))
      ((isa? obj JsString)
       (with-access::JsString obj (val)
	  (js-cast-string-normalize! %this val)))
      (else
       (js-tostring (js-toobject %this obj) %this))))

;*---------------------------------------------------------------------*/
;*    %js-string ...                                                   */
;*---------------------------------------------------------------------*/
(define (%js-string %this)
   (lambda (this . args)
      (if (null? args)
	  (js-ascii->jsstring "")
	  (js-string->jsstring (js-tostring (car args) %this)))))

;*---------------------------------------------------------------------*/
;*    js-valueof ::JsString ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsString %this)
   (with-access::JsString this (val)
      val))

;*---------------------------------------------------------------------*/
;*    init-builtin-string-prototype! ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.3.1     */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4       */
;*---------------------------------------------------------------------*/
(define (init-builtin-string-prototype! %this::JsGlobalObject js-string obj)
   
   ;; length
   (js-bind! %this obj 'length
      :value 0
      :enumerable #f
      :hidden-class #t)
   
   ;; constructor
   (js-bind! %this obj 'constructor
      :value js-string
      :enumerable #f
      :hidden-class #t)
   
   ;; toString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.2
   (define (tostring this)
      (cond
	 ((isa? this JsString)
	  (with-access::JsString this (val) val))
	 ((js-jsstring? this)
	  this)
	 (else
	  (js-raise-type-error %this "argument not a string ~a" (typeof this)))))
   
   (js-bind! %this obj 'toString
      :value (js-make-function %this tostring 0 'toString)
      :enumerable #f
      :hidden-class #t)
   
   ;; valueOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.3
   (define (valueof this)
      (cond
	 ((isa? this JsString)
	  (with-access::JsString this (val) val))
	 ((js-jsstring? this)
	  this)
	 (else
	  (js-raise-type-error %this "argument not a string ~a" this))))
   
   (js-bind! %this obj 'valueOf
      :value (js-make-function %this valueof 0 'valueOf)
      :enumerable #f
      :hidden-class #t)
   
   ;; charAt
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.4
   (define (charat this index)
      (js-jsstring-charat (js-cast-string-normalize! %this this) index %this))
   
   (js-bind! %this obj 'charAt
      :value (js-make-function %this charat 1 'charAt)
      :enumerable #f
      :hidden-class #t)
   
   ;; charCodeAt
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.5
   (define (charcodeat this index)
      (js-jsstring-charcodeat (js-cast-string-normalize! %this this) index %this))
   
   (js-bind! %this obj 'charCodeAt
      :value (js-make-function %this charcodeat 1 'charCodeAt)
      :enumerable #f
      :hidden-class #t)
   
   ;; concat
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.6
   (define (concat this . rest)
      (let ((left (js-cast-string %this this)))
	 (match-case rest
	    (()
	     left)
	    ((?right)
	     (js-jsstring-append left (js-tojsstring right %this)))
	    ((?r1 ?r2)
	     (js-jsstring-append left
		(js-jsstring-append
		   (js-tojsstring r1 %this)
		   (js-tojsstring r2 %this))))
	    ((?r1 ?r2 ?r3)
	     (js-jsstring-append left
		(js-jsstring-append
		   (js-tojsstring r1 %this)
		   (js-jsstring-append
		      (js-tojsstring r2 %this)
		      (js-tojsstring r3 %this)))))
	    (else
	     (let loop ((str left)
			(rest rest))
		(if (null? rest)
		    str
		    (loop (js-jsstring-append str
			     (js-tojsstring (car rest) %this))
		       (cdr rest))))))))
   
   (js-bind! %this obj 'concat
      :value (js-make-function %this concat 1 'concat)
      :enumerable #f
      :hidden-class #t)

   ;; indexOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.7
   (define (indexof this::obj search position)
      (let ((searchstr (js-tostring search %this)))
	 (js-jsstring-indexof
	    (js-cast-string-normalize! %this this) searchstr position %this)))

   (js-bind! %this obj 'indexOf
      :value (js-make-function %this indexof 1 'indexOf)
      :enumerable #f
      :hidden-class #t)
   
   ;; lastIndexOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.8
   (define (last-indexof this search position)
      (let ((searchstr (js-tostring search %this)))
	 (js-jsstring-lastindexof
	    (js-cast-string-normalize! %this this) searchstr position %this)))
   
   (js-bind! %this obj 'lastIndexOf
      :value (js-make-function %this last-indexof 1 'lastIndexOf)
      :enumerable #f
      :hidden-class #t)
   
   ;; localeCompare
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.9
   (define (locale-compare this::obj that)
      (js-jsstring-localecompare (js-cast-string-normalize! %this this) that %this))
   
   (js-bind! %this obj 'localeCompare
      :value (js-make-function %this locale-compare 1 'localeCompare)
      :enumerable #f
      :hidden-class #t)
   
   ;; naturalCompare
   ;; hopscript extension
   (define (natural-compare this::obj that)
      (js-jsstring-naturalcompare (js-cast-string-normalize! %this this) that %this))
   
   (js-bind! %this obj 'naturalCompare
      :value (js-make-function %this natural-compare 1 'naturalCompare)
      :enumerable #f
      :hidden-class #t)
   
   ;; match
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.10
   (define (match this::obj regexp)
      (js-jsstring-match (js-cast-string %this this) regexp %this))
   
   (js-bind! %this obj 'match
      :value (js-make-function %this match 1 'match)
      :enumerable #f
      :hidden-class #t)
   
   ;; replace
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11
   (define (replace this::obj searchvalue replacevalue)
      (js-jsstring-replace (js-cast-string %this this)
	 searchvalue replacevalue %this))

      
   (js-bind! %this obj 'replace
      :value (js-make-function %this replace 2 'replace)
      :enumerable #f
      :hidden-class #t)
   
   ;; search
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.12
   (define (search this::obj regexp)
      (with-access::JsGlobalObject %this (js-regexp)
	 (let ((string (js-jsstring->string (js-cast-string %this this)))
	       (rx (if (isa? regexp JsRegExp)
		       regexp
		       (js-new %this js-regexp regexp))))
	    (with-access::JsRegExp rx (rx global)
	       (let ((pos (pregexp-match-positions rx string)))
		  (if pos
		      (caar pos)
		      -1))))))
   (js-bind! %this obj 'search
      :value (js-make-function %this search 1 'search)
      :enumerable #f
      :hidden-class #t)
   
   ;; slice
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.13
   (define (slice this::obj start end)
      (let* ((jss (js-cast-string %this this))
	     (len (uint32->fixnum (js-jsstring-length jss)))
	     (intstart (js-tointeger start %this))
	     (intend (if (eq? end (js-undefined)) len (js-tointeger end %this)))
	     (from (->fixnum
		      (if (< intstart 0)
			  (max (+ len intstart) 0)
			  (min intstart len))))
	     (to (->fixnum
		    (if (< intend 0)
			(max (+ len intend) 0)
			(min intend len))))
	     (span (maxfx (-fx to from) 0))
	     (end (+ from span)))
	 (if (or (>fx from 0) (<fx end len))
	     (js-jsstring-substring jss from end %this)
	     jss)))
   
   (js-bind! %this obj 'slice
      :value (js-make-function %this slice 2 'slice)
      :enumerable #f
      :hidden-class #t)
   
   ;; split
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.14
   (define (split this::obj separator limit)
      (js-jsstring-split (js-cast-string %this this) separator limit %this))
   
   (js-bind! %this obj 'split
      :value (js-make-function %this split 2 'split)
      :enumerable #f
      :hidden-class #t)
   
   ;; substring
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.15
   (define (js-substring this::obj start end)
      (js-jsstring-substring (js-cast-string-normalize! %this this) start end %this))
   
   (js-bind! %this obj 'substring
      :value (js-make-function %this js-substring 2 'substring)
      :enumerable #f
      :hidden-class #t)
   
   ;; toLowerCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.16
   (define (tolowercase this::obj)
      (js-jsstring-tolowercase (js-cast-string-normalize! %this this)))
   
   (js-bind! %this obj 'toLowerCase
      :value (js-make-function %this tolowercase 0 'toLowerCase)
      :enumerable #f
      :hidden-class #t)
   
   ;; toLocaleLowerCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.17
   (define (tolocalelowercase this::obj)
      (js-jsstring-tolocalelowercase (js-cast-string-normalize! %this this)))
   
   (js-bind! %this obj 'toLocaleLowerCase
      :value (js-make-function %this tolocalelowercase 0 'toLocaleLowerCase)
      :enumerable #f
      :hidden-class #t)
   
   ;; toUpperCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.18
   (define (touppercase this::obj)
      (js-jsstring-touppercase (js-cast-string-normalize! %this this)))
   
   (js-bind! %this obj 'toUpperCase
      :value (js-make-function %this touppercase 0 'toUpperCase)
      :enumerable #f
      :hidden-class #t)
   
   ;; toLocaleUpperCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.19
   (define (tolocaleuppercase this::obj)
      (js-jsstring-tolocaleuppercase (js-cast-string-normalize! %this this)))
   
   (js-bind! %this obj 'toLocaleUpperCase
      :value (js-make-function %this tolocaleuppercase 0 'toLocaleUpperCase)
      :enumerable #f
      :hidden-class #t)
   
   ;; trim
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.20
   (define (trim this::obj)
      (js-jsstring-trim (js-cast-string-normalize! %this this)))
   
   (js-bind! %this obj 'trim
      :value (js-make-function %this trim 0 'trim)
      :enumerable #f
      :hidden-class #t)
   
   ;; substr
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.3
   (define (substr this::obj start length)
      (js-jsstring-substr (js-tostring this %this) start length %this))
   
   (js-bind! %this obj 'substr
      :value (js-make-function %this substr 2 'substr)
      :enumerable #f
      :hidden-class #t)
   
   ;; iterator
   ;; http://www.ecma-international.org/ecma-262/6.0/#sec-21.1.3.27
   (define (string-prototype-string-values this::obj)
      ;; because of code point and code units, cannot use the generic
      ;; js-make-iterator (see generator.scm) function
      (letrec ((%gen (js-make-generator
			(lambda (%v %e)
			   (let* ((val (js-cast-string %this this))
				  (len (js-jsstring-character-length val)))
			      (let ((i #u32:0))
				 (let loop ((%v %v) (%e %e))
				    (if (>=u32 i len)
					(js-generator-yield %gen
					   (js-undefined) #t
					   loop %this)
					(let ((char (js-jsstring-character-ref val i)))
					   (set! i (+u32 i #u32:1))
					   (js-generator-yield %gen
					      char #f
					      loop %this)))))))
			(with-access::JsGlobalObject %this (js-generator-prototype)
			   js-generator-prototype)
			%this)))
	 %gen))
   
   (with-access::JsGlobalObject %this (js-symbol-iterator)
      (js-bind! %this obj js-symbol-iterator
	 :value (js-make-function %this string-prototype-string-values
		   0 '@@iterator
		   :prototype (js-undefined))
	 :enumerable #f
	 :hidden-class #t)))

;*---------------------------------------------------------------------*/
;*    js-tonumber ::JsString ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tonumber this::JsString %this)
   (with-access::JsString this (val)
      (js-tonumber val %this)))

;*---------------------------------------------------------------------*/
;*    js-tointeger ::JsString ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tointeger this::JsString %this)
   (with-access::JsString this (val)
      (js-tointeger val %this)))

;*---------------------------------------------------------------------*/
;*    js-properties-name ::JsString ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-properties-name obj::JsString enump %this)
   (with-access::JsString obj (val)
      (vector-append
	 (apply vector
	    (map! js-integer->jsstring
	       (iota
		  (uint32->fixnum
		     (js-jsstring-character-length (js-cast-string %this val))))))
	 (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsString ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.5.2     */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsString p %this)
   (let ((index (js-toindex p)))
      (if (js-isindex? index)
	  (let* ((len (js-jsstring-character-length (js-cast-string %this o))))
	     (if (<=u32 len index)
		 (call-next-method)
		 #t))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::JsString ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.5.2     */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsString p %this)
   
   (define (ascii-get-own-property val::bstring index)
      (let ((len (string-length val)))
	 (if (<=fx len index)
	     (call-next-method)
	     (instantiate::JsValueDescriptor
		(name (js-toname p %this))
		(value (js-ascii->jsstring
			  (make-string 1 (string-ref-ur val index))))
		(enumerable #t)
		(writable #f)
		(configurable #f)))))
   
   (define (utf8-get-own-property str::JsStringLiteralUTF8 index)
      (let* ((val (js-jsstring->string str))
	     (len (utf8-string-length val)))
	 (if (<=fx len index)
	     (call-next-method)
	     (instantiate::JsValueDescriptor
		(name (js-toname p %this))
		(value (js-utf8-ref str val index))
		(enumerable #t)
		(writable #f)
		(configurable #f)))))
   
   (let ((index (js-toindex p)))
      (if (js-isindex? index)
	  (with-access::JsString o (val)
	     (let ((index (uint32->fixnum index)))
		(cond
		   ((string? val)
		    (ascii-get-own-property val index))
		   ((isa? val JsStringLiteralASCII)
		    (ascii-get-own-property (js-jsstring->string val) index))
		   (else
		    (utf8-get-own-property val index)))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get-property-value ::JsString ...                             */
;*    -------------------------------------------------------------    */
;*    This method is optional. It could be removed without changing    */
;*    the programs behaviors. It merely optimizes access to strings.   */
;*---------------------------------------------------------------------*/
(define-method (js-get-property-value o::JsString base p::obj %this::JsGlobalObject)
   
   (define (ascii-get-property-value val index)
      (let ((len (string-length val)))
	 (if (<=fx len index)
	     (call-next-method)
	     (js-ascii->jsstring
		(make-string 1 (string-ref-ur val index))))))
   
   (define (utf8-get-property-value str::JsStringLiteralUTF8 index)
      (let* ((val (js-jsstring->string str))
	     (len (utf8-string-length val)))
	 (if (<=fx len index)
	     (call-next-method)
	     (js-utf8-ref str val index))))
   
   (let ((index (js-toindex p)))
      (if (not (js-isindex? index))
	  (call-next-method)
	  (with-access::JsString o (val)
	     (let ((index (uint32->fixnum index)))
		(cond
		   ((string? val)
		    (ascii-get-property-value val index))
		   ((isa? val JsStringLiteralASCII)
		    (ascii-get-property-value (js-jsstring->string val) index))
		   (else
		    (utf8-get-property-value val index))))))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsString ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsString p %this)
   (let ((index (js-toindex p)))
      (if (not (js-isindex? index))
	  (call-next-method)
	  (with-access::JsString o (val)
	     (js-jsstring-ref val index)))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsString ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::JsString proc %this)
   (with-access::JsString o (val)
      (let ((len (js-jsstring-length val)))
	 (if (>u32 len #u32:0)
	     (let loop ((i #u32:0))
		(if (<u32 i len)
		    (begin
		       (proc (js-integer->jsstring (uint32->fixnum i)))
		       (loop (+u32 i #u32:1)))
		    (call-next-method)))
	     (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-template-raw ...                                              */
;*---------------------------------------------------------------------*/
(define (js-template-raw arr::JsArray raw::JsArray %this)
   (js-bind! %this arr 'raw :value raw
      :writable #f :enumerable #f :configurable #f
      :hidden-class #t)
   (js-freeze arr %this)
   (js-freeze raw %this)
   arr)

;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
