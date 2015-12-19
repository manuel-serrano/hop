;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/string.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Sat Dec 12 11:48:15 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
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

   (export (js-init-string! ::JsGlobalObject)
	   (utf8-codeunit-length::long ::bstring)))

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
;*    js-init-string! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-string! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-string js-function)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; builtin prototype
	 (define js-string-prototype
	    (instantiate::JsString
	       (val (js-string->jsstring ""))
	       (__proto__ __proto__)
	       (extensible #t)))

	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5
	 (define (js-string-construct o::JsString . arg)
	    
	    (define (set-string! v)
	       (let ((len (instantiate::JsValueDescriptor
			     (name 'length)
			     (writable #f)
			     (configurable #f)
			     (enumerable #f)
			     (value (utf8-codeunit-length v))))
		     (str (string-ascii-sentinel-mark! v)))
		  (with-access::JsString o (val properties)
		     (set! val (js-string->jsstring str))
		     (set! properties (list len)))))
	    
	    (if (null? arg)
		;; 2
		(set-string! "")
		(let ((value (car arg)))
		   (if (string? value)
		       (set-string! value)
		       (set-string! (js-tostring value %this))))))

	 ;; string allocation
	 (define (js-string-alloc::JsString constructor::JsFunction)
	    (instantiate::JsString
	       (val (js-string->jsstring ""))
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
	    :configurable #t)

	 ;; raw
	 ;; http://www.ecma-international.org/ecma-262/6.0/#21.1.2.4
	 (define (js-string-raw this . a)
	    (js-stringlist->jsstring
	       (map! (lambda (v) (js-tostring v %this)) a)))
		    
	 (js-bind! %this js-string 'raw
	    :value (js-make-function %this
		      js-string-raw 1 'raw)
	    :writable #t
	    :enumerable #f
	    :configurable #t)
	 
	 ;; prototype properties
	 (init-builtin-string-prototype! %this js-string js-string-prototype)
	 
	 ;; bind String in the global object
	 (js-bind! %this %this 'String
	    :configurable #f :enumerable #f :value js-string)
	 js-string)))

;*---------------------------------------------------------------------*/
;*    utf8-codeunit-length ...                                         */
;*    -------------------------------------------------------------    */
;*    Returns the number of code points required to encode that        */
;*    UTF8 string (might be bigger than the UTF8 length).              */
;*---------------------------------------------------------------------*/
(define (utf8-codeunit-length str)
   (let ((len (string-length str))
	 (sen (string-ascii-sentinel str)))
      (if (>fx sen len)
	  len
	  (let loop ((r 0)
		     (l 0))
	     (if (>=fx r len)
		 l
		 (let* ((c (string-ref str r))
			(s (utf8-char-size c)))
		    (if (and (=fx s 4)
			     (or (=fx (char->integer c) #xf0)
				 (=fx (char->integer c) #xf4)))
			(loop (+fx r s) (+fx l 2))
			(loop (+fx r s) (+fx l 1)))))))))

;*---------------------------------------------------------------------*/
;*    codepoint-length ...                                             */
;*    -------------------------------------------------------------    */
;*    Returns the number of code units of this code point.             */
;*---------------------------------------------------------------------*/
(define (codepoint-length s c)
   (case (char->integer c)
      ((#xf0 #xf4 #xf8 #xfc) 2)
      (else 1)))

;*---------------------------------------------------------------------*/
;*    utf8-left-replacement-codeunit ...                               */
;*    -------------------------------------------------------------    */
;*    See UCS2-STRING->UTF8-STRING.                                    */
;*---------------------------------------------------------------------*/
(define (utf8-left-replacement-codeunit str r)
   (let* ((b1 (char->integer (string-ref str (+fx 1 r))))
	  (b2 (char->integer (string-ref str (+fx 2 r))))
	  (b3 (char->integer (string-ref str (+fx 3 r))))
	  (u4u3 (bit-lsh (bit-and b3 #x3) 2))
	  (xx (bit-and (bit-rsh b2 4) #x3))
	  (wwww (bit-and b1 #xf))
	  (u2u1 (bit-and (bit-rsh b1 4) #x3))
	  (uuuu (bit-or u4u3 u2u1))
	  (vvvv (-fx uuuu 1))
	  (hi #b110110))
      (bit-or xx
	 (bit-or
	    (bit-lsh wwww 2)
	    (bit-or (bit-lsh vvvv 6) (bit-lsh hi 10))))))
   
;*---------------------------------------------------------------------*/
;*    utf8-right-replacement-codeunit ...                              */
;*    -------------------------------------------------------------    */
;*    See UCS2-STRING->UTF8-STRING.                                    */
;*---------------------------------------------------------------------*/
(define (utf8-right-replacement-codeunit str r)
   (let* ((b1 (char->integer (string-ref str (+fx 1 r))))
	  (b2 (char->integer (string-ref str (+fx 2 r))))
	  (b3 (char->integer (string-ref str (+fx 3 r))))
	  (zzzzzz (bit-and b3 #x3f))
	  (yyyy (bit-and b2 #xf))
	  (hi #b110111))
      (bit-or zzzzzz (bit-or (bit-lsh yyyy 6) (bit-lsh hi 10)))))
   
;*---------------------------------------------------------------------*/
;*    utf8-string-codeunit-ref ...                                     */
;*    -------------------------------------------------------------    */
;*    Returns the ith code unit (UTF16 code unit) of the UTF8 source   */
;*    string.                                                          */
;*---------------------------------------------------------------------*/
(define (utf8-codeunit-ref str i::int)
   (let ((sentinel (string-ascii-sentinel str)))
      (if (<fx i sentinel)
	  (char->integer (string-ref str i))
	  (let ((len (string-length str)))
	     (let loop ((r sentinel) (i (-fx i sentinel)))
		(let* ((c (string-ref str r))
		       (s (utf8-char-size c))
		       (u (codepoint-length s c)))
		   (cond
		      ((>=fx i u)
		       (loop (+fx r s) (-fx i u)))
		      ((=fx s 1)
		       (char->integer (string-ref str r)))
		      ((char=? c (integer->char #xf8))
		       (utf8-left-replacement-codeunit str r))
		      ((char=? c (integer->char #xfc))
		       (utf8-right-replacement-codeunit str r))
		      (else
		       (let* ((utf8 (substring str r (+fx r s)))
			      (ucs2 (utf8-string->ucs2-string utf8)))
			  (ucs2->integer (ucs2-string-ref ucs2 i)))))))))))

;*---------------------------------------------------------------------*/
;*    js-cast-string ...                                               */
;*---------------------------------------------------------------------*/
(define (js-cast-string %this obj)
   (if (js-jsstring? obj) obj (js-toobject %this obj)))

;*---------------------------------------------------------------------*/
;*    %js-string ...                                                   */
;*---------------------------------------------------------------------*/
(define (%js-string %this)
   (lambda::JsStringLiteral (this . args)
      (if (null? args)
	  (js-string->jsstring "")
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
      :enumerable #f)
   
   ;; constructor
   (js-bind! %this obj 'constructor
      :value js-string
      :enumerable #f)
   
   ;; toString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.2
   (define (tostring::JsStringLiteral this)
      (if (isa? this JsString)
	  (with-access::JsString this (val) val)
	  (js-raise-type-error %this "argument not a string ~a" (typeof this))))
   
   (js-bind! %this obj 'toString
      :value (js-make-function %this tostring 0 'toString)
      :enumerable #f)
   
   ;; valueOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.3
   (define (valueof::JsStringLiteral this)
      (if (isa? this JsString)
	  (with-access::JsString this (val) val)
	  (js-raise-type-error %this "argument not a string ~a" this)))
   
   (js-bind! %this obj 'valueOf
      :value (js-make-function %this valueof 0 'valueOf)
      :enumerable #f)
   
   ;; charAt
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.4
   (define (charat::JsStringLiteral this index)
      (let* ((val (js-tostring (js-cast-string %this this) %this))
	     (pos (js-tointeger index %this))
	     (fxpos (->fixnum pos)))
	 (cond
	    ((or (<fx fxpos 0) (>=fx fxpos (utf8-codeunit-length val)))
	     (js-string->jsstring ""))
	    ((<fx fxpos (string-ascii-sentinel val))
	     (js-string->jsstring
		(string-ascii-sentinel-set!
		   (string (string-ref val fxpos))
		   1)))
	    (else
	     (js-string->jsstring
		(ucs2-string->utf8-string
		   (ucs2-string
		      (integer->ucs2 (utf8-codeunit-ref val fxpos)))))))))
   
   (js-bind! %this obj 'charAt
      :value (js-make-function %this charat 1 'charAt)
      :enumerable #f)
   
   ;; charCodeAt
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.5
   (define (charcodeat this index)
      (let* ((val (js-tostring (js-cast-string %this this) %this))
	     (pos (js-tointeger index %this)))
	 (if (or (< pos 0) (>= pos (utf8-codeunit-length val)))
	     +nan.0
	     (utf8-codeunit-ref val (->fixnum pos)))))
   
   (js-bind! %this obj 'charCodeAt
      :value (js-make-function %this charcodeat 1 'charCodeAt)
      :enumerable #f)
   
   ;; concat
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.6
   (define (concat::JsStringLiteral this . rest)
      (let ((left (js-tojsstring (js-cast-string %this this) %this)))
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
      :enumerable #f)
   
   ;; indexOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.7
   (define (indexof this::obj search position)
      (let* ((s (js-tostring (js-cast-string %this this) %this))
	     (searchstr (js-tostring search %this))
	     (pos (if (eq? position (js-undefined))
		      0
		      (js-tointeger position %this)))
	     (len (string-length s))
	     (ulen (utf8-string-length s))
	     (start (inexact->exact (min (max pos 0) ulen))))
	 (let ((i (utf8-string-index->string-index s start)))
	    (if (<fx i 0)
		i
		(let loop ((i i)
			   (u start))
		   (cond
		      ((=fx i len)
		       -1)
		      ((substring-at? s searchstr i)
		       u)
		      (else
		       (let ((c (string-ref s i)))
			  (loop (+fx i (utf8-char-size c)) (+fx u 1))))))))))
   
   (js-bind! %this obj 'indexOf
      :value (js-make-function %this indexof 1 'indexOf)
      :enumerable #f)
   
   ;; lastIndexOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.8
   (define (last-indexof this search position)
      (let* ((s (js-tostring (js-cast-string %this this) %this))
	     (searchstr (js-tostring search %this))
	     (searchlen (string-length searchstr))
	     (usearchlen (utf8-string-length searchstr))
	     (len (string-length s))
	     (ulen (utf8-string-length s))
	     (numpos (js-tonumber position %this))
	     (pos (if (and (flonum? numpos) (nanfl? numpos))
		      (+ ulen 1)
		      (js-tointeger numpos %this)))
	     (start (inexact->exact (min (max pos 0) ulen))))
	 ;; utf-8 imposes a left-to-right parsing
	 (let loop ((i 0)
		    (u 0)
		    (r -1))
	    (cond
	       ((or (=fx i len) (>fx u start))
		r)
	       ((substring-at? s searchstr i)
		(loop (+fx searchlen i) (+fx u usearchlen) u))
	       (else
		(let ((c (string-ref s i)))
		   (loop (+fx i (utf8-char-size c)) (+fx u 1) r)))))))
   
   (js-bind! %this obj 'lastIndexOf
      :value (js-make-function %this last-indexof 1 'lastIndexOf)
      :enumerable #f)
   
   ;; localeCompare
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.9
   (define (locale-compare this::obj that)
      (let ((s (js-tostring (js-cast-string %this this) %this))
	    (t (js-tostring that %this)))
	 (utf8-string-locale-compare3 s t)))
   
   (js-bind! %this obj 'localeCompare
      :value (js-make-function %this locale-compare 1 'localeCompare)
      :enumerable #f)
   
   ;; naturalCompare
   ;; hopscript extension
   (define (natural-compare this::obj that)
      (let ((s (js-tostring (js-cast-string %this this) %this))
	    (t (js-tostring that %this)))
	 (string-natural-compare3 s t)))
   
   (js-bind! %this obj 'naturalCompare
      :value (js-make-function %this natural-compare 1 'naturalCompare)
      :enumerable #f)
   
   ;; match
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.10
   (define (match this::obj regexp)
      (with-access::JsGlobalObject %this (js-regexp js-array)
	 (let* ((s (js-tojsstring (js-cast-string %this this) %this))
		(rx (if (isa? regexp JsRegExp)
			regexp
			(js-new %this js-regexp regexp)))
		(exec (js-get (js-get js-regexp 'prototype %this)
			 'exec %this))
		(global (js-get rx 'global %this)))
	    ;; 7
	    (if (not global)
		(js-call1 %this exec rx s)
		;; 8
		(let ((previousLastIndex 0)
		      (a (js-null)))
		   (js-put! rx 'lastIndex 0  #f %this)
		   (let loop ((n 0))
		      (let ((result (js-call1 %this exec rx s)))
			 (if (eq? result (js-null))
			     a
			     (let ((thisIndex (js-get rx 'lastIndex %this)))
				(if (= thisIndex previousLastIndex)
				    (begin
				       (js-put! rx 'lastIndex
					  (+ thisIndex 1) #f %this)
				       (set! previousLastIndex (+ 1 thisIndex)))
				    (set! previousLastIndex thisIndex))
				(when (eq? a (js-null))
				   (set! a (js-new %this js-array 1)))
				(let ((matchStr (js-get result 0 %this)))
				   (js-define-own-property a n
				      (instantiate::JsValueDescriptor
					 (name (js-toname n %this))
					 (value matchStr)
					 (writable #t)
					 (enumerable #t)
					 (configurable #t))
				      #f %this))
				(loop (+fx 1 n)))))))))))
   
   (js-bind! %this obj 'match
      :value (js-make-function %this match 1 'match)
      :enumerable #f)
   
   ;; replace
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11
   (define (replace this::obj searchvalue replacevalue)
      
      (define (digit->number c)
	 (-fx (char->integer c) (char->integer #\0)))
      
      (define (digit10->number c1 c2)
	 (+fx (*fx (digit->number c1) 10) (digit->number c2)))
      
      (define (table22::pair-nil fmt::bstring match string::bstring init::bstring)
	 (let ((stop (-fx (string-length fmt) 1)))
	    (let loop ((i 0)
		       (j 0)
		       (segments '()))
	       (cond
		  ((>=fx i stop)
		   (reverse (cons* init (substring fmt j (+fx stop 1)) segments)))
		  ((not (char=? (string-ref fmt i) #\$))
		   (loop (+fx i 1) j segments))
		  (else
		   (let ((segments (cons (substring fmt j i) segments)))
		      (case (string-ref fmt (+fx i 1))
			 ((#\$)
			  (loop (+fx i 2) (+fx i 2)
			     (cons "$" segments)))
			 ((#\&)
			  (let ((seg (js-get match (js-toname 0 %this) %this)))
			     (loop (+fx i 2) (+fx i 2)
				(cons (js-jsstring->string seg) segments))))
			 ((#\`)
			  (let* ((k (js-get match 'index %this))
				 (portion (substring string 0 k)))
			     (loop (+fx i 2) (+fx i 2)
				(cons portion segments))))
			 ((#\')
			  (let* ((k (js-get match 'index %this))
				 (s (js-get match (js-toname 0 %this) %this))
				 (l (js-jsstring-length s))
				 (portion (substring string (+fx k l))))
			     (loop (+fx i 2) (+fx i 2)
				(cons portion segments))))
			 ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8\ #\9)
			  (let ((len (-fx (js-get match 'length %this) 1)))
			     (if (or (=fx i (-fx stop 1))
				     (not (char-numeric? (string-ref fmt (+fx i 2)))))
				 (let ((n (digit->number (string-ref fmt (+fx i 1)))))
				    (if (>fx n len)
					(loop (+fx i 2) j segments)
					(let ((s (js-get match n %this)))
					   (loop (+fx i 2) (+fx i 2)
					      (if (js-jsstring? s)
						  (cons (js-jsstring->string s)
						     segments)
						  segments)))))
				 (let ((n (digit10->number
					     (string-ref fmt (+fx i 1))
					     (string-ref fmt (+fx i 2)))))
				    (if (>fx n len)
					(let ((n (digit->number (string-ref fmt (+fx i 1)))))
					   (if (>=fx n len)
					       (loop (+fx i 3) j segments)
					       (let ((s (js-get match n %this)))
						  (loop (+fx i 2) (+fx i 2)
						     (if (js-jsstring? s)
							 (cons (js-jsstring->string s)
							    segments)
							 segments)))))
					(let ((s (js-get match n %this)))
					   (loop (+fx i 3) (+fx i 3)
					      (if (js-jsstring? s)
						  (cons (js-jsstring->string s)
						     segments)
						  segments))))))))
			 (else
			  (loop (+fx i 2) j segments)))))))))
      
      (define (matches->string-list::pair-nil a)
	 (let ((len (js-get a 'length %this)))
	    (let loop ((i 1)
		       (l '()))
	       (if (=fx i len)
		   (reverse! l)
		   (let ((v (js-get a (js-toname i %this) %this)))
		      (loop (+fx i 1)
			 (cons (if (eq? v (js-undefined)) (js-string->jsstring "") v)
			    l)))))))
      
      (with-access::JsGlobalObject %this (js-regexp js-array)
	 (let ((string (js-tojsstring (js-cast-string %this this) %this)))
	    (cond
	       ((not (isa? searchvalue JsRegExp))
		(let* ((searchstr (js-tojsstring searchvalue %this))
		       (i (string-contains (js-jsstring->string string)
			     (js-jsstring->string searchstr) 0)))
		   (cond
		      ((not i)
		       string)
		      ((isa? replacevalue JsFunction)
		       (let ((str (js-jsstring->string string)))
			  (js-stringlist->jsstring
			     (list
				(substring str 0 i)
				(js-tostring
				   (js-call3 %this replacevalue (js-undefined)
				      searchstr i string) %this)
				(substring str
				   (+fx i (js-jsstring-length searchstr)))))))
		      (else
		       (let ((newstring (js-tostring replacevalue %this))
			     (a (js-new %this js-array 1))
			     (str (js-jsstring->string string)))
			  (js-put! a 'input string #f %this)
			  (js-put! a (js-toname 0 %this) searchstr #f %this)
			  (js-stringlist->jsstring
			     (cons (substring str 0 i)
				(table22 newstring a str
				   (substring str (+fx i (js-jsstring-length searchstr)))))))))))
	       ((not (js-get searchvalue 'global %this))
		(let* ((exec (js-get (js-get js-regexp 'prototype %this)
				'exec %this))
		       (res (js-call1 %this exec searchvalue string)))
		   (cond
		      ((eq? res (js-null))
		       string)
		      ((isa? replacevalue JsFunction)
		       (let ((i (js-get res 'index %this))
			     (str (js-jsstring->string string)))
			  (js-stringlist->jsstring
			     (list
				(substring str 0 i)
				(js-tostring
				   (js-apply %this replacevalue (js-undefined)
				      (cons (js-get res (js-toname 0 %this) %this)
					 (append (matches->string-list res)
					    (list i str))))
				   %this)
				(substring str
				   (+fx i (js-jsstring-length (js-get res (js-toname 0 %this) %this))))))))
		      (else
		       (let ((newstring (js-tostring replacevalue %this))
			     (i (js-get res 'index %this))
			     (str (js-jsstring->string string)))
			  (js-stringlist->jsstring 
			     (cons (substring str 0 i)
				(table22 newstring res str
				   (substring str
				      (+fx i (js-jsstring-length (js-get res (js-toname 0 %this) %this))))))))))))
	       (else
		(let* ((previousLastIndex 0)
		       (exec (js-get (js-get js-regexp 'prototype %this)
				'exec %this)))
		   (js-put! searchvalue 'lastIndex 0 #f %this)
		   (let loop ((n 0)
			      (ms '()))
		      (let ((result (js-call1 %this exec searchvalue string)))
			 (if (eq? result (js-null))
			     (cond
				((null? ms)
				 string)
				((isa? replacevalue JsFunction)
				 (let loop ((matches (reverse! ms))
					    (res string)
					    (offset 0))
				    (if (null? matches)
					res
					(let* ((m (car matches))
					       (i (js-get m 'index %this))
					       (l (js-jsstring-length (js-get m (js-toname 0 %this) %this)))
					       (v (js-tostring
						     (js-apply %this replacevalue
							(js-undefined)
							(cons
							   (js-get m (js-toname 0 %this) %this)
							   (append (matches->string-list m)
							      (list i string))))
						     %this)))
					   (let ((str (js-jsstring->string res)))
					      (loop (cdr matches)
						 (js-stringlist->jsstring
						    (list (substring str 0 (+fx offset i))
						       v
						       (substring str (+fx offset (+fx i l)))))
						 (+fx offset (-fx (string-length v) l))))))))
				(else
				 (let ((newstring (js-tostring replacevalue %this))
				       (str (js-jsstring->string string)))
				    (let loop ((matches (reverse! ms))
					       (res string)
					       (offset 0))
				       (if (null? matches)
					   res
					   (let* ((m (car matches))
						  (i (js-get m 'index %this))
						  (l (js-jsstring-length (js-get m (js-toname 0 %this) %this)))
						  (sres (js-jsstring->string res))
						  (v (apply utf8-string-append* (table22 newstring m str ""))))
					      (loop (cdr matches)
						 (js-stringlist->jsstring
						    (list (substring sres 0 (+fx offset i))
						       v
						       (substring sres (+fx offset (+fx i l)))))
						 (+fx offset (-fx (string-length v) l)))))))))
			     (let ((thisIndex (js-get searchvalue 'lastIndex %this)))
				(if (= thisIndex previousLastIndex)
				    (begin
				       (js-put! searchvalue 'lastIndex (+ thisIndex 1) #f %this)
				       (set! previousLastIndex (+ 1 thisIndex)))
				    (set! previousLastIndex thisIndex))
				(let ((matchStr (js-get result 0 %this)))
				   (loop (+fx 1 n) (cons result ms)))))))))))))
   
   (js-bind! %this obj 'replace
      :value (js-make-function %this replace 2 'replace)
      :enumerable #f)
   
   ;; search
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.12
   (define (search this::obj regexp)
      (with-access::JsGlobalObject %this (js-regexp)
	 (let ((string (js-tostring (js-cast-string %this this) %this))
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
      :enumerable #f)

   ;; slice
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.13
   (define (slice this::obj start end)
      (let* ((jss (js-tojsstring (js-cast-string %this this) %this))
	     (s (js-jsstring->string jss))
	     (len (utf8-string-length s))
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
	     (js-string->jsstring (utf8-substring s from end))
	     jss)))
   
   (js-bind! %this obj 'slice
      :value (js-make-function %this slice 2 'slice)
      :enumerable #f)
   
   ;; split
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.14
   (define (split this::obj separator limit)
      
      (define (minelong2::elong n1::elong n2::elong)
	 (if (<elong n1 n2) n1 n2))
      
      (define (split-match S::bstring q R)
	 (if (isa? R JsRegExp)
	     (with-access::JsRegExp R (rx)
		(or (pregexp-match-positions rx S q) 'failure))
	     (let ((r (string-length R))
		   (s (string-length S)))
		(cond
		   ((>fx (+fx q r) s)
		    'failure)
		   ((substring-at? S R q)
		    (list (cons q (+fx q r))))
		   (else
		    'failure)))))
      
      (with-access::JsGlobalObject %this (js-array)
	 (let* ((jsS (js-tojsstring (js-cast-string %this this) %this))
		(S (js-jsstring->string jsS))
		(A (js-new %this js-array 0))
		(lim (if (eq? limit (js-undefined))
			 (+fx (string-length S) 1)
			 (elong->fixnum
			    (minelong2
			       (uint32->elong (js-touint32 limit %this))
			       (fixnum->elong (+fx 1 (string-length S)))))))
		(s (string-length S))
		(p 0)
		(R (if (isa? separator JsRegExp)
		       separator
		       (js-tostring separator %this))))
	    (cond
	       ((=fx lim 0)
		;; 9
		A)
	       ((eq? separator (js-undefined))
		;; 10
		(js-define-own-property A 0
		   (instantiate::JsValueDescriptor
		      (name (js-toname 0 %this))
		      (value jsS)
		      (writable #t)
		      (enumerable #t)
		      (configurable #t))
		   #f %this)
		A)
	       ((=fx s 0)
		;; 11
		(let ((z (split-match S 0 R)))
		   (when (eq? z 'failure)
		      (js-define-own-property A 0
			 (instantiate::JsValueDescriptor
			    (name (js-toname 0 %this))
			    (value jsS)
			    (writable #t)
			    (enumerable #t)
			    (configurable #t))
			 #f %this))
		   A))
	       (else
		;; 13
		(let loop ((q p)
			   (p p))
		   (if (not (=fx q s))
		       (let ((z (split-match S q R)))
			  (if (eq? z 'failure)
			      (loop (+fx q (utf8-char-size (string-ref S q))) p)
			      ;; 13.c.i
			      (let ((e (cdar z))
				    (q (caar z))
				    (cap (cdr z)))
				 (if (=fx e p)
				     ;; 13.c.ii
				     (loop (+fx q (utf8-char-size (string-ref S q))) p)
				     ;; 13.c.iii.1
				     (let ((T (substring S p q))
					   (l (->fixnum (js-get A 'length %this))))
					;; 13.c.iii.2
					(js-define-own-property A l
					   (instantiate::JsValueDescriptor
					      (name (js-toname l %this))
					      (value (js-string->jsstring T))
					      (writable #t)
					      (enumerable #t)
					      (configurable #t))
					   #f %this)
					(if (=fx (+fx l 1) lim)
					    ;; 13.c.iii.4
					    A
					    ;; 13.c.iii.5
					    (let ((p e))
					       (let repeat ((cap cap)
							    (l (+fx l 1)))
						  (if (pair? cap)
						      (begin
							 ;; 13.c.iii.7.b
							 (js-define-own-property A l
							    (instantiate::JsValueDescriptor
							       (name (js-toname l %this))
							       (value (car cap))
							       (writable #t)
							       (enumerable #t)
							       (configurable #t))
							    #f %this)
							 (if (=fx (+fx l 1) lim)
							     ;; 13.c.iii.7.d
							     A
							     ;; 13.c.iii.8
							     (repeat (cdr cap) (+fx l 1))))
						      (loop p e))))))))))
		       ;; 14
		       (let ((T (substring S p s))
			     (l (js-get A 'length %this)))
			  ;; 15
			  (js-define-own-property A l
			     (instantiate::JsValueDescriptor
				(name (js-toname l %this))
				(value (js-string->jsstring T))
				(writable #t)
				(enumerable #t)
				(configurable #t))
			     #f %this)
			  ;;16
			  A))))))))
   (js-bind! %this obj 'split
      :value (js-make-function %this split 2 'split)
      :enumerable #f)
   
   ;; substring
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.15
   (define (js-substring::JsStringLiteral this::obj start end)
      (let* ((s (js-tostring (js-cast-string %this this) %this))
	     (len (utf8-string-length s))
	     (intstart (js-tointeger start %this))
	     (intend (if (eq? end (js-undefined)) len (js-tointeger end %this)))
	     (finalstart (->fixnum (min (max intstart 0) len)))
	     (finalend (->fixnum (min (max intend 0) len)))
	     (from (minfx finalstart finalend))
	     (to (maxfx finalstart finalend)))
	 (js-string->jsstring (utf8-substring s from to))))
   
   (js-bind! %this obj 'substring
      :value (js-make-function %this js-substring 2 'substring)
      :enumerable #f)
   
   ;; toLowerCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.16
   (define (tolowercase::JsStringLiteral this::obj)
      (let ((s (js-tostring (js-cast-string %this this) %this)))
	 (js-string->jsstring
	    (ucs2-string->utf8-string (ucs2-string-downcase (utf8-string->ucs2-string s))))))
   
   (js-bind! %this obj 'toLowerCase
      :value (js-make-function %this tolowercase 0 'toLowerCase)
      :enumerable #f)
   
   ;; toLocaleLowerCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.17
   (define (tolocalelowercase::JsStringLiteral this::obj)
      (let ((s (js-tostring (js-cast-string %this this) %this)))
	 (js-string->jsstring
	    (utf8-string-locale-downcase s))))
   (js-bind! %this obj 'toLocaleLowerCase
      :value (js-make-function %this tolocalelowercase 0 'toLocaleLowerCase)
      :enumerable #f)
   
   ;; toUpperCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.18
   (define (touppercase::JsStringLiteral this::obj)
      (let ((s (js-tostring (js-cast-string %this this) %this)))
	 (js-string->jsstring
	    (ucs2-string->utf8-string (ucs2-string-upcase (utf8-string->ucs2-string s))))))
   
   (js-bind! %this obj 'toUpperCase
      :value (js-make-function %this touppercase 0 'toUpperCase)
      :enumerable #f)
   
   ;; toLocaleUpperCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.19
   (define (tolocaleuppercase::JsStringLiteral this::obj)
      (let ((s (js-tostring (js-cast-string %this this) %this)))
	 (js-string->jsstring
	    (utf8-string-locale-upcase s))))
   
   (js-bind! %this obj 'toLocaleUpperCase
      :value (js-make-function %this tolocaleuppercase 0 'toLocaleUpperCase)
      :enumerable #f)
   
   ;; trim
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.20
   (define (trim::JsStringLiteral this::obj)
      (js-string->jsstring
	 (trim-whitespaces+ (js-tostring (js-cast-string %this this) %this)
	    :left #t :right #t)))
   (js-bind! %this obj 'trim
      :value (js-make-function %this trim 0 'trim)
      :enumerable #f)
   
   ;; substr
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.3
   (define (substr::JsStringLiteral this::obj start length)
      (let* ((r1 (js-tostring this %this))
	     (r2 (js-tointeger start %this))
	     (r3 (if (eq? length (js-undefined))
		     (maxvalfx)
		     (js-tointeger length %this)))
	     (r4 (utf8-string-length r1))
	     (r5 (if (>=fx r2 0) r2 (maxfx (+fx r4 r2) 0)))
	     (r6 (minfx (maxfx r3 0) (-fx r4 r5))))
	 (if (<=fx r6 0)
	     (js-string->jsstring "")
	     (js-string->jsstring (utf8-substring r1 r5 (+fx r5 r6))))))
   
   (js-bind! %this obj 'substr
      :value (js-make-function %this substr 2 'substr)
      :enumerable #f))

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
	       (iota (utf8-string-length (js-jsstring->string val)))))
	 (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsString ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.5.2     */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsString p %this)
   (let ((index (js-toindex p)))
      (if (js-isindex? index)
	  (with-access::JsString o (val)
	     (let* ((val (js-jsstring->string val))
		    (len (utf8-string-length val))
		    (index (uint32->fixnum index)))
		(if (<=fx len index)
		    (call-next-method)
		    #t)))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::JsString ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.5.2     */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsString p %this)
   (let ((index (js-toindex p)))
      (if (js-isindex? index)
	  (with-access::JsString o (val)
	     (let* ((val (js-jsstring->string val))
		    (len (utf8-string-length val))
		    (index (uint32->fixnum index)))
		(if (<=fx len index)
		    (call-next-method)
		    (instantiate::JsValueDescriptor
		       (name (js-toname p %this))
		       (value (js-string->jsstring (utf8-string-ref val index)))
		       (enumerable #t)
		       (writable #f)
		       (configurable #f)))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get-property-value ::JsString ...                             */
;*    -------------------------------------------------------------    */
;*    This method is optional. It could be removed without changing    */
;*    the programs behaviors. It merely optimizes access to strings.   */
;*---------------------------------------------------------------------*/
(define-method (js-get-property-value o::JsString base p::obj %this::JsGlobalObject)
   (let ((index (js-toindex p)))
      (if (js-isindex? index)
	  (with-access::JsString o (val)
	     (let* ((val (js-jsstring->string val))
		    (len (utf8-string-length val))
		    (index (uint32->fixnum index)))
		(if (<=fx len index)
		    (call-next-method)
		    (js-string->jsstring (utf8-string-ref val index)))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsString ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsString p %this)
   (with-access::JsString o (val)
      (let ((i (js-toindex p)))
	 (if (not (js-isindex? i))
	     (call-next-method)
	     (let* ((val (js-jsstring->string val))
		    (len (utf8-string-length val))
		    (index (uint32->fixnum i)))
		(if (<=fx len index)
		    (call-next-method)
		    (js-string->jsstring (utf8-string-ref val index))))))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsString ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::JsString proc %this)
   (with-access::JsString o (val)
      (let ((len (utf8-string-length (js-jsstring->string val))))
	 (if (>fx len 0)
	     (let loop ((i 0))
		(if (<fx i len)
		    (begin
		       (proc (js-integer->jsstring i))
		       (loop (+fx i 1)))
		    (call-next-method)))
	     (call-next-method)))))
  
;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
