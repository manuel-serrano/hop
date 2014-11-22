;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/string.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Sat Nov 22 09:24:45 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
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
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_error
	   __hopscript_worker)

   (export (js-init-string! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsString ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsString
   (lambda (o)
      (call-with-output-string
	 (lambda (op)
	    (obj->javascript-expr o op))))
   (lambda (s)
      (call-with-input-string s
	 javascript->jsobj)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-string-literal-begin!)

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsString ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsString op compile isexpr)
   (with-access::JsString o (val)
      (display "new String( \"" op)
      (display (string-for-read (js-string->string val)) op)
      (display "\"" op)))

;*---------------------------------------------------------------------*/
;*    js-init-string! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-string! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (__proto__ js-string js-function)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; builtin prototype
	 (define js-string-prototype
	    (instantiate::JsString
	       (val (string->js-string ""))
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
			     (value (utf8-codepoint-length v))))
		     (str (string-ascii-sentinel-mark! v)))
		  (with-access::JsString o (val properties)
		     (set! val (string->js-string str))
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
	       (val (string->js-string ""))
	       (__proto__ (js-get constructor 'prototype %this))))

	 ;; then, create a HopScript object
	 (set! js-string
	    (js-make-function %this
	       (%js-string %this) 1 'JsString
	       :__proto__ js-function-prototype
	       :prototype js-string-prototype
	       :construct js-string-construct
	       :alloc js-string-alloc))
	 
	 ;; fromCharCode
	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.3.2
	 (define (js-string-fromcharcode this . l)
	    (string->js-string
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
	 
	 ;; prototype properties
	 (init-builtin-string-prototype! %this js-string js-string-prototype)
	 ;; bind String in the global object
	 (js-bind! %this %this 'String
	    :configurable #f :enumerable #f :value js-string)
	 js-string)))

;*---------------------------------------------------------------------*/
;*    js-cast-string ...                                               */
;*---------------------------------------------------------------------*/
(define (js-cast-string %this obj)
   (if (js-string? obj) obj (js-toobject %this obj)))

;*---------------------------------------------------------------------*/
;*    %js-string ...                                                   */
;*---------------------------------------------------------------------*/
(define (%js-string %this)
   (lambda::JsStringLiteral (this . args)
      (if (null? args)
	  (string->js-string "")
	  (string->js-string (js-tostring (car args) %this)))))

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
	     (pos (js-tointeger index %this)))
	 (if (or (< pos 0) (>= pos (utf8-string-length val)))
	     (string->js-string "")
	     (string->js-string (utf8-string-ref val (->fixnum pos))))))
   
   (js-bind! %this obj 'charAt
      :value (js-make-function %this charat 1 'charAt)
      :enumerable #f)
   
   ;; charCodeAt
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.5
   (define (charcodeat this index)
      (let* ((val (js-tostring (js-cast-string %this this) %this))
	     (pos (js-tointeger index %this)))
	 (if (or (< pos 0) (>= pos (utf8-string-length val)))
	     +nan.0
	     (let* ((utf8 (utf8-string-ref val pos))
		    (ucs2 (utf8-string->ucs2-string utf8))
		    (uc (ucs2-string-ref-ur ucs2 0)))
		(ucs2->integer uc)))))
   
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
	     (js-string-append left (js-tojsstring right %this)))
	    (else
	     (js-string-appendN left rest %this)))))
   
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
		       (segments (list init)))
	       (cond
		  ((>=fx i stop)
		   (let ((strs (cons (substring fmt j (+fx stop 1)) segments)))
		      (reverse! strs)))
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
				(cons seg segments))))
			 ((#\`)
			  (let* ((k (js-get match 'index %this))
				 (portion (substring string 0 k)))
			     (loop (+fx i 2) (+fx i 2)
				(cons portion segments))))
			 ((#\')
			  (let* ((k (js-get match 'index %this))
				 (s (js-get match (js-toname 0 %this) %this))
				 (l (js-string-length s))
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
					      (if (js-string? s)
						  (cons (js-string->string s)
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
						     (if (js-string? s)
							 (cons (js-string->string s)
							    segments)
							 segments)))))
					(let ((s (js-get match n %this)))
					   (loop (+fx i 3) (+fx i 3)
					      (if (js-string? s)
						  (cons (js-string->string s)
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
			 (cons (if (js-string? v) v (string->js-string ""))
			    l)))))))
      
      (with-access::JsGlobalObject %this (js-regexp js-array)
	 (let ((string (js-tojsstring (js-cast-string %this this) %this)))
	    (cond
	       ((not (isa? searchvalue JsRegExp))
		(let* ((searchstr (js-tostring searchvalue %this))
		       (i (string-contains (js-string->string string)
			     searchstr 0)))
		   (cond
		      ((not i)
		       string)
		      ((isa? replacevalue JsFunction)
		       (let ((str (js-string->string string)))
			  (string-list->js-string
			     (list
				(substring str 0 i)
				(js-tostring
				   (js-call3 %this replacevalue (js-undefined)
				      searchstr i string) %this)
				(substring str
				   (+fx i (string-length searchstr)))))))
		      (else
		       (let ((newstring (js-tostring replacevalue %this))
			     (a (js-new %this js-array 1))
			     (str (js-string->string string)))
			  (js-put! a 'input string #f %this)
			  (js-put! a (js-toname 0 %this) searchstr #f %this)
			  (string-list->js-string
			     (cons (substring str 0 i)
				(table22 newstring a str
				   (substring str (+fx i (string-length searchstr)))))))))))
	       ((not (js-get searchvalue 'global %this))
		(let* ((exec (js-get (js-get js-regexp 'prototype %this)
				'exec %this))
		       (res (js-call1 %this exec searchvalue string)))
		   (cond
		      ((eq? res (js-null))
		       string)
		      ((isa? replacevalue JsFunction)
		       (let ((i (js-get res 'index %this))
			     (str (js-string->string string)))
			  (string-list->js-string
			     (list
				(substring str 0 i)
				(js-tostring
				   (js-apply %this replacevalue (js-undefined)
				      (cons (js-get res (js-toname 0 %this) %this)
					 (append (matches->string-list res)
					    (list i str))))
				   %this)
				(substring str
				   (+fx i (js-string-length (js-get res (js-toname 0 %this) %this))))))))
		      (else
		       (let ((newstring (js-tostring replacevalue %this))
			     (i (js-get res 'index %this))
			     (str (js-string->string string)))
			  (string-list->js-string 
			     (cons (substring str 0 i)
				(table22 newstring res str
				   (substring str
				      (+fx i (js-string-length (js-get res (js-toname 0 %this) %this))))))))))))
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
					       (l (js-string-length (js-get m (js-toname 0 %this) %this)))
					       (v (js-tostring
						     (js-apply %this replacevalue
							(js-undefined)
							(cons
							   (js-get m (js-toname 0 %this) %this)
							   (append (matches->string-list m)
							      (list i string))))
						     %this)))
					   (let ((str (js-string->string res)))
					      (loop (cdr matches)
						 (string-list->js-string
						    (list (substring str 0 (+fx offset i))
						       v
						       (substring str (+fx offset (+fx i l)))))
						 (+fx offset (-fx (string-length v) l))))))))
				(else
				 (let ((newstring (js-tostring replacevalue %this)))
				    (let loop ((matches (reverse! ms))
					       (res string)
					       (offset 0))
				       (if (null? matches)
					   res
					   (let* ((m (car matches))
						  (i (js-get m 'index %this))
						  (l (js-string-length (js-get m (js-toname 0 %this) %this)))
						  (str (js-string->string string))
						  (sres (js-string->string res))
						  (v (substring sres (+fx offset (+fx i l)))))
					      (loop (cdr matches)
						 (string-list->js-string
						    (list (substring sres 0 (+fx offset i))
						       (table22 newstring m str v)))
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
	     (s (js-string->string jss))
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
	     (string->js-string (utf8-substring s from end))
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
		(S (js-string->string jsS))
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
					      (value (string->js-string T))
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
				(value (string->js-string T))
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
	 (string->js-string
	    (utf8-substring s from to))))
   
   (js-bind! %this obj 'substring
      :value (js-make-function %this js-substring 2 'substring)
      :enumerable #f)
   
   ;; toLowerCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.16
   (define (tolowercase::JsStringLiteral this::obj)
      (let ((s (js-tostring (js-cast-string %this this) %this)))
	 (string->js-string
	    (ucs2-string->utf8-string (ucs2-string-downcase (utf8-string->ucs2-string s))))))
   
   (js-bind! %this obj 'toLowerCase
      :value (js-make-function %this tolowercase 0 'toLowerCase)
      :enumerable #f)
   
   ;; toLocaleLowerCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.17
   (define (tolocalelowercase::JsStringLiteral this::obj)
      (let ((s (js-tostring (js-cast-string %this this) %this)))
	 (string->js-string
	    (utf8-string-locale-downcase s))))
   (js-bind! %this obj 'toLocaleLowerCase
      :value (js-make-function %this tolocalelowercase 0 'toLocaleLowerCase)
      :enumerable #f)
   
   ;; toUpperCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.18
   (define (touppercase::JsStringLiteral this::obj)
      (let ((s (js-tostring (js-cast-string %this this) %this)))
	 (string->js-string
	    (ucs2-string->utf8-string (ucs2-string-upcase (utf8-string->ucs2-string s))))))
   
   (js-bind! %this obj 'toUpperCase
      :value (js-make-function %this touppercase 0 'toUpperCase)
      :enumerable #f)
   
   ;; toLocaleUpperCase
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.19
   (define (tolocaleuppercase::JsStringLiteral this::obj)
      (let ((s (js-tostring (js-cast-string %this this) %this)))
	 (string->js-string
	    (utf8-string-locale-upcase s))))
   
   (js-bind! %this obj 'toLocaleUpperCase
      :value (js-make-function %this tolocaleuppercase 0 'toLocaleUpperCase)
      :enumerable #f)
   
   ;; trim
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.20
   (define (trim::JsStringLiteral this::obj)
      (string->js-string
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
	     (string->js-string "")
	     (string->js-string (utf8-substring r1 r5 (+fx r5 r6))))))
   
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
      (let ((val (js-string->string val)))
	 (cond
	    ((string=? val "Infinity")
	     +inf.0)
	    ((string=? val "NaN")
	     +nan.0)
	    (else
	     (or (string->number val) +nan.0))))))

;*---------------------------------------------------------------------*/
;*    js-tointeger ::JsString ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tointeger this::JsString %this)
   (with-access::JsString this (val)
      (let ((val (js-string->string val)))
	 (cond
	    ((string=? val "Infinity")
	     +inf.0)
	    ((string=? val "NaN")
	     +nan.0)
	    (else
	     (let ((i (string->number val)))
		(if (fixnum? i) i 0)))))))

;*---------------------------------------------------------------------*/
;*    js-properties-name ::JsString ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-properties-name obj::JsString enump %this)
   (with-access::JsString obj (val)
      (vector-append
	 (apply vector
	    (map! integer->string
	       (iota (utf8-string-length (js-string->string val)))))
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
	     (let* ((val (js-string->string val))
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
	     (let* ((val (js-string->string val))
		    (len (utf8-string-length val))
		    (index (uint32->fixnum index)))
		(if (<=fx len index)
		    (call-next-method)
		    (instantiate::JsValueDescriptor
		       (name (js-toname p %this))
		       (value (utf8-string-ref val index))
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
	     (let* ((val (js-string->string val))
		    (len (utf8-string-length val))
		    (index (uint32->fixnum index)))
		(if (<=fx len index)
		    (call-next-method)
		    (string->js-string (utf8-string-ref val index)))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsString ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsString p %this)
   (with-access::JsString o (val)
      (let ((i (js-toindex p)))
	 (if (not (js-isindex? i))
	     (call-next-method)
	     (let* ((val (js-string->string val))
		    (len (utf8-string-length val))
		    (index (uint32->fixnum i)))
		(if (<=fx len index)
		    (call-next-method)
		    (string->js-string (utf8-string-ref val index))))))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsString ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::JsString proc %this)
   (with-access::JsString o (val)
      (let ((len (utf8-string-length (js-string->string val))))
	 (if (>fx len 0)
	     (let loop ((i 0))
		(if (<fx i len)
		    (begin
		       (proc (integer->js-string i))
		       (loop (+fx i 1)))
		    (call-next-method)))
	     (call-next-method)))))
  
;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-string-literal-end!)
