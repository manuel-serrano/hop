;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/string.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Fri Mar 14 10:44:54 2014 (serrano)                */
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

   (library hop)
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_error)

   (export js-string
	   js-string-prototype
	   (js-init-string! ::JsObject)
	   
	   (inline js-string-append ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsString ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsString op compile isexpr)
   (with-access::JsString o (val)
      (display "new String( \"" op)
      (display (string-for-read val) op)
      (display "\"" op)))

;*---------------------------------------------------------------------*/
;*    js-string ...                                                    */
;*---------------------------------------------------------------------*/
(define js-string #f)
(define js-string-prototype #f)

;*---------------------------------------------------------------------*/
;*    js-init-string! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-string! %this)
   ;; first, bind the builtin string prototype
   (set! js-string-prototype
      (instantiate::JsString
	 (val "")
	 (__proto__ js-object-prototype)
	 (extensible #t)))
   ;; then, create a HopScript object
   (let ((obj (js-make-function %js-string 1 "JsString"
		 :prototype js-string-prototype
		 :__proto__ js-function-prototype
		 :alloc js-string-alloc
		 :construct js-string-construct)))
      (set! js-string obj)
      ;; other properties of the String constructor
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.3
      (js-bind! obj 'fromCharCode
	 :value (js-make-function js-string-fromcharcode 1 "fromCharCode")
	 :writable #t
	 :enumerable #f
	 :configurable #t)
      ;; prototype properties
      (init-builtin-string-prototype! js-string-prototype)
      ;; bind String in the global object
      (js-bind! %this 'String :configurable #f :enumerable #f :value js-string)
      js-string))

;*---------------------------------------------------------------------*/
;*    js-cast-string ...                                               */
;*---------------------------------------------------------------------*/
(define (js-cast-string obj)
   (if (string? obj) obj (js-toobject obj)))

;*---------------------------------------------------------------------*/
;*    %js-string ...                                                   */
;*---------------------------------------------------------------------*/
(define (%js-string this . args)
   (if (null? args)
       ""
       (js-tostring (car args))))

;*---------------------------------------------------------------------*/
;*    js-string-alloc ...                                              */
;*---------------------------------------------------------------------*/
(define (js-string-alloc::JsString constructor::JsFunction)
   (instantiate::JsString
      (__proto__ (js-get constructor 'prototype))))

;*---------------------------------------------------------------------*/
;*    js-string-construct ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5         */
;*---------------------------------------------------------------------*/
(define (js-string-construct o::JsString . arg)

   (define (set-string! v)
      (let ((len (instantiate::JsValueDescriptor
		    (name 'length)
		    (writable #f)
		    (configurable #f)
		    (enumerable #f)
		    (value (utf8-string-length v)))))
	 (with-access::JsString o (val properties)
	    (set! val v)
	    (set! properties (list len)))))
   
   (if (null? arg)
       ;; 2
       (set-string! "")
       (let ((value (car arg)))
	  (if (string? value)
	      (set-string! value)
	      (set-string! (js-tostring value))))))

;*---------------------------------------------------------------------*/
;*    js-valueof ::JsString ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-valueof this::JsString)
   (with-access::JsString this (val)
      val))
   
;*---------------------------------------------------------------------*/
;*    init-builtin-string-prototype! ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.3.1     */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4       */
;*---------------------------------------------------------------------*/
(define (init-builtin-string-prototype! obj)
   ;; prototype fields
   (js-bind! obj 'length
      :value 0
      :enumerable #f)
   (js-bind! obj 'constructor
      :value js-string
      :enumerable #f)
   ;; toString
   (js-bind! obj 'toString
      :value (js-make-function string-prototype-tostring 0 "toString")
      :enumerable #f)
   ;; valueOf
   (js-bind! obj 'valueOf
      :value (js-make-function string-prototype-valueof 0 "valueOf")
      :enumerable #f)
   ;; charAt
   (js-bind! obj 'charAt
      :value (js-make-function string-prototype-charat 1 "charAt")
      :enumerable #f)
   ;; charCodeAt
   (js-bind! obj 'charCodeAt
      :value (js-make-function string-prototype-charcodeat 1 "charCodeAt")
      :enumerable #f)
   ;; concat
   (js-bind! obj 'concat
      :value (js-make-function string-prototype-concat 1 "concat")
      :enumerable #f)
   ;; indexOf
   (js-bind! obj 'indexOf
      :value (js-make-function string-prototype-indexof 1 "indexOf")
      :enumerable #f)
   ;; indexOf
   (js-bind! obj 'lastIndexOf
      :value (js-make-function string-prototype-last-indexof 1 "lastIndexOf")
      :enumerable #f)
   ;; localeCompare
   (js-bind! obj 'localeCompare
      :value (js-make-function string-prototype-locale-compare 1 "localeCompare")
      :enumerable #f)
   ;; match
   (js-bind! obj 'match
      :value (js-make-function string-prototype-match 1 "match")
      :enumerable #f)
   ;; replace
   (js-bind! obj 'replace
      :value (js-make-function string-prototype-replace 2 "replace")
      :enumerable #f)
   ;; search
   (js-bind! obj 'search
      :value (js-make-function string-prototype-search 1 "search")
      :enumerable #f)
   ;; slice
   (js-bind! obj 'slice
      :value (js-make-function string-prototype-slice 2 "slice")
      :enumerable #f)
   ;; split
   (js-bind! obj 'split
      :value (js-make-function string-prototype-split 2 "split")
      :enumerable #f)
   ;; substring
   (js-bind! obj 'substring
      :value (js-make-function string-prototype-substring 2 "substring")
      :enumerable #f)
   ;; toLowerCase
   (js-bind! obj 'toLowerCase
      :value (js-make-function string-prototype-tolowercase 0 "toLowerCase")
      :enumerable #f)
   ;; toLocaleLowerCase
   (js-bind! obj 'toLocaleLowerCase
      :value (js-make-function string-prototype-tolocalelowercase 0 "toLocaleLowerCase")
      :enumerable #f)
   ;; toUpperCase
   (js-bind! obj 'toUpperCase
      :value (js-make-function string-prototype-touppercase 0 "toUpperCase")
      :enumerable #f)
   ;; toLocaleUpperCase
   (js-bind! obj 'toLocaleUpperCase
      :value (js-make-function string-prototype-tolocaleuppercase 0 "toLocaleUpperCase")
      :enumerable #f)
   ;; trim
   (js-bind! obj 'trim
      :value (js-make-function string-prototype-trim 0 "trim")
      :enumerable #f)
   ;; substr
   (js-bind! obj 'substr
      :value (js-make-function string-prototype-substr 2 "substr")
      :enumerable #f))

;*---------------------------------------------------------------------*/
;*    js-tonumber ::JsString ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tonumber this::JsString)
   (with-access::JsString this (val)
      (cond
	 ((string=? val "Infinity")
	  +inf.0)
	 ((string=? val "NaN")
	  +nan.0)
	 (else
	  (or (string->number val) +nan.0)))))

;*---------------------------------------------------------------------*/
;*    js-tointeger ::JsString ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tointeger this::JsString)
   (with-access::JsString this (val)
      (cond
	 ((string=? val "Infinity")
	  +inf.0)
	 ((string=? val "NaN")
	  +nan.0)
	 (else
	  (let ((i (string->number val)))
	     (if (fixnum? i) i 0))))))

;*---------------------------------------------------------------------*/
;*    js-property-names ::JsString ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-property-names obj::JsString enump)
   (with-access::JsString obj (val)
      (vector-append
	 (apply vector (map! integer->string (iota (utf8-string-length val))))
	 (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::JsString ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.5.2     */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsString p)
   (let ((desc (call-next-method)))
      (if (eq? desc (js-undefined))
	  (let ((index (js-toindex p)))
	     (if (js-isindex? index)
		 (with-access::JsString o (val)
		    (let ((len (utf8-string-length val))
			  (index (uint32->fixnum index)))
		       (if (<=fx len index)
			   (js-undefined)
			   (instantiate::JsValueDescriptor
			      (name (js-toname p))
			      (value (utf8-string-ref val index))
			      (enumerable #t)
			      (writable #f)
			      (configurable #f)))))
		 (js-undefined)))
	  desc)))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsString ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.5.2     */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsString p)
   (let ((index (js-toindex p)))
      (if (js-isindex? index)
	  (with-access::JsString o (val)
	     (let ((len (utf8-string-length val))
		   (index (uint32->fixnum index)))
		(if (<=fx len index)
		    (call-next-method)
		    #t)))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get-property ::JsString ...                                   */
;*---------------------------------------------------------------------*/
(define-method (js-get-property o::JsString p)
   (let ((index (js-toindex p)))
      (if (js-isindex? index)
	  (with-access::JsString o (val)
	     (let ((len (utf8-string-length val))
		   (index (uint32->fixnum index)))
		(if (<=fx len index)
		    (call-next-method)
		    (instantiate::JsValueDescriptor
		       (name (js-toname p))
		       (value (utf8-string-ref val index))
		       (enumerable #t)
		       (writable #f)
		       (configurable #f)))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsString ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsString p)
   (with-access::JsString o (val)
      (let ((i (js-toindex p)))
	 (if (not (js-isindex? i))
	     (call-next-method)
	     (let ((len (utf8-string-length val))
		   (index (uint32->fixnum i)))
		(if (<=fx len index)
		    (call-next-method)
		    (utf8-string-ref val index)))))))
       
;*---------------------------------------------------------------------*/
;*    js-get/base ::JsString ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-get/base o::JsString base p)
   (let ((i (js-toindex p)))
      (if (not (js-isindex? i))
	  (call-next-method)
	  (let ((len (utf8-string-length base))
		(index (uint32->fixnum i)))
	     (if (<=fx len index)
		 (call-next-method)
		 (utf8-string-ref base index))))))
       
;*---------------------------------------------------------------------*/
;*    js-string-fromcharcode ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.3.2     */
;*---------------------------------------------------------------------*/
(define (js-string-fromcharcode this . l)
   (ucs2-string->utf8-string
      (apply ucs2-string
	 (map (lambda (c)
		 (integer->ucs2 (uint16->fixnum (js-touint16 c))))
	    l))))

;*---------------------------------------------------------------------*/
;*    string-prototype-tostring ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.2     */
;*---------------------------------------------------------------------*/
(define (string-prototype-tostring this)
   (if (isa? this JsString)
       (with-access::JsString this (val) val)
       (js-raise-type-error "argument not a string ~a" (typeof this))))

;*---------------------------------------------------------------------*/
;*    string-prototype-valueof ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.3     */
;*---------------------------------------------------------------------*/
(define (string-prototype-valueof this)
   (if (isa? this JsString)
       (with-access::JsString this (val) val)
       (js-raise-type-error "argument not a string ~a" this)))

;*---------------------------------------------------------------------*/
;*    string-prototype-charat ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.4     */
;*---------------------------------------------------------------------*/
(define (string-prototype-charat this index)
   (let ((val (js-tostring (js-cast-string this)))
	 (pos (js-tointeger index)))
      (if (or (< pos 0) (>= pos (utf8-string-length val)))
	  ""
	  (utf8-string-ref val (->fixnum pos)))))

;*---------------------------------------------------------------------*/
;*    string-prototype-charcodeat ...                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.5     */
;*---------------------------------------------------------------------*/
(define (string-prototype-charcodeat this index)
   (let* ((val (js-tostring (js-cast-string this)))
	  (pos (js-tointeger index)))
      (if (or (< pos 0) (>= pos (utf8-string-length val)))
	  +nan.0
	  (let* ((utf8 (utf8-string-ref val pos))
		 (ucs2 (utf8-string->ucs2-string utf8))
		 (uc (ucs2-string-ref-ur ucs2 0)))
	     (ucs2->integer uc)))))

;*---------------------------------------------------------------------*/
;*    string-prototype-concat ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.6     */
;*---------------------------------------------------------------------*/
(define (string-prototype-concat this . rest)
   (apply string-append (js-tostring (js-cast-string this))
      (map! js-tostring rest)))

;*---------------------------------------------------------------------*/
;*    string-prototype-indexof ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.7     */
;*---------------------------------------------------------------------*/
(define (string-prototype-indexof this::obj search position)
   (let* ((s (js-tostring (js-cast-string this)))
	  (searchstr (js-tostring search))
	  (pos (if (eq? position (js-undefined))
		   0
		   (js-tointeger position)))
	  (len (utf8-string-length s))
	  (start (inexact->exact (min (max pos 0) len))))
      (or (string-contains s searchstr start) -1)))

;*---------------------------------------------------------------------*/
;*    string-prototype-last-indexof ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.8     */
;*---------------------------------------------------------------------*/
(define (string-prototype-last-indexof this search position)
   (let* ((s (js-tostring (js-cast-string this)))
	  (searchstr (js-tostring search))
	  (len (utf8-string-length s))
	  (numpos (js-tonumber position))
	  (pos (if (and (flonum? numpos) (nanfl? numpos))
		   (+ len 1)
		   (js-tointeger numpos)))
	  (start (inexact->exact (min (max pos 0) len)))
	  (searchlen (utf8-string-length searchstr)))
      (let loop ((i start))
	 (cond
	    ((=fx i -1) -1)
	    ((substring-at? s searchstr i) i)
	    (else (loop (-fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    string-prototype-locale-compare ...                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.9     */
;*---------------------------------------------------------------------*/
(define (string-prototype-locale-compare this::obj that)
   (let ((s (js-tostring (js-cast-string this)))
	 (t (js-tostring that)))
      (utf8-string-locale-compare3 s t)))

;*---------------------------------------------------------------------*/
;*    string-prototype-match ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.10    */
;*---------------------------------------------------------------------*/
(define (string-prototype-match this::obj regexp)
   (let* ((s (js-tostring (js-cast-string this)))
	  (rx (if (isa? regexp JsRegExp)
		  regexp
		  (js-new js-regexp regexp)))
	  (exec (js-get (js-get js-regexp 'prototype) 'exec))
	  (global (js-get rx 'global)))
      ;; 7
      (if (not global)
	  (js-call1 exec rx s)
	  ;; 8
	  (let ((previousLastIndex 0)
		(a (js-null)))
	     (js-put! rx 'lastIndex 0 #f)
	     (let loop ((n 0))
		(let ((result (js-call1 exec rx s)))
		   (if (eq? result (js-null))
		       a
		       (let ((thisIndex (js-get rx 'lastIndex)))
			  (if (= thisIndex previousLastIndex)
			      (begin
				 (js-put! rx 'lastIndex (+ thisIndex 1) #f)
				 (set! previousLastIndex (+ 1 thisIndex)))
			      (set! previousLastIndex thisIndex))
			  (when (eq? a (js-null)) (set! a (js-new js-array 1)))
			  (let ((matchStr (js-get result 0)))
			     (js-define-own-property a n
				(instantiate::JsValueDescriptor
				   (name (js-toname n))
				   (value matchStr)
				   (writable #t)
				   (enumerable #t)
				   (configurable #t))
				#f))
			  (loop (+fx 1 n))))))))))

;*---------------------------------------------------------------------*/
;*    string-prototype-replace ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (string-prototype-replace this::obj searchvalue replacevalue)
   
   (define (digit->number c)
      (-fx (char->integer c) (char->integer #\0)))
   
   (define (digit10->number c1 c2)
      (+fx (*fx (digit->number c1) 10) (digit->number c2)))
   
   (define (table22 fmt match string)
      (let ((stop (-fx (string-length fmt) 1)))
	 (let loop ((i 0)
		    (j 0)
		    (segments '()))
	    (cond
	       ((>=fx i stop)
		(apply string-append
		   (reverse (cons (substring fmt j (+fx stop 1)) segments))))
	       ((not (char=? (string-ref fmt i) #\$))
		(loop (+fx i 1) j segments))
	       (else
		(let ((segments (cons (substring fmt j i) segments)))
		   (case (string-ref fmt (+fx i 1))
		      ((#\$)
		       (loop (+fx i 2) (+fx i 2) (cons "$" segments)))
		      ((#\&)
		       (let ((seg (js-get match (js-toname 0))))
			  (loop (+fx i 2) (+fx i 2) (cons seg segments))))
		      ((#\`)
		       (let* ((k (js-get match 'index))
			      (portion (substring string 0 k)))
			  (loop (+fx i 2) (+fx i 2) (cons portion segments))))
		      ((#\')
		       (let* ((k (js-get match 'index))
			      (s (js-get match (js-toname 0)))
			      (l (string-length s))
			      (portion (substring string (+fx k l))))
			  (loop (+fx i 2) (+fx i 2) (cons portion segments))))
		      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8\ #\9)
		       (let ((len (-fx (js-get match 'length) 1)))
			  (if (or (=fx i (-fx stop 1))
				  (not (char-numeric? (string-ref fmt (+fx i 2)))))
			      (let ((n (digit->number (string-ref fmt (+fx i 1)))))
				 (if (>fx n len)
				     (loop (+fx i 2) j segments)
				     (let ((s (js-get match n)))
					(loop (+fx i 2) (+fx i 2)
					   (if (string? s)
					       (cons s segments)
					       segments)))))
			      (let ((n (digit10->number
					  (string-ref fmt (+fx i 1))
					  (string-ref fmt (+fx i 2)))))
				 (if (>fx n len)
				     (let ((n (digit->number (string-ref fmt (+fx i 1)))))
					(if (>=fx n len)
					    (loop (+fx i 3) j segments)
					    (let ((s (js-get match n)))
					       (loop (+fx i 2) (+fx i 2)
						  (if (string? s)
						      (cons s segments)
						      segments)))))
				     (let ((s (js-get match n)))
					(loop (+fx i 3) (+fx i 3)
					   (if (string? s)
					       (cons s segments)
					       segments))))))))
		      (else
		       (loop (+fx i 2) j segments)))))))))
   
   (define (matches->string-list a)
      (let ((len (js-get a 'length)))
	 (let loop ((i 1)
		    (l '()))
	    (if (=fx i len)
		(reverse! l)
		(let ((v (js-get a (js-toname i))))
		   (loop (+fx i 1)
		      (cons (if (eq? v (js-undefined)) "" v) l)))))))
   
   (let ((string (js-tostring (js-cast-string this))))
      (cond
	 ((not (isa? searchvalue JsRegExp))
	  (let* ((searchstr (js-tostring searchvalue))
		 (i (string-contains string searchstr 0)))
	     (cond
		((not i)
		 string)
		((isa? replacevalue JsFunction)
		 (string-append (substring string 0 i)
		    (js-tostring (js-call3 replacevalue (js-undefined)
				    searchstr i string))
		    (substring string (+fx i (string-length searchstr)))))
		(else
		 (let ((newstring (js-tostring replacevalue))
		       (a (js-new js-array 1)))
		    (js-put! a 'input string #f)
		    (js-put! a (js-toname 0) searchstr #f)
		    (string-append (substring string 0 i)
		       (table22 newstring a string)
		       (substring string (+fx i (string-length searchstr)))))))))
	 ((not (js-get searchvalue 'global))
	  (let* ((exec (js-get (js-get js-regexp 'prototype) 'exec))
		 (res (js-call1 exec searchvalue string)))
	     (cond
		((eq? res (js-null))
		 string)
		((isa? replacevalue JsFunction)
		 (let ((i (js-get res 'index)))
		    (string-append (substring string 0 i)
		       (js-tostring
			  (js-apply replacevalue (js-undefined)
			     (cons (js-get res (js-toname 0))
				(append (matches->string-list res)
				   (list i string)))))
		       (substring string
			  (+fx i (string-length (js-get res (js-toname 0))))))))
		(else
		 (let ((newstring (js-tostring replacevalue))
		       (i (js-get res 'index)))
		    (string-append (substring string 0 i)
		       (table22 newstring res string)
		       (substring string
			  (+fx i (string-length (js-get res (js-toname 0)))))))))))
	 (else
	  (let* ((previousLastIndex 0)
		 (exec (js-get (js-get js-regexp 'prototype) 'exec)))
	     (js-put! searchvalue 'lastIndex 0 #f)
	     (let loop ((n 0)
			(res '()))
		(let ((result (js-call1 exec searchvalue string)))
		   (if (eq? result (js-null))
		       (cond
			  ((null? res)
			   string)
			  ((isa? replacevalue JsFunction)
			   (let loop ((matches res)
				      (res string))
			      (if (null? matches)
				  res
				  (let* ((m (car matches))
					 (i (js-get m 'index)))
				     (loop (cdr matches)
					(string-append (substring string 0 i)
					   (js-tostring
					      (js-apply replacevalue
						 (js-undefined)
						 (cons
						    (js-get m (js-toname 0))
						    (append
						       (matches->string-list m)
						       (list i string)))))
					   (substring res
					      (+fx i (string-length (js-get m (js-toname 0)))))))))))
			  (else
			   (let ((newstring (js-tostring replacevalue)))
			      (let loop ((matches res)
					 (res string))
				 (if (null? matches)
				     res
				     (let* ((m (car matches))
					    (i (js-get m 'index)))
					(loop (cdr matches)
					   (string-append (substring string 0 i)
					      (table22 newstring m string)
					      (substring res
						 (+fx i (string-length (js-get m (js-toname 0)))))))))))))
		       (let ((thisIndex (js-get searchvalue 'lastIndex)))
			  (if (= thisIndex previousLastIndex)
			      (begin
				 (js-put! searchvalue 'lastIndex (+ thisIndex 1) #f)
				 (set! previousLastIndex (+ 1 thisIndex)))
			      (set! previousLastIndex thisIndex))
			  (let ((matchStr (js-get result 0)))
			     (loop (+fx 1 n) (cons result res))))))))))))

;*---------------------------------------------------------------------*/
;*    string-prototype-search ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.12    */
;*---------------------------------------------------------------------*/
(define (string-prototype-search this::obj regexp)
   (let ((string (js-tostring (js-cast-string this)))
	 (rx (if (isa? regexp JsRegExp)
		 regexp
		 (js-new js-regexp regexp))))
      (with-access::JsRegExp rx (rx global)
	 (let ((pos (pregexp-match-positions rx string)))
	    (if pos
		(caar pos)
		-1)))))

;*---------------------------------------------------------------------*/
;*    string-prototype-slice ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.13    */
;*---------------------------------------------------------------------*/
(define (string-prototype-slice this::obj start end)
   (let* ((s (js-tostring (js-cast-string this)))
	  (len (utf8-string-length s))
	  (intstart (js-tointeger start))
	  (intend (if (eq? end (js-undefined)) len (js-tointeger end)))
	  (from (->fixnum
		   (if (< intstart 0)
		       (max (+ len intstart) 0)
		       (min intstart len))))
	  (to (->fixnum
		 (if (< intend 0)
		     (max (+ len intend) 0)
		     (min intend len))))
	  (span (maxfx (-fx to from) 0)))
      (utf8-substring s from (+ from span))))

;*---------------------------------------------------------------------*/
;*    string-prototype-split ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.14    */
;*---------------------------------------------------------------------*/
(define (string-prototype-split this::obj separator limit)

   (define (minelong2::elong n1::elong n2::elong)
      (if (<elong n1 n2) n1 n2))
   
   (define (split-match S q R)
      (if (isa? R JsRegExp)
	  (with-access::JsRegExp R (rx)
	     (or (pregexp-match-positions rx S q) 'failure))
	  (let ((r (utf8-string-length R))
		(s (utf8-string-length S)))
	     (cond
		((>fx (+fx q r) s)
		 'failure)
		((substring-at? S R q)
		 (list (cons q (+fx q r))))
		(else
		 'failure)))))

   (let* ((S (js-tostring (js-cast-string this)))
	  (A (js-new js-array 0))
	  (lim (if (eq? limit (js-undefined))
		   (+fx (string-length S) 1)
		   (elong->fixnum
		      (minelong2
			 (uint32->elong (js-touint32 limit))
			 (fixnum->elong (+fx 1 (string-length S)))))))
	  (s (utf8-string-length S))
	  (p 0)
	  (R (if (isa? separator JsRegExp) separator (js-tostring separator))))
      (cond
	 ((=fx lim 0)
	  ;; 9
	  A)
	 ((eq? separator (js-undefined))
	  ;; 10
	  (js-define-own-property A 0
	     (instantiate::JsValueDescriptor
		(name (js-toname 0))
		(value S)
		(writable #t)
		(enumerable #t)
		(configurable #t))
	     #f)
	  A)
	 ((=fx s 0)
	  ;; 11
	  (let ((z (split-match S 0 R)))
	     (when (eq? z 'failure)
		(js-define-own-property A 0
		   (instantiate::JsValueDescriptor
		      (name (js-toname 0))
		      (value S)
		      (writable #t)
		      (enumerable #t)
		      (configurable #t))
		   #f))
	     A))
	 (else
	  ;; 13
	  (let loop ((q p)
		     (p p))
	     (if (not (=fx q s))
		 (let ((z (split-match S q R)))
;* 		    (tprint "split-match S=" S " q=" q " p=" p " -> z=" z) */
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
				     (l (->fixnum (js-get A 'length))))
				  ;; 13.c.iii.2
				  (js-define-own-property A l
				     (instantiate::JsValueDescriptor
					(name (js-toname l))
					(value T)
					(writable #t)
					(enumerable #t)
					(configurable #t))
				     #f)
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
							 (name (js-toname l))
							 (value (car cap))
							 (writable #t)
							 (enumerable #t)
							 (configurable #t))
						      #f)
						   (if (=fx (+fx l 1) lim)
						       ;; 13.c.iii.7.d
						       A
						       ;; 13.c.iii.8
						       (repeat (cdr cap) (+fx l 1))))
						(loop p e))))))))))
		 ;; 14
		 (let ((T (substring S p s))
		       (l (js-get A 'length)))
		    ;; 15
		    (js-define-own-property A l
		       (instantiate::JsValueDescriptor
			  (name (js-toname l))
			  (value T)
			  (writable #t)
			  (enumerable #t)
			  (configurable #t))
		       #f)
		    ;;16
		    A)))))))

;*---------------------------------------------------------------------*/
;*    string-prototype-substring ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.15    */
;*---------------------------------------------------------------------*/
(define (string-prototype-substring this::obj start end)
   (let* ((s (js-tostring (js-cast-string this)))
	  (len (utf8-string-length s))
	  (intstart (js-tointeger start))
	  (intend (if (eq? end (js-undefined)) len (js-tointeger end)))
	  (finalstart (->fixnum (min (max intstart 0) len)))
	  (finalend (->fixnum (min (max intend 0) len)))
	  (from (minfx finalstart finalend))
	  (to (maxfx finalstart finalend)))
      (utf8-substring s from to)))

;*---------------------------------------------------------------------*/
;*    string-prototype-tolowercase ...                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.16    */
;*---------------------------------------------------------------------*/
(define (string-prototype-tolowercase this::obj)
   (let ((s (js-tostring (js-cast-string this))))
      (string-downcase s)))

;*---------------------------------------------------------------------*/
;*    string-prototype-tolocalelowercase ...                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.17    */
;*---------------------------------------------------------------------*/
(define (string-prototype-tolocalelowercase this::obj)
   (let ((s (js-tostring (js-cast-string this))))
      (utf8-string-locale-downcase s)))

;*---------------------------------------------------------------------*/
;*    string-prototype-touppercase ...                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.18    */
;*---------------------------------------------------------------------*/
(define (string-prototype-touppercase this::obj)
   (let ((s (js-tostring (js-cast-string this))))
      (string-upcase s)))

;*---------------------------------------------------------------------*/
;*    string-prototype-tolocaleuppercase ...                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.19    */
;*---------------------------------------------------------------------*/
(define (string-prototype-tolocaleuppercase this::obj)
   (let ((s (js-tostring (js-cast-string this))))
      (utf8-string-locale-upcase s)))

;*---------------------------------------------------------------------*/
;*    string-prototype-trim ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.20    */
;*---------------------------------------------------------------------*/
(define (string-prototype-trim this::obj)
   (trim-whitespaces+ (js-tostring (js-cast-string this)) :left #t :right #t))

;*---------------------------------------------------------------------*/
;*    string-prototype-substr ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.3        */
;*---------------------------------------------------------------------*/
(define (string-prototype-substr this::obj start length)
   (let* ((r1 (js-tostring this))
	  (r2 (js-tointeger start))
	  (r3 (if (eq? length (js-undefined)) (maxvalfx) (js-tointeger length)))
	  (r4 (utf8-string-length r1))
	  (r5 (if (>=fx r2 0) r2 (maxfx (+fx r4 r2) 0)))
	  (r6 (minfx (maxfx r3 0) (-fx r4 r5))))
      (if (<=fx r6 0)
	  ""
	  (utf8-substring r1 r5 r6))))

;*---------------------------------------------------------------------*/
;*    js-string-append ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-string-append left right)
   (utf8-string-append left right))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsString ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::JsString proc)
   (with-access::JsString o (val)
      (let ((len (utf8-string-length val)))
	 (if (>fx len 0)
	     (let loop ((i 0))
		(if (<fx i len)
		    (begin
		       (proc (integer->string i))
		       (loop (+fx i 1)))
		    (call-next-method)))
	     (call-next-method)))))
  
