;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-cast.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 07:13:28 2017                          */
;*    Last change :  Sun Apr 12 13:05:42 2020 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Casting values from JS types to SCM implementation types.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-cast

   (include "context.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_scheme-test
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-constant)
   
   (export (j2s-as expr::obj ::obj ::symbol ::symbol ::struct)
	   (j2s-cast expr::obj ::obj ::symbol ::symbol ::struct)))

;*---------------------------------------------------------------------*/
;*    method-as-int32-table ...                                        */
;*---------------------------------------------------------------------*/
(define method-as-int32-table
   '(js-jsstring-charcodeatu32
     js-jsstring-charcodeat))

;*---------------------------------------------------------------------*/
;*    as-table ...                                                     */
;*---------------------------------------------------------------------*/
(define as-table
   ;; from x to -> conv
   `((int30
	((int30 nop)
	 (uint32 fixnum->uint32)
	 (int32 fixnum->int32)
	 (int53 nop)
	 (integer nop)
	 (number nop)
	 (real fixnum->real)
	 (bint nop)))
     (int32
	((int30 nop)
	 (uint32 int32->uint32)
	 (int53 int32->fixnum)
	 (integer ,js-int32->integer)
	 (number ,js-int32->integer)
	 (real ,js-int32->real)
	 (propname ,js-int32->propname)
	 (bool ,js-int32->bool)
	 (string ,js-int32->string)
	 (scmstring number->string)
	 (object ,js-int32->jsobject)
	 (iterable error)
	 (bint int32->fixnum)
	 (any ,js-int32->integer)))
     (uint32
	((uint29 nop)
	 (int32 uint32->int32)
	 (int53 ,js-uint32->fixnum)
	 (integer ,js-uint32->fixnum)
	 (real ,js-uint32->real)
	 (number ,js-uint32->integer)
	 (propname ,js-uint32->propname)
	 (bool ,js-uint32->bool)
	 (string ,js-uint32->string)
	 (scmstring number->string)
	 (object ,js-uint32->jsobject)
	 (iterable error)
	 (bint uint32->fixnum)
	 (any ,js-uint32->fixnum)))
     (int53
	((int30 nop)
	 (int32 fixnum->int32)
	 (uint32 fixnum->uint32)
	 (integer nop)
	 (number nop)
	 (real fixnum->flonum)
	 (string ,js-fixnum->string)
	 (scmstring fixnum->string)
	 (object ,js-number->jsobject)
	 (iterable error)
	 (bool ,(lambda (v expr ctx) `(not (=fx ,v 0))))
	 (any nop)))
     (integer
	((int32 fixnum->int32)
	 (uint32 fixnum->uint32)
	 (string ,js-integer->string)
	 (scmstring number->string)
	 (real fixnum->flonum)
	 (int53 nop)
	 (propname nop)
	 (number nop)
	 (iterable error)
	 (any nop)))
     (number
	((bool js-totest)
	 (int32 ,js-number->int32)
	 (uint32 ,js-number->uint32)
	 (int53 nop)
	 (integer nop)
	 (real ,js-number->real)
	 (string ,js-number->string)
	 (scmstring number->string)
	 (propname nop)
	 (object ,js-number->jsobject)
	 (iterable error)
	 (any nop)))
     (string
	((propname nop)
	 (bool ,js-string->bool)
	 (object ,js-string->jsobject)
	 (scmstring js-jsstring->string)
	 (iterable ,(lambda (v expr ctx) `(js-jsstring->jsarray ,v %this)))
	 (any nop)))
     (function
	((scmstring js-jsstring->string)
	 (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))
	 (any nop)))
     (object
	((bool ,(lambda (v expr ctx) #t))
	 (array nop)
	 (scmstring ,js->scmstring)
	 (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))
	 (real ,(lambda (v expr ctx) `(js-toflonum (js-tonumber ,v %this))))
	 (integer ,(lambda (v expr ctx) `(js-tointeger ,v %this)))
	 (number ,(lambda (v expr ctx) `(js-tonumber ,v %this)))
	 (any nop)))
     (bool
	((int32 ,js-bool->int32)
	 (uint32 ,js-bool->uint32)
	 (object ,js-bool->jsobject)
	 (scmstring ,js->scmstring)
	 (real ,(lambda (v expr ctx) `(js-toflonum (js-tonumber ,v %this))))
	 (integer ,(lambda (v expr ctx) `(js-tointeger ,v %this)))
	 (number ,(lambda (v expr ctx) `(js-tonumber ,v %this)))
	 (iterable error)
	 (any nop)))
     (null
	((object ,js-toobject)
	 (scmstring ,js->scmstring)
	 (number ,(lambda (v expr ctx) 0))
	 (int32 ,(lambda (v expr ctx) #s32:0))
	 (uint32 ,(lambda (v expr ctx) #u32:0))
	 (integer ,(lambda (v expr ctx) 0))
	 (real ,(lambda (v expr ctx) 0.0))
	 (iterable error)
	 (any nop)))
     (undefined
	((object ,js-toobject)
	 (bool ,(lambda (v expr ctx) #f))
	 (scmstring ,js->scmstring)
	 (number ,(lambda (v expr ctx) +nan.0))
	 (real ,(lambda (v expr ctx) +nan.0))
	 (iterable error)
	 (any nop)))
     (regexp
	((scmstring ,js->scmstring)
	 (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))
	 (any nop)))
     (array
	((scmstring ,js->scmstring)
	 (bool ,(lambda (v expr ctx) #t))
	 (iterable nop)
	 (any nop)))
     (arguments
	((scmstring ,js->scmstring)
	 (iterable ,(lambda (v expr ctx) `(js-arguments->jsarray ,v %this)))
	 (any nop)))
     (date
	((scmstring ,js->scmstring)
	 (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))
	 (any nop)))
     (tilde
	((scmstring ,js->scmstring)
	 (iterable error)
	 (any nop)))
     (scmstring
	((bool js-string->bool)
	 (iterable ,(lambda (v expr ctx) `(js-jsstring->jsarray ,v %this)))
	 (any js-string->jsstring)))
     (real
	((uint32 js-number-touint32)
	 (int32 js-number-toint32)
	 (object ,js-number->jsobject)
	 (number nop)
	 (string ,js-number->string)
	 (scmstring ,number->string)
	 (iterable error)
	 (any nop)))
     (class
	   ((scmstring ,js->scmstring)
	    (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))
	    (any nop)))
     (any
	((int32 ,js->int32)
	 (uint32 ,js->uint32)
	 (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))))))

;*---------------------------------------------------------------------*/
;*    cast-table ...                                                   */
;*---------------------------------------------------------------------*/
(define cast-table
   ;; from x to -> conv
   `((int30
	((int30 nop)
	 (uint32 fixnum->uint32)
	 (int32 fixnum->int32)
	 (int53 nop)
	 (integer nop)
	 (number nop)
	 (real fixnum->real)
	 (bint nop)))
     (int32
	((int30 nop)
	 (uint32 ,js-int32->uint32)
	 (int53 ,js-int32->fixnum)
	 (integer ,js-int32->integer)
	 (number ,js-int32->integer)
	 (real ,js-int32->real)
	 (propname ,js-int32->propname)
	 (bool ,js-int32->bool)
	 (string ,js-int32->string)
	 (scmstring number->string)
	 (object ,js-int32->jsobject)
	 (iterable error)
	 (bint int32->fixnum)
	 (any ,js-int32->integer)))
     (uint32
	((uint29 nop)
	 (int32 ,js-uint32->int32)
	 (int53 ,js-uint32->fixnum)
	 (integer ,js-uint32->integer)
	 (real ,js-uint32->real)
	 (number ,js-uint32->integer)
	 (propname ,js-uint32->propname)
	 (bool ,js-uint32->bool)
	 (string ,js-uint32->string)
	 (scmstring number->string)
	 (object ,js-uint32->jsobject)
	 (iterable error)
	 (bint uint32->fixnum)
	 (any ,js-uint32->integer)))
     (int53
	((int30 nop)
	 (int32 ,js-fixnum->int32)
	 (uint32 ,js-fixnum->uint32)
	 (integer nop)
	 (number nop)
	 (real fixnum->flonum)
	 (string ,js-fixnum->string)
	 (scmstring fixnum->string)
	 (object ,js-number->jsobject)
	 (iterable error)
	 (bool ,(lambda (v expr ctx) `(not (=fx ,v 0))))
	 (any nop)))
     (integer
	((int32 ,js-integer->int32)
	 (uint32 ,js-integer->uint32)
	 (string ,js-integer->string)
	 (scmstring number->string)
	 (real fixnum->flonum)
	 (int53 nop)
	 (propname nop)
	 (number nop)
	 (iterable error)
	 (any nop)))
     (number
	((bool js-totest)
	 (int32 ,js-number->int32)
	 (uint32 ,js-number->uint32)
	 (int53 nop)
	 (integer nop)
	 (real ,js-number->real)
	 (string ,js-number->string)
	 (scmstring number->string)
	 (propname nop)
	 (object ,js-number->jsobject)
	 (iterable error)
	 (any nop)))
     (string
	((propname nop)
	 (bool ,js-string->bool)
	 (object ,js-string->jsobject)
	 (scmstring js-jsstring->string)
	 (iterable ,(lambda (v expr ctx) `(js-jsstring->jsarray ,v %this)))
	 (any nop)))
     (function
	((scmstring js-jsstring->string)
	 (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))
	 (any nop)))
     (object
	((bool ,(lambda (v expr ctx) #t))
	 (array nop)
	 (scmstring ,js->scmstring)
	 (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))
	 (real ,(lambda (v expr ctx) `(js-toflonum (js-tonumber ,v %this))))
	 (integer ,(lambda (v expr ctx) `(js-tointeger ,v %this)))
	 (number ,(lambda (v expr ctx) `(js-tonumber ,v %this)))
	 (any nop)))
     (bool
	((int32 ,js-bool->int32)
	 (uint32 ,js-bool->uint32)
	 (object ,js-bool->jsobject)
	 (scmstring ,js->scmstring)
	 (real ,(lambda (v expr ctx) `(js-toflonum (js-tonumber ,v %this))))
	 (integer ,(lambda (v expr ctx) `(js-tointeger ,v %this)))
	 (number ,(lambda (v expr ctx) `(js-tonumber ,v %this)))
	 (iterable error)
	 (any nop)))
     (null
	((object ,js-toobject)
	 (scmstring ,js->scmstring)
	 (number ,(lambda (v expr ctx) 0))
	 (int32 ,(lambda (v expr ctx) #s32:0))
	 (uint32 ,(lambda (v expr ctx) #u32:0))
	 (integer ,(lambda (v expr ctx) 0))
	 (real ,(lambda (v expr ctx) 0.0))
	 (iterable error)
	 (any nop)))
     (undefined
	((object ,js-toobject)
	 (bool ,(lambda (v expr ctx) #f))
	 (scmstring ,js->scmstring)
	 (number ,(lambda (v expr ctx) +nan.0))
	 (real ,(lambda (v expr ctx) +nan.0))
	 (iterable error)
	 (any nop)))
     (regexp
	((scmstring ,js->scmstring)
	 (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))
	 (any nop)))
     (array
	((scmstring ,js->scmstring)
	 (bool ,(lambda (v expr ctx) #t))
	 (iterable nop)
	 (any nop)))
     (arguments
	((scmstring ,js->scmstring)
	 (iterable ,(lambda (v expr ctx) `(js-arguments->jsarray ,v %this)))
	 (any nop)))
     (date
	((scmstring ,js->scmstring)
	 (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))
	 (any nop)))
     (tilde
	((scmstring ,js->scmstring)
	 (iterable error)
	 (any nop)))
     (scmstring
	((bool js-string->bool)
	 (iterable ,(lambda (v expr ctx) `(js-jsstring->jsarray ,v %this)))
	 (any js-string->jsstring)))
     (real
	((uint32 js-number-touint32)
	 (int32 js-number-toint32)
	 (object ,js-number->jsobject)
	 (number nop)
	 (string ,js-number->string)
	 (scmstring ,number->string)
	 (iterable error)
	 (any nop)))
     (class
	((scmstring ,js->scmstring)
	 (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))
	 (any nop)))
     (any
	((propname nop)
	 (undefined nop)
	 (function nop)
	 (array nop)
	 (null nop)
	 (int53 nop)
	 (bool ,js-any->bool)
	 (string ,js->string)
	 (scmstring ,js->scmstring)
	 (int32 ,js->int32)
	 (real ,js-any->real)
	 (uint32 ,js->uint32)
	 (integer ,js-any->integer)
	 (number ,js->number)
	 (object ,js->object)
	 (iterable ,(lambda (v expr ctx) `(js-jsobject->jsarray ,v %this)))))))

;*---------------------------------------------------------------------*/
;*    with-tmp1 ...                                                    */
;*---------------------------------------------------------------------*/
(define (with-tmp1 v proc)
   (if (symbol? v)
       (proc v)
       (let ((t (gensym '%v)))
	  `(let ((,t ,v))
	      ,(proc t)))))

;*---------------------------------------------------------------------*/
;*    int32->xxx ...                                                   */
;*---------------------------------------------------------------------*/
(define (js-int32->uint32 v expr ctx)
   (if (int32? v) (int32->uint32 v) `(int32->uint32 ,v)))

(define (js-int32->fixnum v expr ctx)
   (if (int32? v) (int32->fixnum v) `(int32->fixnum ,v)))

(define (js-int32->integer v expr ctx)
   (let ((conf (context-conf ctx)))
      (match-case v
	 ((fixnum->int32 ?x)
	  x)
	 (else
	  (cond
	     ((and (int32? v)
		   (or (and (<s32 v (-s32 (bit-lshs32 #s32:1 29) #s32:1))
			    (>=s32 v (-s32 #s32:0 (bit-lshs32 #s32:1 29))))
		       (m64? conf)))
	      (int32->fixnum v))
	     ((or (and (isa? expr J2SExpr) (inrange-int30? expr)) (m64? conf))
	      `(int32->fixnum ,v))
	     (else
	      `(js-int32-tointeger ,v)))))))

(define (js-int32->real v expr ctx)
   (cond
      ((int32? v) 
       (int32->flonum v))
      (else
       `(int32->flonum ,v))))

(define (js-int32->bool v expr ctx)
   (if (int32? v) (not (=s32 v #s32:0)) `(not (=s32 ,v #s32:0))))

(define (js-int32->string v expr ctx)
   (if (int32? v)
       (& (fixnum->string (int32->fixnum v)) (context-program ctx))
       `(js-ascii->jsstring (int32->string ,v))))

;*---------------------------------------------------------------------*/
;*    uint32->xxx ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-uint32->int32 v expr ctx)
   (if (uint32? v) (uint32->int32 v) `(uint32->int32 ,v)))

(define (js-uint32->integer v expr ctx)
   (let ((conf (context-conf ctx)))
      (cond
	 ((and (uint32? v) (or (<u32 v (bit-lshu32 #u32:1 29)) (m64? conf)))
	  (js-uint32->fixnum v expr ctx))
	 ((or (and (isa? expr J2SExpr) (inrange-int30? expr)) (m64? conf))
	  (match-case v
	     ((fixnum->uint32 ?e) e)
	     ((let ?bdgs (begin ?exp0 (fixnum->uint32 ?exp))) `(let ,bdgs ,exp0 ,exp))
	     ((let ?bdgs (fixnum->uint32 ?exp)) `(let ,bdgs ,exp))
	     (else `(uint32->fixnum ,v))))
	 (else
	  `(js-uint32-tointeger ,v)))))

(define (js-uint32->fixnum v expr ctx)
   (match-case v
      ((? uint32?) (uint32->fixnum v))
      ((fixnum->uint32 ?e) e)
      ((let ?bdgs (begin ?exp0 (fixnum->uint32 ?exp))) `(let ,bdgs ,exp0 ,exp))
      ((let ?bdgs (fixnum->uint32 ?exp)) `(let ,bdgs ,exp))
      (else `(uint32->fixnum ,v))))

(define (js-uint32->real v expr ctx)
   (cond
      ((uint32? v) 
       (uint32->flonum v))
      (else
       `(uint32->flonum ,v))))

(define (js-uint32->bool v expr ctx)
   (if (uint32? v) (>u32 v #u32:0) `(>u32 ,v #u32:0)))

(define (js-uint32->string v expr ctx)
   (let ((conf (context-conf ctx)))
      (cond
	 ((uint32? v)
	  (& (llong->string (uint32->llong v)) (context-program ctx)))
	 ((inrange-int32? expr)
	  `(js-integer->jsstring (uint32->fixnum ,v)))
	 ((m64? conf)
	  `(js-integer->jsstring (uint32->fixnum ,v)))
	 (else
	  `(js-ascii->jsstring (llong->string (uint32->llong ,v)))))))

;*---------------------------------------------------------------------*/
;*    fixnum->xxx ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-fixnum->int32 v expr ctx)
   (if (fixnum? v) (fixnum->int32 v) `(fixnum->int32 ,v)))

(define (js-fixnum->uint32 v expr ctx)
   (if (fixnum? v) (fixnum->uint32 v) `(fixnum->uint32 ,v)))

(define (js-fixnum->string v expr ctx)
   (if (fixnum? v)
       (& (integer->string v) (context-program ctx))
       `(js-ascii->jsstring (fixnum->string ,v))))

;; integer
(define (js-integer->int32 v expr ctx)
   (with-tmp1 v
      (lambda (v)
	 `(if (fixnum? ,v) (fixnum->int32 ,v) (flonum->int32 ,v)))))

(define (js-integer->uint32 v expr ctx)
   (with-tmp1 v
      (lambda (v)
	 `(if (fixnum? ,v) (fixnum->uint32 ,v) (flonum->uint32 ,v)))))

(define (js-integer->string v expr ctx)
   (if (integer? v)
       (& (integer->string v) (context-program ctx))
       `(js-ascii->jsstring (integer->string ,v))))

;; number
(define (js-number->string v expr ctx)
   (cond
      ((fixnum? v)
       (& (integer->string v) (context-program ctx)))
      ((int32? v)
       (& (integer->string (int32->fixnum v)) (context-program ctx)))
      ((uint32? v)
       (& (llong->string (uint32->llong v)) (context-program ctx)))
      (else
       `(js-ascii->jsstring (number->string ,v)))))

;; string
(define (js-string->bool v expr ctx)
   (if (string? v)
       (>fx (string-length v) 0)
       `(js-jsstring-toboolean ,v)))

(define (js-string->jsobject v expr ctx)
   `(with-access::JsGlobalObject %this (js-string)
       (js-new1 %this js-string ,v)))

;; any
(define (js->object v expr ctx)
   (if (eq? (j2s-type expr) 'object)
       v
       `(js-toobject %this ,v)))

(define (js-any->bool v expr ctx)
   (j2s-totest v))

(define (js->number v expr ctx)
   (if (memq (j2s-type expr) '(uint32 int32 integer bint number))
       v
       `(js-tonumber ,v %this)))

(define (js->string v expr ctx)
   (match-case v
      ((js-jsstring-ref ?str ?idx ?this)
       v)
      ((js-ascii-ref ?str ?idx ?this)
       v)
      ((js-string-ref ?str ?idx ?this)
       (set-car! v 'js-string-ref-as-string)
       v)
      (else
       `(js-tojsstring ,v %this))))

(define (js->scmstring v expr ctx)
   `(js-tostring ,v %this))

;; bool
(define (js-bool->int32 v expr ctx)
   (if (boolean? v)
       (if v #s32:1 #s32:0)
       (with-tmp1 v (lambda (v) `(if ,v #s32:1 #s32:0)))))

(define (js-bool->uint32 v expr ctx)
   (if (boolean? v) (if v #u32:1 #u32:0)
       (with-tmp1 v (lambda (v) `(if ,v #u32:1 #u32:0)))))

(define (js-bool->jsobject v expr ctx)
   `(with-access::JsGlobalObject %this (js-boolean)
       (js-new1 %this js-boolean ,v)))

(define (js-uint32->propname v expr ctx)
   (if (and (uint32? v) (<u32 v (-u32 (bit-lshu32 #u32:1 29) #u32:1)))
       (uint32->fixnum v)
       v))

(define (js-int32->propname v expr ctx)
   (if (and (int32? v)
	    (<s32 v (-s32 (bit-lshs32 #s32:1 29) #s32:1)) (>=s32 v #s32:0))
       (int32->fixnum v)
       v))

(define (js->int32 v expr ctx)
   (cond
      ((fixnum? v)
       (fixnum->int32 v))
      ((int32? v)
       v)
      ((uint32? v)
       (if (<u32 v (-u32 (bit-lshu32 #u32:1 31) #u32:1))
	   (uint32->int32 v)
	   `(js-toint32 ,(uint32->flonum v) %this)))
      (else
       (match-case v
	  (((and ?fun (? symbol?)) . ?args)
	   (if (memq fun method-as-int32-table)
	       (begin
		  (set-car! v (symbol-append fun '-as-int32))
		  v)
	       `(js-toint32 ,v %this)))
	  (else
	   (if (symbol? v)
	       `(if (fixnum? ,v) (fixnum->int32 ,v) (js-toint32 ,v %this))
	       `(js-toint32 ,v %this)))))))

(define (js->uint32 v expr ctx)
   (cond
      ((fixnum? v)
       (if (>=fx v 0)
	   (fixnum->uint32 v)
	   `(fixnum->uint32 ,v)))
      ((int32? v)
       (if (>=s32 v #s32:0)
	   (int32->uint32 v)
	   `(int->uint32 ,v)))
      ((uint32? v)
       v)
      ((eq? (j2s-type expr) 'uint32)
       `(fixnum->uint32 ,v))
      (else
       (if (symbol? v)
	   `(if (fixnum? ,v) (fixnum->uint32 ,v) (js-touint32 ,v %this))
	   `(js-touint32 ,v %this)))))

(define (js-number->int32 v expr ctx)
   (let ((conf (context-conf ctx)))
      (cond
	 ((int32? v)
	  v)
	 ((and (fixnum? v) (=fx (int32->fixnum (fixnum->int32 v)) v))
	  (fixnum->int32 v))
	 ((inrange-int30? expr)
	  `(fixnum->int32 ,v))
	 ((and (inrange-int32? expr) (m64? conf))
	  `(fixnum->int32 ,v))
	 (else
	  (match-case v
	     ((if (fixnum? ?test) (-fx/overflow ?x ?y) (and ?d (-/overflow ?x ?y)))
	      (if (m64? conf)
		  (with-tmp1 test
		     (lambda (test)
			`(if (fixnum? ,test)
			     (fixnum->int32 (-fx ,x ,y) )
			     (js-number-toint32 ,d))))
		  `(js-number-toint32 ,v)))
	     ((if (fixnum? ?test) (+fx/overflow ?x ?y) (and ?d (+/overflow ?x ?y)))
	      (if (m64? conf)
		  (with-tmp1 test
		     (lambda (test)
			`(if (fixnum? ,test)
			     (fixnum->int32 (+fx ,x ,y) )
			     (js-number-toint32 ,d))))
		  `(js-number-toint32 ,v)))
	     ((?- . ?-)
	      `(js-number-toint32 ,v))
	      (else
	       (with-tmp1 v
		  (lambda (v)
		     `(if (fixnum? ,v)
			  (fixnum->int32 ,v)
			  (js-number-toint32 ,v))))))))))

(define (js-number->uint32 v expr ctx)
   (let ((conf (context-conf ctx)))
      (cond
	 ((uint32? v)
	  v)
	 ((and (fixnum? v) (=fx (uint32->fixnum (fixnum->uint32 v)) v))
	  (fixnum->uint32 v))
	 ((inrange-int30? expr)
	  `(fixnum->uint32 ,v))
	 ((and (inrange-int32? expr) (m64? conf))
	  `(fixnum->uint32 ,v))
	 (else
	  `(js-number-touint32 ,v)))))

(define (js-int32->jsobject v expr ctx)
   (let ((conf (context-conf ctx)))
      (cond
	 ((inrange-int30? expr)
	  `(js-number->jsNumber (int32->fixnum ,v) %this))
	 ((m64? conf)
	  `(js-number->jsNumber (int32->fixnum ,v) %this))
	 (else
	  `(js-number->jsNumber (int32->flonum ,v) %this)))))

(define (js-uint32->jsobject v expr ctx)
   (let ((conf (context-conf ctx)))
      (cond
	 ((inrange-int30? expr)
	  `(js-number->jsNumber (uint32->fixnum ,v) %this))
	 ((m64? conf)
	  `(js-number->jsNumber (uint32->fixnum ,v) %this))
	 (else
	  `(js-number->jsNumber (uint32->flonum ,v) %this)))))

(define (js-number->jsobject v expr ctx)
   `(js-number->jsNumber ,v %this))

(define (js-toobject v expr ctx)
   `(js-toobject %this ,v))

;*---------------------------------------------------------------------*/
;*    js-any->real ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-any->real v expr ctx)
   (js->real v expr ctx #f))

;*---------------------------------------------------------------------*/
;*    js-any->integer ...                                              */
;*---------------------------------------------------------------------*/
(define (js-any->integer v expr ctx)
   `(js-tointeger ,v %this))

;*---------------------------------------------------------------------*/
;*    js-number->real ...                                              */
;*---------------------------------------------------------------------*/
(define (js-number->real v expr ctx)
   (js->real v expr ctx #t))

;*---------------------------------------------------------------------*/
;*    js->real ...                                                     */
;*---------------------------------------------------------------------*/
(define (js->real v expr ctx numberp)
   (let loop ((v v)
	      (return #f))
      (match-case v
	 ((? symbol?)
	  (if numberp
	      `(js-toflonum ,v)
	      `(js-toflonum (js-tonumber ,v %this))))
	 ((? fixnum?)
	  (fixnum->flonum v))
	 ((? flonum?)
	  v)
	 ((? llong?)
	  (llong->flonum v))
	 ((? elong?)
	  (elong->flonum v))
	 (((or let let* letrec letrec*) ?- ?body)
	  (set-car! (cddr v) (loop body return))
	  v)
	 ((if ?- ?then ?else)
	  (set-car! (cddr v) (loop then return))
	  (set-car! (cdddr v) (loop else return))
	  v)
	 (((or +fl -fl *fl /fl) ?- ?-)
	  v)
	 (((or -/overflow +/overflow */overflow) ?- ?-)
	  `(js-toflonum ,v))
	 (((or js-get/debug js-get-name/cache js-get-object-name/cache) . ?-)
	  (if numberp
	      `(js-toflonum ,v)
	      `(js-toflonum (js-tonumber ,v %this))))
	 (((or -js *js /js) ?- ?- ?-)
	  `(js-toflonum ,v))
	 ((bind-exit (?return) ?expr)
	  (loop expr return))
	 (((? (lambda (f) (eq? f return))) ?expr)
	  (loop expr #f))
	 (((or let* letrec*) ?- ?expr)
	  (loop expr return))
	 ((js-int32-tointeger ?expr)
	  `(int32->flonum ,expr))
	 ((llong->flonum ?-)
	  v)
	 ((begin (and ?prof (js-profile-log-call . ?-)) ?call)
	  `(begin ,prof ,(loop call return)))
	 (((or bit-orjs bit-andjs bit-xorjs bitnojs) . ?rest)
	  `(js-toflonum ,v))
	 ((-fx/overflow ?x ?y)
	  `(-fl (fixnum->flonum ,x) (fixnum->flonum ,y)))
	 ((+fx/overflow ?x ?y)
	  `(+fl (fixnum->flonum ,x) (fixnum->flonum ,y)))
	 ((%$$NN ?x ?y)
	  `(js-toflonum ,v))
	 ((js-tonumber
	     (js-global-object-get-name %scope
		(& "Infinity" (context-program ctx)) ?- %this)
	     %this)
	  +inf.0)
	 (else
	  (if numberp
	      `(js-toflonum ,v)
	      `(js-toflonum (js-tonumber ,v %this)))))))

;*---------------------------------------------------------------------*/
;*    j2s-as ...                                                       */
;*---------------------------------------------------------------------*/
(define (j2s-as sexp expr from to ctx)
   
   (define (default sexp expr from to ctx) sexp)
   
   (j2s-cast/table as-table sexp expr from to ctx default))

;*---------------------------------------------------------------------*/
;*    j2s-cast ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-cast sexp expr from to ctx)

   (define (tostring sexp)
      (match-case sexp
	 ((js-jsstring-ref ?str ?idx ?this)
	  sexp)
	 ((js-ascii-ref ?str ?idx ?this)
	  sexp)
	 ((js-string-ref ?str ?idx ?this)
	  (set-car! sexp 'js-string-ref-as-string)
	  sexp)
	 (else
	  `(js-tojsstring ,sexp %this))))

   (define (default sexp expr from to ctx)
      (if (or (eq? from to) (eq? to '*))
	  sexp
	  (cond
	     ((eq? to 'int32)
	      (js->int32 sexp expr ctx))
	     ((eq? to 'uint32)
	      (js->uint32 sexp expr ctx))
	     (else
	      (case from
		 ((index uint32 length)
		  (case to
		     ((uint32 index length) sexp)
		     ((bool) `(> ,sexp 0))
		     (else sexp)))
		 ((int53)
		  (case to
		     ((index uint32 length) (cast-error sexp from to))
		     ((bool) `(not (= ,sexp 0)))
		     (else sexp)))
		 ((integer number)
		  (case to
		     ((index uint32 length) (js-fixnum->uint32 sexp expr ctx))
		     ((bool) `(not (= ,sexp 0)))
		     (else sexp)))
		 (else
		  (case to
		     ((string) (tostring sexp))
		     ((index uint32 length) (js-fixnum->uint32 sexp expr ctx))
		     ((bool) (j2s-totest sexp))
		     (else sexp))))))))
   
   (j2s-cast/table cast-table sexp expr from to ctx default))


;*---------------------------------------------------------------------*/
;*    cast-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (cast-error sexp from to)
   (error "cast" (format "illegal cast ~a -> ~a" from to) sexp))

;*---------------------------------------------------------------------*/
;*    j2s-cast/table ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-cast/table table sexp expr from to ctx default)
   (if (eq? from to)
       sexp
       (let ((fen (assq from table)))
	  (if (pair? fen)
	      (let ((ten (assq to (cadr fen))))
		 (if (pair? ten)
		     (if (symbol? (cadr ten))
			 (case (cadr ten)
			    ((nop) sexp)
			    ((error) (cast-error sexp from to))
			    (else `(,(cadr ten) ,sexp)))
			 ((cadr ten) sexp expr ctx))
		     (default sexp expr from to ctx)))
	      (default sexp expr from to ctx)))))
