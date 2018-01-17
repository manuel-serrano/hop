;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-cast.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 07:13:28 2017                          */
;*    Last change :  Wed Jan 17 17:42:24 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Casting values from JS types to SCM implementation types.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-cast

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_scheme-utils)
   
   (export (j2s-cast expr::obj ::obj ::symbol ::symbol ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    method-as-int32-table ...                                        */
;*---------------------------------------------------------------------*/
(define method-as-int32-table
   '(js-jsstring-charcodeatu32
     js-jsstring-charcodeat))

;*---------------------------------------------------------------------*/
;*    cast-table ...                                                   */
;*---------------------------------------------------------------------*/
(define cast-table
   ;; from x to -> conv
   `((int32
	((int30 nop)
	 (uint32 ,js-int32->uint32)
	 (int53 int32->fixnum)
	 (integer ,js-int32->integer)
	 (number ,js-int32->integer)
	 (propname ,js-int32->propname)
	 (bool ,js-int32->bool)
	 (string ,js-int32->string)
	 (any ,js-int32->integer)))
     (uint32
	((uint29 nop)
	 (int32 ,js-uint32->int32)
	 (int53 ,js-uint32->fixnum)
	 (integer ,js-uint32->integer)
	 (number ,js-uint32->integer)
	 (propname ,js-uint32->propname)
	 (bool ,js-uint32->bool)
	 (string ,js-uint32->string)
	 (any ,js-uint32->integer)))
     (int53
	((int30 nop)
	 (int32 js-int53-toint32)
	 (uint32 js-int53-touint32)
	 (number js-int53-tointeger)
	 (any js-int53-tointeger)))
     (integer
	((int32 ,js-fixnum->int32)
	 (uint32 ,js-fixnum->uint32)
	 (string ,js-fixnum->string)
	 (int53 nop)
	 (propname nop)
	 (number nop)
	 (any nop)))
     (number
	((bool js-totest)
	 (int32 ,js-number->int32)
	 (uint32 ,js-number->uint32)
	 (string ,js-number->string)
	 (integer nop)
	 (propname nop)
	 (any nop)))
     (string
	((propname nop)
	 (bool ,js-string->bool)
	 (any nop)))
     (function
	((any nop)))
     (object
	((any nop)))
     (bool
	((int32 ,js-bool->int32)
	 (uint32 ,js-bool->uint32)
	 (any nop)))
     (null
	((any nop)))
     (undefined
	((any nop)))
     (regexp
	((any nop)))
     (array
	((any nop)))
     (arguments
	((any nop)))
     (date
	((any nop)))
     (tilde
	((any nop)))
     (scmstring
	((any nop)))
     (real
	((uint32 js-number-touint32))
	((int32 js-number-toint32)))
     (class
	((any nop)))
     (any
	((propname nop)
	 (bool js-totest)
	 (array nop)
	 (string ,js->string)
	 (null nop)
	 (int32 ,js->int32)
	 (uint32 ,js->uint32)
	 (int53 nop)
	 (number ,js->number)
	 (object ,js->object)))))

;*---------------------------------------------------------------------*/
;*    cast ancially functions                                          */
;*---------------------------------------------------------------------*/
;; int32
(define (js-int32->uint32 v expr conf)
   (if (int32? v) (int32->uint32 v) `(int32->uint32 ,v)))

(define (js-int32->integer v expr conf)
   (cond
      ((and (int32? v)
	    (or (and (<s32 v (-s32 (bit-lshs32 #s32:1 29) #s32:1))
		     (>=s32 v (-s32 #s32:0 (bit-lshs32 #s32:1 29))))
		(m64? conf)))
       (int32->fixnum v))
      ((or (and (isa? expr J2SExpr) (inrange-int30? expr)) (m64? conf))
       `(int32->fixnum ,v))
      (else
       `(js-int32-tointeger ,v))))

(define (js-int32->bool v expr conf)
   (if (int32? v) (not (=s32 v #s32:0)) `(not (=s32 ,v #s32:0))))

(define (js-int32->string v expr conf)
   (if (int32? v)
       (fixnum->string (int32->fixnum v))
       `(js-ascii->jssstring (int32->string ,v))))

;; uint32
(define (js-uint32->integer v expr conf)
   (cond
      ((and (uint32? v) (or (<u32 v (bit-lshu32 #u32:1 29)) (m64? conf)))
       (uint32->fixnum v))
      ((or (and (isa? expr J2SExpr) (inrange-int30? expr)) (m64? conf))
       `(uint32->fixnum ,v))
      (else
       `(js-uint32-tointeger ,v))))

(define (js-uint32->fixnum v expr conf)
   (if (uint32? v) (uint32->fixnum v) `(uint32->fixnum ,v)))

(define (js-uint32->int32 v expr conf)
   (if (uint32? v) (uint32->int32 v) `(uint32->int32 ,v)))

(define (js-uint32->bool v expr conf)
   (if (uint32? v) (>u32 v #u32:0) `(>u32 ,v #u32:0)))

(define (js-uint32->string v expr conf)
   (cond
      ((uint32? v)
       (llong->string (uint32->llong v)))
      ((inrange-int32? expr)
       `(js-ascii->jssstring (uint32->fixnum ,v)))
      ((m64? conf)
       `(js-ascii->jssstring (uint32->string ,v)))
      (else
       `(js-ascii->jssstring (llong->string (uint32->llong ,v))))))

;; fixnum
(define (js-fixnum->int32 v expr conf)
   (if (fixnum? v) (fixnum->int32 v) `(fixnum->uint32 ,v)))

(define (js-fixnum->uint32 v expr conf)
   (if (fixnum? v) (fixnum->uint32 v) `(fixnum->uint32 ,v)))

(define (js-fixnum->string v expr conf)
   (if (fixnum? v)
       (integer->string v)
       `(js-ascii->jssstring ,v)))

;; number
(define (js-number->string v expr conf)
   (cond
      ((fixnum? v)
       `(js-ascii->jsstring ,(integer->string v)))
      ((int32? v)
       `(js-ascii->jsstring ,(integer->string (int32->fixnum v))))
      ((uint32? v)
       `(js-ascii->jsstring ,(llong->string (uint32->llong v))))
      (else
       `(js-ascii->jsstring (js-tonumber ,v %this)))))

;; string
(define (js-string->bool v expr conf)
   (if (string? v)
       (>fx (string-length v) 0)
       `(js-jsstring->bool ,v)))

;; any
(define (js->object v expr conf)
   (if (eq? (j2s-type expr) 'object)
       v
       `(js-toobject ,v %this)))

(define (js->number v expr conf)
   (if (memq (j2s-type expr) '(uint32 int32 integer bint number))
       v
       `(js-tonumber ,v %this)))

(define (js->string v expr conf)
   (match-case v
      ((js-jsstring-ref ?str ?idx)
       (set-car! v 'js-jsstring-ref-as-string)
       v)
      ((js-string-ref ?str ?idx ?this)
       (set-car! v 'js-string-ref-as-string)
       v)
      (else
       `(js-tojsstring ,v %this))))

;; bool
(define (js-bool->int32 v expr conf)
   (if (boolean? v) (if v #s32:1 #s32:0) `(if ,v #s32:1 #s32:0)))

(define (js-bool->uint32 v expr conf)
   (if (boolean? v) (if v #u32:1 #u32:0) `(if ,v #u32:1 #u32:0)))



(define (js-uint32->propname v expr conf)
   (if (and (uint32? v) (<u32 v (-u32 (bit-lshu32 #u32:1 29) #u32:1)))
       (uint32->fixnum v)
       v))

(define (js-int32->propname v expr conf)
   (if (and (int32? v)
	    (<s32 v (-s32 (bit-lshs32 #s32:1 29) #s32:1)) (>=s32 v #s32:0))
       (int32->fixnum v)
       v))

(define (js->int32 v expr conf)
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
	   `(js-toint32 ,v %this))))))

(define (js->uint32 v expr conf)
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
       `(js-touint32 ,v %this))))

(define (js-number->int32 v expr conf)
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
       `(js-number-toint32 ,v))))

(define (js-number->uint32 v expr conf)
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
       `(js-number-touint32 ,v))))

;*---------------------------------------------------------------------*/
;*    j2s-cast ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-cast sexp expr from to conf)
   
   (define (err)
      (error "cast" (format "illegal cast ~a -> ~a" from to) sexp))
   
   (define (tostring sexp)
      (match-case sexp
	 ((js-jsstring-ref ?str ?idx)
	  (set-car! sexp 'js-jsstring-ref-as-string)
	  sexp)
	 ((js-string-ref ?str ?idx ?this)
	  (set-car! sexp 'js-string-ref-as-string)
	  sexp)
	 (else
	  `(js-tojsstring ,sexp %this))))

   (define (default)
      (if (or (eq? from to) (eq? to '*))
	  sexp
	  (cond
	     ((eq? to 'int32)
	      (js->int32 sexp expr conf))
	     ((eq? to 'uint32)
	      (js->uint32 sexp expr conf))
	     (else
	      (tprint "CAST DEFAULT... " from " -> " to
		 " " (when (isa? expr J2SExpr)
			(with-access::J2SExpr expr (loc) loc))
		 " " (j2s->list expr))
	      (case from
		 ((index uint32 length)
		  (case to
		     ((uint32 index length) sexp)
		     ((bool) `(> ,sexp 0))
		     (else sexp)))
		 ((int53)
		  (case to
		     ((index uint32 length) (err))
		     ((bool) `(not (= ,sexp 0)))
		     (else sexp)))
		 ((integer number)
		  (case to
		     ((index uint32 length) (js-fixnum->uint32 sexp expr conf))
		     ((bool) `(not (= ,sexp 0)))
		     (else sexp)))
		 (else
		  (case to
		     ((string) (tostring sexp))
		     ((index uint32 length) (js-fixnum->uint32 sexp expr conf))
		     ((bool) `(js-totest ,sexp))
		     (else sexp))))))))

   (if (eq? from to)
       sexp
       (let ((fen (assq from cast-table)))
	  (if (pair? fen)
	      (let ((ten (assq to (cadr fen))))
		 (if (pair? ten)
		     (if (symbol? (cadr ten))
			 (if (eq? (cadr ten) 'nop)
			     sexp
			     `(,(cadr ten) ,sexp))
			 ((cadr ten) sexp expr conf))
		     (default)))
	      (default)))))
