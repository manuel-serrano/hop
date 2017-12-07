;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-cast.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 07:13:28 2017                          */
;*    Last change :  Thu Dec  7 20:16:59 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Casting values from JS types to SCM implementation types.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-cast

   (import __js2scheme_ast
	   __js2scheme_utils)
   
   (export (j2s-cast expr::obj ::symbol ::symbol ::symbol)))

;*---------------------------------------------------------------------*/
;*    cast-table ...                                                   */
;*---------------------------------------------------------------------*/
(define cast-table
   ;; from x to -> conv
   `((int32
	((int30 nop)
	 (uint32 ,js-int32->uint32)
	 (integer js-int32-tointeger)
	 (number js-int32-tointeger)
	 (propname ,js-int32->propname)
	 (bool ,js-int32->bool)
	 (any js-int32-tointeger)))
     (uint32
	((uint29 nop)
	 (int32 ,js-uint32->int32)
	 (int53 uint32->fixnum)
	 (integer ,js-uint32->integer)
	 (number ,js-uint32->integer)
	 (propname ,js-uint32->propname)
	 (bool ,js-uint32->bool)
	 (any ,js-uint32->integer)))
     (int53
	((int30 nop)
	 (int32 js-int53-toint32)
	 (uint32 js-int53-touint32)
	 (number js-int53-tointeger)
	 (any js-int53-tointeger)))
     (bint
	((int32 fixnum->int32)
	 (uint32 fixnum->uint32)))
     (integer
	((int32 ,js-number->int32)
	 (propname nop)
	 (number nop)))
     (number
	((bool js-totest)
	 (int32 ,js->int32)
	 (uint32 ,js-number->uint32)
	 (propname nop)
	 (any nop)))
     (string
	((propname nop)
	 (any nop)))
     (function
	((any nop)))
     (object
	((any nop)))
     (bool
	((int32 ,js->int32)
	 (uint32 ,js->uint32)))
     (any
	((propname nop)
	 (bool js-totest)
	 (int32 ,js->int32)
	 (uint32 ,js->uint32)
	 (number ,js->number)))))

;; cast ancillary functions
(define (js-uint32->integer v etype)
   (if (and (uint32? v) (<u32 v (-u32 (bit-lshu32 #u32:1 29) #u32:1)))
       (uint32->fixnum v)
       `(js-uint32-tointeger ,v)))

(define (js-uint32->int32 v etype)
   (if (uint32? v) (uint32->int32 v) `(uint32->int32 ,v)))
(define (js-int32->uint32 v etype)
   (if (int32? v) (int32->uint32 v) `(int32->uint32 ,v)))

(define (js-uint32->bool v etype)
   `(>=u32 ,v #u32:0))
(define (js-int32->bool v etype)
   `(not (=s32 ,v #s32:0)))

(define (js-uint32->propname v etype)
   (if (and (uint32? v) (<u32 v (-u32 (bit-lshu32 #u32:1 29) #u32:1)))
       (uint32->fixnum v)
       v))

(define (js-int32->propname v etype)
   (if (and (int32? v)
	    (<s32 v (-s32 (bit-lshs32 #s32:1 29) #s32:1)) (>=s32 v #s32:0))
       (int32->fixnum v)
       v))

(define (js->int32 v etype)
   `(js-toint32 ,v %this))
(define (js->uint32 v etype)
   `(js-touint32 ,v %this))

(define (js-number->int32 v etype)
   (cond
      ((int32? v)
       v)
      ((and (fixnum? v) (=fx (int32->fixnum (fixnum->int32 v)) v))
       (fixnum->int32 v))
      (else
       (tprint "COMMENT TO BE CHECKED AND REMOVED (2017-12-06)...")
       `(js-number-toint32 ,v))))

(define (js-number->uint32 v etype)
   (cond
      ((uint32? v)
       v)
      ((and (fixnum? v) (=fx (uint32->fixnum (fixnum->uint32 v)) v))
       (fixnum->uint32 v))
      (else
       `(js-number-touint32 ,v))))

(define (js->number v etype)
   `(js-tonumber ,v %this))

;*---------------------------------------------------------------------*/
;*    j2s-cast ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-cast sexp etype from to)
   
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
      (tprint "cast sexp=" sexp " from=" from " etype=" etype " to=" to)
      (if (or (eq? from to) (eq? to '*))
	  sexp
	  (case from
	     ((uint29)
	      (case to
		 ((uint32 index length) (fixnum->uint32 sexp))
		 ((bool) `(>u32 ,sexp #u32:0))
		 (else sexp)))
	     ((index uint32 length)
	      (case to
		 ((uint32 index length) sexp)
		 ((bool) `(> ,sexp 0))
		 (else sexp)))
	     ((int30)
	      (case to
		 ((index uint32 length) (fixnum->uint32 sexp))
		 ((bool) `(not (= ,sexp 0)))
		 (else sexp)))
	     ((int53)
	      (case to
		 ((index uint32 length) (err))
		 ((bool) `(not (= ,sexp 0)))
		 (else sexp)))
	     ((fixnum)
	      (case to
		 ((index uint32 length) (js-fixnum->uint32 sexp))
		 ((bool) `(not (=fx ,sexp 0)))
		 (else sexp)))
	     ((integer number)
	      (case to
		 ((index uint32 length) (js-fixnum->uint32 sexp))
		 ((bool) `(not (= ,sexp 0)))
		 (else sexp)))
	     (else
	      (case to
		 ((index uint32 length) (js-fixnum->uint32 sexp))
		 ((string) (tostring sexp))
		 ((bool) `(js-totest ,sexp))
		 (else sexp))))))

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
			 ((cadr ten) sexp etype))
		     (default)))
	      (default)))))
	  
;*---------------------------------------------------------------------*/
;*    box-table ...                                                    */
;*---------------------------------------------------------------------*/
(define box-table
   ;; from x to -> conv
   `((integer
	((int32 fixnum->int32)
	 (uint32 fixnum->uint32)
	 (int53 nop)))
     (int32
	((uint32 int32->uint32)
	 (integer int32->fixnum)
	 (number int32->fixnum)
	 (any int32->fixnum)))
     (uint32
	((int32 uint32->int32)
	 (integer uint32->fixnum)
	 (number uint32->fixnum)
	 (any uint32->fixnum)))
     (function
	((any nop)))
     (object
	((any nop)))
     (array
	((any nop)))
     (any
	((int32 fixnum->int32)
	 (uint32 fixnum->uint32)
	 (int53 nop)
	 (integer nop)
	 (number nop)
	 (function nop)
	 (array nop)
	 (object nop)))))

;*---------------------------------------------------------------------*/
;*    j2s-box ...                                                      */
;*    -------------------------------------------------------------    */
;*    This function introduces the type conversions that might be      */
;*    needed to box/unbox variable values on read.                     */
;*    -------------------------------------------------------------    */
;*    No type checking is never needed here as the boxing/unboxing     */
;*    are explicitly introduced by the compile and they are safe.      */
;*---------------------------------------------------------------------*/
(define (j2s-box expr from to)
      
   (define (default)
      (error "j2s-box"
	 (format "Cannot convert from \"~a\" to \"~a\"" from to) expr))

   (if (eq? from to)
       expr
       (let ((fen (assq from box-table)))
	  (if (pair? fen)
	      (let ((ten (assq to (cadr fen))))
		 (if (pair? ten)
		     (if (symbol? (cadr ten))
			 (if (eq? (cadr ten) 'nop)
			     expr
			     `(,(cadr ten) ,expr))
			 ((cadr ten) expr))
		     (default)))
	      (default)))))




	  
	     
