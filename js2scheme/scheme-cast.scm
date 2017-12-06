;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-cast.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 07:13:28 2017                          */
;*    Last change :  Wed Dec  6 20:54:32 2017 (serrano)                */
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
   
   (export (j2s-cast expr::obj ::symbol ::symbol)))

;*---------------------------------------------------------------------*/
;*    cast-table ...                                                   */
;*---------------------------------------------------------------------*/
(define cast-table
   ;; from x to -> conv
   `((int32
	((int30 ,js-id)
	 (uint32 ,js-int32->uint32)
	 (integer js-int32-tointeger)
	 (number js-int32-tointeger)
	 (propname ,js-int32->propname)
	 (bool ,js-int32->bool)
	 (any js-int32-tointeger)))
     (uint32
	((uint29 ,js-id)
	 (int32 ,js-uint32->int32)
	 (int53 uint32->fixnum)
	 (integer js-uint32-tointeger)
	 (number js-uint32-tointeger)
	 (propname ,js-uint32->propname)
	 (bool ,js-uint32->bool)
	 (any js-uint32-tointeger)))
     (int53
	((int30 ,js-id)
	 (int32 js-int53-toint32)
	 (uint32 js-int53-touint32)
	 (number js-int53-tointeger)
	 (any js-int53-tointeger)))
     (bint
	((int32 fixnum->int32)
	 (uint32 fixnum->uint32)))
     (integer
	((int32 ,js-number->int32)
	 (propname ,js-id)))
     (number
	((bool js-totest)
	 (int32 ,js->int32)
	 (uint32 ,js-number->uint32)
	 (propname ,js-id)
	 (any ,js-id)))
     (string
	((propname ,js-id)))
     (any
	((propname ,js-id)
	 (bool js-totest)
	 (int32 ,js->int32)
	 (uint32 ,js->uint32)
	 (number ,js->number)))
     (bool
	((int32 ,js->int32)
	 (uint32 ,js->uint32)))))

;; cast ancillary functions
(define (js-id v)
   v)

(define (js-uint32->int32 v)
   (if (uint32? v) (uint32->int32 v) `(uint32->int32 ,v)))
(define (js-int32->uint32 v)
   (if (int32? v) (int32->uint32 v) `(int32->uint32 ,v)))

(define (js-uint32->bool v)
   `(>=u32 ,v #u32:0))
(define (js-int32->bool v)
   `(not (=s32 ,v #s32:0)))

(define (js-uint32->propname v)
   (if (and (uint32? v) (<u32 v (-u32 (bit-lshu32 #u32:1 29) #u32:1)))
       (uint32->fixnum v)
       v))

(define (js-int32->propname v)
   (if (and (int32? v)
	    (<s32 v (-s32 (bit-lshs32 #s32:1 29) #s32:1)) (>=s32 v #s32:0))
       (int32->fixnum v)
       v))

(define (js->int32 v)
   `(js-toint32 ,v %this))
(define (js->uint32 v)
   `(js-touint32 ,v %this))

(define (js-number->int32 v)
   (cond
      ((int32? v)
       v)
      ((and (fixnum? v) (=fx (int32->fixnum (fixnum->int32 v)) v))
       (fixnum->int32 v))
      (else
       (tprint "COMMENT TO BE CHECKED AND REMOVED (2017-12-06)...")
       `(js-number-toint32 ,v))))

(define (js-number->uint32 v)
   (cond
      ((uint32? v)
       v)
      ((and (fixnum? v) (=fx (uint32->fixnum (fixnum->uint32 v)) v))
       (fixnum->uint32 v))
      (else
       `(js-number-touint32 ,v))))

(define (js->number v)
   `(js-tonumber ,v %this))

;*---------------------------------------------------------------------*/
;*    j2s-cast ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-cast expr from to)
   
   (define (err)
      (error "cast" (format "illegal cast ~a -> ~a" from to) expr))
   
   (define (tostring expr)
      (match-case expr
	 ((js-jsstring-ref ?str ?idx)
	  (set-car! expr 'js-jsstring-ref-as-string)
	  expr)
	 ((js-string-ref ?str ?idx ?this)
	  (set-car! expr 'js-string-ref-as-string)
	  expr)
	 (else
	  `(js-tojsstring ,expr %this))))
   
   (define (default)
      (tprint "cast expr=" expr " from=" from " to=" to)
      (if (or (eq? from to) (eq? to '*))
	  expr
	  (case from
	     ((uint29)
	      (case to
		 ((uint32 index length) (fixnum->uint32 expr))
		 ((bool) `(>u32 ,expr #u32:0))
		 (else expr)))
	     ((index uint32 length)
	      (case to
		 ((uint32 index length) expr)
		 ((bool) `(> ,expr 0))
		 (else expr)))
	     ((int30)
	      (case to
		 ((index uint32 length) (fixnum->uint32 expr))
		 ((bool) `(not (= ,expr 0)))
		 (else expr)))
	     ((int53)
	      (case to
		 ((index uint32 length) (err))
		 ((bool) `(not (= ,expr 0)))
		 (else expr)))
	     ((fixnum)
	      (case to
		 ((index uint32 length) (js-fixnum->uint32 expr))
		 ((bool) `(not (=fx ,expr 0)))
		 (else expr)))
	     ((integer number)
	      (case to
		 ((index uint32 length) (js-fixnum->uint32 expr))
		 ((bool) `(not (= ,expr 0)))
		 (else expr)))
	     (else
	      (case to
		 ((index uint32 length) (js-fixnum->uint32 expr))
		 ((string) (tostring expr))
		 ((bool) `(js-totest ,expr))
		 (else expr))))))

   (if (eq? from to)
       expr
       (let ((fen (assq from cast-table)))
	  (if (pair? fen)
	      (let ((ten (assq to (cadr fen))))
		 (if (pair? ten)
		     (if (symbol? (cadr ten))
			 `(,(cadr ten) ,expr)
			 ((cadr ten) expr))
		     (default)))
	      (default)))))
	  

