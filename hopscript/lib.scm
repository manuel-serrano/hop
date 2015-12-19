;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/lib.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:16:17 2013                          */
;*    Last change :  Wed Dec 16 07:47:40 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Hop client-side compatibility kit (share/hop-lib.js)         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_lib

   (library hop)

   (import __hopscript_types
	   __hopscript_property
	   __hopscript_object
	   __hopscript_function
	   __hopscript_public
	   __hopscript_worker
	   __hopscript_stringliteral
	   __hopscript_array
	   __hopscript_date
	   __hopscript_boolean
	   __hopscript_number
	   __hopscript_regexp
	   __hopscript_arraybuffer
	   __hopscript_arraybufferview)

   (export (generic js-obj->jsobject ::obj ::JsGlobalObject)
	   (js-alist->jsobject ::pair-nil ::JsGlobalObject)
	   (js-plist->jsobject ::pair-nil ::JsGlobalObject)
	   (js-jsobject->plist ::JsObject ::JsGlobalObject)
	   (js-jsobject->keyword-plist ::JsObject ::JsGlobalObject)
	   (js-jsobject->alist ::JsObject ::JsGlobalObject)
	   (js-dsssl-args->jsargs ::pair ::JsGlobalObject)
	   (js-object->keyword-arguments*::pair-nil ::JsObject ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    js-obj->jsobject ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (js-obj->jsobject obj::obj %this::JsGlobalObject)
   (cond
      ((string? obj) (js-string->jsstring obj))
      ((date? obj) (js-date->jsdate obj %this))
      ((vector? obj) (js-vector->jsobject obj %this))
      ((struct? obj) (js-struct->jsobject obj %this))
      ((int64? obj) (int64->flonum obj))
      ((elong? obj) (elong->flonum obj))
      ((regexp? obj) (js-regexp->jsregexp obj %this))
      ((keyword? obj) (js-string->jsstring (keyword->string obj)))
      ((symbol? obj) (js-string->jsstring (symbol->string obj)))
      ((pair? obj) (js-pair->jsobject obj %this))
      ((u8vector? obj) (js-u8vector->jsarraybuffer obj %this))
      ((null? obj) (js-undefined))
      (else obj)))

;*---------------------------------------------------------------------*/
;*    js-vector->jsobject ...                                          */
;*---------------------------------------------------------------------*/
(define (js-vector->jsobject vec %this)
   (vector-map! (lambda (o) (js-obj->jsobject o %this)) vec)
   (js-vector->jsarray vec %this))

;*---------------------------------------------------------------------*/
;*    js-struct->jsobject ...                                          */
;*---------------------------------------------------------------------*/
(define (js-struct->jsobject stu %this)
   (case (struct-key stu)
      ((__JsBoolean__) (js-bool->jsboolean (struct-ref stu 0) %this))
      ((__JsNumber__) (js-number->jsnumber (struct-ref stu 0) %this))
      ((__JsCustom__) ((struct-ref stu 1) (struct-ref stu 0) %this))
      (else (js-obj->jsobject (list->vector (struct->list stu)) %this))))

;*---------------------------------------------------------------------*/
;*    js-pair->jsobject ...                                            */
;*---------------------------------------------------------------------*/
(define (js-pair->jsobject l %this)
   
   (define (plist? l)
      (and (or (keyword? (car l)) (symbol? (car l))) (list? (cdr l))))

   (define (alist? l)
      (when (list? l)
	 (every (lambda (e)
		   (and (pair? e) (or (keyword? (car e)) (symbol? (car e)))))
	    l)))

   (cond
      ((plist? l)
       (js-plist->jsobject l %this))
      ((alist? l)
       (js-alist->jsobject l %this))
      (else
       (map! (lambda (o) (js-obj->jsobject o %this)) l))))
   
;*---------------------------------------------------------------------*/
;*    js-alist->jsobject ...                                           */
;*---------------------------------------------------------------------*/
(define (js-alist->jsobject alist %this)
   (with-access::JsGlobalObject %this (js-object)
      (let ((obj (js-new %this js-object)))
	 (for-each (lambda (e)
		      (js-put! obj (if (keyword? (car e))
				       (keyword->symbol (car e))
				       (car e))
			 (js-obj->jsobject (cdr e) %this)
			 #f %this))
	    alist)
	 obj)))

;*---------------------------------------------------------------------*/
;*    js-plist->jsobject ...                                           */
;*---------------------------------------------------------------------*/
(define (js-plist->jsobject plist %this)
   (with-access::JsGlobalObject %this (js-object)
      (let ((obj (js-new %this js-object)))
	 (let loop ((plist plist))
	    (cond
	       ((null? plist)
		obj)
	       ((null? (cdr plist))
		obj)
	       (else
		(js-put! obj 
		   (if (keyword? (car plist))
		       (keyword->symbol (car plist))
		       (car plist))
		   (js-obj->jsobject (cadr plist) %this)
		   #f %this)
		(loop (cddr plist))))))))

;*---------------------------------------------------------------------*/
;*    js-jsobject->plist ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsobject->plist obj %this)
   (let ((args '()))
      (js-for-in obj
	 (lambda (p)
	    (let ((p (string->symbol (js-jsstring->string p))))
	       (set! args (cons (symbol->keyword p) args))
	       (set! args (cons (js-get obj p %this) args))))
	 %this)
      (reverse! args)))

;*---------------------------------------------------------------------*/
;*    js-jsobject->keyword-plist ...                                   */
;*---------------------------------------------------------------------*/
(define (js-jsobject->keyword-plist obj %this)
   (let ((args '()))
      (js-for-in obj
	 (lambda (p)
	    (let ((p (string->symbol (js-jsstring->string p))))
	       (set! args (cons (symbol->keyword p) args))
	       (set! args (cons (js-get obj p %this) args))))
	 %this)
      (reverse! args)))

;*---------------------------------------------------------------------*/
;*    js-jsobject->alist ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsobject->alist obj %this)
   (let ((args '()))
      (js-for-in obj
	 (lambda (p)
	    (let* ((n (js-jsstring->string p))
		   (k (string->symbol n))
		   (e (cons (string->keyword n) (js-get obj k %this))))
	       (set! args (cons e args))))
	 %this)
      (reverse! args)))

;*---------------------------------------------------------------------*/
;*    js-dsssl-args->jsargs ...                                        */
;*    -------------------------------------------------------------    */
;*    Convert all dsssl values.                                        */
;*---------------------------------------------------------------------*/
(define (js-dsssl-args->jsargs args %this)
   (let loop ((as args))
      (if (null? as)
	  args
	  (begin
	     (set-car! (cdr as) (js-obj->jsobject (cadr as) %this))
	     (loop (cddr as))))))

;*---------------------------------------------------------------------*/
;*    js-object->keyword-arguments* ...                                */
;*---------------------------------------------------------------------*/
(define (js-object->keyword-arguments* obj %this)
   
   (define (flatten lst)
      (let flatten ((lst lst)
		    (res '()))
	 (cond
	    ((null? lst)
	     (reverse! res))
	    ((isa? (car lst) JsArray)
	     (flatten (append (xml-unpack (car lst)) (cdr lst)) res))
	    (else
	     (flatten (cdr lst) (cons (car lst) res))))))

   (let ((acc '()))
      (js-for-in obj
	 (lambda (k)
	    (let ((val (js-get obj k %this))
		  (key (string->keyword (js-jsstring->string k))))
	       (if (isa? val JsArray)
		   (with-access::JsArray val (vec)
		      (let ((l (flatten (vector->list vec))))
			 (if (pair? l)
			     (set! acc (append (reverse! l) (cons key acc)))
			     (set! acc (cons* '() (cons key acc))))))
		   (set! acc
		      (cons* val key acc)))))
	 %this)
      (reverse! acc)))
