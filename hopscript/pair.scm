;*=====================================================================*/
;*    /tmp/HOPNEW/hop/hopscript/pair.scm                               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May 24 07:51:25 2014                          */
;*    Last change :  Sun Feb 23 14:50:20 2020 (serrano)                */
;*    Copyright   :  2014-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript JS/Hop pair binding                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_pair

   (library hop)

   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_function
	   __hopscript_property
	   __hopscript_array
	   __hopscript_lib)
   
   (export  (js-init-pair! ::JsGlobalObject)
	    (js-properties-name-pair::vector ::pair ::JsGlobalObject)
	    (js-get-own-property-pair ::pair ::obj ::JsGlobalObject)
	    (js-get-property-value-pair ::pair ::pair ::obj ::JsGlobalObject)
	    (js-get-pair ::pair ::obj ::JsGlobalObject)
	    (js-get-null ::nil ::obj ::JsGlobalObject)
	    (js-put-pair! ::pair ::obj v throw::bool ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-init-pair! ...                                                */
;*---------------------------------------------------------------------*/
(define (js-init-pair! %this)
   (unless (vector? __js_strings) (set! __js_strings (&init!))))

;*---------------------------------------------------------------------*/
;*    js-properties-name-pair ...                                      */
;*---------------------------------------------------------------------*/
(define (js-properties-name-pair::vector o::pair %this)
   (if (epair? o)
       `#(,(& "car") ,(& "cdr") ,(& "cer") ,(& "toArray"))
       `#(,(& "car") ,(& "cdr") ,(& "toArray"))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property-pair ...                                     */
;*---------------------------------------------------------------------*/
(define (js-get-own-property-pair o::pair p::obj %this)
   (let ((n (js-toname p %this)))
      (case n
	 ((car)
	  (instantiate::JsValueDescriptor
	     (name (& "car"))
	     (writable #t)
	     (value (js-obj->jsobject (car o) %this))
	     (enumerable #t)
	     (configurable #f)))
	 ((cdr)
	  (instantiate::JsValueDescriptor
	     (name (& "cdr"))
	     (writable #t)
	     (value (js-obj->jsobject (cdr o) %this))
	     (enumerable #t)
	     (configurable #f)))
	 ((cer)
	  (if (epair? o)
	      (instantiate::JsValueDescriptor	
		 (name (& "cer"))
		 (writable #t)
		 (value (js-obj->jsobject (cer o) %this))
		 (enumerable #t)
		 (configurable #f))
	      (js-undefined)))
	 (else
	  (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    js-get-property-value-pair ...                                   */
;*---------------------------------------------------------------------*/
(define (js-get-property-value-pair o::pair base::pair p::obj %this)
   (let ((n (js-toname p %this)))
      (case n
	 ((eq? n (& "car"))
	  (js-obj->jsobject (car base) %this))
	 ((eq? n (& "cdr"))
	  (js-obj->jsobject (cdr base) %this))
	 ((eq? n (& "cer"))
	  (if (epair? o) (js-obj->jsobject (cer base) %this) (js-undefined)))
	 ((eq? n (& "toArray"))
	  (js-obj->jsobject base %this))
	 ((eq? n (& "nodeType"))
	  11)
	 (else
	  (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    js-get-pair ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-get-pair o::pair prop %this)
   (let ((n (js-toname prop %this)))
      (cond
	 ((eq? n (& "car"))
	  (js-obj->jsobject (car o) %this))
	 ((eq? n (& "cdr"))
	  (js-obj->jsobject (cdr o) %this))
	 ((eq? n (& "cer"))
	  (if (epair? o)
	      (js-obj->jsobject (cer o) %this)
	      (js-raise-type-error %this
		 (format "no such field \"~a\" ~~a" (js-toname prop %this)) o)))
	 ((eq? n (& "length"))
	  (js-make-function %this length
	     0 (& "length")))
	 ((eq? n (& "map"))
	  (js-make-function %this
	     (lambda (this proc)
		(map (lambda (x) (js-call1 %this proc o x)) o))
	     1 (& "map")))
	 ((eq? n (& "forEach"))
	  (js-make-function %this
	     (lambda (this proc)
		(for-each (lambda (x) (js-call1 %this proc o x)) o))
	     1 (& "forEach")))
	 ((eq? n (& "assoc"))
	  (js-make-function %this
	     (lambda (this key) (assoc o key))
	     1 (& "assoc")))
	 ((eq? n (& "reverse"))
	  (js-make-function %this reverse
	     0 (& "reverse")))
	 ((eq? n (& "concat"))
	  (js-make-function %this
	     (lambda (this . l) (apply append this l))
	     -1 (& "concat")))
	 ((eq? n (& "keys"))
	  (js-make-function %this
	     (lambda (this . l)
		(js-vector->jsarray
		   (if (epair? this)
		       (vector "car" "cdr" "cer")
		       (vector "car" "cdr"))
		   %this))
	     0 (& "keys")))
	 ((eq? n (& "toArray"))
	  (js-make-function %this
	     (lambda (this)
		(js-vector->jsarray (list->vector this) %this))
	     0 (& "toArray")))
	 ((eq? n (& "inspect"))
	  (js-undefined))
	 ((eq? n (& "nodeType"))
	  11)
	 ((eq? n (& "getElementById"))
	  (js-make-function %this
	     (lambda (this id)
		(dom-get-element-by-id this (js-tostring id %this)))
	     1 (& "getElementById")))
	 ((eq? n (& "getElementsByTagName"))
	  (js-make-function %this
	     (lambda (this tag)
		(js-vector->jsarray
		   (list->vector
		      (dom-get-elements-by-tag-name this (js-tostring tag %this)))
		   %this))
	     1 (& "getElementsByTagName")))
	 ((eq? n (& "getElementsByClassName"))
	  (js-make-function %this
	     (lambda (this tag)
		(js-vector->jsarray
		   (list->vector
		      (dom-get-elements-by-class this (js-tostring tag %this)))
		   %this))
	     1 (& "getElementsByClassName")))
	 ((eq? n (& "childNodes"))
	  (js-vector->jsarray (list->vector o) %this))
	 (else
	  (js-raise-type-error %this
	     (format "no such field \"~a\" ~~a" (js-tostring prop %this))
	     o)))))
   
;*---------------------------------------------------------------------*/
;*    js-get-null ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-get-null o::nil prop %this)
   (js-raise-type-error %this
      (format "no such field \"~a\" ~~a" (js-tostring prop %this)) o))
   
;*---------------------------------------------------------------------*/
;*    js-put-pair! ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-put-pair! o::pair prop::obj v throw::bool %this::JsGlobalObject)
   (let ((n (js-toname o %this)))
      (cond
	 ((eq? n (& "car"))
	  (set-car! o v)
	  v)
	 ((eq? n (& "cdr"))
	  (set-cdr! o v)
	  v)
	 (else
	  (if throw
	      (js-raise-type-error %this
		 "[[PUT]], read-only or unbound ~s" (js-tostring prop %this))
	      v)))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
