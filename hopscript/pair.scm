;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/pair.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May 24 07:51:25 2014                          */
;*    Last change :  Wed Dec 13 20:10:53 2017 (serrano)                */
;*    Copyright   :  2014-17 Manuel Serrano                            */
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
   
   (export  (js-properties-name-pair::vector ::pair ::JsGlobalObject)
	    (js-get-own-property-pair ::pair ::obj ::JsGlobalObject)
	    (js-get-property-value-pair ::pair ::pair ::obj ::JsGlobalObject)
	    (js-get-pair ::pair ::obj ::JsGlobalObject)
	    (js-get-null ::nil ::obj ::JsGlobalObject)
	    (js-put-pair! ::pair ::obj v throw::bool ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    js-properties-name-pair ...                                      */
;*---------------------------------------------------------------------*/
(define (js-properties-name-pair::vector o::pair %this)
   (if (epair? o)
       `#(,(js-string->jsstring "car") ,(js-string->jsstring "cdr") ,(js-string->jsstring "cer") ,(js-string->jsstring "toArray"))
       `#(,(js-string->jsstring "car") ,(js-string->jsstring "cdr") ,(js-string->jsstring "toArray"))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property-pair ...                                     */
;*---------------------------------------------------------------------*/
(define (js-get-own-property-pair o::pair p::obj %this)
   (let ((n (js-toname p %this)))
      (case n
	 ((car)
	  (instantiate::JsValueDescriptor
	     (name 'car)
	     (writable #t)
	     (value (js-obj->jsobject (car o) %this))
	     (enumerable #t)
	     (configurable #f)))
	 ((cdr)
	  (instantiate::JsValueDescriptor
	     (name 'cdr)
	     (writable #t)
	     (value (js-obj->jsobject (cdr o) %this))
	     (enumerable #t)
	     (configurable #f)))
	 ((cer)
	  (if (epair? o)
	      (instantiate::JsValueDescriptor	
		 (name 'cer)
		 (writable #t)
		 (value (js-obj->jsobject (cer o) %this))
		 (enumerable #t)
		 (configurable #f))
	      (js-undefined)))
	 ((toArray)
	  (if (epair? o)
	      (instantiate::JsValueDescriptor	
		 (name 'cer)
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
	 ((car) (js-obj->jsobject (car base) %this))
	 ((cdr) (js-obj->jsobject (cdr base) %this))
	 ((cer) (if (epair? o) (js-obj->jsobject (cer base) %this) (js-undefined)))
	 ((toArray) (js-obj->jsobject base %this))
	 ((nodeType) 11)
	 (else (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    js-get-pair ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-get-pair o::pair prop %this)
   (case prop
      ((car)
       (js-obj->jsobject (car o) %this))
      ((cdr)
       (js-obj->jsobject (cdr o) %this))
      ((cer)
       (if (epair? o)
	   (js-obj->jsobject (cer o) %this)
	   (js-raise-type-error %this
	      (format "no such field \"~a\" ~~a" (js-toname prop %this)) o)))
      ((length)
       (js-make-function %this length
	  0 'length))
      ((map)
       (js-make-function %this
	  (lambda (this proc)
	     (map (lambda (x) (js-call1 %this proc o x)) o))
	  1 'map))
      ((forEach)
       (js-make-function %this
	  (lambda (this proc)
	     (for-each (lambda (x) (js-call1 %this proc o x)) o))
	  1 'forEach))
      ((assoc)
       (js-make-function %this
	  (lambda (this key) (assoc o key))
	  1 'assoc))
      ((reverse)
       (js-make-function %this reverse
	  0 'reverse))
      ((concat)
       (js-make-function %this
	  (lambda (this . l) (apply append this l))
	  -1 'concat))
      ((keys)
       (js-make-function %this
	  (lambda (this . l)
	     (js-vector->jsarray
		(if (epair? this)
		    (vector "car" "cdr" "cer")
		    (vector "car" "cdr"))
		%this))
	  0 'keys))
      ((toArray)
       (js-make-function %this
	  (lambda (this)
	     (js-vector->jsarray (list->vector this) %this))
	  0 'toArray))
      ((inspect)
       (js-undefined))
      ((nodeType)
       11)
      ((getElementById)
       (js-make-function %this
	  (lambda (this id)
	     (dom-get-element-by-id this (js-tostring id %this)))
	  1 'getElementById))
      ((getElementsByTagName)
       (js-make-function %this
	  (lambda (this tag)
	     (js-vector->jsarray
		(list->vector
		   (dom-get-elements-by-tag-name this (js-tostring tag %this)))
		%this))
	  1 'getElementsByTagName))
      ((getElementsByClassName)
       (js-make-function %this
	  (lambda (this tag)
	     (js-vector->jsarray
		(list->vector
		   (dom-get-elements-by-class this (js-tostring tag %this)))
		%this))
	  1 'getElementsByClassName))
      ((childNodes)
       (js-vector->jsarray (list->vector o) %this))
      (else
       (js-raise-type-error %this
	  (format "no such field \"~a\" ~~a" (js-toname prop %this)) o))))
   
;*---------------------------------------------------------------------*/
;*    js-get-null ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-get-null o::nil prop %this)
   (js-raise-type-error %this (format "no such field \"~a\" ~~a" prop) o))
   
;*---------------------------------------------------------------------*/
;*    js-put-pair! ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-put-pair! o::pair prop::obj v throw::bool %this::JsGlobalObject)
   (case prop
      ((car)
       (set-car! o v)
       v)
      ((cdr)
       (set-cdr! o v)
       v)
      (else
       (if throw
	   (js-raise-type-error %this
	      "[[PUT]], read-only or unbound ~s" (js-toname prop %this))
	   v))))
      
;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
