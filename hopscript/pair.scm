;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/pair.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May 24 07:51:25 2014                          */
;*    Last change :  Fri Jun 20 09:19:04 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript JS/Hop pair binding                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_pair

   (library hop)

   (import __hopscript_types
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_function
	   __hopscript_property
	   __hopscript_array)
   
   (export  (js-get-pair o::pair prop::symbol %this::JsGlobalObject)
	    (js-get-null o::nil prop::symbol %this::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    js-get-pair ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-get-pair o::pair prop %this)
   (case prop
      ((car)
       (car o))
      ((cdr)
       (cdr o))
      ((cer)
       (if (epair? o)
	   (cer o)
	   (js-raise-type-error %this
	      (format "no such field.3 \"~a\" ~~a" (js-toname prop %this)) o)))
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
      (else
       (js-raise-type-error %this
	  (format "no such field.1 \"~a\" ~~a" (js-toname prop %this)) o))))
   
;*---------------------------------------------------------------------*/
;*    js-get-null ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-get-null o::nil prop %this)
   (case prop
      ((length)
       (js-make-function %this (lambda (this proc) 0)
	  0 'length))
      ((map)
       (js-make-function %this (lambda (this proc) '())
	  1 'map)) 
      ((forEach)
       (js-make-function %this (lambda (this proc) (js-undefined))
	  1 'forEach))
      ((assoc)
       (js-make-function %this (lambda (this key) #f)
	  1 'assoc))
      ((reverse)
       (js-make-function %this (lambda (this) '())
	  0 'reverse))
      ((concat)
       (js-make-function %this
	  (lambda (this . l) (apply append l))
	  -1 'concat))
      ((keys)
       (js-make-function %this
	  (lambda (this . l)
	     (js-vector->jsarray '#() %this))
	  0 'keys))
      ((inspect)
       (js-undefined))
      (else
       (js-raise-type-error %this
	  (format "no such field \"~a\" ~~a" (js-toname prop %this)) o))))
   

