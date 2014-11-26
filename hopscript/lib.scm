;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/lib.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:16:17 2013                          */
;*    Last change :  Mon Nov 24 17:24:32 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
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
	   __hopscript_stringliteral)

   (export (js-alist->jsobject ::pair-nil ::JsGlobalObject)
	   (js-plist->jsobject ::pair-nil ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    js-alist->jsobject ...                                           */
;*---------------------------------------------------------------------*/
(define (js-alist->jsobject alist %this)
   (with-access::JsGlobalObject %this (js-object)
      (let ((obj (js-new %this js-object)))
	 (for-each (lambda (e)
		      (js-put! obj (car e)
			 (cond
			    ((string? (cdr e))
			     (string->js-string (cdr e)))
			    ((keyword? (cdr e))
			     (string->js-string
				(keyword->string (cdr e))))
			    ((symbol? (cdr e))
			     (string->js-string
				(symbol->string (cdr e))))
			    ((pair? (cdr e))
			     (js-alist->jsobject (cdr e) %this))
			    ((int64? (cdr e))
			     (int64->flonum (cdr e)))
			    ((elong? (cdr e))
			     (elong->flonum (cdr e)))
			    (else
			     (cdr e)))
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
		   (if (string? (cadr plist))
		       (string->js-string (cadr plist))
		       (cadr plist))
		   #f %this)
		(loop (cddr plist))))))))

