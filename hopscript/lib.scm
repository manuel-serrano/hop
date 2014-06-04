;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/lib.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:16:17 2013                          */
;*    Last change :  Wed Jun  4 13:39:00 2014 (serrano)                */
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
	   __hopscript_worker)

   (export (alist->jsobject ::pair-nil)
	   (plist->jsobject ::pair-nil ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    alist->jsobject ...                                              */
;*---------------------------------------------------------------------*/
(define (alist->jsobject alist)
   (let ((%this (js-initial-global-object)))
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new %this js-object)))
	    (for-each (lambda (e)
			 (js-put! obj (car e)
			    (if (keyword? (cdr e))
				(keyword->symbol (cdr e))
				(cdr e))
			    #f %this))
	       alist)
	    obj))))

;*---------------------------------------------------------------------*/
;*    plist->jsobject ...                                              */
;*---------------------------------------------------------------------*/
(define (plist->jsobject plist %this)
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
		   (cadr plist)
		   #f %this)
		(loop (cddr plist))))))))

