;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/lib.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:16:17 2013                          */
;*    Last change :  Thu Jan 15 11:45:46 2015 (serrano)                */
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
	   __hopscript_array)

   (export (js-alist->jsobject ::pair-nil ::JsGlobalObject)
	   (js-plist->jsobject ::pair-nil ::JsGlobalObject)
	   (js-jsobject->plist ::JsObject ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    javascript-plist->obj ::JsGlobalObject ...                       */
;*    -------------------------------------------------------------    */
;*    See __hop_json                                                   */
;*---------------------------------------------------------------------*/
(define-method (javascript-plist->obj %this::JsGlobalObject l)
   (js-alist->jsobject l %this))

;*---------------------------------------------------------------------*/
;*    js-alist->jsobject ...                                           */
;*---------------------------------------------------------------------*/
(define (js-alist->jsobject alist %this)
   (with-access::JsGlobalObject %this (js-object)
      (let ((obj (js-new %this js-object)))
	 (for-each (lambda (e)
		      (js-put! obj (car e) (scm->js (cdr e) %this) #f %this))
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
		   (scm->js (cadr plist) %this)
		   #f %this)
		(loop (cddr plist))))))))

;*---------------------------------------------------------------------*/
;*    js-jsobject->plist ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsobject->plist obj %this)
   (let ((args '()))
      (js-for-in obj
	 (lambda (p)
	    (let ((p (string->symbol (js-string->string p))))
	       (set! args (cons (js-get obj p %this) args))
	       (set! args (cons (symbol->keyword p) args))))
	 %this)
      args))

;*---------------------------------------------------------------------*/
;*    scm->js ...                                                      */
;*---------------------------------------------------------------------*/
(define (scm->js scm %this)
   (let loop ((scm scm))
      (cond
	 ((string? scm) (string->js-string scm))
	 ((keyword? scm) (string->js-string (keyword->string scm)))
	 ((symbol? scm) (string->js-string (symbol->string scm)))
	 ((pair? scm) (js-alist->jsobject scm %this))
	 ((int64? scm) (int64->flonum scm))
	 ((elong? scm) (elong->flonum scm))
	 ((vector? scm) (js-vector->jsarray (vector-map! loop scm) %this))
	 (else scm))))
