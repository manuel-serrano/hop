;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/dom.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 19 13:51:54 2015                          */
;*    Last change :  Sun Mar  3 15:07:30 2024 (serrano)                */
;*    Copyright   :  2015-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Server-side DOM API implementation                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_dom

   (library hop)
   
   (include "stringliteral.sch" "types_expd.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_string
	   __hopscript_function
	   __hopscript_number
	   __hopscript_math
	   __hopscript_boolean
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_arraybuffer
	   __hopscript_arraybufferview
	   __hopscript_date
	   __hopscript_error
	   __hopscript_json
	   __hopscript_service
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_worker
	   __hopscript_websocket
	   __hopscript_lib)

   (export (js-init-dom! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-init-dom! ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-init-dom! %this)
   (with-access::JsGlobalObject %this (js-xml-markups)
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      (set! js-xml-markups
	 (list
	    (& "classname") (& "id") (& "nodeType") (& "attributes")
	    (& "children") (& "getElementsById") (& "getElementsByTagName")
	    (& "getElementsByClassName")))))

;*---------------------------------------------------------------------*/
;*    js-inspect ::xml ...                                             */
;*---------------------------------------------------------------------*/
(define-method (js-inspect o::xml cnt)
   (js-ascii->jsstring (typeof xml)))

;*---------------------------------------------------------------------*/
;*    js-inspect ::xml-verbatim ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-inspect o::xml-verbatim cnt)
   (with-access::xml-verbatim o (data)
      (js-inspect (js-string->jsstring data) cnt)))

;*---------------------------------------------------------------------*/
;*    js-tostring ::xml-verbatim ...                                   */
;*---------------------------------------------------------------------*/
(define-method (js-tostring o::xml-verbatim %this)
   (with-access::xml-verbatim o (data)
      data))

;*---------------------------------------------------------------------*/
;*    js-inspect ::xml-tilde ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-inspect o::xml-tilde cnt)
   (with-access::xml-tilde o (%js-expression)
      (js-inspect (js-string->jsstring (format "~~{~a}" %js-expression)) cnt)))

;*---------------------------------------------------------------------*/
;*    js-tostring ::xml-tilde ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-tostring o::xml-tilde %this)
   (with-access::xml-tilde o (%js-expression)
      (format "~~{~a}" %js-expression)))

;*---------------------------------------------------------------------*/
;*    js-inspect ::xml-comment ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-inspect o::xml-comment cnt)
   (with-access::xml-comment o (data)
      (js-stringlist->jsstring (list "<!--" data "-->"))))

;*---------------------------------------------------------------------*/
;*    js-tostring ::xml-comment ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-tostring o::xml-comment %this)
   (with-access::xml-comment o (data)
      (string-append "<!--" data "-->")))

;*---------------------------------------------------------------------*/
;*    js-tostring ::xml-markup ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-tostring o::xml-markup %this)
   (xml->string o (hop-xml-backend)))

;*---------------------------------------------------------------------*/
;*    js-inspect ::xml-markup ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-inspect o::xml-markup cnt)
   (js-string->jsstring (xml->string o (hop-xml-backend))))

;*---------------------------------------------------------------------*/
;*    js-inspect ::xml-tilde ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-inspect o::xml-tilde cnt)
   (js-string->jsstring
      (with-access::xml-tilde o (%js-statement)
	 (call-with-output-string
	    (lambda (op)
	       (display "~{" op)
	       (display %js-statement op)
	       (display " }" op))))))

;*---------------------------------------------------------------------*/
;*    js-get ::xml-html ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-get o::xml-html prop %this::JsGlobalObject)
   (let ((name (js-toname prop %this)))
      (cond
	 ((eq? name (& "body"))
	  (with-access::xml-markup o (body)
	     (js-vector->jsarray
		(list->vector
		   (map (lambda (o)
			   (if (string? o)
			       (js-string->jsstring o)
			       o))
		      body))
		%this)))
	 ((eq? name (& "createTextNode"))
	  (js-make-function %this
	     (lambda (this txt)
		(instantiate::xml-verbatim
		   (data (js-tostring txt %this))))
	     (js-function-arity 1 0)
	     (js-function-info :name "createTextNode" :len 1)))
	 ((eq? name (& "createComment"))
	  (js-make-function %this
	     (lambda (this txt)
		(instantiate::xml-comment
		   (data (js-tostring txt %this))))
	     (js-function-arity 1 0)
	     (js-function-info :name "createComment" :len 1)))
	 ((eq? name (& "createElement"))
	  (js-make-function %this
	     (lambda (this txt)
		(instantiate::xml-element
		   (tag (string->symbol (js-tostring txt %this)))
		   (body '())))
	     (js-function-arity 1 0)
	     (js-function-info :name "createElement" :len 1)))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-put! ::xml-html ...                                           */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::xml-html prop v throw::bool %this::JsGlobalObject)
   (let ((name (js-toname prop %this)))
      (cond
	 ((eq? name (& "body"))
	  #f)
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-get ::xml-document ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-get o::xml-document prop %this::JsGlobalObject)
   (let ((name (js-toname prop %this)))
      (cond
	 ((eq? name (& "id"))
	  (with-access::xml-document o (id)
	     (js-string->jsstring id)))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-put! ::xml-document ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::xml-document prop v throw::bool %this::JsGlobalObject)
   (let ((name (js-toname prop %this)))
      (cond
	 ((eq? name (& "id"))
	  (with-access::xml-document o (id)
	     (set! id (js-tostring v %this))))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-get ::xml-verbatim ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-get o::xml-verbatim prop %this::JsGlobalObject)
   (let ((name (js-toname prop %this)))
      (cond
	 ((eq? name (& "data"))
	  (with-access::xml-verbatim o (data)
	     (js-string->jsstring data)))
	 ((eq? name (& "toString"))
	  (js-make-function %this
	     (lambda (this txt)
		(js-string->jsstring (js-tostring o %this)))
	     (js-function-arity 0 0)
	     (js-function-info :name "toString" :len 0)))
	 ((eq? name (& "inspect"))
	  (js-make-function %this js-inspect
	     (js-function-arity js-inspect)
	     (js-function-info :name "inspect" :len 1)))
	 ((eq? name (& "nodeType"))
	  3)
	 ((eq? name (& "parentNode"))
	  (with-access::xml-verbatim o (parent) parent))
	 ((eq? name (& "nextSibling"))
	  (dom-next-sibling o))
	 ((eq? name (& "previousSibling"))
	  (dom-previous-sibling o))
	 ((eq? name (& "childNodes"))
	  (js-empty-vector->jsarray %this))
	 ((eq? name (& "innerHTML"))
	  (with-access::xml-verbatim o (data)
	     (js-string->jsstring data)))
	 (else
	  (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    js-put! ::xml-verbatim ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::xml-verbatim prop v throw::bool %this::JsGlobalObject)
   (let ((name (js-toname prop %this)))
      (cond
	 ((eq? name (& "data"))
	  (with-access::xml-verbatim o (data)
	     (set! data (js-tostring v %this))))
	 (else
	  (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    js-get ::xml-comment ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-get o::xml-comment prop %this::JsGlobalObject)
   (let ((name (js-toname prop %this)))
      (cond
	 ((eq? name (& "nodeType"))
	  8)
	 ((eq? name (& "inspect"))
	  (js-make-function %this js-inspect
	     (js-function-arity js-inspect)
	     (js-function-info :name "inspect" :len 1)))
	 ((eq? name (& "toString"))
	  (js-make-function %this
	     (lambda (this txt)
		(js-string->jsstring (js-tostring o %this)))
	     (js-function-arity 0 0)
	     (js-function-info :name "toString" :len 0)))
	 ((eq? name (& "parentNode"))
	  (with-access::xml-comment o (parent) parent))
	 ((eq? name (& "nextSibling"))
	  (dom-next-sibling o))
	 ((eq? name (& "previousSibling"))
	  (dom-previous-sibling o))
	 (else
	  (js-undefined)))))
   
;*---------------------------------------------------------------------*/
;*    js-get ::xml-markup ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-get o::xml-markup prop %this::JsGlobalObject)
   (let loop ((pname (js-toname prop %this)))
      (cond
	 ((eq? pname (& "tagName"))
	  (with-access::xml-markup o (tag)
	     (js-string->jsstring (symbol->string! tag))))
	 ((eq? pname (& "attributes"))
	  (with-access::xml-markup o (attributes)
	     (js-plist->jsobject attributes %this)))
	 ((eq? pname (& "children"))
	  (with-access::xml-markup o (body)
	     body))
	 ((eq? pname (& "inspect"))
	  (js-make-function %this js-inspect
	     (js-function-arity js-inspect)
	     (js-function-info :name "inspect" :len 1)))
	 ((eq? pname (& "constructor"))
	  (js-undefined))
	 ((eq? pname (& "toString"))
	  (js-make-function %this
	     (lambda (this)
		(js-string->jsstring (js-tostring o %this)))
	     (js-function-arity 0 0)
	     (js-function-info :name "toString" :len 0)))
	 ((eq? pname (& "getElementById"))
	  (js-make-function %this
	     (lambda (this id)
		(dom-get-element-by-id this (js-tostring id %this)))
	     (js-function-arity 1 0)
	     (js-function-info :name "getElementById" :len 1)))
	 ((eq? pname (& "getElementsByTagName"))
	  (js-make-function %this
	     (lambda (this tag)
		(js-vector->jsarray
		   (list->vector
		      (dom-get-elements-by-tag-name this
			 (js-tostring tag %this)))
		   %this))
	     (js-function-arity 1 0)
	     (js-function-info :name "getElementsByTagName" :len 1)))
	 ((eq? pname (& "getElementsByClassName"))
	  (js-make-function %this
	     (lambda (this clazz)
		(js-vector->jsarray
		   (list->vector
		      (dom-get-elements-by-class this
			 (js-tostring clazz %this)))
		   %this))
	     (js-function-arity 1 0)
	     (js-function-info :name "getElementsByClassName" :len 1)))
	 ((eq? pname (& "appendChild"))
	  (js-make-function %this
	     (lambda (this child)
		(dom-append-child! this child))
	     (js-function-arity 1 0)
	     (js-function-info :name "appendChild" :len 1)))
	 ((eq? pname (& "removeChild"))
	  (js-make-function %this
	     (lambda (this child)
		(dom-remove-child! this child))
	     (js-function-arity 1 0)
	     (js-function-info :name "removeChild" :len 1)))
	 ((eq? pname (& "previousSibling"))
	  (dom-previous-sibling o))
	 ((eq? pname (& "childNodes"))
	  (with-access::xml-markup o (body)
	     (js-vector->jsarray
		(list->vector
		   (map (lambda (o)
			   (cond
			      ((string? o)
			       (instantiate::xml-verbatim
				  (parent o)
				  (data o)))
			      ((js-jsstring? o)
			       (instantiate::xml-verbatim
				  (parent o)
				  (data (js-jsstring->string o))))
			      (else
			       o)))
		      body))
		%this)))
	 ((eq? pname (& "firstChild"))
	  (with-access::xml-markup o (body)
	     (if (null? body)
		 (js-undefined)
		 (let ((o (car body)))
		    (cond
		       ((string? o)
			(instantiate::xml-verbatim
			   (parent o)
			   (data o)))
		       ((js-jsstring? o)
			(instantiate::xml-verbatim
			   (parent o)
			   (data (js-jsstring->string o))))
		       (else
			o))))))
	 ((eq? pname (& "lastChild"))
	  (with-access::xml-markup o (body)
	     (if (null? body)
		 (js-undefined)
		 (let ((o (car (last-pair body))))
		    (cond
		       ((string? o)
			(instantiate::xml-verbatim
			   (parent o)
			   (data o)))
		       ((js-jsstring? o)
			(instantiate::xml-verbatim
			   (parent o)
			   (data (js-jsstring->string o))))
		       (else
			o))))))
	 (else
	  (with-access::xml-markup o (attributes)
	     (let ((name (if (eq? pname (& "className"))
			     "class"
			     (js-jsstring->string pname))))
		(if (dom-has-attribute? o name)
		    (let ((v (dom-get-attribute o name)))
		       (if (string? v)
			   (js-string->jsstring v)
			   v))
		    (js-undefined))))))))

;*---------------------------------------------------------------------*/
;*    js-put! ::xml-markup ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::xml-markup prop v throw::bool %this::JsGlobalObject)
   
   (define (->obj v)
      (cond
	 ((js-jsstring? v) (js-jsstring->string v))
	 (else v)))
   
   (let loop ((pname (js-toname prop %this)))
      (cond
	 ((eq? pname (& "className"))
	  (loop 'class))
	 ((eq? pname (& "attributes"))
	  (when (js-object? v)
	     (with-access::xml-markup o (attributes)
		(set! attributes (js-jsobject->keyword-plist v %this)))))
	 ((eq? pname (& "childNodes"))
	  (with-access::xml-markup o (body)
	     (if (js-array? v)
		 (set! body (jsarray->list v %this))
		 (js-raise-type-error %this
		    (format "Bad children (~a)" (typeof v))
		    v))))
	 (else
	  (let ((val (js-jsstring->string pname)))
	     (dom-set-attribute! o val (->obj v)))))))

;*---------------------------------------------------------------------*/
;*    js-get ::xml-element ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-get o::xml-element prop %this::JsGlobalObject)
   (let ((name (js-toname prop %this)))
      (cond
	 ((eq? name (& "id"))
	  (with-access::xml-element o (id)
	     (if (string? id)
		 (js-string->jsstring id)
		 (js-undefined))))
	 ((eq? name (& "nodeType"))
	  1)
	 ((eq? name (& "parentNode"))
	  (with-access::xml-element o (parent) parent))
	 ((eq? name (& "outerHTML"))
	  (js-string->jsstring (js-tostring o %this)))
	 ((eq? name (& "innerHTML"))
	  (with-access::xml-element o (body)
	     (js-stringlist->jsstring
		(map (lambda (b)
			(if (string? b) b (js-tostring b %this)))
		   body))))
	 (else
	  (with-access::xml-element o (attributes)
	     (let* ((k (string->keyword (js-tostring name %this)))
		    (c (memq k attributes)))
		(if (pair? c)
		    (js-obj->jsobject (cadr c) %this)
		    (call-next-method))))))))
   
;*---------------------------------------------------------------------*/
;*    js-put! ::xml-element ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::xml-element prop v throw::bool %this::JsGlobalObject)
   (let ((name (js-toname prop %this)))
      (cond
	 ((eq? name (& "id"))
	  (with-access::xml-element o (id)
	     (set! id (js-tostring v %this))))
	 ((eq? name (& "attributes"))
	  (when (js-object? v)
	     (let* ((attrs (js-jsobject->keyword-plist v %this))
		    (lid (memq id: attrs)))
		(with-access::xml-element o (attributes id)
		   (if lid
		       (begin
			  (set! id (car lid))
			  (set! lid (remq! (cadr lid) lid))
			  (set! attributes (remq! lid (car lid))))
		       (set! attributes attrs))))))
	 ((eq? name (& "childNodes"))
	  #f)
	 ((eq? name (& "parentNode"))
	  #f)
	 ((eq? name (& "outerHTML"))
	  #f)
	 ((eq? name (& "innerHTML"))
	  (call-with-input-string (js-tostring v %this)
	     (lambda (ip)
		(let ((xml (%js-eval ip 'repl %this o %this)))
		   (with-access::xml-element o (body)
		      (cond
			 ((isa? xml xml-element)
			  (set! body (list xml)))
			 ((js-array? xml)
			  (set! body (jsarray->list xml %this)))
			 (else
			  (js-raise-type-error %this
			     (format "Bad html value (~a)" (typeof xml))
			     xml))))))))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::xml-comment ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::xml-comment name %this)
   (or (eq? name (& "nodeType")) (eq? name (& "data"))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::xml-verbatim ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::xml-verbatim name %this)
   (or (eq? name (& "nodeType")) (eq? name (& "data"))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::xml-markup ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::xml-markup name %this)
   (with-access::JsGlobalObject %this (js-xml-markups)
      (or (member name js-xml-markups)
	  (let ((k (string->keyword (js-jsstring->string name))))
	     (with-access::xml-markup o (attributes)
		(let loop ((attributes attributes))
		   (cond
		      ((null? attributes) #f)
		      ((eq? k (car attributes)) #t)
		      (else (loop (cddr attributes))))))))))

;*---------------------------------------------------------------------*/
;*    base-properties-name ...                                         */
;*---------------------------------------------------------------------*/
(define base-properties-name #f)

;*---------------------------------------------------------------------*/
;*    js-properties-names ::xml-markup ...                             */
;*---------------------------------------------------------------------*/
(define-method (js-properties-names o::xml-markup enump %this)
   (with-access::xml-markup o (attributes)
      (let loop ((attributes attributes)
		 (attrs `(,(& "nodeType")
			  ,(& "tagName")
			  ,(& "attributes")
			  ,(& "className")
			  ,(& "attributes")
			  ,(& "childNodes")
			  ,(& "parentNode")
			  ,(& "getElementById")
			  ,(& "getElementsByTagName")
			  ,(& "getElementsByClassName")
			  ,(& "appendChild")
			  ,(& "removeChild"))))
	 (cond
	    ((null? attributes)
	     (if (or (isa? o xml-element) (isa? o xml-html))
		 (cons (& "id") attrs)
		 attrs))
	    (else
	     (loop (cddr attributes)
		(cons (js-string->jsstring (keyword->string! (car attributes)))
		   attrs)))))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ::xml-markup ...                             */
;*---------------------------------------------------------------------*/
(define-method (js-has-own-property o::xml-markup p %this::JsGlobalObject)
   (not (eq? (js-get-own-property o p %this) (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::xml-markup ...                             */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::xml-markup p::obj %this::JsGlobalObject)
   (let ((n (js-toname p %this)))
      (if (js-has-property o n %this)
	  (instantiate::JsValueDescriptor
	     (name n)
	     (writable #t)
	     (value (js-get o n %this))
	     (enumerable #t)
	     (configurable #f))
	  (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property-descriptor ::xml-markup ...                  */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property-descriptor o::xml-markup p::obj %this::JsGlobalObject)
   (let ((n (js-toname p %this)))
      (if (js-has-property o n %this)
	  (js-property-descriptor %this #t
	     :writable #t
	     :value (js-get o n %this)
	     :enumerable #t
	     :configurable #f)
	  (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-get-property-value ::xml-markup ...                           */
;*---------------------------------------------------------------------*/
(define-method (js-get-property-value o::xml-markup base p %this::JsGlobalObject)
   (let ((n (js-toname p %this)))
      (if (js-has-property o n %this)
	  (js-get o n %this)
	  (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::xml-markup ...                                       */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::xml-markup proc %this)
   (with-access::xml-markup o (id attributes)
      (let loop ((attributes attributes))
	 (when (pair? attributes)
	    (if (eq? (car attributes) 'class:)
		(proc (js-string->jsstring "className") %this)
		(proc (js-string->jsstring (keyword->string (car attributes))) %this))
	    (loop (cddr attributes))))))
	 
;*---------------------------------------------------------------------*/
;*    js-properties-names ::xml-html ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-properties-names o::xml-html enump::bool %this)
   (with-access::xml-markup o (attributes)
      `(,(& "createTextNode")
	,(& "createElement"))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::xml-element ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::xml-element proc %this)
   (with-access::xml-markup o (id)
      (proc (& "id") %this)
      (call-next-method)))
	 
;*---------------------------------------------------------------------*/
;*    js-for-in ::xml-html ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::xml-html proc %this)
   (with-access::xml-markup o (id)
      (proc (& "id") %this)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
