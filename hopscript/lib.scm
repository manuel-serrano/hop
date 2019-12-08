;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/lib.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:16:17 2013                          */
;*    Last change :  Fri Dec  6 08:12:22 2019 (serrano)                */
;*    Copyright   :  2013-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Hop client-side compatibility kit (share/hop-lib.js)         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_lib

   (library hop)

   (include "types.sch" "property.sch" "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_names
	   __hopscript_arithmetic
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

   (extern (macro $make-stack-cell::cell (::obj) "MAKE_STACK_CELL"))
   
   (export 
	   (js-constant-init ::obj ::obj ::JsGlobalObject)
	   (js-with-context ::obj ::bstring ::procedure)
	   (generic js-obj->jsobject ::obj ::JsGlobalObject)
	   (js-literal->jsobject::JsObject ::vector ::vector ::JsGlobalObject)
	   (js-alist->jsobject::JsObject ::pair-nil ::JsGlobalObject)
	   (js-plist->jsobject::JsObject ::pair-nil ::JsGlobalObject)
	   (generic js-jsobject->obj ::obj ::JsGlobalObject)
	   (js-jsobject->plist ::JsObject ::JsGlobalObject)
	   (js-jsobject->keyword-plist ::JsObject ::JsGlobalObject)
	   (js-jsobject->alist ::JsObject ::JsGlobalObject)
	   (js-dsssl-args->jsargs ::pair ::JsGlobalObject)
	   (js-object->keyword-arguments*::pair-nil ::JsObject ::JsGlobalObject)
	   (js-iterable->list::pair-nil ::obj ::JsGlobalObject)
	   (generic js-jsobject->jsarray ::obj ::JsGlobalObject)
	   (inline fixnums?::bool ::obj ::obj)
	   (inline make-stack-cell::cell ::obj)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-constant-init ...                                             */
;*---------------------------------------------------------------------*/
(define (js-constant-init vec-or-false str-or-vec %this)
   
   (define (constant-plain-regexp el)
      (with-access::JsGlobalObject %this (js-regexp)
	 (let* ((cnsts (vector-ref el 1))
		(flags (vector-ref el 2))
		(rx (js-string->jsstring cnsts)))
	    (if flags
		(if (eq? (vector-ref el 0) 4)
		    (js-new3 %this js-regexp rx
		       (js-string->jsstring flags)
		       (vector-ref el 3))
		    (js-new2 %this js-regexp rx
		       (js-string->jsstring flags)))
		(if (eq? (vector-ref el 0) 4)
		    (js-new2 %this js-regexp rx
		       (vector-ref el 3))
		    (js-new1 %this js-regexp rx))))))
   
   (define (constant-inline-regexp el)
      (with-access::JsGlobalObject %this (js-regexp)
	 (let* ((cnsts (vector-ref el 1))
		(flags (vector-ref el 2))
		(rx (js-string->jsstring cnsts))
		(regexp (if flags
			    (if (eq? (vector-ref el 0) 5)
				(js-new3 %this js-regexp rx
				   (js-string->jsstring flags)
				   (vector-ref el 3))
				(js-new2 %this js-regexp rx
				   (js-string->jsstring flags)))
			    (if (eq? (vector-ref el 0) 5)
				(js-new2 %this js-regexp rx
				   (vector-ref el 3))
				(js-new1 %this js-regexp rx)))))
	    (with-access::JsRegExp regexp (rx)
	       rx))))
   
   (define (constant-objinit el cnsts)
      (with-access::JsGlobalObject %this (__proto__)
	 (let* ((cmap (vector-ref cnsts (vector-ref el 1)))
		(vals (vector-ref el 2))
		(obj (js-make-jsobject (vector-length vals)
			cmap __proto__)))
	    (with-access::JsObject obj (elements)
	       (let loop ((i (-fx (vector-length vals) 1)))
		  (if (<fx i 0)
		      obj
		      (let ((val (vector-ref vals i)))
			 (vector-set! elements i
			    (cond
			       ((pair? val) (vector-ref cnsts (car val)))
			       ((string? val) (js-string->jsstring val))
			       (else val)))
			 (loop (-fx i 1)))))))))

   (let ((cnsts (if (string? str-or-vec) (string->obj str-or-vec) str-or-vec)))
      (let loop ((i 0))
	 (when (<fx i (vector-length cnsts))
	    (let ((el (vector-ref cnsts i)))
	       (cond
		  ((isa? el JsRegExp)
		   ;; patch the regexp prototype
		   (with-access::JsGlobalObject %this (js-regexp-prototype js-regexp-cmap)
		      (with-access::JsRegExp el (__proto__ cmap)
			 (set! cmap js-regexp-cmap)
			 (set! __proto__ js-regexp-prototype))))
		  ((vector? el)
		   (vector-set! cnsts i
		      (case (vector-ref el 0)
			 ((0)
			  ;; a plain string
			  (let ((str (vector-ref el 1)))
			     (js-name->jsstring str)))
			 ((6)
			  ;; an ascii name
			  (let ((str (vector-ref el 1)))
			     (js-ascii-name->jsstring str)))
			 ((7)
			  ;; an utf8 name
			  (let ((str (vector-ref el 1)))
			     (js-utf8-name->jsstring str)))
			 ((1 4)
			  ;; a plain regexp
			  (constant-plain-regexp el))
			 ((2)
			  ;; a literal cmap
			  (let ((props (vector-ref el 1)))
			     (js-strings->cmap props %this)))
			 ((3 5)
			  ;; an inlined regexp
			  (constant-inline-regexp el))
			 ((8)
			  ;; a literal object (it contains the constant indexes
			  ;; of its cmap and its elements value)
			  (constant-objinit el cnsts))))))
	       (loop (+fx i 1)))))
      ;; start fill at index 1 because of the C declaration
      ;; of the constant vector (see constants_expd.sch)
      (when vec-or-false (vector-copy! vec-or-false 1 cnsts 0))
      cnsts))

;*---------------------------------------------------------------------*/
;*    js-with-context ...                                              */
;*---------------------------------------------------------------------*/
(define (js-with-context ctx::obj ctxname::bstring thunk::procedure)
   (if (isa? ctx JsGlobalObject)
       (with-access::JsGlobalObject ctx (worker name)
	  (js-worker-exec worker name #t thunk))
       (error ctxname "Not a JavaScript context" ctx)))

;*---------------------------------------------------------------------*/
;*    js-obj->jsobject ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (js-obj->jsobject obj::obj %this::JsGlobalObject)
   (cond
      ((number? obj) (js-number->jsnumber obj))
      ((boolean? obj) obj)
      ((eq? obj #unspecified) obj)
      ((null? obj) obj)
      ((string? obj) (js-string->jsstring obj))
      ((date? obj) (js-date->jsdate obj %this))
      ((vector? obj) (js-vector->jsobject obj %this))
      ((struct? obj) (js-struct->jsobject obj %this))
      ((regexp? obj) (js-regexp->jsregexp obj %this))
      ((keyword? obj) (js-string->jsstring (keyword->string obj)))
      ((symbol? obj) (js-string->jsstring (symbol->string! obj)))
      ((pair? obj) (js-pair->jsobject obj %this))
      ((u8vector? obj) (js-u8vector->jsarraybuffer obj %this))
      ((null? obj) (js-undefined))
      ((socket? obj) (js-socket->jsobject obj %this))
      ((procedure? obj) (js-procedure->jsobject obj %this))
      (else (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-obj->jsobject ::JsObject ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-obj->jsobject obj::JsObject %this)
   obj)

;*---------------------------------------------------------------------*/
;*    js-vector->jsobject ...                                          */
;*---------------------------------------------------------------------*/
(define (js-vector->jsobject vec %this)
   (let ((vec (vector-map (lambda (o) (js-obj->jsobject o %this)) vec)))
      (js-vector->jsarray vec %this)))

;*---------------------------------------------------------------------*/
;*    js-struct->jsobject ...                                          */
;*---------------------------------------------------------------------*/
(define (js-struct->jsobject stu %this)
   (case (struct-key stu)
      ((__JsBoolean__) (js-bool->jsBoolean (struct-ref stu 0) %this))
      ((__JsNumber__) (js-number->jsNumber (struct-ref stu 0) %this))
      ((__JsCustom__) ((struct-ref stu 1) (struct-ref stu 0) %this))
      (else (js-obj->jsobject (list->vector (struct->list stu)) %this))))

;*---------------------------------------------------------------------*/
;*    js-pair->jsobject ...                                            */
;*---------------------------------------------------------------------*/
(define (js-pair->jsobject l %this)

   (define (plist? l)
      (let loop ((l l))
	 (cond
	    ((null? l)
	     #t)
	    ((and (or (keyword? (car l)) (symbol? (car l))) (pair? (cdr l)))
	     (loop (cddr l)))
	    (else
	     #f))))

   (define (alist? l)
      (when (list? l)
	 (every (lambda (e)
		   (and (pair? e)
			(or (keyword? (car e)) (symbol? (car e)))
			(not (null? (cdr (last-pair e))))))
	    l)))

   (cond
      ((plist? l)
       (js-plist->jsobject l %this))
      ((alist? l)
       (js-alist->jsobject l %this))
      ((list? l)
       (map (lambda (o) (js-obj->jsobject o %this)) l))
      (else
       (cons (js-obj->jsobject (car l) %this) (js-obj->jsobject (cdr l) %this)))))

;*---------------------------------------------------------------------*/
;*    js-literal->jsobject ...                                         */
;*    -------------------------------------------------------------    */
;*    The cmap structure is defined in property.scm.                   */
;*---------------------------------------------------------------------*/
(define (js-literal->jsobject::JsObject elements::vector names::vector %this)
   (with-access::JsGlobalObject %this (__proto__)
      (instantiateJsObject
	 (cmap (js-names->cmap names #f))
	 (__proto__ __proto__)
	 (elements elements))))

;*---------------------------------------------------------------------*/
;*    js-key-name->jsstring ...                                        */
;*---------------------------------------------------------------------*/
(define (js-key-name->jsstring s)
   (cond
      ((keyword? s) (js-name->jsstring (keyword->string! s)))
      ((string? s) (js-name->jsstring s))
      ((symbol? s) (js-name->jsstring (symbol->string! s)))
      (else s)))

;*---------------------------------------------------------------------*/
;*    js-alist->jsobject ...                                           */
;*    -------------------------------------------------------------    */
;*    The cmap structure is defined in property.scm.                   */
;*---------------------------------------------------------------------*/
(define (js-alist->jsobject alist %this)
   (with-access::JsGlobalObject %this (js-object __proto__)
      (let* ((len (length alist))
	     (props ($create-vector len))
	     (methods (make-vector len #f))
	     (cmap (instantiate::JsConstructMap
		      (inline #t)
		      (props props)
		      (methods methods)))
	     (obj (instantiateJsObject
		     (cmap cmap)
		     (__proto__ __proto__)
		     (elements ($create-vector len)))))
	 (with-access::JsObject obj (elements)
	    (let ((vec elements))
	       (let loop ((i 0)
			  (alist alist))
		  (if (=fx i len)
		      obj
		      (let* ((name (js-key-name->jsstring (caar alist)))
			     (val (js-obj->jsobject (cdar alist) %this)))
			 (vector-set! props i
			    (prop name (property-flags-default)))
			 (vector-set! vec i val)
			 (when (js-function? val)
			    (vector-set! methods i #t))
			 (loop (+fx i 1) (cdr alist))))))))))

;*---------------------------------------------------------------------*/
;*    js-plist->jsobject ...                                           */
;*---------------------------------------------------------------------*/
(define (js-plist->jsobject plist %this)
   (with-access::JsGlobalObject %this (js-object __proto__)
      (let* ((len (/fx (length plist) 2))
	     (elements ($create-vector len))
	     (props ($create-vector len))
	     (methods (make-vector len #f)))
	 (let loop ((i 0)
		    (plist plist))
	    (if (=fx i len)
		(let ((cmap (instantiate::JsConstructMap
			       (methods methods)
			       (props props))))
		   (instantiateJsObject
		      (cmap cmap)
		      (__proto__ __proto__)
		      (elements elements)))
		(let* ((name (js-key-name->jsstring (car plist)))
		       (val (js-obj->jsobject (cadr plist) %this)))
		   (vector-set! props i (prop name (property-flags-default)))
		   (vector-set! elements i val)
		   (when (js-function? val) (vector-set! methods i #t))
		   (loop (+fx i 1) (cddr plist))))))))

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
	    ((js-array? (car lst))
	     (flatten (append (xml-unpack (car lst) %this) (cdr lst)) res))
	    (else
	     (flatten (cdr lst) (cons (car lst) res))))))

   (let ((acc '()))
      (js-for-in obj
	 (lambda (k %this)
	    (let ((val (js-get/name-cache obj k %this))
		  (key (string->keyword (js-jsstring->string k))))
	       (if (js-array? val)
		   (with-access::JsArray val (vec)
		      (let ((l (flatten (vector->list vec))))
			 (if (pair? l)
			     (set! acc (append (reverse! l) (cons key acc)))
			     (set! acc (cons* '() (cons key acc))))))
		   (set! acc (cons* val key acc)))))
	 %this)
      (reverse! acc)))

;*---------------------------------------------------------------------*/
;*    js-socket->jsobject ...                                          */
;*---------------------------------------------------------------------*/
(define (js-socket->jsobject obj %this)
   (with-access::JsGlobalObject %this (__proto__)
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      (let ((sock (instantiateJsWrapper
		     (__proto__ __proto__)
		     (data #unspecified)
		     (obj obj))))
	 (js-bind! %this sock (& "hostname")
	    :value (js-string->jsstring (socket-hostname obj))
	    :writable #f :configurable #f
	    :hidden-class #t)
	 (js-bind! %this sock (& "hostAddress")
	    :value (js-string->jsstring (socket-host-address obj))
	    :writable #f :configurable #f
	    :hidden-class #t)
	 (js-bind! %this sock (& "localAddress")
	    :value (js-string->jsstring (socket-local-address obj))
	    :writable #f :configurable #f
	    :hidden-class #t)
	 (js-bind! %this sock (& "port")
	    :value (socket-port-number obj)
	    :writable #f :configurable #f
	    :hidden-class #t)
	 (js-bind! %this sock (& "ssl")
	    :value (ssl-socket? obj)
	    :writable #f :configurable #f
	    :hidden-class #t)
	 sock)))

;*---------------------------------------------------------------------*/
;*    js-procedure->jsobject ...                                       */
;*---------------------------------------------------------------------*/
(define (js-procedure->jsobject obj %this)
   (js-make-function %this obj (procedure-arity obj) "native"))

;*---------------------------------------------------------------------*/
;*    js-jsobject->obj ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (js-jsobject->obj obj %this)
   obj)

;*---------------------------------------------------------------------*/
;*    js-iterable->list ...                                            */
;*---------------------------------------------------------------------*/
(define (js-iterable->list obj %this)
   (cond
      ((js-array? obj)
       (jsarray->list obj %this))
      ((js-jsstring? obj)
       (js-jsstring->list obj %this))
      (else
       (error "js-iterable->list"
	  (format "not implemented yet \"~a\"" (typeof obj)) obj))))

;*---------------------------------------------------------------------*/
;*    js-jsobject->jsarray ::obj ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (js-jsobject->jsarray o::obj %this::JsGlobalObject)
   (if (js-jsstring? o)
       (js-jsstring->jsarray o %this)
       (js-raise-type-error %this "call: not an object ~s" o)))

;*---------------------------------------------------------------------*/
;*    js-jsobject->jsarray ::JsObject ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-jsobject->jsarray o::JsObject %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-symbol-iterator)
      (let ((fun (js-get o js-symbol-iterator %this))
	    (acc '()))
	 (if (js-function? fun)
	     (begin
		(js-for-of-iterator (js-call0 %this fun o) o
		   (lambda (e %this) (set! acc (cons e acc))) #f %this)
		(js-vector->jsarray (list->vector (reverse! acc)) %this))
	     (js-raise-type-error %this "call: not an interator ~s"
	       o)))))

;*---------------------------------------------------------------------*/
;*    js-jsobject->plist ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsobject->plist obj %this)
   (let ((args '()))
      (js-for-in obj
	 (lambda (p %this)
	    (let ((s (string->symbol (js-jsstring->string p))))
	       (set! args
		  (cons (symbol->keyword s) args))
	       (set! args
		  (cons (js-jsobject->obj
			   (js-get/name-cache obj p %this) %this)
		     args))))
	 %this)
      (reverse! args)))

;*---------------------------------------------------------------------*/
;*    js-jsobject->keyword-plist ...                                   */
;*---------------------------------------------------------------------*/
(define (js-jsobject->keyword-plist obj %this)
   (let ((args '()))
      (js-for-in obj
	 (lambda (p %this)
	    (let ((s (string->symbol (js-jsstring->string p))))
	       (set! args
		  (cons (symbol->keyword s) args))
	       (set! args
		  (cons (js-jsobject->obj
			   (js-get/name-cache obj p %this) %this)
		     args))))
	 %this)
      (reverse! args)))

;*---------------------------------------------------------------------*/
;*    js-jsobject->alist ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsobject->alist obj %this)
   (let ((args '()))
      (js-for-in obj
	 (lambda (p %this)
	    (let* ((n (js-jsstring->string p))
		   (e (cons (string->keyword n)
			 (js-jsobject->obj
			    (js-get/name-cache obj p %this) %this))))
	       (set! args (cons e args))))
	 %this)
      (reverse! args)))

;*---------------------------------------------------------------------*/
;*    fixnums? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (fixnums? a b)
   (cond-expand
      ((and bigloo-c (config nan-tagging #f))
       (pragma::bool "INTEGERP( TAG_INT == 0 ? ((long)$1 | (long)$2) : ((long)$1 & (long)$2) )" a b))
      (else
       (and (fixnum? a) (fixnum? b)))))

;*---------------------------------------------------------------------*/
;*    make-stack-cell ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (make-stack-cell v)
   ($make-stack-cell v))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

