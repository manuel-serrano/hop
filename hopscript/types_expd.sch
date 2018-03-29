;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/types_expd.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct 25 15:52:55 2017                          */
;*    Last change :  Thu Mar 29 08:41:24 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Types Companion macros                                           */
;*=====================================================================*/
      
;*---------------------------------------------------------------------*/
;*    define-instantiate ...                                           */
;*---------------------------------------------------------------------*/
(define-expander define-instantiate-expander
   (lambda (x e)

      (define (def-default clazz)
	 `(define (,(symbol-append 'js-instantiate- clazz '-expander) x e)
	     
	     (define builtins
		'((properties '())
		  (mode ,(if (pair? (cddr x))
			     (caddr x)
			     '(js-object-default-mode)))))
	     
	     (define (builtin? f)
		(assq (car f) builtins))
	     
	     (let* ((nobj (gensym 'nobj))
		    (id (symbol-append 'instantiate:: ',clazz))
		    (nx (list 'let
			   (list (list nobj
				    (cons id (filter (lambda (f)
							(not (builtin? f)))
						(cdr x)))))
			   (cons 'begin
			      (map (lambda (f)
				      (let ((c (assq (car f) (cdr x)))
					    (set (symbol-append
						    'js-object- (car f)
						    '-set!)))
					 (if (pair? c)
					     (list set nobj (cadr c))
					     (list set nobj (cadr f)))))
				 builtins))
			   nobj)))
		(e nx e))))

      (define (def-jsobject)
	 `(define (js-instantiate-JsObject-expander x e)
	     
	     (define builtins
		'((properties '())
		  (mode ,(if (pair? (cddr x))
			     (caddr x)
			     '(js-object-default-mode)))))
	     
	     (define (builtin? f)
		(assq (car f) builtins))

	     (match-case x
		((instantiateJsObject
		    (cmap ?cmap)
		    (__proto__ ?__proto__)
		    (elements (vector . ?elements)))
		 ;; The purpose of this special expansion is to force allocate
		 ;; the object with the JS-MAKE-JSOBJECT that creates
		 ;;; properties inline, in contrast to the regular
		 ;; constructor that allocates properties in a separate vector.
		 (let ((obj (gensym 'o))
		       (vec (gensym 'v)))
		    (e `(let* ((,obj ((@ js-make-jsobject __hopscript_public)
				      ,(length elements)
				      ,cmap
				      ,__proto__))
			       (,vec (with-access::JsObject ,obj (elements)
					elements)))
			   ,@(map (lambda (el idx)
				     `(vector-set! ,vec ,idx ,el))
				elements (iota (length elements)))
			   ,obj)
		       e)))
		((instantiateJsObject
		    (cmap ?cmap)
		    (__proto__ ?__proto__)
		    (elements (make-vector ?n)))
		 ;; The purpose of this special expansion is to force allocate
		 ;; the object with the JS-MAKE-JSOBJECT that creates
		 ;;; properties inline, in contrast to the regular
		 ;; constructor that allocates properties in a separate vector.
		 (let ((obj (gensym 'o))
		       (vec (gensym 'v)))
		    (e `(let* ((,obj ((@ js-make-jsobject __hopscript_public)
				      ,n
				      ,cmap
				      ,__proto__))
			       (,vec (with-access::JsObject ,obj (elements)
					elements)))
			   ,@(map (lambda (idx)
				     `(vector-set! ,vec ,idx (js-undefined)))
				(iota n))
			   ,obj)
		       e)))
		(else
		 (let* ((nobj (gensym 'nobj))
			(nx (list 'let
			       (list (list nobj
					(cons 'instantiate::JsObject
					   (filter (lambda (f)
							    (not (builtin? f)))
						    (cdr x)))))
			       (cons 'begin
				  (map (lambda (f)
					  (let ((c (assq (car f) (cdr x)))
						(set (symbol-append
							'js-object- (car f)
							'-set!)))
					     (if (pair? c)
						 (list set nobj (cadr c))
						 (list set nobj (cadr f)))))
				     builtins))
			       nobj)))
		    (e nx e))))))
      
      (define (def clazz)
	 (if (eq? clazz 'JsObject)
	     (def-jsobject)
	     (def-default clazz)))

      (e (def (cadr x)) e)))

;*---------------------------------------------------------------------*/
;*    define-instantiate ...                                           */
;*---------------------------------------------------------------------*/
(define-expander define-instantiate
   (lambda (x e)
      (let* ((clazz (cadr x))
	     (defe `(define-expander ,(symbol-append 'instantiate clazz)
		       ,(symbol-append 'js-instantiate- clazz '-expander))))
	 (eval `(define-instantiate-expander ,clazz))
	 (e defe  e))))

;*---------------------------------------------------------------------*/
;*    js-instantiate-JsObjectLiteral-expander ...                      */
;*    -------------------------------------------------------------    */
;*---------------------------------------------------------------------*/
(define (js-instantiate-JsObjectLiteral-expander x e)
   (match-case x
      ((instantiateJsObjectLiteral
	  (cmap ?cmap)
	  (elements (vector . ?elements))
	  (__proto__ ?__proto__))
       (let ((obj (gensym 'o))
	     (vec (gensym 'v)))
	  (e `(let* ((,obj ((@ js-make-jsobject __hopscript_public)
			    ,(length elements)
			    ,cmap
			    ,__proto__))
		     (,vec (with-access::JsObject ,obj (elements) elements)))
		 ,@(map (lambda (el idx)
			   `(vector-set! ,vec ,idx ,el))
		      elements (iota (length elements)))
		 ,obj)
	     e)))
      (else
       (e `(instantiateJsObject ,@(cdr x)) e))))
	      
;*---------------------------------------------------------------------*/
;*    constructors                                                     */
;*---------------------------------------------------------------------*/
(define-instantiate-expander JsObject)
(define-instantiate-expander JsWrapper)
(define-instantiate-expander JsGlobalObject)
(define-instantiate-expander JsArray (js-array-default-mode))
(define-instantiate-expander JsArrayBuffer)
(define-instantiate-expander JsArrayBufferView)
(define-instantiate-expander JsTypedArray)
(define-instantiate-expander JsInt8Array)
(define-instantiate-expander JsUint8Array)
(define-instantiate-expander JsInt16Array)
(define-instantiate-expander JsUint16Array)
(define-instantiate-expander JsInt32Array)
(define-instantiate-expander JsUint32Array)
(define-instantiate-expander JsFloat32Array)
(define-instantiate-expander JsFloat64Array)
(define-instantiate-expander JsDataView)
(define-instantiate-expander JsArguments)
(define-instantiate-expander JsString)
(define-instantiate-expander JsSymbol)
(define-instantiate-expander JsFunction)
(define-instantiate-expander JsService)
(define-instantiate-expander JsFunction1)
(define-instantiate-expander JsFunction2)
(define-instantiate-expander JsFunction3)
(define-instantiate-expander JsFunction4)
(define-instantiate-expander JsFunction5)
(define-instantiate-expander JsHopFrame)
(define-instantiate-expander JsServer)
(define-instantiate-expander JsNumber)
(define-instantiate-expander JsMath)
(define-instantiate-expander JsRegExp)
(define-instantiate-expander JsBoolean)
(define-instantiate-expander JsError)
(define-instantiate-expander JsDate)
(define-instantiate-expander JsJSON)
(define-instantiate-expander JsWorker)
(define-instantiate-expander JsPromise)
(define-instantiate-expander JsGenerator)
(define-instantiate-expander JsWebSocket)
(define-instantiate-expander JsWebSocketClient)
(define-instantiate-expander JsWebSocketServer)
(define-instantiate-expander JsWebSocketEvent)

