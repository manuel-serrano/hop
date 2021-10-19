;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/types_expd.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct 25 15:52:55 2017                          */
;*    Last change :  Mon Oct 18 18:25:03 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
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

	     (define (epairify nx x)
		(if (epair? x)
		    (econs (car nx) (cdr nx) (cer x))
		    nx))
	     
	     (define (builtin-name id)
		(if (eq? id '__proto__) 'proto id))

	     (define builtins
		'((mode ,(if (pair? (cddr x))
			     (caddr x)
			     '(bit-andu32 (js-object-default-mode)
			       (bit-notu32 (JS-OBJECT-MODE-INLINE)))))
		  (__proto__ ,(if (pair? (cddr x))
				  (caddr x)
				  '(js-null)))))
	     
	     (define (builtin? f)
		(assq (car f) builtins))
	     
	     (let* ((nobj (gensym 'nobj))
		    (id (symbol-append 'instantiate:: ',clazz))
		    (nx (list 'let
			   (list (list nobj
				    (cons id
				       (filter (lambda (f)
						  (not (builtin? f)))
					  (cdr x)))))
			   (cons 'begin
			      (map (lambda (f)
				      (let ((c (assq (car f) (cdr x)))
					    (set (symbol-append
						    'js-object-
						    (builtin-name (car f))
						    '-set!)))
					 (if (pair? c)
					     (list set nobj (cadr c))
					     (list set nobj (cadr f)))))
				 builtins))
			   nobj)))
		(e (epairify nx x) e))))

      (define (def-jsobject)
	 `(define (js-instantiate-JsObject-expander x e)

	     (define (epairify nx x)
		(if (epair? x)
		    (econs (car nx) (cdr nx) (cer x))
		    nx))
	     
	     (define (builtin-name id)
		(if (eq? id '__proto__) 'proto id))

	     (define builtins
		'((mode ,(if (pair? (cddr x))
			     (caddr x)
			     '(js-object-default-mode)))
		  (__proto__ ,(if (pair? (cddr x))
				  (caddr x)
				  '(js-null)))))
	     
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
		    (e (epairify
			  `(let* ((,obj ((@ js-make-jsobject __hopscript_types)
					 ,(length elements)
					 ,cmap
					 ,__proto__))
				  (,vec (js-object-alloc-elements ,obj)))
			      ,@(map (lambda (el idx)
					`(vector-set! ,vec ,idx ,el))
				   elements (iota (length elements)))
			      ,obj)
			  x)
		       e)))
		((instantiateJsObject
		    (cmap ?cmap)
		    (__proto__ ?__proto__)
		    (elements (subvector ?ctorsize . ?elements)))
		 ;; The purpose of this special expansion is to force allocate
		 ;; the object with the JS-MAKE-JSOBJECT that creates
		 ;;; properties inline, in contrast to the regular
		 ;; constructor that allocates properties in a separate vector.
		 (let ((obj (gensym 'o))
		       (vec (gensym 'v)))
		    (e (epairify
			  `(let* ((,obj ((@ js-make-jsobject __hopscript_types)
					 ,ctorsize
					 ,cmap
					 ,__proto__))
				  (,vec (js-object-alloc-elements ,obj)))
			      ,@(map (lambda (el idx)
					`(vector-set! ,vec ,idx ,el))
				   elements (iota (length elements)))
			      ,obj)
			  x)
		       e)))
		((instantiateJsObject
		    (cmap ?cmap)
		    (__proto__ ?__proto__)
		    (elements (make-vector (and (? integer?) ?n))))
		 ;; The purpose of this special expansion is to force allocate
		 ;; the object with the JS-MAKE-JSOBJECT that creates
		 ;;; properties inline, in contrast to the regular
		 ;; constructor that allocates properties in a separate vector.
		 (let ((obj (gensym 'o))
		       (vec (gensym 'v)))
		    (e (epairify
			  `(let* ((,obj ((@ js-make-jsobject __hopscript_types)
					 ,n
					 ,cmap
					 ,__proto__))
				  (,vec (js-object-alloc-elements ,obj)))
			      ,@(map (lambda (idx)
					`(vector-set! ,vec ,idx (js-undefined)))
				   (iota n))
			      ,obj)
			  x)
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
		    (e `(let* ((,obj ((@ js-make-jsobject __hopscript_types)
				      ,n
				      ,cmap
				      ,__proto__))
			       (,vec (js-object-alloc-elements ,obj)))
			   (vector-fill! ,vec (js-undefined))
			   ,obj)
		       e)))
		((instantiateJsObject
		    (cmap ?cmap)
		    (__proto__ ?__proto__)
		    (elements (make-vector ?n ?init)))
		 ;; The purpose of this special expansion is to force allocate
		 ;; the object with the JS-MAKE-JSOBJECT that creates
		 ;;; properties inline, in contrast to the regular
		 ;; constructor that allocates properties in a separate vector.
		 (let ((obj (gensym 'o))
		       (vec (gensym 'v))
		       (val (gensym 'i)))
		    (e (epairify
			  `(let* ((,obj ((@ js-make-jsobject __hopscript_types)
					 ,n
					 ,cmap
					 ,__proto__))
				  (,val ,init)
				  (,vec (js-object-alloc-elements ,obj)))
			      ,@(map (lambda (idx)
					`(vector-set! ,vec ,idx ,val))
				   (iota n))
			      ,obj)
			  x)
		       e)))
		((instantiateJsObject
		    (cmap ?cmap)
		    (__proto__ ?__proto__)
		    (elements ($create-vector ?n)))
		 ;; The purpose of this special expansion is to force allocate
		 ;; the object with the JS-MAKE-JSOBJECT that creates
		 ;;; properties inline, in contrast to the regular
		 ;; constructor that allocates properties in a separate vector.
		 (let ((obj (gensym 'o))
		       (vec (gensym 'v)))
		    (e (epairify
			  `(let* ((,obj ((@ js-make-jsobject __hopscript_types)
					 ,n
					 ,cmap
					 ,__proto__)))
			      ,obj)
			  x)
		       e)))
		((instantiateJsObject
		    (__proto__ ?__proto__)
		    (elements ($create-vector ?n)))
		 ;; The purpose of this special expansion is to force allocate
		 ;; the object with the JS-MAKE-JSOBJECT that creates
		 ;;; properties inline, in contrast to the regular
		 ;; constructor that allocates properties in a separate vector.
		 (let ((obj (gensym 'o))
		       (vec (gensym 'v)))
		    (e (epairify
			  `(let* ((,obj ((@ js-make-jsobject __hopscript_types)
					 ,n
					 (js-make-jsconstructmap)
					 ,__proto__)))
			      ,obj)
			  x)
		       e)))
		(else
		 (let* ((nobj (gensym 'nobj))
			(nx (list 'let
			       (list (list nobj
					(cons 'instantiate::JsObject
					   (append (if (assq 'cmap (cdr x))
						       '()
						       '((cmap (js-make-jsconstructmap))))
					      (filter (lambda (f)
							 (not (builtin? f)))
						 (cdr x))))))
			       (cons* 'begin
				  `(%object-widening-set! ,nobj '())
				  (map (lambda (f)
					  (let ((c (assq (car f) (cdr x)))
						(set (symbol-append
							'js-object- (builtin-name (car f))
							'-set!)))
					     (if (pair? c)
						 (list set nobj (cadr c))
						 (list set nobj (cadr f)))))
				     builtins))
			       `(js-object-mode-inline-set! ,nobj #f)
			       nobj)))
		    (e (epairify nx x) e))))))

      (define (def-jsgenerator)
	 `(define (js-instantiate-JsGenerator-expander x e)

	     (define (epairify nx x)
		(if (epair? x)
		    (econs (car nx) (cdr nx) (cer x))
		    nx))
	     
	     (match-case x
		((instantiateJsGenerator
		    (cmap ?cmap)
		    (elements '#())
		    (__proto__ ?proto)
		    (%next ?next)
		    (%env (if (>fx ?size 0) (make-vector ?size) '#())))
		 (let ((nx `($js-make-jsgenerator ,cmap ,proto ,size ,next
			       (bit-andu32 (js-object-default-mode)
				  (bit-notu32 (JS-OBJECT-MODE-INLINE))))))
		    (e (epairify nx x) e)))
		((instantiateJsGenerator
		    (cmap ?cmap)
		    (elements ?elements)
		    (__proto__ ?proto)
		    (%next ?next)
		    (%env ?env))
		 (warning "instantiateJsGenerator" "unrecognized form" x)
		 (e `(let ((o (instantiate::JsGenerator
				 (cmap ,cmap)
				 (elements ,elements)
				 (%next ,next)
				 (%env ,env))))
			(js-object-proto-set! o ,proto)
			o)
		    e))
		(else
		 (error "def-jsgenerator" "bad form" x)))))
      
      (define (def clazz)
	 (cond
	    ((eq? clazz 'JsObject)
	     (def-jsobject))
	    ((eq? clazz 'JsGenerator)
	     (def-jsgenerator))
	    (else
	     (def-default clazz))))

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
	 (e defe e))))

;*---------------------------------------------------------------------*/
;*    js-instantiate-JsObjectLiteral-expander ...                      */
;*---------------------------------------------------------------------*/
(define (js-instantiate-JsObjectLiteral-expander x e)
   (match-case x
      ((instantiateJsObjectLiteral
	  (cmap ?cmap)
	  (elements (vector . ?elements))
	  (__proto__ ?__proto__))
       (let ((obj (gensym 'o))
	     (vec (gensym 'v)))
	  (e `(let* ((,obj ((@ js-make-jsobject __hopscript_types)
			    ,(length elements)
			    ,cmap
			    ,__proto__))
		     (,vec (js-object-alloc-elements ,obj)))
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
(define-instantiate-expander JsModule)
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
(define-instantiate-expander JsBigInt64Array)
(define-instantiate-expander JsBigUint64Array)
(define-instantiate-expander JsFloat32Array)
(define-instantiate-expander JsFloat64Array)
(define-instantiate-expander JsDataView)
(define-instantiate-expander JsArguments)
(define-instantiate-expander JsString)
(define-instantiate-expander JsSymbol)
(define-instantiate-expander JsFunction (js-function-default-mode))
(define-instantiate-expander JsMethod (js-function-default-mode))
(define-instantiate-expander JsService (js-function-default-mode))
(define-instantiate-expander JsClass (js-function-default-mode))
(define-instantiate-expander JsHopFrame)
(define-instantiate-expander JsServer)
(define-instantiate-expander JsNumber)
(define-instantiate-expander JsBigInt)
(define-instantiate-expander JsMath)
(define-instantiate-expander JsRegExp)
(define-instantiate-expander JsBoolean)
(define-instantiate-expander JsError)
(define-instantiate-expander JsDate)
(define-instantiate-expander JsProxy)
(define-instantiate-expander JsJSON)
(define-instantiate-expander JsWorker)
(define-instantiate-expander JsPromise)
(define-instantiate-expander JsGenerator)
(define-instantiate-expander JsMap)
(define-instantiate-expander JsWebSocket)
(define-instantiate-expander JsWebSocketClient)
(define-instantiate-expander JsWebSocketServer)
(define-instantiate-expander JsWebSocketEvent)

;*---------------------------------------------------------------------*/
;*    js-export                                                        */
;*    -------------------------------------------------------------    */
;*    See js2scheme/scheme-program.scm and nodejs/require.scm.         */
;*---------------------------------------------------------------------*/
(define (js-export-expander x e)
   (match-case x
      ((?- ?id ?idx ?redirect ?ronly)
       (e `(vector ,@(cdr x)) e))
      (else
       (error "js-export" "wrong syntax" x))))

(define (js-export-id-expander x e)
   (match-case x
      ((?- ?o)
       (e `(vector-ref ,o 0) e))
      (else
       (error "export-id" "wrong syntax" x))))

(define (js-export-index-expander x e)
   (match-case x
      ((?- ?o)
       (e `(vector-ref ,o 1) e))
      (else
       (error "export-index" "wrong syntax" x))))

(define (js-export-redirect-expander x e)
   (match-case x
      ((?- ?o)
       (e `(vector-ref ,o 2) e))
      (else
       (error "export-redirect" "wrong syntax" x))))

(define (js-export-writable-expander x e)
   (match-case x
      ((?- ?o)
       (e `(vector-ref ,o 3) e))
      (else
       (error "export-writable" "wrong syntax" x))))
