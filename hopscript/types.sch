;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/types.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct 25 15:52:55 2017                          */
;*    Last change :  Wed Apr 28 07:34:17 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Companion macros                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    directives                                                       */
;*---------------------------------------------------------------------*/
(directives (option (loadq "types_expd.sch")))

;*---------------------------------------------------------------------*/
;*    define-instantiate-sans ...                                      */
;*---------------------------------------------------------------------*/
(define-expander define-instantiate-sans
   (lambda (x e)
      (let* ((clazz (cadr x))
	     (defe `(define-expander ,(symbol-append 'instantiate clazz)
		       ,(symbol-append 'js-instantiate- clazz '-expander))))
	 (e defe e))))

;*---------------------------------------------------------------------*/
;*    constructors                                                     */
;*---------------------------------------------------------------------*/
(define-instantiate-sans JsObject)
(define-instantiate-sans JsWrapper)
(define-instantiate-sans JsGlobalObject)
(define-instantiate-sans JsArray)
(define-instantiate-sans JsArrayBuffer)
(define-instantiate-sans JsArrayBufferView)
(define-instantiate-sans JsTypedArray)
(define-instantiate-sans JsInt8Array)
(define-instantiate-sans JsUint8Array)
(define-instantiate-sans JsInt16Array)
(define-instantiate-sans JsUint16Array)
(define-instantiate-sans JsInt32Array)
(define-instantiate-sans JsUint32Array)
(define-instantiate-sans JsFloat32Array)
(define-instantiate-sans JsFloat64Array)
(define-instantiate-sans JsDataView)
(define-instantiate-sans JsArguments)
(define-instantiate-sans JsString)
(define-instantiate-sans JsSymbol)
(define-instantiate-sans JsFunction)
(define-instantiate-sans JsMethod)
(define-instantiate-sans JsService)
(define-instantiate-sans JsHopFrame)
(define-instantiate-sans JsServer)
(define-instantiate-sans JsNumber)
(define-instantiate-sans JsMath)
(define-instantiate-sans JsModule)
(define-instantiate-sans JsRegExp)
(define-instantiate-sans JsBoolean)
(define-instantiate-sans JsError)
(define-instantiate-sans JsDate)
(define-instantiate-sans JsProxy)
(define-instantiate-sans JsJSON)
(define-instantiate-sans JsWorker)
(define-instantiate-sans JsPromise)
(define-instantiate-sans JsGenerator)
(define-instantiate-sans JsMap)
(define-instantiate-sans JsWebSocket)
(define-instantiate-sans JsWebSocketClient)
(define-instantiate-sans JsWebSocketServer)
(define-instantiate-sans JsWebSocketEvent)

;*---------------------------------------------------------------------*/
;*    js-export                                                        */
;*---------------------------------------------------------------------*/
(define-expander js-export js-export-expander)
(define-expander js-export-id js-export-id-expander)
(define-expander js-export-index js-export-index-expander)
(define-expander js-export-redirect js-export-redirect-expander)
(define-expander js-export-writable js-export-writable-expander)

;*---------------------------------------------------------------------*/
;*    isa?                                                             */
;*---------------------------------------------------------------------*/
(define-expander isa?
   (lambda (x e)
      
      (define (epairify nx x)
	 (if (epair? x)
	     (econs (car nx) (cdr nx) (cer x))
	     nx))

      (match-case x
	 ((isa? ?obj JsObject)
	  (e (epairify `(js-object? ,obj) x) e))
	 ((isa? ?obj JsStringLiteral)
	  (e (epairify `(js-jsstring? ,obj) x) e))
	 (else
	  (set-car! x '(@ isa? __object))
	  (e x e)))))

