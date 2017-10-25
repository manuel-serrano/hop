;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/types.sch               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct 25 15:52:55 2017                          */
;*    Last change :  Wed Oct 25 23:48:32 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
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
	     (defe `(define-expander ,(symbol-append 'instantiate- clazz)
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
(define-instantiate-sans JsService)
(define-instantiate-sans JsFunction1)
(define-instantiate-sans JsFunction2)
(define-instantiate-sans JsFunction3)
(define-instantiate-sans JsFunction4)
(define-instantiate-sans JsFunction5)
(define-instantiate-sans JsHopFrame)
(define-instantiate-sans JsServer)
(define-instantiate-sans JsNumber)
(define-instantiate-sans JsMath)
(define-instantiate-sans JsRegExp)
(define-instantiate-sans JsBoolean)
(define-instantiate-sans JsError)
(define-instantiate-sans JsDate)
(define-instantiate-sans JsJSON)
(define-instantiate-sans JsWorker)
(define-instantiate-sans JsPromise)
(define-instantiate-sans JsGenerator)
(define-instantiate-sans JsWebSocket)
(define-instantiate-sans JsWebSocketClient)
(define-instantiate-sans JsWebSocketServer)
(define-instantiate-sans JsWebSocketEvent)
