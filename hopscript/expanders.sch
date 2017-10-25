;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/expanders.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 18:25:11 2006                          */
;*    Last change :  Wed Oct 25 18:04:48 2017 (serrano)                */
;*    Copyright   :  2006-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript expanders installer                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    hopscript-install-expanders! ...                                 */
;*---------------------------------------------------------------------*/
(define (hopscript-install-expanders!)

   (eval `(define-expander %define-pcache
	     ,%define-pcache-expander))
   (eval `(define-expander js-make-pcache
	     ,js-make-pcache-expander))
   (eval `(define-expander js-pcache-ref
	     ,js-pcache-ref-expander))
   (eval `(define-expander js-pcache-cmap
	     ,js-pcache-cmap-expander))
   (eval `(define-expander js-pcache-pmap
	     ,js-pcache-pmap-expander))
   (eval `(define-expander js-pcache-index
	     ,js-pcache-index-expander))
   (eval `(define-expander js-pcache-vindex
	     ,js-pcache-vindex-expander))
   (eval `(define-expander js-pcache-owner
	     ,js-pcache-owner-expander))
   (eval `(define-expander js-pcache-method
	     ,js-pcache-method-expander))
   (eval `(define-expander js-get-name/cache
	     ,js-get-name/cache-expander))
   
   (eval `(define-expander js-object-get-name/cache
	     ,js-object-get-name/cache-expander))
   (eval `(define-expander js-object-get-name/cache-level1
	     ,js-object-get-name/cache-level1-expander))
   (eval `(define-expander js-object-get-name/cache-level2
	     ,js-object-get-name/cache-level2-expander))
   (eval `(define-expander js-global-object-get-name
	     ,js-global-object-get-name-expander))
   (eval `(define-expander js-global-object-get-name/cache
	     ,js-global-object-get-name/cache-expander))
   
   (eval `(define-expander js-get-length
	     ,js-get-length-expander))
   
   (eval `(define-expander js-put-name/cache!
	     ,js-put-name/cache-expander))
   (eval `(define-expander js-object-put-name/cache!
	     ,js-object-put-name/cache-expander))
   (eval `(define-expander js-object-put-name/cache-level1!
	     ,js-object-put-name/cache-level1-expander))
   (eval `(define-expander js-object-put-name/cache-level2!
	     ,js-object-put-name/cache-level2-expander))
   
   (eval `(define-expander js-method-call-name/cache
	     ,js-method-call-name/cache-expander))
   (eval `(define-expander js-object-method-call-name/cache
	     ,js-object-method-call-name/cache-expander))
   (eval `(define-expander js-non-object-method-call-name
	     ,js-non-object-method-call-name-expander))
   (eval `(define-expander js-object-method-call-name/cache-level2
	     ,js-object-method-call-name/cache-level2-expander))
   (eval `(define-expander js-call/cache
	     ,js-call/cache-expander))
   
   (eval `(define-expander js-let-set!
	     ,js-let-set!-expander))
   
   (eval `(define-expander js-jsstring-append
	     ,js-jsstring-append-expander))
   
   (eval `(define-expander instantiate-JsObject
	     ,js-instantiate-JsObject-expander))
   (eval `(define-expander instantiate-JsWrapper
	     ,js-instantiate-JsWrapper-expander))
   (eval `(define-expander instantiate-JsGlobalObject
	     ,js-instantiate-JsGlobalObject-expander))
   (eval `(define-expander instantiate-JsArray
	     ,js-instantiate-JsArray-expander))
   (eval `(define-expander instantiate-JsArrayBuffer
	     ,js-instantiate-JsArrayBuffer-expander))
   (eval `(define-expander instantiate-JsArrayBufferView
	     ,js-instantiate-JsArrayBufferView-expander))
   (eval `(define-expander instantiate-JsTypedArray
	     ,js-instantiate-JsTypedArray-expander))
   (eval `(define-expander instantiate-JsInt8Array
	     ,js-instantiate-JsInt8Array-expander))
   (eval `(define-expander instantiate-JsUint8Array
	     ,js-instantiate-JsUint8Array-expander))
   (eval `(define-expander instantiate-JsInt16Array
	     ,js-instantiate-JsInt16Array-expander))
   (eval `(define-expander instantiate-JsUint16Array
	     ,js-instantiate-JsUint16Array-expander))
   (eval `(define-expander instantiate-JsInt32Array
	     ,js-instantiate-JsInt32Array-expander))
   (eval `(define-expander instantiate-JsUint32Array
	     ,js-instantiate-JsUint32Array-expander))
   (eval `(define-expander instantiate-JsFloat32Array
	     ,js-instantiate-JsFloat32Array-expander))
   (eval `(define-expander instantiate-JsFloat64Array
	     ,js-instantiate-JsFloat64Array-expander))
   (eval `(define-expander instantiate-JsDataView
	     ,js-instantiate-JsDataView-expander))
   (eval `(define-expander instantiate-JsArguments
	     ,js-instantiate-JsArguments-expander))
   (eval `(define-expander instantiate-JsString
	     ,js-instantiate-JsString-expander))
   (eval `(define-expander instantiate-JsSymbol
	     ,js-instantiate-JsSymbol-expander))
   (eval `(define-expander instantiate-JsFunction
	     ,js-instantiate-JsFunction-expander))
   (eval `(define-expander instantiate-JsService
	     ,js-instantiate-JsService-expander))
   (eval `(define-expander instantiate-JsFunction1
	     ,js-instantiate-JsFunction1-expander))
   (eval `(define-expander instantiate-JsFunction2
	     ,js-instantiate-JsFunction2-expander))
   (eval `(define-expander instantiate-JsFunction3
	     ,js-instantiate-JsFunction3-expander))
   (eval `(define-expander instantiate-JsFunction4
	     ,js-instantiate-JsFunction4-expander))
   (eval `(define-expander instantiate-JsFunction5
	     ,js-instantiate-JsFunction5-expander))
   (eval `(define-expander instantiate-JsHopFrame
	     ,js-instantiate-JsHopFrame-expander))
   (eval `(define-expander instantiate-JsServer
	     ,js-instantiate-JsServer-expander))
   (eval `(define-expander instantiate-JsNumber
	     ,js-instantiate-JsNumber-expander))
   (eval `(define-expander instantiate-JsMath
	     ,js-instantiate-JsMath-expander))
   (eval `(define-expander instantiate-JsRegExp
	     ,js-instantiate-JsRegExp-expander))
   (eval `(define-expander instantiate-JsBoolean
	     ,js-instantiate-JsBoolean-expander))
   (eval `(define-expander instantiate-JsError
	     ,js-instantiate-JsError-expander))
   (eval `(define-expander instantiate-JsDate
	     ,js-instantiate-JsDate-expander))
   (eval `(define-expander instantiate-JsJSON
	     ,js-instantiate-JsJSON-expander))
   (eval `(define-expander instantiate-JsWorker
	     ,js-instantiate-JsWorker-expander))
   (eval `(define-expander instantiate-JsPromise
	     ,js-instantiate-JsPromise-expander))
   (eval `(define-expander instantiate-JsGenerator
	     ,js-instantiate-JsGenerator-expander))
   (eval `(define-expander instantiate-JsWebSocket
	     ,js-instantiate-JsWebSocket-expander))
   (eval `(define-expander instantiate-JsWebSocketClient
	     ,js-instantiate-JsWebSocketClient-expander))
   (eval `(define-expander instantiate-JsWebSocketServer
	     ,js-instantiate-JsWebSocketServer-expander))
   (eval `(define-expander instantiate-JsWebSocketEvent
	     ,js-instantiate-JsWebSocketEvent-expander)))
