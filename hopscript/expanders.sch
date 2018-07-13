;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/expanders.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 18:25:11 2006                          */
;*    Last change :  Fri Jul 13 08:18:11 2018 (serrano)                */
;*    Copyright   :  2006-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript expanders installer                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    hopscript-install-expanders! ...                                 */
;*---------------------------------------------------------------------*/
(define (hopscript-install-expanders!)

   (eval `(define-expander define-jseval
	     ,js-define-jseval-expander))
   (eval `(define-expander %define-pcache
	     ,%define-pcache-expander))
   (eval `(define-expander js-make-pcache
	     ,js-make-pcache-expander))
   (eval `(define-expander js-pcache-ref
	     ,js-pcache-ref-expander))
   (eval `(define-expander js-pcache-pcache
	     ,js-pcache-pcache-expander))
   (eval `(define-expander js-pcache-imap
	     ,js-pcache-imap-expander))
   (eval `(define-expander js-pcache-cmap
	     ,js-pcache-cmap-expander))
   (eval `(define-expander js-pcache-pmap
	     ,js-pcache-pmap-expander))
   (eval `(define-expander js-pcache-emap
	     ,js-pcache-emap-expander))
   (eval `(define-expander js-pcache-amap
	     ,js-pcache-amap-expander))
   (eval `(define-expander js-pcache-index
	     ,js-pcache-index-expander))
   (eval `(define-expander js-pcache-vindex
	     ,js-pcache-vindex-expander))
   (eval `(define-expander js-pcache-owner
	     ,js-pcache-owner-expander))
   (eval `(define-expander js-pcache-method
	     ,js-pcache-method-expander))
   (eval `(define-expander js-pcache-function
	     ,js-pcache-function-expander))
   (eval `(define-expander js-get-name/cache
	     ,js-get-name/cache-expander))

   (eval `(define-expander js-profile-log-cache
	     ,js-profile-log-cache-expander))
   (eval `(define-expander js-profile-log-index
	     ,js-profile-log-index-expander))
   
   (eval `(define-expander js-object-get-name/cache
	     ,js-object-get-name/cache-expander))
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
   
   (eval `(define-expander js-call/cache
	     ,js-call/cache-expander))
   (eval `(define-expander js-method-call-name/cache
	     ,js-method-call-name/cache-expander))
   (eval `(define-expander js-object-method-call-name/cache
	     ,js-object-method-call-name/cache-expander))
   (eval `(define-expander js-non-object-method-call-name
	     ,js-non-object-method-call-name-expander))
   
   (eval `(define-expander js-let-set!
	     ,js-let-set!-expander))
   
   (eval `(define-expander js-jsstring-append
	     ,js-jsstring-append-expander))
   (eval `(define-expander js-ascii->jsstring
	     ,js-ascii->jsstring-expander))

   (eval `(define-expander instantiateJsObject
	     ,js-instantiate-JsObject-expander))
   (eval `(define-expander instantiateJsWrapper
	     ,js-instantiate-JsWrapper-expander))
   (eval `(define-expander instantiateJsGlobalObject
	     ,js-instantiate-JsGlobalObject-expander))
   (eval `(define-expander instantiateJsArray
	     ,js-instantiate-JsArray-expander))
   (eval `(define-expander instantiateJsArrayBuffer
	     ,js-instantiate-JsArrayBuffer-expander))
   (eval `(define-expander instantiateJsArrayBufferView
	     ,js-instantiate-JsArrayBufferView-expander))
   (eval `(define-expander instantiateJsTypedArray
	     ,js-instantiate-JsTypedArray-expander))
   (eval `(define-expander instantiateJsInt8Array
	     ,js-instantiate-JsInt8Array-expander))
   (eval `(define-expander instantiateJsUint8Array
	     ,js-instantiate-JsUint8Array-expander))
   (eval `(define-expander instantiateJsInt16Array
	     ,js-instantiate-JsInt16Array-expander))
   (eval `(define-expander instantiateJsUint16Array
	     ,js-instantiate-JsUint16Array-expander))
   (eval `(define-expander instantiateJsInt32Array
	     ,js-instantiate-JsInt32Array-expander))
   (eval `(define-expander instantiateJsUint32Array
	     ,js-instantiate-JsUint32Array-expander))
   (eval `(define-expander instantiateJsFloat32Array
	     ,js-instantiate-JsFloat32Array-expander))
   (eval `(define-expander instantiateJsFloat64Array
	     ,js-instantiate-JsFloat64Array-expander))
   (eval `(define-expander instantiateJsDataView
	     ,js-instantiate-JsDataView-expander))
   (eval `(define-expander instantiateJsArguments
	     ,js-instantiate-JsArguments-expander))
   (eval `(define-expander instantiateJsString
	     ,js-instantiate-JsString-expander))
   (eval `(define-expander instantiateJsSymbol
	     ,js-instantiate-JsSymbol-expander))
   (eval `(define-expander instantiateJsFunction
	     ,js-instantiate-JsFunction-expander))
   (eval `(define-expander instantiateJsService
	     ,js-instantiate-JsService-expander))
   (eval `(define-expander instantiateJsFunction1
	     ,js-instantiate-JsFunction1-expander))
   (eval `(define-expander instantiateJsFunction2
	     ,js-instantiate-JsFunction2-expander))
   (eval `(define-expander instantiateJsFunction3
	     ,js-instantiate-JsFunction3-expander))
   (eval `(define-expander instantiateJsFunction4
	     ,js-instantiate-JsFunction4-expander))
   (eval `(define-expander instantiateJsFunction5
	     ,js-instantiate-JsFunction5-expander))
   (eval `(define-expander instantiateJsHopFrame
	     ,js-instantiate-JsHopFrame-expander))
   (eval `(define-expander instantiateJsServer
	     ,js-instantiate-JsServer-expander))
   (eval `(define-expander instantiateJsNumber
	     ,js-instantiate-JsNumber-expander))
   (eval `(define-expander instantiateJsMath
	     ,js-instantiate-JsMath-expander))
   (eval `(define-expander instantiateJsRegExp
	     ,js-instantiate-JsRegExp-expander))
   (eval `(define-expander instantiateJsBoolean
	     ,js-instantiate-JsBoolean-expander))
   (eval `(define-expander instantiateJsError
	     ,js-instantiate-JsError-expander))
   (eval `(define-expander instantiateJsDate
	     ,js-instantiate-JsDate-expander))
   (eval `(define-expander instantiateJsJSON
	     ,js-instantiate-JsJSON-expander))
   (eval `(define-expander instantiateJsWorker
	     ,js-instantiate-JsWorker-expander))
   (eval `(define-expander instantiateJsPromise
	     ,js-instantiate-JsPromise-expander))
   (eval `(define-expander instantiateJsGenerator
	     ,js-instantiate-JsGenerator-expander))
   (eval `(define-expander instantiateJsWebSocket
	     ,js-instantiate-JsWebSocket-expander))
   (eval `(define-expander instantiateJsWebSocketClient
	     ,js-instantiate-JsWebSocketClient-expander))
   (eval `(define-expander instantiateJsWebSocketServer
	     ,js-instantiate-JsWebSocketServer-expander))
   (eval `(define-expander instantiateJsWebSocketEvent
	     ,js-instantiate-JsWebSocketEvent-expander)))
