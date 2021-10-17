;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/expanders.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 18:25:11 2006                          */
;*    Last change :  Sun Oct 17 09:03:05 2021 (serrano)                */
;*    Copyright   :  2006-21 Manuel Serrano                            */
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
   (eval `(define-expander js-make-pcache-table
	     ,js-make-pcache-table-expander))
   (eval `(define-expander js-pcache-ref
	     ,js-pcache-ref-expander))
   (eval `(define-expander js-pcache-imap
	     ,js-pcache-imap-expander))
   (eval `(define-expander js-pcache-cmap
	     ,js-pcache-cmap-expander))
   (eval `(define-expander js-pcache-pmap
	     ,js-pcache-pmap-expander))
   (eval `(define-expander js-pcache-xmap
	     ,js-pcache-xmap-expander))
   (eval `(define-expander js-pcache-nmap
	     ,js-pcache-nmap-expander))
   (eval `(define-expander js-pcache-emap
	     ,js-pcache-emap-expander))
   (eval `(define-expander js-pcache-amap
	     ,js-pcache-amap-expander))
   (eval `(define-expander js-pcache-nextemap
	     ,js-pcache-nextemap-expander))
   (eval `(define-expander js-pcache-nextnmap
	     ,js-pcache-nextnmap-expander))
   (eval `(define-expander js-pcache-iindex
	     ,js-pcache-iindex-expander))
   (eval `(define-expander js-pcache-eindex
	     ,js-pcache-eindex-expander))
   (eval `(define-expander js-pcache-cindex
	     ,js-pcache-cindex-expander))
   (eval `(define-expander js-pcache-pindex
	     ,js-pcache-pindex-expander))
   (eval `(define-expander js-pcache-nindex
	     ,js-pcache-nindex-expander))
   (eval `(define-expander js-pcache-aindex
	     ,js-pcache-aindex-expander))
   (eval `(define-expander js-pcache-vindex
	     ,js-pcache-vindex-expander))
   (eval `(define-expander js-pcache-owner
	     ,js-pcache-owner-expander))
   (eval `(define-expander js-pcache-method
	     ,js-pcache-method-expander))
   (eval `(define-expander js-pcache-function
	     ,js-pcache-function-expander))

   (eval `(define-expander js-record-cache-check-proto-method
	     ,js-record-cache-check-proto-method-expander))
   (eval `(define-expander js-record-cmap-cache-check-proto-method
	     ,js-record-cmap-cache-check-proto-method-expander))
   (eval `(define-expander js-object-cache-check-proto-method
	     ,js-object-cache-check-proto-method-expander))
   (eval `(define-expander js-object-cmap-cache-check-proto-method
	     ,js-object-cmap-cache-check-proto-method-expander))
   
   (eval `(define-expander js-get-name/cache
	     ,js-get-name/cache-expander))

   (eval `(define-expander js-getprototypeof
	     ,js-getprototypeof-expander))
   (eval `(define-expander js-has-own-property
	     ,js-has-own-property-expander))
   
   (eval `(define-expander %define-cnst-table
	     ,%define-cnst-table-expander))
   (eval `(define-expander js-cnst-table
	     ,js-cnst-table-expander))
   (eval `(define-expander js-cnst-table-ref
	     ,js-cnst-table-ref-expander))
   
   (eval `(define-expander js-profile-log-cache
	     ,js-profile-log-cache-expander))
   (eval `(define-expander js-profile-log-index
	     ,js-profile-log-index-expander))
   
   (eval `(define-expander js-get-jsobject-name/cache
	     ,js-get-jsobject-name/cache-expander))
   (eval `(define-expander js-global-object-get-name/cache
	     ,js-global-object-get-name/cache-expander))

   (eval `(define-expander js-get-length
	     ,js-get-length-expander))
   (eval `(define-expander js-get-lengthu32
	     ,js-get-lengthu32-expander))
   (eval `(define-expander js-get-length-maybe-string
	     ,js-get-length-maybe-string-expander))
   (eval `(define-expander js-get-lengthu32-maybe-string
	     ,js-get-lengthu32-maybe-string-expander))
   (eval `(define-expander js-get-length-maybe-arguments
	     ,js-get-length-maybe-arguments-expander))
   (eval `(define-expander js-get-lengthu32-maybe-arguments
	     ,js-get-lengthu32-maybe-arguments-expander))

   (eval `(define-expander js-put-name/cache!
	     ,js-put-name/cache-expander))
   (eval `(define-expander js-put-jsobject-name/cache!
	     ,js-put-jsobject-name/cache-expander))

   (eval `(define-expander js-call/cache
	     ,js-call/cache-expander))
   (eval `(define-expander js-method-call-name/cache
	     ,js-method-call-name/cache-expander))
   (eval `(define-expander js-method-jsobject-call-name/cache
	     ,js-method-jsobject-call-name/cache-expander))
   (eval `(define-expander js-method-non-jsobject-call-name
	     ,js-method-non-jsobject-call-name-expander))
   (eval `(define-expander js-method-jsrecord-call-index
	     ,js-method-jsrecord-call-index-expander))
   
   (eval `(define-expander js-let-set!
	     ,js-let-set!-expander))
   (eval `(define-expander js-null-or-undefined?
	     ,js-null-or-undefined?-expander))
   (eval `(define-expander js-tonumber
	     ,js-tonumber-expander))
   (eval `(define-expander js-tonumber-for-flonum
	     ,js-tonumber-for-flonum-expander))
   (eval `(define-expander js-toprimitive-for-string
	     ,js-toprimitive-for-string-expander))
   (eval `(define-expander js-nullish
	     ,js-nullish-expander))
   
   (eval `(define-expander &
	     ,&-expander))
   (eval `(define-expander &with!
	     ,&with!-expander))
   (eval `(define-expander &begin!
	     ,&begin!-expander))
   (eval `(define-expander &end!
	     ,&end!-expander))
   (eval `(define-expander &init!
	     ,&init!-expander))

   (eval `(define-expander instantiateJsObject
	     ,js-instantiate-JsObject-expander))
   (eval `(define-expander instantiateJsModule
	     ,js-instantiate-JsModule-expander))
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
   (eval `(define-expander instantiateJsHopFrame
	     ,js-instantiate-JsHopFrame-expander))
   (eval `(define-expander instantiateJsServer
	     ,js-instantiate-JsServer-expander))
   (eval `(define-expander instantiateJsNumber
	     ,js-instantiate-JsNumber-expander))
   (eval `(define-expander instantiateJsBigInt
	     ,js-instantiate-JsBigInt-expander))
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
   (eval `(define-expander instantiateJsProxy
	     ,js-instantiate-JsProxy-expander))
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
	     ,js-instantiate-JsWebSocketEvent-expander))

   (eval `(define-expander js-export
	     ,js-export-expander))
   (eval `(define-expander js-export-id
	     ,js-export-id-expander))
   (eval `(define-expander js-export-index
	     ,js-export-index-expander))
   (eval `(define-expander js-export-redirect
	     ,js-export-redirect-expander))
   (eval `(define-expander js-export-writable
	     ,js-export-writable-expander))

   (eval `(define-expander js-with-handler-no-unwind
	     ,js-with-handler-no-unwind-expander))
   (eval `(define-expander js-call%
	     ,js-call%-expander))
   (eval `(define-expander js-call-procedure
	     ,js-call-procedure-expander))
   (eval `(define-expander js-call-jsprocedure
	     ,js-call-jsprocedure-expander))
   (eval `(define-expander js-new
	     ,js-new-expander))

   (eval `(define-expander js-jsobject->jsarray
	     ,js-jsobject->jsarray-expander))

   (eval `(define-expander declare-tls
	     ,js-declare-tls-expander))
   (eval `(define-expander define-tls
	     ,js-define-tls-expander)))
      
