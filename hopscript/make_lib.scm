;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/make_lib.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 14:00:32 2013                          */
;*    Last change :  Sat Apr 11 13:53:49 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    THe module used to build the hopscript heap file.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_makelib

   (library hop)
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_arithmetic32
	   __hopscript_arithmetic64
	   __hopscript_property
	   __hopscript_profile
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_builtin
	   __hopscript_arguments
	   __hopscript_function
	   __hopscript_service
	   __hopscript_array
	   __hopscript_arraybuffer	   
	   __hopscript_arraybufferview
	   __hopscript_string
	   __hopscript_stringliteral
	   __hopscript_names
	   __hopscript_number 
	   __hopscript_math
	   __hopscript_boolean
	   __hopscript_date
	   __hopscript_regexp
	   __hopscript_error
	   __hopscript_json
	   __hopscript_worker
	   __hopscript_websocket
	   __hopscript_promise
	   __hopscript_generator
	   __hopscript_proxy
	   __hopscript_spawn
	   __hopscript_expanders)
   
   (eval   (export-all)

           (class JsStringLiteral)
           (class JsStringLiteralASCII)
           (class JsStringLiteralUTF8)
      
           (class JsObject)
	   (class JsGlobalObject)
	   (class JsFunction)
	   (class JsService)
	   (class JsArray)
	   (class JsArguments)
	   (class JsString)
	   (class JsNumber)
	   (class JsRegExp)
	   (class JsBoolean)
	   (class JsDate)
	   (class JsError)
	   
	   (class JsModule)

	   (class JsWorker)
	   (class JsPromise)
	   (class JsGenerator)
	   
	   (class JsWebSocket)
	   (class JsWebSocketClient)
	   (class JsWebSocketServer)
	   
	   (class JsArrayBuffer)
	   (class JsArrayBufferView)
	   (class JsTypedArray)
	   (class JsInt8Array)
	   (class JsUint8Array)
	   (class JsUint8ClampedArray)
	   (class JsInt16Array)
	   (class JsUint16Array)
	   (class JsInt32Array)
	   (class JsUint32Array)
	   (class JsFloat32Array)
	   (class JsFloat64Array)
	   (class JsDataView)
	   
	   (class JsPropertyCache)
	   (class JsConstructMap)
	   
	   (class JsValueDescriptor)
	   (class JsAccessorDescriptor)

	   (class MessageEvent)
	   (class WorkerHopThread)
	   (class JsWebSocketEvent)

	   (class JsProxy)
	   
	   (class JsWrapper)))
