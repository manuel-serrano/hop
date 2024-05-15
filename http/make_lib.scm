;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/http/make_lib.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 14:00:32 2013                          */
;*    Last change :  Tue May 14 11:59:36 2024 (serrano)                */
;*    Copyright   :  2013-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    THe module used to build the http heap file.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __http_makelib
   
   (library pthread)
   
   (import __http_types
	   __http_accept
	   __http_pipeline
	   __http_scheduler
	   __http_scheduler-nothread
	   __http_scheduler-accept-many
	   __http_scheduler-one-to-one
	   __http_scheduler-queue
	   __http_scheduler-pool
	   __http_parser)
   
   (eval   (export-all)

	   (class %http-message)
	   
	   (class http-request)
	   (class http-server-request)
	   (class http-proxy-request)
	   
	   (class %http-response)
	   (class http-response-proxy)
	   (class %http-response-server)
	   (class http-response-string)
	   (class http-response-async)
	   (class http-response-abort)
	   (class http-response-file)
	   (class http-response-error)))
