;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/make-lib.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Thu Apr 27 08:09:52 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The module used to build the HOP heap file.                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_makelib

   (option  (set! *dlopen-init* #t))
   
   (import __hop_configure
	   __hop_param
	   __hop_misc
	   __hop_mime
	   __hop_types
	   __hop_thread
	   __hop_xml
	   __hop_dom
	   __hop_read
	   __hop_read-js
	   __hop_css
	   __hop_cgi
	   __hop_user
	   __hop_js-lib
	   __hop_job
	   __hop_hop
	   __hop_service
	   __hop_http-lib
	   __hop_http-response
	   __hop_http-shoutcast
	   __hop_http-error
	   __hop_hop-extra
	   __hop_hop-inline
	   __hop_hop-paned
	   __hop_hop-tabslider
	   __hop_hop-notepad
	   __hop_hop-slider
	   __hop_hop-tree
	   __hop_hop-foldlist
	   __hop_hop-window
	   __hop_event
	   __hop_color
	   __hop_prefs
	   __hop_weblets
	   __hop_cache)

   (eval   (export-all)
	   
	   (class xml-document)
	   (class hop-event)
	   (class job)

	   (class user)
	   
	   (class %http-message)
	   (class http-request)
	   (class %http-response)
	   (class http-response-remote)
	   (class http-response-filter)
	   (class http-response-hop)
	   (class http-response-procedure)
	   (class http-response-file)
	   (class http-response-shoutcast)
	   (class http-response-string)
	   (class http-response-obj)
	   (class http-response-js)
	   (class http-response-authentication)
	   (class http-response-cgi)
	   (class http-response-persistent)
	   (class http-response-put)
	   
	   (class hop-service)

	   (class xml)
	   (class xml-markup)
	   (class xml-element)
	   (class xml-html)

	   (class cache-entry)
	   (class cache)))
	   
	   
