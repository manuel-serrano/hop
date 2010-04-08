;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/make-lib.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Wed Apr  7 20:48:00 2010 (serrano)                */
;*    Copyright   :  2006-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the HOP heap file.                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_makelib

   (import __hop_configure
	   __hop_param
	   __hop_expanders
	   __hop_misc
	   __hop_mime
	   __hop_types
	   __hop_xml
	   __hop_img
	   __hop_charset
	   __hop_dom
	   __hop_read
	   __hop_module
	   __hop_read-js
	   __hop_css
	   __hop_clientc
	   __hop_cgi
	   __hop_user
	   __hop_password
	   __hop_js-lib
	   __hop_job
	   __hop_hop
	   __hop_service
	   __hop_http-lib
	   __hop_http-request
	   __hop_http-response
	   __hop_http-remote
	   __hop_http-shoutcast
	   __hop_http-webdav
	   __hop_http-error
	   __hop_hop-extra
	   __hop_hop-inline
	   __hop_hop-sym
	   __hop_hop-rss
	   __hop_hop-svg
	   __hop_hop-mathml
	   __hop_event
	   __hop_color
	   __hop_prefs
	   __hop_weblets
	   __hop_cache
	   __hop_wiki
	   __hop_wiki-syntax
	   __hop_wiki-parser
           __hop_wiki-toc
	   __hop_hz

	   (hop-event-policy-file __hop_event))

   (eval   (export-all)

	   (class job)
	   
	   (class user)
	   
	   (class %http-message)
	   (class http-request)
	   (class http-server-request)
	   (class http-proxy-request)
	   (class %http-response)
	   (class http-response-remote)
	   (class http-response-filter)
	   (class http-response-hop)
	   (class http-response-procedure)
	   (class http-response-file)
	   (class http-response-shoutcast)
	   (class http-response-string)
	   (class http-response-error)
	   (class http-response-raw)
	   (class http-response-js)
	   (class http-response-authentication)
	   (class http-response-cgi)
	   (class http-response-persistent)
	   (class http-response-put)
	   
	   (class hop-service)

	   (class xml-backend)

	   (class xml-http-request)
	   
	   (class xml)
	   (class xml-markup)
	   (class xml-element)
	   (class xml-cdata)
	   (class xml-html)
	   (class xml-document)

	   (class cache)
	   (class cache-disk)
	   (class cache-memory)
	   
	   (class cache-entry)

  	   (class wiki-syntax)))
