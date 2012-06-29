;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/make_lib.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Fri Jun 29 09:25:14 2012 (serrano)                */
;*    Copyright   :  2006-12 Manuel Serrano                            */
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
	   __hop_xml-types
	   __hop_xml
	   __hop_html-base
	   __hop_html-img
	   __hop_html-head
	   __hop_charset
	   __hop_dom
	   __hop_read
	   __hop_module
	   __hop_read-js
	   __hop_css
	   __hop_css-match
	   __hop_clientc
	   __hop_cgi
	   __hop_user
	   __hop_password
	   __hop_js-comp
	   __hop_json
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
	   __hop_hop-inline
	   __hop_hop-rss
	   __hop_svg
	   __hop_mathml
	   __hop_event
	   __hop_color
	   __hop_preferences
	   __hop_weblets
	   __hop_cache
	   __hop_wiki
	   __hop_wiki-syntax
	   __hop_wiki-parser
           __hop_wiki-toc
	   __hop_hz
	   __hop_security
	   __hop_discovery
	   __hop_zeroconf)
	   
   (eval   (export-all)

	   (class user)

	   (class %http-message)
	   (class http-request)
	   (class http-server-request)
	   (class http-proxy-request)
	   (class %http-response)
	   (class http-response-remote)
	   (class http-response-autoload)
	   (class http-response-filter)
	   (class http-response-xml)
	   (class http-response-procedure)
	   (class http-response-file)
	   (class http-response-shoutcast)
	   (class http-response-string)
	   (class http-response-error)
	   (class http-response-raw)
	   (class http-response-hop)
	   (class http-response-authentication)
	   (class http-response-cgi)
	   (class http-response-persistent)
	   (class http-response-put)
	   
	   (class hop-service)

	   (class xml-backend)
	   (class security-manager)
	   
	   (class xml-http-request)
	   
	   (class xml)
	   (class xml-verbatim)
	   (class xml-markup)
	   (class xml-element)
	   (class xml-empty-element)
	   (class xml-cdata)
	   (class xml-tilde)
	   (class xml-html)
	   (class xml-document)

	   (class xml-lazy-attribute)

	   (class css-style)
	   
	   (class cache)
	   (class cache-disk)
	   (class cache-memory)
	   
	   (class cache-entry)

  	   (class wiki-syntax)

	   (class event)

	   (class discover-event)
	   (class discoverer)

	   (class zeroconf)
	   (class zeroconf-discoverer)
	   (class zeroconf-service-discoverer)

	   (class zeroconf-service-event))

   (cond-expand
      ((and enable-avahi (library pthread) (library avahi))
       (library avahi)))
      
   (cond-expand
      ((and enable-avahi (library pthread) (library avahi))
       (eval (class avahi)))))

