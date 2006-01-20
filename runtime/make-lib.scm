;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/make-lib.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Thu Jan 19 10:15:36 2006 (serrano)                */
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
	   __hop_builtin
	   __hop_http-lib
	   __hop_http-response
	   __hop_http-shoutcast
	   __hop_http-error
	   __hop_html-extra
	   __hop_html-inline
	   __hop_html-paned
	   __hop_html-tabslider
	   __hop_html-notepad
	   __hop_html-slider
	   __hop_html-tree
	   __hop_event)

   (eval   (export-all)))
