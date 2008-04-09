;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/hop-video.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 29 08:37:12 2007                          */
;*    Last change :  Fri Feb 22 15:40:12 2008 (serrano)                */
;*    Copyright   :  2007-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop Video support.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-video
   
   (library multimedia
	    web)

   (include "xml.sch"
	    "service.sch")
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_xml
	    __hop_img
	    __hop_misc
	    __hop_js-lib
	    __hop_service
	    __hop_hop-slider
	    __hop_hop-extra
	    __hop_cgi
	    __hop_read
	    __hop_event
	    __hop_http-error
	    __hop_charset)
   
   (export  (<VIDEO> . args)))

;*---------------------------------------------------------------------*/
;*    <VIDEO> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <VIDEO> ((id #unspecified string)
			      (src #f)
			      (attr)
			      body)
   
   (define (expr->function expr)
      (cond
	 ((xml-tilde? expr)
	  (format "function( event ) { ~a }" (tilde->string expr)))
	 ((string? expr)
	  (format "function( event ) { ~a }" expr))
	 (else
	  "false")))
   
   (let* ((id (xml-make-id id 'video))
	  (pid (xml-make-id 'hopvideo))
	  (init (<VIDEO-INIT> :id id :pid pid :src src)))
      (<VIDEO-OBJECT> id pid init)))


;*---------------------------------------------------------------------*/
;*    <VIDEO-OBJECT> ...                                               */
;*---------------------------------------------------------------------*/
(define (<VIDEO-OBJECT> id pid init)
   (let ((swf (make-file-path (hop-share-directory) "flash" "HopVideo.swf"))
	 (fvar (string-append "arg=hop_video_flash_init_" pid)))
      (<DIV> :id id :class "hop-video"
	 (<OBJECT> :id (string-append "object-" id) :class "hop-video"
	    :width "300px" :height "300px"
	    :title "hop-video" :classId "HopVideo.swf"
	    (<PARAM> :name "movie" :value swf)
	    (<PARAM> :name "src" :value swf)
	    (<PARAM> :name "wmode" :value "transparent")
	    (<PARAM> :name "play" :value "1")
	    (<PARAM> :name "allowScriptAccess" :value "sameDomain")
	    (<PARAM> :name "FlashVars" :value fvar)
	    (<EMBED> :id (string-append "embed-" id) :class "hop-video"
	       :width "300px" :height "300px"
	       :src swf
	       :type "application/x-shockwave-flash"
	       :name id
	       :swliveconnect #t
	       :allowScriptAccess "sameDomain"
	       :FlashVars fvar))
	 init)))

;*---------------------------------------------------------------------*/
;*    <VIDEO-INIT> ...                                                 */
;*---------------------------------------------------------------------*/
(define (<VIDEO-INIT> #!key id pid src)
   (<SCRIPT>
      (format "function hop_video_flash_init_~a() {hop_video_flash_init( ~s, ~a );};"
	      pid id
	      (if (string? src) (string-append "'" src "'") "false"))
      (format "hop_window_onload_add(
                function() {hop_video_init( ~s, ~a )})"
	      id
	      (if (string? src) (string-append "'" src "'") "false"))))
