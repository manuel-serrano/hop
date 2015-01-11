;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/hop_video.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 29 08:37:12 2007                          */
;*    Last change :  Sun Jan 11 20:41:07 2015 (serrano)                */
;*    Copyright   :  2007-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop Video support.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-video
   
   (library multimedia web)

   (include "xml.sch"
	    "service.sch")
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_xml
	    __hop_img
	    __hop_misc
	    __hop_js-comp
	    __hop_service
	    __hop_hop-slider
	    __hop_hop-extra
	    __hop_read
	    __hop_event
	    __hop_http-error
	    __hop_charset)
   
   (export  (<VIDEO> . args)))

;*---------------------------------------------------------------------*/
;*    <VIDEO> ...                                                      */
;*    -------------------------------------------------------------    */
;*    The current version of the Hop player uses JW FLV player         */
;*        http://www.longtailvideo.com/players/jw-flv-player/          */
;*---------------------------------------------------------------------*/
(define-xml-compound <VIDEO> ((id #unspecified string)
			      (src #f)
			      (img #f)
			      (width 640)
			      (height 480)
			      (bg #f)
			      (attr)
			      body)
   (let ((dummy (symbol->string (gensym 'video-container)))
	 (tmp (gensym 'video_tmp)))
      (<DIV> :id dummy
	 (<SCRIPT> :type "text/javascript"
	    (format "var ~a = new SWFObject('~a','~a','~a','~a','9','#FFFFFF');"
		    tmp
		    (make-file-path (hop-share-directory) "flash" "player.swf")
		    (xml-make-id id 'video)
		    width height src)
	    (format "~a.addParam('allowfullscreen','true');" tmp)
	    (format "~a.addParam('allowscriptaccess','always');" tmp)
	    (if img
		(format "~a.addParam('flashvars','file=~a&image=~a');" tmp src img)
		(format "~a.addParam('flashvars','file=~a');" tmp src))
	    (when bg (format "~a.addParam('screencolor','~a');" tmp bg))
	    (format "~a.write('~a');" tmp dummy)))))
