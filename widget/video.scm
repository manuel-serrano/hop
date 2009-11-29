;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/widget/video.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 29 08:37:12 2007                          */
;*    Last change :  Sun Nov 29 17:59:24 2009 (serrano)                */
;*    Copyright   :  2007-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop Video support.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-video
   
   (library multimedia web hop)

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
			      (backend 'html5)
			      (attr)
			      body)
   
   (define (<flash> tmp vid bool)
      (<SCRIPT> :type "text/javascript"
	 (format "if( ~a || !('play' in document.createElement('video')) ) {
                 var ~a = new SWFObject('~a','~a','~a','~a','9','#FFFFFF');"
		 (if bool "true" "false")
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
	 (format "~a.write('~a'); }" tmp vid)))
   
   (define (source s)
      (let ((t (mime-type s "video/ogg")))
	 (instantiate::xml-empty-element
	    (markup 'SOURCE)
	    (attributes `(:src ,s :type ,t))
	    (body '()))))
   
   (let ((vid (symbol->string (gensym 'video-container))))
      (<DIV> :id vid
	 (cond
	    ((eq? backend 'flash)
	     (<flash> (gensym 'video_tmp) vid #t))
	    ((pair? src)
	     (instantiate::xml-element
		(id (xml-make-id id 'video))
		(markup 'VIDEO)
		(attributes attr)
		(body (append (map source src)
			      (list (<flash> (gensym 'video_tmp) vid #f))))))
	    (else
	     (instantiate::xml-element
		(id (xml-make-id id 'video))
		(markup 'VIDEO)
		(attributes `(:src ,src ,@attr))
		(body (list (<flash> (gensym 'video_tmp) vid #f)))))))))

