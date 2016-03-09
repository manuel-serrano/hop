;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/widget/video.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 29 08:37:12 2007                          */
;*    Last change :  Tue Feb  9 14:26:07 2016 (serrano)                */
;*    Copyright   :  2007-16 Manuel Serrano                            */
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
(define-tag <VIDEO> ((id #unspecified)
		     (src #f)
		     (img #f)
		     (width "auto")
		     (height "auto")
		     (bg #f)
		     (backend 'html5)
		     (attr)
		     body)
   
   (define (<flash> src vid bool)
      (let ((tmp (gensym 'flv)))
	 (<SCRIPT> :type "text/javascript"
	    (format "if( ~a || !hop_config.html5_video ) {
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
	    (format "~a.write('~a'); }" tmp vid))))
   
   (define (flv body)
      (filter-map (lambda (x)
		     (when (xml-markup-is? x 'source)
			(let ((src (dom-get-attribute x "src"))
			      (typ (dom-get-attribute x "type")))
			   (cond
			      ((string? typ)
			       (when (substring-at? typ "video/x-flv" 0)
				  src))
			      ((is-suffix? src "flv")
			       src)))))
		  body))
   
   (let ((vid (symbol->string (gensym 'video-container)))
	 (src (xml-primitive-value src)))
      (cond
	 ((eq? backend 'flash)
	  (<DIV> :id vid
	     (<flash> src vid #t)))
	 ((eq? backend 'html5)
	  (instantiate::xml-element
	     (id (xml-make-id (xml-primitive-value id) 'video))
	     (tag 'VIDEO)
	     (attributes `(:src ,src :width ,width :height ,height
			     ,@(map xml-primitive-value attr)))
	     (body body)))
	 ((string? src)
	  (if (is-suffix? src "flv")
	      (<DIV> :id vid
		 (<flash> src vid #t))
	      (instantiate::xml-element
		 (id (xml-make-id id 'video))
		 (tag 'VIDEO)
		 (attributes `(:src ,src :width ,width :height ,height
				 ,@(map xml-primitive-value attr)))
		 (body body))))
	 ((flv body)
	  =>
	  (lambda (els)
	     (instantiate::xml-element
		(id vid)
		(tag 'VIDEO)
		(attributes `(:width ,width :height ,height ,@attr))
		(body (append body (list (<flash> (car els) vid #f)))))))
	 (else
	  (instantiate::xml-element
	     (id vid)
	     (tag 'VIDEO)
	     (attributes `(:width ,width :height ,height ,@attr))
	     (body body))))))

