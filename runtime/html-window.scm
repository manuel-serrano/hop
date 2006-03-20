;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/html-window.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Wed Mar  1 12:23:29 2006                          */
;*    Last change :  Thu Mar 16 13:56:23 2006 (eg)                     */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of <HOP-WINDOW>.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-window

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_xml)


   (export  (<FLOAT-WINDOW> . ::obj)))


;*---------------------------------------------------------------------*/
;*    <FLOAT-WINDOW> ...                                               */
;*---------------------------------------------------------------------*/
(define-xml-compound <FLOAT-WINDOW> ((id #unspecified string)
				     (class #f)
				     (title "")
				     (width #f)
				     (height #f)
				     (visible #f)
				     (in-frame #f)
				     body)
  (define (<TD-BORDER> img)
    (<TD> :width 8 :height 8
	  (if img (<IMG> :src (format "~A/~A"(hop-icons-directory) img)) "")))
  
  (define (<TD-EAST> img)
    (<TD> :width 8
	  :style (format "background-image:url(~a/~a);background-repeat:repeat-y"
			 (hop-icons-directory) img)))
  
  (define (<TD-SOUTH> img)
    (<TD> :height 8 :width "100%"
	  :style (format "background-image:url(~a/~a);background-repeat:repeat-x"
			 (hop-icons-directory) img)))
  
  (let* ((id         (xml-make-id id 'HOP-WINDOW))
	 (cls        (or class "hop-float-window"))
	 (handle-id  (string-append id "-handle"))
	 (content-id (string-append id "-content"))
	 (resize-id  (string-append id "-rsz")))
    (list 
      (<DIV> :id id :class cls
	     :style (string-append
		        (if width  (format "width: ~A; " width) "")
			(if height (format "height: ~A; " height) "")
			(format "display:~a" (if visible "block" "none")))
	(<TABLE> :width "100%" :height "100%" :cellpadding 0 :cellspacing 0
		 :border 0 :rules "none"
	 (<TR>	; Line #1
	  (<TD> :colspan 3 :rowspan 3 :valign "top"
	    (<TABLE> :class "hop-float-handle" :id handle-id :width "100%"
		     :cellpadding 0 :cellspacing 0 :border 0 :rules "none"
	      (<TR> :style (format "background-image: url('~a/window-handle.png')"
				   (hop-icons-directory))
	       (<TD> :class "hop-float-title" :width "100%" title)
	       (<TD> :class "hop-float-close-button"
		     (<A> :onclick (format "hop_close_float_window(~s)" id)
			  (<B> "X")))))
	    (<DIV> :class "hop-float-content" :id content-id body))
	  (<TD-BORDER> #f))

	 (<TR>	; Line #2
	  (<TD-BORDER> "shadow-ne.png"))

	 (<TR>	; Line #3
	  (<TD-EAST> "shadow-e.png"))

	 (<TR>	; Line #4
	  (<TD-BORDER> #f) (<TD-BORDER> "shadow-sw.png")
	  (<TD-SOUTH> "shadow-s.png") (<TD-BORDER> "shadow-se.png")))
	(<IMG> :src (make-file-path (hop-icons-directory) "resize-grip.png")
	       :class "hop-float-resize" :id resize-id))
      (<SCRIPT> :type "text/javascript"
		(format "hop_float_window_init(~s, ~a)"
			id
			(if in-frame "true" "false"))))))

