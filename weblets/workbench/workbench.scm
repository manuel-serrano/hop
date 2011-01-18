;*=====================================================================*/
;*    .../prgm/project/hop/2.2.x/weblets/workbench/workbench.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 21 06:00:36 2010                          */
;*    Last change :  Sat Dec 25 18:37:50 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Client side for the workbench                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module workbench_client
   $(import workbench)
   (export (set-log! v)
	   (event-key-set! key)
	   (load file)
	   (run url)
	   (server-event-handler event)
	   (url-keydown-handler input event)))

;*---------------------------------------------------------------------*/
;*    logp ...                                                         */
;*---------------------------------------------------------------------*/
(define logp #t)

;*---------------------------------------------------------------------*/
;*    key ...                                                          */
;*---------------------------------------------------------------------*/
(define key #f)

;*---------------------------------------------------------------------*/
;*    run-once ...                                                     */
;*---------------------------------------------------------------------*/
(define run-once #f)

;*---------------------------------------------------------------------*/
;*    event-key-set! ...                                               */
;*---------------------------------------------------------------------*/
(define (event-key-set! k)
   (set! key k))

;*---------------------------------------------------------------------*/
;*    set-log! ...                                                     */
;*---------------------------------------------------------------------*/
(define (set-log! v)
   (set! logp v))
   
;*---------------------------------------------------------------------*/
;*    load ...                                                         */
;*---------------------------------------------------------------------*/
(define (load file)
   (let ((el (dom-get-element-by-id "url")))
      (when (string=? el.value "")
	 (set! el.value (format "/hop/~a" (prefix (basename file))))))
   (when run-once
      (node-style-set! "iframemask" :display "block"))
   (with-hop ($workbench/load file key)
      (lambda (h)
	 (innerHTML-set!
	  "msg"
	  (<SPAN> :class (if h "succeeded" "failed")
	     (format "Loading file ~s" file))))))

;*---------------------------------------------------------------------*/
;*    run ...                                                          */
;*---------------------------------------------------------------------*/
(define (run url)
   (set! run-once #t)
   (let ((ifr (dom-get-element-by-id "iframe"))
	 (ifrm (dom-get-element-by-id "iframemask")))
      (set! ifr.src ($workbench/run url key))
      (node-style-set! ifrm :display "none")))

;*---------------------------------------------------------------------*/
;*    server-event-handler ...                                         */
;*---------------------------------------------------------------------*/
(define (server-event-handler event)
   (when logp
      (let ((el (dom-get-element-by-id "console")))
	 (dom-append-child! el event.value))))

;*---------------------------------------------------------------------*/
;*    url-keydown-handler ...                                          */
;*---------------------------------------------------------------------*/
(define (url-keydown-handler input event)
   (case (event-key-code event)
      ((9)
       ;; tab
       (if (and input.stamp (< (- event.timeStamp input.stamp) 180))
	   ;; double tab
	   (let* ((cw (main-window-width))
		  (ch (main-window-height))
		  (w (min (- cw 80) 850))
		  (h (min (- ch 80) 568)))
	      (window-open :parent document.body :id "filebrowser"
		 :width w :height h
		 :left (/ (- cw w) 2) :top (min 40 (/ (- ch h) 2))
		 :src (<FILECHOOSER>
			 :url (dirname input.value)
			 :oncancel (window-close "filebrowser")
			 :onopen (begin
				    (set! input.value this.value)
				    (window-close "filebrowser")
				    (load this.value)))))
	   (set! input.stamp event.timeStamp)))
      ((13)
       ;; return
       (load input.value))))
