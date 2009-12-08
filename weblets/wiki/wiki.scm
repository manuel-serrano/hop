;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/weblets/wiki/wiki.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 10 09:57:17 2009                          */
;*    Last change :  Tue Dec  8 10:33:42 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Wiki runtime system                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    wiki-key-modifier ...                                            */
;*---------------------------------------------------------------------*/
(define wiki-key-modifier #f)
(define wiki-anim-step 0.3)
(define wiki-anim-speed 5)

;*---------------------------------------------------------------------*/
;*    wiki-keyup-handler ...                                           */
;*---------------------------------------------------------------------*/
(define (wiki-keyup-handler event)
   (when (= event.which 18) (set! wiki-key-modifier #f)))

;*---------------------------------------------------------------------*/
;*    wiki-keydown-handler ...                                         */
;*---------------------------------------------------------------------*/
(define (wiki-keydown-handler event)
   (let ((k event.which))
      (case k
	 ((18)
	  ;; alt
	  (set! wiki-key-modifier 'alt))
	 ((36)
	  ;; home
	  (window.scrollTo 0 0)
	  (stop-event-propagation event #f))
	 ((35)
	  ;; end
	  (window.scrollTo 0 (hop_element_y (dom-get-element-by-id "wiki-foot")))
	  (stop-event-propagation event #f))
	 ((82)
	  ;; r
	  (when (eq? wiki-key-modifier 'alt)
	     (location.reload)
	     (stop-event-propagation event #f)))
	 ((83)
	  ;; s
	  (when (eq? wiki-key-modifier 'alt)
	     (let ((el (dom-get-element-by-id "wiki-toc-popup")))
		(if (equal? (node-style-get el :display) "block")
		    (wiki-hide-toc-popup el)
		    (wiki-show-toc-popup el)))
	     (stop-event-propagation event #f))))))

;*---------------------------------------------------------------------*/
;*    wiki-show-toc-popup ...                                          */
;*---------------------------------------------------------------------*/
(define (wiki-show-toc-popup el)
   (let ((m (format "~apx" (/ (window-height) 4))))
      (node-style-set! el :left m)
      (node-style-set! el :right m)
      (let ((opacity (node-computed-style-get el :opacity)))
	 (if opacity
	     (let ((i 0))
		(node-style-set! el :opacity 0)
		(node-style-set! el :display "block")
		(timeout wiki-anim-speed
			 (lambda ()
			    (node-style-set! el :opacity i)
			    (if (> (- opacity i) wiki-anim-step)
				(begin
				   (set! i (+ i wiki-anim-step))
				   #t)
				(begin
				   (node-style-set! el :opacity opacity)
				   #f)))))
	     (node-style-set! el :display "block")))))

;*---------------------------------------------------------------------*/
;*    wiki-hide-menu ...                                               */
;*---------------------------------------------------------------------*/
(define (wiki-hide-toc-popup el)
   (let ((opacity (node-computed-style-get el :opacity)))
      (if opacity
	  (let ((i 0))
	     (timeout wiki-anim-speed
		      (lambda ()
			 (node-style-set! el :opacity i)
			 (if (> i wiki-anim-step)
			     (begin
				(set! i (- i wiki-anim-step))
				#t)
			     (begin
				(node-style-set! el :display "none")
				(node-style-set! el :opacity opacity)
				#f)))))
	  (node-style-set! el :display "none"))))

;*---------------------------------------------------------------------*/
;*    event handlers                                                   */
;*---------------------------------------------------------------------*/
(add-event-listener! document "keydown" wiki-keydown-handler #t)
(add-event-listener! document "keyup" wiki-keyup-handler))
