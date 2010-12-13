;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/share/hop-spage.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  6 17:58:58 2010                          */
;*    Last change :  Mon Dec 13 08:42:11 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Client-side library for spage                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop-spage
   
   (JS     hop_add_native_event_listener
           HopEvent)
   
   (export (spage-init spage onchange)
	   (spage-resize spage)
	   (spage-push spage tab)
	   (spage-pop spage)
	   (spage-tab-update tab)
	   (spage-tab-pop tab)
	   (spage-pop-update el)
	   (spage-push-service tab svc)
	   (spage-push-node tab node)
	   (spage-head spage)
	   (spage-tab spage)))

;*---------------------------------------------------------------------*/
;*    spage-init ...                                                   */
;*---------------------------------------------------------------------*/
(define (spage-init spage onchange)
   (let* ((spage (if (string? spage) (dom-get-element-by-id spage) spage))
	  (childs (dom-child-nodes spage)))
      (set! spage.sphead (car childs))
      (set! spage.spstyle (cadr childs))
      (set! spage.spviewport (caddr childs))
      (set! spage.num 0)
      (set! spage.tabs (list (dom-first-child spage.spviewport)))
      (set! spage.heads '())
      (set! spage.onchange onchange)
      (set! spage.transitionstyle 'plain)
      (set! spage.hop_add_event_listener spage-add-event-listener!)
      (set! spage.hop_update (lambda () (spage-update this)))
      ;; adjust the body size
      (spage-resize spage)
      ;; set the transition effet
      (after 1
	 (lambda ()
	    (node-style-set! spage.spviewport
	       :-webkit-transition-property "all"
	       :-moz-transition-property "all"
	       :-o-transition-property "all")))))

;*---------------------------------------------------------------------*/
;*    spage-update ...                                                 */
;*---------------------------------------------------------------------*/
(define (spage-update spage)
   (spage-resize spage)
   (for-each hop-update (dom-child-nodes spage)))

;*---------------------------------------------------------------------*/
;*    spage-resize ...                                                 */
;*---------------------------------------------------------------------*/
(define (spage-resize spage)
   (let ((spage (if (string? spage) (dom-get-element-by-id spage) spage)))
      (set! spage.spwidth (- spage.clientWidth (frameBorderWidth spage.spviewport)))
      (set! spage.spheight (- spage.clientHeight (frameBorderHeight spage.spviewport)
			      spage.sphead.offsetHeight))
      (set! spage.spscrollwidth (*fx (+fx spage.num 1) spage.spwidth))
      (set! spage.spoffset (*fx spage.num spage.spwidth))
      (node-style-set! spage.spviewport
	 :width (format "~apx" spage.spscrollwidth)
	 :height (format "~apx" spage.spheight))
      (node-style-set! (dom-first-child spage.spviewport)
	 :width (format "~apx" spage.spwidth)
	 :height (format "~apx" spage.spheight))
      (when (eq? spage.transitionstyle 'slide)
	 (node-style-set! (dom-first-child spage.spviewport)
	    :left (format "~apx"spage.spoffset )))))
   
;*---------------------------------------------------------------------*/
;*    spage-add-event-listener! ...                                    */
;*---------------------------------------------------------------------*/
(define (spage-add-event-listener! obj event proc capture)
   (if (string=? event "onchange")
       (set! obj.onchange proc)
       (hop_add_native_event_listener obj event proc capture)))

;*---------------------------------------------------------------------*/
;*    spage-invoke-onchange-listener! ...                              */
;*---------------------------------------------------------------------*/
(define (spage-invoke-onchange-listener! spage tab)
   (when (procedure? spage.onchange)
      (let ((evt (new HopEvent "change" tab)))
	 (set! evt.target tab)
	 (spage.onchange evt))))

;*---------------------------------------------------------------------*/
;*    spage-transition-style ...                                       */
;*---------------------------------------------------------------------*/
(define (spage-transition-style spage)
   (string->symbol (node-computed-style-get spage.spstyle :cursor)))

;*---------------------------------------------------------------------*/
;*    frameHeight ...                                                  */
;*---------------------------------------------------------------------*/
(define (frameHeight el)
   (+ (string->integer (node-computed-style-get el "marginTop"))
      (string->integer (node-computed-style-get el "marginBottom"))
      (string->integer (node-computed-style-get el "borderBottomWidth"))
      (string->integer (node-computed-style-get el "borderTopWidth"))))

;*---------------------------------------------------------------------*/
;*    frameBorderHeight ...                                            */
;*---------------------------------------------------------------------*/
(define (frameBorderHeight el)
   (+ (string->integer (node-computed-style-get el "borderBottomWidth"))
      (string->integer (node-computed-style-get el "borderTopWidth"))))

;*---------------------------------------------------------------------*/
;*    frameWidth ...                                                   */
;*---------------------------------------------------------------------*/
(define (frameWidth el)
   (+ (string->integer (node-computed-style-get el "marginLeft"))
      (string->integer (node-computed-style-get el "marginRight"))
      (string->integer (node-computed-style-get el "borderRightWidth"))
      (string->integer (node-computed-style-get el "borderLeftWidth"))))

;*---------------------------------------------------------------------*/
;*    frameBorderWidth ...                                             */
;*---------------------------------------------------------------------*/
(define (frameBorderWidth el)
   (+ (string->integer (node-computed-style-get el "borderRightWidth"))
      (string->integer (node-computed-style-get el "borderLeftWidth"))))

;*---------------------------------------------------------------------*/
;*    css-transition-duration ...                                      */
;*---------------------------------------------------------------------*/
(define (css-transition-duration el)
   
   (define (css-get el key)
      (let ((v (node-computed-style-get el key)))
	 (and (string? v) (string->real v))))
   
   (or (css-get el "transition-duration")
       (css-get el "-webkit-transition-duration")
       (css-get el "-firefox-transition-duration")
       (css-get el "-o-transition-duration")))

;*---------------------------------------------------------------------*/
;*    fade ...                                                         */
;*---------------------------------------------------------------------*/
(define (fade el el2 duration val0 val1 proc)
   (let* ((width (- val1 val0))
	  (n (/ duration 10))
	  (inc (/ width n)))
      (timeout 10
	 (lambda ()
	    (if (>= n 0)
		(begin
		   (set! val0 (+ val0 inc))
		   (node-style-set! el :opacity val0)
		   (node-style-set! el2 :opacity (- 1 val0))
		   (set! n (-fx n 1))
		   #t)
		(begin
		   (node-style-set! el :opacity val1)
		   (node-style-set! el2 :opacity (- 1 val1))
		   (when (procedure? proc) (proc el))
		   #f))))))

;*---------------------------------------------------------------------*/
;*    slide ...                                                        */
;*---------------------------------------------------------------------*/
(define (slide el duration offset0 offset1 proc)
   (let* ((width (- offset1 offset0))
	  (n (/ duration 10))
	  (inc (/ width n)))
      (timeout 10
	 (lambda ()
	    (if (>= n 0)
		(begin
		   (set! offset0 (round (+ offset0 inc)))
		   (node-style-set! el
		      :left (string-append "-" (integer->string offset0) "px"))
		   (set! n (- n 1))
		   #t)
		(begin
		   (node-style-set! el
		      :left (string-append "-" (integer->string offset1) "px"))
		   (when (procedure? proc) (proc el))
		   #f))))))
		       
;*---------------------------------------------------------------------*/
;*    spage-push ...                                                   */
;*---------------------------------------------------------------------*/
(define (spage-push spage tab)
   
   (define (spage-push-plain spage spviewport tab)
      ;; mark the transition style (needed on resize)
      (set! spage.transitionstyle 'plain)
      (node-style-set! tab :z-index spage.num :left 0))

   (define (spage-push-slide spage spviewport tab)
      ;; mark the transition style (needed on resize)
      (set! spage.transitionstyle 'slide)
      ;; expand the body div when necessary
      (set! spage.spscrollwidth (+ spage.spwidth spage.spoffset))
      ;; add the tab and push the view
      (node-style-set! tab
	 :left (format "~apx" spage.spoffset))
      (if (hop-config 'css_transition)
	  (node-style-set! spviewport
	     :left (format "-~apx" spage.spoffset)
	     :width (format "~apx" spage.spscrollwidth))
	  (let ((offset0 (- spage.spoffset spage.spwidth))
		(offset1 spage.spoffset))
	     (node-style-set! spviewport
		:width (format "~apx" spage.spscrollwidth))
	     (slide spviewport (or (css-transition-duration tab) 400) offset0 offset1 list))))

   (define (spage-push-fade spage spviewport tab)
      ;; mark the transition style (needed on resize)
      (set! spage.transitionstyle 'fade)
      (node-style-set! tab
	 :-webkit-transition-property "none"
	 :-moz-transition-property "none"
	 :-o-transition-property "none"
	 :opacity 0
	 :z-index spage.num :left 0)
      (if (hop-config 'css_transition)
	  (after 1 (lambda ()
		      (node-style-set! tab
			 :-webkit-transition-property "all"
			 :-moz-transition-property "all"
			 :-o-transition-property "all"
			 :opacity 1)
		      (node-style-set! (cadr spage.tabs)
			 :-webkit-transition-property "all"
			 :-moz-transition-property "all"
			 :-o-transition-property "all"
			 :opacity 0)))
	  (fade tab (cadr spage.tabs)
		(or (css-transition-duration tab) 400) 0 1 #f)))

   (define (spage-push-auto spage spviewport tab)
      (cond
	 ((< (hop-config :cpu_speed) 60)
	  (spage-push-plain spage spviewport tab))
	 ((hop-config 'css_transition)
	  (spage-push-slide spage spviewport tab))
	 (else
	  (spage-push-plain spage spviewport tab))))
   
   (let* ((spage (if (string? spage) (dom-get-element-by-id spage) spage))
	  (spviewport spage.spviewport)
	  (tab (if (string? tab) (dom-get-element-by-id tab) tab)))
      ;; increment the number of pushed elements
      (set! spage.num (+fx spage.num 1))
      (set! spage.tabs (cons tab spage.tabs))
      (set! spage.spoffset (+fx spage.spoffset spage.spwidth))
      ;; set the tab dimension
      (dom-append-child! spviewport tab)
      (node-style-set! tab
	 :right 0 :left 0
	 :top 0 :bottom 0
	 :width (format "~apx" (- spage.spwidth (frameWidth tab)))
	 :height (format "~apx" (- spage.spheight (frameHeight tab))))
;*       (spage.hop_update)                                            */
      (spage-invoke-onchange-listener! spage tab)
      ;; show the new tab
      (case (spage-transition-style spage)
	 ((move)
	  (spage-push-slide spage spviewport tab))
	 ((help)
	  (spage-push-fade spage spviewport tab))
	 ((auto)
	  (spage-push-auto spage spviewport tab))
	 (else
	  (spage-push-plain spage spviewport tab)))))

;*---------------------------------------------------------------------*/
;*    spage-pop ...                                                    */
;*---------------------------------------------------------------------*/
(define (spage-pop spage)

   (define (spage-pop-plain spage spviewport tab)
      (dom-remove-child! spviewport tab)
;*       (spage.hop_update)                                            */
      (spage-invoke-onchange-listener! spage tab))
   
   (define (spage-pop-fade spage spviewport tab)
      (if (hop-config 'css_transition)
	  (let ((d (css-transition-duration tab)))
	     (begin
		(node-style-set! tab :opacity 0)
		(node-style-set! (car spage.tabs) :opacity 1))
	     (after (+ 100 (round (* 1000 d)))
		(lambda ()
		   (node-style-set! tab
		      :-webkit-transition-property "none"
		      :-moz-transition-property "none"
		      :-o-transition-property "none")
		   (dom-remove-child! spviewport tab)
		   (spage-invoke-onchange-listener! spage tab))))
	  (fade tab (car spage.tabs) (or (css-transition-duration tab) 400) 1 0
		(lambda (tab)
		   (dom-remove-child! (dom-parent-node tab) tab)
;* 		   (spage.hop_update)                                  */
		   (spage-invoke-onchange-listener! spage tab)))))
   
   (define (spage-pop-slide spage spviewport tab)
      (set! spage.spoffset (-fx spage.spoffset spage.spwidth))
      (set! spage.spscrollwidth (+ spage.spwidth spage.spoffset))
      (if (hop-config 'css_transition)
	  (begin
	     (node-style-set! spviewport
		:left (format "-~apx" spage.spoffset)
		:width (format "~apx" spage.spscrollwidth))
	     (let ((d (css-transition-duration tab)))
		(after (+ 100 (round (* 1000 d)))
		   (lambda ()
		      (dom-remove-child! spviewport tab)
		      (spage-invoke-onchange-listener! spage tab)))))
	  (let ((offset0 (+ spage.spoffset spage.spwidth))
		(offset1 spage.spoffset))
	     (node-style-set! spviewport
		:width (format "~apx" spage.spscrollwidth))
	     (slide spviewport (or (css-transition-duration tab) 400) offset0 offset1
		    (lambda (el)
		       (dom-remove-child! (dom-parent-node tab) tab)
;* 		       (spage.hop_update)                              */
		       (spage-invoke-onchange-listener! spage tab))))))

   (define (spage-pop-auto spage spviewport tab)
      (cond
	 ((< (hop-config :cpu_speed) 60)
	  (spage-pop-plain spage spviewport tab))
	 ((hop-config 'css_transition)
	  (spage-pop-slide spage spviewport tab))
	 (else
	  (spage-pop-plain spage spviewport tab))))
   
   (let* ((spage (if (string? spage) (dom-get-element-by-id spage) spage))
	  (spviewport spage.spviewport))
      (when (pair? spage.tabs)
	 (let ((tab (car spage.tabs)))
	    ;; decrement the number of pushed elements
	    (set! spage.num (-fx spage.num 1))
	    (set! spage.tabs (cdr spage.tabs))
	    ;; pop the element from the gui
	    (case (spage-transition-style spage)
	       ((move)
		(spage-pop-slide spage spviewport tab))
	       ((help)
		(spage-pop-fade spage spviewport tab))
	       ((auto)
		(spage-pop-auto spage spviewport tab))
	       (else
		(spage-pop-plain spage spviewport tab)))))))

;*---------------------------------------------------------------------*/
;*    find-spage ...                                                   */
;*---------------------------------------------------------------------*/
(define (find-spage el)
   (let loop ((parent (dom-parent-node el)))
      (cond
	 ((string=? (parent.getAttribute "hssclass") "hop-spage")
	  parent)
	 ((or (not parent) (eq? parent #unspecified))
	  (error "find-spage" "cannot find parent spage" el))
	 (else
	  (loop (dom-parent-node parent))))))

;*---------------------------------------------------------------------*/
;*    spage-pop-update ...                                             */
;*---------------------------------------------------------------------*/
(define (spage-pop-update button)
   (let* ((spage (find-spage button))
	  (head (car spage.heads))
	  (sphead spage.sphead)
	  (spheadcontent (dom-first-child sphead))
	  (spheadbutton (dom-last-child sphead)))
      (set! spage.heads (cdr spage.heads))
      (innerHTML-set! (dom-first-child spheadbutton) (car head))
      
      (innerHTML-set! spheadcontent (cdr head))
      (spage-pop spage)
      (when (= spage.num 0)
	 (set! spheadbutton.className ""))))
   
;*---------------------------------------------------------------------*/
;*    spage-push-service ...                                           */
;*---------------------------------------------------------------------*/
(define (spage-push-service tab svc)
   (with-hop (svc)
      (lambda (body)
	 (let* ((spage (find-spage tab))
		(sphead spage.sphead)
		(spheadcontent (dom-first-child sphead))
		(spheadbutton (dom-last-child sphead))
		(tabhead (dom-first-child (dom-first-child tab))))
	    (set! tab.svc svc)
	    (set! spage.heads (cons (cons spheadbutton.innerHTML
					  spheadcontent.innerHTML)
				    spage.heads))
	    (innerHTML-set! (dom-first-child spheadbutton) spheadcontent.innerHTML)
	    (innerHTML-set! spheadcontent tabhead.innerHTML)
	    (set! spheadbutton.className "visible")
	    (spage-push spage body)))))
	    
;*---------------------------------------------------------------------*/
;*    spage-push-node ...                                              */
;*---------------------------------------------------------------------*/
(define (spage-push-node tab node)
   (let* ((body (dom-clone-node node #t))
	  (spage (find-spage tab))
	  (sphead spage.sphead)
	  (spheadcontent (dom-first-child sphead))
	  (spheadbutton (dom-last-child sphead))
	  (tabhead (dom-first-child (dom-first-child tab))))
      (set! spage.heads (cons (cons spheadbutton.innerHTML
				    spheadcontent.innerHTML)
			      spage.heads))
      (innerHTML-set! (dom-first-child spheadbutton) spheadcontent.innerHTML)
      (innerHTML-set! spheadcontent tabhead.innerHTML)
      (set! spheadbutton.className "visible")
      (spage-push spage body)))
	    
;*---------------------------------------------------------------------*/
;*    spage-tab-update ...                                             */
;*---------------------------------------------------------------------*/
(define (spage-tab-update tab)
   (let ((tab (if (string? tab) (dom-get-element-by-id tab) tab)))
      (when tab.svc
	 (with-hop (tab.svc)
	    (lambda (body)
	       (let* ((spage (find-spage tab))
		      (spviewport spage.spviewport))
		  (dom-remove-child! spviewport (dom-last-child spviewport))
		  (dom-append-child! spviewport body)))))))

;*---------------------------------------------------------------------*/
;*    spage-tab-pop ...                                                */
;*---------------------------------------------------------------------*/
(define (spage-tab-pop tab)
   (let ((tab (if (string? tab) (dom-get-element-by-id tab) tab)))
      (spage-pop (find-spage tab))))

;*---------------------------------------------------------------------*/
;*    spage-head ...                                                   */
;*---------------------------------------------------------------------*/
(define (spage-head spage)
   (dom-first-child spage.sphead))

;*---------------------------------------------------------------------*/
;*    spage-tab ...                                                    */
;*---------------------------------------------------------------------*/
(define (spage-tab spage)
   (dom-first-child spage.spcontent))
