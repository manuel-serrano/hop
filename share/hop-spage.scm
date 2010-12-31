;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/share/hop-spage.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  6 17:58:58 2010                          */
;*    Last change :  Thu Dec 30 16:47:22 2010 (serrano)                */
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
	   (sptab-add-event-listener! obj event proc capture)
	   (spage-push spage tab tbody)
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
      (set! spage.transitionstyle 'none)
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
   (let ((spage (if (string? spage) (dom-get-element-by-id spage) spage))
	 (spbody (car (dom-child-nodes spage.spviewport))))
      (set! spage.spwidth spage.clientWidth)
      (set! spage.spbodywidth (- spage.clientWidth (frameBorderWidth spbody)))
      (set! spage.spbodyheight (- spage.clientHeight (frameBorderHeight spbody)
				  spage.sphead.offsetHeight))
      ;; webkit got the animation wrong if the viewport is just not
      ;; larger enough before adding a new tab
      (set! spage.spscrollwidth (*fx (+fx spage.num 2) spage.spwidth))
      (set! spage.spoffset (*fx spage.num spage.spwidth))
      ;; we have to enforce the page size otherwise the browsers
      ;; use the viewport width for the containing block width
      (node-style-set! spage
	 :width (format "~apx" spage.spwidth))
      (node-style-set! spage.spviewport
	 :width (format "~apx" spage.spscrollwidth))
      (node-style-set! (dom-first-child spage.spviewport)
	 :width (format "~apx" spage.spbodywidth))
      (when (eq? spage.transitionstyle 'slide)
	 (node-style-set! (dom-first-child spage.spviewport)
	    :left (format "~apx"spage.spoffset)))))
   
;*---------------------------------------------------------------------*/
;*    spage-add-event-listener! ...                                    */
;*---------------------------------------------------------------------*/
(define (spage-add-event-listener! obj event proc capture)
   (if (string=? event "onchange")
       (set! obj.onchange proc)
       (hop_add_native_event_listener obj event proc capture)))

;*---------------------------------------------------------------------*/
;*    sptab-add-event-listener! ...                                    */
;*---------------------------------------------------------------------*/
(define (sptab-add-event-listener! obj event proc capture)
   (if (string=? event "onselect")
       (set! obj.onselect proc)
       (hop_add_native_event_listener obj event proc capture)))

;*---------------------------------------------------------------------*/
;*    spage-invoke-onchange-listener! ...                              */
;*---------------------------------------------------------------------*/
(define (spage-invoke-onchange-listener! spage tbody)
   (when (procedure? spage.onchange)
      (let ((evt (new HopEvent "change" spage)))
	 (set! evt.target tbody)
	 (spage.onchange evt))))

;*---------------------------------------------------------------------*/
;*    sptab-invoke-onselect-listener! ...                              */
;*---------------------------------------------------------------------*/
(define (sptab-invoke-onselect-listener! sptab tbody action)
   (when (procedure? sptab.onselect)
      (let ((evt (new HopEvent "select" sptab)))
	 (set! evt.target tbody)
	 (set! evt.action action)
	 (sptab.onselect evt))))
   
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
(define (spage-push spage tab tbody)
   
   (define (spage-push-none spage spviewport tbody)
      ;; mark the transition style (needed on resize)
      (set! spage.transitionstyle 'none)
      (node-style-set! (cadr spage.tabs)
	 :opacity 0)
      (node-style-set! tbody
	 :z-index spage.num
	 :left (format "-~apx" (* spage.num spage.spwidth))))

   (define (spage-push-slide spage spviewport tbody)
      ;; mark the transition style (needed on resize)
      (set! spage.transitionstyle 'slide)
      (if (hop-config 'css_transition)
	  (node-style-set! spviewport
	     :left (format "-~apx" spage.spoffset))
	  (let ((offset0 (- spage.spoffset spage.spwidth))
		(offset1 spage.spoffset))
	     (slide spviewport (or (css-transition-duration tbody) 400) offset0 offset1 #f))))

   (define (spage-push-fade spage spviewport tbody)
      ;; mark the transition style (needed on resize)
      (set! spage.transitionstyle 'fade)
      (node-style-set! tbody
	 :-webkit-transition-property "none"
	 :-moz-transition-property "none"
	 :-o-transition-property "none"
	 :opacity 0
	 :z-index spage.num
	 :left (format "-~apx" (* spage.num spage.spwidth)))
      (if (hop-config 'css_transition)
	  (after 1 (lambda ()
		      (node-style-set! tbody
			 :-webkit-transition-property "opacity"
			 :-moz-transition-property "opacity"
			 :-o-transition-property "opacity"
			 :opacity 1)
		      (node-style-set! (cadr spage.tabs)
			 :-webkit-transition-property "opacity"
			 :-moz-transition-property "opacity"
			 :-o-transition-property "opacity"
			 :opacity 0)))
	  (fade tbody (cadr spage.tabs)
		(or (css-transition-duration tbody) 400) 0 1 #f)))

   (define (spage-push-auto spage spviewport tbody)
      (cond
	 ((< (hop-config :cpu_speed) 60)
	  (spage-push-none spage spviewport tbody))
	 ((hop-config 'css_transition)
	  (spage-push-slide spage spviewport tbody))
	 (else
	  (spage-push-none spage spviewport tbody))))
   
   (let* ((spage (if (string? spage) (dom-get-element-by-id spage) spage))
	  (tab (if (string? tab) (dom-get-element-by-id tab) tab))
	  (spviewport spage.spviewport)
	  (tbody (if (string? tbody) (dom-get-element-by-id tbody) tbody)))
      ;; increment the number of pushed elements
      (set! spage.num (+fx spage.num 1))
      (set! spage.tabs (cons tbody spage.tabs))
      (set! spage.spoffset (+fx spage.spoffset spage.spwidth))
      (set! tbody.tab tab)
      ;; expand the body div when necessary
      (set! spage.spscrollwidth (+ spage.spscrollwidth spage.spoffset))
      (node-style-set! spviewport
	 :width (format "~apx" spage.spscrollwidth))
      ;; set the tab dimension
      (dom-append-child! spviewport tbody)
      (node-style-set! tbody
	 :width (format "~apx" spage.spbodywidth))
      ;; the event listeners
      (spage-invoke-onchange-listener! spage tbody)
      (when tbody.tab
	 (sptab-invoke-onselect-listener! tbody.tab tbody "push"))
      ;; show the new tbody
      (case (spage-transition-style spage)
	 ((move)
	  (spage-push-slide spage spviewport tbody))
	 ((help)
	  (spage-push-fade spage spviewport tbody))
	 ((auto)
	  (spage-push-auto spage spviewport tbody))
	 (else
	  (spage-push-none spage spviewport tbody)))))

;*---------------------------------------------------------------------*/
;*    spage-pop ...                                                    */
;*---------------------------------------------------------------------*/
(define (spage-pop spage)

   (define (shrink-viewport spviewport)
      (set! spage.spscrollwidth (- spage.spscrollwidth spage.spoffset))
      (node-style-set! spviewport
	 :width (format "~apx" spage.spscrollwidth)))

   (define (invoke-listeners spage tbody)
      (spage-invoke-onchange-listener! spage tbody)
      (when (and tbody.tab (not (eq? tbody.tab #unspecified)))
	 (sptab-invoke-onselect-listener! tbody.tab tbody "pop")))
   
   (define (spage-pop-none spage spviewport tbody)
      (dom-remove-child! spviewport tbody)
      (node-style-set! (car spage.tabs) :opacity 1)
      (shrink-viewport spviewport))
   
   (define (spage-pop-fade spage spviewport tbody)
      (if (hop-config 'css_transition)
	  (let ((d (css-transition-duration tbody)))
	     (begin
		(node-style-set! tbody :opacity 0)
		(node-style-set! (car spage.tabs) :opacity 1))
	     (after (+ 100 (round (* 1000 d)))
		(lambda ()
		   (node-style-set! tbody
		      :-webkit-transition-property "none"
		      :-moz-transition-property "none"
		      :-o-transition-property "none")
		   (dom-remove-child! spviewport tbody)
		   (shrink-viewport spviewport))))
	  (fade tbody (car spage.tabs) (or (css-transition-duration tbody) 400) 1 0
		(lambda (tbody)
		   (dom-remove-child! (dom-parent-node tbody) tbody)
		   (shrink-viewport spviewport)))))
   
   (define (spage-pop-slide spage spviewport tbody)
      (if (hop-config 'css_transition)
	  (let ((d (css-transition-duration tbody)))
	     (node-style-set! spviewport
		:left (format "-~apx" spage.spoffset))
	     (after (+ 100 (round (* 1000 d)))
		(lambda ()
		   (dom-remove-child! spviewport tbody)
		   (node-style-set! spviewport
		      :width (format "~apx" spage.spscrollwidth)))))
	  (let ((offset0 (+ spage.spoffset spage.spwidth))
		(offset1 spage.spoffset))
	     (slide spviewport (or (css-transition-duration tbody) 400) offset0 offset1
		    (lambda (el)
		       (dom-remove-child! (dom-parent-node tbody) tbody)
		       (shrink-viewport spviewport))))))

   (define (spage-pop-auto spage spviewport tbody)
      (cond
	 ((< (hop-config :cpu_speed) 60)
	  (spage-pop-none spage spviewport tbody))
	 ((hop-config 'css_transition)
	  (spage-pop-slide spage spviewport tbody))
	 (else
	  (spage-pop-none spage spviewport tbody))))
   
   (let* ((spage (if (string? spage) (dom-get-element-by-id spage) spage))
	  (spviewport spage.spviewport))
      (when (pair? spage.tabs)
	 (let ((tbody (car spage.tabs)))
	    ;; decrement the number of pushed elements
	    (set! spage.num (-fx spage.num 1))
	    (set! spage.tabs (cdr spage.tabs))
	    (set! spage.spoffset (-fx spage.spoffset spage.spwidth))
	    ;; invoke the listener before removing any node
	    (when (pair? spage.tabs)
	       (invoke-listeners spage (car spage.tabs)))
	    ;; pop the element from the gui
	    (case (spage-transition-style spage)
	       ((move)
		(spage-pop-slide spage spviewport tbody))
	       ((help)
		(spage-pop-fade spage spviewport tbody))
	       ((auto)
		(spage-pop-auto spage spviewport tbody))
	       (else
		(spage-pop-none spage spviewport tbody)))))))

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
	    (spage-push spage tab body)))))
	    
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
      (spage-push spage tab body)))
	    
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
