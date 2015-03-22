;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/share/hop-spage.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  6 17:58:58 2010                          */
;*    Last change :  Sun Mar 22 07:35:17 2015 (serrano)                */
;*    Copyright   :  2010-15 Manuel Serrano                            */
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
	   (spage-reset-size spage)
	   (spage-resize spage)
	   (sptab-add-event-listener! obj event proc capture)
	   (spage-push spage tab tbody)
	   (spage-pop spage #!optional kont)
	   (spage-pop-all spage)
	   (spage-tab-update tab)
	   (spage-tab-pop tab)
	   (spage-tab-push tab)
	   (spage-pop-update el)
	   (spage-push-service tab svc)
	   (spage-push-node tab node)
	   (spage-head spage)
	   (spage-tab spage)
	   (spage-depth spage)
	   (spage-current-tab spage)
	   (find-spage id)
	   (find-sptab id)))

;*---------------------------------------------------------------------*/
;*    spage-init ...                                                   */
;*---------------------------------------------------------------------*/
(define (spage-init spage onchange)
   (let* ((spage (if (string? spage) (dom-get-element-by-id spage) spage))
	  (childs (dom-child-nodes spage)))
      (set! spage.sphead (car childs))
      (set! spage.spstyle (cadr childs))
      (set! spage.spwindow (caddr childs))
      (set! spage.spviewport (dom-first-child spage.spwindow))
      (set! spage.num 0)
      (set! spage.tabs (list (dom-first-child spage.spviewport)))
      (set! spage.heads '())
      (set! spage.inpop #f)
      (set! spage.onchg onchange)
      (set! spage.transitionstyle 'none)
      (set! spage.hop_add_event_listener spage-add-event-listener!)
      (set! spage.hop_update (lambda () (spage-update this)))
      ;; adjust the body size
      (spage-reset-size spage)
      ;; set the transition effet
      (after 1
	 (lambda ()
	    (node-style-set! spage.spviewport
	       :-webkit-transition-property "all"
	       :-moz-transition-property "all"
	       :-o-transition-property "all"
	       :transition-property "all")))))

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
   (let* ((spage (if (string? spage) (dom-get-element-by-id spage) spage))
	  (spbody (car (dom-child-nodes spage.spviewport)))
	  (parent (dom-parent-node spage))
	  (cwidth (- spage.clientWidth (frameBorderWidth spage)))
	  (cheight (- spage.clientHeight (frameBorderHeight spage))))
      (set! spage.spwidth cwidth)
      (set! spage.spbodywidth (- cwidth (frameBorderWidth spbody)))
      (set! spage.spbodyheight (- parent.clientHeight (frameBorderHeight spbody)
				  spage.sphead.offsetHeight))))

;*---------------------------------------------------------------------*/
;*    spage-reset-size ...                                             */
;*---------------------------------------------------------------------*/
(define (spage-reset-size spage)
   (let ((spage (if (string? spage) (dom-get-element-by-id spage) spage)))
      (unless (eq? spage.spviewport #unspecified)
	 ;; proceed only when initialized
	 (spage-resize spage)
	 ;; webkit got the animation wrong if the viewport is just not
	 ;; larger enough before adding a new tab
	 (set! spage.spscrollwidth (*fx (+fx spage.num 2) spage.spwidth))
	 (set! spage.spoffset (*fx spage.num spage.spwidth))
	 ;; we have to enforce the page size otherwise the browsers
	 ;; use the viewport width for the containing block width
	 (node-style-set! spage.spwindow
	    :width (format "~apx" spage.spwidth))
	 (node-style-set! spage.spviewport
            :width (format "~apx" spage.spscrollwidth))
	 (node-style-set! (dom-first-child spage.spviewport)
	    :width (format "~apx" spage.spbodywidth)))))
   
;*---------------------------------------------------------------------*/
;*    spage-add-event-listener! ...                                    */
;*---------------------------------------------------------------------*/
(define (spage-add-event-listener! obj event proc capture)
   (if (string=? event "onchange")
       (set! obj.onchg proc)
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
(define (spage-invoke-onchange-listener! spage tbody action)
   (when (procedure? spage.onchg)
      (let ((evt (new HopEvent "change" spage)))
	 (set! evt.target tbody)
	 (set! evt.action action)
	 (spage.onchg evt))))

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
       (css-get el "-moz-transition-duration")
       (css-get el "-o-transition-duration")
       0.5))

;*---------------------------------------------------------------------*/
;*    fade ...                                                         */
;*---------------------------------------------------------------------*/
(define (fade el el2 duration val0 val1 proc)
   (let* ((width (- val1 val0))
	  (n (round (* duration 100)))
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
	  (n (round (* duration 100)))
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
   
   (define (spage-push-none spage spviewport tbody otab)
      ;; mark the transition style (needed on resize)
      (set! spage.transitionstyle 'none)
      (node-style-set! otab :display "none"))
   
   (define (spage-push-slide spage spviewport tbody otab)
      ;; mark the transition style (needed on resize)
      (set! spage.transitionstyle 'slide)
      (if (hop-config 'css_transition)
	  (node-style-set! spviewport
	     :left (format "-~apx" spage.spoffset))
	  (let ((offset0 (- spage.spoffset spage.spwidth))
		(offset1 spage.spoffset))
	     (slide spviewport (css-transition-duration tbody) offset0 offset1 #f))))
   
   (define (spage-push-fade spage spviewport tbody otab)
      ;; mark the transition style (needed on resize)
      (set! spage.transitionstyle 'fade)
      (node-style-set! tbody
	 :-webkit-transition-property "none"
	 :-moz-transition-property "none"
	 :-o-transition-property "none"
	 :transition-property "none"
	 :opacity 0
	 :z-index spage.num
	 :top 0
	 :left (format "-~apx" spage.spoffset))
      (if (hop-config 'css_transition)
	  (after 1 (lambda ()
		      (node-style-set! tbody
			 :-webkit-transition-property "opacity"
			 :-moz-transition-property "opacity"
			 :-o-transition-property "opacity"
			 :transition-property "opacity"
			 :opacity 1)
		      (node-style-set! (cadr spage.tabs)
			 :-webkit-transition-property "opacity"
			 :-moz-transition-property "opacity"
			 :-o-transition-property "opacity"
			 :transition-property "opacity"
			 :opacity 0)))
	  (fade tbody (cadr spage.tabs)
		(css-transition-duration tbody) 0 1 #f)))
   
   (define (spage-push-auto spage spviewport tbody otab)
      (cond
	 ((< (hop-config :cpu_speed) 60)
	  (spage-push-none spage spviewport tbody otab))
	 ((hop-config 'css_transition)
	  (spage-push-slide spage spviewport tbody otab))
	 (else
	  (spage-push-none spage spviewport tbody otab))))

   (let* ((spage (if (string? spage) (dom-get-element-by-id spage) spage))
	  (tab (if (string? tab) (dom-get-element-by-id tab) tab))
	  (spviewport spage.spviewport)
	  (otab (car spage.tabs)))
      ;; adjust the size of the viewport
      (spage-resize spage)
      ;; increment the number of pushed elements
      (set! spage.num (+fx spage.num 1))
      (set! spage.tabs (cons tbody spage.tabs))
      (set! tbody.tab tab)
      ;; expand the body div when necessary
      (set! spage.spoffset (*fx spage.num spage.spwidth))
      (set! spage.spscrollwidth (*fx (+fx spage.num 1) spage.spwidth))
      ;; set the tab and viewport dimensions
      ;; webkit requires spviewport to larger that the sum of the bodies
      ;; we provision it with an extra body width
      (node-style-set! spviewport
	 :width (format "~apx" (+ spage.spbodywidth spage.spscrollwidth)))
      (node-style-set! otab
	     :width (format "~apx" spage.spbodywidth))
      (node-style-set! tbody
	 :width (format "~apx" spage.spbodywidth))
      ;; sptab event listener
      (sptab-invoke-onselect-listener! tab tbody "select")
      ;; add the new tab
      (dom-append-child! spviewport tbody)
      ;; the event listeners
      (spage-invoke-onchange-listener! spage tbody "push")
      (sptab-invoke-onselect-listener! tab tbody "push")
      ;; show the new tbody
      (case (spage-transition-style spage)
	 ((move)
	  (spage-push-slide spage spviewport tbody otab))
	 ((help)
	  (spage-push-fade spage spviewport tbody otab))
	 ((auto)
	  (spage-push-auto spage spviewport tbody otab))
	 (else
	  (spage-push-none spage spviewport tbody otab)))))

;*---------------------------------------------------------------------*/
;*    spage-pop ...                                                    */
;*---------------------------------------------------------------------*/
(define (spage-pop spage #!optional kont)

   (define (shrink-viewport spviewport)
      (set! spage.spscrollwidth (- spage.spscrollwidth spage.spoffset))
      (node-style-set! spviewport
	 :width (format "~apx" spage.spscrollwidth)))

   (define (invoke-pop-listeners spage tbody)
      (spage-invoke-onchange-listener! spage tbody "pop")
      (when (and tbody.tab (not (eq? tbody.tab #unspecified)))
	 (sptab-invoke-onselect-listener! tbody.tab tbody "pop")))
   
   (define (restore-static-body tab)
      (unless (eq? tab.static-node #unspecified)
	 (let ((p tab.static-body)
	       (n tab.static-node))
	    (dom-append-child! p n))))
   
   (define (spage-pop-none spage spviewport tbody otab)
      (dom-remove-child! spviewport tbody)
      (restore-static-body tbody.tab)
      (shrink-viewport spviewport)
      (node-style-set! otab :display "block")
      (when (procedure? kont) (kont spage))
      (set! spage.inpop #f))

   (define (spage-pop-fade spage spviewport tbody otab)
      (if (hop-config 'css_transition)
	  (let ((d (css-transition-duration tbody)))
	     (begin
		(node-style-set! tbody :opacity 0)
		(node-style-set! (car spage.tabs) :opacity 1))
	     (after d
		(lambda ()
		   (node-style-set! tbody
		      :-webkit-transition-property "none"
		      :-moz-transition-property "none"
		      :-o-transition-property "none"
		      :transition-property "none")
		   (dom-remove-child! spviewport tbody)
		   (restore-static-body tbody.tab)
		   (shrink-viewport spviewport)
		   (when (procedure? kont) (kont spage))
		   (set! spage.inpop #f))))
	  (fade tbody (car spage.tabs) (css-transition-duration tbody) 1 0
	     (lambda (tbody)
		(dom-remove-child! (dom-parent-node tbody) tbody)
		(restore-static-body tbody.tab)
		(shrink-viewport spviewport)
		(when (procedure? kont) (kont spage))
		(set! spage.inpop #f)))))
   
   (define (spage-pop-slide spage spviewport tbody otab)
      (if (hop-config 'css_transition)
	  (let ((d (css-transition-duration tbody)))
	     (node-style-set! spviewport
		:left (format "-~apx" spage.spoffset))
	     (after d
		(lambda ()
		   (dom-remove-child! spviewport tbody)
		   (restore-static-body tbody.tab)
		   (node-style-set! spviewport
		      :width (format "~apx" spage.spscrollwidth))
		   (when (procedure? kont) (kont spage))
		   (set! spage.inpop #f))))
	  (let ((offset0 (+ spage.spoffset spage.spwidth))
		(offset1 spage.spoffset))
	     (slide spviewport (css-transition-duration tbody) offset0 offset1
		(lambda (el)
		   (dom-remove-child! (dom-parent-node tbody) tbody)
		   (restore-static-body tbody.tab)
		   (shrink-viewport spviewport)
		   (when (procedure? kont) (kont spage))
		   (set! spage.inpop #f))))))

   (define (spage-pop-auto spage spviewport tbody otab)
      (cond
	 ((< (hop-config :cpu_speed) 60)
	  (spage-pop-none spage spviewport tbody otab))
	 ((hop-config 'css_transition)
	  (spage-pop-slide spage spviewport tbody otab))
	 (else
	  (spage-pop-none spage spviewport tbody otab))))
   
   (let* ((spage (if (string? spage) (dom-get-element-by-id spage) spage))
	  (spviewport spage.spviewport))
      (when (pair? spage.tabs)
	 (let ((tbody (car spage.tabs))
	       (otab (cadr spage.tabs)))
	    ;; mark the tab no longer pushed
	    (set! tbody.tab.pushed #f)
	    ;; decrement the number of pushed elements
	    (set! spage.num (-fx spage.num 1))
	    (set! spage.tabs (cdr spage.tabs))
	    (set! spage.spoffset (-fx spage.spoffset spage.spwidth))
	    ;; invoke the listener before removing any node
	    (when (pair? spage.tabs)
	       (invoke-pop-listeners spage (car spage.tabs)))
	    ;; pop the element from the gui
	    (case (spage-transition-style spage)
	       ((move)
		(spage-pop-slide spage spviewport tbody otab))
	       ((help)
		(spage-pop-fade spage spviewport tbody otab))
	       ((auto)
		(spage-pop-auto spage spviewport tbody otab))
	       (else
		(spage-pop-none spage spviewport tbody otab)))))))

;*---------------------------------------------------------------------*/
;*    spage-pop-all ...                                                */
;*---------------------------------------------------------------------*/
(define (spage-pop-all spage)
   (let loop ()
      (when (>fx spage.num 0)
	 (spage-pop spage)
	 (loop))))

;*---------------------------------------------------------------------*/
;*    find-spage-tag ...                                               */
;*---------------------------------------------------------------------*/
(define (find-spage-tag el tag)
   (let ((el (if (string? el)
		 (dom-get-element-by-id el)
		 el)))
      (let loop ((parent (dom-parent-node el)))
	 (cond
	    ((or (not parent) (eq? parent #unspecified))
	     (error "find-spage-tag" "cannot find parent spage" el))
	    ((string=? (parent.getAttribute "data-hss-tag") tag)
	     parent)
	    (else
	     (loop (dom-parent-node parent)))))))

;*---------------------------------------------------------------------*/
;*    find-spage ...                                                   */
;*---------------------------------------------------------------------*/
(define (find-spage el)
   (find-spage-tag el "hop-spage"))

;*---------------------------------------------------------------------*/
;*    find-sptab ...                                                   */
;*---------------------------------------------------------------------*/
(define (find-sptab el)
   (find-spage-tag el "hop-sptab"))

;*---------------------------------------------------------------------*/
;*    spage-current-tab ...                                            */
;*---------------------------------------------------------------------*/
(define (spage-current-tab spage)
   (let* ((tabs spage.tabs)
	  (tbody (car tabs)))
      tbody.tab))

;*---------------------------------------------------------------------*/
;*    spage-pop-update ...                                             */
;*---------------------------------------------------------------------*/
(define (spage-pop-update button)
   
   (define (pop-body-node spage body kont)
      (let* ((head (car spage.heads))
	     (sphead spage.sphead)
	     (spheadcontent (dom-first-child sphead))
	     (spheadbutton (dom-last-child sphead))
	     (tabs spage.tabs)
	     (tbody (car tabs))
	     (tab tbody.tab))
	 (set! spage.heads (cdr spage.heads))
	 (innerHTML-set! (dom-first-child spheadbutton) (caddr head))
	 (innerHTML-set! (dom-first-child (dom-first-child tab)) body)
	 (innerHTML-set! spheadcontent (cadr head))
	 (spage-pop spage kont)
	 (when (= spage.num 0) (set! spheadbutton.className ""))))

   (let ((spage (find-spage button)))
      (unless spage.inpop
	 (set! spage.inpop #t)
	 (let* ((head (car spage.heads))
		(tabs spage.tabs)
		(tbody (car tabs))
		(tparent (if (pair? (cdr tabs)) (cadr tabs)))
		(tab (if tparent tparent.tab)))
	    (when tbody.tab.pushed
	       (if (and (not (eq? tab #unspecified))
			(procedure? tab.svc)
			(equal? (tab.getAttribute "data-hop-svc-direction") "both"))
		   (pop-body-node spage (car head)
		      (lambda (spage)
			 (let ((t (car spage.tabs)))
			    (spage-tab-update t.tab))))
		   (pop-body-node spage (car head) #f)))))))

;*---------------------------------------------------------------------*/
;*    spage-push-body ...                                              */
;*---------------------------------------------------------------------*/
(define (spage-push-body tab body)
   (let* ((spage (find-spage tab))
	  (sphead spage.sphead)
	  (spheadcontent (dom-first-child sphead))
	  (spheadbutton (dom-last-child sphead))
	  (tabhead (dom-first-child (dom-first-child tab))))
      ;; reparent the tab nodes
      (let ((button (dom-child-nodes (dom-first-child spheadbutton)))
	    (content (dom-child-nodes spheadcontent))
	    (tab (dom-child-nodes tabhead)))
	 (set! spage.heads (cons (list tab content button) spage.heads))
	 (innerHTML-set! (dom-first-child spheadbutton) content)
	 (innerHTML-set! spheadcontent tab))
      (set! spheadbutton.className "visible")
      (spage-push spage tab body)))
   
;*---------------------------------------------------------------------*/
;*    spage-push-service ...                                           */
;*---------------------------------------------------------------------*/
(define (spage-push-service tab svc)
   (unless (and (js-in? "pushed" tab) (eq? tab.pushed #t))
      (set! tab.pushed #t)
      (with-hop (svc)
	 (lambda (body)
	    (set! tab.svc svc)
	    (set! tab.static-node #unspecified)
	    (spage-push-body tab body))
	 (lambda (xhr)
	    (set! tab.pushed #f)))))
	    
;*---------------------------------------------------------------------*/
;*    spage-push-node ...                                              */
;*---------------------------------------------------------------------*/
(define (spage-push-node tab node)
   ;; save the static-body that will be restore when popped
   (let ((p (dom-parent-node node)))
      (set! tab.pushed #t)
      (set! tab.static-node node)
      (set! tab.static-body p)
      (dom-remove-child! p node))
   (spage-push-body tab node))
	    
;*---------------------------------------------------------------------*/
;*    spage-tab-push ...                                               */
;*---------------------------------------------------------------------*/
(define (spage-tab-push tab)
   (let* ((el (if (string? tab) (dom-get-element-by-id tab) tab))
	  (svc (el.getAttribute "data-hop-svc")))
      (if (string? svc)
	  (spage-push-service el (lambda () svc))
	  (spage-push-node el (dom-first-child (dom-last-child el))))))

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
		  (spage-resize spage)
		  (node-style-set! body
		     :width (format "~apx" spage.spbodywidth))
                  (dom-remove-child! spviewport (car spage.tabs))
		  (set! spage.tabs (cons body (cdr spage.tabs)))
		  (set! body.tab tab)
                  (dom-append-child! spviewport body)))))))

;*---------------------------------------------------------------------*/
;*    spage-tab-pop ...                                                */
;*---------------------------------------------------------------------*/
(define (spage-tab-pop tab)
   (spage-pop-update tab))

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

;*---------------------------------------------------------------------*/
;*    spage-depth ...                                                  */
;*---------------------------------------------------------------------*/
(define (spage-depth spage)
   spage.num)

