;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/share/hop-gauge.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Apr 13 08:24:51 2010                          */
;*    Last change :  Tue Dec 10 19:00:37 2013 (serrano)                */
;*    Copyright   :  2010-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Gauge client-side implementation                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop-gauge
   
   (export (hop_create_gauge attrs body)
	   (gauge-init! id fid tid value min max text listener)
	   (gauge-value el)
	   (gauge-value-set! el value)
	   (gauge-max el)
	   (gauge-max-set! el value)
	   (gauge-min el)
	   (gauge-min-set! el value)
	   (gauge-text el)
	   (gauge-text-set! el text))
   
   (JS     hop_add_native_event_listener
	   HopEvent)
   
   (scheme2js-pragma
           (hop_create_gauge (JS hop_create_gauge) (arity 2))))

;*---------------------------------------------------------------------*/
;*    hop_create_gauge ...                                             */
;*---------------------------------------------------------------------*/
(define (hop_create_gauge attrs body)
   (let ((listener (let ((l (memq :onchange attrs)))
		      (when (pair? l) (cadr l))))
	 (value (let ((l (memq :value attrs)))
		   (if (pair? l) (cadr l) 0)))
	 (min (let ((l (memq :min attrs)))
		 (if (pair? l) (cadr l) 0)))
	 (max (let ((l (memq :max attrs)))
		 (if (pair? l) (cadr l) 99)))
	 
	 (id (let ((l (memq :id attrs)))
		(if (pair? l) (cadr l) (symbol->string (gensym)))))
	 (fid (symbol->string (gensym)))
	 (tid (symbol->string (gensym))))
      (let ((format (let ((l (memq :format attrs)))
		       (if (pair? l) (cadr l) "~a/~a"))))
	 (<DIV> :data-hss-tag "hop-gauge" :id id
	    "0"
	    (<DIV> :data-hss-tag "hop-gauge-fill" :id fid)
	    (<DIV> :data-hss-tag "hop-gauge-text" :id tid "0")
	    (add-event-listener! id "ready"
	       (lambda (e)
		  (gauge-init! id fid tid value min max format listener))
	       #t)
	    body))))

;*---------------------------------------------------------------------*/
;*    gauge-init! ...                                                  */
;*---------------------------------------------------------------------*/
(define (gauge-init! id fid tid value min max fmt listener)
   (let ((el (dom-get-element-by-id id))
	 (fel (dom-get-element-by-id fid))
	 (tel (dom-get-element-by-id tid)))
      (after 1
	 (lambda ()
	    (set! el.hop_add_event_listener add-gauge-event-listener!)
	    (set! el.format fmt)
	    (set! el.min min)
	    (set! el.max max)
	    (set! el.fill fel)
	    (set! el.tel tel)
	    (gauge-text-set! el (format fmt value max min))
	    (if listener
		(add-gauge-event-listener! el "change" listener #t)
		(set! el.onchange #f))
	    (gauge-value-set! el value)))))

;*---------------------------------------------------------------------*/
;*    add-gauge-event-listener! ...                                    */
;*---------------------------------------------------------------------*/
(define (add-gauge-event-listener! el event proc capture)
   (if (string=? event "change")
       (set! el.onchange proc))
   (hop_add_native_event_listener el event proc capture))

;*---------------------------------------------------------------------*/
;*    gauge-value ...                                                  */
;*---------------------------------------------------------------------*/
(define (gauge-value o)
   (let ((el (if (string? o) (dom-get-element-by-id o) o)))
      el.value))

;*---------------------------------------------------------------------*/
;*    gauge-value-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (gauge-value-set! o v)
   (let ((el (if (string? o) (dom-get-element-by-id o) o)))
      ;; bound the value
      (when (> v el.max) (set! v el.max))
      (when (< v el.min) (set! v el.min))
      ;; the value is inbound, set it
      (set! el.value v)
      ;; adjust the filler
      (let* ((w el.clientWidth)
	     (s (round (* w (/ v (- el.max el.min))))))
	 (node-style-set! el.fill :right (format "~apx" (- w s))))
      ;; invoke the onchange listener
      (if (procedure? el.onchange)
	  (let ((evt (new HopEvent "change" v)))
	     (el.onchange evt)
	     (unless (event-stopped? evt)
		(gauge-text-set! el (format el.format v el.max el.min))))
	  (gauge-text-set! el (format el.format v el.max el.min)))))

;*---------------------------------------------------------------------*/
;*    gauge-max ...                                                    */
;*---------------------------------------------------------------------*/
(define (gauge-max o)
   (let ((el (if (string? o) (dom-get-element-by-id o) o)))
      el.max))

;*---------------------------------------------------------------------*/
;*    gauge-max-set! ...                                               */
;*---------------------------------------------------------------------*/
(define (gauge-max-set! o v)
   (let ((el (if (string? o) (dom-get-element-by-id o) o)))
      (set! el.max v)
      (when (> el.value v)
	 (set! el.value v))))

;*---------------------------------------------------------------------*/
;*    gauge-min ...                                                    */
;*---------------------------------------------------------------------*/
(define (gauge-min o)
   (let ((el (if (string? o) (dom-get-element-by-id o) o)))
      el.min))

;*---------------------------------------------------------------------*/
;*    gauge-min-set! ...                                               */
;*---------------------------------------------------------------------*/
(define (gauge-min-set! o v)
   (let ((el (if (string? o) (dom-get-element-by-id o) o)))
      (set! el.min v)
      (when (< el.value v)
	 (set! el.value v))))

;*---------------------------------------------------------------------*/
;*    gauge-text ...                                                   */
;*---------------------------------------------------------------------*/
(define (gauge-text o)
   (let* ((el (if (string? o) (dom-get-element-by-id o) o)))
      el.tel.innerHTML))

;*---------------------------------------------------------------------*/
;*    gauge-text ...                                                   */
;*---------------------------------------------------------------------*/
(define (gauge-text-set! o t)
   (let ((el (if (string? o) (dom-get-element-by-id o) o)))
      (innerHTML-set! el.tel t)))
