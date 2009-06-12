;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/share/hop-spinbutton.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 11 19:03:05 2009                          */
;*    Last change :  Fri Jun 12 20:11:27 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    SpinButton client side implementation                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    hop_create_spinbutton ...                                        */
;*---------------------------------------------------------------------*/
(define (hop_create_spinbutton attrs body)
   
   (define (input-width minvalue maxvalue)
      (let loop ((v (max (abs minvalue) (abs maxvalue)))
		 (w 0))
	 (if (> v 0)
	     (loop (round (/ v 10)) (+ w 1))
	     (if (or (< minvalue 0) (< maxvalue 0))
		 (+ 1 w)
		 w))))
   
   (if (pair? body)
       (error '<SPINBUTTON> "arguments ignored" body)
       (let* ((minvalue (let ((l (memq :minvalue attrs)))
			   (if (pair? l) (cadr l) 0)))
	      (maxvalue (let ((l (memq :maxvalue attrs)))
			   (if (pair? l) (cadr l) 100)))
	      (value (let ((l (memq :value attrs)))
			(if (pair? l) (cadr l) 100)))
	      (onchange (let ((l (memq :onchange attrs)))
			   (when (pair? l) (cadr l))))
	      (id (let ((l (memq :id attrs)))
		     (if (pair? l) (cadr id) (symbol->string (gensym)))))
	      (w (input-width minvalue maxvalue)))
	  (<TABLE> :hssclass "hop-spinbutton"
	     :id id
	     :cellspacing 0 :cellpadding 0
	     :value value :minvalue minvalue :maxvalue maxvalue
	     :onchange onchange
	     (<TR>
		(<TD> :class "hop-spinbutton-value" :rowspan 2
		   (<INPUT> :class "hop-spinbutton-entry"
		      :id (string-append id "-entry")
		      :type 'text
		      :style (format "width: ~aem" w)
		      :onchange (hop_spinbutton_set id this.value)
		      :value (integer->string value)))
		(<TD> :class "hop-spinbutton-button hop-spinbutton-button-top"
		   :onmousedown (hop_spinbutton_inc id)
		   (<DIV> "codepoint(#u25b2)")))
	     (<TR> 
		(<TD> :class "hop-spinbutton-button hop-spinbutton-button-bottom"
		   :onmousedown (hop_spinbutton_dec id)
		   (<DIV> "codepoint(&#9660)")))))))

;*---------------------------------------------------------------------*/
;*    spinbutton-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (spinbutton-set! el val)
   (let ((el (if (string? el) (dom-get-element-by-id el) el)))
      (when el
	 (cond
	    ((<= val el.minvalue)
	     (set! el.value el.minvalue)
	     (set! el.dir "min"))
	    ((>= val el.maxvalue)
	     (set! el.value el.maxvalue)
	     (set! el.dir "max"))
	    (else
	     (set! el.dir "")
	     (set! el.value val)))
	 (let ((inp (dom-get-element-by-id (string-append el.id "-entry"))))
	    (unless (= el.value (number->string inp.value))
	       (set! inp.value (number->string el.value))))
	 (when (procedure? el.onchange)
	    (el.onchange)))))

;*---------------------------------------------------------------------*/
;*    hop_spinbutton_set ...                                           */
;*---------------------------------------------------------------------*/
(define (hop_spinbutton_set el val)
   (spinbutton-set! el (string->number val)))

;*---------------------------------------------------------------------*/
;*    hop_spinbutton_inc ...                                           */
;*---------------------------------------------------------------------*/
(define (hop_spinbutton_inc el)
   (let ((el (if (string? el)
		 (dom-get-element-by-id el)
		 el)))
      (when el (spinbutton-set! el (+ el.value 1)))))
   
;*---------------------------------------------------------------------*/
;*    hop_spinbutton_dec ...                                           */
;*---------------------------------------------------------------------*/
(define (hop_spinbutton_dec el)
   (let ((el (if (string? el)
		 (dom-get-element-by-id el)
		 el)))
      (when el (spinbutton-set! el (- el.value 1)))))
