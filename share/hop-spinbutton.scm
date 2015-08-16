;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/share/hop-spinbutton.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 11 19:03:05 2009                          */
;*    Last change :  Wed Jan  5 21:00:22 2011 (serrano)                */
;*    Copyright   :  2009-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    SpinButton client side implementation                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop-spinbutton
   (export (hop_create_spinbutton attrs body)
	   (spinbutton-value el)
	   (spinbutton-value-set! el val)
	   (spinbutton-value-update! el val)
	   (spinbutton-inc! el)
	   (spinbutton-dec! el))
   (scheme2js-pragma
           (hop_create_spinbutton (JS hop_create_spinbutton) (arity 2))))
	   
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
       (error "<SPINBUTTON>" "arguments ignored" body)
       (let* ((min (let ((l (memq :min attrs)))
		      (if (pair? l) (cadr l) 0)))
	      (max (let ((l (memq :max attrs)))
		      (if (pair? l) (cadr l) 100)))
	      (value (let ((l (memq :value attrs)))
			(if (pair? l) (cadr l) 100)))
	      (onchange (let ((l (memq :onchange attrs)))
			   (when (pair? l) (cadr l))))
	      (id (let ((l (memq :id attrs)))
		     (if (pair? l) (cadr l) (symbol->string (gensym)))))
	      (w (input-width min max)))
	  (<TABLE> :hssclass "hop-spinbutton"
	     :id id
	     :cellspacing 0 :cellpadding 0
	     :value value :min min :max max
	     :onchange onchange
	     (<TR>
		(<TD> :class "hop-spinbutton-value"
		   (<INPUT> :class "hop-spinbutton-entry"
		      :id (string-append id "-entry")
		      :type 'text
		      :style (format "width: ~aem" w)
		      :onchange (spinbutton-value-update! id (string->number this.value))
		      :value (integer->string value))
		   (<INPUT> :class "hop-spinbutton-entry-onchange"
		      :id (string-append id "-onchange")
		      :onchange onchange))
		(<TD> :class "hop-spinbutton-buttons"
		   (<TABLE>
		      (<TR>
			 (<TD> :class "hop-spinbutton-button-top"
			    :onmousedown (spinbutton-inc! id)
			    "codepoint(#u25b2)"))
		      (<TR>
			 (<TD> :class "hop-spinbutton-button-bottom"
			    :onmousedown (spinbutton-dec! id)
			    "codepoint(&#9660)")))))))))

;*---------------------------------------------------------------------*/
;*    spinbutton-value ...                                             */
;*---------------------------------------------------------------------*/
(define (spinbutton-value el)
   el.value)

;*---------------------------------------------------------------------*/
;*    spinbutton-value-update! ...                                     */
;*---------------------------------------------------------------------*/
(define (spinbutton-value-update! el val)
   (let ((el (if (string? el) (dom-get-element-by-id el) el)))
      (when el
	 (spinbutton-value-set! el val)
	 (let ((inp2 (dom-get-element-by-id (string-append el.id "-onchange")))
	       (s (number->string el.value)))
	    (unless (string=? s inp2.value)
	       (set! inp2.value s)
	       (when (procedure? inp2.onchange)
		  (inp2.onchange #unspecified)
		  #f))))))

;*---------------------------------------------------------------------*/
;*    spinbutton-value-set! ...                                        */
;*---------------------------------------------------------------------*/
(define (spinbutton-value-set! el val)
   (let ((el (if (string? el) (dom-get-element-by-id el) el)))
      (when el
	 (cond
	    ((<= val el.min)
	     (set! el.value el.min)
	     (set! el.bound "min"))
	    ((>= val el.max)
	     (set! el.value el.max)
	     (set! el.bound "max"))
	    (else
	     (set! el.bound "")
	     (set! el.value val)))
	 (let ((inp (dom-get-element-by-id (string-append el.id "-entry")))
	       (s (number->string el.value)))
	    (unless (string=? s inp.value)
	       (set! inp.value s))))))

;*---------------------------------------------------------------------*/
;*    spinbutton-inc! ...                                              */
;*---------------------------------------------------------------------*/
(define (spinbutton-inc! el)
   (let ((el (dom-get-element-by-id el)))
      (when el (spinbutton-value-update! el (+ el.value 1)))))
   
;*---------------------------------------------------------------------*/
;*    spinbutton-dec! ...                                              */
;*---------------------------------------------------------------------*/
(define (spinbutton-dec! el)
   (let ((el (dom-get-element-by-id el)))
      (when el (spinbutton-value-update! el (- el.value 1)))))
