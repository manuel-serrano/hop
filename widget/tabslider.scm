;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/widget/tabslider.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Wed Nov 24 13:51:08 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of TABSLIDER.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-tabslider

   (library hop)
   
   (static  (class html-tabslider::xml-element
	       (clazz (default #f))
	       (width (default #f))
	       (height (default #f))
	       (index (default 0))
	       (onchange (default #f))
	       (history read-only (default #t)))
	    (class html-tspan::xml-element)
	    (class html-tshead::xml-element))

   (export  (<TABSLIDER> . ::obj)
	    (<TSPAN> . ::obj)
	    (<TSHEAD> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    <TABSLIDER> ...                                                  */
;*---------------------------------------------------------------------*/
(define-markup <TABSLIDER> ((id #unspecified string)
			    (class #unspecified string)
			    (width #f)
			    (height #f)
			    (index 0)
			    (onchange #f)
			    (history #unspecified)
			    body)
   
   ;; Verify that the body is a list of <TSPAN>
   (for-each (lambda (x)
		(unless (and (xml-element? x)
			     (eq? (xml-element-tag x) 'tspan))
		   (error "<TABSLIDER>" "Component is not a <TSPAN>" x)))
	     body)
   
   (instantiate::html-tabslider
      (tag 'tabslider)
      (id (xml-make-id id 'TABSLIDER))
      (clazz (if (string? class)
		 (string-append class " uninitialized")
		 "uninitialized"))
      (width width)
      (height height)
      (index index)
      (onchange onchange)
      (history (if (boolean? history) history (not (eq? id #unspecified))))
      (body body)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-tabslider ...                                   */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-tabslider p backend)
   (with-access::html-tabslider obj (clazz id width height body index history onchange)
      (fprintf p "<div hssclass='hop-tabslider' class='~a' id='~a'" clazz id)
      (when (or width height)
	 (fprintf p " style=\"~a~a\""
		  (if width  (format "width: ~a;" width) "")
		  (if height (format "height: ~a;" height) "")))
      (display ">" p)
      (xml-write body p backend)
      (display "</div>" p)
      (fprintf p
	       "<script type='~a'>hop_tabslider_init('~a', ~a, ~a, ~a)</script>"
	       (hop-javascript-mime-type)
	       id index
	       (if history "true" "false")
	       (hop->js-callback onchange))))

;*---------------------------------------------------------------------*/
;*    <TSPAN> ...                                                      */
;*---------------------------------------------------------------------*/
(define-markup <TSPAN> ((id #unspecified string)
			(onselect #f)
			body)
   
   (define (tspan-onselect id onselect)
      (when onselect
	 (<SCRIPT>
	    (format "document.getElementById('~a').onselect=~a;"
		    id (hop->js-callback onselect)))))
   
   (let ((id (xml-make-id id 'TSPAN)))
      ;; Check that body is well formed
      (cond
	 ((or (null? body) (null? (cdr body)))
	  (error "<TSPAN>" "Illegal body, at least two elements needed" body))
	 ((and (xml-delay? (cadr body)) (null? (cddr body)))
	  ;; a delayed tspan
	  (instantiate::html-tspan
	     (tag 'tspan)
	     (body (list (car body)
			 (<DIV>
			    :id id
			    :hssclass "hop-tabslider-pan"
			    :class "inactive"
			    :lang "delay"
			    :onkeyup (secure-javascript-attr
				      (format "return ~a;"
					      (call-with-output-string
					       (lambda (op)
						  (obj->javascript
						   (procedure->service
						    (xml-delay-thunk (cadr body)))
						   op
						   #f)))))
			    (tspan-onselect id onselect)
			    "delayed tab")))))
	 (else
	  ;; an eager static tspan
	  (instantiate::html-tspan
	     (tag 'tspan)
	     (body (list (car body)
			 (apply <DIV>
				:id id
				:hssclass "hop-tabslider-pan"
				:class "inactive"
				(tspan-onselect id onselect)
				(cdr body)))))))))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-tspan ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-tspan p backend)
   (xml-write (html-tspan-body obj) p backend))

;*---------------------------------------------------------------------*/
;*    <TSHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-alias <TSHEAD> <DIV>
   :hssclass "hop-tabslider-head"
   :class "inactive"
   :onclick (secure-javascript-attr "hop_tabslider_select( this )"))
