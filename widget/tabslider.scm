;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/widget/tabslider.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Tue Feb 16 12:18:15 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of TABSLIDER.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-tabslider

   (library hop)
   
   (static  (class html-tabslider::xml-element
	       (width (default #f))
	       (height (default #f))
	       (index (default 0))
	       (onchange (default #f))
	       (history read-only (default #t))
	       (speed read-only (default 14)))
	    (class html-tspan::xml-element)
	    (class html-tshead::xml-element))

   (export  (<TABSLIDER> . ::obj)
	    (<TSPAN> . ::obj)
	    (<TSHEAD> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    <TABSLIDER> ...                                                  */
;*---------------------------------------------------------------------*/
(define-markup <TABSLIDER> ((id #unspecified string)
			    (width #f)
			    (height #f)
			    (index 0)
			    (onchange #f)
			    (history #unspecified)
			    (speed 20 integer)
			    body)
   
   ;; Verify that the body is a list of <TSPAN>
   (for-each (lambda (x)
		(unless (and (xml-element? x)
			     (eq? (xml-element-markup x) 'tspan))
		   (error '<TABSLIDER> "Component is not a <TSPAN>" x)))
	     body)
   
   (instantiate::html-tabslider
      (markup 'tabslider)
      (id (xml-make-id id 'TABSLIDER))
      (width width)
      (height height)
      (index index)
      (onchange onchange)
      (speed speed)
      (history (if (boolean? history) history (not (eq? id #unspecified))))
      (body body)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-tabslider ...                                   */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-tabslider p backend)
   (with-access::html-tabslider obj (id width height body index history onchange speed)
      (fprintf p "<div class='hop-tabslider' id='~a'" id)
      (when (or width height)
	 (fprintf p " style=\"~a~a\""
		  (if width  (format "width: ~a;" width) "")
		  (if height (format "height: ~a;" height) "")))
      (display ">" p)
      (xml-write body p backend)
      (display "</div>" p)
      (fprintf p
	       "<script type='~a'>hop_tabslider_init('~a', ~a, ~a, ~a, ~a)</script>"
	       (hop-javascript-mime-type)
	       id index
	       (if history "true" "false")
	       (hop->js-callback onchange)
	       speed)))

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
	  (error '<TSPAN> "Illegal body, at least two elements needed" body))
	 ((and (xml-delay? (cadr body)) (null? (cddr body)))
	  ;; a delayed tspan
	  (instantiate::html-tspan
	     (markup 'tspan)
	     (body (list (car body)
			 (<DIV>
			    :id id
			    :class "hop-tabslider-content"
			    :lang "delay"
			    :onkeyup (format "return ~a;"
					     (hop->json
					      (procedure->service
					       (xml-delay-thunk (cadr body)))
					      #f #f))
			    (tspan-onselect id onselect)
			    "delayed tab")))))
	 (else
	  ;; an eager static tspan
	  (instantiate::html-tspan
	     (markup 'tspan)
	     (body (list (car body)
			 (apply <DIV>
				:id id
				:class "hop-tabslider-content"
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
   :class "hop-tabslider-head"
   :onclick "hop_tabslider_select( this )")
