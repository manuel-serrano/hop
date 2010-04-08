;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/widget/notepad.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Thu Apr  8 08:08:02 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of notepads.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-notepad

   (library hop)

   (static  (class xml-nphead-element::xml-element)
	    (class xml-nptabhead-element::xml-element)
	    (class xml-nptab-element::xml-element
	       (head::xml-nptabhead-element read-only)
	       (onselect read-only)
	       klass::bstring ))
   
   (export  (<NOTEPAD> . ::obj)
	    (<NPHEAD> . ::obj)
	    (<NPTAB> . ::obj)
	    (<NPTABHEAD> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    <NOTEPAD> ...                                                    */
;*    -------------------------------------------------------------    */
;*    See __hop_css for HSS types.                                     */
;*---------------------------------------------------------------------*/
(define-markup <NOTEPAD> ((id #unspecified string)
			  (class #unspecified string)
			  (history #unspecified)
			  (onchange #f)
			  (attrs)
			  body)
   (let ((id (xml-make-id id 'NOTEPAD))
	 (history (if (boolean? history) history (not (eq? id #unspecified))))
	 head)
      (if (and (pair? body) (xml-nphead-element? (car body)))
	  (begin
	     (set! head (car body))
	     (set! body (filter xml-nptab-element? (cdr body))))
	  (begin
	     (set! head #f)
	     (set! body (filter xml-nptab-element? body))))
      (if (null? body)
	  (error '<NOTEPAD> "Missing <NPTAB> elements" id)
	  (notepad id class history attrs head body onchange))))

;*---------------------------------------------------------------------*/
;*    nptab-get-body ...                                               */
;*---------------------------------------------------------------------*/
(define (nptab-get-body tab)
   (with-access::xml-nptab-element tab (body)
      (if (and (xml-delay? (car body)) (null? (cdr body)))
	  ((xml-delay-thunk (car body)))
	  body)))

;*---------------------------------------------------------------------*/
;*    make-class-name ...                                              */
;*---------------------------------------------------------------------*/
(define (make-class-name::bstring default::bstring name)
   (if (string? name)
       (string-append default " " name)
       default))

;*---------------------------------------------------------------------*/
;*    notepad ...                                                      */
;*---------------------------------------------------------------------*/
(define (notepad id klass history attrs head tabs onchange)
   
   (define svc
      (hop->javascript
       (procedure->service
	(lambda (i)
	   (nptab-get-body (list-ref tabs i))))
       #f #f))
   
   (define (make-tab-div tab i)
      (with-access::xml-nptab-element tab (attributes (idt id) body klass)
	 (let ((click (format "hop_notepad_select( '~a', '~a', ~a )"
			      id idt (if history "true" "false"))))
	    (set! attributes
		  `(:onclick ,click
		    :class ,(string-append klass
					   (if (=fx i 0)
					       " hop-nptab-active"
					       " hop-nptab-inactive"))
		    ,@attributes)))
	 (when (and (pair? (xml-element-body tab))
		    (xml-delay? (car (xml-element-body tab)))
		    (null? (cdr (xml-element-body tab))))
	    (set! attributes `(:lang "delay" ,@attributes)))
	 (<DIV> :hssclass "hop-notepad-tab-body"
	    :style (if (=fx i 0) "display: block" "display: none")
	    :id (string-append id "-notepad-tab-body")
	    (cond
	       ((=fx i 0)
		(nptab-get-body tab))
	       ((and (xml-delay? (car body)) (null? (cdr body)))
		;; we must not eagerly evaluate the tab...
		"")
	       (else
		body)))))
   
   (let ((bodies (map (lambda (t i) (make-tab-div t i))
		      tabs (iota (length tabs))))
	 (attrs (append-map (lambda (a)
			       (list (symbol->keyword (car a)) (cdr a)))
			    attrs)))
      (apply <DIV>
	     :id id
	     :hssclass "hop-notepad"
	     :class (make-class-name "hop-notepad" klass)
	     :onkeyup (format "return ~a;" svc)
	     head
	     (<TABLE> :hssclass "hop-notepad"
		(<TR>
		   (<TD> :id (string-append id "-tabs")
		      :hssclass "hop-notepad-tabs"
		      tabs))
		(<TR>
		   (<TD> :id (string-append id "-body")
		      :hssclass "hop-notepad-body" bodies)))
	     (when onchange
		(<SCRIPT> :class "hop-notepad-init"
		   (format "document.getElementById('~a').onchange = ~a"
			   id (hop->js-callback onchange))))
	     attrs)))
   
;*---------------------------------------------------------------------*/
;*    <NPHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-markup <NPHEAD> ((id #unspecified string)
			 (attr)
			 body)
   (instantiate::xml-nphead-element
      (markup 'div)
      (id (xml-make-id id 'NPHEAD))
      (attributes `(:hssclass "hop-nphead" ,@attr))
      (body body)))
   
;*---------------------------------------------------------------------*/
;*    <NPTAB> ...                                                      */
;*---------------------------------------------------------------------*/
(define-markup <NPTAB> ((id #unspecified string)
			(class #unspecified string)
			(selected #f)
			(onselect #f)
			(attr)
			body)
   
   (cond
      ((null? body)
       (error '<NPTAB> "Missing <NPTABHEAD> " id))
      ((not (xml-nptabhead-element? (car body)))
       (error '<NPTAB> "Illegal <NPTABHEAD> " (car body)))
      (else
       (let ((cla (make-class-name "hop-nptab " class)))
	  (instantiate::xml-nptab-element
	     (markup 'span)
	     (id (xml-make-id id 'NPTAB))
	     (attributes `(:hssclass "hop-nptab" ,@attr))
	     (klass cla)
	     (onselect onselect)
	     (head (car body))
	     (body (cdr body)))))))

;*---------------------------------------------------------------------*/
;*    <NPTABHEAD> ...                                                  */
;*---------------------------------------------------------------------*/
(define-markup <NPTABHEAD> ((id #unspecified string)
			    (attr)
			    body)
   (instantiate::xml-nptabhead-element
      (markup 'span)
      (id (xml-make-id id 'NPTABHEAD))
      (attributes `(:hssclass "hop-nptab-head" ,@attr))
      (body body)))
   
;*---------------------------------------------------------------------*/
;*    xml-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-nptab-element p backend)
   (with-access::xml-nptab-element obj (id head attributes onselect)
      (display "<span id='" p)
      (display id p)
      (display "'" p)
      (xml-write-attributes attributes p backend)
      (display ">" p)
      (when onselect
	 (display "<script>" p)
	 (display "document.getElementById( '" p)
	 (display id p)
	 (display "' ).onselect = " p)
	 (display (hop->js-callback onselect) p)
	 (display "</script>" p))
      (xml-write head p backend)
      (display "</span>" p)))

