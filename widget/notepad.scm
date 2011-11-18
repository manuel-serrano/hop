;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/widget/notepad.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Sat Nov 12 06:36:06 2011 (serrano)                */
;*    Copyright   :  2005-11 Manuel Serrano                            */
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
	       (idtag::bstring read-only)
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
(define-tag <NOTEPAD> ((id #unspecified string)
		       (class #unspecified string)
		       (history #unspecified)
		       (onchange #f)
		       (attrs)
		       body)
   (let ((id (xml-make-id id 'NOTEPAD))
	 (history (if (boolean? history) history (not (eq? id #unspecified))))
	 head)
      (if (and (pair? body) (isa? (car body) xml-nphead-element))
	  (begin
	     (set! head (car body))
	     (set! body (filter (lambda (e) (isa? e xml-nptab-element))
			   (cdr body))))
	  (begin
	     (set! head #f)
	     (set! body (filter (lambda (e) (isa? e xml-nptab-element))
			   body))))
      (if (null? body)
	  (error "<NOTEPAD>" "Missing <NPTAB> elements" id)
	  (notepad id class history attrs head body onchange))))

;*---------------------------------------------------------------------*/
;*    nptab-get-body ...                                               */
;*---------------------------------------------------------------------*/
(define (nptab-get-body tab)
   (with-access::xml-nptab-element tab (body)
      (if (and (isa? (car body) xml-delay) (null? (cdr body)))
	  (with-access::xml-delay (car body) (thunk)
	     (thunk))
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
      (call-with-output-string
       (lambda (op)
	  (obj->javascript
	   (procedure->service
	    (lambda (i)
	       (nptab-get-body (list-ref tabs i))))
	   op
	   #f))))
   
   (define (make-tab-div tab i)
      (with-access::xml-nptab-element tab (attributes (idt id) idtag body klass)
	 (let ((click (format "hop_notepad_select( '~a', '~a', ~a )"
			      id idtag (if history "true" "false"))))
	    (set! attributes
		  `(:onclick ,(secure-javascript-attr click)
		    :class ,(string-append klass
					   (if (=fx i 0)
					       " hop-nptab-active"
					       " hop-nptab-inactive"))
		    ,@attributes)))
	 (with-access::xml-element tab (body)
	    (when (and (pair? body)
		       (isa? (car body) xml-delay)
		       (null? (cdr body)))
	       (set! attributes `(:lang "delay" ,@attributes))))
	 (<DIV> :hssclass "hop-notepad-tab-body"
	    :style (if (=fx i 0) "display: block" "display: none")
	    :id idt
	    (cond
	       ((=fx i 0)
		(nptab-get-body tab))
	       ((and (isa? (car body) xml-delay) (null? (cdr body)))
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
	     head
	     (<TABLE> :hssclass "hop-notepad"
		(<TR>
		   (<TD> :id (string-append id "-tabs")
		      :hssclass "hop-notepad-tabs"
		      tabs))
		(<TR>
		   (<TD> :id (string-append id "-body")
		      :hssclass "hop-notepad-body" bodies)))
		(<SCRIPT> :class "hop-notepad-init"
		   (when onchange
		      (format "document.getElementById('~a').onchange = ~a"
			      id (hop->js-callback onchange)))
		   (format "document.getElementById('~a').onkeyup = function(_) { return ~a;}"
			   id svc))
	     attrs)))
   
;*---------------------------------------------------------------------*/
;*    <NPHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-tag <NPHEAD> ((id #unspecified string)
		      (attr)
		      body)
   (instantiate::xml-nphead-element
      (tag 'div)
      (id (xml-make-id id 'NPHEAD))
      (attributes `(:hssclass "hop-nphead" ,@attr))
      (body body)))
   
;*---------------------------------------------------------------------*/
;*    <NPTAB> ...                                                      */
;*---------------------------------------------------------------------*/
(define-tag <NPTAB> ((id #unspecified string)
		     (class #unspecified string)
		     (selected #f)
		     (onselect #f)
		     (attr)
		     body)
   
   (cond
      ((null? body)
       (error "<NPTAB>" "Missing <NPTABHEAD> " id))
      ((not (isa? (car body) xml-nptabhead-element))
       (error "<NPTAB>" "Illegal <NPTABHEAD> " (car body)))
      (else
       (let ((cla (make-class-name "hop-nptab " class)))
	  (instantiate::xml-nptab-element
	     (tag 'span)
	     (id (xml-make-id id 'NPTAB))
	     (idtag (xml-make-id id 'NPTABTAG))
	     (attributes `(:hssclass "hop-nptab" ,@attr))
	     (klass cla)
	     (onselect onselect)
	     (head (car body))
	     (body (cdr body)))))))

;*---------------------------------------------------------------------*/
;*    <NPTABHEAD> ...                                                  */
;*---------------------------------------------------------------------*/
(define-tag <NPTABHEAD> ((id #unspecified string)
			 (attr)
			 body)
   (instantiate::xml-nptabhead-element
      (tag 'span)
      (id (xml-make-id id 'NPTABHEAD))
      (attributes `(:hssclass "hop-nptab-head" ,@attr))
      (body body)))
   
;*---------------------------------------------------------------------*/
;*    xml-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-nptab-element p backend)
   (with-access::xml-nptab-element obj (id idtag head attributes onselect)
      (display "<span id='" p)
      (display idtag p)
      (display "'" p)
      (xml-write-attributes attributes p backend)
      (display ">" p)
      (when onselect
	 (display "<script>" p)
	 (display "document.getElementById( '" p)
	 (display idtag p)
	 (display "' ).onselect = " p)
	 (display (hop->js-callback onselect) p)
	 (display "</script>" p))
      (xml-write head p backend)
      (display "</span>" p)))

;*---------------------------------------------------------------------*/
;*    xml-compare ::xml-nphead-element ...                             */
;*---------------------------------------------------------------------*/
(define-method (xml-compare a1::xml-nphead-element a2)
   (if (and (isa? a2 xml-markup)
	    (with-access::xml-markup a2 (tag)
	       (eq? tag 'div))
	    (equal? (dom-get-attribute a2 "class") "hop-nphead"))
       (with-access::xml-markup a1 ((body1 body))
	  (with-access::xml-markup a2 ((body2 body))
	     (xml-compare body1 body2)))
       (call-next-method)))

;*---------------------------------------------------------------------*/
;*    xml-compare ::xml-nptabhead-element ...                          */
;*---------------------------------------------------------------------*/
(define-method (xml-compare a1::xml-nptabhead-element a2)
   (if (and (isa? a2 xml-markup)
	    (with-access::xml-markup a2 (tag)
	       (eq? tag 'span))
	    (equal? (dom-get-attribute a2 "hssclass") "hop-nptab-head"))
       (with-access::xml-markup a1 ((body1 body))
	  (with-access::xml-markup a2 ((body2 body))
	     (xml-compare body1 body2)))
       (call-next-method)))

;*---------------------------------------------------------------------*/
;*    xml-compare ::xml-nptab-element ...                              */
;*---------------------------------------------------------------------*/
(define-method (xml-compare a1::xml-nptab-element a2)
   (if (and (isa? a2 xml-markup)
	    (with-access::xml-markup a2 (tag)
	       (eq? tag 'span))
	    (equal? (dom-get-attribute a2 "hssclass") "hop-nptab")
	    (let ((head (dom-first-child a2))
		  (body (cadr (dom-child-nodes a2))))
	       (xml-compare (cadr (dom-child-nodes a1)) head)))
       (call-next-method)))
