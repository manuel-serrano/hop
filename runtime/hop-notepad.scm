;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/hop-notepad.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Mon Apr 14 17:19:55 2008 (serrano)                */
;*    Copyright   :  2005-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of notepads.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-notepad

   (include "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_hop-extra
	    __hop_misc
	    __hop_js-lib
	    __hop_service)

   (static  (class xml-nphead-element::xml-element)
	    (class xml-nptabhead-element::xml-element)
	    (class xml-nptab-element::xml-element
	       (head::xml-nptabhead-element read-only)
	       (onselect read-only)))
   
   (export  (<NOTEPAD> . ::obj)
	    (<NPHEAD> . ::obj)
	    (<NPTAB> . ::obj)
	    (<NPTABHEAD> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    nptab-get-body ...                                               */
;*---------------------------------------------------------------------*/
(define (nptab-get-body tab)
   (with-access::xml-nptab-element tab (body)
      (if (and (xml-delay? (car body)) (null? (cdr body)))
	  ((xml-delay-thunk (car body)))
	  body)))

;*---------------------------------------------------------------------*/
;*    notepad ...                                                      */
;*---------------------------------------------------------------------*/
(define (notepad id history attrs head tabs onchange)
   
   (define svc
      (hop->json
       (procedure->service
	(lambda (i)
	   (nptab-get-body (list-ref tabs i))))
       #f #f))
   
   (define (make-tab-div tab i)
      (with-access::xml-nptab-element tab (attributes (idt id) body)
	 (let ((click (format "hop_notepad_select( '~a', '~a', ~a )"
			      id idt (if history "true" "false"))))
	    (set! attributes
		  (cons* (cons "onclick" click)
			 (cons "class" (if (=fx i 0)
					   "hop-nptab-active hop-nptab"
					   "hop-nptab-inactive hop-nptab"))
			 attributes)))
	 (when (and (xml-delay? (car (xml-element-body tab)))
		    (null? (cdr (xml-element-body tab))))
	    (set! attributes (cons (cons "lang" "delay") attributes)))
	 (<DIV> :class "hop-notepad-body-tab"
	    :style (if (=fx i 0) "display: block" "display: none")
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
	     :class "hop-notepad"
	     :onkeyup (format "return ~a;" svc)
	     head
	     (<DIV> :class "hop-notepad-tabs" tabs)
	     (<DIV> :class "hop-notepad-body" bodies)
	     (when onchange
		(<SCRIPT> :class "hop-notepad-init"
		   (format "document.getElementById('~a').onchange = ~a"
			   id (obj->thunk onchange))))
	     attrs)))
   
;*---------------------------------------------------------------------*/
;*    <NOTEPAD> ...                                                    */
;*---------------------------------------------------------------------*/
(define-xml-compound <NOTEPAD> ((id #unspecified string)
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
	  (notepad id history attrs head body onchange))))

;*---------------------------------------------------------------------*/
;*    <NPHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <NPHEAD> ((id #unspecified string)
			       (attr)
			       body)
   (instantiate::xml-nphead-element
      (markup 'div)
      (id (xml-make-id id 'NPHEAD))
      (attributes (append! attr '(("class" . "hop-notepad-title"))))
      (body body)))
   
;*---------------------------------------------------------------------*/
;*    <NPTABHEAD> ...                                                  */
;*---------------------------------------------------------------------*/
(define-xml-compound <NPTABHEAD> ((id #unspecified string)
				  (attr)
				  body)
   (instantiate::xml-nptabhead-element
      (markup 'span)
      (id (xml-make-id id 'NPTABHEAD))
      (attributes (append! attr '(("class" . "hop-nptab-head"))))
      (body body)))
   
;*---------------------------------------------------------------------*/
;*    <NPTAB> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <NPTAB> ((id #unspecified string)
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
       (instantiate::xml-nptab-element
	  (markup 'span)
	  (id (xml-make-id id 'NPTAB))
	  (attributes attr)
	  (onselect onselect)
	  (head (car body))
	  (body (cdr body))))))

;*---------------------------------------------------------------------*/
;*    obj->thunk ...                                                   */
;*---------------------------------------------------------------------*/
(define (obj->thunk obj)
   (cond
      ((xml-tilde? obj)
       (format "function( event ) { ~a }" (xml-tilde-body obj)))
      ((string? obj)
       (format "function( event ) { ~a }" obj))
      (else
       "false")))

;*---------------------------------------------------------------------*/
;*    xml-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-nptab-element p backend)
   (with-access::xml-nptab-element obj (id head attributes onselect)
      (display "<span id='" p)
      (display id p)
      (display "'" p)
      (xml-write-attributes attributes p)
      (display ">" p)
      (when onselect
	 (display "<script>" p)
	 (display "document.getElementById( '" p)
	 (display id p)
	 (display "' ).onselect = " p)
	 (display (obj->thunk onselect) p)
	 (display "</script>" p))
      (xml-write head p backend)
      (display "</span>" p)))

