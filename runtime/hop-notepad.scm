;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-notepad.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Tue Oct 10 16:44:42 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of notepads.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-notepad

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_js-lib
	    __hop_service)

   (static  (class xml-nphead-element::xml-element)
	    (class xml-nptabhead-element::xml-element)
	    (class xml-nptab-element::xml-element
	       (head::xml-nptabhead-element read-only)))
   
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
(define (notepad id attrs head tabs)
   (define svc
      (hop->json
       (procedure->service
	(lambda (i)
	   (nptab-get-body (list-ref tabs i))))))
   (define (make-tab-div tab i)
      (let* ((click (format "hop_notepad_inner_select( document.getElementById( \"~a\" ), ~a )"
			    id i)))
	 (with-access::xml-element tab (attributes)
	    (set! attributes
		  (cons* (cons "onclick" click)
			 (cons "class" (if (=fx i 0)
					   "hop-nptab-active hop-nptab"
					   "hop-nptab-inactive hop-nptab"))
			 attributes)))
	 (with-access::xml-nptab-element tab (attributes body)
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
		       body))))))
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
	     attrs)))
   
;*---------------------------------------------------------------------*/
;*    <NOTEPAD> ...                                                    */
;*---------------------------------------------------------------------*/
(define-xml-compound <NOTEPAD> ((id #unspecified string)
				(attrs)
				body)
   (let ((id (xml-make-id id 'NOTEPAD))
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
	  (notepad id attrs head body))))

;*---------------------------------------------------------------------*/
;*    <NPHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <NPHEAD> ((id #unspecified string)
			       (attr)
			       body)
   (instantiate::xml-nphead-element
      (markup 'DIV)
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
      (markup 'SPAN)
      (id (xml-make-id id 'NPTABHEAD))
      (attributes attr)
      (body body)))
   
;*---------------------------------------------------------------------*/
;*    <NPTAB> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <NPTAB> ((id #unspecified string)
			      (selected #f)
			      (attr)
			      body)
   (cond
      ((null? body)
       (error '<NPTAB> "Missing <NPTABHEAD> " id))
      ((not (xml-nptabhead-element? (car body)))
       (error '<NPTAB> "Illegal <NPTABHEAD> " (car body)))
      (else
       (instantiate::xml-nptab-element
	  (markup 'SPAN)
	  (id (xml-make-id id 'NPTAB))
	  (attributes attr)
	  (head (car body))
	  (body (cdr body))))))

;*---------------------------------------------------------------------*/
;*    xml-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-nptab-element p encoding backend)
   (with-access::xml-nptab-element obj (id head attributes)
      (display "<span id=\"" p)
      (display id p)
      (display "\"" p)
      (xml-write-attributes attributes p)
      (display ">" p)
      (xml-write head p encoding backend)
      (display "</span>" p)))

