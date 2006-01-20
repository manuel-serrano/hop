;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/html-notepad.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Thu Jan 19 10:32:33 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of notepads.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-notepad

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
	    (class xml-nptab-element::xml-element))
   
   (export  (<NOTEPAD> . ::obj)
	    (<NPHEAD> . ::obj)
	    (<NPTAB> . ::obj)
	    (<NPTABHEAD> . ::obj))
   
   (eval    (export-exports)))

;*---------------------------------------------------------------------*/
;*    notepad-inline ...                                               */
;*---------------------------------------------------------------------*/
(define (notepad-inline id attr head tabs)
   (define (mark-nptab! i tab)
      (let* ((tid (xml-element-id tab))
	     (gid (xml-make-id #unspecified 'NOTEPAD-INLINE))
	     (ghost (cadr (xml-element-body tab)))
	     (cla (if (=fx i 1) "hop-nptab-active" "hop-nptab-inactive"))
	     (click (format "hop_notepad_inline( \"~a\", \"~a\", \"~a\" )"
			    tid gid id)))
	 (with-access::xml-element tab (attributes)
	    (set! attributes
		  (cons* (cons "class" cla)
			 (cons "onclick" click)
			 attributes)))
	 (<DIV> :class "hop-notepad-body-tab"
		:id gid
		:style (if (=fx i 1) "display: block" "display: none")
		(xml-ghost-body (cadr (xml-element-body tab))))))
   (let* ((tids (map mark-nptab! (iota (length tabs) 1) tabs))
	  (content  (list head
			  (<DIV> :class "hop-notepad-tabs" tabs)
			  (<DIV> :class "hop-notepad-body" tids))))
      (apply <DIV> :id id :class "hop-notepad" (append attr content))))

;*---------------------------------------------------------------------*/
;*    notepad-remote ...                                               */
;*---------------------------------------------------------------------*/
(define (notepad-remote id::bstring attr head tabs)
   (define (mark-nptab! i tab)
      (let* ((tid (xml-element-id tab))
	     (cla (if (=fx i 1) "hop-nptab-active" "hop-nptab-inactive"))
	     (ghost (cadr (xml-element-body tab)))
	     (gid (xml-element-id ghost))
	     (svc (scheme->javascript
		   (procedure->service
		    (lambda (tid)
		       (let loop ((tabs tabs))
			  (cond
			     ((null? tabs)
			      (error 'notepad "Can't find tab" tid))
			     ((string=? tid (xml-element-id (car tabs)))
			      (let* ((tab (cadr (xml-element-body (car tabs))))
				     (el (xml-ghost-body tab)))
				 (if (xml-delay? el)
				     ((xml-delay-thunk el))
				     el)))
			     (else
			      (loop (cdr tabs)))))))))
	     (click (format "hop_notepad_remote( ~a, \"~a\", \"~a\", \"~a\" )"
			    svc
			    tid gid id)))
	 (with-access::xml-element tab (attributes)
	    (set! attributes
		  (cons* (cons "class" cla)
			 (cons "onclick" click)
			 attributes)))))
   (let* ((default-tab (car tabs))
	  (default-el (xml-ghost-body (cadr (xml-element-body default-tab))))
	  (default-body (if (xml-delay? default-el)
			    ((xml-delay-thunk default-el))
			    default-el))
	  (content (list head
			 (<DIV> :class "hop-notepad-tabs" tabs)
			 (<DIV> :class "hop-notepad-body" default-body))))
      (for-each mark-nptab! (iota (length tabs) 1) tabs)
      (apply <DIV> :id id :class "hop-notepad" (append attr content))))
   
;*---------------------------------------------------------------------*/
;*    <NOTEPAD> ...                                                    */
;*---------------------------------------------------------------------*/
(define-xml-compound NOTEPAD ((id #unspecified string)
			      (inline #t)
			      (attr)
			      body)
   (let ((id (xml-make-id id 'NOTEPAD))
	 head)
      (if (and (pair? body) (xml-nphead-element? (car body)))
	  (begin
	     (set! head (car body))
	     (set! body (filter xml-nptab-element? (cdr body))))
	  (begin
	     (set! head (<NPHEAD> ""))
	     (set! body (filter xml-nptab-element? body))))
      (if (null? body)
	  (error '<NOTEPAD> "Missing <NPTAB> elements" id)
	  (if inline
	      (notepad-inline id attr head body)
	      (notepad-remote id attr head body)))))

;*---------------------------------------------------------------------*/
;*    <NPHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound NPHEAD ((id #unspecified string)
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
(define-xml-compound NPTABHEAD ((id #unspecified string)
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
(define-xml-compound NPTAB ((id #unspecified string)
			    (attr)
			    body)
   (if (and (pair? body) (xml-nptabhead-element? (car body)))
       (instantiate::xml-nptab-element
	  (markup 'SPAN)
	  (id (xml-make-id id 'NPTAB))
	  (attributes (append! attr '((class . "hop-notepad-tab"))))
	  (body (list (car body) (<GHOST> (cdr body)))))))
