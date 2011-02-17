;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/widget/paned.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Thu Feb 17 09:33:56 2011 (serrano)                */
;*    Copyright   :  2005-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of paned.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-paned

   (library hop)

   (static  (class html-paned::xml-element
	       (klass read-only)
	       (fraction read-only)
	       (style read-only (default #f))
	       (onresize read-only)
	       (orientation::symbol read-only))

	    (class html-pan::xml-element
	       (klass read-only)))

   (export  (<PANED> . ::obj)
	    (<PAN> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    <PANED> ...                                                      */
;*    -------------------------------------------------------------    */
;*    See __hop_css for HSS types.                                     */
;*---------------------------------------------------------------------*/
(define-markup <PANED> ((id #unspecified string)
			(class #unspecified string)
			(fraction #f)
			(onresize "")
			(orientation 'vertical)
			(style #f)
			body)
   (cond
      ((null? body)
       (error "<PANED>" "Illegal body, missing two pans" body))
      ((null? (cdr body))
       (error "<PANED>" "Illegal body, missing one pan" body))
      ((not (memq orientation '(horizontal vertical)))
       (error "<PANED>" "Illegal orientation" orientation))
      ((null? (cddr body))
       (instantiate::html-paned
	  (tag 'paned)
	  (id (xml-make-id id 'PANED))
	  (klass class)
	  (fraction fraction)
	  (onresize onresize)
	  (style style)
	  (orientation orientation)
	  (body body)))
      (else
       (error "<PANED>" "Illegal body, too many pan" body))))

;*---------------------------------------------------------------------*/
;*    <PAN> ...                                                        */
;*---------------------------------------------------------------------*/
(define-markup <PAN> ((id #unspecified string)
		      (class #unspecified string)
		      (attr)
		      body)
   (instantiate::html-pan
      (tag 'pan)
      (id (xml-make-id id 'PAN))
      (klass class)
      (attributes attr)
      (body body)))

;*---------------------------------------------------------------------*/
;*    xml-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-paned p backend)
   (with-access::html-paned obj (id klass fraction onresize style body orientation)
      (fprintf p "<div hssclass='hop-paned' id='~a'" id)
      (when (string? klass) (fprintf p " class='~a'" klass))
      (when style (fprintf p " style='~a'" style))
      (display ">" p)
      (fprintf p "<div class='hop-paned-inner hop-paned-~a' id='~a-inner'>"
	       orientation id)
      (fprintf p "<span class='hop-paned-fraction' id='~a-fraction'></span>" id)
      (fprintf p "<div class='hop-paned-pan hop-paned-el1' id='~a-el1'>" id)
      (xml-write (car body) p backend)
      (fprint p "</div>")
      (fprintf p "<div class='hop-paned-cursor' id='~a-cursor'></div>" id)
      (fprintf p "<div class='hop-paned-pan hop-paned-el2' id='~a-el2'>" id)
      (xml-write (cadr body) p backend)
      (fprint p "</div>")
      (fprint p "</div>")
      (display " <script type='" p)
      (display (hop-javascript-mime-type) p)
      (display "'>" p)
      (fprintf p
	       "hop_add_event_listener( ~s, 'ready', function(e ) {
	       hop_init_paned_~a( ~s, ~a, function(event) { ~a } )} )"
	       id
	       orientation
	       id
	       (cond
		  ((string? fraction)
		   (string-append "\"" fraction "\""))
		  ((not fraction)
		   "false")
		  (else
		   fraction))
	       (cond
		  ((xml-tilde? onresize)
		   (xml-tilde->return onresize))
		  ((string? onresize)
		   onresize)
		  (else
		   "")))
      (display " </script></div>" p)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-pan ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-pan p backend)
   (with-access::html-pan obj (id klass body attributes)
      (display "<div id='" p)
      (display id p)
      (display "' class='hop-pan" p)
      (when (string? klass)
	 (display " " p)
	 (display klass p))
      (display "' " p)
      (xml-write-attributes attributes p backend)
      (display ">" p)
      (xml-write body p backend)
      (display "</div>" p)))

;*---------------------------------------------------------------------*/
;*    xml-compare ::html-paned ...                                     */
;*---------------------------------------------------------------------*/
(define-method (xml-compare a1::html-paned a2)
   
   (define (xml-compare-pan c1 c2)
      (if (and (xml-markup? c2) (eq? (xml-markup-tag c2) 'div))
	  (xml-compare c1 (dom-first-child c2))
	  (call-next-method)))
   
   (if (and (xml-markup? a2)
	    (eq? (xml-markup-tag a2) 'div)
	    (equal? (dom-get-attribute a2 "hssclass") "hop-paned"))
       (let ((b (dom-first-child a2)))
	  (xml-compare-pan (car (html-paned-body a1))
			   (dom-first-child b))
	  (xml-compare-pan (cadr (html-paned-body a1))
			   (caddr (dom-child-nodes b))))
       (call-next-method)))

;*---------------------------------------------------------------------*/
;*    xml-compare ::html-pan ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-compare a1::html-pan a2)
   (if (and (xml-markup? a2)
	    (eq? (xml-markup-tag a2) 'div)
	    (equal? (dom-get-attribute a2 "class") "hop-pan"))
       (xml-compare (dom-child-nodes a1) (dom-child-nodes a2))
       (call-next-method)))

;*---------------------------------------------------------------------*/
;*    ast->string-list ...                                             */
;*---------------------------------------------------------------------*/
(define (ast->string-list ast)
   (cond
      ((or (string? ast) (number? ast))
       "_")
      ((list? ast)
       (map ast->string-list ast))
      ((xml-element? ast)
       (with-access::xml-element ast (tag body id)
	  (let ((c (dom-get-attribute ast "class")))
	     (if c
		 `(,(symbol-append '< tag '>) :class
		     ,(string-append "\"" c "\"")
		     :id ,id
		     ,@(map ast->string-list body))
		 `(,(symbol-append '< tag '>)
		   :id ,id
		   ,@(map ast->string-list body))))))
      ((xml-markup? ast)
       (with-access::xml-markup ast (tag body)
	  `(,(symbol-append '< tag '>)
	    ,@(map ast->string-list body))))
      ((xml-tilde? ast)
       (with-access::xml-tilde ast (body)
	  `(~ -)))
      ((symbol? ast)
       (symbol->string ast))
      (else
       (typeof ast))))

