;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/hop-svg.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  2 08:22:25 2007                          */
;*    Last change :  Fri Jun 27 10:58:59 2008 (serrano)                */
;*    Copyright   :  2007-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop SVG support.                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-svg

   (library web)
   
   (include "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_charset
	    __hop_js-lib
	    __hop_service)

   (static (class xml-svg::xml-element)
	   (class svg-img-markup
	      name
	      attributes)
	   (class svg-img-markup+::svg-img-markup
	      body))
   
   (export (<SVG> . ::obj)
	   (<SVG:G> . ::obj)
	   (<SVG:DEFS> . ::obj)
	   (<SVG:RECT> . ::obj)
	   (<SVG:CIRCLE> . ::obj)
	   (<SVG:ELLIPSE> . ::obj)
	   (<SVG:LINE> . ::obj)
	   (<SVG:POLYLINE> . ::obj)
	   (<SVG:POLYGON> . ::obj)
	   (<SVG:TEXT> . ::obj)
	   (<SVG:TSPAN> . ::obj)
	   (<SVG:TREF> . ::obj)
	   (<SVG:TEXTPATH> . ::obj)
	   (<SVG:PATH> . ::obj)
	   (<SVG:FOREIGN> . ::obj)
	   (<SVG:IMG> . ::obj)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-svg ...                                          */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-svg p backend)
   (with-access::xml-svg obj (body)
      (let loop ((body body))
	 (cond
	    ((string? body)
	     (display body p))
	    ((pair? body)
	     (for-each loop body))))))

;*---------------------------------------------------------------------*/
;*    Standards SVG elements                                           */
;*---------------------------------------------------------------------*/
(define-xml-compound <SVG> ((id #unspecified string)
			    (xmlns "http://www.w3.org/2000/svg" string)
			    (attributes)
			    body)
   (instantiate::xml-element
      (markup 'svg)
      (id (xml-make-id id 'svg))
      (attributes (cons (cons 'xmlns xmlns) attributes))
      (body body)))

;; misc
(define-xml xml-element #t <SVG:PATH> :markup path)
(define-xml xml-element #t <SVG:G> :markup g)
(define-xml xml-element #t <SVG:DEFS> :markup defs)
(define-xml xml-element #t <SVG:FOREIGN> :markup foreignObject)
;; basic shapes
(define-xml xml-element #t <SVG:RECT> :markup rect)
(define-xml xml-element #t <SVG:CIRCLE> :markup circle)
(define-xml xml-element #t <SVG:ELLIPSE> :markup ellipse)
(define-xml xml-element #t <SVG:LINE> :markup line)
(define-xml xml-element #t <SVG:POLYLINE> :markup polyline)
(define-xml xml-element #t <SVG:POLYGON> :markup polygon)
;; text
(define-xml xml-element #t <SVG:TEXT> :markup text)
(define-xml xml-element #t <SVG:TSPAN> :markup tspan)
(define-xml xml-element #t <SVG:TREF> :markup tref)
(define-xml xml-element #t <SVG:TEXTPATH> :markup textPath)

;*---------------------------------------------------------------------*/
;*    read-attribute-value ...                                         */
;*---------------------------------------------------------------------*/
(define (read-attribute-value ip)
   (read/rp (regular-grammar ()
	       ((or (: #\" (* (or (out #\") (: #\\ #\"))) #\")
		    (: #\' (* (or (out #\") (: #\\ #\'))) #\'))
		(the-string))
	       (else
		(let ((c (the-failure)))
		   (if (eof-object? c)
		       (error '<SVG-IMG>
			      "Premature end of file"
			      ip)
		       (error '<SVG-IMG>
			      "Illegal svg image attribute value"
			      (string-append "{" (string c) "}"
					     (read-line ip)))))))
	    ip))
		 
;*---------------------------------------------------------------------*/
;*    read-attribute ...                                               */
;*---------------------------------------------------------------------*/
(define (read-attribute ip)
   (read/rp (regular-grammar ((blank (in " \t\n")))
	       ((+ blank)
		(ignore))
	       ((: (+ (out ">= \t\n")) #\=)
		(let* ((key (the-subsymbol 0 (-fx (the-length) 1)))
		       (val (read-attribute-value ip)))
		   (values key val)))
	       (#\>
		(values '> #unspecified))
	       (else
		(let ((c (the-failure)))
		   (if (eof-object? c)
		       (error '<SVG-IMG>
			      "Premature end of file"
			      ip)
		       (error '<SVG-IMG>
			      "Illegal svg image attribute"
			      (string-append "{" (string c) "}"
					     (read-line ip)))))))
	    ip))

;*---------------------------------------------------------------------*/
;*    show-svg-img ::obj ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (show-svg-img o prefix)
   (cond
      ((string? o)
       (display o))
      ((number? o)
       (display o))
      ((pair? o)
       (cond
	  ((eq? (car o) 'instruction)
	   #unspecified)
	  ((eq? (car o) 'declaration)
	   (display (cdr o)))
	  (else
	  (for-each (lambda (o) (show-svg-img o prefix)) o))))
      (else
       (error 'show-svg-img "Illegal object" o))))
       
;*---------------------------------------------------------------------*/
;*    show-svg-img-attributes                                          */
;*---------------------------------------------------------------------*/
(define (show-svg-img-attributes l prefix)
   (for-each (lambda (a)
		(display " ")
		(display (car a))
		(display "='")
		(cond
		   ((eq? (car a) 'id)
		    (display prefix)
		    (display (xml-attribute-encode (cdr a))))
		   ((and (eq? (car a) 'xlink:href)
			 (string? (cdr a))
			 (char=? (string-ref (cdr a) 0) #\#))
		       (display "#")
		       (display prefix)
		       (display (substring (cdr a) 1 (string-length (cdr a)))))
		   ((and (eq? (car a) 'style) (string? (cdr a)))
		    (display
		     (pregexp-replace*
		      "url[(]#" (cdr a) (string-append "url(#" prefix))))
		   ((and (string? (cdr a)) (substring-at? (cdr a) "url(#" 0))
		    (display "url(#")
		    (display prefix)
		    (display (substring (cdr a) 5 (string-length (cdr a)))))
		   (else
		    (display (xml-attribute-encode (cdr a)))))
		(display "'"))
	     l))

;*---------------------------------------------------------------------*/
;*    show-svg-img-markup-open ...                                     */
;*---------------------------------------------------------------------*/
(define (show-svg-img-markup-open m a prefix)
   (display "<")
   (display m)
   (when (pair? a) (show-svg-img-attributes a prefix)))
			 
;*---------------------------------------------------------------------*/
;*    show-svg-img ::svg-img-markup ...                                */
;*---------------------------------------------------------------------*/
(define-method (show-svg-img o::svg-img-markup prefix)
   (with-access::svg-img-markup o (name attributes)
      (show-svg-img-markup-open name attributes prefix)
      (display "/>")))

;*---------------------------------------------------------------------*/
;*    show-svg-img ::svg-img-markup+ ...                               */
;*---------------------------------------------------------------------*/
(define-method (show-svg-img o::svg-img-markup+ prefix)
   (with-access::svg-img-markup+ o (name attributes body)
      (show-svg-img-markup-open name attributes prefix)
      (display ">")
      (show-svg-img body prefix)
      (display "</")
      (display name)
      (display ">")))

;*---------------------------------------------------------------------*/
;*    create-svg-img-markup ...                                        */
;*---------------------------------------------------------------------*/
(define (create-svg-img-markup n a b)
   (if (null? b)
       (instantiate::svg-img-markup (name n) (attributes a))
       (instantiate::svg-img-markup+ (name n) (attributes a) (body b))))

;*---------------------------------------------------------------------*/
;*    read-svg-img-prefix ...                                          */
;*    -------------------------------------------------------------    */
;*    This function parses the entire SVG image.                       */
;*    -------------------------------------------------------------    */
;*    It prefixes all the id and xlink:ref attribute                   */
;*    with PREFIX. This stage is in general required in order          */
;*    to avoid identifiers collisions when including several svg       */
;*    images inside a single xhtml document.                           */
;*---------------------------------------------------------------------*/
(define (read-svg-img-prefix id uattributes p)
   
   (define (dimension-value str)
      (let ((len (string-length str)))
	 (if (and (>fx len 2) (substring-at? str "px" (-fx len 2)))
	     (substring str 0 (-fx len 2))
	     str)))

   (define (tune-svg! el)
      (with-access::svg-img-markup el (attributes)
	 (let ((height (assq 'height attributes))
	       (width (assq 'width attributes))
	       (viewbox (assq 'viewBox attributes))
	       (uid (assq 'id attributes)))
	    ;; fix the id
	    (if uid
		(set-cdr! uid id)
		(set! attributes (cons (cons 'id id) attributes)))
	    ;; the dimensions
	    (cond
	       (viewbox
		(when height
		   (set-cdr! height "100%"))
		(when width
		   (set-cdr! width "100%")))
	       ((and width height)
		(let ((vb (format "0 0 ~a ~a"
				  (dimension-value (cdr width))
				  (dimension-value (cdr height)))))
		   (set-cdr! width "100%")
		   (set-cdr! height "100%")
		   (set! attributes (cons (cons 'viewBox vb) attributes)))))
	    (when (pair? uattributes)
	       (set! attributes (append! attributes uattributes))))))
   
   (with-output-to-string
      (lambda ()
	 (let ((tree (xml-parse (current-input-port)
				:content-length 0
				:encoding (hop-charset)
				:procedure create-svg-img-markup)))

	    ;; patch the svg element
	    (let loop ((tree tree))
	       (cond
		  ((and (svg-img-markup? tree)
			(eq? (svg-img-markup-name tree) 'svg))
		   (tune-svg! tree)
		   #t)
		  ((and (pair? tree) (not (symbol? (car tree))))
		   (any loop tree))
		  (else
		   #f)))
	    
	    (let ((pref (if (string? id)
			    (string-append id "-")
			    (string-append (symbol->string (gensym)) "-"))))
	       (show-svg-img tree pref))))))

;*---------------------------------------------------------------------*/
;*    read-svg-img-brute ...                                           */
;*    -------------------------------------------------------------    */
;*    This function does not parse the image. It simply scans the      */
;*    first SVG markup then it reads the entire file.                  */
;*---------------------------------------------------------------------*/
(define (read-svg-img-brute id attributes p)
   
   (define (dimension-value str)
      (let ((len (string-length str)))
	 (if (and (>fx len 4) (substring-at? str "px" (-fx len 3)))
	     (substring str 1 (-fx len 3))
	     (substring str 1 (-fx len 1)))))

   (define (read-upto-svg-markup ip)
      (let ((encoding #f))
	 (read/rp (regular-grammar ()
		     ("<?xml"
		      (set! encoding (read-svg-img-encoding ip))
		      (ignore))
		     ("<svg"
		      (values encoding "\n<svg"))
		     ("<svg:svg"
		      (values encoding "\n<svg:svg"))
		     ((or (+ (out #\<)) (+ #\<))
		      (ignore))
		     (else
		      (error '<SVG:IMG> "Illegal file format" ip)))
		  ip)))

   (multiple-value-bind (encoding prelude)
      (read-upto-svg-markup p)
      (let loop ((attrs '())
		 (height #f)
		 (width #f)
		 (viewbox #f))
	 (multiple-value-bind (key val)
	    (read-attribute p)
	    (case key
	       ((>)
		(let* ((str (read-string p))
		       (body (charset-convert str encoding (hop-charset))))
		   (cond
		      (viewbox
		       ;; the image has already a viewbox
		       ;; don't change anything
		       (let* ((l (pregexp-match
				  "\"([^ ]+) ([^ ]+) ([^ ]+) ([^ ]+)\""
				  (cadr viewbox)))
			      (vb (if (pair? l)
				      (format "\"~a ~a ~a ~a\""
					      0 0
					      (car (cdddr l))
					      (cadr (cdddr l)))
				      viewbox)))
			  (list prelude
				" height=\"100%\" width=\"100%\""
				(car viewbox)
				vb
				(reverse! attrs)
				">" body)))
		      ((and height width)
		       ;; we have both a width and height, we force
		       ;; the inclusion of a viewBox
		       (list prelude
			     " height=\"100%\" width=\"100%\""
			     (format " viewBox=\"~a ~a ~a ~a\""
				     0 0
				     (dimension-value (cadr width))
				     (dimension-value (cadr height)))
			     (reverse! attrs)
			     ">" body))
		      (else
		       ;; we don't have enough information, we leave
		       ;; the sizing as it is
		       (list prelude
			     height
			     width
			     (reverse! attrs) ">" body)))))
	       ((height)
		(loop attrs (list " height=" val) width viewbox))
	       ((width)
		(loop attrs height (list " width=" val) viewbox))
	       ((viewBox)
		(loop attrs height width (list " viewBox=" val)))
	       ((id)
		(loop (if (string? id)
			  (cons (list " id=\"" id "\"") attrs)
			  (cons (list " id=" val) attrs))
		      height
		      width
		      viewbox))
	       (else
		(loop (cons (list " " (symbol->string key) "=" val)
			    attrs)
		      height
		      width
		      viewbox)))))))

;*---------------------------------------------------------------------*/
;*    read-svg-img-encoding ...                                        */
;*---------------------------------------------------------------------*/
(define (read-svg-img-encoding ip)
   (let loop ((encoding 'UTF-8))
      (multiple-value-bind (key val)
	 (read-attribute ip)
	 (case key
	    ((>)
	     encoding)
	    ((encoding)
	     (string->symbol (string-upcase! val)))
	    (else
	     (loop encoding))))))

;*---------------------------------------------------------------------*/
;*    read-svg-img ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function reads a SVG image and:                             */
;*      1- it removes the header <?xml ...>                            */
;*      2- it inject the user required identity id='...'               */
;*      3- it set height and width to "100%"                           */
;*      4- it introduces a viewBox from older height and width         */
;*      5- it translates the iso-latin encoding into HOP-CHARSET       */
;*      6- it rebinds svg identifiers                                  */   
;*---------------------------------------------------------------------*/
(define (read-svg-img id prefix attributes p)
   (if prefix
       (read-svg-img-prefix id attributes p)
       (read-svg-img-brute id attributes p)))

;*---------------------------------------------------------------------*/
;*    height->width ...                                                */
;*---------------------------------------------------------------------*/
(define (height->width dim)
   (if (and (string? dim) (string-suffix? "ex" dim))
       (string-append (substring dim 0 (-fx (string-length dim) 2)) "em")
       dim))

;*---------------------------------------------------------------------*/
;*    width->height ...                                                */
;*---------------------------------------------------------------------*/
(define (width->height dim)
   (if (and (string? dim) (string-suffix? "em" dim))
       (string-append (substring dim 0 (-fx (string-length dim) 2)) "ex")
       dim))

;*---------------------------------------------------------------------*/
;*    SVG:IMG ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <SVG:IMG> ((id #unspecified)
				(class #f)
				(width #f)
				(height #f)
				(style "text-align: center" string)
				(src #unspecified string)
				(prefix #t boolean)
				(display "-moz-inline-box; -moz-box-orient:vertical; display:inline-block")
				(attrs))
   (cond
      ((not (string? src))
       (error '<SVG-IMG> "Illegal image src" src))
      ((not (file-exists? src))
       (error '<SVG-IMG> "Cannot find image" src))
      (else
       (let* ((img (with-input-from-file
			 (if (string=? (suffix src) "svgz")
			     (string-append "gzip:" src)
			     src)
		      (lambda ()
 			 (read-svg-img id prefix attrs (current-input-port)))))
	      (style0 (format "display: ~a; position: relative; ~a" display style))
	      (style1 (cond
			 (width
			  (format "width: ~a; ~a" width style0))
			 (height
			  (format "width: ~a; ~a" (height->width height) style0))
			 (else
			  style0)))
	      (style2 (cond
			 (height
			  (format "height: ~a; ~a" height style1))
			 (width
			  (format "height: ~a; ~a" (width->height width) style1))
			 (else
			  style1))))
	  (<DIV> :style style2 :class class
	     (instantiate::xml-svg
		(markup 'dummy)
		(id 'dummy)
		(attributes '())
		(body (list img))))))))
