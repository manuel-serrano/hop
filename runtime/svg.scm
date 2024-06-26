;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/runtime/svg.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  2 08:22:25 2007                          */
;*    Last change :  Tue May 14 12:49:54 2024 (serrano)                */
;*    Copyright   :  2007-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop SVG support.                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_svg
   
   (library web http)
   
   (include "xml.sch")
   
   (import  __hop_param
	    __hop_types
	    __hop_xml-types
	    __hop_xml
	    __hop_html-base
	    __hop_misc
	    __hop_charset
	    __hop_js-comp
	    __hop_service
	    __hop_cache
	    __hop_priv)
   
   (static (class svg-img-markup
	      name
	      attributes)
	   (class svg-img-markup+::svg-img-markup
	      body))
   
   (export (<SVG> . ::obj)
	   
	   (<SVG:DEFS> . ::obj)
	   (<SVG:RECT> . ::obj)
	   (<SVG:CIRCLE> . ::obj)
	   (<SVG:ELLIPSE> . ::obj)
	   (<SVG:FILTER> . ::obj)
	   (<SVG:FEGAUSSIANBLUR>  . ::obj)
	   (<SVG:FECOLORMATRIX> . ::obj)
	   (<SVG:FOREIGNOBJECT> . ::obj)
	   (<SVG:G> . ::obj)
	   (<SVG:IMG> . ::obj)
	   (<SVG:LINE> . ::obj)
	   (<SVG:PATH> . ::obj)
	   (<SVG:POLYLINE> . ::obj)
	   (<SVG:POLYGON> . ::obj)
	   (<SVG:TEXT> . ::obj)
	   (<SVG:TEXTPATH> . ::obj)
	   (<SVG:TREF> . ::obj)
	   (<SVG:TSPAN> . ::obj)
	   (<SVG:RADIALGRADIENT> . ::obj)
	   (<SVG:LINEARGRADIENT> . ::obj)))

;*---------------------------------------------------------------------*/
;*    svg-img-tree-cache ...                                           */
;*---------------------------------------------------------------------*/
(define svg-img-tree-cache #f)

;*---------------------------------------------------------------------*/
;*    init-svg-img-cache! ...                                          */
;*---------------------------------------------------------------------*/
(define (init-svg-img-cache!)
   (unless svg-img-tree-cache
      (when (>fx (hop-svg-img-cache-size) 0)
	 (set! svg-img-tree-cache
	       (instantiate::cache-memory
		  (max-entries (hop-svg-img-cache-size))
		  (max-file-size (hop-svg-img-max-file-size-cache)))))))

;*---------------------------------------------------------------------*/
;*    svg-img-cache-put! ...                                           */
;*---------------------------------------------------------------------*/
(define (svg-img-cache-put! name tree)
   (when svg-img-tree-cache
      (with-access::cache-memory svg-img-tree-cache ((size max-file-size))
	 (when (and (file-exists? name) (<elong (file-size name) size))
	    (cache-put! svg-img-tree-cache name tree))
	 tree)))

;*---------------------------------------------------------------------*/
;*    svg-img-cache-get ...                                            */
;*---------------------------------------------------------------------*/
(define (svg-img-cache-get name)
   (when svg-img-tree-cache
      (let ((ce (cache-get svg-img-tree-cache name)))
	 (when (isa? ce cache-entry)
	    (with-access::cache-entry ce (value)
	       value)))))

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
(define-tag <SVG> ((id #unspecified)
		   (%context #f)
		   (xmlns "http://www.w3.org/2000/svg")
		   (attributes)
		   body)
   (instantiate::xml-element
      (tag 'svg)
      (id (xml-make-id (xml-primitive-value id %context) 'svg))
      (attributes `(:xmlns ,(xml-primitive-value xmlns %context)
		      ,@(map (lambda (a) (xml-primitive-value a %context))
			   attributes)))
      (body body)))

;; misc
(define-xml xml-element #t <SVG:PATH> :tag path)
(define-xml xml-element #t <SVG:G> :tag g)
(define-xml xml-element #t <SVG:DEFS> :tag defs)
(define-xml xml-element #t <SVG:FOREIGNOBJECT> :tag foreignObject)
;; filter
(define-xml xml-element #t <SVG:FILTER> :tag filter)
(define-xml xml-element #t <SVG:FEGAUSSIANBLUR> :tag feGaussianBlur)
(define-xml xml-element #t <SVG:FECOLORMATRIX> :tag feColorMatrix)
;; basic shapes
(define-xml xml-element #t <SVG:RECT> :tag rect)
(define-xml xml-element #t <SVG:CIRCLE> :tag circle)
(define-xml xml-element #t <SVG:ELLIPSE> :tag ellipse)
(define-xml xml-element #t <SVG:LINE> :tag line)
(define-xml xml-element #t <SVG:POLYLINE> :tag polyline)
(define-xml xml-element #t <SVG:POLYGON> :tag polygon)
;; text
(define-xml xml-element #t <SVG:TEXT> :tag text)
(define-xml xml-element #t <SVG:TSPAN> :tag tspan)
(define-xml xml-element #t <SVG:TREF> :tag tref)
(define-xml xml-element #t <SVG:TEXTPATH> :tag textPath)
;; gradients
(define-xml xml-element #t <SVG:RADIALGRADIENT> :tag radialGradient)
(define-xml xml-element #t <SVG:LINEARGRADIENT> :tag linearGradient)

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
		       (error "<SVG-IMG>"
			      "Premature end of file"
			      ip)
		       (error "<SVG-IMG>"
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
		(let* ((key (the-subsymbol 0 -1))
		       (val (read-attribute-value ip)))
		   (values key val)))
	       (#\>
		(values '> #unspecified))
	       (else
		(let ((c (the-failure)))
		   (if (eof-object? c)
		       (error "<SVG-IMG>"
			      "Premature end of file"
			      ip)
		       (error "<SVG-IMG>"
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
	  ((or (eq? (car o) 'xml-decl) (eq? (car o) 'instruction))
	   #unspecified)
	  ((memq (car o) '(declaration comment))
	   ;; ms: 13 jan 2009
 	   ;; (display (cdr o))
	   #unspecified)
	  (else
	  (for-each (lambda (o) (show-svg-img o prefix)) o))))
      (else
       (error "<SVG:IMG>" "Illegal object" o))))
       
;*---------------------------------------------------------------------*/
;*    show-svg-img-attributes                                          */
;*---------------------------------------------------------------------*/
(define (show-svg-img-attributes attr prefix)
   
   (define (kwote s)
      (if (string? s)
	  (string-replace! s #\' #\")
	  s))
   
   (let loop ((a attr))
      (when (pair? a)
	 (display " ")
	 (if (and (eq? (car a) :%location) (pair? (cdr a)))
	     (loop (cddr a))
	     (begin
		(display (keyword->string! (car a)))
		(display "='")
		(cond
		   ((eq? (car a) :id)
		    (display prefix)
		    (when (cadr a)
		       (display #\-)
		       (display (xml-attribute-encode (kwote (cadr a))))))
		   ((and (eq? (car a) :xlink:href)
			 (string? (cadr a))
			 (char=? (string-ref (cadr a) 0) #\#))
		    (display "#")
		    (display prefix)
		    (display #\-)
		    (display
		       (kwote (substring (cadr a) 1 (string-length (cadr a))))))
		   ((and (eq? (car a) :style) (string? (cadr a)))
		    (display
		       (kwote
			  (pregexp-replace*
			     "url[(]#" (cadr a)
			     (string-append "url(#" prefix "-")))))
		   ((and (string? (cadr a)) (substring-at? (cadr a) "url(#" 0))
		    (display "url(#")
		    (display prefix)
		    (display #\-)
		    (display
		       (kwote (substring (cadr a) 5 (string-length (cadr a))))))
		   (else
		    (display (kwote (xml-attribute-encode (cadr a))))))
		(display "'")
		(loop (cddr a)))))))

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
   (let ((attr (append-map (lambda (a)
			      (list (symbol->keyword (car a)) (cdr a)))
			   a)))
      (if (null? b)
	  (instantiate::svg-img-markup (name n) (attributes attr))
	  (instantiate::svg-img-markup+ (name n) (attributes attr) (body b)))))

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
(define (read-svg-img-prefix id uattributes p src title inl width height class)
   
   (define (dimension-value str)
      (let ((len (string-length str)))
	 (if (and (>fx len 2) (substring-at? str "px" (-fx len 2)))
	     (substring str 0 (-fx len 2))
	     str)))

   (define (tune-svg! el)
      (with-access::svg-img-markup el (attributes)
	 (let ((svgheight (plist-assq :height attributes))
	       (svgwidth (plist-assq :width attributes))
	       (svgclass (plist-assq :class attributes))
	       (viewbox (plist-assq :viewBox attributes))
	       (uid (plist-assq :id attributes)))
	    ;; fix the id
	    (when id
	       (if uid
		   (plist-set-first! uid id)
		   (set! attributes (cons* :id id attributes))))
	    ;; the dimensions
	    (cond
	       (inl
		(cond
		   (width
		    (if svgwidth
			(plist-set-first! svgwidth width)
			(set! attributes (cons* :width width attributes))))
		   (height
		    (when svgwidth
		       ;; remove the width element
		       (set-cdr! svgwidth (cddr svgwidth))
		       (set! attributes (remq! :width attributes)))))
		(cond
		   (height
		    (if svgheight
		       (plist-set-first! svgheight height)
		       (set! attributes (cons* :height height attributes))))
		   (width
		    (when svgheight
		       ;; remove the height element
		       (set-cdr! svgheight (cddr svgheight))
		       (set! attributes (remq! :height attributes)))))
		(when class
		   (if svgclass
		       (plist-set-first! svgclass class)
		       (set! attributes (cons* :class class attributes)))))
	       (viewbox
		(when svgheight
		   (plist-set-first! svgheight "100%"))
		(when svgwidth
		   (plist-set-first! svgwidth "100%")))
	       ((and svgwidth svgheight)
		(let ((vb (format "0 0 ~a ~a"
				  (dimension-value (cadr svgwidth))
				  (dimension-value (cadr svgheight)))))
		   (plist-set-first! svgwidth "100%")
		   (plist-set-first! svgheight "100%")
		   (set! attributes `(:viewBox ,vb ,@attributes)))))
	    (when (pair? uattributes)
	       (set! attributes (append! attributes uattributes))))))

   (define (set-svg-title! tree title)
      (when (isa? tree svg-img-markup+)
	 (with-access::svg-img-markup+ tree (body)
	    (for-each (lambda (n)
			 (when (isa? n svg-img-markup+)
			    (with-access::svg-img-markup+ n (name body)
			       (when (eq? name 'title)
				  (set! body (list title))))))
	       body))))

   (define (parse-and-cache-xml-tree port src)
      (let ((tree (xml-parse port
			     :content-length 0
			     :encoding (hop-charset)
			     :procedure create-svg-img-markup)))
	 (svg-img-cache-put! src tree)))

   (init-svg-img-cache!)

   (with-output-to-string
      (lambda ()
	 (let ((tree (or (svg-img-cache-get src)
			 (parse-and-cache-xml-tree p src))))

	    ;; patch the svg element
	    (let loop ((tree tree))
	       (cond
		  ((and (isa? tree svg-img-markup)
			(with-access::svg-img-markup tree (name)
			   (or (eq? name 'svg) (eq? name 'svg:svg))))
		   (tune-svg! tree)
		   (when title (set-svg-title! tree title))
		   #t)
		  ((and (pair? tree) (not (symbol? (car tree))))
		   (any loop tree))
		  (else
		   #f)))
	    
	    (let ((pref (if (string? id) id (symbol->string (gensym)))))
	       (show-svg-img tree pref))))))

;*---------------------------------------------------------------------*/
;*    read-svg-img-brute ...                                           */
;*    -------------------------------------------------------------    */
;*    This function does not parse the image. It simply scans the      */
;*    first SVG markup then it reads the entire file.                  */
;*---------------------------------------------------------------------*/
(define (read-svg-img-brute id attributes p inl width height class)
   
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
		      (error "<SVG:IMG>" "Illegal file format" ip)))
		  ip)))

   (multiple-value-bind (encoding prelude)
      (read-upto-svg-markup p)
      (let loop ((attrs '())
		 (svgheight #f)
		 (svgwidth #f)
		 (viewbox #f)
		 (svgclass #f))
	 (multiple-value-bind (key val)
	    (read-attribute p)
	    (case key
	       ((>)
		(let* ((str (read-string p))
		       (body (charset-convert str encoding (hop-charset))))
		   (cond
		      (inl
		       (tprint "SVGclass=" svgclass " " class)
		       (list prelude
			  (if svgheight svgheight (if height (format " height=~a" height) ""))
			  (if svgwidth svgwidth (if width (format " width=~a" width) ""))
			  (if svgclass svgclass (if class (format " class=~a" class) ""))
			  viewbox
			  (reverse! attrs) ">" body))
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
		      ((and svgheight svgwidth)
		       ;; we have both a width and height, we force
		       ;; the inclusion of a viewBox
		       (list prelude
			     " height=\"100%\" width=\"100%\""
			     (format " viewBox=\"~a ~a ~a ~a\""
				     0 0
				     (dimension-value (cadr svgwidth))
				     (dimension-value (cadr svgheight)))
			     (reverse! attrs)
			     ">" body))
		      (else
		       ;; we don't have enough information, we leave
		       ;; the sizing as it is
		       (list prelude
			     svgheight
			     svgwidth
			     (reverse! attrs) ">" body)))))
	       ((height)
		(loop attrs (list " height=" (if (and inl height) height val)) svgwidth viewbox svgclass))
	       ((width)
		(loop attrs svgheight (list " width=" (if (and inl width) width val)) viewbox svgclass))
	       ((class)
		(loop attrs svgheight (list " class=" (if (and inl class) class val)) viewbox svgclass))
	       ((viewBox)
		(loop attrs svgheight svgwidth (list " viewBox=" val) svgclass))
	       ((id)
		(loop (if (string? id)
			  (cons (list " id=\"" id "\"") attrs)
			  (cons (list " id=" val) attrs))
		      svgheight
		      svgwidth
		      viewbox
		      svgclass))
	       (else
		(loop (cons (list " " (symbol->string key) "=" val)
			    attrs)
		      svgheight
		      svgwidth
		      viewbox
		      svgclass)))))))

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
(define (read-svg-img id prefix attributes p src title inl width height class)
   (if prefix
       (read-svg-img-prefix id attributes p src title inl width height class)
       (read-svg-img-brute id attributes p inl width height class)))

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
(define-tag <SVG:IMG> ((id #f)
		       (%context #t)
		       (class #f)
		       (width #f)
		       (height #f)
		       (style "text-align: center")
		       (title #f)
		       (src #unspecified)
		       (prefix #t boolean)
		       (display "-moz-inline-box; -moz-box-orient:vertical; display: inline-block")
		       (container "div")
		       (attrs))
   (set! src (xml-primitive-value src %context))
   (set! style (xml-primitive-value style %context))
   (set! title (or title (basename src)))
   (cond
      ((not (string? src))
       (error "<SVG:IMG>" "Illegal image src" src))
      ((not (file-exists? src))
       (error "<SVG:IMG>" "Cannot find image" src))
      (else
       (let* ((inl (string=? display "inline"))
	      (img (call-with-input-file
			 (if (string=? (suffix src) "svgz")
			     (string-append "gzip:" src)
			     src)
		      (lambda (port)
			 (read-svg-img id prefix attrs port src title inl
			    width height class))))
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
	  (if inl
	      (instantiate::xml-svg
		 (tag 'svg:img)
		 (id (xml-make-id 'svg:img))
		 (attributes `(:src ,src ,@attrs))
		 (body (list img)))
	      (apply
		 <DIV> :%context %context :style style2 :class class :id id
		 (instantiate::xml-svg
		    (tag 'svg:img)
		    (id (xml-make-id 'svg:img))
		    (attributes `(:src ,src))
		    (body (list img)))
		 attrs))))))
