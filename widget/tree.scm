;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/widget/tree.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Fri Jul 19 16:03:47 2013 (serrano)                */
;*    Copyright   :  2005-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of trees.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-tree

   (library hop)

   (static  (class html-tree::xml-element
	       (klass read-only)
	       (head read-only)
	       (open::obj read-only)
	       (multiselect::bool read-only)
	       (onselect read-only)
	       (onunselect read-only)
	       (onopen read-only)
	       (onclose read-only)
	       (value read-only)
	       (history::bool read-only)
	       (inline::bool (default #t))
	       (visible::bool (default #t))
	       (iconopen read-only)
	       (iconclose read-only)
	       (icondir read-only (default #f)))
	    
	    (class html-trbody::xml-element)
	    
	    (class html-tree-leaf::xml-element
	       (klass read-only)
	       (value read-only)
	       (icon read-only)
	       (iconerr read-only)))

   (export  (<TREE> . ::obj)
	    (<TRHEAD> . ::obj)
	    (<TRBODY> . ::obj)
	    (<TRLEAF> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    <TREE> ...                                                       */
;*---------------------------------------------------------------------*/
(define-tag <TREE> ((id #unspecified string)
		    (class #f)
		    (visible #t)
		    (open #f)
		    (multiselect #f)
		    (onselect #f)
		    (onunselect #f)
		    (onopen #f)
		    (onclose #f)
		    (value #unspecified)
		    (history #unspecified)
		    (inline #t boolean)
		    (iconopen #t)
		    (iconclose #t)
		    body)
   (let ((head ""))
      (when (and (pair? body) (xml-markup-is? (car body) 'trhead))
	 (set! head (car body))
	 (set! body (cdr body)))
      (when (any (lambda (e) (not (xml-markup-is? e 'trbody))) body)
	 (error (if (string? id) (format "<TREE id='~a'>" id) "<TREE>")
		"Illegal body"
		body))
      (instantiate::html-tree
	 (tag 'tree)
	 (klass (if (string? class) class ""))
	 (id (xml-make-id id 'TREE))
	 (visible visible)
	 (open open)
	 (history (if (boolean? history) history (not (eq? id #unspecified))))
	 (head head)
	 (multiselect multiselect)
	 (onselect onselect)
	 (onunselect onunselect)
	 (onopen onopen)
	 (onclose onclose)
	 (value value)
	 (inline inline)
	 (iconopen iconopen)
	 (iconclose iconclose)
	 (body body))))

;*---------------------------------------------------------------------*/
;*    <TRHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-element <TRHEAD>)   
(define-xml html-trbody #t <TRBODY>)
   
;*---------------------------------------------------------------------*/
;*    <TRLEAF> ...                                                     */
;*---------------------------------------------------------------------*/
(define-tag <TRLEAF> ((id #unspecified string)
		      (class #f)
		      (value #unspecified)
		      (inline #t boolean)
		      (icon #t)
		      body)
   (instantiate::html-tree-leaf
      (tag 'tree-leaf)
      (klass (if (string? class) class ""))
      (id (xml-make-id id 'TRLEAF))
      (value value)
      (icon (tree-icon icon inline "file.png"))
      (iconerr icon)
      (body body)))

;*---------------------------------------------------------------------*/
;*    tree-icon ...                                                    */
;*---------------------------------------------------------------------*/
(define (tree-icon icon inline default)
   (cond
      ((and (eq? icon #t) (not inline))
       ;; default non-inlined icon
       (make-file-name (hop-icons-directory) default))
      ((and inline (string? icon) (file-exists? icon))
       ;; an inlined image
       (img-base64-encode icon))
      (else
       ;; user specified icon
       icon)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-tree ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-tree p backend)
   (let ((parent (symbol->string (gensym 'TREE-PARENT))))
      (with-access::html-tree obj (klass)
	 (fprintf p "<div id='~a' class='hop-tree-container ~a'>" parent klass))
      (display " <script type='" p)
      (display (hop-mime-type) p)
      (display "'>" p)
      (display "hop_add_event_listener('" p)
      (display (string-escape parent #\') p)
      (display "', \"ready\", function() {" p)
      (html-write-tree 0 obj parent p backend)
      (display "}, true )" p)
      (display " </script>" p)
      (display "</div>" p)))

;*---------------------------------------------------------------------*/
;*    obj->js-tree-thunk ...                                           */
;*---------------------------------------------------------------------*/
(define (obj->js-tree-thunk obj)
   (cond
      ((isa? obj xml-tilde)
       (format "function( event ) { ~a }" (xml-tilde->return obj)))
      ((string? obj)
       (format "function( event ) { ~a }" obj))
      (else
       "false")))

;*---------------------------------------------------------------------*/
;*    html-write-tree ...                                              */
;*---------------------------------------------------------------------*/
(define (html-write-tree level
			 obj::html-tree
			 parent::bstring
			 p::output-port
			 be::xml-backend)
   (with-access::html-tree obj (id visible
				   open head body
				   multiselect
				   onselect onunselect
				   onopen onclose
				   value history
				   inline iconopen iconclose icondir)
      ;; set the parent relationship with the body
      (when (and (pair? body) (isa? (car body) html-trbody))
	 (with-access::html-trbody (car body) (parent)
	    (set! parent obj)))
      ;; the constructor call
      (display "hop_make_tree(" p)
      ;; parent 
      (display "document.getElementById('" p)
      (display (string-escape parent #\') p)
      (display "'), " p)
      ;; the ident of the tree
      (display "'" p)
      (display (string-escape id #\') p)
      (display "', " p)
      ;; the visibility
      (display (if visible "true," "false,") p)
      ;; the nesting level of the tree
      (display level p)
      (display ", " p)
      ;; service to get the body of the tree
      (cond
	 ((null? body)
	  (display "false" p))
	 ((delayed-tree-body? body)
	  (obj->javascript-attr
	   (procedure->service
	    (lambda ()
	       (let* ((p (open-output-string))
		      (v (html-write-tree-body
			    (+ 1 level) (car body) id p be))
		      (vp (close-output-port p)))
		  (or v vp))))
	   p))
	 ((service-tree-body? body)
	  (obj->javascript-attr
	   (procedure->service
	    (lambda ()
	       (let* ((p (open-output-string))
		      (v (html-write-tree-body
			    (+ 1 level) (car body) id p be))
		      (vp (close-output-port p)))
		  (or v vp))))
	   p))
	 (else
	  (display "function() {" p)
	  (html-write-tree-body (+ 1 level) (car body) id p be)
	  (display "}" p)))
      (display ", " p)
      ;; the title
      (let ((title (let ((ps (open-output-string)))
		      (with-access::xml-element head (body)
			 (xml-write-body body ps be))
		      (close-output-port ps))))
	 (display "\"" p)
	 (display (url-path-encode title) p)
	 (display "\"" p))
      (display ", " p)
      ;; is the tree open
      (if (isa? open xml-tilde)
	  (begin
	     (display (xml-tilde->expression open) p)
	     (display ", " p))
	  (display (if open "true, " "false, ") p))
      ;; is the tree cached
      (display
	 (if (or (delayed-tree-body? body) (service-tree-body? body))
	     "true, "
	     "false, ")
	 p)
      ;; multi-selection
      (display (if multiselect "true, " "false, ") p)
      ;; onselect/onunselect event handlers
      (display (obj->js-tree-thunk onselect) p)
      (display ", " p)
      (display (obj->js-tree-thunk onunselect) p)
      (display ", " p)
      ;; onopen/onclose event handlers
      (display (obj->js-tree-thunk onopen) p)
      (display ", " p)
      (display (obj->js-tree-thunk onclose) p)
      (display ", " p)
      ;; the value associated with the tree
      (if (string? value)
	  (begin
	     (display "'" p)
	     (display (string-escape value #\') p)
	     (display "', " p))
	  (display "''," p))
      ;; history
      (display (if history "true," "false,") p)
      ;; the icons
      (let ((iopen (cond
		      ((not iconopen)
		       #f)
		      ((>fx level 0)
		       (tree-icon iconopen inline "folder-open.png"))
		      (inline
		       (if (string? iconopen)
			   (img-base64-encode iconopen)
			   0))
		      (else
		       (tree-icon iconopen
				  inline
				  (make-file-name (hop-icons-directory)
						  "device.png")))))
	    (iclose (cond
		       ((not iconclose)
			#f)
		       ((>fx level 0)
			(tree-icon iconclose inline "folder-close.png"))
		       (inline
			(if (string? iconclose)
			    (img-base64-encode iconclose)
			    0))
		       (else
			(tree-icon iconclose
				   inline
				   (make-file-name (hop-icons-directory)
						   "device.png"))))))
	 (xml-write-expression iopen p)
	 (display "," p)
	 (xml-write-expression iconopen p)
	 (display "," p)
	 (xml-write-expression iclose p)
	 (display "," p)
	 (xml-write-expression iconclose p))
      ;; icon dir
      (cond
	 ((string? icondir)
	  (display ",'" p)
	  (display (string-escape icondir #\') p)
	  (display "' " p))
	 ((not inline)
	  (display ",'" p)
	  (display (string-escape (hop-icons-directory) #\') p)
	  (display "' " p)))
      (display ")" p)))

;*---------------------------------------------------------------------*/
;*    delayed-tree-body? ...                                           */
;*---------------------------------------------------------------------*/
(define (delayed-tree-body? body)
   (when (and (pair? body) (isa? (car body) html-trbody))
      (with-access::xml-element (car body) (body)
	 (and (pair? body) (isa? (car body) xml-delay)))))
	   
;*---------------------------------------------------------------------*/
;*    service-tree-body? ...                                           */
;*---------------------------------------------------------------------*/
(define (service-tree-body? body)
   (when (and (pair? body) (isa? (car body) html-trbody))
      (with-access::xml-element (car body) (body)
	 (and (pair? body) (service? (car body))))))
	   
;*---------------------------------------------------------------------*/
;*    xml-write-body ...                                               */
;*---------------------------------------------------------------------*/
(define (xml-write-body body p be)
   (for-each (lambda (b) (xml-write b p be)) body))

;*---------------------------------------------------------------------*/
;*    html-write-tree-body ...                                         */
;*---------------------------------------------------------------------*/
(define (html-write-tree-body level obj parent p be)
   (with-access::xml-element obj (body)
      (bind-exit (return)
	 (for-each (lambda (b)
		      (let loop ((b b))
			 (cond
			    ((pair? b)
			     (for-each loop b))
			    ((isa? b xml-delay)
			     (with-access::xml-delay b (thunk)
				(loop (thunk))))
			    ((service? b)
			     (loop ((service-proc b))))
			    ((isa? b html-tree)
			     (html-write-tree level b parent p be)
			     (display ";\n" p))
			    ((isa? b html-tree-leaf)
			     (html-write-tree-leaf b parent p be)
			     (display ";\n" p))
			    ((null? b)
			     #f)
			    ((isa? b %http-response)
			     (return b))
			    ((isa? b xml-tilde)
			     (return
			      (instantiate::http-response-hop
				 (backend (hop-xml-backend))
				 (start-line "HTTP/1.0 501 Internal Server Error")
				 (content-type (hop-mime-type))
				 (value b))))
			    (else
			     (error "<TREE>" "Illegal tree body" b)))))
		   body)
	 #f)))

;*---------------------------------------------------------------------*/
;*    html-write-tree-leaf ...                                         */
;*---------------------------------------------------------------------*/
(define (html-write-tree-leaf obj::html-tree-leaf parent p be)
   (with-access::html-tree-leaf obj (icon iconerr body value klass)
      (display "hop_make_tree_leaf(" p)
      ;; parent
      (display "document.getElementById('" p)
      (display parent p)
      (display "'), " p)
      ;; the class
      (display "\"" p)
      (display klass p)
      (display "\", " p)
      ;; the body
      (let ((sbody (let ((ps (open-output-string)))
		      (with-access::xml-element obj (body)
			 (xml-write-body body ps be))
		      (close-output-port ps))))
	 (display "\"" p)
	 (display (url-path-encode sbody) p)
	 (display "\"" p))
      (display ", " p)
      ;; the value
      (if (string? value)
	  (begin
	     (display #\' p)
	     (display (string-escape value #\') p)
	     (display "', " p))
	  (display "''," p))
      ;; the icon
      (xml-write-expression icon p)
      ;; the icon error
      (display ", " p)
      (xml-write-expression iconerr p)
      (display ")" p)))
