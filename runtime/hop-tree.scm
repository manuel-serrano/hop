;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-tree.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Mon Oct  9 08:48:27 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP implementation of trees.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-tree

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_js-lib
	    __hop_service)

   (static  (class html-tree::xml-element
	       (foldero read-only)
	       (folderc read-only)
	       (head read-only)
	       (open::bool read-only)
	       (multiselect::bool read-only)
	       (onselect read-only)
	       (onunselect read-only)
	       (cached::bool read-only)
	       (value read-only))
	    
	    (class html-trbody::xml-element)
	    
	    (class html-tree-leaf::xml-element
	       (file read-only)
	       (value read-only)))

   (export  (<TREE> . ::obj)
	    (<TRHEAD> . ::obj)
	    (<TRBODY> . ::obj)
	    (<TRLEAF> . ::obj)))
   
;*---------------------------------------------------------------------*/
;*    <TREE> ...                                                       */
;*---------------------------------------------------------------------*/
(define-xml-compound <TREE> ((id #unspecified string)
			     (foldero #f)
			     (folderc #f)
			     (open #f)
			     (multiselect #f)
			     (onselect #f)
			     (onunselect #f)
			     (cached #f)
			     (value #unspecified)
			     body)
   (let ((head ""))
      (when (and (pair? body) (xml-markup-is? (car body) 'trhead))
	 (set! head (car body))
	 (set! body (cdr body)))
      (when (any? (lambda (e) (not (xml-markup-is? e 'trbody))) body)
	 (error (if (string? id) (format "<tree id='~a'>" id) '<tree>)
		"Illegal body"
		body))
      (instantiate::html-tree
	 (markup 'tree)
	 (id (xml-make-id id 'TREE))
	 (foldero foldero)
	 (folderc folderc)
	 (open open)
	 (head head)
	 (multiselect multiselect)
	 (onselect onselect)
	 (onunselect onunselect)
	 (cached cached)
	 (value value)
	 (body body))))

;*---------------------------------------------------------------------*/
;*    <TRHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-element <TRHEAD>)   
(define-xml html-trbody #t <TRBODY>)
   
;*---------------------------------------------------------------------*/
;*    <TRLEAF> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <TRLEAF> ((id #unspecified string)
			       (value #unspecified)
			       (file #f)
			       body)
   (instantiate::html-tree-leaf
      (markup 'tree-leaf)
      (id (xml-make-id id 'TRLEAF))
      (file file)
      (value value)
      (body body)))

;*---------------------------------------------------------------------*/
;*    xml-write ::html-tree ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::html-tree p encoding backend)
   (let ((parent (symbol->string (gensym 'TREE-PARENT))))
      (fprintf p "<span id='~a' class='hop-tree-parent'>" parent)
      (display " <script type='" p)
      (display (hop-javascript-mime-type) p)
      (display "'>" p)
      (html-write-tree 0 obj parent p backend)
      (display " </script>" p)
      (display "</span>" p)))

;*---------------------------------------------------------------------*/
;*    obj->js-thunk ...                                                */
;*---------------------------------------------------------------------*/
(define (obj->js-thunk obj)
   (cond
      ((xml-tilde? obj)
       (tilde-make-thunk obj))
      ((string? obj)
       (format "function() { ~a }" obj))
      (else
       "function() { return false; }")))

;*---------------------------------------------------------------------*/
;*    html-write-tree ...                                              */
;*---------------------------------------------------------------------*/
(define (html-write-tree level
			 obj::html-tree
			 parent::bstring
			 p::output-port
			 be::xml-backend)
   (with-access::html-tree obj (id foldero folderc open head body
				   multiselect onselect onunselect cached value)
      (let ((title (let ((ps (open-output-string)))
		      (xml-write-body (xml-element-body head) ps be)
		      (close-output-port ps)))
	    (fo (cond
		   (foldero
		    foldero)
		   ((>fx level 0)
		    (make-file-name (hop-icons-directory) "folder-open.png"))
		   (else
		    (make-file-name (hop-icons-directory) "base.png"))))
	    (fc (cond
		   (folderc
		    folderc)
		   ((>fx level 0)
		    (make-file-name (hop-icons-directory) "folder-close.png"))
		   (else
		    (make-file-name (hop-icons-directory) "base.png"))))
	    (svc (if (null? body)
		     "false"
		     (hop->json
		      (procedure->service
		       (lambda (level)
			  (html-write-tree-body level (car body) id p be)))))))
	 ;; set the parent relationship with the body
	 (when (and (pair? body) (html-trbody? (car body)))
	    (html-trbody-parent-set! (car body) obj))
	 (fprint p
		 "hop_make_tree( "
		 ;; parent 
		 "document.getElementById('" parent "'), "
		 ;; the ident of the tree
		 "'" id "', "
		 ;; the nesting level of the tree
		 level ", "
		 ;; service to get the body of the tree
		 svc ", "
		 ;; the title
		 "\"" (string-for-read title) "\", "
		 ;; is the tree open
		 (if open "true, " "false, ")
		 ;; is the tree cached
		 (if cached "true, " "false, ")
		 ;; the icons
		 "'" (string-escape (hop-icons-directory) #\') "', "
		 "'" (string-escape fo #\') "', " 
		 "'" (string-escape fc #\') "', " 
		 ;; multi-selection
		 (if multiselect "true, " "false, ")
		 ;; onselect/onunselect event handlers
		 (obj->js-thunk onselect) ", "
		 (obj->js-thunk onunselect) ", "
		 ;; the value associated with the tree
		 "'"
		 (if (string? value) (string-escape value #\') "")
		 "' "
 		 ")"))))

;*---------------------------------------------------------------------*/
;*    xml-write-body ...                                               */
;*---------------------------------------------------------------------*/
(define (xml-write-body body p be)
   (for-each (lambda (b) (xml-write b p 'ISO-8859-1 be)) body))

;*---------------------------------------------------------------------*/
;*    html-write-tree-body ...                                         */
;*---------------------------------------------------------------------*/
(define (html-write-tree-body level obj parent p be)
   (instantiate::http-response-procedure
      (request (instantiate::http-request))
      (proc (lambda (p)
	       (with-access::xml-element obj (body)
		  (for-each (lambda (b)
			       (let loop ((b b))
				  (cond
				     ((pair? b)
				      (map loop b))
				     ((xml-delay? b)
				      (loop ((xml-delay-thunk b))))
				     ((html-tree? b)
				      (html-write-tree level b parent p be))
				     ((html-tree-leaf? b)
				      (html-write-tree-leaf b parent p be))
				     ((null? b)
				      #unspecified)
				     (else
				      (error '<TREE> "Illegal tree body" b)))))
			    body))))))

;*---------------------------------------------------------------------*/
;*    html-write-tree-leaf ...                                         */
;*---------------------------------------------------------------------*/
(define (html-write-tree-leaf obj::html-tree-leaf parent p be)
   (with-access::html-tree-leaf obj (file body value)
      (let ((sbody (let ((ps (open-output-string)))
		      (xml-write-body (xml-element-body obj) ps be)
		      (close-output-port ps)))
	    (icon (or file
		      (make-file-name (hop-icons-directory) "file.png"))))
	 (fprint p
		 "hop_make_tree_leaf( "
		 ;; parent
		 "document.getElementById('" parent "'), "
		 ;; the body
		 "\"" (string-for-read sbody) "\", "
		 ;; the value
		 "'"
		 (if (string? value) (string-escape value #\') "")
		 "', "
		 ;; the icon
		 "'" (string-escape icon #\') "' "
		 " )"))))
	    
