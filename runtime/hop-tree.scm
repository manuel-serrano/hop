;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-tree.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 18 10:01:02 2005                          */
;*    Last change :  Thu Apr 27 08:12:31 2006 (serrano)                */
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
	       (cached::bool read-only))
	    
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
	 (body body))))

;*---------------------------------------------------------------------*/
;*    <TRHEAD> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-element <TRHEAD>)   
(define-xml html-trbody <TRBODY>)
   
;*---------------------------------------------------------------------*/
;*    <TRLEAF> ...                                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <TRLEAF> ((id #unspecified string)
			       (value "")
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
(define-method (xml-write obj::html-tree p encoding)
   (let ((parent (symbol->string (gensym 'TREE-PARENT))))
      (fprintf p "<span id='~a' class='hop-tree-parent'>" parent)
      (display " <script language='JavaScript'>" p)
      (html-write-tree 0 obj parent p)
      (display " </script>" p)
      (display "</span>" p)))

;*---------------------------------------------------------------------*/
;*    html-write-tree ...                                              */
;*---------------------------------------------------------------------*/
(define (html-write-tree level obj::html-tree parent::bstring p::output-port)
   (with-access::html-tree obj (id foldero folderc open head body
				   multiselect onselect onunselect cached)
      (let ((title (let ((ps (open-output-string)))
		      (xml-write-body (xml-element-body head) ps)
		      (close-output-port ps)))
	    (value (let ((c (assoc "value" (xml-element-attributes head))))
		      (if (pair? c)
			  (cdr c)
			  "")))
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
		     (scheme->javascript
		      (procedure->service
		       (lambda (level)
			  (html-write-tree-body level (car body) id p)))))))
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
		 (if onselect onselect "false")  ", "
		 (if onunselect onunselect "false")  ", "
		 ;; the value associated with the tree
		 "'" (if (string? value) (string-escape value #\') "") "' "
 		 ")"))))

;*---------------------------------------------------------------------*/
;*    xml-write-body ...                                               */
;*---------------------------------------------------------------------*/
(define (xml-write-body body p)
   (for-each (lambda (b) (xml-write b p 'ISO-8859-1)) body))

;*---------------------------------------------------------------------*/
;*    html-write-tree-body ...                                         */
;*---------------------------------------------------------------------*/
(define (html-write-tree-body level obj parent p)
   (instantiate::http-response-procedure
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
				      (html-write-tree level b parent p))
				     ((html-tree-leaf? b)
				      (html-write-tree-leaf b parent p))
				     (else
				      (error '<TREE> "Illegal tree body" b)))))
			    body))))))

;*---------------------------------------------------------------------*/
;*    html-write-tree-leaf ...                                         */
;*---------------------------------------------------------------------*/
(define (html-write-tree-leaf obj::html-tree-leaf parent p)
   (with-access::html-tree-leaf obj (file body value)
      (let ((sbody (let ((ps (open-output-string)))
		      (xml-write-body (xml-element-body obj) ps)
		      (close-output-port ps)))
	    (icon (or file (make-file-name (hop-icons-directory) "file.png"))))
	 (fprint p
		 "hop_make_tree_leaf( "
		 ;; parent
		 "document.getElementById('" parent "'), "
		 ;; the body
		 "\"" (string-for-read sbody) "\", "
		 ;; the value
		 "'" (or (and (string? value) (string-escape value #\')) "")
		 "', "
		 ;; the icon
		 "'" (string-escape icon #\') "' "
		 " )"))))
	    
