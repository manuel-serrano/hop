;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/dom.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 23 16:55:15 2005                          */
;*    Last change :  Wed Apr 12 19:10:27 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Restricted DOM implementation                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_dom

   (import __hop_xml)

   (export (%make-xml-document ::xml-document)
	   (dom-attributes::pair-nil ::obj)
	   (dom-owner-document node)
	   (dom-child-nodes::pair-nil ::obj)
	   (dom-first-child ::obj)
	   (dom-last-child ::obj)
	   (dom-next-sibling ::obj)
	   (dom-node-name node)
	   (dom-node-type node)
	   (dom-node-value node)
	   (dom-parent-node node)
	   (dom-previous-sibling node)
	   (dom-append-child! ::xml-markup new)
	   (generic dom-clone-node ::obj ::bool)
	   (dom-has-attributes node)
	   (dom-has-child-nodes node)
	   (dom-insert-before! node new ref)
	   (dom-normalize! node)
	   (dom-remove-child! node old)
	   (dom-replace-child! node new old)
	   (generic dom-get-element-by-id obj ::bstring)
	   (dom-get-elements-by-tag-name::pair-nil obj ::bstring)
	   (dom-get-attribute node ::bstring)
	   (dom-has-attribute?::bool node ::bstring)
	   (dom-remove-attribute! node name)
	   (dom-set-attribute! node name value))
   
   (export (class xml-document::xml-markup
	      (%make-xml-document)
	      (%idtable read-only (default (make-hashtable))))))

;*---------------------------------------------------------------------*/
;*    %make-xml-document ...                                           */
;*---------------------------------------------------------------------*/
(define (%make-xml-document doc::xml-document)
   (with-access::xml-document doc (body %idtable)
      (let loop ((body body))
	 (for-each (lambda (obj)
		      (when (xml-markup? obj)
			 (hashtable-put! %idtable (xml-markup-id obj) obj)
			 (loop (xml-markup-body obj))))
		   body))
      doc))

;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (dom-get-element-by-id obj id)
   (cond
      ((or (string? obj) (number? obj) (symbol? obj))
       #f)
      ((null? obj)
       #f)
      ((not obj)
       #f)
      (else
       (error 'dom-get-element-by-id "Illegal xml object" obj))))
   
;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id ::dom-document ...                         */
;*---------------------------------------------------------------------*/
(define-method (dom-get-element-by-id obj::xml-document id)
   (with-access::xml-document obj (%idtable)
      (hashtable-get %idtable id)))

;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id ::xml-if ...                               */
;*---------------------------------------------------------------------*/
(define-method (dom-get-element-by-id obj::xml-if id)
   (with-access::xml-if obj (then otherwise)
      (or (dom-get-element-by-id* then id)
	  (dom-get-element-by-id* otherwise id))))

;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id ::xml-ghost ...                            */
;*---------------------------------------------------------------------*/
(define-method (dom-get-element-by-id obj::xml-ghost id)
   (if (eq? id (xml-ghost-id obj))
       obj
       (dom-get-element-by-id* (xml-ghost-body obj) id)))

;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id ::xml-delay ...                            */
;*---------------------------------------------------------------------*/
(define-method (dom-get-element-by-id obj::xml-delay id)
   (if (eq? id (xml-delay-id obj))
       obj
       #f))

;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id ::xml-element ...                          */
;*---------------------------------------------------------------------*/
(define-method (dom-get-element-by-id obj::xml-element id)
   (if (eq? id (xml-element-id obj))
       obj
       (dom-get-element-by-id* (xml-element-body obj) id)))

;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id* ...                                       */
;*---------------------------------------------------------------------*/
(define (dom-get-element-by-id* els id)
   (if (pair? els)
       (or (dom-get-element-by-id* (car els) id)
	   (dom-get-element-by-id* (cdr els) id))
       (dom-get-element-by-id els id)))

;*---------------------------------------------------------------------*/
;*    dom-owner-document ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-owner-document obj)
   (when (xml-element? obj)
      (let loop ((parent (xml-element-parent obj)))
	 (cond
	    ((xml-document? parent)
	     parent)
	    ((xml-element? parent)
	     (loop (xml-element-parent obj)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    dom-attributes ...                                               */
;*---------------------------------------------------------------------*/
(define (dom-attributes node)
   (if (xml-markup? node)
       (with-access::xml-markup node (attributes)
	  attributes)
       '()))

;*---------------------------------------------------------------------*/
;*    dom-child-nodes ...                                              */
;*---------------------------------------------------------------------*/
(define (dom-child-nodes node)
   (if (xml-markup? node)
       (with-access::xml-markup node (body)
	  body)
       '()))

;*---------------------------------------------------------------------*/
;*    dom-first-child ...                                              */
;*---------------------------------------------------------------------*/
(define (dom-first-child node)
   (and (xml-markup? node)
	(with-access::xml-markup node (body)
	   (and (pair? body) (car body)))))

;*---------------------------------------------------------------------*/
;*    dom-last-child ...                                               */
;*---------------------------------------------------------------------*/
(define (dom-last-child node)
   (and (xml-markup? node)
	(with-access::xml-markup node (body)
	   (and (pair? body) (car (last-pair body))))))

;*---------------------------------------------------------------------*/
;*    dom-next-sibling ...                                             */
;*---------------------------------------------------------------------*/
(define (dom-next-sibling node)
   (and (xml-element? node)
	(with-access::xml-element node (parent)
	   (and (xml-markup? parent)
		(with-access::xml-markup parent (body)
		   (let ((child (assq node body)))
		      (and (pair? child)
			   (pair? (cdr child))
			   (cadr child))))))))

;*---------------------------------------------------------------------*/
;*    dom-node-name ...                                                */
;*---------------------------------------------------------------------*/
(define (dom-node-name node)
   (cond
      ((string? node)
       "#text")
      ((xml-markup? node)
       (symbol->string (xml-markup-markup node)))
      ((xml-document? node)
       "#document")
      (else
       "")))

;*---------------------------------------------------------------------*/
;*    dom-node-type ...                                                */
;*---------------------------------------------------------------------*/
(define (dom-node-type node)
   (cond
      ((string? node)
       'text)
      ((xml-markup? node)
       (xml-markup-markup node))
      ((xml-markup? node)
       'element)
      ((xml-document? node)
       'document)
      (else
       'unknown)))

;*---------------------------------------------------------------------*/
;*    dom-node-value ...                                               */
;*---------------------------------------------------------------------*/
(define (dom-node-value node)
   (cond
      ((string? node)
       node)
      ((xml-markup? node)
       #f)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    dom-parent-node ...                                              */
;*---------------------------------------------------------------------*/
(define (dom-parent-node node)
   (and (xml-element? node) (xml-element-parent node)))

;*---------------------------------------------------------------------*/
;*    dom-previous-sibling ...                                         */
;*---------------------------------------------------------------------*/
(define (dom-previous-sibling node)
   (and (xml-element? node)
	(with-access::xml-element node (parent)
	   (and (xml-markup? parent)
		(with-access::xml-markup parent (body)
		   (let loop ((body body))
		      (cond
			 ((null? body)
			  #f)
			 ((eq? (cdr body) node)
			  (car body))
			 (else
			  (loop (cdr body))))))))))

;*---------------------------------------------------------------------*/
;*    dom-append-child! ...                                            */
;*---------------------------------------------------------------------*/
(define (dom-append-child! node::xml-markup new)
   (with-access::xml-markup node (body)
      (let ((doc (dom-owner-document node)))
	 (when (xml-document? doc)
	    (with-access::xml-document doc (%idtable)
	       (let ((id (xml-markup-id new)))
		  (hashtable-remove! %idtable id)
		  (hashtable-put! %idtable id new)))))
      (set! body (cons new (remq! new body)))))

;*---------------------------------------------------------------------*/
;*    dom-clone-node ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (dom-clone-node obj deep::bool)
   (cond
      ((string? obj)
       (string-copy obj))
      ((pair? obj)
       (map (lambda (x) x) obj))
      (else
       obj)))

;*---------------------------------------------------------------------*/
;*    dom-clone-node ::css ...                                         */
;*---------------------------------------------------------------------*/
(define-method (dom-clone-node node::css deep::bool)
   (duplicate::css node))

;*---------------------------------------------------------------------*/
;*    dom-clone-node ::xml-if ...                                      */
;*---------------------------------------------------------------------*/
(define-method (dom-clone-node node::xml-if deep::bool)
   (if (not deep)
       (duplicate::xml-if node)
       (duplicate::xml-if node
	  (test (dom-clone-node (xml-if-test xml-if) deep))
	  (then (dom-clone-node (xml-if-then xml-if) deep))
	  (otherwise (dom-clone-node (xml-if-otherwise xml-if) deep)))))

;*---------------------------------------------------------------------*/
;*    dom-clone-node ::xml-ghost ...                                   */
;*---------------------------------------------------------------------*/
(define-method (dom-clone-node node::xml-ghost deep::bool)
   (duplicate::xml-ghost node))

;*---------------------------------------------------------------------*/
;*    dom-clone-node ::xml-delay ...                                   */
;*---------------------------------------------------------------------*/
(define-method (dom-clone-node node::xml-delay deep::bool)
   (duplicate::xml-delay node))

;*---------------------------------------------------------------------*/
;*    dom-clone-node ::xml-markup ...                                  */
;*---------------------------------------------------------------------*/
(define-method (dom-clone-node node::xml-markup deep::bool)
   (if (not deep)
       (duplicate::xml-markup node)
       (duplicate::xml-markup node
	  (attributes (dom-clone-node (xml-markup-attributes node) deep))
	  (body (dom-clone-node (xml-markup-body node) deep)))))
       
;*---------------------------------------------------------------------*/
;*    dom-clone-node ::xml-element ...                                 */
;*---------------------------------------------------------------------*/
(define-method (dom-clone-node node::xml-element deep::bool)
   (if (not deep)
       (duplicate::xml-element node)
       (duplicate::xml-element node
	  (attributes (dom-clone-node (xml-element-attributes node) deep))
	  (body (dom-clone-node (xml-element-body node) deep)))))

;*---------------------------------------------------------------------*/
;*    dom-clone-node ::xml-html ...                                    */
;*---------------------------------------------------------------------*/
(define-method (dom-clone-node node::xml-html deep::bool)
   (if (not deep)
       (duplicate::xml-html node)
       (duplicate::xml-html node
	  (prelude (dom-clone-node (xml-html-prelude node) deep))
	  (attributes (dom-clone-node (xml-html-attributes node) deep))
	  (body (dom-clone-node (xml-html-body node) deep)))))
       
;*---------------------------------------------------------------------*/
;*    dom-has-attributes ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-has-attributes node)
   (and (xml-markup? node) (pair? (xml-markup-attributes node))))

;*---------------------------------------------------------------------*/
;*    dom-has-child-nodes ...                                          */
;*---------------------------------------------------------------------*/
(define (dom-has-child-nodes node)
   (and (xml-markup? node) (pair? (xml-markup-body node))))

;*---------------------------------------------------------------------*/
;*    dom-insert-before! ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-insert-before! node new ref)
   (and (xml-element? node)
	(with-access::xml-element node (parent)
	   (and (xml-markup? parent)
		(with-access::xml-markup parent (body)
		   (let ((doc (dom-owner-document node))
			 (id (xml-markup-id new)))
		      (when (xml-document? doc)
			 (with-access::xml-document doc (%idtable)
			    (hashtable-remove! %idtable id)
			    (hashtable-put! %idtable id new))))
		   (let ((body (remq! new body)))
		      (cond
			 ((null? body)
			  (set! body (list new)))
			 ((eq? (car body) ref)
			  (set! body (cons new body)))
			 (else
			  (let loop ((body (cdr body))
				     (prev body))
			     (if (or (null? body) (eq? (car body) ref))
				 (set-cdr! prev (cons new body))
				 (loop (cdr body) body)))))))))))

;*---------------------------------------------------------------------*/
;*    dom-normalize! ...                                               */
;*---------------------------------------------------------------------*/
(define (dom-normalize! node)
   (when (xml-markup? node)
      (let loop ((body (xml-markup-body node))
		 (new '()))
	 (cond
	    ((null? body)
	     (set! body (reverse! new)))
	    ((not (string? (car body)))
	     (loop (cdr body) (cons (dom-normalize! (car body)) new)))
	    ((string=? body "")
	     (loop (cdr body) new))
	    ((null? (cdr body))
	     (loop '() (cons (car body) new)))
	    ((not (string? (cdr body)))
	     (loop (cdr body) (cons (car body) new)))
	    (else
	     (let liip ((body body)
			(str '()))
		(cond
		   ((null? body)
		    (loop (apply string-append (reverse! str)) '()))
		   ((not (string? (car body)))
		    (loop (apply string-append (reverse! str)) body))
		   (else
		    (liip (cdr body) (cons (car body) str))))))))))

;*---------------------------------------------------------------------*/
;*    dom-remove-child! ...                                            */
;*---------------------------------------------------------------------*/
(define (dom-remove-child! node old)
   (when (xml-markup? node)
      (let ((doc (dom-owner-document node)))
	 (when (xml-document? doc)
	    (with-access::xml-document doc (%idtable)
	       (hashtable-remove! %idtable (xml-markup-id old)))))
      (with-access::xml-markup node (body)
	 (set! body (remq! old body))
	 old)))

;*---------------------------------------------------------------------*/
;*    dom-replace-child! ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-replace-child! node new old)
   (and (xml-element? node)
	(with-access::xml-element node (parent)
	   (and (xml-markup? parent)
		(with-access::xml-markup parent (body)
		   (let loop ((body (remq! new body)))
		      (cond
			 ((null? body)
			  (error 'dom-replace-child "old not a child" node))
			 ((eq? (car body) old)
			  (let ((doc (dom-owner-document node))
				(id (xml-markup-id new)))
			     (when (xml-document? doc)
				(with-access::xml-document doc (%idtable)
				   (hashtable-remove! %idtable id)
				   (hashtable-put! %idtable id new))))
			  (set-car! body new))
			 (else
			  (loop (cdr body))))))))))
   
;*---------------------------------------------------------------------*/
;*    dom-get-elements-by-tag-name ...                                 */
;*---------------------------------------------------------------------*/
(define (dom-get-elements-by-tag-name::pair-nil obj name)
   (let ((sym (string->symbol (string-downcase name))))
      (let loop ((obj obj))
	 (cond
	    ((pair? obj)
	     (append-map loop obj))
	    ((xml-markup? obj)
	     (if (eq? sym (xml-markup-markup obj))
		 (cons obj (loop (xml-markup-body obj)))
		 (loop (xml-markup-body obj))))
	    (else
	     '())))))

;*---------------------------------------------------------------------*/
;*    dom-get-attribute ...                                            */
;*---------------------------------------------------------------------*/
(define (dom-get-attribute node name)
   (when (xml-markup? node)
      (with-access::xml-markup node (attributes)
	 (let ((c (assoc name attributes)))
	    (and (pair? c) (cdr c))))))

;*---------------------------------------------------------------------*/
;*    dom-has-attribute? ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-has-attribute? node name)
   (when (xml-markup? node)
      (with-access::xml-markup node (attributes)
	 (pair? (assq (string->symbol name) attributes)))))

;*---------------------------------------------------------------------*/
;*    dom-remove-attribute! ...                                        */
;*---------------------------------------------------------------------*/
(define (dom-remove-attribute! node name)
   (when (xml-markup? node)
      (with-access::xml-markup node (attributes)
	 (let ((c (assq (string->symbol name) attributes)))
	    (when (pair? c)
	       (set! attributes (remq! c attributes)))))))

;*---------------------------------------------------------------------*/
;*    dom-set-attribute!...                                            */
;*---------------------------------------------------------------------*/
(define (dom-set-attribute! node name value)
   (when (xml-markup? node)
      (with-access::xml-markup node (attributes)
	 (let ((c (assq (string->symbol name) attributes)))
	    (if (pair? c)
		(set-cdr! c value)
		(set! attributes (cons (cons name value) attributes)))))))
