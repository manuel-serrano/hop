;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/dom.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 23 16:55:15 2005                          */
;*    Last change :  Mon May 31 17:20:31 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Restricted DOM implementation                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_dom

   (import __hop_xml
	   __hop_priv)

   (export (dom-get-attributes::pair-nil ::obj)
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
	   (dom-remove-child! node old)
	   (dom-append-child! ::xml-markup new)
	   (dom-set-child-node! ::xml-markup new)
	   (dom-replace-child! node new old)
	   (dom-insert-before! node new ref)
	   (generic dom-clone-node ::obj ::bool)
	   (dom-has-attributes? node)
	   (dom-has-child-nodes? node)
	   (dom-normalize! node)
	   (generic dom-get-element-by-id obj ::bstring)
	   (dom-get-elements-by-tag-name::pair-nil obj ::bstring)
	   (dom-get-elements-by-class::pair-nil obj ::bstring)
	   (dom-get-attribute node ::bstring)
	   (dom-has-attribute?::bool node ::bstring)
	   (dom-remove-attribute! node name)
	   (dom-set-attribute! node name value)
	   (dom-node-element? node)
	   (dom-node-text? node)
	   (dom-node-document? node)
	   (dom-node-document-fragment? node)
	   (dom-node-attr? node)
	   (dom-inner-html-set! ::xml-markup ::obj)
	   (innerHTML-set! ::xml-markup obj)))

;*---------------------------------------------------------------------*/
;*    doc-update-idtable! ...                                          */
;*---------------------------------------------------------------------*/
(define (doc-update-idtable! doc body)
   (with-access::xml-document doc (%idtable)
      
      (define (update-xml-markup! obj)
	 (when (xml-element? obj)
	    (hashtable-put! %idtable (xml-element-id obj) obj))
	 (update-body! (xml-markup-body obj)))

      (define (update-body! body)
	 (cond
	    ((xml-markup? body)
	     (update-xml-markup! body))
	    ((pair? body)
	     (for-each update-body! body))))

      (update-body! body)))

;*---------------------------------------------------------------------*/
;*    %xml-constructor ::xml-document ...                              */
;*---------------------------------------------------------------------*/
(define-method (%xml-constructor doc::xml-document)
   (with-access::xml-document doc (body %idtable)
      (doc-update-idtable! doc body)
      doc))

;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (dom-get-element-by-id obj id::bstring)
   (when (pair? obj)
      (dom-get-element-by-id* obj id)))
   
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
;*    dom-get-element-by-id ::xml-delay ...                            */
;*---------------------------------------------------------------------*/
(define-method (dom-get-element-by-id obj::xml-delay id)
   (when (string=? id (xml-delay-id obj))
      obj))

;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id ::xml-markup ...                           */
;*---------------------------------------------------------------------*/
(define-method (dom-get-element-by-id obj::xml-markup id)
   (dom-get-element-by-id* (xml-markup-body obj) id))

;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id ::xml-element ...                          */
;*---------------------------------------------------------------------*/
(define-method (dom-get-element-by-id obj::xml-element id)
   (if (and (string? (xml-element-id obj)) (string=? id (xml-element-id obj)))
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
	     (loop (xml-element-parent parent)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    dom-get-attributes ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-get-attributes node)
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
;*    dom-node-name ...                                                */
;*---------------------------------------------------------------------*/
(define (dom-node-name node)
   (cond
      ((string? node)
       "#text")
      ((xml-markup? node)
       (symbol->string (xml-markup-tag node)))
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
       (xml-markup-tag node))
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
;*    dom-set-parent-node! ...                                         */
;*---------------------------------------------------------------------*/
(define (dom-set-parent-node! node parent)
   (cond
      ((xml-element? node) (xml-element-parent-set! node parent))
      ((xml-tilde? node) (xml-tilde-parent-set! node parent))))

;*---------------------------------------------------------------------*/
;*    dom-ancestor-node ...                                            */
;*    -------------------------------------------------------------    */
;*    Transitive closure on parent node until no parent found.         */
;*---------------------------------------------------------------------*/
(define (dom-ancestor-node obj)
   (when (xml-element? obj)
      (let ((parent (xml-element-parent obj)))
	 (when (xml-element? parent)
	    (let loop ((nparent (xml-element-parent parent))
		       (parent parent))
	       (cond
		  ((not nparent)
		   parent)
		  ((xml-document? nparent)
		   nparent)
		  (else
		   (loop (xml-element-parent nparent) nparent))))))))

;*---------------------------------------------------------------------*/
;*    dom-previous-sibling ...                                         */
;*---------------------------------------------------------------------*/
(define (dom-previous-sibling node)
   (and (xml-element? node)
	(with-access::xml-element node (parent)
	   (and (xml-markup? parent)
		(with-access::xml-markup parent (body)
		   (let loop ((body body)
			      (prev #f))
		      (cond
			 ((null? body)
			  prev)
			 ((eq? (car body) node)
			  prev)
			 (else
			  (loop (cdr body) (car body))))))))))

;*---------------------------------------------------------------------*/
;*    dom-next-sibling ...                                             */
;*---------------------------------------------------------------------*/
(define (dom-next-sibling node)
   (and (xml-element? node)
	(with-access::xml-element node (parent)
	   (and (xml-markup? parent)
		(with-access::xml-markup parent (body)
		   (let ((child (memq node body)))
		      (and (pair? child)
			   (pair? (cdr child))
			   (cadr child))))))))

;*---------------------------------------------------------------------*/
;*    dom-remove-child! ...                                            */
;*---------------------------------------------------------------------*/
(define (dom-remove-child! node old)
   (when (xml-markup? node)
      (let ((doc (dom-owner-document node)))
	 (when (xml-document? doc)
	    (with-access::xml-document doc (%idtable)
	       (hashtable-remove! %idtable (xml-element-id old)))))
      (with-access::xml-markup node (body)
	 (set! body (remq! old body))
	 (when (eq? (dom-parent-node old) node)
	    (dom-set-parent-node! old #f))
	 old)))

;*---------------------------------------------------------------------*/
;*    dom-append-child! ...                                            */
;*---------------------------------------------------------------------*/
(define (dom-append-child! node::xml-markup new)
   (let ((ancp (dom-ancestor-node node))
	 (ancn (dom-ancestor-node new)))
      ;; re-parent new
      (if (and ancp (eq? ancp ancn))
	  (let ((parent (dom-parent-node new)))
	     (when (xml-markup? parent)
		(with-access::xml-markup parent (body)
		   (set! body (remq! new body)))))
	  (let ((doc (dom-owner-document node)))
	     (when (xml-document? doc)
		(with-access::xml-document doc (%idtable)
		   (doc-update-idtable! doc new))))))
   ;; and new to node
   (dom-set-parent-node! new node)
   (with-access::xml-markup node (body)
      (set! body (append! body (list new)))))

;*---------------------------------------------------------------------*/
;*    dom-set-child-node! ...                                          */
;*---------------------------------------------------------------------*/
(define (dom-set-child-node! node::xml-markup new)
   (with-access::xml-markup node (body)
      (set! body '())
      (dom-append-child! node new)))

;*---------------------------------------------------------------------*/
;*    dom-replace-child! ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-replace-child! node new old)
   (when (xml-element? node)
      (let ((ancp (dom-ancestor-node node))
	    (ancn (dom-ancestor-node new))
	    (anco (dom-ancestor-node old))
	    (docp (dom-owner-document node)))
	 ;; re-parent the new node
	 (if (and ancp (eq? ancp ancn))
	     (let ((parent (dom-parent-node new)))
		(when (xml-markup? parent)
		   (with-access::xml-markup parent (body)
		      (set! body (remq! new body)))))
	     (when (xml-document? docp)
		(with-access::xml-document docp (%idtable)
		   (doc-update-idtable! docp new))))
	 ;; unparent the old node
	 (dom-set-parent-node! old #f)
	 (let ((doco (dom-owner-document old)))
	    (when (and docp (eq? docp doco))
	       (with-access::xml-document doco (%idtable)
		  (hashtable-remove! %idtable (xml-element-id old))))))
      ;; replace the elements
      (dom-set-parent-node! new node)
      (with-access::xml-markup node (body)
	 (let loop ((body body))
	    (cond
	       ((null? body)
		(error 'dom-replace-child "old not a child" node))
	       ((eq? (car body) old)
		(set-car! body new))
	       (else
		(loop (cdr body))))))))

;*---------------------------------------------------------------------*/
;*    dom-insert-before! ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-insert-before! node new ref)
   (when (xml-markup? node)
      (with-access::xml-markup node (body)
	 (cond
	    ((not ref)
	     (dom-append-child! node new))
	    ((null? body)
	     (error 'dom-insert-before! "ref not a parent child" ref))
	    (else
	     (let ((ancp (dom-ancestor-node node))
		   (ancn (dom-ancestor-node new)))
		;; re-parent new
		(if (and ancp (eq? ancp ancn))
		    (let ((parent (dom-parent-node new)))
		       (when (xml-markup? parent)
			  (with-access::xml-markup parent (body)
			     (set! body (remq! new body)))))
		    (let ((doc (dom-owner-document node)))
		       (when (xml-document? doc)
			  (with-access::xml-document doc (%idtable)
			     (doc-update-idtable! doc new))))))
	     ;; add new
	     (dom-set-parent-node! new node)
	     (if (eq? (car body) ref)
		 (set! body (cons new body))
		 (let loop ((body (cdr body))
			    (prev body))
		    (cond
		       ((null? body)
			(set! body (list new)))
		       ((eq? (car body) ref)
			(set-cdr! prev (cons new body)))
		       (else
			(loop (cdr body) body))))))))))

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
	  (attributes (dom-clone-node (xml-html-attributes node) deep))
	  (body (dom-clone-node (xml-html-body node) deep)))))
       
;*---------------------------------------------------------------------*/
;*    dom-has-attributes? ...                                          */
;*---------------------------------------------------------------------*/
(define (dom-has-attributes? node)
   (and (xml-markup? node) (pair? (xml-markup-attributes node))))

;*---------------------------------------------------------------------*/
;*    dom-has-child-nodes? ...                                         */
;*---------------------------------------------------------------------*/
(define (dom-has-child-nodes? node)
   (and (xml-markup? node) (pair? (xml-markup-body node))))

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
;*    dom-get-elements-by-tag-name ...                                 */
;*---------------------------------------------------------------------*/
(define (dom-get-elements-by-tag-name::pair-nil obj name)
   (let ((sym (string->symbol (string-downcase name))))
      (let loop ((obj obj))
	 (cond
	    ((pair? obj)
	     (append-map loop obj))
	    ((xml-markup? obj)
	     (if (eq? sym (xml-markup-tag obj))
		 (cons obj (loop (xml-markup-body obj)))
		 (loop (xml-markup-body obj))))
	    (else
	     '())))))

;*---------------------------------------------------------------------*/
;*    dom-get-elements-by-class ...                                    */
;*---------------------------------------------------------------------*/
(define (dom-get-elements-by-class::pair-nil obj name)
   (let loop ((obj obj))
      (cond
	 ((pair? obj)
	  (append-map loop obj))
	 ((xml-markup? obj)
	  (let ((c (dom-get-attribute obj "class")))
	     ;; CARE: may be we should check that name is part
	     ;; of the class attribute. If we decide to change this,
	     ;; the change should be reported in the JS implementation
	     ;; inside the file hop-dom.js
	     (if (and (string? c) (string=? c name))
		 (cons obj (loop (xml-markup-body obj)))
		 (loop (xml-markup-body obj)))))
	 (else
	  '()))))

;*---------------------------------------------------------------------*/
;*    dom-get-attribute ...                                            */
;*---------------------------------------------------------------------*/
(define (dom-get-attribute node name)
   (when (xml-markup? node)
      (with-access::xml-markup node (attributes)
	 (let ((a (plist-assq (string->keyword name) attributes)))
	    (when a (cadr a))))))

;*---------------------------------------------------------------------*/
;*    dom-has-attribute? ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-has-attribute? node name)
   (when (xml-markup? node)
      (with-access::xml-markup node (attributes)
	 (pair? (plist-assq (string->keyword name) attributes)))))

;*---------------------------------------------------------------------*/
;*    dom-remove-attribute! ...                                        */
;*---------------------------------------------------------------------*/
(define (dom-remove-attribute! node name)
   (when (xml-markup? node)
      (with-access::xml-markup node (attributes)
	 (set! attributes (plist-remq! (string->keyword name) attributes))
	 attributes)))

;*---------------------------------------------------------------------*/
;*    dom-set-attribute!...                                            */
;*---------------------------------------------------------------------*/
(define (dom-set-attribute! node name value)
   (when (xml-markup? node)
      (with-access::xml-markup node (attributes)
	 (let ((a (plist-assq (string->keyword name) attributes)))
	    (when a
	       (set-car! (cdr a) value))))))

;*---------------------------------------------------------------------*/
;*    dom-node-element? ...                                            */
;*---------------------------------------------------------------------*/
(define (dom-node-element? node)
   (xml-element? node))

;*---------------------------------------------------------------------*/
;*    dom-node-text? ...                                               */
;*---------------------------------------------------------------------*/
(define (dom-node-text? node)
   (string? node))

;*---------------------------------------------------------------------*/
;*    dom-node-document? ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-node-document? node)
   (xml-markup-is? node 'document))

;*---------------------------------------------------------------------*/
;*    dom-node-document-fragment? ...                                  */
;*---------------------------------------------------------------------*/
(define (dom-node-document-fragment? node)
   (dom-node-document? node))

;*---------------------------------------------------------------------*/
;*    dom-node-attr? ...                                               */
;*---------------------------------------------------------------------*/
(define (dom-node-attr? node)
   #f)

;*---------------------------------------------------------------------*/
;*    dom-inner-html-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (dom-inner-html-set! node::xml-markup body)
   (let ((body (if (pair? body) body (list body))))
      ;; set the new body
      (xml-markup-body-set! node body)
      ;; update the id hashtable
      (let ((doc (dom-owner-document node)))
	 (when (xml-document? doc)
	    (doc-update-idtable! doc body)))
      node))

;*---------------------------------------------------------------------*/
;*    innerHTML-set! ...                                               */
;*---------------------------------------------------------------------*/
(define (innerHTML-set! node body)
   (dom-inner-html-set! node body))
   
