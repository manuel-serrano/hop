;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/dom.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 23 16:55:15 2005                          */
;*    Last change :  Thu Sep 24 17:20:48 2015 (serrano)                */
;*    Copyright   :  2005-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Restricted DOM implementation                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_dom

   (import __hop_xml-types
	   __hop_xml
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
	   (dom-add-class! node ::bstring)
	   (dom-remove-class! node ::bstring)
	   (dom-has-attributes? node)
	   (dom-has-child-nodes? node)
	   (dom-normalize! node)
	   (generic dom-get-element-by-id obj ::bstring)
	   (dom-get-elements-by-tag-name::pair-nil obj ::bstring)
	   (dom-get-elements-by-hss-tag-name::pair-nil obj ::bstring)
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
	 (when (isa? obj xml-element)
	    (with-access::xml-element obj (id)
	       (hashtable-put! %idtable id obj)))
	 (with-access::xml-markup obj (body)
	    (update-body! body)))

      (define (update-body! body)
	 (cond
	    ((isa? body xml-markup)
	     (update-xml-markup! body))
	    ((pair? body)
	     (for-each update-body! body))))

      (update-body! body)))

;*---------------------------------------------------------------------*/
;*    %xml-constructor ::xml-document ...                              */
;*---------------------------------------------------------------------*/
;* (define-method (%xml-constructor doc::xml-document)                 */
;*    (with-access::xml-document doc (body %idtable)                   */
;*       (doc-update-idtable! doc body)                                */
;*       doc))                                                         */

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
   (with-access::xml-delay obj (id)
      (when (string=? id id)
	 obj)))

;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id ::xml-markup ...                           */
;*---------------------------------------------------------------------*/
(define-method (dom-get-element-by-id obj::xml-markup id)
   (with-access::xml-markup obj (body)
      (dom-get-element-by-id* body id)))

;*---------------------------------------------------------------------*/
;*    dom-get-element-by-id ::xml-element ...                          */
;*---------------------------------------------------------------------*/
(define-method (dom-get-element-by-id obj::xml-element id)
   (with-access::xml-element obj ((oid id) body)
      (if (and (string? oid) (string=? id oid))
	  obj
	  (dom-get-element-by-id* body id))))

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
   (when (isa? obj xml-element)
      (let loop ((parent (with-access::xml-element obj (parent) parent)))
	 (cond
	    ((isa? parent xml-document)
	     parent)
	    ((isa? parent xml-element)
	     (with-access::xml-element parent (parent)
		(loop parent)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    dom-get-attributes ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-get-attributes node)
   (if (isa? node xml-markup)
       (with-access::xml-markup node (attributes)
	  attributes)
       '()))

;*---------------------------------------------------------------------*/
;*    dom-child-nodes ...                                              */
;*---------------------------------------------------------------------*/
(define (dom-child-nodes node)
   (if (isa? node xml-markup)
       (with-access::xml-markup node (body)
	  body)
       '()))

;*---------------------------------------------------------------------*/
;*    dom-first-child ...                                              */
;*---------------------------------------------------------------------*/
(define (dom-first-child node)
   (and (isa? node xml-markup)
	(with-access::xml-markup node (body)
	   (and (pair? body) (car body)))))

;*---------------------------------------------------------------------*/
;*    dom-last-child ...                                               */
;*---------------------------------------------------------------------*/
(define (dom-last-child node)
   (and (isa? node xml-markup)
	(with-access::xml-markup node (body)
	   (and (pair? body) (car (last-pair body))))))

;*---------------------------------------------------------------------*/
;*    dom-node-name ...                                                */
;*---------------------------------------------------------------------*/
(define (dom-node-name node)
   (cond
      ((string? node)
       "#text")
      ((isa? node xml-markup)
       (with-access::xml-markup node (tag)
	  (symbol->string tag)))
      ((isa? node xml-document)
       "#document")
      (else
       "")))

;*---------------------------------------------------------------------*/
;*    dom-node-type ...                                                */
;*---------------------------------------------------------------------*/
(define (dom-node-type node)
   (cond
      ((or (string? node) (isa? node xml-verbatim)) 'text)
      ((isa? node xml-markup) (with-access::xml-markup node (tag) tag))
      ((isa? node xml-element) 'element)
      ((isa? node xml-document) 'document)
      ((isa? node xml-comment) 'comment)
      (else 'unknown)))

;*---------------------------------------------------------------------*/
;*    dom-node-value ...                                               */
;*---------------------------------------------------------------------*/
(define (dom-node-value node)
   (cond
      ((string? node) node)
      ((isa? node xml-markup) #f)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    dom-parent-node ...                                              */
;*---------------------------------------------------------------------*/
(define (dom-parent-node node)
   (when (isa? node xml-element)
      (with-access::xml-element node (parent)
	 parent)))

;*---------------------------------------------------------------------*/
;*    dom-set-parent-node! ...                                         */
;*---------------------------------------------------------------------*/
(define (dom-set-parent-node! node parent)
   (cond
      ((isa? node xml-element)
       (with-access::xml-element node ((xparent parent))
	  (set! xparent parent)))
      ((isa? node xml-tilde)
       (with-access::xml-tilde node ((xparent parent))
	  (set! xparent parent)))))

;*---------------------------------------------------------------------*/
;*    dom-ancestor-node ...                                            */
;*    -------------------------------------------------------------    */
;*    Transitive closure on parent node until no parent found.         */
;*---------------------------------------------------------------------*/
(define (dom-ancestor-node obj)
   (when (isa? obj xml-element)
      (with-access::xml-element obj (parent)
	 (when (isa? parent xml-element)
	    (with-access::xml-element parent ((nparent parent))
	       (let loop ((nparent nparent)
			  (parent parent))
		  (cond
		     ((not nparent)
		      parent)
		     ((isa? nparent xml-document)
		      nparent)
		     (else
		      (with-access::xml-element nparent (parent)
			 (loop parent nparent))))))))))

;*---------------------------------------------------------------------*/
;*    eq-verbatim? ...                                                 */
;*---------------------------------------------------------------------*/
(define (eq-verbatim? left right)
   (cond
      ((eq? left right)
       #t)
      ((isa? right xml-verbatim)
       (with-access::xml-verbatim right (data) (eq? data left)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    memq-verbatim ...                                                */
;*---------------------------------------------------------------------*/
(define (memq-verbatim n lst)
   (cond
      ((null? lst) #f)
      ((eq-verbatim? n (car lst)) #t)
      (else (memq-verbatim n (cdr lst)))))

;*---------------------------------------------------------------------*/
;*    remq-verbatim! ...                                               */
;*---------------------------------------------------------------------*/
(define (remq-verbatim! x y)
   (cond
      ((null? y)
       y)
      ((eq? x (car y))
       (remq-verbatim! x (cdr y)))
      (else
       (let loop ((prev y))
	  (cond ((null? (cdr prev))
		 y)
		((eq-verbatim? (cadr prev) x)
		 (set-cdr! prev (cddr prev))
		 (loop prev))
		(else (loop (cdr prev))))))))

;*---------------------------------------------------------------------*/
;*    dom-previous-sibling ...                                         */
;*---------------------------------------------------------------------*/
(define (dom-previous-sibling node)
   (and (isa? node xml-element)
	(with-access::xml-element node (parent)
	   (and (isa? parent xml-markup)
		(with-access::xml-markup parent (body)
		   (let loop ((body body)
			      (prev #f))
		      (cond
			 ((null? body)
			  prev)
			 ((eq-verbatim? (car body) node)
			  prev)
			 (else
			  (loop (cdr body) (car body))))))))))

;*---------------------------------------------------------------------*/
;*    dom-next-sibling ...                                             */
;*---------------------------------------------------------------------*/
(define (dom-next-sibling node)
   (and (isa? node xml-element)
	(with-access::xml-element node (parent)
	   (and (isa? parent xml-markup)
		(with-access::xml-markup parent (body)
		   (let ((child (memq-verbatim node body)))
		      (and (pair? child)
			   (pair? (cdr child))
			   (cadr child))))))))

;*---------------------------------------------------------------------*/
;*    dom-remove-child! ...                                            */
;*---------------------------------------------------------------------*/
(define (dom-remove-child! node old)
   (when (isa? node xml-markup)
      (let ((doc (dom-owner-document node)))
	 (when (isa? doc xml-document)
	    (with-access::xml-document doc (%idtable)
	       (with-access::xml-element old (id)
		  (hashtable-remove! %idtable id)))))
      (with-access::xml-markup node (body)
	 (set! body (remq-verbatim! old body))
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
	     (when (isa? parent xml-markup)
		(with-access::xml-markup parent (body)
		   (set! body (remq-verbatim! new body)))))
	  (let ((doc (dom-owner-document node)))
	     (when (isa? doc xml-document)
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
   (when (isa? node xml-element)
      (let ((ancp (dom-ancestor-node node))
	    (ancn (dom-ancestor-node new))
	    (anco (dom-ancestor-node old))
	    (docp (dom-owner-document node)))
	 ;; re-parent the new node
	 (if (and ancp (eq? ancp ancn))
	     (let ((parent (dom-parent-node new)))
		(when (isa? parent xml-markup)
		   (with-access::xml-markup parent (body)
		      (set! body (remq-verbatim! new body)))))
	     (when (isa? docp xml-document)
		(with-access::xml-document docp (%idtable)
		   (doc-update-idtable! docp new))))
	 ;; unparent the old node
	 (dom-set-parent-node! old #f)
	 (let ((doco (dom-owner-document old)))
	    (when (and docp (eq? docp doco))
	       (with-access::xml-document doco (%idtable)
		  (with-access::xml-element old (id)
		     (hashtable-remove! %idtable id))))))
      ;; replace the elements
      (dom-set-parent-node! new node)
      (with-access::xml-markup node (body)
	 (let loop ((body body))
	    (cond
	       ((null? body)
		(error "dom-replace-child!" "not a child" old))
	       ((eq? (car body) old)
		(set-car! body new))
	       (else
		(loop (cdr body))))))))

;*---------------------------------------------------------------------*/
;*    dom-insert-before! ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-insert-before! node new ref)
   (when (isa? node xml-markup)
      (with-access::xml-markup node (body)
	 (cond
	    ((not ref)
	     (dom-append-child! node new))
	    ((null? body)
	     (error "dom-insert-before!" "ref not a parent child" ref))
	    (else
	     (let ((ancp (dom-ancestor-node node))
		   (ancn (dom-ancestor-node new)))
		;; re-parent new
		(if (and ancp (eq? ancp ancn))
		    (let ((parent (dom-parent-node new)))
		       (when (isa? parent xml-markup)
			  (with-access::xml-markup parent (body)
			     (set! body (remq-verbatim! new body)))))
		    (let ((doc (dom-owner-document node)))
		       (when (isa? doc xml-document)
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
       (with-access::xml-if node (test then otherwise)
	  (duplicate::xml-if node 
	     (test (dom-clone-node test deep))
	     (then (dom-clone-node then deep))
	     (otherwise (dom-clone-node otherwise deep))))))

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
       (with-access::xml-markup node (attributes body)
	  (duplicate::xml-markup node
	     (attributes (dom-clone-node attributes deep))
	     (body (dom-clone-node body deep))))))

;*---------------------------------------------------------------------*/
;*    dom-clone-node ::xml-element ...                                 */
;*---------------------------------------------------------------------*/
(define-method (dom-clone-node node::xml-element deep::bool)
   (if (not deep)
       (duplicate::xml-element node)
       (with-access::xml-element node (attributes body)
	  (duplicate::xml-element node
	     (attributes (dom-clone-node attributes deep))
	     (body (dom-clone-node body deep))))))

;*---------------------------------------------------------------------*/
;*    dom-clone-node ::xml-html ...                                    */
;*---------------------------------------------------------------------*/
(define-method (dom-clone-node node::xml-html deep::bool)
   (if (not deep)
       (duplicate::xml-html node)
       (with-access::xml-html node (attributes body)
	  (duplicate::xml-html node
	     (attributes (dom-clone-node attributes deep))
	     (body (dom-clone-node body deep))))))

;*---------------------------------------------------------------------*/
;*    dom-add-class! ...                                               */
;*---------------------------------------------------------------------*/
(define (dom-add-class! node name)
   (when (isa? node xml-markup)
      (let ((cname (dom-get-attribute node "class")))
	 (if (not (string? cname))
	     (dom-set-attribute! node "class" name)
	     (let ((regexp (string-append name "\\b")))
		(unless (pregexp-match regexp cname)
		   (dom-set-attribute! node "class"
		      (string-append cname " " name))))))))

;*---------------------------------------------------------------------*/
;*    dom-remove-class! ...                                            */
;*---------------------------------------------------------------------*/
(define (dom-remove-class! node name)
   (when (isa? node xml-markup)
      (let ((cname (dom-get-attribute node "class"))
	    (regexp (string-append "[ \\t]*" name "\\b")))
	 (dom-set-attribute! node "class" (pregexp-replace regexp cname "")))))

;*---------------------------------------------------------------------*/
;*    dom-has-attributes? ...                                          */
;*---------------------------------------------------------------------*/
(define (dom-has-attributes? node)
   (when (isa? node xml-markup)
      (with-access::xml-markup node (attributes)
	 (pair? attributes))))

;*---------------------------------------------------------------------*/
;*    dom-has-child-nodes? ...                                         */
;*---------------------------------------------------------------------*/
(define (dom-has-child-nodes? node)
   (when (isa? node xml-markup)
      (with-access::xml-markup node (body)
	 (pair? body))))

;*---------------------------------------------------------------------*/
;*    dom-normalize! ...                                               */
;*---------------------------------------------------------------------*/
(define (dom-normalize! node)
   (when (isa? node xml-markup)
      (let loop ((body (with-access::xml-markup node (body) body))
		 (new '()))
	 (cond
	    ((null? body)
	     (set! body (reverse! new)))
	    ((not (string? (car body)))
	     (loop (cdr body) (cons (dom-normalize! (car body)) new)))
	    ((string=? (car body) "")
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
	    ((isa? obj xml-markup)
	     (with-access::xml-markup obj (body tag)
		(if (eq? sym tag)
		    (cons obj (loop body))
		    (loop body))))
	    (else
	     '())))))

;*---------------------------------------------------------------------*/
;*    dom-get-elements-by-hss-tag-name ...                             */
;*---------------------------------------------------------------------*/
(define (dom-get-elements-by-hss-tag-name::pair-nil obj name)
   (let loop ((obj obj))
      (cond
	 ((pair? obj)
	  (append-map loop obj))
	 ((isa? obj xml-markup)
	  (with-access::xml-markup obj (body)
	     (let ((c (xml-primitive-value (dom-get-attribute obj "data-hss-tag"))))
		(if (and (string? c) (string=? c name))
		    (cons obj (loop body))
		    (loop body)))))
	 (else
	  '()))))

;*---------------------------------------------------------------------*/
;*    dom-get-elements-by-class ...                                    */
;*---------------------------------------------------------------------*/
(define (dom-get-elements-by-class::pair-nil obj name)

   (define (string-in? c name)
      (or (string=? c name)
	  (let ((cl (string-length c))
		(nl  (string-length name)))
	     (and (>fx cl nl)
		  (or (and (string-prefix? name c)
			   (memq (string-ref c nl)
			      '(#\space #\newline #\tab)))
		      (and (string-suffix? name c)
			   (memq (string-ref c (-fx (-fx cl nl) 1))
			      '(#\space #\newline #\tab)))
		      (and (string-contains c name)
			   (pregexp-match
			      (string-append "[ \t\n]" name "[ \t\n]") c)))))))
   
   (let loop ((obj obj))
      (cond
	 ((pair? obj)
	  (append-map loop obj))
	 ((isa? obj xml-markup)
	  (with-access::xml-markup obj (body tag)
	     (let ((c (xml-primitive-value (dom-get-attribute obj "class"))))
		(if (and (string? c) (string-in? c name))
		    (cons obj (loop body))
		    (loop body)))))
	 (else
	  '()))))

;*---------------------------------------------------------------------*/
;*    dom-get-attribute ...                                            */
;*---------------------------------------------------------------------*/
(define (dom-get-attribute node name)
   (when (isa? node xml-markup)
      (with-access::xml-markup node (attributes)
	 (let ((a (plist-assq (string->keyword name) attributes)))
	    (when a (cadr a))))))

;*---------------------------------------------------------------------*/
;*    dom-has-attribute? ...                                           */
;*---------------------------------------------------------------------*/
(define (dom-has-attribute? node name)
   (when (isa? node xml-markup)
      (with-access::xml-markup node (attributes)
	 (pair? (plist-assq (string->keyword name) attributes)))))

;*---------------------------------------------------------------------*/
;*    dom-remove-attribute! ...                                        */
;*---------------------------------------------------------------------*/
(define (dom-remove-attribute! node name)
   (when (isa? node xml-markup)
      (with-access::xml-markup node (attributes)
	 (set! attributes (plist-remq! (string->keyword name) attributes))
	 attributes)))

;*---------------------------------------------------------------------*/
;*    dom-set-attribute!...                                            */
;*---------------------------------------------------------------------*/
(define (dom-set-attribute! node name value)
   (when (isa? node xml-markup)
      (with-access::xml-markup node (attributes)
	 (let* ((key (string->keyword name))
		(a (plist-assq key attributes)))
	    (if a
		(set-car! (cdr a) value)
		(set! attributes (cons* key value attributes)))))))

;*---------------------------------------------------------------------*/
;*    dom-node-element? ...                                            */
;*---------------------------------------------------------------------*/
(define (dom-node-element? node)
   (isa? node xml-element))

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
      (with-access::xml-markup node ((xbody body))
	 (set! xbody body))
      ;; update the id hashtable
      (let ((doc (dom-owner-document node)))
	 (when (isa? doc xml-document)
	    (doc-update-idtable! doc body)))
      node))

;*---------------------------------------------------------------------*/
;*    innerHTML-set! ...                                               */
;*---------------------------------------------------------------------*/
(define (innerHTML-set! node body)
   (dom-inner-html-set! node body))
   
