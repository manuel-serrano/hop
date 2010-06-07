;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/cssmatch.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 19 14:53:16 2010                          */
;*    Last change :  Sun May 30 17:58:50 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Parsing and dealing with CSS.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_css-match
   
   (library web)

   (import  __hop_xml
	    __hop_dom
	    __hop_css)

   (use	    __hop_user
	    __hop_hop
	    __hop_cgi
	    __hop_misc
	    __hop_service
	    __hop_mime
	    __hop_types)

   (export (class css-style
	      (attributes::pair-nil (default '()))
	      (after (default #f))
	      (before (default #f)))

	   (css-style-has-attribute?::bool ::css-style ::symbol)
	   (css-style-get-attribute ::css-style ::symbol)
	   (css-style-set-attribute! ::css-style ::symbol ::obj)
	   
	   (node-computed-style ::xml-element ::obj css)
	   (generic css-get-computed-style css ::xml-element)
	   (generic css-find-matching-rules::pair-nil css ::xml-element)))

;*---------------------------------------------------------------------*/
;*    css-mutex ...                                                    */
;*---------------------------------------------------------------------*/
(define css-mutex (make-mutex))

;*---------------------------------------------------------------------*/
;*    css-style-has-attribute? ...                                     */
;*---------------------------------------------------------------------*/
(define (css-style-has-attribute? cs::css-style a::symbol)
   (mutex-lock! css-mutex)
   (let ((v (pair? (assq a (css-style-attributes cs)))))
      (mutex-unlock! css-mutex)
      v))

;*---------------------------------------------------------------------*/
;*    css-style-get-attribute ...                                      */
;*---------------------------------------------------------------------*/
(define (css-style-get-attribute cs::css-style a::symbol)
   (mutex-lock! css-mutex)
   (let ((p (assq a (css-style-attributes cs))))
      (mutex-unlock! css-mutex)
      (when (pair? p)
	 (cdr p))))

;*---------------------------------------------------------------------*/
;*    css-style-set-attribute! ...                                     */
;*---------------------------------------------------------------------*/
(define (css-style-set-attribute! cs::css-style a::symbol val)
   (with-lock css-mutex
      (lambda ()
	 (let ((p (assq a (css-style-attributes cs))))
	    (if (pair? p)
		(set-cdr! p val)
		(with-access::css-style cs (attributes)
		   (set! attributes (cons (cons a val) attributes))))))))

;*---------------------------------------------------------------------*/
;*    node-computed-style ...                                          */
;*---------------------------------------------------------------------*/
(define (node-computed-style el::xml-element a css)
   (let ((a (cond
	       ((string? a) a)
	       ((keyword? a) (keyword->string a))
	       ((symbol? a) (symbol->string a))
	       (else (error 'node-computed-style "Illegal attribute" a)))))
      (if (dom-has-attribute? el a)
	  (dom-get-attribute el a)
	  (let* ((a (cond
		       ((symbol? a) a)
		       ((string? a) (string->symbol a))
		       ((keyword? a) (keyword->symbol a))
		       (else (error 'node-computed-style "Illegal attribute" a))))
		 (style (css-get-computed-style css el)))
	     (when style
		(let ((v (assq a (css-style-attributes style))))
		   (when (pair? v)
		      (cdr v))))))))

;*---------------------------------------------------------------------*/
;*    css-get-computed-style ::obj ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (css-get-computed-style css::obj el::xml-element)
   (let ((rules (css-find-matching-rules (cons el css) el)))
      (when (pair? rules)
	 (let ((style (instantiate::css-style)))
	    (for-each (lambda (rule) (css-set-style! style rule))
		      (css-sort-rules rules))
	    #;(when (and (eq? (xml-element-tag el) 'div)
		       (equal? (dom-get-attribute el "class") "GLOP"))
	       (with-output-to-port (current-error-port)
		  (lambda ()
		     (print "----------------------------------------")
		     (print "el=" el)
		     (for-each (lambda (r)
				  (display "rule: ")
				  (write r)
				  (newline))
			       rules)
		     (newline)
		     (print "style=" style)
		     (newline))))
	    style))))

;*---------------------------------------------------------------------*/
;*    css-sort-rules ...                                               */
;*---------------------------------------------------------------------*/
(define (css-sort-rules rules)
   
   (define (css-ruleset< rs1 rs2)
      (with-access::css-ruleset rs1 ((s1 specificity) (stamp1 stamp))
	 (with-access::css-ruleset rs2 ((s2 specificity) (stamp2 stamp))
	    (or (<fx (car s1) (car s2))
		(and (=fx (car s1) (car s2))
		     (or (<fx (cadr s1) (cadr s2))
			 (and (=fx (cadr s1) (cadr s2))
			      (or (<fx (caddr s1) (caddr s2))
				  (and (=fx (caddr s1) (caddr s2))
				       (or (<fx (cadddr s1) (cadddr s2))
					   (<fx stamp1 stamp2)))))))))))
   
   (sort css-ruleset< rules))

;*---------------------------------------------------------------------*/
;*    css-set-style! ...                                               */
;*---------------------------------------------------------------------*/
(define (css-set-style! style rule)
   
   (define (plain-css-set-style! style rule)
      (with-access::css-ruleset rule (declaration*)
	 (for-each (lambda (d)
		      (with-access::css-declaration d (property expr prio)
			 (with-access::css-style style (attributes)
			    (let* ((k (string->symbol property))
				   (o (assq k attributes)))
			       (if (pair? o)
				   (set-cdr! o (car expr))
				   (set! attributes (cons (cons  k (car expr))
							  attributes)))))))
		   declaration*)))
   
   (cond
      ((css-ruleset-before? rule)
       (let ((sstyle (instantiate::css-style)))
	  (plain-css-set-style! sstyle rule)
	  (css-style-before-set! style sstyle)))
      ((css-ruleset-after? rule)
       (let ((sstyle (instantiate::css-style)))
	  (plain-css-set-style! sstyle rule)
	  (css-style-after-set! style sstyle)))
      (else
       (plain-css-set-style! style rule))))

;*---------------------------------------------------------------------*/
;*    css-ruleset-before? ...                                          */
;*---------------------------------------------------------------------*/
(define (css-ruleset-before? rule)
   (css-ruleset-pseudo? rule "before"))

;*---------------------------------------------------------------------*/
;*    css-ruleset-after? ...                                           */
;*---------------------------------------------------------------------*/
(define (css-ruleset-after? rule)
   (css-ruleset-pseudo? rule "after"))

;*---------------------------------------------------------------------*/
;*    css-ruleset-pseudo? ...                                          */
;*---------------------------------------------------------------------*/
(define (css-ruleset-pseudo? rule pseudo)
   (with-access::css-ruleset rule (selector+)
      (let ((s (car (last-pair (car selector+)))))
	 (with-access::css-selector s (attr*)
	    (any? (lambda (a)
		     (and (css-selector-pseudo? a)
			  (equal? (css-selector-pseudo-expr a) pseudo)))
		  attr*)))))

;*---------------------------------------------------------------------*/
;*    css-find-matching-rules ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (css-find-matching-rules::pair-nil css el::xml-element)
   (if (pair? css)
       (append-map (lambda (css) (css-find-matching-rules css el)) css)
       '()))

;*---------------------------------------------------------------------*/
;*    css-find-matching-rules ::xml-element ...                        */
;*---------------------------------------------------------------------*/
(define-method (css-find-matching-rules es::xml-element el::xml-element)
   (let ((style (dom-get-attribute el "style")))
      ;; if the element contains a style, create a dummy stylesheet
      ;; out of it with a rule of high specificity
      (if (and (string? style) (>fx (string-length style) 0))
	  (let* ((l (string-length style))
		 (s (if (char=? (string-ref style (-fx l 1)) #\;)
			(format "* {~a}" style)
			(format "* {~a;}" style)))
		 (p (open-input-string s)))
	     (with-handler
		(lambda (e)
		   '())
		(let ((css (css->ast p :extension hss-extension)))
		   ;; extract and patch the ruleset
		   (with-access::css-stylesheet css (rule*)
		      (with-access::css-ruleset (caar rule*) (specificity)
			 (set! specificity (list 1 0 0 0)))
		      (list (caar rule*))))))
	  '())))

;*---------------------------------------------------------------------*/
;*    css-find-matching-rules ::css-stylesheet ...                     */
;*---------------------------------------------------------------------*/
(define-method (css-find-matching-rules css::css-stylesheet el::xml-element)
   (with-access::css-stylesheet css (rule*)
      (css-find-matching-rules rule* el)))

;*---------------------------------------------------------------------*/
;*    css-media-tex? ...                                               */
;*---------------------------------------------------------------------*/
(define (css-media-tex? medium+)
   (any? (lambda (q)
	    (with-access::css-media-query q (operator type)
	       (and (string=? type "tex")
		    (or (not operator) (eq? operator 'only)))))
	 medium+))

;*---------------------------------------------------------------------*/
;*    css-find-matching-rules ...                                      */
;*---------------------------------------------------------------------*/
(define-method (css-find-matching-rules css::css-media el::xml-element)
   (with-access::css-media css (medium+ ruleset*)
      (if (css-media-tex? medium+)
	  (css-find-matching-rules ruleset* el)
	  '())))

;*---------------------------------------------------------------------*/
;*    css-find-matching-rules ::css-ruleset ...                        */
;*---------------------------------------------------------------------*/
(define-method (css-find-matching-rules css::css-ruleset el::xml-element)
   (with-access::css-ruleset css (selector+ declaration* stamp)
      (filter-map (lambda (selector)
                     (when (css-rule-match? selector el)
                        (multiple-value-bind (a b c d)
                           (css-selector-specificity selector)
                           (instantiate::css-ruleset
			      (stamp stamp)
                              (selector+ (list selector))
                              (declaration* declaration*)
                              (specificity (list a b c d))))))
                  selector+)))

;*---------------------------------------------------------------------*/
;*    css-rule-match? ...                                              */
;*---------------------------------------------------------------------*/
(define (css-rule-match? selector::pair el::xml-element)
   (let ((selectors (reverse selector)))
      (when (css-selector-match? (car selectors) el)
	 (let loop ((selectors (cdr selectors))
		    (el el))
	    (cond
	       ((null? selectors)
		#t)
	       ((null? (cdr selectors))
		(error 'css-rule-match? "bad css selector" selector))
	       ((eq? (car selectors) '+)
		;; sibling
		(let ((sibling (node-get-sibling el)))
		   (when (and sibling
			      (css-selector-match? (cadr selectors) sibling))
		      (loop (cddr selectors) (dom-parent-node el)))))
	       ((eq? (car selectors) '>)
		;; child of
		(let ((selector (cadr selectors))
		      (parent (dom-parent-node el)))
		   (cond
		      ((not (xml-element? parent))
		       #f)
		      ((css-selector-match? selector parent)
		       (loop (cddr selectors) parent))
		      (else
		       #f))))
	       ((eq? (car selectors) '| |)
		;; descendant
		(let ((selector (cadr selectors)))
		   (let liip ((parent (dom-parent-node el)))
		      (cond
			 ((not (xml-element? parent))
			  #f)
			 ((css-selector-match? selector parent)
			  (loop (cddr selectors) parent))
			 (else
			  (liip (dom-parent-node parent)))))))
	       (else
		(error 'css-rule-match? "bad css selector" selector)))))))

;*---------------------------------------------------------------------*/
;*    node-get-sibling ...                                             */
;*---------------------------------------------------------------------*/
(define (node-get-sibling el)
   (let ((parent (dom-parent-node el)))
      (when (xml-element? parent)
	 (let loop ((childs (dom-child-nodes parent)))
	    (cond
	       ((null? childs) #f)
	       ((null? (cdr childs)) #f)
	       ((not (xml-element? (car childs))) (loop (cdr childs)))
	       ((eq? (cadr childs) el) (car childs))
	       (else (loop (cdr childs))))))))

;*---------------------------------------------------------------------*/
;*    css-selector-specificity* ...                                    */
;*---------------------------------------------------------------------*/
(define (css-selector-specificity* s* a b c d)
   (if (null? s*)
       (values a b c d)
       (multiple-value-bind (a2 b2 c2 d2)
	  (css-selector-specificity (car s*))
	  (css-selector-specificity* (cdr s*)
				     (+fx a a2) (+fx b b2)
				     (+fx c c2) (+fx d d2)))))

;*---------------------------------------------------------------------*/
;*    css-selector-specificity ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (css-selector-specificity selector)
   (if (pair? selector)
       (css-selector-specificity* selector 0 0 0 0)
       (values 0 0 0 0)))

;*---------------------------------------------------------------------*/
;*    css-selector-specificity ::css-selector ...                      */
;*---------------------------------------------------------------------*/
(define-method (css-selector-specificity selector::css-selector)
   (with-access::css-selector selector (element attr*)
      (multiple-value-bind (a b c d)
	 (css-selector-specificity element)
	 (css-selector-specificity* attr* a b c d))))

;*---------------------------------------------------------------------*/
;*    css-selector-specificity ::css-selector-pseudo ...               */
;*---------------------------------------------------------------------*/
(define-method (css-selector-specificity selector::css-selector-pseudo)
   (values 0 0 0 1))

;*---------------------------------------------------------------------*/
;*    css-selector-specificity ::css-selector-name ...                 */
;*---------------------------------------------------------------------*/
(define-method (css-selector-specificity selector::css-selector-name)
   (with-access::css-selector-name selector (name)
      (if (eq? name '*)
	  (values 0 0 0 0)
	  (values 0 0 0 1))))
   
;*---------------------------------------------------------------------*/
;*    css-selector-specificity ::css-selector-hash ...                 */
;*---------------------------------------------------------------------*/
(define-method (css-selector-specificity selector::css-selector-hash)
   (values 0 1 0 0))
   
;*---------------------------------------------------------------------*/
;*    css-selector-specificity ::css-selector-class ...                */
;*---------------------------------------------------------------------*/
(define-method (css-selector-specificity selector::css-selector-class)
   (values 0 0 1 0))
   
;*---------------------------------------------------------------------*/
;*    css-selector-specificity ::css-selector-attr ...                 */
;*---------------------------------------------------------------------*/
(define-method (css-selector-specificity selector::css-selector-attr)
   (values 0 0 1 0))
   
;*---------------------------------------------------------------------*/
;*    css-selector-match? ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (css-selector-match? selector el::xml-element)
   #f)

;*---------------------------------------------------------------------*/
;*    css-selector-match? ::css-selector-pseudo ...                    */
;*---------------------------------------------------------------------*/
(define-method (css-selector-match? selector::css-selector-pseudo el)
   #t)

;*---------------------------------------------------------------------*/
;*    css-selector-match? ::css-selector ...                           */
;*---------------------------------------------------------------------*/
(define-method (css-selector-match? selector::css-selector el)
   (with-access::css-selector selector (element attr*)
      (and (or (not element) (css-selector-match? element el))
	   (every? (lambda (a) (css-selector-match? a el)) attr*))))

;*---------------------------------------------------------------------*/
;*    css-selector-match? ::css-selector-name ...                      */
;*---------------------------------------------------------------------*/
(define-method (css-selector-match? selector::css-selector-name  el)
   (with-access::css-selector-name selector (name)
      (when (string? name)
	 (string-ci=? name (symbol->string! (xml-element-tag el))))))

;*---------------------------------------------------------------------*/
;*    css-selector-match? ::css-selector-hash ...                      */
;*---------------------------------------------------------------------*/
(define-method (css-selector-match? selector::css-selector-hash el)
   (equal? (css-selector-hash-name selector) (xml-element-id el)))

;*---------------------------------------------------------------------*/
;*    css-selector-match? ::css-selector-class ...                     */
;*---------------------------------------------------------------------*/
(define-method (css-selector-match? selector::css-selector-class el)
   (let ((c (dom-get-attribute el "class")))
      (when (string? c)
	 (with-access::css-selector-class selector (name)
	    (or (string=? name c)
		(and (string-index c #\space)
		     (member name (string-split c " "))))))))

;*---------------------------------------------------------------------*/
;*    css-selector-match? ::css-selector-attr ...                      */
;*---------------------------------------------------------------------*/
(define-method (css-selector-match? selector::css-selector-attr el)
   (with-access::css-selector-attr selector (ident op arg)
      (cond
	 ((string=? op "=") (equal? arg (dom-get-attribute el ident)))
	 ((string=? op "~=") #f)
	 ((string=? op "|=") #f)
	 (else #f))))
   
   


