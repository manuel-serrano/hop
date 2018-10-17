;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/runtime/cssmatch.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 19 14:53:16 2010                          */
;*    Last change :  Fri Apr 22 14:44:22 2016 (serrano)                */
;*    Copyright   :  2010-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Parsing and dealing with CSS.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_css-match
   
   (library web)

   (import  __hop_xml
	    __hop_xml-types
	    __hop_dom
	    __hop_css)

   (use	    __hop_user
	    __hop_hop
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
	   
	   (node-computed-style ::xml ::obj css)
	   (generic css-get-computed-style css ::obj)
	   (generic css-find-match-rulesets::pair-nil css ::obj)))

;*---------------------------------------------------------------------*/
;*    xml-write ::css-stylesheet ...                                   */
;*---------------------------------------------------------------------*/
(define-method (xml-write o::css-stylesheet op backend)
   (display "<style>\n" op)
   (css-write o op)
   (display "</style>\n" op))

;*---------------------------------------------------------------------*/
;*    css-write ::css-style ...                                        */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-style op)
   (with-access::css-style o (before after attributes)
      (let ((sep ""))
	 (display "{" op)
	 (when before
	    (display "before: " op)
	    (css-write before op)
	    (display ";" op)
	    (set! sep " "))
	 (display sep op)
	 (when after
	    (display "after: " op)
	    (css-write after op)
	    (display ";" op)
	    (set! sep " "))
	 (display sep op)
	 (let loop ((attr attributes)
		    (sep sep))
	    (if (null? attr)
		(display "}" op)
		(let ((a (car attr)))
		   (display sep op)
		   (display (car a) op)
		   (display ": " op)
		   (css-write (cdr a) op)
		   (display ";" op)
		   (loop (cdr attr) " ")))))))

;*---------------------------------------------------------------------*/
;*    css-mutex ...                                                    */
;*---------------------------------------------------------------------*/
(define css-mutex (make-mutex))

;*---------------------------------------------------------------------*/
;*    css-style-has-attribute? ...                                     */
;*---------------------------------------------------------------------*/
(define (css-style-has-attribute? cs::css-style a::symbol)
   (synchronize css-mutex
      (with-access::css-style cs (attributes)
	 (pair? (assq a attributes)))))

;*---------------------------------------------------------------------*/
;*    css-style-get-attribute ...                                      */
;*---------------------------------------------------------------------*/
(define (css-style-get-attribute cs::css-style a::symbol)
   (synchronize css-mutex
      (with-access::css-style cs (attributes)
	 (let ((p (assq a attributes)))
	    (when (pair? p)
	       (cadr p))))))

;*---------------------------------------------------------------------*/
;*    css-style-set-attribute! ...                                     */
;*---------------------------------------------------------------------*/
(define (css-style-set-attribute! cs::css-style a::symbol val)
   (synchronize css-mutex
      (with-access::css-style cs (attributes)
	 (let ((p (assq a attributes)))
	    (if (pair? p)
		(set-car! (cdr p) val)
		(set! attributes (cons (cons a (cons val #f)) attributes)))))))

;*---------------------------------------------------------------------*/
;*    node-computed-style ...                                          */
;*---------------------------------------------------------------------*/
(define (node-computed-style el::xml a css)
   (let ((a (cond
	       ((string? a) a)
	       ((keyword? a) (keyword->string a))
	       ((symbol? a) (symbol->string a))
	       (else (error "node-computed-style" "Illegal attribute" a)))))
      (if (dom-has-attribute? el a)
	  (dom-get-attribute el a)
	  (let* ((a (cond
		       ((symbol? a) a)
		       ((string? a) (string->symbol a))
		       ((keyword? a) (keyword->symbol a))
		       (else (error "node-computed-style" "Illegal attribute" a))))
		 (style (css-get-computed-style css el)))
	     (when style
		(css-style-get-attribute style a))))))

;*---------------------------------------------------------------------*/
;*    css-get-computed-style ::obj ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (css-get-computed-style css::obj el::obj)
   (let ((rulesets (css-find-match-rulesets (cons el css) el)))
      (when (pair? rulesets)
	 (let ((style (instantiate::css-style)))
	    (for-each (lambda (rule) (css-set-style! style rule))
	       (css-sort-rulesets rulesets))
	    style))))

;*---------------------------------------------------------------------*/
;*    css-get-computed-style ::css-stylesheet ...                      */
;*---------------------------------------------------------------------*/
(define-method (css-get-computed-style css::css-stylesheet el::obj)
   (with-access::css-stylesheet css (rule*)
      (css-get-computed-style rule* el)))

;*---------------------------------------------------------------------*/
;*    css-sort-rulesets ...                                            */
;*---------------------------------------------------------------------*/
(define (css-sort-rulesets rulesets)
   
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
   
   (sort css-ruleset< rulesets))

;*---------------------------------------------------------------------*/
;*    css-set-style! ...                                               */
;*---------------------------------------------------------------------*/
(define (css-set-style! style rule)

   (define (expand-attr property)
      (case property
	 ((margin)
	  '(margin-left margin-right margin-top margin-bottom))
	 ((padding)
	  '(padding-left padding-right padding-top padding-bottom))
	 (else
	  '())))

   (define (add-field! style k v p)
      (with-access::css-style style (attributes)
	 (let ((o (assq k attributes)))
	    (if (pair? o)
		(set-cdr! o (cons v p))
		(set! attributes (cons (cons k (cons v p)) attributes))))))
      
   (define (plain-css-set-style! style rule)
      (with-access::css-ruleset rule (declaration*)
	 (for-each (lambda (d)
		      (with-access::css-declaration d (property expr prio)
			 (let ((k (string->symbol property))
			       (v (car expr)))
			    (add-field! style k v prio)
			    (for-each (lambda (k)
					 (add-field! style k v prio))
			       (expand-attr k)))))
	    declaration*)))
   
   (cond
      ((css-ruleset-before? rule)
       (let ((sstyle (instantiate::css-style)))
	  (plain-css-set-style! sstyle rule)
	  (with-access::css-style style (before)
	     (set! before sstyle))))
      ((css-ruleset-after? rule)
       (let ((sstyle (instantiate::css-style)))
	  (plain-css-set-style! sstyle rule)
	  (with-access::css-style style (after)
	     (set! after sstyle))))
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
	    (any (lambda (a)
		    (and (isa? a css-selector-pseudo)
			 (with-access::css-selector-pseudo a (expr)
			    (equal? expr pseudo))))
	       attr*)))))

;*---------------------------------------------------------------------*/
;*    css-find-match-rulesets ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (css-find-match-rulesets::pair-nil css el::obj)
   (if (pair? css)
       (append-map (lambda (css) (css-find-match-rulesets css el)) css)
       '()))

;*---------------------------------------------------------------------*/
;*    css-find-match-rulesets ::xml ...                                */
;*---------------------------------------------------------------------*/
(define-method (css-find-match-rulesets es::xml el::obj)
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
		   (if (isa? e &io-parse-error)
		       '()
		       (raise e)))
		(let ((css (css->ast p :extension hss-extension)))
		   ;; extract and patch the ruleset
		   (with-access::css-stylesheet css (rule*)
		      (with-access::css-ruleset (caar rule*) (specificity)
			 (set! specificity (list 1 0 0 0)))
		      (list (caar rule*))))))
	  '())))

;*---------------------------------------------------------------------*/
;*    css-find-match-rulesets ::css-stylesheet ...                     */
;*---------------------------------------------------------------------*/
(define-method (css-find-match-rulesets css::css-stylesheet el::obj)
   (with-access::css-stylesheet css (rule*)
      (css-find-match-rulesets rule* el)))

;*---------------------------------------------------------------------*/
;*    css-media-tex? ...                                               */
;*---------------------------------------------------------------------*/
(define (css-media-tex? medium+)
   (any (lambda (q)
	   (with-access::css-media-query q (operator type)
	      (and (string=? type "tex")
		   (or (not operator) (eq? operator 'only)))))
	 medium+))

;*---------------------------------------------------------------------*/
;*    css-find-match-rulesets ...                                      */
;*---------------------------------------------------------------------*/
(define-method (css-find-match-rulesets css::css-media el::obj)
   (with-access::css-media css (medium+ ruleset*)
      (if (css-media-tex? medium+)
	  (css-find-match-rulesets ruleset* el)
	  '())))

;*---------------------------------------------------------------------*/
;*    css-find-match-rulesets ::css-ruleset ...                        */
;*---------------------------------------------------------------------*/
(define-method (css-find-match-rulesets css::css-ruleset el::obj)
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
(define (css-rule-match? selector::pair el::obj)
   (let ((selectors (reverse selector)))
      (when (css-selector-match? (car selectors) el)
	 (let loop ((selectors (cdr selectors))
		    (el el))
	    (cond
	       ((null? selectors)
		#t)
	       ((null? (cdr selectors))
		(error "css-rule-match?" "bad css selector" selector))
	       ((eq? (car selectors) '+)
		;; sibling
		(let ((sibling (dom-next-sibling el)))
		   (when (and sibling
			      (css-selector-match? (cadr selectors) sibling))
		      (loop (cddr selectors) (dom-parent-node el)))))
	       ((eq? (car selectors) '>)
		;; child of
		(let ((selector (cadr selectors))
		      (parent (dom-parent-node el)))
		   (cond
		      ((not (isa? parent xml-element))
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
			 ((not (isa? parent xml-element))
			  #f)
			 ((css-selector-match? selector parent)
			  (loop (cddr selectors) parent))
			 (else
			  (liip (dom-parent-node parent)))))))
	       (else
		(error "css-rule-match?" "bad css selector" selector)))))))

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
(define-generic (css-selector-match? selector el::obj)
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
   (when (isa? el xml-element)
      (with-access::css-selector selector (element attr*)
	 (and (or (not element) (css-selector-match? element el))
	      (every (lambda (a) (css-selector-match? a el)) attr*)))))

;*---------------------------------------------------------------------*/
;*    css-selector-match? ::css-selector-name ...                      */
;*---------------------------------------------------------------------*/
(define-method (css-selector-match? selector::css-selector-name el)
   (with-access::css-selector-name selector (name)
      (cond
	 ((eq? name '*)
	  #t)
	 ((string? name)
	  (when (isa? el xml-element)
	     (with-access::xml-element el (tag)
		(string-ci=? name (symbol->string! tag))))))))

;*---------------------------------------------------------------------*/
;*    css-selector-match? ::css-selector-hash ...                      */
;*---------------------------------------------------------------------*/
(define-method (css-selector-match? selector::css-selector-hash el)
   (when (isa? el xml-element)
      (with-access::xml-element el (id)
	 (with-access::css-selector-hash selector (name)
	    (equal? name id)))))

;*---------------------------------------------------------------------*/
;*    css-selector-match? ::css-selector-class ...                     */
;*---------------------------------------------------------------------*/
(define-method (css-selector-match? selector::css-selector-class el)
   (when (isa? el xml-element)
      (let ((c (xml-primitive-value (dom-get-attribute el "class"))))
	 (when (string? c)
	    (with-access::css-selector-class selector (name)
	       (or (string=? name c)
		   (and (string-index c #\space)
			(member name (string-split c " ")))))))))

;*---------------------------------------------------------------------*/
;*    css-selector-match? ::css-selector-attr ...                      */
;*---------------------------------------------------------------------*/
(define-method (css-selector-match? selector::css-selector-attr el)
   (with-access::css-selector-attr selector (ident op arg)
      (cond
	 ((not op) (dom-get-attribute el ident))
	 ((string=? op "=") (equal? arg (dom-get-attribute el ident)))
	 ((string=? op "~=") #f)
	 ((string=? op "|=") #f)
	 (else #f))))
   
   


