;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/widget/prefs.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  5 14:11:23 2011                          */
;*    Last change :  Mon Jan 24 13:53:55 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Preferences editor                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-prefs

   (library hop)

   (import  __hopwidget-slider
	    __hopwidget-spinbutton)

   (export (<PREFS> . ::obj)
	   (<PRLABEL> . ::obj)
	   (<PRSEP> . ::obj)
	   (<PR> . ::obj)))

;*---------------------------------------------------------------------*/
;*    make-class-name ...                                              */
;*---------------------------------------------------------------------*/
(define (make-class-name::bstring default::bstring name)
   (if (string? name)
       (string-append default " " name)
       default))

;*---------------------------------------------------------------------*/
;*    default-value ...                                                */
;*---------------------------------------------------------------------*/
(define (default-value type)
   (case type
      ((string) "")
      ((symbol) '||)
      ((number integer) 0)
      ((real) 0.0)
      (else
       (match-case type
	  ((list . ?-) '())
	  ((alist . ?-) '())
	  (else #f)))))

;*---------------------------------------------------------------------*/
;*    value->string ...                                                */
;*    -------------------------------------------------------------    */
;*    This function has to check the values type because ad manually   */
;*    edited preference file may have corrupted types.                 */
;*---------------------------------------------------------------------*/
(define (value->string type val)
   (let loop ((type type)
	      (val val)
	      (kwote #f))
      (case type
	 ((string path)
	  (if kwote
	      (cond
		 ((eq? val #unspecified)
		  "\"\"")
		 ((string? val)
		  (string-append "\"" (string-for-read val) "\"") )
		 (else
		  (bigloo-type-error 'preferences "string" val)))
	      (cond
		 ((eq? val #unspecified)
		  "")
		 ((string? val)
		  val)
		 (else
		  (bigloo-type-error 'preferences "string" val)))))
	 ((symbol)
	  (cond
	     ((symbol? val)
	      val)
	     ((eq? val #unspecified)
	      '||)
	     (else
	      (bigloo-type-error 'preferences "symbol" val))))
	 ((expr quote)
	  (let ((s (open-output-string)))
	     (write val s)
	     (close-output-port s)))
	 ((number integer)
	  (cond
	     ((eq? val #unspecified)
	      0)
	     ((number? val)
	      (number->string val))
	     (else
	      (bigloo-type-error 'preferences "number" val))))
	 ((bool)
	  (if val "true" "false"))
	 (else
	  (match-case type
	     ((cons ?ta ?td)
	      (format "(~a . ~a)"
		      (loop ta (car val) #t)
		      (loop td (cdr val) #t)))
	     ((pair ?ta ?td)
	      (format "(~a ~a)"
		      (loop ta (car val) #t)
		      (loop td (cadr val) #t)))
	     ((list ?t)
	      (if (eq? val #unspecified)
		  ""
		  (let ((s (open-output-string)))
		     (for-each (lambda (v)
				  (display (loop t v #t) s)
				  (display " " s))
			       val)
		     (close-output-port s))))
	     ((enum  ?a . ?-)
	      (cond
		 ((eq? val #unspecified)
		  (symbol->string a))
		 ((symbol? val)
		  (symbol->string val))
		 (else
		  (bigloo-type-error 'preferences "enum" val))))
	     ((text ?- . ?-)
	      val)
	     (else
	      (error "value->string"
		     "Illegal type (see the documentation)"
		     type)))))))
		 
;*---------------------------------------------------------------------*/
;*    <PREFS> ...                                                      */
;*    -------------------------------------------------------------    */
;*    See __hop_css for HSS types.                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <PREFS> ((id #unspecified string)
			      (class #unspecified string)
			      (attrs)
			      body)
   (instantiate::xml-element
      (tag 'table)
      (id (xml-make-id id 'PREFS))
      (attributes (cons* :class (make-class-name "hop-prefs" class) attrs))
      (body (cons (<COLGROUP> (<COL>) (<COL>) (<COL> :width "0*")) body))))

;*---------------------------------------------------------------------*/
;*    <PRLABEL> ...                                                    */
;*    -------------------------------------------------------------    */
;*    See __hop_css for HSS types.                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <PRLABEL> ((id #unspecified string)
				(class #unspecified string)
				(attrs)
				body)
   (<TR>
      (<TD> :class (make-class-name "hop-prefs-label" class)
	 :id (xml-make-id id 'PRLABEL)
	 :colspan 3 body)))

;*---------------------------------------------------------------------*/
;*    <PRSEP> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <PRSEP> ((id #unspecified string)
			      (class #unspecified string)
			      (attrs)
			      body)
   (<TD> :class (make-class-name "hop-prefs-separator" class)
      :id (xml-make-id id 'PRSEP)
      :colspan 3 (<PRE> " ")))

;*---------------------------------------------------------------------*/
;*    <PR> ...                                                         */
;*    -------------------------------------------------------------    */
;*    See __hop_css for HSS types.                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <PR> ((id #unspecified string)
			   (pref #unspecified symbol)
			   (default #unspecified)
			   (param #unspecified pair)
			   (type 'string)
			   (class #unspecified string)
			   (validate #unspecified procedure)
			   (parse #f)
			   (title #unspecified string)
			   (attrs)
			   body)
   (when (procedure? validate) (preferences-add-validator! pref validate))
   (let ((cla (make-class-name "hop-pr" class))
	 (id (xml-make-id id 'PR))
	 (name (when (pair? body) (car body)))
	 (extra (when (pair? body) (cdr body))))
      (<TR> :id id :class cla 
	 (<TD> :class "hop-pr-name" name)
	 (<TD> :class "hop-pr-editor"
	    :colspan (if (pair? extra) 1 2)
	    (cond
	       ((symbol? pref)
		(pr-pref pref type default title parse))
	       ((pair? param)
		(match-case param
		   (((? symbol?)
		     (? (lambda (p) (and (procedure? p) (correct-arity? p 0))))
		     (? (lambda (p) (and (procedure? p) (correct-arity? p 1)))))
		    (pr-param param type title parse))
		   (else
		    (error "<PR>" "Illegal :param attribute" param))))
	       (else
		(error "<PR>" "Either `pref' or `param' must be provided" id))))
	 (when (pair? extra)
	    (<TD> :class "hop-pr-editor-extra" extra)))))

;*---------------------------------------------------------------------*/
;*    pr-pref ...                                                      */
;*---------------------------------------------------------------------*/
(define (pr-pref pref type default title parse)
   (let ((val (preference-get pref :default default)))
      ((pr-editor type) pref type val title parse "pref")))

;*---------------------------------------------------------------------*/
;*    pr-param ...                                                     */
;*---------------------------------------------------------------------*/
(define (pr-param param type title parse)
   (let ((val ((cadr param)))
	 (key (gensym (car param))))
      (preferences-add-pr-param! (symbol->string key) (caddr param))
      ((pr-editor type) (car param) type val title parse key)))
   
;*---------------------------------------------------------------------*/
;*    pr-editor ...                                                    */
;*---------------------------------------------------------------------*/
(define (pr-editor type)
   (match-case type
      (bool
       pr-editor-bool)
      ((bool ?yes ?no)
       pr-editor-bool)
      ((enum . ?-)
       pr-editor-enum)
      ((text . ?rest)
       (match-case rest
	  (()
	   (lambda (name type value title parse key)
	      (pr-editor-text name type value title parse key 10 80)))
	  ((?rows)
	   (lambda (name type value title parse key)
	      (pr-editor-text name type value title parse key rows 80)))
	  ((?rows ?cols)
	   (lambda (name type value title parse key)
	      (pr-editor-text name type value title parse key rows cols)))
	  (else
	   (error "pr-editor" "Illegal syntax" type))))
      ((slider . ?-)
       pr-editor-slider)
      ((spinbutton . ?-)
       pr-editor-spinbutton)
      ((list . ?-)
       pr-editor-list)
      ((alist . ?-)
       pr-editor-alist)
      (else
       pr-editor-input)))

;*---------------------------------------------------------------------*/
;*    pr-editor-input ...                                              */
;*---------------------------------------------------------------------*/
(define (pr-editor-input name type value title parse key)
   (tprint "pr-editor-input type=" type)
   (<INPUT> :class "hop-pr-editor-expr hop-pr-saved"
      :type "text"
      :value (value->string type value)
      :title (format "~a (hit [return] to validate)"
		     (if (string? title) title name))
      :onkeyup (secure-javascript-attr
		(format "hop_prefs_editor_expr( event, this, \"~a\", ~a, \"~a\", \"~a\" )"
			name (hop->js-callback parse) type key))))

;*---------------------------------------------------------------------*/
;*    pr-editor-text ...                                               */
;*---------------------------------------------------------------------*/
(define (pr-editor-text name type value title parse key rows cols)
   (<TEXTAREA> :class "hop-pr-editor-text hop-pr-saved"
      :rows rows :cols cols
      :title (format "~a (hit [return] to validate)"
		     (if (string? title) title name))
      :onkeyup (secure-javascript-attr
		(format "hop_prefs_editor_expr( event, this, \"~a\", ~a, \"~a\", \"~a\" )"
			name (hop->js-callback parse) type key))
      (value->string type value)))

;*---------------------------------------------------------------------*/
;*    pr-editor-bool ...                                               */
;*---------------------------------------------------------------------*/
(define (pr-editor-bool name type value title parse key)
   (multiple-value-bind (yes-string no-string)
      (match-case type
	 (bool
	  (values "on" "off"))
	 ((bool (and (? string?) ?y) (and (? string?) ?n))
	  (values y n))
	 ((bool (and (? symbol?) ?y) (and (? symbol?) ?n))
	  (values (symbol->string y) (symbol->string n))))
      (let ((name (symbol->string name)))
	 (<TABLE> :class "hop-pr-editor-bool"
	    (<COLGROUP>
	       (<COL> :span 2 :width "1*"))
	    (<TR>
	       (<TD>
		  (<INPUT>
		     :type "radio"
		     :checked value
		     :name name
		     :onclick (secure-javascript-attr
			       (format "hop_prefs_editor_bool( event, \"true\", \"~a\", ~a, \"~a\", \"~a\" )"
				       name (hop->js-callback parse) 'bool key)))
		  yes-string)
	       (<TD> 
		  (<INPUT>
		     :type "radio"
		     :checked (not value)
		     :name name
		     :onclick (secure-javascript-attr
			       (format "hop_prefs_editor_bool( event, \"false\", \"~a\", ~a, \"~a\", \"~a\" )"
				      name (hop->js-callback parse) 'bool key)))
		  no-string))))))

;*---------------------------------------------------------------------*/
;*    pr-editor-enum ...                                               */
;*---------------------------------------------------------------------*/
(define (pr-editor-enum name type value title parse key)
   (let* ((name (symbol->string name))
	  (enum (cdr type))
	  (len (length enum)))
      (<TABLE> :class "hop-pr-editor-enum"
	 (map (lambda (s)
		 (<TR>
		    (<TD>
		       (<INPUT>
			  :type "radio"
			  :checked (eq? s value)
			  :name name
			  :onclick (secure-javascript-attr
				    (format "hop_prefs_editor_bool( event, \"~a\", \"~a\", ~a, \"~a\", \"~a\" )"
					    s name (hop->js-callback parse) 'enum key)))
		       (symbol->string s))))
	      enum))))

;*---------------------------------------------------------------------*/
;*    pr-editor-slider ...                                             */
;*---------------------------------------------------------------------*/
(define (pr-editor-slider name type value title parse key)
   (match-case type
      ((?- ?min ?max ?step)
       (let ((name (symbol->string name)))
	  (<SLIDER> :class "hop-pr-editor-slider"
	     :min min
	     :max max
	     :step step
	     :value value
	     :onchange (secure-javascript-attr
			(format "hop_prefs_editor_number( event, this.value.toString(), \"~a\", ~a, \"~a\", \"~a\" )"
				name (hop->js-callback parse) 'real key)))))
      (else
       (error "<PR>" "Wrong \"slider\" declaration" type))))

;*---------------------------------------------------------------------*/
;*    pr-editor-spinbutton ...                                         */
;*---------------------------------------------------------------------*/
(define (pr-editor-spinbutton name type value title parse key)
   (match-case type
      ((?- ?min ?max)
       (let ((name (symbol->string name)))
	  (<SPINBUTTON> :class "hop-pr-editor-spinbutton"
	     :min min
	     :max max
	     :value value
	     :onchange (secure-javascript-attr
			(format "hop_prefs_editor_number( event, this.value.toString(), \"~a\", ~a, \"~a\", \"~a\" )"
				name (hop->js-callback parse) 'integer key)))))
      (else
       (error "<PR>" "Wrong \"spinbutton\" declaration" type))))

;*---------------------------------------------------------------------*/
;*    pr-editor-list ...                                               */
;*---------------------------------------------------------------------*/
(define (pr-editor-list name type value title parse key)
   (match-case type
      ((?- ?type . ?def)
       (tprint "pr-editor-list type=" type " " (map typeof value))
       (<DIV> :class "hop-pr-editor-list"
	  (<TABLE> 
	     (<TR> :class "hop-pr-editor-list-add"
		(<TD>
		   ((pr-editor type) (list 'add name) type (if (pair? def) (car def) (default-value type)) title parse key))
		(<TD> :class "hop-pr-editor-list-button"))
	     (map (lambda (v i)
		     (let ((edit ((pr-editor type) (list 'set i name) type v title parse key)))
			(<TR>
			   (<TD> edit)
			   (<TD> :class "hop-pr-editor-list-button"
			      (<BUTTON> :onclick
				 (secure-javascript-attr
				  (format "node_style_set( this.parentNode.parentNode, 'opacity', '0.2' ); hop_prefs_editor_list_item( event, \"\", \"(del ~a ~a)\", false, \"~a\", \"~a\" )"
					  i name
					  type key))
				 "Delete")))))
		  value
		  (iota (length value))))))
      (else
       (error "<PR>" "Wrong \"list\" declaration" type))))

;*---------------------------------------------------------------------*/
;*    pr-editor-alist ...                                              */
;*---------------------------------------------------------------------*/
(define (pr-editor-alist name type value title parse key)
   (match-case type
      ((?- ?typek ?typev . ?def)
       (<DIV> :class "hop-pr-editor-alist"
	  (<TABLE>
	     (<TR> :class "hop-pr-editor-list-add"
		(<TH>
		   ((pr-editor typek) (list 'aaddk name) typek (if (pair? def) (car def) (default-value type)) title parse key))
		(<TD>
		   ((pr-editor typev) (list 'aaddv name) typev (if (and (pair? def) (pair? (cdr def))) (cadr def) (default-value type)) title parse key))
		(<TD> :class "hop-pr-editor-list-button"))
	     (map (lambda (v i)
		     (let ((editk ((pr-editor typek) (list 'asetk i name) typek (car v) title parse key))
			   (editv ((pr-editor typev) (list 'asetv i name) typev (cadr v) title parse key)))
			(<TR>
			   (<TH> editk)
			   (<TD> editv)
			   (<TD> :class "hop-pr-editor-list-button"
			      (<BUTTON> :onclick
				    (secure-javascript-attr
				     (format "node_style_set( this.parentNode.parentNode, 'opacity', '0.2' ); hop_prefs_editor_list_item( event, \"\", \"(del ~a ~a)\", false, \"~a\", \"~a\" )"
					     i name
					     typev key))
				    "Delete")))))
		  value
		  (iota (length value))))))
      (else
       (error "<PR>" "Wrong \"alist\" declaration" type))))
