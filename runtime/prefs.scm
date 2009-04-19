;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/prefs.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 28 07:45:15 2006                          */
;*    Last change :  Sun Apr 19 07:32:32 2009 (serrano)                */
;*    Copyright   :  2006-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Preferences editor                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_prefs
   
   (include "xml.sch"
	    "service.sch")
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml
	    __hop_hop-extra
	    __hop_cgi
	    __hop_service
	    __hop_js-lib
	    __hop_read
	    __hop_hop
	    __hop_user
	    __hop_http-error)
   
   (export (<PREFS> . ::obj)
	   (<PRLABEL> . ::obj)
	   (<PRSEP> . ::obj)
	   (<PR> . ::obj)
	   
	   (init-hop-prefs-services!)
	   (preferences-mutex)
	   
	   (preferences-register-save! ::bstring ::procedure)
	   
	   (user-write-preferences ::user)
	   (user-preference-get ::user ::symbol #!key default)
	   (user-preference-set! ::user ::symbol ::obj)
	   (user-preference-store! ::user ::symbol ::obj)
	   (user-preference-update! ::user ::symbol ::obj
				    #!key (kons cons) (init '()))
	   
	   (write-preferences request)
	   (preference-get ::symbol #!key default (request (current-request)))
	   (preference-set! ::symbol ::obj #!key (request (current-request)))
	   (preference-store! ::symbol ::obj #!key (request (current-request)))
	   (preference-update! ::symbol ::obj
			       #!key
			       (request (current-request))
			       (kons cons)
			       (init '()))))

;*---------------------------------------------------------------------*/
;*    *preferences-mutex* ...                                          */
;*---------------------------------------------------------------------*/
(define *preferences-mutex* (make-mutex "preferences"))

;*---------------------------------------------------------------------*/
;*    preferences-mutex ...                                            */
;*---------------------------------------------------------------------*/
(define (preferences-mutex)
   *preferences-mutex*)

;*---------------------------------------------------------------------*/
;*    *pref-validator-table* ...                                       */
;*---------------------------------------------------------------------*/
(define *pref-validator-table*
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    *pref-set-table* ...                                             */
;*---------------------------------------------------------------------*/
(define *pref-set-table*
   (make-hashtable))
 
;*---------------------------------------------------------------------*/
;*    *pref-save-table* ...                                            */
;*---------------------------------------------------------------------*/
(define *pref-save-table*
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    *prefs-service* ...                                              */
;*---------------------------------------------------------------------*/
(define *prefs-edit-svc* #unspecified)
(define *prefs-save-svc* #unspecified)

;*---------------------------------------------------------------------*/
;*    preferences-register-save! ...                                   */
;*---------------------------------------------------------------------*/
(define (preferences-register-save! key procedure)
   (mutex-lock! (preferences-mutex))
   (hashtable-put! *pref-save-table* key procedure)
   (mutex-unlock! (preferences-mutex)))

;*---------------------------------------------------------------------*/
;*    init-hop-prefs-services! ...                                     */
;*    -------------------------------------------------------------    */
;*    The svc URLs are used in hop-prefs.js                            */
;*---------------------------------------------------------------------*/
(define (init-hop-prefs-services!)
   ;; prefs/edit
   (set! *prefs-edit-svc*
	 (service :name "admin/preferences/edit" (name type value key)
	    (if (and name type value key)
		(begin
		   (mutex-lock! (preferences-mutex))
		   (let* ((pref (string->symbol name))
			  (value (string->value (string->symbol type) value))
			  (valid (hashtable-get *pref-validator-table* pref)))
		      (mutex-unlock! (preferences-mutex))
		      (when (or (not valid) (valid value))
			 (if (string=? key "pref")
			     (preference-store! pref value)
			     (begin
				(mutex-lock! (preferences-mutex))
				(let ((s (hashtable-get *pref-set-table* key)))
				   (mutex-unlock! (preferences-mutex))
				   (when (procedure? s)
				      (s value)))))
			 #t)))
		(http-bad-request "admin/preferences/edit"))))
   ;; prefs/save
   (set! *prefs-save-svc*
	 (service :name "admin/preferences/save" (key file ov)
	    (if (and key file)
		(begin
		   (mutex-lock! (preferences-mutex))
		   (let ((save (hashtable-get *pref-save-table* key))
			 (req (current-request)))
		      (mutex-unlock! (preferences-mutex))
		      (when (procedure? save)
			 (if (and (or (authorized-service? req 'admin)
				      (authorized-service? req 'admin/preferences/save))
				  (authorized-path? req file))
			     (save file ov)
			     (user-access-denied req)))))
		(http-bad-request "admin/preferences/save")))))

;*---------------------------------------------------------------------*/
;*    string->value ...                                                */
;*---------------------------------------------------------------------*/
(define (string->value type val)
   (case type
      ((string path text)
       val)
      ((number)
       (string->number val))
      ((integer)
       (string->integer val))
      ((symbol)
       (string->symbol val))
      ((bool)
       (string=? val "true"))
      ((enum)
       (string->symbol val))
      ((expr quote)
       (with-input-from-string val read))
      (else
       (let ((v (with-input-from-string (string-append "(" val ")") read)))
	  (if (or (memq type '(|(list string)| |(list path)|)))
	      ;; just a hack to avoid usual error
	      (map (lambda (s)
		      (cond
			 ((string? s) s)
			 ((symbol? s) (symbol->string s))
			 ((integer? s) (integer->string s))
			 ((real? s) (real->string s))
			 ((keyword? s) (keyword->string s))
			 (else (error 'string->value "Illegal value" val))))
		   v)
	      v)))))

;*---------------------------------------------------------------------*/
;*    value->string ...                                                */
;*---------------------------------------------------------------------*/
(define (value->string type val)
   (let loop ((type type)
	      (val val)
	      (kwote #f))
      (case type
	 ((string path)
	  (if kwote
	      (if (eq? val #unspecified)
		  "\"\""
		  (string-append "\"" (string-for-read val) "\"") )
	      (if (eq? val #unspecified)
		  ""
		  val)))
	 ((symbol)
	  (if (eq? val #unspecified)
	      '||
	      val))
	 ((expr quote)
	  (let ((s (open-output-string)))
	     (write val s)
	     (close-output-port s)))
	 ((number integer)
	  (if (eq? val #unspecified)
	      0
	      (number->string val)))
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
	      (if (eq? val #unspecified)
		  (symbol->string a)
		  (symbol->string val)))
	     ((text ?- . ?-)
	      val)
	     (else
	      (error 'value->string
		     "Illegal type (see the documentation)"
		     type)))))))
		 
;*---------------------------------------------------------------------*/
;*    add-validator! ...                                               */
;*---------------------------------------------------------------------*/
(define (add-validator! pref validator)
   (unless (correct-arity? validator 1)
      (error '<PR> "Illegal validate arity" validator))
   (mutex-lock! (preferences-mutex))
   (hashtable-update! *pref-validator-table* pref cons (list validator))
   (mutex-unlock! (preferences-mutex)))
   
;*---------------------------------------------------------------------*/
;*    user-write-preferences ...                                       */
;*---------------------------------------------------------------------*/
(define (user-write-preferences user::user)
   (mutex-lock! (preferences-mutex))
   (with-access::user user (preferences preferences-filename)
      (with-output-to-file preferences-filename
	 (lambda ()
	    (display "(\n")
	    (for-each (lambda (p)
			 (display " ")
			 (write p)
			 (newline))
		      preferences)
	    (display ")\n"))))
   (mutex-unlock! (preferences-mutex)))

;*---------------------------------------------------------------------*/
;*    user-preference-get ...                                          */
;*---------------------------------------------------------------------*/
(define (user-preference-get user::user prop #!key default)
   (with-access::user user (preferences)
      (mutex-lock! (preferences-mutex))
      (let* ((c (assq prop preferences))
	     (v (if (pair? c) (cadr c) default)))
	 (mutex-unlock! (preferences-mutex))
	 v)))

;*---------------------------------------------------------------------*/
;*    inner-preference-set! ...                                        */
;*---------------------------------------------------------------------*/
(define (inner-preference-set! user::user prop val store)
   (with-access::user user (preferences)
      (mutex-lock! (preferences-mutex))
      (let ((c (assq prop preferences)))
	 (if (pair? c)
	     (set-cdr! c (cons val store))
	     (set! preferences (cons (cons prop (cons val store)) preferences)))
	 (mutex-unlock! (preferences-mutex)))))

;*---------------------------------------------------------------------*/
;*    user-preference-set! ...                                         */
;*---------------------------------------------------------------------*/
(define (user-preference-set! user::user prop val)
   (inner-preference-set! user prop val #f))

;*---------------------------------------------------------------------*/
;*    user-preference-store! ...                                       */
;*---------------------------------------------------------------------*/
(define (user-preference-store! user::user prop val)
   (inner-preference-set! user prop val #t)
   (when (hop-store-preferences) (user-write-preferences user)))

;*---------------------------------------------------------------------*/
;*    user-preference-update! ...                                      */
;*---------------------------------------------------------------------*/
(define (user-preference-update! user::user prop nv
				 #!key (kons cons) (init '()))
   (with-access::user user (preferences)
      (mutex-lock! (preferences-mutex))
      (let ((c (assq prop preferences)))
	 (if (pair? c)
	     (set-car! (cdr c) (kons nv (cadr c)))
	     (set! preferences (cons (list prop (kons nv init)) preferences)))
	 (mutex-unlock! (preferences-mutex))
	 (user-write-preferences user))))

;*---------------------------------------------------------------------*/
;*    preference-get ...                                               */
;*---------------------------------------------------------------------*/
(define (preference-get key #!key default (request (current-request)))
   (if (http-request? request)
       (with-access::http-request request (user)
	  (if (string? (user-preferences-filename user))
	      (user-preference-get user key :default default)
	      default))
       default))

;*---------------------------------------------------------------------*/
;*    preference-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (preference-set! key val #!key (request (current-request)))
   (when (http-request? request)
      (with-access::http-request request (user)
	 (when (string? (user-preferences-filename user))
	    (user-preference-set! user key val)))))

;*---------------------------------------------------------------------*/
;*    preference-store! ...                                            */
;*---------------------------------------------------------------------*/
(define (preference-store! key val #!key (request (current-request)))
   (when (http-request? request)
      (with-access::http-request request (user)
	 (when (string? (user-preferences-filename user))
	    (user-preference-store! user key val)))))

;*---------------------------------------------------------------------*/
;*    preference-update! ...                                           */
;*---------------------------------------------------------------------*/
(define (preference-update! key nv
			    #!key
			    (request (current-request))
			    (kons cons)
			    (init '()))
   (when (http-request? request)
      (with-access::http-request request (user)
	 (when (string? (user-preferences-filename user))
	    (user-preference-update! user key nv :kons kons :init init)))))
				  
;*---------------------------------------------------------------------*/
;*    write-preferences ...                                            */
;*---------------------------------------------------------------------*/
(define (write-preferences request)
   (when (http-request? request)
      (with-access::http-request request (user)
	 (when (string? (user-preferences-filename user))
	    (user-write-preferences user)))))

;*---------------------------------------------------------------------*/
;*    make-class-name ...                                              */
;*---------------------------------------------------------------------*/
(define (make-class-name::bstring default::bstring name)
   (if (string? name)
       (string-append default " " name)
       default))

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
      (markup 'table)
      (id (xml-make-id id 'PREFS))
      (attributes (cons (cons 'class (make-class-name "hop-prefs" class))
			attrs))
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
   (when (procedure? validate) (add-validator! pref validate))
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
		    (error '<PR> "Illegal :param attribute" param))))
	       (else
		(error '<PR> "Either `pref' or `param' must be provided" id))))
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
      (mutex-lock! (preferences-mutex))
      (hashtable-put! *pref-set-table* (symbol->string key) (caddr param))
      (mutex-unlock! (preferences-mutex))
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
	   (error 'pr-editor "Illegal syntax" type))))
      (else
       pr-editor-input)))

;*---------------------------------------------------------------------*/
;*    pr-editor-input ...                                              */
;*---------------------------------------------------------------------*/
(define (pr-editor-input name type value title parse key)
   (<INPUT> :class "hop-pr-editor-expr hop-pr-saved"
      :type "text"
      :value (value->string type value)
      :title (format "~a (hit [return] to validate)"
		     (if (string? title) title name))
      :onkeyup (format "hop_prefs_editor_expr( event, this, \"~a\", ~a, \"~a\", \"~a\" )"
		       name (hop->js-callback parse) type key)))

;*---------------------------------------------------------------------*/
;*    pr-editor-text ...                                               */
;*---------------------------------------------------------------------*/
(define (pr-editor-text name type value title parse key rows cols)
   (<TEXTAREA> :class "hop-pr-editor-text hop-pr-saved"
      :rows rows :cols cols
      :title (format "~a (hit [return] to validate)"
		     (if (string? title) title name))
      :onkeyup (format "hop_prefs_editor_expr( event, this, \"~a\", ~a, \"~a\", \"~a\" )"
		       name (hop->js-callback parse) type key)
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
      (let ((name (symbol->string (gensym))))
	 (<TABLE> :class "hop-pr-editor-bool"
	    (<COLGROUP>
	       (<COL> :span 2 :width "1*"))
	    (<TR>
	       (<TD>
		  (<INPUT>
		     :type "radio"
		     :checked value
		     :name name
		     :onclick (format "hop_prefs_editor_bool( event, \"true\", \"~a\", ~a, \"~a\", \"~a\" )"
				      name (hop->js-callback parse) 'bool key))
		  yes-string)
	       (<TD> 
		  (<INPUT>
		     :type "radio"
		     :checked (not value)
		     :name name
		     :onclick (format "hop_prefs_editor_bool( event, \"false\", \"~a\", ~a, \"~a\", \"~a\" )"
				      name (hop->js-callback parse) 'bool key))
		  no-string))))))

;*---------------------------------------------------------------------*/
;*    pr-editor-enum ...                                               */
;*---------------------------------------------------------------------*/
(define (pr-editor-enum name type value title parse key)
   (let* ((name (symbol->string (gensym)))
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
			  :onclick (format "hop_prefs_editor_bool( event, \"~a\", \"~a\", ~a, \"~a\", \"~a\" )"
					   s name (hop->js-callback parse) 'enum key))
		       (symbol->string s))))
	      enum))))
