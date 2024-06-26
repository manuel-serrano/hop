;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/runtime/preferences.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 28 07:45:15 2006                          */
;*    Last change :  Tue May 14 13:12:47 2024 (serrano)                */
;*    Copyright   :  2006-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Preferences editor                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_preferences

   (library http)
   
   (include "xml.sch"
	    "service.sch")
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml-types
	    __hop_xml
	    __hop_html-base
	    __hop_html-head
	    __hop_service
	    __hop_js-comp
	    __hop_read
	    __hop_hop
	    __hop_user
	    __hop_http-error
	    __hop_security)
   
   (export (init-hop-prefs-services!)
	   (preferences-mutex)

	   (preferences-add-validator! ::obj ::procedure)
	   (preferences-add-pr-param! ::obj ::obj)
	   (preferences-register-save! ::bstring ::procedure)
	   
	   (user-write-preferences ::user)
	   (user-preference-get ::user ::symbol #!key default)
	   (user-preference-set! ::user ::symbol ::obj)
	   (user-preference-store! ::user ::symbol ::obj)
	   (user-preference-update! ::user ::symbol ::obj #!key (kons cons) (init '()))
	   
	   (write-preferences request)
	   (preference-get ::symbol #!key default request)
	   (preference-set! ::symbol ::obj #!key request)
	   (preference-store! ::symbol ::obj #!key request)
	   (preference-update! ::symbol ::obj #!key request (kons cons) (init '()))))

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
   (synchronize (preferences-mutex)
      (hashtable-put! *pref-save-table* key procedure)))

;*---------------------------------------------------------------------*/
;*    preferences-add-pr-param! ...                                    */
;*---------------------------------------------------------------------*/
(define (preferences-add-pr-param! key param)
   (synchronize (preferences-mutex)
      (hashtable-put! *pref-set-table* key param)))
      
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
	     (multiple-value-bind (pref value)
		(prefs-decode-value name value type)
		(let ((valid (synchronize (preferences-mutex)
				(hashtable-get *pref-validator-table* pref))))
		   (when (or (not valid) (valid value))
		      (if (string=? key "pref")
			  (preference-store! pref value)
			  (let ((s (synchronize (preferences-mutex)
				      (hashtable-get *pref-set-table* key))))
			     (when (procedure? s)
				(s value))))
		      #t)))
	     (http-bad-request "admin/preferences/edit"))))
   ;; prefs/save
   (set! *prefs-save-svc*
      (service :name "admin/preferences/save" (key file ov)
	 (if (and key file)
	     (let ((save (synchronize (preferences-mutex)
			    (hashtable-get *pref-save-table* key))))
		(let ((req (current-request)))
		   (when (procedure? save)
		      (if (and (or (authorized-service? req 'admin)
				   (authorized-service? req 'admin/preferences/save))
			       (authorized-path? req file))
			  (save file ov)
			  (access-denied req)))))
	     (http-bad-request "admin/preferences/save")))))

;*---------------------------------------------------------------------*/
;*    prefs-decode-value ...                                           */
;*---------------------------------------------------------------------*/
(define (prefs-decode-value name value type)
   (match-case (with-input-from-string name read)
      ((and ?name (? symbol?))
       (values name
	       (string->value (string->symbol type) value)))
      ((add ?name)
       (values name
	       (cons (string->value (string->symbol type) value)
		     (preference-get name :default '()))))
      ((aaddk ?name)
       (let ((def (preference-get name :default '())))
	  (values name
		  (if (and (pair? def) (eq? (caar def) #unspecified))
		      (cons (list (string->value (string->symbol type) value)
				  (cadar def))
			    (cdr def))
		      (cons (list (string->value (string->symbol type) value)
				  #unspecified)
			    def)))))
      ((aaddv ?name)
       (let ((def (preference-get name :default '())))
	  (values name
		  (if (and (pair? def) (eq? (cadr (car def)) #unspecified))
		      (cons (list (caar def)
				  (string->value (string->symbol type) value))
			    (cdr def))
		      (cons (list #unspecified
				  (string->value (string->symbol type) value))
			    def)))))
      ((set ?i ?name)
       (values name
	       (let loop ((l (preference-get name :default '())))
		  (cond
		     ((null? l)
		      l)
		     ((=fx i 0)
		      (cons (string->value (string->symbol type) value)
			    (cdr l)))
		     (else
		      (cons (car l) (loop (cdr l))))))))
      ((asetk ?i ?name)
       (values name
	       (let loop ((l (preference-get name :default '())))
		  (cond
		     ((null? l)
		      l)
		     ((=fx i 0)
		      (cons (list (string->value (string->symbol type) value)
				  (cadar l))
			    (cdr l)))
		     (else
		      (cons (car l) (loop (cdr l))))))))
      ((asetv ?i ?name)
       (values name
	       (let loop ((l (preference-get name :default '()))
			  (i i))
		  (cond
		     ((null? l)
		      l)
		     ((=fx i 0)
		      (cons (list (caar l)
				  (string->value (string->symbol type) value))
			    (cdr l)))
		     (else
		      (cons (car l) (loop (cdr l) (- i 1))))))))
      ((del ?i ?name)
       (values name
	       (let loop ((l (preference-get name :default '()))
			  (i i))
		  (cond
		     ((null? l)
		      l)
		     ((=fx i 0)
		      (cdr l))
		     (else
		      (cons (car l) (loop (cdr l) (- i 1))))))))
      (else
       (error "<PR>" "Illegal prefs name" name))))

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
      ((elong)
       (string->elong val))
      ((real)
       (string->real val))
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
			 ((elong? s) (elong->string s))
			 ((real? s) (real->string s))
			 ((keyword? s) (keyword->string s))
			 (else (error "string->value" "Illegal value" val))))
		   v)
	      v)))))

;*---------------------------------------------------------------------*/
;*    preferences-add-validator! ...                                   */
;*---------------------------------------------------------------------*/
(define (preferences-add-validator! pref validator)
   (unless (correct-arity? validator 1)
      (error "<PR>" "Illegal validate arity" validator))
   (synchronize (preferences-mutex)
      (hashtable-update! *pref-validator-table* pref cons (list validator))))
   
;*---------------------------------------------------------------------*/
;*    user-write-preferences ...                                       */
;*---------------------------------------------------------------------*/
(define (user-write-preferences user::user)
   (synchronize (preferences-mutex)
      (with-access::user user (preferences preferences-filename)
	 (with-output-to-file preferences-filename
	    (lambda ()
	       (display "(\n")
	       (for-each (lambda (p)
			    (display " ")
			    (write p)
			    (newline))
		  preferences)
	       (display ")\n"))))))

;*---------------------------------------------------------------------*/
;*    user-preference-get ...                                          */
;*---------------------------------------------------------------------*/
(define (user-preference-get user::user prop #!key default)
   (with-access::user user (preferences)
      (synchronize (preferences-mutex)
	 (let ((c (assq prop preferences)))
	    (if (pair? c) (cadr c) default)))))

;*---------------------------------------------------------------------*/
;*    inner-preference-set! ...                                        */
;*---------------------------------------------------------------------*/
(define (inner-preference-set! user::user prop val store)
   (with-access::user user (preferences)
      (synchronize (preferences-mutex)
	 (let ((c (assq prop preferences)))
	    (if (pair? c)
		(set-cdr! c (cons val store))
		(set! preferences
		   (cons (cons prop (cons val store)) preferences)))))))

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
      (synchronize (preferences-mutex)
	 (let ((c (assq prop preferences)))
	    (if (pair? c)
		(set-car! (cdr c) (kons nv (cadr c)))
		(set! preferences
		   (cons (list prop (kons nv init)) preferences)))))
      (user-write-preferences user)))

;*---------------------------------------------------------------------*/
;*    preference-get ...                                               */
;*---------------------------------------------------------------------*/
(define (preference-get key #!key default request)
   (if (isa? request http-request)
       (let ((user (http-request-user request)))
	  (with-access::user user (preferences-filename)
	     (if (string? preferences-filename)
		 (user-preference-get user key :default default)
		 default)))
       default))

;*---------------------------------------------------------------------*/
;*    preference-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (preference-set! key val #!key request)
   (when (isa? request http-request)
      (let ((user (http-request-user request)))
	 (with-access::user user (preferences-filename)
	    (when (string? preferences-filename)
	       (user-preference-set! user key val))))))

;*---------------------------------------------------------------------*/
;*    preference-store! ...                                            */
;*---------------------------------------------------------------------*/
(define (preference-store! key val #!key request)
   (when (isa? request http-request)
      (let ((user (http-request-user request)))
	 (with-access::user user (preferences-filename)
	    (when (string? preferences-filename)
	       (user-preference-store! user key val))))))

;*---------------------------------------------------------------------*/
;*    preference-update! ...                                           */
;*---------------------------------------------------------------------*/
(define (preference-update! key nv #!key request (kons cons) (init '()))
   (when (isa? request http-request)
      (let ((user (http-request-user request)))
	 (with-access::user user (preferences-filename)
	    (when (string? preferences-filename)
	       (user-preference-update! user key nv :kons kons :init init))))))
				  
;*---------------------------------------------------------------------*/
;*    write-preferences ...                                            */
;*---------------------------------------------------------------------*/
(define (write-preferences request)
   (when (isa? request http-request)
      (let ((user (http-request-user request)))
	 (with-access::user user (preferences-filename)
	    (when (string? preferences-filename)
	       (user-write-preferences user))))))


