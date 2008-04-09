;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/prefs.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 28 07:45:15 2006                          */
;*    Last change :  Mon Apr  7 16:50:33 2008 (serrano)                */
;*    Copyright   :  2006-08 Manuel Serrano                            */
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
	    __hop_user)
   
   (export (preferences-mutex)
	   (preferences-editor ::obj ::obj ::obj)
	   
	   (user-write-preferences ::user)
	   (user-preference-get ::user ::symbol #!key default)
	   (user-preference-set! ::user ::symbol ::obj)
	   (user-preference-store! ::user ::symbol ::obj)
	   (user-preference-update! ::user ::symbol ::obj
				    #!key (kons cons) (init '()))
	   (preference-get ::symbol #!key default (request (current-request)))
	   (preference-set! ::symbol ::obj #!key (request (current-request)))
	   (preference-store! ::symbol ::obj #!key (request (current-request)))
	   (preference-update! ::symbol ::obj
			       #!key
			       (request (current-request))
			       (kons cons)
			       (init '()))
	   (write-preferences request)))

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
;*    preferences-editor ...                                           */
;*---------------------------------------------------------------------*/
(define (preferences-editor type get set)
   (match-case type
      (integer
       (text-editor "An integer" get (service (v) (set (string->integer v)) #t)))
      (string
       (text-editor "A string" get (service (v) (set v) #t)))
      (expr
       (sexp-editor get set))
      (bool
       (bool-editor "yes" "no" get (service (v) (set v) #t)))
      ((bool (and ?yes (? string?)) (and ?no (? string?)))
       (bool-editor yes no get (service (v) (set v) #t)))
      (else
       (error 'preferences-editor "Illegal type" type))))

;*---------------------------------------------------------------------*/
;*    text-editor ...                                                  */
;*---------------------------------------------------------------------*/
(define (text-editor title get svc)
   (<INPUT>
      :class "pref_saved"
      :type "text"
      :size "100%"
      :value (get)
      :title (string-append title " (hit [return] to validate)")
      :onkeyup (format "{ if ( event.keyCode == 13 ) {
                            hop( ~a( this.value ) );
                            this.className = 'pref_applied';
                          } else {
                            this.className = 'pref_modified';
                          } }"
		       (hop->json svc #f))))

;*---------------------------------------------------------------------*/
;*    sexp-editor ...                                                  */
;*---------------------------------------------------------------------*/
(define (sexp-editor get set)
   (text-editor "An expression"
		(lambda ()
		   (with-output-to-string
		      (lambda ()
			 (write (get)))))
		(service (v)
		   (with-input-from-string v
		      (lambda ()
			 (set (read))))
		   #t)))

;*---------------------------------------------------------------------*/
;*    bool-editor ...                                                  */
;*---------------------------------------------------------------------*/
(define (bool-editor yes-string no-string get svc)
   (let ((checked (get))
	 (name (symbol->string (gensym))))
      (<TABLE>
	 (<COLGROUP>
	    (<COL> :span 2 :width "0*"))
	 (<TR>
	    (<TD>
	       (<INPUT>
		  :type "radio"
		  :checked checked
		  :name name
		  :onclick (format
			    "hop(~a(true)); this.className = 'pref_applied';"
			    (hop->json svc #f)))
		  yes-string)
	    (<TD> 
	       (<INPUT>
		  :type "radio"
		  :checked (not checked)
		  :name name
		  :onclick (format
			    "hop(~a(false)); this.className = 'pref_applied';"
			    (hop->json svc #f)))
		  no-string)))))


;*---------------------------------------------------------------------*/
;*    user-write-preferences ...                                       */
;*---------------------------------------------------------------------*/
(define (user-write-preferences user::user)
   (mutex-lock! (preferences-mutex))
   (with-access::user user (preferences preferences-filename)
      (when (hop-store-preferences)
	 (with-output-to-file preferences-filename
	    (lambda ()
	       (display "(\n")
	       (for-each (lambda (p)
			    (display " ")
			    (write p)
			    (newline))
			 preferences)
	       (display ")\n")))))
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
;*    user-preference-set! ...                                         */
;*---------------------------------------------------------------------*/
(define (user-preference-set! user::user prop val)
   (with-access::user user (preferences)
      (mutex-lock! (preferences-mutex))
      (let ((c (assq prop preferences)))
	 (if (pair? c)
	     (set-cdr! c (list val))
	     (set! preferences (cons (list prop val) preferences)))
	 (mutex-unlock! (preferences-mutex)))))

;*---------------------------------------------------------------------*/
;*    user-preference-store! ...                                       */
;*---------------------------------------------------------------------*/
(define (user-preference-store! user::user prop val)
   (user-preference-set! user prop val)
   (user-write-preferences user))

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

