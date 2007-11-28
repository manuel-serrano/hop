;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/prefs.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 28 07:45:15 2006                          */
;*    Last change :  Wed Nov 28 08:07:27 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
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
	    __hop_cgi
	    __hop_service
	    __hop_js-lib
	    __hop_read)

   (export (preferences-editor ::obj ::obj ::obj)))

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
		       (hop->json svc))))

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
			    (hop->json svc)))
		  yes-string)
	    (<TD> 
	       (<INPUT>
		  :type "radio"
		  :checked (not checked)
		  :name name
		  :onclick (format
			    "hop(~a(false)); this.className = 'pref_applied';"
			    (hop->json svc)))
		  no-string)))))

