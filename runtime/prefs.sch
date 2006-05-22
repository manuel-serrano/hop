;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/prefs.sch                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 28 07:04:20 2006                          */
;*    Last change :  Mon May 22 12:39:01 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The definition of the DEFINE-PREFERENCES macro.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-preferences ...                                           */
;*    -------------------------------------------------------------    */
;*    This generates three functions: id-load, id-save, and id-edit.   */
;*---------------------------------------------------------------------*/
(define-macro (define-preferences id . clauses)

   (define (make-load id)
      (let ((mod (eval-module)))
	 `(define (,id file)
	     ,(if (evmodule? mod)
		  `(hop-load file (eval-find-module ',(evmodule-name mod)))
		  `(hop-load file)))))

   (define (make-save-clause c)
      (match-case c
	 ((?- ?type ?get ?set)
	  `(begin
	      ,(if (eq? type 'expr)
		   `(write (list ,set '(,get)))
		   `(write (list ,set (,get))))
	      (newline)))
	 ((?- ?type (and (? symbol?) ?get))
	  `(begin
	      ,(if (eq? type 'expr)
		   `(write (list ',(symbol-append get '-set!)
				 (list 'quote (,get))))
		   `(write (list ',(symbol-append get '-set!) (,get))))
	      (newline)))
	 ((or (? string?) (? symbol?))
	  #unspecified)
	 (((? string?) ?-)
	  #unspecified)
	 (else
	  (error 'define-preferences "Illegal clause" c))))

   (define (make-save id)
      `(define (,id file)
	  (with-output-to-file file
	     (lambda ()
		,@(map make-save-clause clauses)))))

   (define (make-value-clause lbl value)
      `(<TR>
	  (<TH> ,lbl)
	  (<TD> ,value)))

   (define (make-edit-clause/set-get lbl type get set)
      `(<TR>
	  (<TH> ,lbl)
	  (<TD> (preferences-editor ',type ,get ,set))))

   (define (make-label-clause lbl)
      `(<TR>
	  (<TH> :class "label" :colspan 2 ,lbl)))
   
   (define (make-sep-clause lbl)
      `(<TR>
	  (<TD> :class "separator" :colspan 2 "&nbsp;")))
   
   (define (make-edit-clause c)
      (match-case c
	 (((and (? string?) ?lbl) ?value)
	  (make-value-clause lbl value))
	 ((?lbl ?type ?get ?set)
	  (make-edit-clause/set-get lbl type get set))
	 ((?lbl ?type (and (? symbol?) ?get))
	  (make-edit-clause/set-get lbl type get (symbol-append get '-set!)))
	 ((? string?)
	  (make-label-clause c))
	 (--
	  (make-sep-clause c))
	 (else
	  (error 'define-preferences "Illegal clause" c))))
      
   (define (make-edit id)
      `(define (,id #!key onclick)
	  (<TABLE>
	     :class "preferences"
	     (<COLGROUP>
		(<COL> :width "30%" :class "col1")
		(<COL> :class "col2"))
	     (when onclick
		(<TR> (<TD>
			 :class "save"
			 :colspan 2
			 :title "Save current preferences"
			 (<BUTTON>
			    :onclick
			    (string-append
			     "var els = document.getElementsByClass( 'pref_applied' ); var i; for( i = 0; i < els.length; i++ ) { els[ i ].className = 'pref_saved'; }"
			     onclick)
			    "save preferences"))))
	     ,@(map make-edit-clause clauses))))
   
   (unless (symbol? id)
      (error 'define-preferences "Illegal identifier" id))

   `(begin
       ,(make-load (symbol-append id '-load))
       ,(make-save (symbol-append id '-save))
       ,(make-edit (symbol-append id '-edit))))

