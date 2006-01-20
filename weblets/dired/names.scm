;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/dired/names.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  6 17:34:20 2005                          */
;*    Last change :  Mon Jan  9 14:32:52 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    directories displayed with names                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    dired-pp-filename ...                                            */
;*---------------------------------------------------------------------*/
(define (dired-pp-filename name path)
   (cond
      ((member name '("COYPING" "ChangeLog" "INSTALL" "LICENSE" "Makefile"
				"README"))
       (<FONT> :color "LimeGreen" (<B> name)))
      ((string=? name "configure")
       (<FONT> :color "red" (<B> name)))
      ((char=? (string-ref name (-fx (string-length name) 1)) #\~)
       (<FONT> :color "#505050" name))
      ((directory? path)
       (<FONT> :color "SlateBlue" (<B> name)))
      ((suffix-member name '("lsm" "prj" "spec"))
       (<FONT> :color "Goldenrod" (<B> name)))
      (else
       name)))

;*---------------------------------------------------------------------*/
;*    dired-as-name-entry ...                                          */
;*---------------------------------------------------------------------*/
(define (dired-as-name-entry req dir f)
   (let* ((path (string-append dir (string (file-separator)) f))
	  (name (if (directory? path)
		    (string-append f (string (file-separator)))
		    f))
	  (mo (file-modification-time path))
	  (df (seconds->date mo)))
      (<TR>
       (<TD> :align 'right (linux-mode (file-mode path)))
       (<TD> :align 'right (file-uid path))
       (<TD> :align 'right (file-gid path))
       (<TD> :align 'right (number->string (file-size path)))
       (<TD> :align 'right :lang mo (month-aname (date-month df)))
       (<TD> :align 'right :lang mo (date-day df))
       (<TD> :align 'right :lang mo (if (=fx (date-year df)
					     (date-year (current-date)))
					(format "~a:~a"
						(date-hour df)
						(date-minute df))
					(date-year df)))
       (<TD> (<A> :href path (dired-pp-filename name path))))))

;*---------------------------------------------------------------------*/
;*    dired-as-name ...                                                */
;*---------------------------------------------------------------------*/
(define (dired-as-name req dir hide sort order)
   (let* ((adir (dired-canonicalize-dirname dir))
	  (files (dired-get-files dir hide sort order))
	  (rows (map (lambda (f)
			(dired-as-name-entry req adir f))
		     files)))
      (instantiate::http-response-hop
	 (xml (<HTML>
	       (<DIRED-HEAD> req adir)
	       (<BODY> :class "dired"
		       (<DIV> :class "header"
			      (<DIRED-DIR> req adir)
			      (<DIRED-CONFIG> req dir hide 'name sort order))
		       (<DIV> :class "entries"
			      (<PRE>
			       (<SORTTABLE>
				:border 0
				(<TR> (<TH> :align 'center "mode")
				      (<TH> :align 'right "user")
				      (<TH> "group")
				      (<TH> "size")
				      (<TH> "month")
				      (<TH> "day")
				      (<TH> "time")
				      (<TH> "name"))
				rows)))))))))
