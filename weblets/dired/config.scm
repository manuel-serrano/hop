;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/dired/config.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  6 17:57:14 2005                          */
;*    Last change :  Tue Jan 24 17:51:51 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The dired user configuration                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    dired-config ...                                                 */
;*---------------------------------------------------------------------*/
(define-service (dired/config path prop val)
   (let* ((req (the-current-request))
	  (mode (if (string=? prop "mode")
		    (string->symbol val)
		    (dired-cookie-val req "dired" :mode 'icon)))
	  (hide (if (string=? prop "hide")
		    (string->symbol val)
		    (dired-cookie-val req "dired" :hide 'false)))
	  (sort (if (string=? prop "sort")
		    (string->symbol val)
		    (dired-cookie-val req "dired" :sort 'name)))
	  (order (if (string=? prop "order")
		     (string->symbol val)
		     (dired-cookie-val req "dired" :order 'increase)))
	  (cookie (cons Set-Cookie: 
			(format "dired=\"(:mode ~a :hide ~a :sort ~a :order ~a)\"; path=/;"
				mode
				hide
				sort
				order))))
      ;; add the cookie for the response
      (http-request-hook-add! req
			      (lambda (rep)
				 (with-access::%http-response rep (header)
				    (set! header (cons cookie header)))))
      ;; strip the dired-config prefix for the regular directory filter
      (http-request-path-set! req path)
      ;; apply the regular directory filter
      (dired-show-directory req mode (eq? hide 'true) sort order)))

;*---------------------------------------------------------------------*/
;*    <DIRED-CONFIG> ...                                               */
;*---------------------------------------------------------------------*/
(define (<DIRED-CONFIG> req dir hide mode sort order)
   (define (config-mode dir)
      (<TABLE>
       (<TR>
	(<TH> "Show as...")
	(<TD> "icons")
	(<TD> (<INPUT>
	       :type 'radio
	       :name "mode"
	       :value "icon"
	       :checked (eq? mode 'icon)
	       :onclick {hop( $dired/config( $dir, "mode", "icon" ),
			      hop_replace_document )})))
       (<TR>
	(<TD>)
	(<TD> "names")
	(<TD> (<INPUT>
	       :type 'radio
	       :name "mode"
	       :value "name"
	       :checked (eq? mode 'name)
	       :onclick {hop( $dired/config( $dir, "mode", "name" ),
			      hop_replace_document )})))))
   (define (config-sort dir)
      (<TABLE>
       (<TR>
	(<TH> "Sort by...")
	(<TD> "name")
	(<TD> (<INPUT>
	       :type 'radio
	       :name "sort"
	       :value "name"
	       :checked (eq? sort 'name)
	       :onclick {hop( $dired/config( $dir, "sort", "name" ),
			      hop_replace_document )})))
       (<TR>
	(<TH>)
	(<TD> "date")
	(<TD> (<INPUT>
	       :type 'radio
	       :name "sort"
	       :value "date"
	       :checked (eq? sort 'date)
	       :onclick {hop( $dired/config( $dir, "sort", "date" ),
			      hop_replace_document )})))
       (<TR>
	(<TH>)
	(<TD> "size")
	(<TD> (<INPUT>
	       :type 'radio
	       :name "sort"
	       :value "size"
	       :checked (eq? sort 'size)
	       :onclick {hop( $dired/config( $dir, "sort", "size" ),
			      hop_replace_document )})))
       (<TR>
	(<TH>)
	(<TD> "size")
	(<TD> (<INPUT>
	       :type 'radio
	       :name "sort"
	       :value "user"
	       :checked (eq? sort 'user)
	       :onclick {hop( $dired/config( $dir, "sort", "user" ),
			      hop_replace_document )})))))
   (define (config-order dir)
      (<TABLE>
       (<TR>
	(<TH> "Order...")
	(<TD> "increase")
	(<TD> (<INPUT>
	       :type 'radio
	       :name "order"
	       :value "increate"
	       :checked (eq? order 'increase)
	       :onclick {hop( $dired/config( $dir, "order", "increase" ),
			      hop_replace_document )})))
       (<TR>
	(<TD>)
	(<TD> "decrease")
	(<TD> (<INPUT>
	       :type 'radio
	       :name "order"
	       :value "decrease"
	       :checked (eq? order 'decrease)
	       :onclick {hop( $dired/config( $dir, "order", "decrease" ),
			      hop_replace_document )})))))
   (define (config-hide dir)
      (<TABLE>
       (<TR>
	(<TH> "Show hidden files")
	(<TD> (<INPUT>
	       :type 'checkbox
	       :name "show-hidden-files"
	       :value "yes"
	       :checked (not hide)
	       :onclick {hop( $dired/config( $dir,
					     "hide",
					     this.checked ?
					     "false" : "true" ),
			      hop_replace_document )})))))
   (define (config-css dir)
      (let* ((sep (if (char=? (string-ref dir (-fx (string-length dir) 1))
			      (file-separator))
		      ""
		      (string (file-separator))))
	     (css1 (string-append dir sep "dired.css"))
	     (css2 (string-append dir sep ".dired.css"))
	     (c (suffix-assoc-ci "dired.css" *dired-commands*))
	     (cmd (if (not (pair? c))
		      "emacs"
		      (let* ((k (if (http-request-localclientp req)
				    'local
				    'remote))
			     (apps (assq k (cdr c))))
			 (if (not apps)
			     "emacs"
			     (car (cadr apps)))))))
	 (define (<EDIT> css)
	    (<SPAN> :class "edit"
		    :onclick {hop( $dired/exec( $cmd, $css ), false )}
		    (basename css)))
	 (cond
	    ((and (file-exists? css1) (file-exists? css2))
	     (<TABLE>
	      (<TR>
	       (<TH> "Edit css")
	       (<TD> (<TABLE>
		      (<TR> (<EDIT> css1))
		      (<TR> (<EDIT> css2)))))))
	    ((file-exists? css1)
	     (<TABLE>
	      (<TR>
	       (<TH> "Edit css")
	       (<TD> (<EDIT> css1)))))
	    ((file-exists? css2)
	     (<TABLE>
	      (<TR>
	       (<TH> "Edit css")
	       (<TD> (<EDIT> css2)))))
	    (else
	     (<TABLE>
	      (<TR>
	       (<TH> "Edit css")
	       (<TD> (<EDIT> css2))))))))
   (with-access::http-request req (path)
      (<DIV>
       :class "config"
       (<FORM>
	:method 'put
	:action path
	(<TABLE>
	 (<TR>
	  (<TD> :valign "top" (config-mode dir))
	  (<TD> :valign "top" (config-sort dir))
	  (<TD> :valign "top" (config-order dir))
	  (<TD> :valign "top" (config-hide dir))
	  (<TD> :valign "top" (config-css dir))))))))

