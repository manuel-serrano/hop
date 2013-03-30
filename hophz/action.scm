;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/hophz/action.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 29 09:52:32 2012                          */
;*    Last change :  Sat Mar 30 19:49:34 2013 (serrano)                */
;*    Copyright   :  2012-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    hophz actions                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hophz_action
   
   (library scheme2js hopscheme hop)

   (import  hophz_parseargs
	    hophz_param
	    hophz_login
	    hz_api)

   (export  (abstract-class action
	       (options::int (default 0))
	       (args::pair-nil (default '())))

	    (class install-action::action
	       (name::bstring read-only))

	    (class uninstall-action::action
	       (name::bstring read-only))

	    (class update-action::action)
	    
	    (class upgrade-action::action)

	    (class list-action::action
	       (verbose::int read-only (default 1)))
	    
	    (class depends-action::action
	       (name::bstring read-only))

	    (class clean-action::action)

	    (class config-action::action)

	    (class publisher-action::action
	       (action::symbol read-only))
	       
	    (class download-action::action
	       (url::bstring read-only))

	    (generic action-exec ::action)))

;*---------------------------------------------------------------------*/
;*    action-exec ::action ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (action-exec a::action))

;*---------------------------------------------------------------------*/
;*    host-port-name ...                                               */
;*---------------------------------------------------------------------*/
(define (host-port-name host)
   (let ((i (string-index host #\:)))
      (if (fixnum? i)
	  (values (substring host 0 i)
	     (string->integer (substring host (+fx i 1))))
	  (values host 8080))))

;*---------------------------------------------------------------------*/
;*    login->header ...                                                */
;*---------------------------------------------------------------------*/
(define (login->header login)
   (if login
       `((:authorization ,login))
       '()))

;*---------------------------------------------------------------------*/
;*    display-color ...                                                */
;*---------------------------------------------------------------------*/
(define (display-color col . msg)
   (let ((c (let ((c (assq col (hophz-colors))))
	       (if (pair? c)
		   (+fx (modulofx (cadr c) 16) 1)
		   0))))
      (for-each (lambda (m) (display (trace-color c m))) msg)))

;*---------------------------------------------------------------------*/
;*    filter-publishers ...                                            */
;*---------------------------------------------------------------------*/
(define (filter-publishers w)
   (if (null? (hophz-publishers))
       w
       (filter (lambda (w)
		  (with-access::weblet w (publisher)
		     (member publisher (hophz-publishers))))
	  w)))

;*---------------------------------------------------------------------*/
;*    show-weblet ...                                                  */
;*---------------------------------------------------------------------*/
(define (show-weblet w #!optional (verb 1))
   (with-access::weblet w (name version install category title comment url publisher depends installable)
      (display-color 'category category "/")
      (display-color 'weblet name)
      (display " ")
      (display-color 'version version)
      (display " ")
      (unless (string-null? install)
	 (if (string=? install version)
	     (display-color 'install "[installed]")
	     (display-color 'out-of-date "[installed " install "]")))
      (unless installable
	 (display-color 'error "[incompatible]"))
      (when (>=fx verb 2)
	 (display-color 'keyword "\n  url: ") (print url)
	 (when (string? publisher)
	    (display-color 'keyword "  publisher: ") (print publisher))
	 (when (pair? depends)
	    (display-color 'keyword "  depends: ")
	    (printf "~(, )"
	       (map (lambda (x)
		       (format "~l" x))
		  depends)))
	 (newline))
      (newline)
      (when (>=fx verb 1)
	 (when (>fx (string-length title) 0)
	    (print title ":")))
      (when (and (>=fx verb 3) (not (string-null? comment)))
	 (print "    " comment))
      (when (>=fx verb 1)
	 (newline))))

;*---------------------------------------------------------------------*/
;*    hz-with-hop ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (hz-with-hop url callback)
   `(multiple-value-bind (hname hport)
      (host-port-name (hophz-host))
      (let loopauth ()
	 (with-hop ,url
	    :host hname :port hport :authorization (hophz-login)
	    ,callback
	    (lambda (e)
	       (with-access::xml-http-request e (status)
		  (if (=fx status 401)
		      (begin
			 (printf "~a:~a authentication required\n" hname hport)
			 (login!)
			 (loopauth))
		      (error "hophz" "Illegal request" status))))))))

;*---------------------------------------------------------------------*/
;*    install-weblets ...                                              */
;*---------------------------------------------------------------------*/
(define (install-weblets lweblets)
   
   (define (collect-uninstalled-weblets l)
      (let loop ((l l))
	 (if (null? l)
	     '()
	     (with-access::weblet (car l) (install version)
		(if (or (string-null? install)
			(>fx (string-natural-compare3 version install) 0))
		    (cons (car l) (append-map loop (cdr l)))
		    '())))))

   (define (confirm-install-weblets? l)
      (or (hophz-noconfirm)
	  (null? l)
	  (null? (cdr l))
	  (begin
	     (printf "The following weblets are going to be installed: ~(, )\n"
		(map (lambda (w)
			(with-access::weblet w (name) name))
		   l))
	     (yes-or-no? "Confirm installation"))))

   (let ((to-install (delete-duplicates!
			(append-map collect-uninstalled-weblets lweblets))))
      (when (confirm-install-weblets? to-install)
	 (let loop ((to-install to-install))
	    (when (pair? to-install)
	       (with-access::weblet (car to-install) (installable name)
		  (if (and (not installable) (not (hophz-force-action)))
		      (error "hophz" "Weblet not installable" name)
		      (hz-with-hop (hz/weblet/install :weblet (car to-install)
				      :override (hophz-force-download))
			 (lambda (w)
			    (cond
			       ((isa? w weblet)
				(with-access::weblet w (name category version)
				   (display-color 'category category "/")
				   (display-color 'weblet name)
				   (display " ")
				   (display-color 'version version)
				   (print "...installed.")
				   (loop (cdr to-install))))
			       ((string? w)
				(display-color 'error w #\Newline))
			       (else
				(display-color 'error name)
				(print " cannot be installed."))))))))))))
   
;*---------------------------------------------------------------------*/
;*    action-exec ::install-action ...                                 */
;*---------------------------------------------------------------------*/
(define-method (action-exec a::install-action)
   (with-access::install-action a (name args)
      ;; collect all the dependences
      (hz-with-hop (hz/find/weblet
		      :name name
		      :category (when (pair? args) (car args)))
	 (lambda (w)
	    (if (isa? w weblet)
		(hz-with-hop (hz/weblet/depends :weblet w)
		   (lambda (l)
		      (when l
			 (install-weblets (list l)))))
		(error "hophz" "Cannot find weblet" name))))))

;*---------------------------------------------------------------------*/
;*    action-exec ::uninstall-action ...                               */
;*---------------------------------------------------------------------*/
(define-method (action-exec a::uninstall-action)
   (with-access::uninstall-action a (name args)
      (hz-with-hop (hz/find/weblet
		      :name name
		      :category (when (pair? args) (car args)))
	 (lambda (w)
	    (hz-with-hop (hz/weblet/uninstall :weblet w)
	       (lambda (w)
		  (cond
		     ((isa? w weblet)
		      (with-access::weblet w (name category version)
			 (display-color 'category category "/")
			 (display-color 'weblet name)
			 (display " ")
			 (display-color 'version version)
			 (print "...removed.")))
		     ((string? w)
		      (display-color 'error w #\Newline))
		     (else
		      (display-color 'error name)
		      (print " cannot be removed.")))))))))

;*---------------------------------------------------------------------*/
;*    action-exec ::update-action ...                                  */
;*---------------------------------------------------------------------*/
(define-method (action-exec a::update-action)
   (let loop ()
      (hz-with-hop (hz/update)
	 (lambda (r)
	    (if (string? r)
		(begin
		   (display "Authentication needed for \"")
		   (display-color 'error r)
		   (print "\"")
		   (multiple-value-bind (user prompt)
		      (display "login: ")
		      (flush-output-port (current-output-port))
		      (let* ((user (read-line))
			     (passwd (password "password: ")))
			 (hz-with-hop (hz/sync/authenticate
					 :url r :user user :passwd passwd)
			    (lambda (_)
			       (loop))))))
		(let ((w (filter-publishers r)))
		   (if (pair? w)
		       (for-each show-weblet w)
		       (print "No upgrade needed."))))))))
   
;*---------------------------------------------------------------------*/
;*    action-exec ::upgrade-action ...                                 */
;*---------------------------------------------------------------------*/
(define-method (action-exec a::upgrade-action)
   (hz-with-hop (hz/list/update)
      (lambda (w)
	 (let loop ((w (filter-publishers w))
		    (all '()))
	    (if (null? w)
		(install-weblets all)
		(hz-with-hop (hz/weblet/depends :weblet (car w))
		   (lambda (l)
		      (loop (cdr w) (cons l all)))))))))
   
;*---------------------------------------------------------------------*/
;*    action-exec ::list-action ...                                    */
;*---------------------------------------------------------------------*/
(define-method (action-exec a::list-action)
   (with-access::list-action a (args verbose)
      (if (null? args)
	  ;; a plain list
	  (hz-with-hop (hz/list/weblets)
	     (lambda (w)
		(for-each (lambda (w)
			     (show-weblet w (+fx (hophz-verbose) verbose)))
		   (filter-publishers w))))
	  ;; a search
	  (hz-with-hop (hz/search/weblets
			  :regexp (car args)
			  :category (when (pair? (cdr args)) (cadr args)))
	     (lambda (w)
		(for-each (lambda (w)
			     (show-weblet w (+fx (hophz-verbose) verbose)))
		   (filter-publishers w)))))))

;*---------------------------------------------------------------------*/
;*    action-exec ::depends-action ...                                 */
;*---------------------------------------------------------------------*/
(define-method (action-exec a::depends-action)
   (with-access::depends-action a (name args)
      (hz-with-hop (hz/find/weblet
		      :name name
		      :category (when (pair? args) (car args)))
	 (lambda (w)
	    (when w
	       (hz-with-hop (hz/weblet/depends :weblet w)
		  (lambda (l)
		     (let loop ((l l)
				(m ""))
			(when (pair? l)
			   (display m)
			   (show-weblet (car l) 0)
			   (let ((nm (string-append m "    ")))
			      (for-each (lambda (d) (loop d nm)) (cdr l))))))))))))

;*---------------------------------------------------------------------*/
;*    action-exec ::clean-action ...                                   */
;*---------------------------------------------------------------------*/
(define-method (action-exec a::clean-action)
   (hz-with-hop (hz/clearcache)
      (lambda (path)
	 (display "Cache \"")
	 (display-color 'path path)
	 (print "\" cleared."))))

;*---------------------------------------------------------------------*/
;*    action-exec ::config-action ...                                  */
;*---------------------------------------------------------------------*/
(define-method (action-exec a::config-action)
   (hz-with-hop (hz/config)
      (lambda (l)
	 (display-color 'keyword "hophz")
	 (display ": ")
	 (display-color 'version (hophz-version))
	 (newline) (newline)
	 (for-each (lambda (e)
		      (display-color 'keyword (car e))
		      (apply printf ": ~(, )" (cdr e))
		      (newline))
	    l))))

;*---------------------------------------------------------------------*/
;*    action-exec ::publisher-action ...                               */
;*---------------------------------------------------------------------*/
(define-method (action-exec a::publisher-action)
   (with-access::publisher-action a (action args)
      (hz-with-hop (hz/publisher :url (car args) :action action)
	 (lambda (l)
	    (display-color 'keyword "New publisher list")
	    (printf ": ~(, )\n" l)))))

;*---------------------------------------------------------------------*/
;*    action-exec ::download-action ...                                */
;*---------------------------------------------------------------------*/
(define-method (action-exec a::download-action)
   (with-access::download-action a (url)
      (hz-with-hop (hz/download :url url)
	 (lambda (w)
	    (cond
	       ((isa? w weblet)
		(with-access::weblet w (name category version)
		   (display-color 'category category "/")
		   (display-color 'weblet name)
		   (display " ")
		   (display-color 'version version)
		   (print "...installed.")))
	       ((string? w)
		(display-color 'error w #\Newline))
	       (else
		(display-color 'error url)
		(print " cannot be installed.")))))))
   
;*---------------------------------------------------------------------*/
;*    yes-or-no? ...                                                   */
;*---------------------------------------------------------------------*/
(define (yes-or-no? message)
   (let loop ()
      (display message)
      (display " (y/n)? ")
      (let ((c (read)))
	 (or (eq? c 'y)
	     (if (eq? c 'n)
		 #f
		 (loop))))))
