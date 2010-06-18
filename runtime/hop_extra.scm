;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/hop_extra.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 14 05:36:34 2005                          */
;*    Last change :  Fri Jun 18 13:48:06 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Various HTML extensions                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-extra

   (include "xml.sch")

   (library web)

   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml
	    __hop_html
	    __hop_img
	    __hop_js-lib
	    __hop_hop
	    __hop_user
	    __hop_css
	    __hop_clientc
	    __hop_hz
	    __hop_priv
	    __hop_read
	    __hop_security)

   (export  (<HTML> . ::obj)
	    (<HEAD> . ::obj)
	    (head-parse args)
	    (<FOOT> . ::obj)
	    (<FOOT-BUTTON> . ::obj)

	    (<LINK> . ::obj)
	    (<SCRIPT> . ::obj)
	    (<STYLE> . ::obj)
	    (<INPUT> . ::obj)

	    (<PRAGMA> . ::obj)
	    
	    (<TOOLTIP> . ::obj)
	    (<SORTTABLE> . ::obj)))

;*---------------------------------------------------------------------*/
;*    head-runtime-system-packed ...                                   */
;*---------------------------------------------------------------------*/
(define head-runtime-system-packed #f)

;*---------------------------------------------------------------------*/
;*    head-runtime-system-unpacked ...                                 */
;*---------------------------------------------------------------------*/
(define head-runtime-system-unpacked #f)

;*---------------------------------------------------------------------*/
;*    head-runtime-system-inline ...                                   */
;*---------------------------------------------------------------------*/
(define head-runtime-system-inline #f)

;*---------------------------------------------------------------------*/
;*    <HTML> ...                                                       */
;*---------------------------------------------------------------------*/
(define-xml xml-html #f <HTML>
   ;; the macro define-xml binds attr, init, and body
   (let* ((body (reverse! body))
	  (nbody (cond
		    ((null? body)
		     body)
		    ((not (xml-markup-is? (car body) 'head))
		     (cons (<HEAD>) body))
		    (else
		     body))))
      (instantiate::xml-html
	 (tag 'html)
	 (attributes attr)
	 (body nbody))))
 
;*---------------------------------------------------------------------*/
;*    <HOP-SETUP> ...                                                  */
;*---------------------------------------------------------------------*/
(define (<HOP-SETUP>)
   (<SCRIPT> :type (hop-configure-javascript-mime-type)
      (string-append "
function hop_etc_directory() { return \"" (hop-etc-directory) "\"; }
function hop_bin_directory() { return \"" (hop-bin-directory) "\"; }
function hop_lib_directory() { return \"" (hop-lib-directory) "\"; }
function hop_share_directory() { return \"" (hop-share-directory) "\"; }
function hop_var_directory() { return \"" (hop-var-directory) "\"; }
function hop_contribs_directory() { return \"" (hop-contribs-directory) "\"; }
function hop_weblets_directory() { return \"" (hop-weblets-directory) "\"; }
function hop_debug() { return " (integer->string (bigloo-debug)) "; }
function hop_session() { return " (integer->string (hop-session))) "; }
function hop_realm() { return \"" (hop-realm) "\"; }
"))

;*---------------------------------------------------------------------*/
;*    preload-css ...                                                  */
;*---------------------------------------------------------------------*/
(define (preload-css p base)
   
   (define (preload-http p)
      (with-access::http-request (current-request) (scheme host port)
	 (let ((pref (format "~a://~a:~a"
			     (if (eq? scheme '*) "http" scheme)
			     host port)))
	    (when (substring-at? p pref 0)
	       (preload-css
		(substring p (string-length pref) (string-length p)) #f)))))

   (cond
      ((and (http-request? (current-request))
	    (or (substring-at? p "http://" 0) (substring-at? p "https://" 0)))
       (preload-http p))
      ((and (file-exists? p) (char=? (string-ref p 0) (file-separator)))
       (hop-get-hss p))
      ((and base (>fx (string-length p) 0) (not (char=? (string-ref p 0) #\/)))
       (preload-css (string-append base p) #f))))

;*---------------------------------------------------------------------*/
;*    init-extra! ...                                                  */
;*---------------------------------------------------------------------*/
(define (init-extra!)
   ;; this is used for non-inlined header on common regular browsers
   (unless head-runtime-system-packed
      (let ((hopcss (make-file-name (hop-share-directory) "hop.hss")))
	 ;; force loading to evaluate hop hss types
	 (preload-css hopcss #f)
	 (set! head-runtime-system-packed 
	       (cons* (<HOP-SETUP>)
		      (<LINK> :inline #f
			 :rel "stylesheet"
			 :type (hop-configure-css-mime-type) 
			 :href hopcss)
		      (map (lambda (f)
			      (let ((p (make-file-name (hop-share-directory) f)))
				 (<SCRIPT> :inline #f
				    :type (hop-configure-javascript-mime-type)
				    :src p)))
			   (hop-runtime-system))))
	 ;; this is used for non-inlined header for browsers that restrict
	 ;; size of javascript files (e.g., IE6 on WinCE)
	 (set! head-runtime-system-unpacked
	       (cons* (<HOP-SETUP>)
		      (<LINK> :inline #f
			 :rel "stylesheet"
			 :type (hop-configure-css-mime-type) 
			 :href hopcss)
		      (map (lambda (f)
			      (let ((p (make-file-name (hop-share-directory) f)))
				 (<SCRIPT> :inline #f
				    :type (hop-configure-javascript-mime-type)
				    :src p)))
			   (hop-runtime-system-files))))
	 ;; this is used for inlined headers
	 (set! head-runtime-system-inline
	       (cons* (<HOP-SETUP>)
		      (<LINK> :inline #t
			 :rel "stylesheet"
			 :type (hop-configure-css-mime-type) 
			 :href hopcss)
		      (map (lambda (f)
			      (let ((p (make-file-name (hop-share-directory) f)))
				 (<SCRIPT> :inline #t
				    :type (hop-configure-javascript-mime-type)
				    :src p)))
			   (append (hop-runtime-system)
				   (list "hop-exception.scm"))))))))

;*---------------------------------------------------------------------*/
;*    head-parse ...                                                   */
;*---------------------------------------------------------------------*/
(define (head-parse args)

   (define (absolute-path p dir)
      (cond
	 ((not dir)
	  p)
	 ((=fx (string-length p) 0)
	  p)
	 ((char=? (string-ref p 0) #\/)
	  p)
	 ((and (>fx (string-length p) 8)
	       (substring-at? p "http" 0)
	       (or (substring-at? p "://" 4)
		   (substring-at? p "s://" 4)))
	  p)
	 (else
	  (string-append dir "/" p))))
   
   (define (script p inl)
      (<SCRIPT> :type (hop-javascript-mime-type) :inline inl :src p))
   
   (define (find-head p)
      (with-handler
	 (lambda (e) '())
	 (call-with-input-file p
	    (lambda (in)
	       (let loop ((hd (hop-read in)))
		  (match-case hd
		     ((<HEAD> . ?head) head)
		     ((module (? symbol?) . ?-) (loop (hop-read in)))
		     (else '())))))))

   (define (favicon p inl)
      (<LINK> :rel "shortcut icon" :href p :inline inl))

   (define (css p base inl)
      ;; force pre-loading the hss file in order to force
      ;; pre-evaluating hss type declarations.
      (preload-css p base)
      (<LINK> :inline inl
	 :rel "stylesheet"
	 :type (hop-configure-css-mime-type)
	 :href p))

   (define (hz-get-weblet-info-files path)
      (let ((info (make-file-path path "etc" "weblet.info")))
	 (when (file-exists? info)
	    (let ((l (call-with-input-file info read)))
	       (when (pair? l)
		  (let ((c (assq 'client l)))
		     (when (pair? c)
			(cadr c))))))))
      
   (define (hz-get-files path)
      (or (hz-get-weblet-info-files path)
	  (directory->list path)))

   (define (read-file file)
      (call-with-input-file file
	 (lambda (ip)
	    (display (read-string ip)))))

   (define (hz f path)
      (let* ((url (hz-resolve-name f (cons path (hop-hz-repositories))))
	     (path (hz-download-to-cache url))
	     (base (basename path))
	     (hzfiles (hz-get-files path))
	     (hss (string-append base ".hss"))
	     (jscript1 (string-append base "-client.hop"))
	     (jscript2 (string-append base ".scm")))
	 `(:with-base ,path
		      (,@(if (member hss hzfiles) (list :css hss) '())
			 ,@(apply append
				  (filter-map (lambda (f)
						 (when (string-suffix? ".js" f)
						    (list :jscript
						       (make-file-name path f))))
					      hzfiles))
			 ,@(if (member jscript1 hzfiles)
			       (cons* :jscript jscript1
				  (find-head (make-file-name path jscript1)))
			       '())
			 ,@(if (member jscript2 hzfiles)
			       (cons* :jscript jscript2
				  (find-head (make-file-name path jscript2)))
			       '())))))

   (define (find-incl-dep f path)
      (let* ((path (append path (list (hop-share-directory))))
	     (scm (find-file/path (string-append f ".scm") path)))
	 (if (string? scm)
	     (find-head scm)
	     (let ((hop (find-file/path (string-append f ".hop") path)))
		(if (string? hop)
		    (find-head hop)
		    '())))))

   (define (incl f inl path)
      (let* ((res '())
	     (path (append path (list (hop-share-directory))))
	     (gwf (let* ((f (make-file-name "flash" (string-append f ".swf")))
			 (p (find-file/path f path)))
		     (when (string? p)
			(let ((gw (make-file-path (hop-share-directory)
						  "flash"
						  "JavaScriptFlashGateway.js")))
			   (set! res (cons (script gw inl) res))))))
	     (js (let ((p (find-file/path (string-append f ".js") path)))
		    (when (and (string? p)
			       (or (not (string=? f "dashboard"))
				   (hop-enable-dashboard)))
		       (set! res (cons (script p inl) res)))))
	     (scm (let ((p (find-file/path (string-append f ".scm") path)))
		     (when (string? p)
			(set! res (cons (script p inl) res)))))
	     (hop (unless scm
		     (let ((p (find-file/path (string-append f ".hop") path)))
			(when (string? p)
			   (set! res (cons (script p inl) res))))))
	     (ss (let ((p (find-file/path (string-append f ".css") path)))
		    (when (string? p)
		       (set! res (cons (css p #f inl) res)))))
	     (hss (let ((p (find-file/path (string-append f ".hss") path)))
		     (when (string? p)
			(set! res (cons (css p #f inl) res)))))
	     (uhss (let* ((n (string-append (basename f) ".hss"))
			  (p (make-file-path (hop-rc-directory) (hop-hss-theme) n)))
		      (when (file-exists? p)
			 (set! res (cons (css p #f inl) res))))))
	 (if (null? res)
	     (cond
		((not (file-exists? f))
		 (error '<HEAD> "include file does not exist" f))
		((or (is-suffix? f "hss") (is-suffix? f "css"))
		 (list (css f #f inl)))
		((or (is-suffix? f "scm") (is-suffix? f "hop"))
		 (list (script f inl)))
		((is-suffix? f "js")
		 (list (script f inl)))
		(else
		 (error '<HEAD> "Can't find include file" f)))
	     res)))

   (define incs '())

   (let loop ((a args)
	      (mode #f)
	      (rts #t)
	      (dir #f)
	      (path '())
	      (base #f)
	      (inl #f)
	      (packed #t)
	      (els '()))
      (cond
	 ((null? a)
	  (let ((body (reverse! els)))
	     (if rts
		 (append (cond
			    (inl head-runtime-system-inline)
			    (packed head-runtime-system-packed)
			    (else head-runtime-system-unpacked))
			 body)
		 body)))
	 ((pair? (car a))
	  (loop (append (car a) (cdr a))
		mode rts dir path base inl packed els))
	 ((null? (car a))
	  (loop (cdr a) mode rts dir path base inl packed els))
	 ((keyword? (car a))
	  (if (null? (cdr a))
	      (error '<HEAD> (format "Missing ~a value" (car a)) a)
	      (case (car a)
		 ((:css :jscript :include :hz)
		  (loop (cdr a) (car a) rts dir path base inl packed els))
		 ((:favicon)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts dir path base inl packed 
			    (cons (favicon (absolute-path (cadr a) dir) inl)
				  els))
		      (error '<HEAD> "Illegal :favicon" (cadr a))))
		 ((:rts)
		  (if (boolean? (cadr a))
		      (loop (cddr a) #f (cadr a) dir path base inl packed els)
		      (error '<HEAD> "Illegal :rts" (cadr a))))
		 ((:title)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts dir path base inl packed 
			    (cons (<TITLE> (cadr a)) els))
		      (error '<HEAD> "Illegal :title" (cadr a))))
		 ((:base)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts dir path (cadr a) inl packed
			    (cons (<BASE> :href (cadr a)) els))
		      (error '<HEAD> "Illegal :base" (cadr a))))
		 ((:dir)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts (cadr a) path base inl packed els)
		      (error '<HEAD> "Illegal :dir" (cadr a))))
		 ((:path)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts dir (append! path (list (cadr a))) base
			    inl packed els)
		      (error '<HEAD> "Illegal :path" (cadr a))))
		 ((:inline)
		  (if (or (boolean? (cadr a)) (symbol? (cadr a)))
		      (loop (cddr a) #f rts dir path base (cadr a) packed els)
		      (error '<HEAD> "Illegal :inline" (cadr a))))
		 ((:packed)
		  (if (or (boolean? (cadr a)) (symbol? (cadr a)))
		      (loop (cddr a) #f rts dir path base inl (cadr a) els)
		      (error '<HEAD> "Illegal :inline" (cadr a))))
		 ((:with-base)
		  (if (and (string? (cadr a)) (pair? (cddr a)))
		      (let ((wbels (loop (caddr a) #f #f (cadr a) (list (cadr a)) (cadr a) inl packed '())))
			 (loop (cdddr a) #f rts dir path base inl (cadr a) 
			       (append (reverse! wbels) els)))))
		 (else
		  (error '<HEAD>
			 (format "Unknown ~a argument" (car a))
			 (cadr a))))))
	 ((string? (car a))
	  (case mode
	     ((:css)
	      (loop (cdr a) mode rts dir path base inl packed 
		    (cons (css (absolute-path (car a) dir) base inl) els)))
	     ((:jscript)
	      (loop (cdr a) mode rts dir path base inl packed 
		    (cons (script (absolute-path (car a) dir) inl) els)))
	     ((:include)
	      (cond
		 ((member (car a) incs)
		  (loop (cdr a) mode rts dir path base inl packed els))
		 ((hz-package-filename? (car a))
		  ;; automatic detection of hz package (is it really a
		  ;; good idea since there is the special :hz keyword?)
		  (loop a :include-hz rts dir path base inl packed els))
		 (else
		  (set! incs (cons (car a) incs))
		  (let* ((heads (find-incl-dep (car a) path))
			 (hels (loop heads #f #f dir path base inl packed '()))
			 (iels (incl (car a) inl path)))
		     (loop (cdr a) mode rts dir path base inl packed
			   (append iels (reverse! hels) els))))))
	     ((:hz :include-hz)
	      (set! incs (cons (car a) incs))
	      (let* ((hds (hz (car a) path))
		     (hels (loop hds #f #f dir path base inl packed '())))
		 (loop (cdr a)
		       (if (eq? mode :hz) :hz :include)
		       rts dir path base inl packed
		       (append (reverse! hels) els))))
	     (else
	      (loop (cdr a) #f rts dir path base inl packed
		    (cons (car a) els)))))
	 ((xml-tilde? (car a))
	  (loop (cdr a) :jscript rts dir path base inl packed
		(cons (car a) els)))
	 ((not (car a))
	  (loop (cdr a) #f rts dir path base inl packed els))
	 (else
	  (loop (cdr a) #f rts dir path base inl packed (cons (car a) els))))))

;*---------------------------------------------------------------------*/
;*    <HEAD> ...                                                       */
;*    -------------------------------------------------------------    */
;*    Move the base on top of the HEAD body.                           */
;*---------------------------------------------------------------------*/
(define (<HEAD> . args)
   (init-extra!)
   (let* ((body0 (head-parse args))
	  (ubase (filter (lambda (x)
			    (xml-markup-is? x 'base))
			 body0))
	  (body1 (if (pair? ubase)
		     (cons (car (last-pair ubase))
			   (filter! (lambda (x)
				       (not (xml-markup-is? x 'base)))
				    body0))
		     body0))
	  (body2 (if (not (any? (lambda (x) (xml-markup-is? x 'meta)) body0))
		     (let ((meta (<META> :http-equiv "Content-Type")))
			(cons meta body1))
		     body1)))
      (instantiate::xml-markup
	 (tag 'head)
	 (attributes '())
	 (body body2))))

;*---------------------------------------------------------------------*/
;*    <FOOT> ...                                                       */
;*---------------------------------------------------------------------*/
(define-markup <FOOT> ((id #unspecified string)
		       (class "foot" string)
		       (inline #f)
		       body)
   (<DIV> :id (xml-make-id id 'FOOT)
      :class class
      (<DIV> :align "center"
	 :class "foot-buttons"
	 (<FOOT-BUTTON>
	    :href "http://hop.inria.fr"
	    :title "HOP home page"
	    :inline inline
	    :src "hop.png")
	 body)))

;*---------------------------------------------------------------------*/
;*    <FOOT-BUTTON> ...                                                */
;*---------------------------------------------------------------------*/
(define-markup <FOOT-BUTTON> ((id #unspecified string)
			      (class "foot-button")
			      (href #f string)
			      (title #f string)
			      (path #f)
			      (inline #f)
			      (src #f))
   (<A> :class class
      :href href
      :title title
      (<IMG> :alt title
	 :inline inline
	 :src (cond
		 ((string? path)
		  path)
		 ((string? src)
		  (if (string=? (dirname src) ".")
		      (format "~a/buttons/~a"
			      (url-path-encode (hop-share-directory))
			      src)
		      src))
		 (else
		  (error '<FOOT-BUTTON> "Illegal source" src))))))
				     
;*---------------------------------------------------------------------*/
;*    <TOOLTIP> ...                                                    */
;*---------------------------------------------------------------------*/
(define-markup <TOOLTIP> ((id #unspecified string)
			  (onclick 0)
			  (onmouseout 0)
			  body)
   (<DIV> :id (xml-make-id id 'TOOLTIP)
      :class "hoptooltip"
      :onclick (format "~a; hop_tooltip_hide()" onclick)
      :onmouseout (format "~a; hop_tooltip_hide()" onmouseout)
      body))
	  
;*---------------------------------------------------------------------*/
;*    <SORTTABLE> ...                                                  */
;*    -------------------------------------------------------------    */
;*    See __hop_css for HSS type.                                      */
;*---------------------------------------------------------------------*/
(define-markup <SORTTABLE> ((id #unspecified string)
			    (attributes)
			    body)
   (let ((i (xml-make-id)))
      (<SPAN> :id i 
	 :class "hop-sorttable"
	 (<TABLE> :id (if (string? id) id (xml-make-id 'SORTTABLE))
	    attributes body)
	 (<SCRIPT> (format "hop_sorttable( '~a' )" i)))))

;*---------------------------------------------------------------------*/
;*    LINK ...                                                         */
;*---------------------------------------------------------------------*/
(define-markup <LINK> ((id #unspecified string)
		       (inline #f boolean)
		       (href #f string)
		       (attributes)
		       body)
   
   (define (default href)
      (when (and (string? href) inline)
	 (warning '<LINK> "Cannot inline file -- " href))
      (instantiate::xml-element
	 (tag 'link)
	 (id (xml-make-id id 'link))
	 (attributes `(:href ,href ,@attributes))
	 (body '())))
   
   (define (inl body)
      (let ((c (plist-assq :rel attributes)))
	 (if (not c)
	     (default href)
	     (cond
		((string=? (cadr c) "stylesheet")
		 (apply <STYLE> "\n" body (plist-remq :rel attributes)))
		((string=? (cadr c) "shortcut icon")
		 (instantiate::xml-element
		    (tag 'link)
		    (id (xml-make-id id 'link))
		    (attributes `(:href ,(img-base64-encode href) ,@attributes))
		    (body '())))
		(else
		 (default href))))))

   (if (string-suffix? ".hss" href)
       ;; this is a file that need compilation
       (if (and inline (null? body) (file-exists? href))
	   (let ((req (current-request)))
	      (if (or (not req) (authorized-path? req href))
		  (let ((body (hss->css href)))
		     (if body
			 (inl body)
			 (default (hss->css-url href))))
		  (default (hss->css-url href))))
	   (default (hss->css-url href)))
       ;; this is a plain css file
       (if (and inline (null? body) (file-exists? href))
	   (let ((req (current-request)))
	      (if (or (not req) (authorized-path? req href))
		  (let ((body (with-input-from-file href read-string)))
		     (if body
			 (inl body)
			 (default href)))
		  (user-access-denied req)))
	   (default href))))
   
;*---------------------------------------------------------------------*/
;*    SCRIPT ...                                                       */
;*---------------------------------------------------------------------*/
(define-markup <SCRIPT> ((inline #f boolean)
			 (src #unspecified string)
			 (type (hop-javascript-mime-type) string)
			 (attributes)
			 body)

   (define (purify node)
      (if (>=fx (hop-security) 1)
	  ((hop-security-script-purifier) node)
	  node))
   
   (define (default src)
      (if (string? src)
	  (let ((src (if (member (suffix src) (hop-client-script-suffixes))
			 (clientc-url src)
			 src)))
	     (when inline (warning '<SCRIPT> "Cannot inline file -- " src))
	     (instantiate::xml-cdata
		(tag 'script)
		(attributes `(:src ,src type: ,type ,@attributes))
		(body body)))
	  (instantiate::xml-cdata
	     (tag 'script)
	     (attributes `(:type ,type ,@attributes))
	     (body body))))
   
   (define (inl src)
      (let ((body (if (member (suffix src) (hop-client-script-suffixes))
		      (get-clientc-compiled-file src)
		      (with-input-from-file src read-string))))
	 (if body
	     (instantiate::xml-cdata
		(tag 'script)
		(attributes `(:type ,type ,@attributes))
		(body (list "\n" body)))
	     (default src))))

   (purify
    (if (and inline (string? src))
	(if (file-exists? src)
	    (inl src)
	    (default src))
	(default src))))
 
;*---------------------------------------------------------------------*/
;*    <STYLE> ...                                                      */
;*---------------------------------------------------------------------*/
(define-markup <STYLE> ((type (hop-configure-css-mime-type) string)
			(attributes)
			body)
   (instantiate::xml-cdata
      (tag 'style)
      (attributes `(:type ,type ,@attributes))
      (body body)))

;*---------------------------------------------------------------------*/
;*    <INPUT> ...                                                      */
;*---------------------------------------------------------------------*/
(define-markup <INPUT> ((id #unspecified string)
			(type 'text)
			(onkeydown #f)
			(attributes))
   (if (or (eq? type 'url) (equal? type "url"))
       (let* ((id (xml-make-id id 'input))
	      (comp "hop_inputurl_keydown( this, event )")
	      (onkeydown (if onkeydown
			     (format "~a; ~a" comp
				     (if (xml-tilde? onkeydown)
					 (xml-tilde->statement onkeydown)
					 onkeydown))
			     comp)))
	  (instantiate::xml-empty-element
	     (tag 'input)
	     (id id)
	     (attributes `(:type ,type
				 :onkeydown ,(secure-javascript-attr onkeydown)
				 ,@attributes))
	     (body '())))
       (instantiate::xml-empty-element
	  (tag 'input)
	  (id (xml-make-id id 'input))
	  (attributes `(type: ,type
			      ,@(if onkeydown `(onkeydown: ,(secure-javascript-attr onkeydown)) '())
			      ,@attributes))
	  (body '()))))

;*---------------------------------------------------------------------*/
;*    <PRAGMA> ...                                                     */
;*---------------------------------------------------------------------*/
(define (<PRAGMA> . obj)
   (cond
      ((and (pair? obj) (null? (cdr obj)) (string? (car obj)))
       (instantiate::xml-verbatim (body (car obj))))
      ((every? string? obj)
       (instantiate::xml-verbatim (body (apply string-append obj))))
      (else
       (error '<PRAGMA> "Illegal arguments" obj))))
