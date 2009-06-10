;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/hop-extra.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 14 05:36:34 2005                          */
;*    Last change :  Wed Jun 10 08:58:48 2009 (serrano)                */
;*    Copyright   :  2005-09 Manuel Serrano                            */
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
	    __hop_img
	    __hop_js-lib
	    __hop_hop
	    __hop_user
	    __hop_css
	    __hop_clientc
	    __hop_hop-file
	    __hop_hz)

   (export  (<HTML> . ::obj)
	    (<HEAD> . ::obj)
	    (head-parse args)
	    (<FOOT> . ::obj)
	    (<FOOT-BUTTON> . ::obj)

	    (<LINK> . ::obj)
	    (<SCRIPT> . ::obj)
	    (<STYLE> . ::obj)
	    (<INPUT> . ::obj)
	    
	    (<TOOLTIP> . ::obj)
	    (<SORTTABLE> . ::obj)

	    (<LFRAME> . ::obj)
	    (<LFLABEL> . ::obj)))

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
	 (markup 'html)
	 (attributes attr)
	 (initializations init)
	 (body nbody))))

;*---------------------------------------------------------------------*/
;*    <HOP-SETUP> ...                                                  */
;*---------------------------------------------------------------------*/
(define (<HOP-SETUP>)
   (<SCRIPT> :type (hop-configure-javascript-mime-type)
      (string-append
       "function hop_etc_directory() { return \"" (hop-etc-directory) "\"; }
function hop_bin_directory() { return \"" (hop-bin-directory) "\"; }
function hop_lib_directory() { return \"" (hop-lib-directory) "\"; }
function hop_share_directory() { return \"" (hop-share-directory) "\"; }
function hop_var_directory() { return \"" (hop-var-directory) "\"; }
function hop_contribs_directory() { return \"" (hop-contribs-directory) "\"; }
function hop_weblets_directory() { return \"" (hop-weblets-directory) "\"; }
function hop_debug() { return " (integer->string (bigloo-debug)) "; }")))

;*---------------------------------------------------------------------*/
;*    init-extra! ...                                                  */
;*---------------------------------------------------------------------*/
(define (init-extra!)
   ;; this is used for non-inlined header on common regular browsers
   (unless head-runtime-system-packed
      (let ((hopcss (make-file-name (hop-share-directory) "hop.css")))
	 (set! head-runtime-system-packed 
	       (cons* (<LINK> :inline #f
			 :rel "stylesheet"
			 :type (hop-configure-css-mime-type) 
			 :href hopcss)
		      (<HOP-SETUP>)
		      (map (lambda (f)
			      (let ((p (make-file-name (hop-share-directory) f)))
				 (<SCRIPT> :inline #f
				    :type (hop-configure-javascript-mime-type)
				    :src p)))
			   (append (hop-runtime-system)
				   (list "hop-exception.scm")))))
	 ;; this is used for non-inlined header for browsers that restrict
	 ;; size of javascript files (e.g., IE6 on WinCE)
	 (set! head-runtime-system-unpacked
	       (cons* (<LINK> :inline #f
			 :rel "stylesheet"
			 :type (hop-configure-css-mime-type) 
			 :href hopcss)
		      (<HOP-SETUP>)
		      (map (lambda (f)
			      (let ((p (make-file-name (hop-share-directory) f)))
				 (<SCRIPT> :inline #f
				    :type (hop-configure-javascript-mime-type)
				    :src p)))
			   (append (hop-runtime-system-files)
				   (list "hop-exception.scm")))))
	 ;; this is used for inlined headers
	 (set! head-runtime-system-inline
	       (cons* (<LINK> :inline #t
			 :rel "stylesheet"
			 :type (hop-configure-css-mime-type) 
			 :href hopcss)
		      (<HOP-SETUP>)
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
   
   (define (favicon p inl)
      (<LINK> :rel "shortcut icon" :href p :inline inl))
   
   (define (css p inl)
      (<LINK> :inline inl
	 :rel "stylesheet"
	 :type (hop-configure-css-mime-type)
	 :href p))

   (define (hz-get-weblet-info-files path)
      (let ((hop (make-file-path path "etc" "weblet.info")))
	 (when (file-exists? hop)
	    (let ((l (call-with-input-file hop read)))
	       (when (pair? l)
		  (let ((c (assq 'api l)))
		     (when (pair? c)
			(cadr c))))))))
      
   (define (hz-get-files path suffix)
      (filter-map (lambda (f)
		     (when (is-suffix? f suffix)
			(make-file-name path f)))
		  (or (hz-get-weblet-info-files path)
		      (directory->list path))))

   (define (read-file file)
      (call-with-input-file file
	 (lambda (ip)
	    (display (read-string ip)))))
   
   (define (hz->client path suffix file-to-string)
      (let ((file (make-file-name path (string-append ".hop." suffix))))
	 (if (file-exists? file)
	     file
	     (let ((files (hz-get-files path suffix)))
		(when (pair? files)
		   (with-output-to-file file
		      (lambda ()
			 (for-each file-to-string files)))
		   file)))))
   
   (define (hz f inl)
      ;; hss file be all merged into a single file, jscript files
      ;; have to be compiled separatly.
      (let* ((path (hz-download-to-cache f))
	     (hss (hz->client path "hss" read-file))
	     (jscript1 (hz->client path "hop" get-clientc-compiled-file))
	     (jscript2 (hz->client path "js" read-file))
	     (res '()))
	 (when hss
	    (set! res (cons (css hss inl) res)))
	 (when jscript1
	    (set! res (cons (script jscript1 inl) res)))
	 (when jscript2
	    (set! res (cons (script jscript2 inl) res)))
	 res))
   
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
		       (set! res (cons (css p inl) res)))))
	     (hss (let ((p (find-file/path (string-append f ".hss") path)))
		     (when (string? p)
			(set! res (cons (css p inl) res))))))
	 (if (null? res)
	     (error '<HEAD> "Can't find include file" f)
	     res)))

   (let loop ((a args)
	      (mode #f)
	      (rts #t)
	      (dir #f)
	      (path '())
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
	  (loop (append (car a) (cdr a)) mode rts dir path inl packed els))
	 ((null? (car a))
	  (loop (cdr a) mode rts dir path inl packed els))
	 ((keyword? (car a))
	  (if (null? (cdr a))
	      (error '<HEAD> (format "Missing ~a value" (car a)) a)
	      (case (car a)
		 ((:css)
		  (cond
		     ((string? (cadr a))
		      (loop (cddr a) :css rts dir path inl packed
			    (cons (css (absolute-path (cadr a) dir) inl) els)))
		     ((list? (cadr a))
		      (let ((css-files (map (lambda (f)
					       (css (absolute-path f dir) inl))
					    (cadr a))))
			 (loop (cddr a) :css rts dir path inl packed
			       (append! (reverse! css-files) els))))
		     ((not (cadr a))
		      (loop (cddr a) :css rts dir path inl packed els))
		     (else
		      (error '<HEAD> "Illegal :css" (cadr a)))))
		 ((:jscript)
		  (cond
		     ((string? (cadr a))
		      (loop (cddr a) :jscript rts dir path inl packed
			    (cons (script (absolute-path (cadr a) dir) inl)
				  els)))
		     ((xml-tilde? (cadr a))
		      (loop (cddr a) :jscript rts dir path inl packed
			    (cons (cadr a) els)))
		     ((list? (cadr a))
		      (let ((js-files (map (lambda (f)
					      (script (absolute-path f dir)
						      inl))
					   (cadr a))))
			 (loop (cddr a) :jscript rts dir path inl packed
			       (append! (reverse! js-files) els))))
		     ((not (cadr a))
		      (loop (cddr a) :jscript rts dir path inl packed els))
		     (else
		      (error '<HEAD> "Illegal :jscript" (cadr a)))))
		 ((:favicon)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts dir path inl packed
			    (cons (favicon (absolute-path (cadr a) dir) inl)
				  els))
		      (error '<HEAD> "Illegal :favicon" (cadr a))))
		 ((:include)
		  (cond
		     ((string? (cadr a))
		      ;; automatic detection of hz package (it is really
		      ;; a good idea since there is the special :hz keyword)
		      (if (hz-package-filename? (cadr a))
			  (loop (cddr a) :include rts dir path inl packed
				(append (hz (cadr a) inl) els))
			  (loop (cddr a) :include rts dir path inl packed
				(append (incl (cadr a) inl path) els))))
		     ((list? (cadr a))
		      (loop (cddr a) :include rts dir path inl packed
			    (append (reverse!
				     (append-map (lambda (i)
						    (incl i inl path))
						 (cadr a)))
				    els)))
		     ((not (cadr a))
		      (loop (cddr a) :include rts dir path inl packed els))
		     (else
		      (error '<HEAD> "Illegal :include" (cadr a)))))
		 ((:hz)
		  (if (string? (cadr a))
		      (loop (cddr a) :include rts dir path inl packed
			    (append (hz (cadr a) inl) els))
		      (error '<HEAD> "Illegal :hz" (cadr a))))
		 ((:rts)
		  (if (boolean? (cadr a))
		      (loop (cddr a) #f (cadr a) dir path inl packed els)
		      (error '<HEAD> "Illegal :rts" (cadr a))))
		 ((:title)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts dir path inl packed
			    (cons (<TITLE> (cadr a)) els))
		      (error '<HEAD> "Illegal :title" (cadr a))))
		 ((:base)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts dir path inl packed
			    (cons (<BASE> :href (cadr a)) els))
		      (error '<HEAD> "Illegal :base" (cadr a))))
		 ((:dir)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts (cadr a) path inl packed els)
		      (error '<HEAD> "Illegal :dir" (cadr a))))
		 ((:path)
		  (if (string? (cadr a))
		      (loop (cddr a) #f rts dir (append! path (list (cadr a)))
			    inl packed els)
		      (error '<HEAD> "Illegal :path" (cadr a))))
		 ((:inline)
		  (if (or (boolean? (cadr a)) (symbol? (cadr a)))
		      (loop (cddr a) #f rts dir path (cadr a) packed els)
		      (error '<HEAD> "Illegal :inline" (cadr a))))
		 ((:packed)
		  (if (or (boolean? (cadr a)) (symbol? (cadr a)))
		      (loop (cddr a) #f rts dir path inl (cadr a) els)
		      (error '<HEAD> "Illegal :inline" (cadr a))))
		 (else
		  (error '<HEAD>
			 (format "Unknown ~a argument" (car a))
			 (cadr a))))))
	 ((string? (car a))
	  (case mode
	     ((:css)
	      (loop (cdr a) mode rts dir path inl packed
		    (cons (css (absolute-path (car a) dir) inl) els)))
	     ((:jscript)
	      (loop (cdr a) mode rts dir path inl packed
		    (cons (script (absolute-path (car a) dir) inl) els)))
	     ((:include)
	      (loop (cdr a) mode rts dir path inl packed
		    (append (incl (car a) inl path) els)))
	     (else
	      (loop (cdr a) #f rts dir path inl packed
		    (cons (car a) els)))))
	 ((not (car a))
	  (loop (cdr a) #f rts dir path inl packed els))
	 (else
	  (loop (cdr a) #f rts dir path inl packed (cons (car a) els))))))

;*---------------------------------------------------------------------*/
;*    <HEAD> ...                                                       */
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
	  (body2 (if (not (xml-markup-is? (car body0) 'meta))
		     (let ((meta (<META> :http-equiv "Content-Type")))
			(cons meta body1))
		     body1)))
      (instantiate::xml-markup
	 (markup 'head)
	 (attributes '())
	 (body body2))))

;*---------------------------------------------------------------------*/
;*    <FOOT> ...                                                       */
;*---------------------------------------------------------------------*/
(define-xml-compound <FOOT> ((id #unspecified string)
			     (class "foot" string)
			     (inline #f)
			     body)
   (<DIV> :id (xml-make-id id 'FOOT)
      :class class
      (<DIV>
	 :align "center"
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
(define-xml-compound <FOOT-BUTTON> ((id #unspecified string)
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
(define-xml-compound <TOOLTIP> ((id #unspecified string)
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
(define-xml-compound <SORTTABLE> ((id #unspecified string)
				  (attributes)
				  body)
   (let ((i (xml-make-id))
	 (attr (map! (lambda (e)
			(list (symbol->keyword (car e)) (cdr e)))
		     attributes)))
      (<SPAN> :id i 
	 :class "hop-sorttable"
	 (apply <TABLE> :id (if (string? id) id (xml-make-id 'SORTTABLE))
		(append! attr body))
	 (<SCRIPT> (format "hop_sorttable( '~a' )" i)))))

;*---------------------------------------------------------------------*/
;*    LINK ...                                                         */
;*---------------------------------------------------------------------*/
(define-xml-compound <LINK> ((id #unspecified string)
			     (inline #f boolean)
			     (href #f string)
			     (attributes)
			     body)
   
   (define (default href)
      (when (and (string? href) inline)
	 (warning '<LINK> "Cannot inline file -- " href))
      (instantiate::xml-element
	 (markup 'link)
	 (id (xml-make-id id 'link))
	 (attributes (cons `(href . ,href) attributes))
	 (body '())))
   
   (define (inl body)
      (let ((c (assq 'rel attributes)))
	 (if (not (pair? c))
	     (default href)
	     (cond
		((string=? (cdr c) "stylesheet")
		 (apply <STYLE> "\n" body
			(append-map (lambda (x)
				       (list (symbol->keyword (car x)) (cdr x)))
				    (remq c attributes))))
		((string=? (cdr c) "shortcut icon")
		 (instantiate::xml-element
		    (markup 'link)
		    (id (xml-make-id id 'link))
		    (attributes (cons `(href . ,(img-base64-encode href))
				      attributes))
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
(define-xml-compound <SCRIPT> ((inline #f boolean)
			       (src #unspecified string)
			       (type (hop-javascript-mime-type) string)
			       (attributes)
			       body)

   (define (default src)
      (if (string? src)
	  (let ((src (if (member (suffix src) (hop-client-script-suffixes))
			 (clientc-url src)
			 src)))
	     (when inline (warning '<SCRIPT> "Cannot inline file -- " src))
	     (instantiate::xml-cdata
		(markup 'script)
		(attributes (cons* `(src . ,src) `(type . ,type) attributes))
		(body body)))
	  (instantiate::xml-cdata
	     (markup 'script)
	     (attributes (cons `(type . ,type) attributes))
	     (body body))))
   
   (define (inl src)
      (let ((body (if (member (suffix src) (hop-client-script-suffixes))
		      (get-clientc-compiled-file src)
		      (with-input-from-file src read-string))))
	 (if body
	     (instantiate::xml-cdata
		(markup 'script)
		(attributes (cons `(type . ,type) attributes))
		(body (list "\n" body)))
	     (default src))))

   (if (and inline (string? src))
       (if (file-exists? src)
	   (inl src)
	   (default src))
       (default src)))
 
;*---------------------------------------------------------------------*/
;*    <STYLE> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <STYLE> ((type (hop-configure-css-mime-type) string)
			      (attributes)
			      body)
   (instantiate::xml-cdata
      (markup 'style)
      (attributes (cons `(type . ,type) attributes))
      (body body)))

;*---------------------------------------------------------------------*/
;*    <INPUT> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <INPUT> ((id #unspecified string)
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
	     (markup 'input)
	     (id id)
	     (attributes `((type . text)
			   (onkeydown . ,onkeydown)
			   ,@attributes))
	     (body '())))
       (instantiate::xml-empty-element
	  (markup 'input)
	  (id (xml-make-id id 'input))
	  (attributes `((type . ,type)
			,@(if onkeydown `((onkeydown . ,onkeydown)) '())
			,@attributes))
	  (body '()))))

;*---------------------------------------------------------------------*/
;*    <LFRAME> ...                                                     */
;*---------------------------------------------------------------------*/
(define (<LFRAME> . body)
   (<DIV> :hssclass "hop-lframe"
      (<DIV> :hssclass "hop-lfborder"
	 body)))

;*---------------------------------------------------------------------*/
;*    <LFLABEL> ...                                                    */
;*---------------------------------------------------------------------*/
(define (<LFLABEL> . body)
   (<DIV> :hssclass "hop-lflabel"
      (<SPAN> body)))



