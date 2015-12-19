;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/html_head.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 14 05:36:34 2005                          */
;*    Last change :  Fri Dec 18 08:28:14 2015 (serrano)                */
;*    Copyright   :  2005-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Various HTML extensions                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-head

   (include "xml.sch"
	    "service.sch")

   (library web)

   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml-types
	    __hop_xml
	    __hop_dom
	    __hop_html-base
	    __hop_html-img
	    __hop_js-comp
	    __hop_hop
	    __hop_user
	    __hop_css
	    __hop_clientc
	    __hop_hz
	    __hop_weblets
	    __hop_priv
	    __hop_read
	    __hop_service
	    __hop_security)

   (export  (<HTML> . ::obj)
	    (<HEAD> . ::obj)
	    (head-parse args)

	    (<LINK> . ::obj)
	    (<SCRIPT> . ::obj)
	    (<STYLE> . ::obj)))


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
;*    head-runtime-favicon ...                                         */
;*---------------------------------------------------------------------*/
(define head-runtime-favicon #f)

;*---------------------------------------------------------------------*/
;*    <HTML> ...                                                       */
;*---------------------------------------------------------------------*/
(define-tag <HTML> ((idiom "scheme")
		    (context #f)
		    (%location #f)
		    (attr)
		    body)
   (let* ((nbody (let loop ((body body))
		    (cond
		       ((not (pair? body))
			body)
		       ((xml-unpack (car body))
			=>
			(lambda (v) (loop (append v (cdr body)))))
		       ((let ((v (xml-primitive-value (car body))))
			   (and (string? v) (not (string-skip v "\n\t "))))
			(loop (cdr body)))
		       (else
			(append-map (lambda (n) (or (xml-unpack n) (list n)))
			   body)))))
	  (hbody (cond
		    ((null? nbody)
		     (list (<HEAD> :idiom idiom :context context
			      :%location %location)))
		    ((xml-markup-is? (car nbody) 'head)
		     nbody)
		    ((find (lambda (o) (xml-markup-is? o 'head)) body)
		     =>
		     (lambda (n)
			(error "<HTML>" "wrong <HEAD> element" n)))
		    (else
		     (cons (<HEAD> :idiom idiom :context context
			      :%location %location)
			nbody)))))
      (instantiate::xml-html
	 (tag 'html)
	 (attributes attr)
	 (body hbody))))
 
;*---------------------------------------------------------------------*/
;*    <HOP-SETUP> ...                                                  */
;*---------------------------------------------------------------------*/
(define (<HOP-SETUP>)
   (<SCRIPT> :type (hop-mime-type)
      (string-append "
function hop_etc_directory() {return \"" (hop-etc-directory) "\";}
function hop_bin_directory() {return \"" (hop-bin-directory) "\";}
function hop_lib_directory() {return \"" (hop-lib-directory) "\";}
function hop_share_directory() {return \"" (hop-share-directory) "\";}
function hop_var_directory() {return \"" (hop-var-directory) "\";}
function hop_contribs_directory() {return \"" (hop-contribs-directory) "\";}
function hop_weblets_directory() {return \"" (hop-weblets-directory) "\";}
function hop_debug() {return " (integer->string (bigloo-debug)) ";}
function hop_session() {return " (integer->string (hop-session)) ";}
function hop_realm() {return \"" (hop-realm) "\";}")))

;*---------------------------------------------------------------------*/
;*    server-initial-context ...                                       */
;*---------------------------------------------------------------------*/
(define (server-initial-context location stack)

   (let* ((stk (filter-map (lambda (f)
			      (match-case f
				 ((?name ?loc . ?rest)
				  `(,name ,loc (type . server) (format . "$~a")))
				 (else
				  f)))
		  stack))
	  (loc (match-case (xml-primitive-value location)
		  ((or (:filename ?fname :pos ?pos :name ?name)
		       (:name ?name :pos ?pos :filename ?fname))
		   `(,(xml-primitive-value name)
		       (at ,(xml-primitive-value fname)
			  ,(xml-primitive-value pos))
		       (type . server) (format . "$~a")))
		  ((?name ?loc . ?rest)
		   `(,name ,loc (type . server) (format . "$~a")))
		  (else
		   #f)))
	  (ctx (if loc (cons loc (cdr stk)) stk)))
      (string-append "hop_initial_stack_context = hop_url_encoded_to_obj('"
	 (url-path-encode (obj->string ctx))
	 "');")))
      
;*---------------------------------------------------------------------*/
;*    <HOP-SERVER> ...                                                 */
;*---------------------------------------------------------------------*/
(define (<HOP-SERVER>)
   (<SCRIPT> :type (hop-mime-type)
      (string-append "var hop_server = new HopServer(\""
	 (hop-server-hostname) "\", undefined, \""
	 (hop-version)
	 "\");var server = hop_server;")))

;*---------------------------------------------------------------------*/
;*    preload-css ...                                                  */
;*---------------------------------------------------------------------*/
(define (preload-css p::bstring base::obj)
   (cond
      ((and (file-exists? p) (char=? (string-ref p 0) (file-separator)))
       (hop-get-hss p)
       #unspecified)
      ((and base (>fx (string-length p) 0) (not (char=? (string-ref p 0) #\/)))
       (preload-css (string-append base p) #f))))

;*---------------------------------------------------------------------*/
;*    init-head! ...                                                   */
;*---------------------------------------------------------------------*/
(define (init-head!)
   ;; this is used for non-inlined header on common regular browsers
   (unless head-runtime-system-packed
      (let* ((hopcss (make-file-path (hop-share-directory) "hop.hss"))
	     (suffix (if (=fx (bigloo-debug) 0) "_u.js" "_s.js"))
	     (rts (map (lambda (s) (string-append s suffix))
		     (hop-runtime-system)))
	     (favicon (make-file-path (hop-share-directory)
			 "icons" "hop" "hop-16x16.png")))
	 ;; favicon to avoid /favicon.ico authentication
	 (set! head-runtime-favicon (<LINK> :rel "shortcut icon" :href favicon))
	 ;; force loading to evaluate hop hss types
	 (preload-css hopcss #f)
	 (set! head-runtime-system-packed 
	    (cons* (<HOP-SETUP>)
	       
	       (<LINK> :inline #f
		  :rel "stylesheet"
		  :type (hop-configure-css-mime-type) 
		  :href hopcss)
	       (append
		  (map (lambda (f)
			  (let ((p (make-file-name (hop-share-directory) f)))
			     (<SCRIPT> :inline #f
				:type (hop-mime-type)
				:src p)))
		     (append rts (hop-runtime-extra)))
		  (list (<HOP-SERVER>)))))
	 ;; this is used for non-inlined header for browsers that restrict
	 ;; size of javascript files (e.g., IE6 on WinCE)
	 (set! head-runtime-system-unpacked
	    (cons* (<HOP-SETUP>)
	       (<LINK> :inline #f
		  :rel "stylesheet"
		  :type (hop-configure-css-mime-type) 
		  :href hopcss)
	       (append
		  (map (lambda (f)
			  (let ((p (make-file-name (hop-share-directory) f)))
			     (<SCRIPT> :inline #f
				:type (hop-mime-type)
				:src p)))
		     (append (hop-runtime-system-files)
			(hop-runtime-extra)))
		  (list (<HOP-SERVER>)))))
	 ;; this is used for inlined headers
	 (set! head-runtime-system-inline
	    (cons* (<HOP-SETUP>)
	       (<LINK> :inline #t
		  :rel "stylesheet"
		  :type (hop-configure-css-mime-type) 
		  :href hopcss)
	       (append
		  (map (lambda (f)
			  (let ((p (make-file-name (hop-share-directory) f)))
			     (<SCRIPT> :inline #t
				:type (hop-mime-type)
				:src p)))
		     (append rts (hop-runtime-extra)))
		  (list (<HOP-SERVER>))))))))

;*---------------------------------------------------------------------*/
;*    library-path ...                                                 */
;*---------------------------------------------------------------------*/
(define (library-path)
   (let ((venv (getenv "BIGLOOLIB")))
      (if (not venv)
	  (bigloo-library-path)
	  (cons "." (unix-path->list venv)))))

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
      (<SCRIPT> :type (hop-mime-type)
	 :inline inl :src p))
   
   (define (require p m inl)
      (<REQUIRE> :type (hop-mime-type)
	 :inline inl :src p :mod m))
   
   (define (find-head p)
      (call-with-input-file p
	 (lambda (in)
	    (let loop ((hd (hop-read in)))
	       (if (eof-object? hd)
		   '()
		   (match-case hd
		      ((<HEAD> . ?head) head)
		      ((module (? symbol?) . ?-) (loop (hop-read in)))
		      (else '())))))))
   
   (define (favicon p inl)
      (<LINK> :rel "shortcut icon" :href p :inline inl))
   
   (define (css p base inl)
      ;; force pre-loading the hss file in order to force
      ;; pre-evaluating hss type declarations.
      (when (string-suffix? ".hss" p)
	 (preload-css p base))
      (<LINK> :inline inl
	 :rel "stylesheet"
	 :type (hop-configure-css-mime-type)
	 :href p))
   
   (define (hz-get-weblet-info-files path)
      (let* ((l (get-weblet-info path))
	     (c (assq 'client l)))
	 (when (pair? c) (cadr c))))
   
   (define (hz-get-files path)
      (or (hz-get-weblet-info-files path)
	  (directory->list path)))
   
   (define (read-file file)
      (call-with-input-file file
	 (lambda (ip)
	    (display (read-string ip)))))
   
   (define (hz-path f path)
      (or (hz-local-weblet-path f (get-autoload-directories))
	  (hz-cache-path f)
	  (hz-download-to-cache f (cons path (hop-hz-repositories)))))
   
   (define (hz f path)
      (let* ((path (hz-path f path))
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
		 (error "<HEAD>" (format "Can't find include ~s in path" f)
		    path))
		((or (is-suffix? f "hss") (is-suffix? f "css"))
		 (list (css f #f inl)))
		((or (is-suffix? f "scm") (is-suffix? f "hop"))
		 (list (script f inl)))
		((is-suffix? f "js")
		 (list (script f inl)))
		(else
		 (error "<HEAD>"
		    (format "Can't find include ~s in path" f)
		    path)))
	     res)))
   
   (define incs '())

   (define idiom "scheme")

   (define context #f)

   (define favico #f)

   (define location #f)

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
	  (let* ((els (if favico els (cons head-runtime-favicon els)))
		 (body (reverse! els)))
	     (if rts
		 (begin
		    (init-head!)
		    (append (cond
			       (inl head-runtime-system-inline)
			       (packed head-runtime-system-packed)
			       (else head-runtime-system-unpacked))
		       (cons
			  (<SCRIPT> :type (hop-mime-type)
			     (string-append "function hop_idiom() { return '"
				idiom "'}\n")
			     (when (>fx (bigloo-debug) 0)
				(server-initial-context location
				   (get-trace-stack))))
			  body)))
		 body)))
	 ((pair? (car a))
	  (loop (append (car a) (cdr a))
		mode rts dir path base inl packed els))
	 ((or (null? (car a)) (not (car a)))
	  (loop (cdr a) mode rts dir path base inl packed els))
	 ((keyword? (car a))
	  (if (null? (cdr a))
	      (error "<HEAD>" (format "Missing ~a value" (car a)) a)
	      (case (car a)
		 ((:css :jscript :script :require :module :include :hz :library)
		  (loop (cdr a) (car a) rts dir path base inl packed els))
		 ((:favicon)
		  (set! favico #t)
		  (let ((v (xml-primitive-value (cadr a))))
		     (if (string? v)
			 (loop (cddr a) #f rts dir path base inl packed 
			    (cons (favicon (absolute-path v dir) inl)
			       els))
			 (error "<HEAD>" "Illegal :favicon" (cadr a)))))
		 ((:rts)
		  (if (boolean? (cadr a))
		      (loop (cddr a) #f (cadr a) dir path base inl packed els)
		      (error "<HEAD>" "Illegal :rts" (cadr a))))
		 ((:title)
		  (let ((v (xml-primitive-value (cadr a))))
		     (if (string? v)
			 (loop (cddr a) #f rts dir path base inl packed 
			    (cons (<TITLE> v) els))
			 (error "<HEAD>" "Illegal :title" (cadr a)))))
		 ((:base)
		  (let ((v (xml-primitive-value (cadr a))))
		     (if (string? (cadr a))
			 (loop (cddr a) #f rts dir path v inl packed
			    (cons (<BASE> :href v) els))
			 (error "<HEAD>" "Illegal :base" (cadr a)))))
		 ((:dir)
		  (let ((v (xml-primitive-value (cadr a))))
		     (if (string? v)
			 (loop (cddr a) #f rts v path base inl packed els)
			 (error "<HEAD>" "Illegal :dir" v))))
		 ((:path)
		  (let ((v (xml-primitive-value (cadr a))))
		     (if (string? v)
			 (loop (cddr a) #f rts dir (append! path (list v))
			    base inl packed els)
			 (error "<HEAD>" "Illegal :path" v))))
		 ((:context)
		  (set! context (cadr a))
		  (loop (cddr a) #f rts dir path base inl packed els))
		 ((:inline)
		  (if (or (boolean? (cadr a)) (symbol? (cadr a)))
		      (loop (cddr a) #f rts dir path base (cadr a) packed els)
		      (error "<HEAD>" "Illegal :inline" (cadr a))))
		 ((:packed)
		  (if (or (boolean? (cadr a)) (symbol? (cadr a)))
		      (loop (cddr a) #f rts dir path base inl (cadr a) els)
		      (error "<HEAD>" "Illegal :inline" (cadr a))))
		 ((:with-base)
		  (let ((v (xml-primitive-value (cadr a))))
		     (if (and (string? v) (pair? (cddr a)))
			 (let ((wbels (loop (caddr a) #f #f v (list v)
					 v inl packed '())))
			    (loop (cdddr a) #f rts dir path base inl v 
			       (append (reverse! wbels) els)))
			 (error "<HEAD>" "Illegal :with-base argument" (cadr a)))))
		 ((:idiom)
		  (let ((v (xml-primitive-value (cadr a))))
		     (if (string? v)
			 (begin
			    (set! idiom v)
			    (loop (cddr a) #f rts dir path base inl packed els))
			 (error "<HEAD>" "Illegal :idiom argument" (cadr a)))))
		 ((:%location)
		  (set! location (xml-primitive-value (cadr a)))
		  (loop (cddr a) mode rts dir path base inl packed els))
		 (else
		  (error "<HEAD>"
		     (format "Unknown ~a argument" (car a))
		     (cadr a))))))
	 ((string? (car a))
	  (case mode
	     ((:css)
	      (let ((file (or (find-file/path (car a) path) (car a))))
		 (loop (cdr a) mode rts dir path base inl packed 
		       (cons (css (absolute-path file dir) base inl) els))))
	     ((:jscript :script)
	      (let ((file (or (find-file/path (car a) path) (car a))))
		 (loop (cdr a) mode rts dir path base inl packed 
		       (cons (script (absolute-path file dir) inl) els))))
	     ((:require :module)
	      (let* ((v (xml-primitive-value (car a)))
		     (file (clientc-resolve-filename v (or context path))))
		 (if (not file)
		     (error "<HEAD>" "Cannot find required file" file)
		     (loop (cdr a) mode rts dir path base inl packed 
			(cons (require file v inl) els)))))
	     ((:library)
	      (let ((file (find-file/path (car a) (library-path))))
		 (if (not (string? file))
		     (error "<HEAD>" "Cannot find library file" (car a))
		     (loop (cdr a) mode rts dir path base inl packed 
			(cons (script file inl) els)))))
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
	 ((isa? (car a) xml-tilde)
	  (loop (cdr a) :jscript rts dir path base inl packed
		(cons (car a) els)))
	 (else
	  (let ((v (xml-primitive-value (car a))))
	     (if (eq? v (car a))
		 (loop (cdr a) #f rts dir path base inl packed (cons (car a) els))
		 (loop (cons v (cdr a)) mode rts dir path base inl packed els)))))))

;*---------------------------------------------------------------------*/
;*    <HEAD> ...                                                       */
;*    -------------------------------------------------------------    */
;*    Move the base on top of the HEAD body.                           */
;*---------------------------------------------------------------------*/
(define (<HEAD> . args)
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
	  (body2 (if (not (any (lambda (x)
				  (and (xml-markup-is? x 'meta)
				       (dom-get-attribute x "http-equiv")))
			     body0))
		     (let ((meta (<META> :http-equiv "Content-Type"
				    :content #t)))
			(cons meta body1))
		     body1)))
      (instantiate::xml-markup
	 (tag 'head)
	 (attributes '())
	 (body body2))))

;*---------------------------------------------------------------------*/
;*    LINK ...                                                         */
;*---------------------------------------------------------------------*/
(define-tag <LINK> ((id #unspecified)
		    (inline #f boolean)
		    (href #f)
		    (attributes)
		    body)
   
   (define (default href)
      (when (and (string? href) inline)
	 (warning "<LINK>" "Cannot inline file -- " href))
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
   
   (let ((href (xml-primitive-value href)))
      (cond
	 ((not (string? href))
	  (bigloo-type-error "<LINK>" "string" href))
	 ((string-suffix? ".hss" href)
	  ;; this is a file that needs compilation
	  (if (and inline (null? body) (file-exists? href))
	      (let ((body (hss->css href)))
		 (if body
		     (inl body)
		     (default (hss->css-url href))))
	      (default (hss->css-url href))))
	 (else
	  ;; this is a plain css file
	  (if (and inline (null? body) (file-exists? href))
	      (let ((body (with-input-from-file href read-string)))
		 (if body
		     (inl body)
		     (default href)))
	      (default href))))))
   
;*---------------------------------------------------------------------*/
;*    SCRIPT ...                                                       */
;*---------------------------------------------------------------------*/
(define-tag <SCRIPT> ((inline #f boolean)
		      (src #unspecified)
		      (type (hop-mime-type))
		      (attributes)
		      body)
   
   (define (purify node)
      (if (>=fx (hop-security) 2)
	  (let ((sm (hop-security-manager)))
	     (with-access::security-manager sm (script-sanitize)
		(script-sanitize node)))
	  node))
   
   (define (default src)
      (if (string? src)
	  (let ((src (cond
			((member (suffix src) (hop-client-script-suffixes))
			 (string-append src "?" (hop-scm-compile-suffix)))
			(else
			 src))))
	     (when inline (warning "<SCRIPT>" "Cannot inline file -- " src))
	     (instantiate::xml-cdata
		(tag 'script)
		(attributes `(:src ,src type: ,type ,@attributes))
		(body body)))
	  (instantiate::xml-cdata
	     (tag 'script)
	     (attributes `(:type ,type ,@attributes))
	     (body body))))
   
   (define (inl src)
      (let ((body (cond
		     ((member (suffix src) (hop-client-script-suffixes))
		      (get-clientc-compiled-file src src))
		     (else
		      (with-input-from-file src read-string)))))
	 (if body
	     (instantiate::xml-cdata
		(tag 'script)
		(attributes `(:type ,type ,@attributes))
		(body (list "\n" body)))
	     (default src))))
   
   (let ((src (xml-primitive-value src))
	 (type (xml-primitive-value type)))
      (purify
	 (if (and inline (string? src))
	     (if (file-exists? src)
		 (inl src)
		 (default src))
	     (default src)))))
 
;*---------------------------------------------------------------------*/
;*    REQUIRE ...                                                      */
;*---------------------------------------------------------------------*/
(define-tag <REQUIRE> ((inline #f boolean)
		       (src #unspecified)
		       (mod #unspecified string)
		       (id #unspecified string)
		       (type (hop-mime-type) string)
		       (attributes)
		       body)
   
   (define (default src)
      (if (string? src)
	  (let ((src (string-append src "?js=" (or mod src))))
	     (when inline (warning "<SCRIPT>" "Cannot inline file -- " src))
	     (instantiate::xml-cdata
		(tag 'script)
		(attributes `(:src ,src type: ,type ,@attributes))
		(body body)))
	  (instantiate::xml-cdata
	     (tag 'script)
	     (attributes `(:type ,type ,@attributes))
	     (body body))))
   
   (define (inl src)
      (let ((body (cond
		     ((member (suffix src) (hop-client-script-suffixes))
		      (get-clientc-compiled-file src mod))
		     (else
		      (with-input-from-file src read-string)))))
	 (if body
	     (instantiate::xml-cdata
		(tag 'script)
		(attributes `(:type ,type ,@attributes))
		(body (list "\n" body)))
	     (default src))))

   (define (require src)
      (if (and inline (string? src))
	  (if (file-exists? src)
	      (inl src)
	      (default src))
	  (default src)))

   (cond
      ((string? src)
       (require src))
      ((pair? src)
       (cons (instantiate::xml-cdata
		(tag 'script)
		(attributes `(:type ,type ,@attributes))
		(body (list
			 (format "hop[ '%requireAlias' ]( ~s, ~s );"
			    (car src) (cadr src)))))
	  (map require (cdr src))))
      (else
       (error "<REQUIRE>" "bad src" src))))
 
;*---------------------------------------------------------------------*/
;*    <STYLE> ...                                                      */
;*---------------------------------------------------------------------*/
(define-tag <STYLE> ((type (hop-configure-css-mime-type) string)
		     (attributes)
		     body)
   (instantiate::xml-style
      (tag 'style)
      (attributes `(:type ,type ,@attributes))
      (body body)))

