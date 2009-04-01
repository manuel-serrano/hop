;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/css.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 19 10:44:22 2005                          */
;*    Last change :  Wed Apr  1 11:39:57 2009 (serrano)                */
;*    Copyright   :  2005-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP css loader                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_css

   (library web)
   
   (include "xml.sch"
	    "service.sch")

   (import  __hop_read
	    __hop_param
	    __hop_cache)

   (use	    __hop_user
	    __hop_hop
	    __hop_cgi
	    __hop_misc
	    __hop_service
	    __hop_mime
	    __hop_types)

   (static  (class css-ruleset-unfold
	       (ruleset+::pair read-only)))
   
   (export  (class hss-compiler
	       (element::bstring read-only)
	       (properties::pair-nil read-only (default '())))
	    (init-hss-compiler!)
	    (hss-response::%http-response ::http-request ::bstring)
	    (hss->css ::bstring)
	    (hss->css-url ::bstring)
	    (hop-load-hss ::bstring)
	    (hop-read-hss ::input-port)

	    (hop-hss-type! ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    aliasing control ...                                             */
;*---------------------------------------------------------------------*/
(define *hss-compiler-mutex* (make-mutex "hop-hss-type"))
(define *hss-types* (make-hashtable))
(define *hss-compilers* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    hss-register-compiler! ...                                       */
;*---------------------------------------------------------------------*/
(define (hss-register-compiler! element c)
   (with-lock *hss-compiler-mutex*
      (lambda ()
	 (hashtable-put! *hss-compilers* (string-downcase element) c)
	 (hashtable-put! *hss-compilers* (string-upcase element) c))))

;*---------------------------------------------------------------------*/
;*    hss-find-compiler ...                                            */
;*---------------------------------------------------------------------*/
(define (hss-find-compiler o)
   (with-access::css-selector o (element)
      (when element
	 (with-access::css-selector-name element (name)
	    (when (string? name)
	       (hashtable-get *hss-compilers* name))))))

;*---------------------------------------------------------------------*/
;*    hss-compile-selector ...                                         */
;*---------------------------------------------------------------------*/
(define (hss-compile-selector o hc)
   (with-access::css-selector o (element attr*)
      (let ((el (css-selector-name-name element)))
	 (instantiate::css-selector
	    (element (hss-compiler-element hc))
	    (attr* (cons (instantiate::css-selector-class
			    (name (format (format "__HSS_~a" el))))
			 attr*))))))

;*---------------------------------------------------------------------*/
;*    hss-compile-declaration* ...                                     */
;*---------------------------------------------------------------------*/
(define (hss-compile-declaration* selector decl hc)
   
   (define (compose-selectors selector sel)
      (append selector
	      (list 'childolf
		    (instantiate::css-selector-name
		       (name sel)))))
   
   (define (compile-alias decl alias)
      (let ((nsel (car alias))
	    (ndecl (cadr alias)))
	 (instantiate::css-ruleset
	    (selector+ (list (compose-selectors selector nsel)))
	    (declaration* (list (duplicate::css-declaration decl
				   (property ndecl)))))))
   
   (let loop ((decl decl)
	      (old '())
	      (nrules '()))
      (if (null? decl)
	  (let ((orules (instantiate::css-ruleset
			   (selector+ (list selector))
			   (declaration* old))))
	     (if (pair? nrules)
		 ;; the compilation of the declarations has created
		 ;; new rules, we have to unfold...
		 (instantiate::css-ruleset-unfold
		    (ruleset+ (list orules nrules)))
		 orules))
	  (with-access::css-declaration (car decl) (property expr prio)
	     (let ((cell (assoc property (hss-compiler-properties hc))))
		(if (pair? cell)
		    (let ((rules (map (lambda (alias)
					 (compile-alias (car decl) alias))
				      (cdr cell))))
		       (loop (cdr decl) old (append rules nrules)))
		    (loop (cdr decl) (cons (car decl) old) nrules)))))))

;*---------------------------------------------------------------------*/
;*    *hss-builtin-types* ...                                          */
;*---------------------------------------------------------------------*/
(define *hss-builtin-types*
   '(;; notepad
     ("notepad" "div.hop-notepad")
     ("notepad" "div.hop-notepad")
     ("notepad-tabs" "td.hop-notepad-tabs")
     ("notepad-tabs-row" "div.hop-notepad-tabs-row")
     ("notepad-body" "td.hop-notepad-body")
     ("nphead" "div.hop-nphead")
     ("nptab" "span.hop-nptab")
     ("nptab-active" "span.hop-nptab-active")
     ("nptab-inactive" "span.hop-nptab-inactive")
     ("nptab-body" "div.hop-notepad-tab-body")
     ("nptabhead" "span.hop-nptab-head")
     ;; paned
     ("paned" "div.hop-paned")
     ;; sorttable
     ("sorttable" "span.hop-sorttable table")
     ;; audio
     ("audio" "div.hop-audio-controls")
     ;; filechooser
     ("filechooser" "div.filechooser")
     ;; prefs
     ("prefs" "table.hop-prefs")
     ("prlabel" "table.hop-prefs td.hop-prefs-label")
     ("prsep" "table.hop-prefs td.hop-prefs-separator")
     ("pr" "table.hop-prefs tr.hop-pr")
     ("pr-name" "table.hop-prefs td.hop-pr-name")
     ("pr-editor" "table.hop-prefs td.hop-pr-editor")
     ("pr-editor-expr" "table.hop-prefs input.hop-pr-editor-expr")
     ("pr-editor-bool" "table.hop-prefs table.hop-pr-editor-bool")
     ("pr-editor-text" "table.hop-prefs textarea.hop-pr-editor-text")
     ;; hop-login
     ("hop-login-panel" "div.hop-login-panel")
     ("hop-login" "div.hop-login-panel table.hop-login-main-table")
     ("hop-login-message" "div.hop-login-panel td.hop-login-message")
     ("hop-login-logo" "div.hop-login-panel div.hop-login-logo")))
   
;*---------------------------------------------------------------------*/
;*    hss-mutex ...                                                    */
;*---------------------------------------------------------------------*/
(define hss-mutex (make-mutex 'hss))

;*---------------------------------------------------------------------*/
;*    hss-cache ...                                                    */
;*---------------------------------------------------------------------*/
(define hss-cache
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hss->css ...                                                     */
;*---------------------------------------------------------------------*/
(define (hss->css path)
   (with-lock hss-mutex
      (lambda ()
	 (let ((cache (cache-get hss-cache path))
	       (mime (mime-type path "text/css")))
	    (if (string? cache)
		(with-input-from-file cache read-string)
		(let* ((hss (hop-load-hss path))
		       (cache (cache-put! hss-cache path hss)))
		   (let ((p (open-output-string)))
		      (css-write hss p)
		      (close-output-port p))))))))

;*---------------------------------------------------------------------*/
;*    hss->css-url ...                                                 */
;*---------------------------------------------------------------------*/
(define (hss->css-url path)
   (string-append path "?" (hop-hss-compile-suffix)))

;*---------------------------------------------------------------------*/
;*    init-hss-compiler! ...                                           */
;*---------------------------------------------------------------------*/
(define (init-hss-compiler!)
   ;; builtin hss types
   (for-each (lambda (t) (apply hop-hss-type! t)) *hss-builtin-types*)
   ;; hss cache
   (set! hss-cache
	 (instantiate::cache-disk
	    (path (make-file-path (hop-rc-directory)
				  "cache"
				  (format "hss-~a" (hop-port))))
	    (out (lambda (o p) (css-write o p))))))

;*---------------------------------------------------------------------*/
;*    hss-response ...                                                 */
;*---------------------------------------------------------------------*/
(define (hss-response req path)
   (if (authorized-path? req path)
       (with-lock hss-mutex
	  (lambda ()
	     (let ((cache (cache-get hss-cache path))
		   (mime (mime-type path "text/css"))
		   (method (http-request-method req)))
		(if (string? cache)
		    (instantiate::http-response-file
		       (request req)
		       (charset (hop-locale))
		       (content-type mime)
		       (bodyp (eq? method 'GET))
		       (file cache))
		    (let* ((hss (hop-load-hss path))
			   (cache (cache-put! hss-cache path hss)))
		       (if (string? cache)
			   (instantiate::http-response-file
			      (request req)
			      (charset (hop-locale))
			      (content-type mime)
			      (bodyp (eq? method 'GET))
			      (file cache))
			   (instantiate::http-response-procedure
			      (request req)
			      (charset (hop-locale))
			      (content-type mime)
			      (bodyp (eq? method 'GET))
			      (proc (lambda (p)
				       (css-write hss p))))))))))
       (user-access-denied req)))

;*---------------------------------------------------------------------*/
;*    hop-load-hss ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop-load-hss file)
   (if (file-exists? file)
       (let ((p (open-input-file file))
	     (mod (eval-module))
	     (loadingf (the-loading-file)))
	  (loading-file-set! file)
	  (if (input-port? p)
	      (unwind-protect
		 (begin
		    ;; each hss file is read inside a dummy empty module
		    (eval `(module ,(gensym)))
		    (css-compile (hop-read-hss p)))
		 (begin
		    (when mod (eval-module-set! mod))
		    (close-input-port p)
		    (loading-file-set! loadingf)))
	      (raise (instantiate::&io-port-error
			(proc 'hop-load)
			(msg "Can't open file")
			(obj file)))))
       (raise (instantiate::&io-file-not-found-error
		 (proc 'hop-load)
		 (msg "file does not exist")
		 (obj file)))))


;*---------------------------------------------------------------------*/
;*    hop-read-hss ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function is invoked when the initial "@{" string has        */
;*    already been parsed.                                             */
;*---------------------------------------------------------------------*/
(define (hop-read-hss iport)
   (with-handler
      (lambda (e)
	 (if (not (&io-parse-error? e))
	     (raise e)
	     (match-case (&io-parse-error-obj e)
		((?token ?val ?file ?pos)
		 (raise (duplicate::&io-parse-error e
			   (obj (format "~a (~a)" token val))
			   (fname file)
			   (location pos))))
		(else
		 (raise e)))))
      (css->ast iport :extension hss-extension)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-ruleset-unfold ...                               */
;*    -------------------------------------------------------------    */
;*    css-rulesef-unfold i produced by the compilation of a            */
;*    ruleset.                                                         */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-ruleset-unfold p::output-port)
   (for-each (lambda (o) (css-write o p)) (css-ruleset-unfold-ruleset+ o)))
			  
;*---------------------------------------------------------------------*/
;*    css-compile ...                                                  */
;*---------------------------------------------------------------------*/
(define (css-compile o::css-stylesheet)
   (duplicate::css-stylesheet o
      (rule* (map hss-compile (css-stylesheet-rule* o)))))

;*---------------------------------------------------------------------*/
;*    hss-compile ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (hss-compile r)
   (if (pair? r)
       (map hss-compile r)
       r))

;*---------------------------------------------------------------------*/
;*    hss-compile ::css-media ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-media)
   (duplicate::css-media o
      (ruleset* (map! hss-compile (css-media-ruleset* o)))))

;*---------------------------------------------------------------------*/
;*    hss-compile ::css-ruleset ...                                    */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-ruleset)

   (define (compile-rule o)
      (with-access::css-ruleset o (selector+ declaration*)
	 (let ((hc (hss-find-compiler (car (last-pair (car selector+)))))
	       (ndeclaration* (map hss-compile declaration*)))
	    (if hc
		(let ((nselector (hss-compile-selector* (car selector+))))
		   (hss-compile-declaration* nselector ndeclaration* hc))
		(duplicate::css-ruleset o
		   (selector+ (map! hss-compile-selector* selector+))
		   (declaration* ndeclaration*))))))
   
   (with-access::css-ruleset o (selector+ declaration*)
      (if (and (pair? (cdr selector+))
	       (any? (lambda (s)
			(let ((hc (hss-find-compiler (car (last-pair s)))))
			   (when hc (pair? (hss-compiler-properties hc)))))
		     selector+))
	  ;; the ruleset is unfolded iff:
	  ;;    it uses several selectors
	  ;;    one of the selector refers to a compiler in the last position
	  (instantiate::css-ruleset-unfold
	     (ruleset+ (map (lambda (s)
			       (compile-rule
				(instantiate::css-ruleset
				   (selector+ (list (hss-compile s)))
				   (declaration* declaration*))))
			    selector+)))
	  (compile-rule o))))

;*---------------------------------------------------------------------*/
;*    hss-compile-selector* ...                                        */
;*---------------------------------------------------------------------*/
(define (hss-compile-selector* lst)
   
   (define (compile o)
      (if (symbol? o)
	  o
	  (let ((hc (hss-find-compiler o)))
	     (with-access::css-selector o (element)
		(if (hss-compiler? hc)
		    (hss-compile-selector o hc)
		    (duplicate::css-selector o
		       (element (hss-compile element))))))))
   
   (map! compile lst))
		 
;*---------------------------------------------------------------------*/
;*    hss-compile ::css-selector-name ...                              */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-selector-name)
   (with-access::css-selector-name o (name)
      (if (string? name)
	  (let ((new (hashtable-get *hss-types* name)))
	     (if new
		 (instantiate::css-selector-name (name new))
		 o))
	  o)))

;*---------------------------------------------------------------------*/
;*    hss-extension ...                                                */
;*---------------------------------------------------------------------*/
(define (hss-extension c ip)
   (when (char=? c #\$)
      (let ((exp (hop-read ip))
	    (pos (input-port-position ip)))
	 (if (eof-object? exp)
	     (read-error/location "Unexpected end-of-file"
				  "Unclosed list"
				  (input-port-name ip)
				  pos)
	     (let ((val (with-handler
			   (lambda (e)
			      (if (&eval-warning? e)
				  (begin
				     (warning-notify e)
				     #unspecified)
				  (raise e)))
			   (eval exp))))
		(cond
		   ((string? val) val)
		   (else #unspecified)))))))

;*---------------------------------------------------------------------*/
;*    hop-hss-type! ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-hss-type! old new)
   (with-lock *hss-compiler-mutex*
      (lambda ()
	 (hashtable-put! *hss-types* (string-upcase old) new)
	 (hashtable-put! *hss-types* (string-downcase old) new)
	 "")))

;*---------------------------------------------------------------------*/
;*    default hss type ...                                             */
;*---------------------------------------------------------------------*/
(hop-hss-type! "window" "table.hop-window td.hop-window-content")

;*---------------------------------------------------------------------*/
;*    Example a hss compiler                                           */
;*---------------------------------------------------------------------*/
;; (hss-register-compiler!
;; "gauge"
;;  (instantiate::hss-compiler
;;    (element "span")
;;    (properties '(("foo" ("span.foo" "bg") ("span.gee" "fg"))
;; 		    ("gee" ("span.foo" "fg"))
;;		    ("bar" ("span.foo span.bar" "fg"))))))
