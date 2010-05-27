;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/css.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 19 10:44:22 2005                          */
;*    Last change :  Thu May 27 07:40:48 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP css loader                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_css

   (library web multimedia)
   
   (include "xml.sch"
	    "service.sch"
	    "hss.sch")

   (import  __hop_read
	    __hop_param
	    __hop_cache
	    __hop_configure
	    __hop_http-error)

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
	       (selector::pair read-only)
	       (body::obj read-only)
	       (properties::pair-nil read-only (default '())))
	    (init-hss-compiler! ::int)
	    (hss-bind-type-compiler! ::symbol ::bstring ::obj ::pair-nil)
	    (hss-bind-property-compiler! ::symbol ::procedure)
	    (hss-bind-function-compiler! ::bstring ::procedure)
	    (hss-property->declaration-list::pair-nil ::bstring)
	    (hss-properties->ruleset-list::pair-nil ::obj)
	    (hss-response::%http-response ::http-request ::bstring)
	    (hss->css ::bstring)
	    (hss->css-url ::bstring)
	    (hop-get-hss ::bstring)
	    (hop-load-hss ::bstring)
	    (hop-read-hss ::input-port)

	    (hop-hss-type! ::bstring ::bstring)))
  
;*---------------------------------------------------------------------*/
;*    aliasing control ...                                             */
;*---------------------------------------------------------------------*/
(define *hss-compiler-mutex* (make-mutex "hop-hss-type"))
(define *hss-type-env* (make-hashtable))
(define *hss-property-env* '())
(define *hss-function-env* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    hop-hss-type! ...                                                */
;*    -------------------------------------------------------------    */
;*    This function is only here for ensuring backward compatility     */
;*    with hop < 2.0.x branch.                                         */
;*---------------------------------------------------------------------*/
(define (hop-hss-type! old new)
   (hss-bind-type-compiler! (string->symbol old) new #f '())
   "")

;*---------------------------------------------------------------------*/
;*    hss-bind-type-compiler! ...                                      */
;*---------------------------------------------------------------------*/
(define (hss-bind-type-compiler! type element body properties)
   (with-lock *hss-compiler-mutex*
      (lambda ()
	 (let* ((p (open-input-string (format "~a {}" element)))
		(ast (css->ast p :extension hss-extension))
		(compiler (with-access::css-stylesheet ast (rule*)
			     (with-access::css-ruleset (caar rule*) (selector+)
				(instantiate::hss-compiler
				   (selector (car selector+))
				   (body body)
				   (properties properties))))))
	    (hashtable-put! *hss-type-env*
			    (string-downcase (symbol->string type))
			    compiler)))))

;*---------------------------------------------------------------------*/
;*    find-selector-compiler ...                                       */
;*---------------------------------------------------------------------*/
(define (find-selector-compiler o)
   (with-access::css-selector o (element)
      (when element
	 (with-access::css-selector-name element (name)
	    (when (string? name)
	       (hashtable-get *hss-type-env* (string-downcase name)))))))

;*---------------------------------------------------------------------*/
;*    hss-bind-property-compiler! ...                                  */
;*---------------------------------------------------------------------*/
(define (hss-bind-property-compiler! id compiler)
   (with-lock *hss-compiler-mutex*
      (lambda ()
	 (set! *hss-property-env*
	       (cons (cons id compiler) *hss-property-env*)))))

;*---------------------------------------------------------------------*/
;*    hss-bind-function-compiler! ...                                  */
;*---------------------------------------------------------------------*/
(define (hss-bind-function-compiler! id compiler)
   (with-lock *hss-compiler-mutex*
      (lambda ()
	 (hashtable-put! *hss-function-env* id compiler))))

;*---------------------------------------------------------------------*/
;*    find-property-compiler ...                                       */
;*---------------------------------------------------------------------*/
(define (find-property-compiler id penv)
   (let ((cell (assq (string->symbol (string-downcase id)) penv)))
      (when (pair? cell) (cdr cell))))

;*---------------------------------------------------------------------*/
;*    hss-parse-ruleset ...                                            */
;*---------------------------------------------------------------------*/
(define (hss-parse-ruleset s)
   (let ((p (open-input-string s)))
      (unwind-protect
	 (let ((ast (css->ast p :extension hss-extension)))
	    (if (not (css-stylesheet? ast))
		(error 'hss-string->rulset "Illegal declaration list" s)
		(with-access::css-stylesheet ast (rule*)
		   (if (not (and (pair? rule*) (null? (cdr rule*))))
		       (error 'hss-string->rulset "Illegal declaration list" s)
		       (caar rule*)))))
	 (close-input-port p))))
   
;*---------------------------------------------------------------------*/
;*    hss-property->declaration-list ...                               */
;*---------------------------------------------------------------------*/
(define (hss-property->declaration-list val)
   (if (string? val)
       (let ((str (string-append "f{" val "}")))
	  (css-ruleset-declaration* (hss-parse-ruleset str)))
       (bigloo-type-error 'hss-property->declaration-list "string" val)))

;*---------------------------------------------------------------------*/
;*    hss-properties->ruleset-list ...                                 */
;*---------------------------------------------------------------------*/
(define (hss-properties->ruleset-list val)

   (define (hss-string->ruleset val)
      (cond
	 ((not (string-index val #\{))
	  (let ((str (string-append "f{" val "}")))
	     (duplicate::css-ruleset (hss-parse-ruleset str)
		(selector+ (list
			    (list
			     (instantiate::css-selector
				(element
				 (instantiate::css-selector-name
				    (name ""))))))))))
	 ((or (substring-at? val "+ " 0) (substring-at? val "> " 0))
	  (let* ((str (string-append "f " val))
		 (rs (hss-parse-ruleset str)))
	     (duplicate::css-ruleset rs
		(selector+ (list (cdr (car (css-ruleset-selector+ rs))))))))
	 (else
	  (hss-parse-ruleset val))))

   (cond
      ((string? val)
       (list (hss-string->ruleset val)))
      ((and (list? val) (every? string? val))
       (map hss-string->ruleset val))
      (else
       (bigloo-type-error 'hss-property->ruleset-list
			  "string or string-list"
			  val))))
	       
;*---------------------------------------------------------------------*/
;*    hss-compile-declaration* ...                                     */
;*---------------------------------------------------------------------*/
(define (hss-compile-declaration* selector decl lenv penv)
   
   (define (compose-selectors selector sel+)
      (if (symbol? (caar sel+))
	  (append selector (car sel+))
	  (append selector (cons 'childof (car sel+)))))
   
   (define (compile-alias decl alias)
      (let ((nsel (compose-selectors selector (css-ruleset-selector+ alias))))
	 (hss-compile (duplicate::css-ruleset alias
			 (selector+ (list nsel)))
		      penv)))

   (define (empty-selector? selector+)
      (and (null? (cdr selector+))
	   (css-selector-name? (car selector+))
	   (string=? (css-selector-name-name (car selector+)) "")))

   (define (args->string-args args)
      (map (lambda (a)
	      (if (string? a)
		  a
		  (let ((p (open-output-string)))
		     (css-write a p)
		     (close-output-port p))))
	   args))

   (let loop ((decl decl)
	      (old '())
	      (nrules '()))
      (if (null? decl)
	  (let ((orules (instantiate::css-ruleset
			   (selector+ (list selector))
			   (declaration* (reverse! old)))))
	     (if (pair? nrules)
		 ;; the compilation of the declarations has created
		 ;; new rules, we have to unfold...
		 (instantiate::css-ruleset-unfold
		    (ruleset+ (if (pair? old)
				  (list orules (reverse! nrules))
				  (reverse! nrules))))
		 orules))
	  (with-access::css-declaration (car decl) (property expr prio)
	     (cond
		((find-property-compiler property lenv)
		 =>
		 (lambda (comp)
		    ;; a local property
		    (let liip ((rules (comp (args->string-args expr) prio))
			       (ndecl '())
			       (old old)
			       (nrules nrules))
		       (if (null? rules)
			   (loop (append ndecl (cdr decl)) old nrules)
			   (with-access::css-ruleset (car rules) (selector+ declaration*)
			      (if (empty-selector? selector+)
				  (let laap ((l declaration*)
					     (ndecl ndecl)
					     (old old))
				     (cond
					((null? l)
					 (liip (cdr rules) ndecl old nrules))
					((equal? property (css-declaration-property (car l)))
					 (laap (cdr l) ndecl (cons (car l) old)))
					(else
					 (laap (cdr l) (cons (car l) ndecl) old))))
				  (liip (cdr rules)
					ndecl
					old
					(cons (compile-alias (car decl) (car rules)) nrules))))))))
		((find-property-compiler property penv)
		 =>
		 (lambda (comp)
		    ;; A global property that have to be processes again
		    ;; unless the generated property is the initial one
		    (let liip ((l (comp (args->string-args expr) prio))
			       (decl (cdr decl))
			       (old old))
		       (cond
			  ((null? l)
			   (loop decl old nrules))
			  ((equal? property (css-declaration-property (car l)))
			   (liip (cdr l) decl (cons (car l) old)))
			  (else
			   (liip (cdr l) (cons (car l) decl) old))))))
		(else
		 (loop (cdr decl) (cons (car decl) old) nrules)))))))

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
(define (init-hss-compiler! port)
   ;; hss cache
   (set! hss-cache
	 (instantiate::cache-disk
	    (path (make-file-path (hop-cache-directory)
				  (format "hss-~a" port)))
	    (out (lambda (o p) (css-write o p))))))

;*---------------------------------------------------------------------*/
;*    hss-response ...                                                 */
;*---------------------------------------------------------------------*/
(define (hss-response req path)
   (if (authorized-path? req path)
       (let ((hss (hop-load-hss path))
	     (mime (mime-type path "text/css"))
	     (method (http-request-method req)))
	  (cond
	     ((string? hss)
	      (instantiate::http-response-file
		 (request req)
		 (charset (hop-locale))
		 (content-type mime)
		 (bodyp (eq? method 'GET))
		 (file hss)))
	     (hss
	      (instantiate::http-response-procedure
		 (request req)
		 (charset (hop-locale))
		 (content-type mime)
		 (bodyp (eq? method 'GET))
		 (proc (lambda (p)
			  (css-write hss p)))))
	     (else
	      (http-file-not-found path))))
       (user-access-denied req)))

;*---------------------------------------------------------------------*/
;*    hop-get-hss ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop-get-hss path)
   (with-lock hss-mutex
      (lambda ()
	 (let ((cache (cache-get hss-cache path)))
	    (if (string? cache)
		cache
		(let* ((hss (hop-load-hss path))
		       (cache (cache-put! hss-cache path hss)))
		   (if (string? cache)
		       cache
		       hss)))))))

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
		    (with-handler
		       (lambda (e)
			  (when (&exception? e)
			     (exception-notify e))
			  (with-error-to-string
			     (lambda ()
				(error-notify e))))
		       (css-compile (hop-read-hss p))))
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
      (rule* (map (lambda (r)
		     (hss-compile r *hss-property-env*))
		  (css-stylesheet-rule* o)))))

;*---------------------------------------------------------------------*/
;*    hss-compile ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (hss-compile r penv)
   (if (pair? r)
       (map (lambda (r) (hss-compile r penv)) r)
       r))

;*---------------------------------------------------------------------*/
;*    hss-compile ::css-media ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-media penv)
   (duplicate::css-media o
      (ruleset* (hss-compile (css-media-ruleset* o) penv))))

;*---------------------------------------------------------------------*/
;*    hss-compile ::css-page ...                                       */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-page penv)
   (duplicate::css-page o
      (declaration* (hss-compile (css-page-declaration* o) penv))))

;*---------------------------------------------------------------------*/
;*    hss-compile ::css-fontface ...                                   */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-fontface penv)
   (duplicate::css-fontface o
      (declaration* (hss-compile (css-fontface-declaration* o) penv))))

;*---------------------------------------------------------------------*/
;*    hss-compile ::css-pseudopage ...                                 */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-pseudopage penv)
   (duplicate::css-pseudopage o))

;*---------------------------------------------------------------------*/
;*    hss-compile ::css-ruleset ...                                    */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-ruleset penv)

   (define (find-compiler selector)
      (let ((hc (find-selector-compiler selector)))
	 (when (and hc (pair? (hss-compiler-properties hc)))
	    hc)))
      
   (define (compile-rule o)
      (with-access::css-ruleset o (selector+ declaration*)
	 (let ((hc (find-compiler (car (last-pair (car selector+))))))
	    (if hc
		(let* ((lenv (hss-compiler-properties hc))
		       (nselector (hss-compile-selector* (car selector+))))
		   (hss-compile-declaration* nselector declaration* lenv penv))
		(let ((ndecl* (apply append (hss-compile declaration* penv))))
		   (duplicate::css-ruleset o
		      (selector+ (map hss-compile-selector* selector+))
		      (declaration* ndecl*)))))))

   (with-access::css-ruleset o (selector+ declaration*)
      (if (and (pair? (cdr selector+))
	       (any? (lambda (s)
			(find-compiler (car (last-pair s))))
		     selector+))
	  ;; the ruleset is unfolded iff:
	  ;;    it uses several selectors
	  ;;    one of the selector refers to a compiler in the last position
	  (instantiate::css-ruleset-unfold
	     (ruleset+ (map (lambda (s)
			       (compile-rule
				(instantiate::css-ruleset
				   (selector+ (list (hss-compile s penv)))
				   (declaration* declaration*))))
			    selector+)))
	  (compile-rule o))))

;*---------------------------------------------------------------------*/
;*    hss-compile ::css-declaration ...                                */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-declaration penv)
   (with-access::css-declaration o (property expr prio)
      (let ((comp (find-property-compiler property penv)))
	 (if comp
	     ;; call hss-compile recursively because the compilation might
	     ;; have generated unnormalized expressions. The recursion is
	     ;; stopped is the generated property is the same as the initial
	     ;; one.
	     (append-map (lambda (o)
			    (if (equal? property (css-declaration-property o))
				(list
				 (duplicate::css-declaration o
				    (expr (hss-compile expr penv))))
				(hss-compile o penv)))
			 (comp expr prio))
	     (list
	      (duplicate::css-declaration o
		 (expr (hss-compile expr penv))))))))
   
;*---------------------------------------------------------------------*/
;*    hss-compile-selector ...                                         */
;*---------------------------------------------------------------------*/
(define (hss-compile-selector::pair o hc bodyp)
   
   (define (pseudo-attr? a)
      (and (css-selector-pseudo? a)
	   (not (css-selector-pseudo-fun a))
	   (member (css-selector-pseudo-expr a)
		   '("first-child" "first-line" "first-letter"))))

   (define (copy-compiler-selectors hc attr*)
      (map (lambda (s)
	      (if (css-selector? s)
		  (duplicate::css-selector s
		     (attr* (append (css-selector-attr* s) attr*)))
		  s))
	   (hss-compiler-selector hc)))
   
   (with-access::css-selector o (element attr*)
      (let ((el (css-selector-name-name element)))
	 (if (and (hss-compiler-body hc) (or bodyp (any? pseudo-attr? attr*)))
	     (append
	      (copy-compiler-selectors hc attr*)
	      (list
	       '| |
	       (instantiate::css-selector
		  (element (hss-compiler-body hc))
		  (attr* (filter pseudo-attr? attr*)))))
	     (copy-compiler-selectors hc attr*)))))

;*---------------------------------------------------------------------*/
;*    hss-parse-function ...                                           */
;*---------------------------------------------------------------------*/
(define (hss-parse-function fun s)
   (define (err msg)
      (error fun msg s))
   (let ((p (open-input-string (format "* {x:~a;}" s))))
      (unwind-protect
	 (let ((ast (css->ast p :extension hss-extension)))
	    (if (not (css-stylesheet? ast))
		(err "result of HSS function not a stylesheet")
		(with-access::css-stylesheet ast (rule*)
		   (if (not (and (pair? rule*) (null? (cdr rule*))))
		       (if (pair? rule*)
			   (err "result contains more than one rule")
			   (err "result contains not rule"))
		       (with-access::css-ruleset (caar rule*) (declaration*)
			  (if (not (and (pair? declaration*)
					(null? (cdr declaration*))))
			      (if (pair? declaration*)
				  (err "result contains more than one declaration")
				  (err "result contains no declaration"))
			      (with-access::css-declaration (car declaration*)
				    (expr)
				 expr)))))))
	 (close-input-port p))))
   
;*---------------------------------------------------------------------*/
;*    hss-compile ::css-function ...                                   */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-function penv)
   (with-access::css-function o (fun expr)
      (let ((comp (with-lock *hss-compiler-mutex*
		     (lambda ()
			(hashtable-get *hss-function-env* fun))))
	    (nexpr (hss-compile expr penv)))
	 (if comp
	     (let ((args (filter (lambda (o) (not (equal? o ","))) nexpr)))
		(hss-parse-function fun (apply comp args)))
	     (duplicate::css-function o
		(expr nexpr))))))

;*---------------------------------------------------------------------*/
;*    hss-compile-selector* ...                                        */
;*---------------------------------------------------------------------*/
(define (hss-compile-selector* lst)
   (cond
      ((null? lst)
       '())
      ((symbol? (car lst))
       (cons (car lst) (hss-compile-selector* (cdr lst))))
      (else
       (let* ((o (car lst))
	      (hc (find-selector-compiler o)))
	  (with-access::css-selector o (element attr*)
	     (cond
		((hss-compiler? hc)
		 (let ((use-body (and (pair? (cdr lst))
				      (symbol? (cadr lst))
				      (not (eq? (cadr lst) '+)))))
		    (append (hss-compile-selector o hc use-body)
			    (hss-compile-selector* (cdr lst)))))
		((css-selector-name? element)
		 (append (hss-unalias-selector-name o element)
			 (hss-compile-selector* (cdr lst))))
		(else
		 (cons o (hss-compile-selector* (cdr lst))))))))))

;*---------------------------------------------------------------------*/
;*    hss-unalias-selector-name ...                                    */
;*---------------------------------------------------------------------*/
(define (hss-unalias-selector-name::pair s::css-selector o::css-selector-name)
   (with-access::css-selector-name o (name)
      (if (string? name)
	  (let ((new (hashtable-get *hss-type-env* (string-downcase name))))
	     (if (hss-compiler? new)
		 (hss-compiler-selector new)
		 (list (duplicate::css-selector s
			  (element o)))))
	  (list (duplicate::css-selector s
		   (element o))))))

#;(define (hss-unalias-selector-name.old o::css-selector-name)
   (with-access::css-selector-name o (name)
      (if (string? name)
	  (let ((new (hashtable-get *hss-type-env* (string-downcase name))))
	     (if (hss-compiler? new)
		 (hss-compiler-element new)
;* 		 (instantiate::css-selector-name                       */
;* 		    (name (hss-compiler-element new)))                 */
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
	     (with-handler
		(lambda (e)
		   (cond
		      ((&eval-warning? e)
		       (warning-notify e)
		       #unspecified)
		      ((and (&exception? e) (not (&exception-location e)))
		       (&exception-location-set! e pos)
		       (&exception-fname-set! e (input-port-name ip))
		       (raise e))
		      (else
		       (raise e))))
		(eval exp))))))

;*---------------------------------------------------------------------*/
;*    object-display ::css-hash-color ...                              */
;*---------------------------------------------------------------------*/
(define-method (object-display o::css-hash-color . port)
   (let ((p (if (pair? port) (car port) (current-output-port))))
      (display "#" p)
      (display (css-hash-color-value o) p)))

;*---------------------------------------------------------------------*/
;*    object-display ::css-function ...                                */
;*---------------------------------------------------------------------*/
(define-method (object-display o::css-function . port)
   (with-access::css-function o (fun expr)
      (let ((p (if (pair? port) (car port) (current-output-port))))
	 (display fun p)
	 (display expr p))))

;*---------------------------------------------------------------------*/
;*    HSS Global compilers ...                                         */
;*---------------------------------------------------------------------*/
;; border-radius
(define-hss-property (border-radius v p)
   (match-case v
      ((?tl ?tr ?br ?bl)
       (format "border-top-left-radius: ~a;
  border-top-right-radius: ~a;
  border-bottom-right-radius: ~a;
  border-bottom-left-radius: ~a;
  -moz-border-radius: ~a ~a ~a ~a;
  -webkit-border-top-left-radius: ~a;
  -webkit-border-top-right-radius: ~a;
  -webkit-border-bottom-right-radius: ~a;
  -webkit-border-bottom-left-radius: ~a;"
	       tl tr br bl tl tr br bl tl tr br bl))
      (else
       (format "border-radius: ~a;
  -moz-border-radius: ~a;
  -webkit-border-radius: ~a;" (car v) (car v) (car v)))))
;; border-top-left-radius
(define-hss-property (border-top-left-radius v p)
   (format "border-top-left-radius: ~a;
  -moz-border-radius-topLeft: ~a;
  -webkit-border-top-left-radius: ~a;"
	   (car v) (car v) (car v)))
;; border-top-right-radius
(define-hss-property (border-top-right-radius v p)
   (format "border-top-right-radius: ~a;
  -moz-border-radius-topRight: ~a;
  -webkit-border-top-right-radius: ~a;"
	   (car v) (car v) (car v)))
;; border-bottom-right-radius
(define-hss-property (border-bottom-right-radius v p)
   (format "border-bottom-right-radius: ~a;
  -moz-border-radius-bottomRight: ~a;
  -webkit-border-bottom-right-radius: ~a;"
	   (car v) (car v) (car v)))
;; border-bottom-left-radius
(define-hss-property (border-bottom-left-radius v p)
   (format "border-bottom-left-radius: ~a;
  -moz-border-radius-bottomLeft: ~a;
  -webkit-border-bottom-left-radius: ~a;"
	   (car v) (car v) (car v)))

;; box-shadow
(define-hss-property (box-shadow v p)
   (format "box-shadow: ~l;
  -moz-box-shadow: ~l;
  -webkit-box-shadow: ~l;"
	   v v v))

;; user-select
(define-hss-property (user-select v p)
   (format "user-select: ~l;
  -moz-user-select: ~l;
  -khtml-user-select: ~l;
  -webkit-user-select: ~l;"
	   v v v v))

;; multi-column
(define-hss-property (column-width v p)
   (format "column-width: ~l;
  -moz-column-width: ~l;
  -khtml-column-width: ~l;
  -webkit-column-width: ~l;"
	   v v v v))

(define-hss-property (column-count v p)
   (format "column-count: ~l;
  -moz-column-count: ~l;
  -khtml-column-count: ~l;
  -webkit-column-count: ~l;"
	   v v v v))

(define-hss-property (column-gap v p)
   (format "column-gap: ~l;
  -moz-column-gap: ~l;
  -khtml-column-gap: ~l;
  -webkit-column-gap: ~l;"
	   v v v v))

(define-hss-property (column-rule v p)
   (format "column-rule: ~l;
  -moz-column-rule: ~l;
  -khtml-column-rule: ~l;
  -webkit-column-rule: ~l;"
	   v v v v))

;; transform
(define-hss-property (transform v p)
   (format "transform: ~l;
  -moz-transform: ~l;
  -webkit-transform: ~l;"
	   v v v v))
;; hsv
(define-hss-function (hsv h s v)
   (multiple-value-bind (r g b)
      (hsv->rgb (string->integer h) (string->integer s) (string->integer v))
      (format "rgb(~a,~a,~a)" r g b)))

;; share
(define-hss-function (share file)
   
   (define (normalize-file file)
      (let ((len (string-length file)))
	 (if (=fx len 0)
	     file
	     (let ((c0 (string-ref file 0))
		   (c1 (string-ref file (-fx len 1))))
		(cond
		   ((and (or (char=? c0 #\") (char=? c0 #\'))
			 (or (char=? c1 #\") (char=? c1 #\')))
		    (substring file 1 (-fx len 1)))
		   ((and (or (char=? c0 #\") (char=? c0 #\')))
		    (substring file 1 len))
		   ((and (or (char=? c1 #\") (char=? c1 #\')))
		    (substring file 0 (-fx len 1)))
		   (else
		    file))))))
   
   (let ((fname (make-file-name (hop-share-directory) (normalize-file file)))
	 (enable-image-inlining #f))
      (if (and enable-image-inlining (file-exists? fname))
	  (let ((p (open-input-file fname)))
	     (if (input-port? p)
		 (unwind-protect
		    (format "url( \"data:~a;base64,~a\" )"
			    (mime-type file (format "image/~a" (suffix file)))
			    (base64-encode (read-string p) 0))
		    (close-input-port p))
		 (format "url( ~s )" fname)))
	  (format "url( ~s )" fname))))
