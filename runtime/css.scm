;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/css.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 19 10:44:22 2005                          */
;*    Last change :  Sat Jun  6 19:12:17 2009 (serrano)                */
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
	    "service.sch"
	    "hss.sch")

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
	    (init-hss-compiler! ::int)
	    (hss-bind-type-compiler! ::symbol ::bstring ::pair-nil)
	    (hss-bind-property-compiler! ::symbol ::procedure)
	    (hss-property->declaration-list::pair-nil ::bstring)
	    (hss-properties->ruleset-list::pair-nil ::obj)
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
(define *hss-type-env* (make-hashtable))
(define *hss-property-env* '())

;*---------------------------------------------------------------------*/
;*    hop-hss-type! ...                                                */
;*    -------------------------------------------------------------    */
;*    This function is only here for ensuring backward compatility     */
;*    with hop < 2.0.x branch.                                         */
;*---------------------------------------------------------------------*/
(define (hop-hss-type! old new)
   (with-lock *hss-compiler-mutex*
      (lambda ()
	 (hss-bind-type-compiler! (string->symbol old) new '())
	 "")))

;*---------------------------------------------------------------------*/
;*    hss-bind-type-compiler! ...                                      */
;*---------------------------------------------------------------------*/
(define (hss-bind-type-compiler! type element properties)
   (with-lock *hss-compiler-mutex*
      (lambda ()
	 (let ((compiler (instantiate::hss-compiler
			    (element element)
			    (properties properties))))
	    (hashtable-put! *hss-type-env* (symbol->string type) compiler)))))

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
      (if (string-index val #\{)
	  (hss-parse-ruleset val)
	  (let ((str (string-append "f{" val "}")))
	     (duplicate::css-ruleset (hss-parse-ruleset str)
		(selector+ (list (instantiate::css-selector-name
				    (name ""))))))))

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
;*    hss-compile-selector ...                                         */
;*---------------------------------------------------------------------*/
(define (hss-compile-selector o hc)
   (with-access::css-selector o (element attr*)
      (let ((el (css-selector-name-name element)))
	 (instantiate::css-selector
	    (element (hss-compiler-element hc))
	    (attr* attr*)))))
;* 	    (attr* (cons (instantiate::css-selector-class              */
;* 			    (name (format (format "__HSS_~a" el))))    */
;* 			 attr*))))))                                   */

;*---------------------------------------------------------------------*/
;*    hss-compile-declaration* ...                                     */
;*---------------------------------------------------------------------*/
(define (hss-compile-declaration* selector decl lenv penv)
   
   (define (compose-selectors selector sel)
      (append selector (list 'childolf sel)))
   
   (define (compile-alias decl alias)
      (let ((nsel (compose-selectors selector (css-ruleset-selector+ alias))))
	 (duplicate::css-ruleset alias
	    (selector+ (list nsel)))))

   (define (empty-selector? selector+)
      (and (null? (cdr selector+))
	   (css-selector-name? (car selector+))
	   (string=? (css-selector-name-name (car selector+)) "")))

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
	     (cond
		((find-property-compiler property lenv)
		 =>
		 (lambda (comp)
		    ;; a local property
		    (let liip ((rules (comp expr prio))
			       (old old)
			       (nrules nrules))
		       (if (null? rules)
			   (loop (cdr decl) old nrules)
			   (with-access::css-ruleset (car rules)
				 (selector+ declaration*)
			      (if (empty-selector? selector+)
				  (liip (cdr rules)
					(append declaration* old)
					nrules)
				  (liip (cdr rules)
					old
					(cons (compile-alias (car decl)
							     (car rules))
					      nrules))))))))
		((find-property-compiler property penv)
		 =>
		 (lambda (comp)
		    ;; a global property
		    (let ((ndecls (comp expr prio)))
		       (loop (cdr decl) (append ndecls old) nrules))))
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
      (rule* (map (lambda (r)
		     (hss-compile r *hss-property-env*))
		  (css-stylesheet-rule* o)))))

;*---------------------------------------------------------------------*/
;*    hss-compile ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (hss-compile r penv)
   (if (pair? r)
       (map! (lambda (r) (hss-compile r penv)) r)
       r))

;*---------------------------------------------------------------------*/
;*    hss-compile ::css-media ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-media penv)
   (duplicate::css-media o
      (ruleset* (hss-compile (css-media-ruleset* o) penv))))

;*---------------------------------------------------------------------*/
;*    hss-compile ::css-ruleset ...                                    */
;*---------------------------------------------------------------------*/
(define-method (hss-compile o::css-ruleset penv)

   (define (compile-rule o)
      (with-access::css-ruleset o (selector+ declaration*)
	 (let ((hc (find-selector-compiler (car (last-pair (car selector+))))))
	    (if hc
		(let* ((lenv (hss-compiler-properties hc))
		       (nselector (hss-compile-selector* (car selector+))))
		   (hss-compile-declaration* nselector declaration* lenv penv))
		(let ((ndecl* (apply append (hss-compile declaration* penv))))
		   (duplicate::css-ruleset o
		      (selector+ (map! hss-compile-selector* selector+))
		      (declaration* ndecl*)))))))
   
   (with-access::css-ruleset o (selector+ declaration*)
      (if (and (pair? (cdr selector+))
	       (any? (lambda (s)
			(let ((hc (find-selector-compiler (car (last-pair s)))))
			   (when hc (pair? (hss-compiler-properties hc)))))
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
	     (comp expr prio)
	     (list o)))))
   
;*---------------------------------------------------------------------*/
;*    hss-compile-selector* ...                                        */
;*---------------------------------------------------------------------*/
(define (hss-compile-selector* lst)
   
   (define (compile o)
      (if (symbol? o)
	  o
	  (let ((hc (find-selector-compiler o)))
	     (with-access::css-selector o (element)
		(if (hss-compiler? hc)
		    (hss-compile-selector o hc)
		    (duplicate::css-selector o
		       (element (hss-unalias-selector-name element))))))))
   
   (map! compile lst))

;*---------------------------------------------------------------------*/
;*    hss-unalias-selector-name ...                                    */
;*---------------------------------------------------------------------*/
(define (hss-unalias-selector-name o::css-selector-name)
   (with-access::css-selector-name o (name)
      (if (string? name)
	  (let ((new (hashtable-get *hss-type-env* (string-downcase name))))
	     (if (hss-compiler? new)
		 (instantiate::css-selector-name
		    (name (hss-compiler-element new)))
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
;*    HSS Global compilers ...                                         */
;*---------------------------------------------------------------------*/
;; -hop-border-radius
(define-hss-property (-hop-border-radius v p)
   (match-case v
      ((?tl ?tr ?br ?bl)
       (format "-moz-border-radius: ~a ~a ~a ~a;
  -webkit-border-top-left-radius: ~a;
  -webkit-border-top-right-radius: ~a;
  -webkit-border-bottom-right-radius: ~a;
  -webkit-border-bottom-left-radius: ~a;"
	       tl tr br bl tl tr br bl))
      (else
       (format "-moz-border-radius: ~a;
  -webkit-border-radius: ~a;" (car v) (car v)))))
;; -hop-border-top-left-radius
(define-hss-property (-hop-border-top-left-radius v p)
   (format "-moz-border-radius-topLeft: ~a;
  -webkit-border-top-left-radius: ~a;" (car v) (car v)))
;; -hop-border-top-right-radius
(define-hss-property (-hop-border-top-right-radius v p)
   (format "-moz-border-radius-topRight: ~a;
  -webkit-border-top-right-radius: ~a;" (car v) (car v)))
;; -hop-border-bottom-right-radius
(define-hss-property (-hop-border-bottom-right-radius v p)
   (format "-moz-border-radius-bottomRight: ~a;
  -webkit-border-bottom-right-radius: ~a;" (car v) (car v)))
;; -hop-border-bottom-left-radius
(define-hss-property (-hop-border-bottom-left-radius v p)
   (format "-moz-border-radius-bottomLeft: ~a;
  -webkit-border-bottom-left-radius: ~a;" (car v) (car v)))
