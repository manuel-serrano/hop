;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/css.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 19 10:44:22 2005                          */
;*    Last change :  Sun Apr 20 08:24:49 2014 (serrano)                */
;*    Copyright   :  2005-14 Manuel Serrano                            */
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
	    __hop_http-error
	    __hop_types
	    __hop_xml-types
	    __hop_xml
	    __hop_mime
	    __hop_misc
	    __hop_user
	    __hop_http-lib)

   (static  (class css-ruleset-unfold
	       (ruleset+::pair read-only)))
   
   (export  (class hss-compiler
	       (selector::pair read-only)
	       (body::obj read-only)
	       (properties::pair-nil read-only (default '())))
	    
	    (init-hss-compiler! ::int)
	    (hss-extension ::char ::input-port)
	    (hss-bind-type-compiler! ::symbol ::bstring ::obj ::pair-nil)
	    (hss-bind-property-compiler! ::symbol ::procedure)
	    (hss-bind-function-compiler! ::bstring ::procedure)
	    (hss-property->declaration-list::pair-nil ::bstring)
	    (hss-properties->ruleset-list::pair-nil ::obj)
	    (hss-response::%http-response ::http-request ::bstring)
	    (hss->css ::bstring)
	    (hss->css-url ::bstring)
	    (hss-compile ::obj)
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
;*    hss->ast ...                                                     */
;*---------------------------------------------------------------------*/
(define (hss->ast s)
   (let ((p (open-input-string s)))
      (with-handler
	 (lambda (e)
	    (if (isa? e &io-parse-error)
		(with-access::&error e (obj)
		   (match-case obj
		      ((?token ?val ?port ?pos)
		       (raise
			  (duplicate::&io-parse-error e
			     (obj (string-append (substring s 0 pos)
				     "==>" (string (string-ref s pos)) "<=="
				     (substring s 0 pos))))))
		      (else
		       (raise e))))
		(raise e)))
	 (let ((ast (css->ast p :extension hss-extension)))
	    (close-input-port p)
	    ast))))

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
   (synchronize *hss-compiler-mutex*
      (let* ((ast (hss->ast (format "~a {}" element)))
	     (compiler (with-access::css-stylesheet ast (rule*)
			  (with-access::css-ruleset (caar rule*) (selector+)
			     (instantiate::hss-compiler
				(selector (car selector+))
				(body body)
				(properties properties))))))
	 (hashtable-put! *hss-type-env*
	    (string-downcase (symbol->string type))
	    compiler))))

;*---------------------------------------------------------------------*/
;*    find-selector-compiler ...                                       */
;*---------------------------------------------------------------------*/
(define (find-selector-compiler o)
   (with-access::css-selector o (element)
      (when element
	 (with-access::css-selector-name element (name)
	    (when (string? name)
	       (synchronize *hss-compiler-mutex*
		  (hashtable-get *hss-type-env* (string-downcase name))))))))

;*---------------------------------------------------------------------*/
;*    hss-bind-property-compiler! ...                                  */
;*---------------------------------------------------------------------*/
(define (hss-bind-property-compiler! id compiler)
   (synchronize *hss-compiler-mutex*
      (set! *hss-property-env*
	 (cons (cons id compiler) *hss-property-env*))))

;*---------------------------------------------------------------------*/
;*    hss-bind-function-compiler! ...                                  */
;*---------------------------------------------------------------------*/
(define (hss-bind-function-compiler! id compiler)
   (synchronize *hss-compiler-mutex*
      (hashtable-put! *hss-function-env* id compiler)))

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
   (let ((ast (hss->ast s)))
      (if (not (isa? ast css-stylesheet))
	  (error "hss-string->rulset" "Illegal declaration list" s)
	  (with-access::css-stylesheet ast (rule*)
	     (if (not (and (pair? rule*) (null? (cdr rule*))))
		 (error "hss-string->rulset" "Illegal declaration list" s)
		 (caar rule*))))))
   
;*---------------------------------------------------------------------*/
;*    hss-parse-rulesets ...                                           */
;*---------------------------------------------------------------------*/
(define (hss-parse-rulesets s)
   (let ((ast (hss->ast s)))
      (if (not (isa? ast css-stylesheet))
	  (error "hss-string->rulset" "Illegal declaration list" s)
	  (with-access::css-stylesheet ast (rule*)
	     (if (not (pair? rule*))
		 (error "hss-string->rulset" "Illegal declaration list" s)
		 (append-map (lambda (rule)
				(filter (lambda (r)
					   (isa? r css-ruleset))
				   rule))
		    rule*))))))
   
;*---------------------------------------------------------------------*/
;*    hss-property->declaration-list ...                               */
;*---------------------------------------------------------------------*/
(define (hss-property->declaration-list val)
   (if (string? val)
       (let ((str (string-append "f{" val "}")))
	  (with-access::css-ruleset (hss-parse-ruleset str) (declaration*)
	     declaration*))
       (bigloo-type-error 'hss-property->declaration-list "string" val)))

;*---------------------------------------------------------------------*/
;*    hss-properties->ruleset-list ...                                 */
;*---------------------------------------------------------------------*/
(define (hss-properties->ruleset-list val)

   (define (hss-string->ruleset::pair-nil val)
      (cond
	 ((not (string-index val #\{))
	  (let ((str (string-append "f{" val "}")))
	     (list
	      (duplicate::css-ruleset (hss-parse-ruleset str)
		 (selector+ (list
			     (list
			      (instantiate::css-selector
				 (element
				  (instantiate::css-selector-name
				     (name "")))))))))))
	 ((or (substring-at? val "+ " 0) (substring-at? val "> " 0))
	  (let* ((str (string-append "f " val))
		 (rs (hss-parse-ruleset str)))
	     (with-access::css-ruleset rs (selector+)
		(list
		   (duplicate::css-ruleset rs
		      (selector+ (list (cdr (car selector+)))))))))
	 (else
	  (hss-parse-rulesets val))))

   (cond
      ((string? val)
       (hss-string->ruleset val))
      ((and (list? val) (every string? val))
       (append-map hss-string->ruleset val))
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
      (with-access::css-ruleset alias (selector+)
	 (let ((nsel (compose-selectors selector selector+)))
	    (compile (duplicate::css-ruleset alias
			(selector+ (list nsel)))
	       penv))))

   (define (empty-selector? selector+)
      (and (null? (cdr selector+))
	   (isa? (car selector+) css-selector-name)
	   (with-access::css-selector-name (car selector+) (name)
	      (string=? name ""))))

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
					((equal? property
					    (with-access::css-declaration (car l) (property)
					       property))
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
		    ;; A global property that have to be processed again
		    ;; unless the generated property is the initial one
		    (let liip ((rules (comp (args->string-args expr) prio))
			       (decl (cdr decl))
			       (old old))
		       (cond
			  ((null? rules)
			   (loop decl old nrules))
			  ((equal? property
			      (with-access::css-declaration (car rules) (property)
				 property))
			   (liip (cdr rules) decl (cons (car rules) old)))
			  (else
			   (liip (cdr rules) (cons (car rules) decl) old))))))
		(else
		 (loop (cdr decl) (append (compile (car decl) penv) old) nrules)))))))

;*---------------------------------------------------------------------*/
;*    hss-mutex ...                                                    */
;*---------------------------------------------------------------------*/
(define hss-mutex (make-mutex "hss"))

;*---------------------------------------------------------------------*/
;*    hss-cache ...                                                    */
;*---------------------------------------------------------------------*/
(define hss-cache
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hss->css ...                                                     */
;*---------------------------------------------------------------------*/
(define (hss->css path)
   (synchronize hss-mutex
      (let ((ce (cache-get hss-cache path)))
	 (if (isa? ce cache-entry)
	     (with-access::cache-entry ce (value)
		(with-input-from-file value read-string))
	     (let ((hss (hop-load-hss path)))
		(cache-put! hss-cache path hss)
		(let ((p (open-output-string)))
		   (css-write hss p)
		   (close-output-port p)))))))

;*---------------------------------------------------------------------*/
;*    hss->css-url ...                                                 */
;*---------------------------------------------------------------------*/
(define (hss->css-url path)
   (string-append path "?" (hop-hss-compile-suffix)))

;*---------------------------------------------------------------------*/
;*    init-hss-compiler! ...                                           */
;*---------------------------------------------------------------------*/
(define (init-hss-compiler! port)
   (set! hss-cache
	 (instantiate::cache-disk
	    (path (make-cache-name "hss"))
	    (out (lambda (o p) (css-write o p))))))

;*---------------------------------------------------------------------*/
;*    hss-response ...                                                 */
;*---------------------------------------------------------------------*/
(define (hss-response req path)
   (if (authorized-path? req path)
       (with-access::http-request req (method header)
	  (let ((hss (hop-get-hss path))
		(mime (mime-type path "text/css")))
	     (cond
		((isa? hss cache-entry)
		 (with-access::cache-entry hss (value signature)
		    (let ((etag (http-header-field header if-none-match:))
			  (hd `((ETag: . ,signature))))
		       (if (and (string? etag)
				(=elong (string->elong etag) signature))
			   (instantiate::http-response-string
			      (request req)
			      (start-line "HTTP/1.1 304 Not Modified")
			      (content-type mime)
			      (header hd)
			      (charset (hop-locale)))
			   (instantiate::http-response-file
			      (request req)
			      (charset (hop-locale))
			      (content-type mime)
			      (bodyp (eq? method 'GET))
			      (header hd)
			      (file value))))))
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
                      (proc (lambda (p) (css-write hss p)))))
		(else
		 (http-file-not-found path)))))
       (user-access-denied req)))

;*---------------------------------------------------------------------*/
;*    hop-get-hss ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop-get-hss path)
   (synchronize hss-mutex
      (let ((ce (cache-get hss-cache path)))
	 (if (isa? ce cache-entry)
	     ce
	     (let* ((hss (hop-load-hss path))
		    (ce (cache-put! hss-cache path hss)))
		(if (isa? ce cache-entry)
		    ce
		    hss))))))

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
			  (when (isa? e &exception)
			     (exception-notify e))
			  (with-error-to-string
			     (lambda ()
				(exception-notify e))))
		       (hss-compile (hop-read-hss p))))
		 (begin
		    (when mod (eval-module-set! mod))
		    (close-input-port p)
		    (loading-file-set! loadingf)))
	      (raise (instantiate::&io-port-error
			(proc "hop-load")
			(msg "Can't open file")
			(obj file)))))
       (raise (instantiate::&io-file-not-found-error
		 (proc "hop-load")
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
	 (if (not (isa? e &io-parse-error))
	     (raise e)
	     (with-access::&io-parse-error e (obj)
		(match-case obj
		   ((?token ?val ?file ?pos)
		    (raise (duplicate::&io-parse-error e
			      (obj (format "~a (~a)" token val))
			      (fname file)
			      (location pos))))
		   (else
		    (raise e))))))
      (css->ast iport :extension hss-extension)))

;*---------------------------------------------------------------------*/
;*    css-write ::xml-tilde ...                                        */
;*---------------------------------------------------------------------*/
(define-method (css-write o::xml-tilde p::output-port)
   (display "\"javascript:" p)
   (display (xml-tilde->statement o) p)
   (display "\"" p))

;*---------------------------------------------------------------------*/
;*    css-write ::css-ruleset-unfold ...                               */
;*    -------------------------------------------------------------    */
;*    css-rulesef-unfold produced by the compilation of a ruleset.     */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-ruleset-unfold p::output-port)
   (with-access::css-ruleset-unfold o (ruleset+)
      (for-each (lambda (o) (css-write o p))
	 ruleset+)))

;*---------------------------------------------------------------------*/
;*    hss-compile ...                                                  */
;*---------------------------------------------------------------------*/
(define (hss-compile o)
   (with-access::css-stylesheet o (import* rule*)
      (duplicate::css-stylesheet o
	 (import* (map hss-compile-import import*))
	 (rule* (map (lambda (r)
			(compile r *hss-property-env*))
		   rule*)))))

;*---------------------------------------------------------------------*/
;*    hss-compile-import ...                                           */
;*---------------------------------------------------------------------*/
(define (hss-compile-import import)
   (with-access::css-import (car import) (value)
      (cond
	 ((and (string? value) (string-suffix? ".hss\"" value))
	  (let ((s (substring value 0 (-fx (string-length value) 1))))
	     (cons (duplicate::css-import (car import)
		      (value (string-append s "?hss\"")))
		   (cdr import))))
	 ((and (string? value) (string-suffix? ".hss" value))
	  (cons (duplicate::css-import (car import)
		   (value (string-append value "?hss")))
		(cdr import)))
	 (else
	  import))))

;*---------------------------------------------------------------------*/
;*    compile ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (compile r penv)
   (if (pair? r)
       (map (lambda (r) (compile r penv)) r)
       r))

;*---------------------------------------------------------------------*/
;*    compile ::css-media ...                                          */
;*---------------------------------------------------------------------*/
(define-method (compile o::css-media penv)
   (with-access::css-media o (ruleset*)
      (duplicate::css-media o
	 (ruleset* (compile ruleset* penv)))))

;*---------------------------------------------------------------------*/
;*    compile ::css-page ...                                           */
;*---------------------------------------------------------------------*/
(define-method (compile o::css-page penv)
   (with-access::css-page o (declaration*)
      (duplicate::css-page o
	 (declaration* (compile declaration* penv)))))

;*---------------------------------------------------------------------*/
;*    compile ::css-fontface ...                                       */
;*---------------------------------------------------------------------*/
(define-method (compile o::css-fontface penv)
   (with-access::css-fontface o (declaration*)
      (duplicate::css-fontface o
	 (declaration* (compile declaration* penv)))))

;*---------------------------------------------------------------------*/
;*    compile ::css-pseudopage ...                                     */
;*---------------------------------------------------------------------*/
(define-method (compile o::css-pseudopage penv)
   (duplicate::css-pseudopage o))

;*---------------------------------------------------------------------*/
;*    compile ::css-ruleset ...                                        */
;*---------------------------------------------------------------------*/
(define-method (compile o::css-ruleset penv)

   (define (find-compiler selector)
      (let ((hc (find-selector-compiler selector)))
	 (when (and hc (pair? (with-access::hss-compiler hc (properties)
				 properties)))
	    hc)))
      
   (define (compile-rule o)
      (with-access::css-ruleset o (selector+ declaration*)
	 (let ((hc (find-compiler (car (last-pair (car selector+))))))
	    (if hc
		(with-access::hss-compiler hc ((lenv properties))
		   (let ((nselector (hss-compile-selector* (car selector+))))
		      (hss-compile-declaration* nselector declaration* lenv penv)))
		(let ((ndecl* (apply append (compile declaration* penv))))
		   (duplicate::css-ruleset o
		      (selector+ (map hss-compile-selector* selector+))
		      (declaration* ndecl*)))))))

   (with-access::css-ruleset o (selector+ declaration*)
      (if (and (pair? (cdr selector+))
	       (any (lambda (s)
		       (find-compiler (car (last-pair s))))
		  selector+))
	  ;; the ruleset is unfolded iff:
	  ;;    it uses several selectors
	  ;;    one of the selector refers to a compiler in the last position
	  (instantiate::css-ruleset-unfold
	     (ruleset+ (map (lambda (s)
			       (compile-rule
				  (instantiate::css-ruleset
				     (selector+ (list (compile s penv)))
				     (declaration* declaration*))))
			  selector+)))
	  (compile-rule o))))

;*---------------------------------------------------------------------*/
;*    compile ::css-declaration ...                                    */
;*---------------------------------------------------------------------*/
(define-method (compile o::css-declaration penv)
   (with-access::css-declaration o (property expr prio)
      (let ((comp (find-property-compiler property penv)))
	 (if comp
	     ;; call compile recursively because the compilation might
	     ;; have generated unnormalized expressions. The recursion is
	     ;; stopped is the generated property is the same as the initial
	     ;; one.
	     (append-map (lambda (o2)
			    (with-access::css-declaration o ((p property))
			       (if (equal? property p)
				   (with-access::css-declaration o2 ((expr2 expr))
				      (list
					 (duplicate::css-declaration o2
					    (expr (compile expr2 penv)))))
				   (compile o penv))))
			 (comp expr prio))
	     (list
	      (duplicate::css-declaration o
		 (expr (compile expr penv))))))))
   
;*---------------------------------------------------------------------*/
;*    hss-compile-selector ...                                         */
;*---------------------------------------------------------------------*/
(define (hss-compile-selector::pair o hc bodyp)
   
   (define (pseudo-attr? a)
      (and (isa? a css-selector-pseudo)
	   (with-access::css-selector-pseudo a (expr fun)
	      (and (not fun)
		   (member expr '("active" "focus" "before" "after"))))))
   
   (define (copy-compiler-selectors hc attr*)
      ;; copy the virtual selector attribute only for the
      ;; first element of the selector
      (let ((npattr* (filter (lambda (a)
				(and (isa? a css-selector-pseudo)
				     (not (pseudo-attr? a))))
			attr*)))
	 (with-access::hss-compiler hc (selector)
	    (let loop ((ls selector))
	       (let ((s (car ls)))
		  (if (null? (cdr ls))
		      (list
			 (if (isa? s css-selector)
			     (with-access::css-selector s ((sattr* attr*))
				(duplicate::css-selector s
				   (attr* (append sattr* attr*))))
			     s))
		      (cons
			 (if (isa? s css-selector)
			     (with-access::css-selector s (attr*)
				(duplicate::css-selector s
				   (attr* (append attr* npattr*))))
			     s)
			 (loop (cdr ls)))))))))
   
   (with-access::css-selector o (element attr*)
      (with-access::hss-compiler hc (body)
	 (with-access::css-selector-name element ((el name))
	    (if (and body bodyp)
		(append
		   (let ((attr* (filter (lambda (a) (not (pseudo-attr? a)))
				   attr*)))
		      (copy-compiler-selectors hc attr*))
		   (list
		      '| |
		      (instantiate::css-selector
			 (element body)
			 (attr* (filter pseudo-attr? attr*)))))
		(copy-compiler-selectors hc attr*))))))

;*---------------------------------------------------------------------*/
;*    hss-parse-function ...                                           */
;*---------------------------------------------------------------------*/
(define (hss-parse-function fun s)
   (define (err msg)
      (error fun msg s))
   (let ((ast (hss->ast (format "* {x:~a;}" s))))
      (if (not (isa? ast css-stylesheet))
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
			   expr))))))))
   
;*---------------------------------------------------------------------*/
;*    compile ::css-function ...                                       */
;*---------------------------------------------------------------------*/
(define-method (compile o::css-function penv)
   (with-access::css-function o (fun expr)
      (let ((comp (synchronize *hss-compiler-mutex*
		     (hashtable-get *hss-function-env* fun)))
	    (nexpr (compile expr penv)))
	 (if comp
	     (let ((args (filter (lambda (o) (not (equal? o ","))) nexpr)))
		(if (correct-arity? comp (length args))
		    (hss-parse-function fun (apply comp args))
		    (error fun "wrong number of arguments" expr)))
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
		((isa? hc hss-compiler)
		 (let ((use-body (and (pair? (cdr lst))
				      (symbol? (cadr lst))
				      (not (eq? (cadr lst) '+)))))
		    (append (hss-compile-selector o hc use-body)
			    (hss-compile-selector* (cdr lst)))))
		((isa? element css-selector-name)
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
	  (let ((new (synchronize *hss-compiler-mutex*
			(hashtable-get *hss-type-env* (string-downcase name)))))
	     (if (isa? new hss-compiler)
		 (with-access::hss-compiler new (selector)
		    selector)
		 (list (duplicate::css-selector s
			  (element o)))))
	  (list (duplicate::css-selector s
		   (element o))))))

;*---------------------------------------------------------------------*/
;*    hss-extension ...                                                */
;*---------------------------------------------------------------------*/
(define (hss-extension c ip)
   
   (define (ill-eof pos)
      (read-error/location "Unexpected end-of-file" "Unclosed hss extension"
	 (input-port-name ip) pos))
   
   (when (char=? c #\$)
      (let ((follow (read-char ip)))
	 (unread-char! follow ip)
	 (cond
	    ((eof-object? exp)
	     (ill-eof (input-port-position ip)))
	    ((char=? follow #\{)
	     ;; a foreign extension
	     ((hop-hss-foreign-eval) ip))
	    (else
	     (let ((exp (hop-read ip))
		   (pos (input-port-position ip)))
		(with-handler
		   (lambda (e)
		      (cond
			 ((isa? e &eval-warning)
			  (warning-notify e)
			  #unspecified)
			 ((and (isa? e &exception)
			       (not (with-access::&exception e (location) location)))
			  (with-access::&exception e (location fname)
			     (set! location pos)
			     (set! fname (input-port-name ip)))
			  (raise e))
			 (else
			  (raise e))))
		   (eval exp))))))))

;*---------------------------------------------------------------------*/
;*    object-display ::css-hash-color ...                              */
;*---------------------------------------------------------------------*/
(define-method (object-display o::css-hash-color . port)
   (let ((p (if (pair? port) (car port) (current-output-port))))
      (display "#" p)
      (with-access::css-hash-color o (value)
	 (display value p))))

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
;; display
(define-hss-property (display v p)
   (if (equal? v '("box"))
       (format "display: box ~a;
  display: -moz-box ~a;
  display: -webkit-box ~a;
  display: -o-box ~a;"
	       p p p p p)
       (format "display: ~l ~a;" v p)))

;; transform-origin
(define-hss-property (transform-origin v p)
   (match-case v
      ((?h ?v)
   (format "transform-origin: ~a ~a;
-webkit-transform-origin: ~a ~a;"
      h v h v))
      (else
       (format "transform-origin: ~l;" v))))

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

;; user-select
(define-hss-property (user-select v p)
   (format "user-select: ~l;
  -moz-user-select: ~l;
  -khtml-user-select: ~l;
  -webkit-user-select: ~l;
  -o-user-select: ~l;"
	   v v v v v))

;; multi-column
(define-hss-property (column-width v p)
   (format "column-width: ~l;
  -moz-column-width: ~l;
  -khtml-column-width: ~l;
  -webkit-column-width: ~l;
  -o-column-width: ~l;"
	   v v v v v))

(define-hss-property (column-count v p)
   (format "column-count: ~l;
  -moz-column-count: ~l;
  -khtml-column-count: ~l;
  -webkit-column-count: ~l;
  -o-column-count: ~l;"
	   v v v v v))

(define-hss-property (column-gap v p)
   (format "column-gap: ~l;
  -moz-column-gap: ~l;
  -khtml-column-gap: ~l;
  -webkit-column-gap: ~l;
  -o-column-gap: ~l;"
	   v v v v v))

(define-hss-property (column-rule v p)
   (format "column-rule: ~l;
  -moz-column-rule: ~l;
  -khtml-column-rule: ~l;
  -webkit-column-rule: ~l;
  -o-column-rule: ~l;"
	   v v v v v))

;; transform
(define-hss-property (transform v p)
   (format "transform: ~l;
  -moz-transform: ~l;
  -webkit-transform: ~l;
  -o-transform: ~l;"
	   v v v v))

;; transition
(define-hss-property (transition v p)
   (format "transition: ~l ~a;
  -moz-transition: ~l ~a;
  -webkit-transition: ~l ~a;
  -o-transition: ~l ~a;"
	   v p v p v p v p))

;; transition-property
(define-hss-property (transition-property v p)
   (format "transition: ~l ~a;
  -moz-transition-property: ~l ~a;
  -webkit-transition-property: ~l ~a;
  -o-transition-property: ~l ~a;"
	   v p v p v p v p))

;; transition-duration
(define-hss-property (transition-duration v p)
   (format "transition-duration: ~l ~a;
  -moz-transition-duration: ~l ~a;
  -webkit-transition-duration: ~l ~a;
  -o-transition-duration: ~l ~a;"
	   v p v p v p v p))

;; transition-delay
(define-hss-property (transition-delay v p)
   (format "transition-delay: ~l ~a;
  -moz-transition-delay: ~l ~a;
  -webkit-transition-delay: ~l ~a;
  -o-transition-delay: ~l ~a;"
	   v p v p v p v p))

;; transition-timing-function
(define-hss-property (transition-timing-function v p)
   (format "transition-timing-function: ~l ~a;
  -moz-transition-timing-function: ~l ~a;
  -webkit-transition-timing-function: ~l ~a;
  -o-transition-timing-function: ~l ~a;"
	   v p v p v p v p))

;; box-align
(define-hss-property (box-align v p)
   (format "box-align: ~l;
  -moz-box-align: ~l;
  -webkit-box-align: ~l;
  -o-box-align: ~l;"
	   v v v v))

;; box-direction
(define-hss-property (box-direction v p)
   (format "box-direction: ~l;
  -moz-box-direction: ~l;
  -webkit-box-direction: ~l;
  -o-box-direction: ~l;"
	   v v v v))

;; box-flex
(define-hss-property (box-flex v p)
   (format "box-flex: ~l;
  -moz-box-flex: ~l;
  -webkit-box-flex: ~l;
  -o-box-flex: ~l;"
	   v v v v))

;; box-flexgroup
(define-hss-property (box-flexgroup v p)
   (format "box-flexgroup: ~l;
  -moz-box-flexgroup: ~l;
  -webkit-box-flexgroup: ~l;
  -o-box-flexgroup: ~l;"
	   v v v v))

;; box-ordinal-group
(define-hss-property (box-ordinal-group v p)
   (format "box-ordinal-group: ~l;
  -moz-box-ordinal-group: ~l;
  -webkit-box-ordinal-group: ~l;
  -o-box-ordinal-group: ~l;"
	   v v v v))

;; box-orient
(define-hss-property (box-orient v p)
   (format "box-orient: ~l;
  -moz-box-orient: ~l;
  -webkit-box-orient: ~l;
  -o-box-orient: ~l;"
	   v v v v))

;; box-pack
(define-hss-property (box-pack v p)
   (format "box-pack: ~l;
  -moz-box-pack: ~l;
  -webkit-box-pack: ~l;
  -o-box-pack: ~l;"
	   v v v v))

;; box-shadow
(define-hss-property (box-shadow v p)
   (format "box-shadow: ~l;
  -moz-box-shadow: ~l;
  -webkit-box-shadow: ~l;
  -o-box-shadow: ~l;"
	   v v v v))

;; box-sizing
(define-hss-property (box-sizing v p)
   (format "box-sizing: ~l;
  -moz-box-sizing: ~l;
  -webkit-box-sizing: ~l;
  -o-box-sizing: ~l;"
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
