;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/allocate_names.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-13                                           */
;*    Last change :  Tue Aug  6 12:13:19 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Set variables name                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module allocate-names
   
   (import config
	   tools
	   nodes
	   dump-node
	   export-desc
	   walk
	   verbose
	   gen-js)
   
   (static (class Name-Env
	      compress?::bool
	      suffix
	      ;; non-compressed
	      (counter::bint (default 0))
	      ;; compressed
	      (last-generated-id::bstring (default "")))
	   
	   (wide-class Global-Var::Var))
   
   (export (wide-class Named-Var::Var
	      (js-id::bstring read-only)))
   
   (export (gen-var-names tree)))

;*---------------------------------------------------------------------*/
;*    gen-var-names ...                                                */
;*---------------------------------------------------------------------*/
(define (gen-var-names tree)
   (verbose "  generating names for vars")
   (find-free tree #f #f '()) ;; local version
   (let ((env (instantiate::Name-Env
		 (compress? (config 'compress))
		 (suffix (when (config 'statics-suffix)
			    (suffix-mangle (config 'statics-suffix)))))))
      (name-gen tree env #f)))

;*---------------------------------------------------------------------*/
;*    find-free ::Node ...                                             */
;*    -------------------------------------------------------------    */
;*    At this compilation-stage 'let's have been removed.              */
;*    we thus only have to look at the 'declared'-vars and at          */
;*    Frame-alloc.                                                     */
;*---------------------------------------------------------------------*/
(define-nmethod (Node.find-free surrounding-fun declared-vars-list)
   (default-walk this surrounding-fun declared-vars-list))

;*---------------------------------------------------------------------*/
;*    find-free ::Execution-Unit ...                                   */
;*---------------------------------------------------------------------*/
(define-nmethod (Execution-Unit.find-free surrounding-fun declared-vars-list)
   (with-access::Execution-Unit this (scope-vars declared-vars free-vars)
      (set! free-vars '())
      ;; scope-vars contains parameters to functions.
      (default-walk this this (list scope-vars declared-vars))
      (when surrounding-fun
	 (let ((this-free-vars free-vars))
	    ;; free vars might be free for surrounding too.
	    (with-access::Execution-Unit surrounding-fun (free-vars)
	       (for-each (lambda (var)
			    (unless (or (any (lambda (s) (memq var s))
					   declared-vars-list)
					(memq var free-vars))
			       (cons-set! free-vars var)))
		  this-free-vars))))))

;*---------------------------------------------------------------------*/
;*    find-free ::Frame-push ...                                       */
;*---------------------------------------------------------------------*/
(define-nmethod (Frame-push.find-free surrounding-fun declared-vars-list)
   (with-access::Frame-push this (frame-alloc)
      ;; the storage-vars allocate new vars that are supposed to be visible
      ;; inside the frame-pushes.
      (let ((declared-vars (with-access::Frame-alloc frame-alloc (vars) vars)))
	 (default-walk this surrounding-fun
	    (cons declared-vars declared-vars-list)))))

;*---------------------------------------------------------------------*/
;*    find-free ::Ref ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (Ref.find-free surrounding-fun declared-vars-list)
   (with-access::Execution-Unit surrounding-fun (free-vars)
      (with-access::Ref this (var)
	 (unless (or (eq? (with-access::Var var (kind) kind) 'this)
		     (any (lambda (l) (memq var l)) declared-vars-list)
		     (memq var free-vars))
	    (cons-set! free-vars var)))))

;*---------------------------------------------------------------------*/
;*    nice-mangle ...                                                  */
;*---------------------------------------------------------------------*/
(define (nice-mangle::bstring str)
   (let* ((str0 (string-replace str #\- #\_))
	  (str_ (string-replace str0 #\* #\@)))
      (cond
	 ((string-suffix? "?" str_)
	  (if (or (string-prefix? "is_" str_)
		  (string-prefix? "has_" str_))
	      (substring str_ 0 (- (string-length str_) 1))
	      (string-append "is_"
		 (substring str_
		    0
		    (- (string-length str_) 1)))))
	 ((string-suffix? "!" str_)
	  (string-append (substring str_ 0 (- (string-length str_) 1))
	     "_bang"))
	 (else
	  str_))))

;*---------------------------------------------------------------------*/
;*    suffix-mangle ...                                                */
;*---------------------------------------------------------------------*/
(define (suffix-mangle str)
   (define (replace s L)
      (if (null? L)
	  s
	  (replace (string-replace s (caar L) (cdar L))
	     (cdr L))))
   
   (let ((str_ (replace str '((#\- . #\_)
			      (#\. . #\_)
			      (#\space . #\_)
			      (#\# . #\_)
			      (#\~ . #\_)
			      (#\* . #\_)
			      (#\: . #\_)))))
      (if (bigloo-need-mangling? str_)
	  (bigloo-mangle str_)
	  str_)))

;*---------------------------------------------------------------------*/
;*    valid? ...                                                       */
;*---------------------------------------------------------------------*/
(define (valid? str)
   (valid-JS-str? str))

;*---------------------------------------------------------------------*/
;*    *dangerous-js* ...                                               */
;*---------------------------------------------------------------------*/
(define *dangerous-js*
   '("Object" "undefined" "Array" "Number" "String" "Date" "RegExp"))

;*---------------------------------------------------------------------*/
;*    dangerous? ...                                                   */
;*---------------------------------------------------------------------*/
(define (dangerous? str)
   (and (member str *dangerous-js*) #t))

;*---------------------------------------------------------------------*/
;*    used? ...                                                        */
;*---------------------------------------------------------------------*/
(define (used? str used-ht)
   (and (hashtable-get used-ht str)))

;*---------------------------------------------------------------------*/
;*    allocate-name ...                                                */
;*---------------------------------------------------------------------*/
(define (allocate-name v::Var env used-ht)
   (unless (isa? v Named-Var)
      (with-access::Var v (kind)
	 (let ((kind (if (isa? v Global-Var)
			 'global
			 kind))
	       (compress? (with-access::Name-Env env (compress?) compress?)))
	    (case kind
	       ((local)
		(if compress?
		    (allocate-compressed-local-name v env used-ht)
		    (allocate-local-name v env used-ht)))
	       ((global)
		(if compress?
		    (allocate-compressed-global-name v env used-ht)
		    (allocate-global-name v env used-ht)))
	       ((this)
		(allocate-this-name v used-ht))
	       ((imported exported)
		(allocate-exported-name v env used-ht)))))
      (hashtable-put! used-ht (with-access::Named-Var v (js-id) js-id) #t)))

;*---------------------------------------------------------------------*/
;*    reset-compressed-id! ...                                         */
;*    -------------------------------------------------------------    */
;*    call to reset the last-generated-id.                             */
;*    Too often and the id-generation will slow down.                  */
;*    Not often enough and the ids will not be as short as possible.   */
;*---------------------------------------------------------------------*/
(define (reset-compressed-id! env)
   (with-access::Name-Env env (last-generated-id)
      (set! last-generated-id "")))

;*---------------------------------------------------------------------*/
;*    generate-compress-id ...                                         */
;*---------------------------------------------------------------------*/
(define (generate-compressed-id env used-ht suffix)
   (define (increment-char c)
      (cond
	 ((char=? #\z c) #\A)
	 ((char=? #\Z c) #\0)
	 (else (integer->char (+fx 1 (char->integer c))))))
   
   (define (increment-buffer buffer)
      (cond
	 ((string-null? buffer)
	  (string-append "a"))
	 (else
	  (let* ((last-pos (-fx (string-length buffer) 1))
		 (last-c (string-ref buffer last-pos)))
	     (cond
		((char=? #\9 last-c)
		 (string-set! buffer last-pos #\a)
		 (let loop ((i (-fx last-pos 1)))
		    (cond
		       ((<fx i 0) ;; buffer was 99999...
			(increment-buffer (string-append "a" buffer)))
		       ((char=? #\9 (string-ref buffer i))
			(string-set! buffer i #\a)
			(loop (-fx i 1)))
		       (else
			(string-set! buffer i
			   (increment-char (string-ref buffer i)))
			buffer))))
		(else
		 (string-set! buffer last-pos (increment-char last-c))
		 buffer))))))
   
   (with-access::Name-Env env (last-generated-id)
      ;; we need to do a copy of the last-generated-id as increment-buffer
      ;; will modify it.
      (let ((big-buff #f)
	    (buffer (string-copy last-generated-id))
	    (suf-len (if suffix (string-length suffix) 0)))
	 (let loop ((new-generated (increment-buffer buffer)))
	    (let* ((new-len (string-length new-generated))
		   (new-id (cond
			      ((not suffix) new-generated)
			      ((or (not big-buff)
				   (not (=fx (+fx new-len suf-len)
					   (string-length big-buff))))
			       (let ((t (string-append new-generated suffix)))
				  (set! big-buff t)
				  t))
			      (else
			       (blit-string! new-generated 0 big-buff 0 new-len)
			       (blit-string! suffix 0 big-buff new-len suf-len)
			       big-buff))))
	       (if (and (valid? new-id)
			(not (dangerous? new-id))
			(not (used? new-id used-ht)))
		   (begin
		      (set! last-generated-id new-generated)
		      new-id)
		   (loop (increment-buffer new-generated))))))))

;*---------------------------------------------------------------------*/
;*    allocate-compress-local-name ...                                 */
;*---------------------------------------------------------------------*/
(define (allocate-compressed-local-name v::Var env used-ht)
   (widen!::Named-Var v (js-id (generate-compressed-id env used-ht ""))))

;*---------------------------------------------------------------------*/
;*    allocate-local-name ...                                          */
;*---------------------------------------------------------------------*/
(define (allocate-local-name v::Var env used-ht)
   ;; generate ids
   (with-access::Var v (escapes? id)
      (let loop ((short (nice-mangle (symbol->string id)))
		 ;;(short (bigloo-mangle (symbol->string id)))
		 (already-tried? #f))
	 (cond
	    ((and (valid? short)
		  (not (dangerous? short))
		  (not (used? short used-ht)))
	     (widen!::Named-Var v (js-id short)))
	    (already-tried?
	     (widen!::Named-Var v (js-id (gen-JS-sym id))))
	    (else
	     (with-access::Name-Env env (counter)
		(set! counter (+fx counter 1))
		(let ((next-try
		       (cond
			  ((or (string-prefix? "sc_" short)
			       (string-prefix? "SC_" short))
			   (string-append "v_" short "_"
					  (number->string counter)))
			  (else
			   (string-append short "_"
					  (number->string counter))))))
		   (loop next-try #t))))))))

;*---------------------------------------------------------------------*/
;*    allocate-this-name ...                                           */
;*---------------------------------------------------------------------*/
(define (allocate-this-name v::Var used-ht)
   (widen!::Named-Var v (js-id "this")))

;*---------------------------------------------------------------------*/
;*    allocate-exported-name ...                                       */
;*---------------------------------------------------------------------*/
(define (allocate-exported-name v::Var env used-ht)
   (with-access::Var v (export-desc)
      (let ((js-id (with-access::Export-Desc export-desc (js-id) js-id)))
	 (widen!::Named-Var v (js-id js-id)))))

;*---------------------------------------------------------------------*/
;*    allocate-compress-global-name ...                                */
;*---------------------------------------------------------------------*/
(define (allocate-compressed-global-name v::Var env used-ht)
   (with-access::Name-Env env (suffix)
      (widen!::Named-Var v
	 (js-id (generate-compressed-id env used-ht (or suffix ""))))))

;*---------------------------------------------------------------------*/
;*    allocate-global-name ...                                         */
;*---------------------------------------------------------------------*/
(define (allocate-global-name v::Var env used-ht)
   (with-access::Name-Env env (suffix)
      (if (not suffix)
	  ;; no suffix -> treat it, as if it was a local var.
	  (allocate-local-name v env used-ht)
	  (with-access::Var v (id escapes?)
	     (widen!::Named-Var v
		(js-id (bigloo-module-mangle
			  (string-append (symbol->string id) "." (JS-stamp))
			  suffix)))))))

;*---------------------------------------------------------------------*/
;*    name-gen ::Node ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (Node.name-gen used-ht)
   (default-walk this used-ht))

;*---------------------------------------------------------------------*/
;*    name-gen ::Module ...                                            */
;*---------------------------------------------------------------------*/
(define-nmethod (Module.name-gen used-ht)
   (let ((used-ht (make-hashtable)))
      (reset-compressed-id! env)
      (with-access::Module this (runtime-vars imported-vars declared-vars
				   this-var)
	 (for-each (lambda (var)
		      (allocate-name var env used-ht))
	    runtime-vars)
	 (for-each (lambda (var)
		      (allocate-name var env used-ht))
	    imported-vars)
	 ;; first allocate Exported, as they have precedence over local vars.
	 ;; ex: (define export_var _some_val_) (let ((export-var ....)) ...)
	 ;;   if 'export-var' is handled before the exported one, it receives
	 ;;   'export_var' as JS-id. -> bad...
	 (for-each (lambda (var)
		      (unless (eq? (with-access::Var var (kind) kind) 'local)
			 (allocate-name var env used-ht)))
	    declared-vars)
	 (reset-compressed-id! env)
	 ;; now that the Exported vars have been handled we can assign the
	 ;; names to the global vars.
	 (for-each (lambda (var)
		      (when (eq? (with-access::Var var (kind) kind) 'local)
			 (widen!::Global-Var var)
			 (allocate-name var env used-ht)))
	    declared-vars)
	 (allocate-name this-var env used-ht)
	 (default-walk this used-ht))))

;*---------------------------------------------------------------------*/
;*    name-gen ::Lambda ...                                            */
;*---------------------------------------------------------------------*/
(define-nmethod (Lambda.name-gen used-ht)
   (with-access::Lambda this (scope-vars declared-vars this-var free-vars)
      (reset-compressed-id! env)
      (let ((lambda-used-ht (make-hashtable)))
	 (for-each (lambda (var)
		      (hashtable-put! lambda-used-ht
			 (with-access::Named-Var var (js-id) js-id)
			 #t))
	    free-vars)
	 
	 (for-each (lambda (var)
		      (allocate-name var env lambda-used-ht))
	    scope-vars)
	 
	 (for-each (lambda (var)
		      (allocate-name var env lambda-used-ht))
	    declared-vars)
	 
	 (allocate-name this-var env lambda-used-ht)
	 (default-walk this lambda-used-ht))))

;*---------------------------------------------------------------------*/
;*    name-gen ::Frame-alloc ...                                       */
;*---------------------------------------------------------------------*/
(define-nmethod (Frame-alloc.name-gen used-ht)
   (with-access::Frame-alloc this (vars storage-var)
      (for-each (lambda (var)
		   (allocate-name var env used-ht))
	 vars)
      (default-walk this used-ht)
      (for-each (lambda (var)
		   (hashtable-remove! used-ht
		      (with-access::Named-Var var (js-id) js-id)))
	 vars)))
